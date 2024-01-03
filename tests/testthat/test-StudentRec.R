test_that("StudentRecord", {
##Start with manifest
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
config.dir <- file.path(library(help="Peanut")$path, "auxdata")
netman1 <- read.csv(file.path(config.dir,"Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
net.dir <- file.path(library(help="PNetica")$path, "testnets")
Nethouse <- PNetica::BNWarehouse(manifest=netman1,session=sess,key="Name",
                       address=net.dir)

dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                     context="*Baseline*")
sm(dsr) <- WarehouseSupply(Nethouse,"miniPP_CM")
PnetCompile(sm(dsr))

## dsr <- updateStats(eng,dsr)
statmat <- read.csv(file.path(config.dir,"Mini-PP-Statistics.csv"),
                    stringsAsFactors=FALSE)
rownames(statmat) <- statmat$Name
statlist <- sapply(statmat$Name,function (st)
    Statistic(statmat[st,"Fun"],statmat[st,"Node"],st))
names(statlist) <- statmat$Name
dsr@stats <- lapply(statlist,
                    function (stat) calcStat(stat,sm(dsr)))
names(dsr@stats) <- names(statlist)
stat(dsr,"Physics_EAP")
stat(dsr,"Physics_Margin")

## dsr <- baselineHist(eng,dsr)

dsr@hist <- lapply(c("Physics"),
                     function (nd)
                     EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
names(dsr@hist) <- "Physics"
history(dsr,"Physics")

## Serialization and unserialization
dsr.ser <- as.json(dsr)

dsr1 <- parseStudentRecord(jsonlite::fromJSON(dsr.ser))
dsr1 <- fetchSM(dsr1,Nethouse)

### dsr and dsr1 should be the same.
stopifnot(
 app(dsr)==app(dsr1),
 uid(dsr)==uid(dsr1),
 context(dsr)==context(dsr1),
# problems with timezones
# all.equal(timestamp(dsr),timestamp(dsr1)),
 all.equal(seqno(dsr),seqno(dsr1)),
 all.equal(stats(dsr),stats(dsr1),tolerance=.0002),
 all.equal(history(dsr,"Physics"),history(dsr1,"Physics")),
 PnetName(sm(dsr)) == PnetName(sm(dsr))
)

})

test_that("toString(StudentRecord) & show()", {

})


test_that("app(StudentRecord)", {

})

test_that("uid(StudentRecord)", {

})

test_that("context(StudentRecord)", {

})

test_that("timestamp(StudentRecord)", {

})

test_that("seqno(StudentRecord)", {
  ## Getter & Setter
})

test_that("stats(StudentRecord), stat() & statNames()", {

stats <- list(Physics_EAP=0,EnergyTransfer_EAP=.15,
              Physics_Margin=c(High=1/3,Medium=1/3,
                               Low=1/3))

dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                     context="*Baseline*",stats=stats)

stats(dsr)
stopifnot(all.equal(stats,stats,tolerance=.0002))

statNames(dsr)
stopifnot(all(statNames(dsr)==names(stats)))

stat(dsr,"Physics_Margin")
stopifnot(all.equal(stat(dsr,"Physics_Margin"),stats[[3]],tolerance=.0002))


})

test_that("hist(StudentRecord), history() & histNames()", {

##Start with manifest
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
config.dir <- file.path(library(help="Peanut")$path, "auxdata")
netman1 <- read.csv(file.path(config.dir,"Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
net.dir <- file.path(library(help="PNetica")$path, "testnets")
Nethouse <- PNetica::BNWarehouse(manifest=netman1,session=sess,key="Name",
                       address=net.dir)
dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                     context="*Baseline*")
sm(dsr) <- WarehouseSupply(Nethouse,"miniPP_CM")
PnetCompile(sm(dsr))
## dsr <- updateStats(eng,dsr)

statmat <- read.csv(file.path(config.dir,"Mini-PP-Statistics.csv"),
                    stringsAsFactors=FALSE)
rownames(statmat) <- statmat$Name
statlist <- sapply(statmat$Name,function (st)
    Statistic(statmat[st,"Fun"],statmat[st,"Node"],st))
names(statlist) <- statmat$Name 
dsr@stats <- lapply(statlist,
                    function (stat) calcStat(stat,sm(dsr)))
names(dsr@stats) <- names(statlist)
stat(dsr,"Physics_EAP")
stat(dsr,"Physics_Margin")

## dsr <- baselineHist(eng,dsr)

dsr@hist <- lapply(c("Physics"),
                     function (nd)
                     EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
names(dsr@hist) <- "Physics"
stopifnot(histNames(dsr)=="Physics")
history(dsr,"Physics")

})



test_that("evidenceLog(StudentRecord)", {
  ## Getter & Setter
})


test_that("useObs(StudentRecord) & ignoreObs(StudentRecord)", {

})

test_that("getIssues(StudentRecord) & logIssues()", {

sr0 <-
    StudentRecord("S1","*baseline*",as.POSIXct("2020-03-30 09:00:00"))

sr0 <- logIssue(sr0,"Test Issue")
err <- simpleError("Another test error.")
sr0 <- logIssue(sr0,err)
getIssues(sr0)

})

test_that("sm(StudentRecord)", {
  ## Getter and Setter
  library(PNetica)

##Start with manifest
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
config.dir <- file.path(library(help="Peanut")$path, "auxdata")
netman1 <- read.csv(file.path(config.dir,"Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
net.dir <- file.path(library(help="PNetica")$path, "testnets")
Nethouse <- PNetica::BNWarehouse(manifest=netman1,session=sess,key="Name",
                       address=net.dir)

dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                     context="*Baseline*")
sm(dsr) <- WarehouseSupply(Nethouse,"miniPP_CM")
PnetCompile(sm(dsr))

})

test_that("fetchSM(StudentRecord)", {
##Start with manifest
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
config.dir <- file.path(library(help="Peanut")$path, "auxdata")
netman1 <- read.csv(file.path(config.dir,"Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
net.dir <- file.path(library(help="PNetica")$path, "testnets")
Nethouse <- PNetica::BNWarehouse(manifest=netman1,session=sess,key="Name",
                       address=net.dir)

dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                     context="*Baseline*")
sm(dsr) <- WarehouseSupply(Nethouse,"miniPP_CM")
PnetCompile(sm(dsr))

## dsr <- updateStats(eng,dsr)
statmat <- read.csv(file.path(config.dir,"Mini-PP-Statistics.csv"),
                    stringsAsFactors=FALSE)
rownames(statmat) <- statmat$Name
statlist <- sapply(statmat$Name,function (st)
    Statistic(statmat[st,"Fun"],statmat[st,"Node"],st))
names(statlist) <- statmat$Name 
dsr@stats <- lapply(statlist,
                    function (stat) calcStat(stat,sm(dsr)))
names(dsr@stats) <- names(statlist)

## dsr <- baselineHist(eng,dsr)
dsr@hist <- lapply(c("Physics"),
                     function (nd)
                     EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
names(dsr@hist) <- "Physics"

pnodenames <- names(PnetPnodes(sm(dsr)))


## Serialization and unserialization
dsr.ser <- as.json(dsr)

dsr1 <- parseStudentRecord(jsonlite::fromJSON(dsr.ser))
stopifnot(is.null(sm(dsr1)))
## at this point, SM has not yet been restored.


## It is there in the serial field
net1 <- unpackSM(dsr1,Nethouse)
stopifnot(all.equal(pnodenames,names(PnetPnodes(net1))))
dsr1 <- fetchSM(dsr1,Nethouse)
stopifnot(all.equal(pnodenames,names(PnetPnodes(sm(dsr1)))))

## Try this again, but first delete net from warehouse,
## So we are sure we are building it from serialized version.
WarehouseFree(Nethouse,PnetName(sm(dsr)))

dsr1 <- parseStudentRecord(jsonlite::fromJSON(dsr.ser))
stopifnot(is.null(sm(dsr1)))
## at this point, SM has not yet been restored.


## It is there in the serial field
net1 <- unpackSM(dsr1,Nethouse)
stopifnot(all.equal(pnodenames,names(PnetPnodes(net1))))
dsr1 <- fetchSM(dsr1,Nethouse)
stopifnot(all.equal(pnodenames,names(PnetPnodes(sm(dsr1)))))

})

test_that("unpackSM(StudentRecord)", {

})

test_that("evidence(StudentRecord)", {
  ## Getter & Setter
})


test_that("as.jlist(StudentRecord)", {

})


test_that("parseStudentRecord(StudentRecord)", {

})

test_that("unparseStats", {

})

test_that("stats2json", {

})

test_that("strsplit2", {

})

test_that("unflatten", {

})

test_that("parseStats", {

stats <- list(Physics_EAP=0,EnergyTransfer_EAP=.15,
              Physics_Margin=c(High=1/3,Medium=1/3,
                               Low=1/3))
stats2json(stats)

stats1 <- parseStats(ununboxer(unparseStats(stats)))
stopifnot(all.equal(stats,stats1,tolerance=.0002))

stats2json(stats,flatten=TRUE)

stats2 <- parseStats(ununboxer(unparseStats(stats,flatten=TRUE)))
stopifnot(all.equal(stats,stats2,tolerance=.0002))

})

test_that("unflattenNames", {

})


test_that("updateRecord", {

})

test_that("StudentRecordSet", {

})

test_that("StudentRecordSet", {

})

test_that("app(StudentRecordSet)", {

##Start with manifest
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
config.dir <- file.path(library(help="Peanut")$path, "auxdata")
netman1 <- read.csv(file.path(config.dir,"Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
net.dir <- file.path(library(help="PNetica")$path, "testnets")
Nethouse <- PNetica::BNWarehouse(manifest=netman1,session=sess,key="Name",
                       address=net.dir)

## Setup to test without Mongo
SRS <- StudentRecordSet(app="Test",warehouse=Nethouse,
                        db=mongo::MongoDB(noMongo=TRUE))
stopifnot(!mdbAvailable((SRS$recorddb())))

## Setup default SR
dsr <- StudentRecord("*DEFAULT*",app="Test",
                     context="*Baseline*")
sm(dsr) <- WarehouseSupply(Nethouse,"miniPP_CM")
PnetCompile(sm(dsr))

## dsr <- updateStats(eng,dsr)
statmat <- read.csv(file.path(config.dir,"Mini-PP-Statistics.csv"),
                    stringsAsFactors=FALSE)
rownames(statmat) <- statmat$Name
statlist <- sapply(statmat$Name,function (st)
    Statistic(statmat[st,"Fun"],statmat[st,"Node"],st))
names(statlist) <- statmat$Name
dsr@stats <- lapply(statlist,
                    function (stat) calcStat(stat,sm(dsr)))
names(dsr@stats) <- names(statlist)

dsr@hist <- lapply(c("Physics"),
                     function (nd)
                     EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
names(dsr@hist) <- "Physics"

SRS$defaultSR <- dsr
saveSR(SRS, dsr)

## Make a new Student Record for a student.
sr1 <- newSR(SRS,"S1")
stopifnot(uid(sr1)=="S1",app(sr1)==app(dsr),
          all.equal(stats(dsr),stats(sr1),.0002))


sr1a <- getSR(SRS,"S1")

clearSRs(SRS)

})

test_that("getSR(StudentRecordSet) No Mongo", {
## Requires PNetica
library(PNetica)  ## Must load to setup Netica DLL
app <- "ecd://epls.coe.fsu.edu/EITest"
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)

config.dir <- file.path(library(help="Peanut")$path, "auxdata")
net.dir <- file.path(library(help="PNetica")$path,"testnets")

netman <- read.csv(file.path(config.dir, "Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
stattab <- read.csv(file.path(config.dir, "Mini-PP-Statistics.csv"),
                    as.is=TRUE)

Nethouse <- PNetica::BNWarehouse(netman,session=sess,
             address=net.dir)

cl <- new("CaptureListener")
listeners <- list("cl"=cl)

ls <- ListenerSet(sender= paste("EAEngine[",app,"]"),
                  db=MongoDB(noMongo=TRUE), listeners=listeners)

eng <- newBNEngineNDB(app=app,warehouse=Nethouse,
                     listenerSet=ls,manifest=netman,
                     profModel="miniPP_CM",
                     histNodes="Physics",
                     statmat=stattab,
                     activeTest="EAActive.txt")

## Standard initialization methods.
loadManifest(eng,netman)
eng$setHistNodes("Physics")
configStats(eng,stattab)
setupDefaultSR(eng)

tr1 <- newSR(eng$studentRecords(),"Test1")
PnetCompile(sm(tr1))
stopifnot(uid(tr1)=="Test1",abs(stat(tr1,"Physics_EAP")) < .0001)
stopifnot(is.na(m_id(tr1)))  # id is NA as it has not been saved yet.

tr1 <- saveSR(eng$studentRecords(),tr1)
m_id(tr1)
stopifnot(!is.na(m_id(tr1)))  # Now set

sr0 <- getRecordForUser(eng,"S1")

eap0 <- stat(sr0,"Physics_EAP")

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))

e1 <- logEvidence(eng,sr0,e1)
sr1 <- accumulateEvidence(eng,sr0,e1)
stopifnot(m_id(sr1)!=m_id(sr0),sr1@prev_id==m_id(sr0))
stopifnot(seqno(sr1)==1L, seqno(e1)==1L)

eap1 <- stat(sr1,"Physics_EAP")
stopifnot(abs(eap1-eap0) > .001)
stopifnot(nrow(history(sr1,"Physcis"))==2L)

sr1.ser <- as.json(sr1)
WarehouseFree(Nethouse,PnetName(sm(sr1))) # Delete student model to
                                           # force restore.

sr1a <- getSR(eng$studentRecords(),"S1",fromJSON(sr1.ser))
PnetCompile(sm(sr1a))
eap1a <- stat(sr1a,"Physics_EAP")
stopifnot(abs(eap1-eap1a) < .001)
stopifnot(nrow(history(sr1a,"Physcis"))==2L)

})


test_that("getSR(StudentRecordSet) Mongo", {
##  <<Here>> Need test with Mongo implementation
fail("Test not implemented")
app <- "ecd://epls.coe.fsu.edu/EITest"
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)

config.dir <- file.path(library(help="Peanut")$path, "auxdata")
net.dir <- file.path(library(help="PNetica")$path,"testnets")

netman <- read.csv(file.path(config.dir, "Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
stattab <- read.csv(file.path(config.dir, "Mini-PP-Statistics.csv"),
                    as.is=TRUE)

Nethouse <- PNetica::BNWarehouse(netman,session=sess,
             address=net.dir)

cl <- new("CaptureListener")
listeners <- list("cl"=cl)

ls <- ListenerSet(sender= paste("EAEngine[",app,"]"),
                  db=mongo::MongoDB("Messages","EARecords", makeDBuri()),
                  registryDB=mongo::MongoDB("OutputFiles","Proc4",makeDBuri()),
                  listeners=listeners)

eng <- newBNEngineMongo(app=app,warehouse=Nethouse,
                     listenerSet=ls,
                     profModel="miniPP_CM",
                     histNodes="Physics",
                     dburi=makeDBuri(),
                     dbname="EARecords",admindbname="Proc4")



## Standard initialization methods.
loadManifest(eng,netman)
eng$setHistNodes("Physics")
configStats(eng,stattab)
setupDefaultSR(eng)

})

test_that("revertSM(StudentRecordSet)", {

})

test_that("saveSR(StudentRecordSet)", {

})

test_that("newSR(StudentRecordSet)", {

})

test_that("clearSRs(StudentRecordSet)", {

})



