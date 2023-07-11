library(EABN)
library(RNetica)  ## Must load to setup Netica DLL
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


e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))
e1@"_id" <- "_E1"


e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                  obs=list("Attempts"=2,"Duration"=38.3))
e2@"_id" <- "_E2"


eng$evidenceSets()$reset(list(e1,e2))

eve1 <- fetchNextMessage(eng)
stopifnot(m_id(eve1)==m_id(e1))

eve1 <- markAsProcessed(eng,eve1)
stopifnot(eve1@processed)

stopifnot(length(evidence(eng))==1L)

eve2 <- fetchNextMessage(eng)
stopifnot(m_id(eve2)==m_id(e2))

