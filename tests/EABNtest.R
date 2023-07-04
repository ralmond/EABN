library(EABN)
library(futile.logger)
flog.threshold(DEBUG)


tapp <- "ecd://epls.coe.fsu.edu/P4test"

NeticaLicenseKey <- Sys.getenv("NETICA_LICENSE_KEY")
sess <- RNetica::NeticaSession(LicenseKey=NeticaLicenseKey)
RNetica::startSession(sess)

cl <- new("CaptureListener")
ul <- UpdateListener(db=mongo::MongoDB("Statistics","Proc4"),
            targetField="data",
            messSet=c("Statistics"),
            jsonEncoder="stats2json")


if (FALSE) {
EAeng <- newBNEngine(tapp,sess,list(cl=cl,ul=ul))
loadManifest(EAeng)
configStats(EAeng)

### One time setup
if (FALSE) {
  testdir <- "/home/ralmond/Projects/EABN/inst"

  mantab <- read.csv(file.path(testdir,"sampleAssessment",
                               "PPsubsetManifest.csv"),as.is=TRUE)
  rownames(mantab) <- mantab$Name
  mantab$Pathname <- file.path(testdir,"sampleAssessment",mantab$Pathname)



  stattab <- read.csv(file.path(testdir,"sampleAssessment",
                                "StatisticList.csv"),as.is=TRUE)
  loadManifest(EAeng,mantab)
  configStats(EAeng,stattab)

  ## Configure Hist Nodes.
  hnodes <- c("Physics","Torque","Energy","LinearMomentum","ForceAndMotion")
  hnodes <- c("Physics")
  EAeng$histNodesdb()$insert(sprintf('{"app":"%s","Nodes":[%s]}',
                                     app(EAeng),
                                     paste(sapply(hnodes,function(x) sprintf('"%s"',x)),
                                           collapse=", ")))

}

setupDefaultSR(EAeng)

srs<- EAeng$studentRecords()

fsr <- getSR(srs,"Phred")
fsr0 <- getSR(srs,"Phred")

es1 <- EvidenceSet(uid="Phred",context="Around the Tree",
                   obs=list(Duration=39.5,Agent="Ramp",
                            NumberAttempts=2,
                            TrophyLevel="Gold"),
                   app=app(EAeng),mess="Observables")

es1 <- logEvidence(EAeng,fsr,es1)

fsr0a <- EABN:::updateSM(EAeng,fsr0,es1)
fsr0a <- updateStats(EAeng,fsr0a)

pstat <- EAeng$stats()$Physics_EAP

  ## manf <-WarehouseManifest(EAeng$warehouse())
  ## emName <-manf[manf$Title==context(es1),"Name"]
  ## em <- WarehouseSupply(EAeng$warehouse(),emName)
  ## obs <- AdjoinNetwork(sm(fsr),em)


fsr0a <- updateHist(EAeng,fsr0a,es1)
announceStats(EAeng,fsr0a)
fsr0a <- saveSR(EAeng$studentRecords(),fsr0a)
}
