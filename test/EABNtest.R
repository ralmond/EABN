library(EABN)
flog.threshold(DEBUG)


tapp <- "ecd://epls.coe.fsu.edu/P4test"

source("~/Netica.R")
sess <- NeticaSession(LicenseKey=NeticaLicenseKey)
startSession(sess)

cl <- new("CaptureListener")
ul <- UpdateListener(dbname="Proc4",dburi="mongodb://localhost",
            colname="Statistics",targetField="data",
            messSet=c("Statistics"),
            jsonEncoder="stats2json")



EAeng <- BNEngine(tapp,sess,list(cl=cl,ul=ul))
loadManifest(EAeng)
configStats(EAeng)

### One time setup
if (false) {
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

es1 <- EvidenceSet(uid="Phred",context="ArountTheTree",
                   obs=list(Duration=39.5,Agent="Ramp",
                            NumberAttempts=2,
                            TrophyLevel="Gold"),
                   app=app(EAeng),mess="Observables")

es1 <- logEvidence(EAeng,fsr,es1)
