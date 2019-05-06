library(EABN)
flog.threshold(DEBUG)

testdir <- "/home/ralmond/Projects/EABN/inst"
mantab <- read.csv(file.path(testdir,"sampleAssessment",
                             "PPsubsetManifest.csv"),as.is=TRUE)
rownames(mantab) <- mantab$Name
mantab$Pathname <- file.path(testdir,"sampleAssessment",mantab$Pathname)



stattab <- read.csv(file.path(testdir,"sampleAssessment",
                             "StatisticList.csv"),as.is=TRUE)

tapp <- "ecd://epls.coe.fsu.edu/P4test"

source("~/.Netica.R")
sess <- NeticaSession(LicenseKey=NeticaLicenseKey)
startSession(sess)

cl <- new("CaptureListener")
ul <- UpdateListener(dbname="Proc4",dburi="mongodb://localhost",
            colname="Statistics",targetField="data",
            messSet=c("Statistics")
            jsonEncoder="stats2json"))



EAeng <- BNEngine(tapp,sess,list(cl,ul))
loadManifest(EAeng)
configStats(EAeng)

loadManifest(EAeng,mantab)
configStats(EAeng,stattab)

setupDefaultSR(EAeng)

srs<- EAeng$studentRecords()

fsr <- getSR(srs,"Phred")

