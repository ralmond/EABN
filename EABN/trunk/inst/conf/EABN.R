library(R.utils)
library(EABN)

if (interactive()) {
  app <- "ecd://epls.coe.fsu.edu/P4test"
  loglevel <- "DEBUG"
  cleanFirst <- TRUE
  evidenceFile <- "/home1/ralmond/ownCloud/Projects/NSFCyberlearning/FSUSSp2019Data/EvidenceSets.Sp2019.json"
  evidenceFileb <- "/home1/ralmond/ownCloud/Projects/NSFCyberlearning/FSUSSp2019Data/EvidenceSets.Sp2019b.json"
  evidenceFile <- NULL
} else {
  app <- cmdArg("app",NULL)
  if (is.null(app) || !grepl("^ecd://",app))
    stop("No app specified, use '--args app=ecd://...'")
  loglevel <- cmdArg("level","INFO")
  cleanFirst <- as.logical(cmdArg("clean",FALSE))
  evidenceFile <- cmdArg("evidence",NULL)

}

source("/usr/local/share/Proc4/EAini.R")

if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(loglevel)

sess <- NeticaSession(LicenseKey=NeticaLicenseKey)
startSession(sess)

listeners <- lapply(names(EA.listenerSpecs),
                    function (ll) do.call(ll,EA.listenerSpecs[[ll]]))
names(listeners) <- names(EA.listenerSpecs)
if (interactive()) {
  cl <- new("CaptureListener")
  listeners <- c(listeners,cl=cl)
}

eng <- do.call(BNEngine,c(EAeng.params,
                          list(session=sess,listeners=listeners,
                               netDirectory=config.dir),
                          EAeng.common))
loadManifest(eng,read.csv(file.path(config.dir,manifestFile),
                          stringsAsFactors=FALSE))
configStats(eng,read.csv(file.path(config.dir,statFile),
                          stringsAsFactors=FALSE))

if (cleanFirst) {
  eng$evidenceSets()$remove(buildJQuery(app=app(eng)))
  eng$studentRecords()$clearAll(TRUE)   #Clear default, as we will set
                                        #it back up in a moment.
  eng$listenerSet$messdb()$remove(buildJQuery(app=app(eng)))
  for (lis in eng$listenerSet$listeners) {
    if (is(lis,"UpdateListener") || is(lis,"InjectionListener"))
      lis$messdb()$remove(buildJQuery(app=app(eng)))
  }

}
setupDefaultSR(eng)

if (!is.null(evidenceFile)) {
  system2("mongoimport",
          sprintf('-d %s -c EvidenceSets --jsonArray', eng$dbname),
          stdin=evidenceFile)
  NN <- eng$evidenceSets()$count(buildJQuery(app=app(eng),processed=FALSE))
}

if (interactive() && !is.null(evidenceFile)) {
  eng$processN <- NN
}
cat("Processing ",NN," records for application ",app(eng),".\n")


## Activate engine (if not already activated.)
eng$activate()
mainLoop(eng)

## This is for running the loop by hand.
if (interactive() && FALSE) {
  eve <- eng$fetchNextEvidence()
  rec1 <- handleEvidence(eng,eve)
  eng$setProcessed(eve)
}

cat("[",file="out/c081c3.srs.json")
for (n in 1:NN) {
  eve <- eng$fetchNextEvidence()
  flog.info("Level %d: %s",n,context(eve))
  sr <- getRecordForUser(eng,uid(eve))
  rec1 <- handleEvidence(eng,eve)
  WriteNetworks(sm(sr),sprintf("out/Net%s-%s-%d.dne",uid(eve),context(eve),n))
  cat(as.json(sr),",",file="out/c081c3.srs.json",append=TRUE)
  eng$setProcessed(eve)
}
cat("{}]",file="out/c081c3.srs.json",append=TRUE)
