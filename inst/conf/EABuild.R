library(R.utils)
library(EABN)
library(PNetica)

source("/usr/local/share/Proc4/EAini.R")

EA.config <- fromJSON(file.path(config.dir,"config.json"),FALSE)

logfile <- (file.path(logpath, sub("<app>","Builder",EA.config$logname)))
if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(EA.config$loglevel)

appStem <- as.character(EA.config$appStem)

apps <- as.character(Proc4.config$apps[appStem])
if (length(apps)==0L || any(apps=="NULL")) {
  flog.error("Could not find apps for ",paste(appStem,collapse=", "))
  stop("Could not find apps for ",paste(appStem,collapse=", "))
}


sess <- NeticaSession(LicenseKey=NeticaLicenseKey)
startSession(sess)

## <<HERE>> Need to load extensions.

doBuild(sess, EA.config$Tables,config.dir)
