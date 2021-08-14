library(R.utils)
library(EABN)
library(PNetica)

if (interactive()) {
  ## Edit these for the local application
  appStem <- "P4test"
  loglevel <- ""
  override <- FALSE
} else {
  appStem <- cmdArg("app",NULL)
  if (is.null(app) || !grepl("^ecd://",app))
    stop("No app specified, use '--args app=ecd://...'")
  loglevel <- cmdArg("level","")
  override <- as.logical(cmdArg("override",FALSE))
}

source("/usr/local/share/Proc4/EAini.R")

EA.config <- fromJSON(file.path(config.dir,"config.json"),FALSE)

logfile <- (file.path(logpath, sub("<app>","Builder",EA.config$logname)))
## Let command line override configuration.
if (nchar(loglevel)==OL) loglevel <- EA.config$logLevel

if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(loglevel)

appStem <- as.character(EA.config$appStem)

apps <- as.character(Proc4.config$apps[appStem])
if (length(apps)==0L || any(apps=="NULL")) {
  flog.error("Could not find apps for ",paste(appStem,collapse=", "))
  stop("Could not find apps for ",paste(appStem,collapse=", "))
}


sess <- NeticaSession(LicenseKey=NeticaLicenseKey)
startSession(sess)

## Load extensions.
for (ext in EA.config$extensions) {
  if (is.character(ext) && nchar(ext) > 0L) {
    if (file.exists(file.path(config.dir,ext))) {
      source(file.path(config.dir,ext))
    } else {
      flog.error("Can't find extension file %s.", ext)
    }
  }
}

doBuild(sess, EA.config$Tables,config.dir)
