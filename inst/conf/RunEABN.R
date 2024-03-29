library(R.utils)
library(EABN)
library(PNetica)

if (interactive()) {
  ## Edit these for the local application
  appStem <- "P4test"
  loglevel <- ""
  noprep <- FALSE
  override <- FALSE
} else {
  appStem <- cmdArg("app",NULL)
  if (is.null(app) || !grepl("^ecd://",app))
    stop("No app specified, use '--args app=ecd://...'")
  loglevel <- cmdArg("level","")
  noprep <- as.logical(cmdArg("noprep",FALSE))
  override <- as.logical(cmdArg("override",FALSE))
}

source("/usr/local/share/Proc4/EAini.R")

EA.config <- fromJSON(file.path(config.dir,"config.json"),FALSE)

app <- as.character(Proc4.config$apps[appStem])
if (length(app)==0L || any(app=="NULL")) {
  stop("Could not find app for ",appStem)
}
if (!(appStem %in% EA.config$appStem)) {
  stop("Configuration not set for app ",appStem)
}

## Start Netica
sess <- NeticaSession(LicenseKey=NeticaLicenseKey)
startSession(sess)

logfile <- (file.path(logpath, sub("<app>",appStem,EA.config$logname)))
if (interactive()) {
  flog.appender(appender.tee(logfile))
} else {
  flog.appender(appender.file(logfile))
}
flog.threshold(EA.config$logLevel)

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

eng <- doRunrun(app,sess,EA.config,EAeng.local,config.dir,outdir,
                logfile=logfile,override=override,noprep=noprep)

