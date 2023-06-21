library(R.utils)
library(EABN)

appStem <- cmdArg("app",NULL)
if (FALSE) {
  appStem <- "userControl"
  appStem <- "linear"
  appStem <- "adaptive"
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

rebuildOutputs(app,EA.config,EAeng.local,outdir)

