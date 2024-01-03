\name{EABN-package}
\alias{EABN-package}
\alias{EABN}
\docType{package}
\title{
\packageTitle{EABN}
}
\description{
\packageDescription{EABN}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{EABN}
\packageIndices{EABN}


The most important object in the package is the
\code{\linkS4class{BNEngine}} which does most of the work of scoring.
In particular, it takes a \code{\link[Proc4]{P4Message}} object
containing observables, and a \code{\linkS4class{StudentRecord}} object
and updates the student record.

It comes in two variants, \code{\linkS4class{BNEngineMongo}} which links
to a Mongo database, and \code{\linkS4class{BNEngineNDB}} which
processes raw messages without the database.


The functions \code{\link{doBuild}} builds the
\code{\linkS4class{BNEngine}}, and the function \code{\link{doRunrun}}
runs the engine on a queue of messages in the database.  The function
\code{\link{handleEvidence}} processes a single evidence message.


}
\author{
\packageAuthor{EABN}

Maintainer: \packageMaintainer{EABN}
}
\references{


  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapters
  5 and 13.


}
\keyword{ package }
\concept{ Bayesian Network }
\concept{ Scoring Engine }
\seealso{
   \code{\link[Proc4:Proc4-package]{Proc4}} -- Low level support for
   messaging.
   \code{\link[EIEvent:EIEvent-package]{EIEvent}} -- Evidence
   Accumulation which produces the input messages for \code{EABN}.
}
\examples{
cat("This sample file is available in", system.file("conf/RunEABN.R",
   package="EABN"), "\n")

\dontrun{
library(R.utils)
library(EABN)
library(PNetica)
library(futile.logger)
library(jsonlite)


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

## This is the default location for INI files for Proc4 tools.
source("/usr/local/share/Proc4/EAini.R")


## Assumes the path config.dir (set in the INI file) contains a file
## config.json giving the location of the necessary configuration files
## and network files.
EA.config <- jsonlite::fromJSON(file.path(config.dir,"config.json"),FALSE)

app <- as.character(Proc4.config$apps[appStem])
if (length(app)==0L || any(app=="NULL")) {
  stop("Could not find app for ",appStem)
}
if (!(isTRUE(match(appStem,EA.config$appStem))) {
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

## This will build the engine and run all messages in the QUEUE.


eng <- doRunrun(app,sess,EA.config,EAeng.local,config.dir,outdir,
                logfile=logfile,override=override,noprep=noprep)

## The engine object can now be used to process further messages or
## access the EABN database.

}
}
