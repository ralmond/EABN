\name{doRunrun}
\alias{doRunrun}
\title{This runs the Evidence Accumulation Bayes net engine to scor or
  rescore an assessment.}
\description{

  This is a system to run the Bayes net scoring engine, taking most of
  the details from a configuration file.  It creates the
  \code{\linkS4class{BNEngine}}
  instance then and then runs it in either scoring or rescoring mode.
  Configuration information in taken from the \code{EA.config} and
  \code{EAeng.local} parameters.

}
\usage{
doRunrun(appid, sess, EA.config, EAeng.local, config.dir,
outdir=config.dir, override = FALSE, logfile="", noprep=FALSE)
}
\arguments{
  \item{appid}{A character string giving the global unique identifier for
    the application being run.  This is normally formatted like a URL,
    and \code{basename(app)} is used as a short name.}
  \item{sess}{A \code{\link[RNetica]{NeticaSession} object to use for
      the Netica link.}}
  \item{EA.config}{A named list containing the configuration details.  See the
    \sQuote{Configuration} section below.}
  \item{EAeng.local}{A named list containing additional parameters for the
    engine constructor.  The intention that these are local
    configuraiton paramete (e.g., database names and passwords) as
    opposed to more global information.  Note this must have an element
    named \dQuote{dburi} which gives the URI for the database, or which
    should be blank if the no database engine is to be used.}
  \item{config.dir}{The pathname of the directory that contains the
    the nets subdirectories.}
  \item{outdir}{The pathname of the directory to which output files
    will be written.}
  \item{override}{A logical flag.  If true, the code will ignore locks
    and restart the run anyway.}
  \item{logfile}{Name for the file in which to do logging.}
  \item{noprep}{Logical flag.  If true, then the database and listener
    preparation steps will be skipped.  This is for forcing a
    continuation without resetting the configuration.}
}
\details{

  The goal is to start a run for scoring (evidence accumultion step) an
  assessment using the  \code{\linkS4class{BNEngine}} class.  This
  function takes care of many
  of the configuration details and preparatory steps and then calls
  \code{\link{mainLoop}} to do the major work.  In particular, the steps
  done by this system are as follows:

  \enumerate{
    \item{Configure the listeners.}
    \item{Configure the engine, including loading manifest and scoring
      list.}
    \item{Clean old scores from the database (optional depending on
      configuration.)}
    \item{Remove selected evidence sets from the collection. Import new
      evidence sets into the database and mark selected evidence as
      unprocessed.}
    \item{Launch engine using \code{\link{mainLoop}}.}
    \item{Build and register the statistics and history file.}
  }

  Note that this will run in either rerun mode, where it will score an
  selection of existing records and stop, or in server mode where it
  will continue waiting for new messages until it gets a shut down signal.

}
\section{Configuration}{

  There are a large number of parameters which can be configured.  These
  are passed in through the \code{EA.config} argument, which is a list
  of parameters.  The intention is that this can be read in from a JSON
  file (using \code{\link[jsonlite]{fromJSON}}).  The \code{RunEABN.R}
  script loads these from a file called \code{config.json}.  A sample of
  this file is available on github \url{https://github.com/ralmond/PP-EA}.

  The following fields are available:

  \describe{

    \item{ConfigName}{An identifier for the configuration. Default value
    \code{"PP-main"}.  Documentation only, not used by \code{doRunrun}.}
    \item{Branch}{The branch name for the git branch for this
      configuraiton.  Default value \code{"PP-main"}. Documentation
      only, not used by \code{doRunrun}. }
    \item{Version}{A version number for the configuration. Documentation
      only, not used by \code{doRunrun}. }
    \item{Date}{A edit date for the configuration. Documentation only,
      not used by \code{doRunrun}. }
    \item{appStem}{A list of app stems that will be affected.  Sample
      value \code{["P4Test"]}.}
    \item{rebuildNets}{A logical flag, should the nets be rebuilt.
      Example value \code{true}.}
    \item{logLevel}{This controls the
      \code{\link[futile.logger]{flog.threshold}}. Default value
      \code{"INFO"}.  Note that \code{doRunrun} does not set the log
      value, that should be done in the calling script.}
    \item{logname}{This is the name of the file to which logs should be
      sent. Example value \code{"EA_<app>0.log"}. Note that
      \code{doRunrun} does not set the log
      file, that should be done in the calling script.}
    \item{Tables}{This is a whole object describing the \code{EA.tables}
      field see \code{\link{doBuild}}.}
    \item{sender}{The sender field on output messages.  Example value
      \code{"EA_<app>"}.}
    \item{lscolname}{The name of the column to which the listener set
      should log messages. Example value \code{"Messages"}.}
    \item{listeners}{This is a list of listener descriptions. See the
      section \sQuote{Listner Configuration} below.}
    \item{SRreset}{Logical value, should the student records be reset
      before running.  Example value \code{true}.}
    \item{listenerReset}{Which listeners should be reset before running.
     This should be a character scalar or vector.  The values should be
     names of listeners.  The special value \dQuote{Self} refers to the
     ListenerSet object, and the special value \dQuote{ALL} resets all
     listeners.  See \code{\link[Proc4]{resetListeners}}.  Example value
     \code{"ALL"}.}
    \item{netdir}{The name of the subdirectory of \code{config.dir}
      which contains the nets.   Default value \code{"nets"}.}
    \item{EAEngine}{A complex object describing engine parameters. See
      the section \sQuote{Engine Configuration} below.}
    \item{filter}{A complex object describing how to prefilter the
      database.  See the section \sQuote{Database Filters} below.}
    \item{extensions}{This should be a list of paths (relative to
      \code{config.dir}) containing additional R code to load.  This is
      not used by \code{doRunrun}, but is supplied for use in scripts
      that might use \code{doRunrun}.}
    \item{limitNN}{An integer: how many events should be processed.
      Two special string values are also accepted.  \dQuote{ALL} will
      process all records currently in the database and stop.
      \dQuote{Inf} will cause the process to run in server mode until it
      is shut down.}
    \item{listenerExports}{Information about data tables which should be
      exported at the end of the run.  See
      \code{\link[Proc4]{generateListenerExports}}.}
  }

  A number of these values do \dQuote{<app>} substitution, that is they
  will substitute the string \dQuote{<app>} for the short name of the
  application.

}
\section{Listener Configruation}{

  The listeners consist of a \code{\link[Proc4]{ListenerSet}} and a
  collection of \code{\link[Proc4]{Listener}} objects.  The listener
  objects are made by using the information from the \dQuote{listeners}
  element of the \code{EA.config} argument.  This should be a list of
  specifications (each specification itself is a list).  These are
  passed to \code{\link[Proc4]{buildListener}}, which provides some
  examples.  The \dQuote{listenerExports} part of the configuration is used
  to call \code{\link[Proc4]{generateListenerExports}} when the engine stops.

  The listener set is controlled by the \code{EAeng.local$dburi} value
  and the \dQuote{lscolname} field.  If \code{dbuir} is a name of a
  database, then the \code{\link[Proc4]{ListenerSet}} is logged into the
  \dQuote{lscolname} collection.  If \code{dburi} is null or an empty
  string, then the listener set will not do logging.

}
\section{Engine Configruation}{

  The type of engine used is controlled by the \code{EAeng.local$dburi}
  value.  If this is a URI, then the \code{\linkS4class{BNEngineMongo}}
  class is used.  If it is null or the empty string, then the
  \code{\linkS4class{BNEngineNDB}} class is used instead.

  The arguments to the appropriate constructor are found between the
  \code{EAeng.local} and \code{EA.config$EAEngine} collections.  The
  intent is for the former to include details (e.g., database user names
  and passwords) which are local to the server on which \code{EABN} is
  running, and for \code{EA.config$EAEngine} to include more public
  details which are local to a particular run.

  See \code{\link{BNEngineMongo}} or \code{\link{BNEngineNDB}} for the
  expected fields.  Note that the \dQuote{processN} field is taken care
  of separately after the database operations (next section).

}
\section{Database Filtering}{

  The \code{EA.config$filter} field controls the database filtering
  process.  There are four steps:

  \describe{
    \item{Remove}{old records from the database.}
    \item{Import}{new records into the database.}
    \item{Purge}{unused records from the database.}
    \item{Reprocess}{Reset the processed flag to ensure records get
      reprocessed.}
  }

  These are controlled by the following elements in the
  \code{EA.config$filter} list:
  \describe{
    \item{doRemove}{Logical, should records be removed before import.}
    \item{remove}{Filter to use for removal. The value \code{\{\}}
      will remove all records for the given \code{app}.}
    \item{importFile}{A list of filenames (in the \code{config.dir})
      which contain evidence sets to be imported before scoring.}
    \item{doPurge}{Logical, should records be removed after
      import.}
    \item{purge}{Filter for the purging (after import removal).
      Leaving this empty will probably not be satisfactory.}
    \item{doReprocess}{Logical, should existing records have the processed
      flag cleared?  Typically \code{TRUE} for rerun mode and
      \code{FALSE} for server mode.}
    \item{reprocess}{Filter for the selected records to be marked
      for reprocessing.  The value \code{\{\}} will mark all records
      (for this \code{app}) for reprocessing.}
  }

}
\section{Locking}{

  It is probably a bad idea to rebuild the nets which a different
  incarnation is using the net directory to score.  It is almost
  certainly a bad idea for two different programs to rebuild the nets in
  the same directory at the same time.

  To prevent such clashes, the \code{\link{doRunrun}} function adds a
  file with the extension \code{.lock} to the directory when it is
  scoring.  The \code{doBuild} function adds the file
  \code{netbuilder.lock} while it is rebuilding the nets.

  If when \code{doBuild} starts, if a \code{.lock} file is found in the
  \dQuote{nets} directory, it issues an warning, and unless the
  \code{override} parameter is set to \code{TRUE} it stops.  Use the
  override only with extreme caution.

  The \code{\link{BNEngineMongo}} version also checks the database for a
  running flag.  If it is found, then again the engine will not start
  unless the \code{override} flag is true.

}
\section{Data Files}{

  If the value of \code{EA.config$statListener} is not null, then the
  final statistic values for all users are put into a table which is
  exported (to the file \code{EA.config$statfile}).

  If the value of \code{EA.config$histListener} is not null, then the
  history of all statistic values for all users are put into a table
  which is exported (to the file \code{EA.config$histfile}).

  Both the statfile and histfile are registered using the
  \code{\link[Proc4]{ListenerSet}$registerOutput} method.

}
\section{Logging}{

  Logging is done through the \code{\link{futile.logger}{flog.logger}}
  mechanism. This allows logs to be save to a file.

  The \dQuote{logLevel} and \dQuote{logname} fields are put in the
  configuration specification to assist scripts in configuring the
  logging system.

  Both the log file is registered using the
  \code{\link[Proc4]{ListenerSet}$registerOutput} method.

}

\value{

  This returns the engine invisibly, in case the calling program wants
  to do something with it.

}
\references{

  The Bobs (1983) Psychokiller.  \emph{My I'm Large.}  Rhino Records.
  \url{https://www.youtube.com/watch?v=-Gu4PKnCLDg}.  (Reference is
  about 2:30 minutes into song.)

}
\author{Russell Almond}
\note{

  This function is meant to be called by the \code{RunEABN.R} script
  found in the config directory.
  (\code{file.path(help(package="EABN")$path,"conf","RunEABN.R")})

  The shell script \code{EABN} found in the same directory will run this
  script.

}
\seealso{

  \code{\linkS4class{BNEngine}}, \code{\link{mainLoop}},   \code{\link{doBuild}}

  \code{\link[Proc4]{resetProcessedMessages}},
  \code{\link[Proc4]{cleanMessageQueue}},
  \code{\link[Proc4]{importMessages}}

  \code{\link[Proc4]{ListenerSet}},   \code{\link[Proc4]{buildListenerSet}},
  \code{\link[Proc4]{generateListenerExports}},
  \code{\link[Proc4]{resetListeners}}



}
\examples{
## This example is in:
file.path(help(package="EABN")$path,"conf","RunEABN.R")
\dontrun{
library(R.utils)
library(EABN)
library(PNetica)

appStem <- cmdArg("app",NULL)
if (FALSE) {
  appStem <- "userControl"
}

source("/usr/local/share/Proc4/EAini.R")

EA.config <- jsonlite::fromJSON(file.path(config.dir,"config.json"),FALSE)

app <- as.character(Proc4.config$apps[appStem])
if (length(app)==0L || any(app=="NULL")) {
  stop("Could not find app for ",appStem)
}

## Start Netica
sess <- NeticaSession(LicenseKey=NeticaLicenseKey)
startSession(sess)

logfile <- (file.path(logpath, sub("<app>",appStem,EA.config$logname)))
if (interactive()) {
  futile.logger::flog.appender(appender.tee(logfile))
} else {
  futile.logger::flog.appender(appender.file(logfile))
}
futile.logger::flog.threshold(EA.config$logLevel)

eng <- doRunrun(app,sess,EA.config,EAeng.local,config.dir,outdir,
                logfile=logfile)

}
}
\keyword{ interface }
\keyword{ graphs }
\keyword{ database }
