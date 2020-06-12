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
doRunrun(app, sess, EA.config, EAeng.local, config.dir, outdir=config.dir, override = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{app}{A character string giving the global unique identifier for
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
}
\details{

  The goal is to start a run or scoring an assessment using the 
  \code{\linkS4class{BNEngine}} class.  This function takes care of many
  of the configuration details and preparatory steps and then calls
  \code{\link{mainloop}} to do the major work.  In particular, the steps
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
    \item{Build a statistics file.}
  }

  Note that this will run in either rerun mode, where it will score an
  selection of existing records and stop, or in server mode where it
  will continue waiting for new messages until it gets a shut down signal.

}
\section{Configuration}{

  There are a large number of parameters which can be configured.  These
  are passed in through the \code{EA.tables} argument, which is a list
  of parameters.  The intention is that this can be read in from a JSON
  file (using \code{\link[jsonlite]{fromJSON}}).  In the current
  implementation, the \code{EA.tables} parameter set is a sub-object of
  the larger \code{EA.config} parameter set.

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
     listeners.  See \code{\link[Proc4]{resetLiseners}}.  Example value
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
    \item{statListener}{The name of the listener which should be
      consulted to gather the Stat table afterwards. Example value
      \code{"PPStats"}.} 
    \item{statfile}{Name of the file, relative to \code{outdir} where
      the statistcs file should be exported. Example value
      \code{"stats-<app>.csv"}.}
  }

  A number of these values do \dQuote{<app>} substitution, that is they
  will substitute the string \dQuote{<app>} for the short name of the
  application. 
  
}
\section{Listener Configruation}{

  The listeners consist of a \code{\link[Proc4]{ListenerSet}} and a
  collection of \code{\link[Proc4]{Listener}} objects.  The listener
  objects are made by using the information from the \dQuote{listners}
  element of the \code{EA.config} argument.  This should be a list of
  specifications (each specification itself is a list).  These are
  passed to \code{\link[Proc4]{buildListener}}, which provides some
  examples.

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
  \code{EA.config$filer} list:
  \describe{
    \item{doRemove}{Logical, should records be removed before import.}
    \item{remove}{Filter to use for removal. The value \code{\{\}}
      will remove all records for the given \code{app}.}
    \item{importFile}{A list of filenames (in the \code{config.dir}
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

  The values of the \code{}
  <<HERE>>

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

}
\section{Logging}{

  Logging is done through the \code{\link{futile.logger}{flog.logger}}
  mechanism. This allows logs to be save to a file.

  The \dQuote{logLevel} and \dQuote{logname} fields are put in the
    configuration specification to assist scripts in configuring the
    logging system.

}
\value{

  Good question, what does it return?
  
}
\references{


  The Bobs (1983) Psychokiller.  \emph{My I'm Large.}  Rhino Records.
  \url{https://www.youtube.com/watch?v=-Gu4PKnCLDg}.  (Reference is
  about 2:30 minutes into song.)

}
\author{Russell Almond}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (app, sess, EA.config, EAeng.local, config.dir, override = FALSE) 
{
    sapp <- basename(app)
    flog.info("Building and configuring engine.")
    listeners <- lapply(EA.config$listeners, buildListener, app, 
        dburi)
    names(listeners) <- sapply(listeners, listenerName)
    EAeng.params <- c(EA.config$EAEngine, EAeng.local)
    EAeng.params$listenerSet <- ListenerSet(sender = sub("<app>", 
        sapp, EA.config$sender), dbname = EA.config$dbname, dburi = EAeng.local$dburi, 
        listeners = listeners, colname = EA.config$lscolname)
    netman <- read.csv(file.path(config.dir, "nets", EA.config$EAEngine$manifestFile), 
        stringsAsFactors = FALSE, strip.white = TRUE)
    EAeng.params$warehouse <- PNetica::BNWarehouse(manifest = netman, 
        session = sess, key = "Name", address = file.path(config.dir, 
            "nets"))
    eng <- do.call(ifelse(EAeng.local$dburi == "", BNEngineNDB, 
        BNEngineMongo), EAeng.params)
    loadManifest(eng, netman)
    stattab <- read.csv(file.path(config.dir, "nets", EA.config$EAEngine$statFile), 
        stringsAsFactors = FALSE, strip.white = TRUE)
    configStats(eng, stattab)
    flog.info("Preparing Database.")
    if (dburi != "") {
        if (EA.config$filter$doRemove) {
            flog.debug("Clearing old evidence sets.")
            rquery <- buildJQuery(c(list(app = app, EA.config$filter$remove)))
            eng$evdienceSets()$remove(rquery)
        }
        if (isTRUE(EA.config$SRreset)) {
            flog.debug("Clearing old student records.")
            eng$studentRecords()$clearAll(TRUE)
        }
        if (!is.null(EA.config$importFile)) {
            flog.warn("Importing from file  \%s.", EA.confg$importFile)
            impf <- file.path(config.dir, EA.config$importFile)
            if (!file.exists(impf)) {
                flog.warn("File \%s does not exist, skipping import.", 
                  EA.confg$importFile)
            }
            else {
                status <- system2("mongoimport", c("--jsonArray", 
                  "-d", EAeng.local$dbname, "-c", "EvidenceSets", 
                  impf), stdout = TRUE, stderr = TRUE)
                if (!is.null(attr(status, "status"))) {
                  flog.error("Got error when loading import file.")
                  flog.error("Error:", status, capture = TRUE)
                }
            }
        }
        if (isTRUE(EA.config$filter$doReprocess)) {
            flog.debug("Clearing reprocessed flags.")
            rquery <- buildJQuery(c(list(app = app, EA.config$filter$update)))
            eng$evidenceSets()$update(rquery, "{\"$set\":{\"processed\":false}}")
        }
    }
    setupDefaultSR(eng)
    if (EA.config$limitNN == "ALL") {
        eng$processN <- eng$evidenceSets()$count(buildJQuery(app = app, 
            processed = FALSE))
    }
    else {
        eng$processN <- as.numeric(EA.config$limitNN)
    }
    flog.info("Begining EA for application \%s.", basename(app))
    if (is.finite(eng$processN)) {
        flog.info("\%d messages queued.", eng$processN)
    }
    else {
        flog.info("Running in server mode.")
    }
    if (eng$isActivated()) {
        flog.warn("Enging for application \%s already active.", 
            sapp)
        if (!isTRUE(override)) 
            stop("Application ", sapp, " already active.")
    }
    tryCatch({
        file.create(file.path(config.dir, "nets", paste(sapp, 
            "lock", sep = ".")))
        mainLoop(eng)
    }, finally = {
        file.remove(file.path(config.dir, "nets", paste(sapp, 
            "lock", sep = ".")))
    })
    if (!is.null(EA.config$statListener)) {
        sl <- listeners[[EA.config$statListener]]
        if (is.null(sl)) {
            flog.warn("Stat listener \%s not found, skipping building stat file.", 
                EA.config$statListener)
        }
        else {
            stat1 <- sl$messdb()$find(buildJQuery(app = app))
            sdat <- data.frame(stat1[, c("app", "uid", "context", 
                "timestamp")], flatten(stat1$data))
            sdat$app <- basename(sdat$app)
            fname <- gsub("<app>", sapp, EA.config$statfile)
            write.csv(stat1, fname)
        }
    }
  }
}
\keyword{ interface }
\keyword{ graphs }
\keyword{ database }
