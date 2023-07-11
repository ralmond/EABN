\name{BNEngineMongo}
\alias{BNEngineMongo}
\alias{newBNEngineMongo}
\title{Creates a Bayes Net Engine attached to a Mongo database.}
\description{

  The \code{\linkS4class{BNEngineMongo}} is a
  \code{\linkS4class{BNEngine}} which is attached to a
  \code{\link[mongo:MongoDB-class]{MongoDB}} database, which hold both the queue and
  the \code{\linkS4class{StudentRecordSet}}.

}
\usage{
newBNEngineMongo(app = "default", warehouse, listenerSet = NULL, processN = Inf,
    statistics = list(),
    dburi = "mongodb://localhost", sslops = mongolite::ssl_options(),
    eadbname = "EARecords", admindbname = "Proc4", waittime = 0.25,
    profModel = character(), histNodes = character(),
    errorRestart = c("checkNoScore", "stopProcessing", "scoreAvailable"),
    srcol = "StudentRecords",
    mongoverbose = FALSE,
    srs = StudentRecordSet(app = app, warehouse = warehouse,
        db = MongoDB(srcol, eadbname, dburi, verbose = mongoverbose,
            options = sslops)),
    manifestCol = "Manifest", manifestDB = MongoDB(manifestCol,
        eadbname, dburi, verbose = mongoverbose, options = sslops),
    evidenceCol = "EvidenceSets", evidenceQueue = new("MongoQueue",
        app = app, messDB = MongoDB(evidenceCol, eadbname, dburi,
        verbose = mongoverbose, options = sslops), builder = Proc4::buildMessage),
    histcol = "histNodes", histNodesDB = MongoDB(histcol, eadbname,
        dburi, verbose = mongoverbose, options = sslops),
    statcol = "Statistics",
    statDB = MongoDB(statcol, eadbname, dburi, verbose = mongoverbose,
        options = sslops),
    admincol = "AuthorizedApps", adminDB = MongoDB(admincol,
        admindbname, dburi, verbose = mongoverbose, options = sslops),
    ...)
}
\arguments{
  \item{app}{A character scalar giving the globally unique identifier
    for the application.}
  \item{warehouse}{A \code{\link[Peanut]{PnetWarehouse}} which stores
    the default student model and evidence models.  (It will also store
    the student models.}
  \item{listenerSet}{A \code{\link[Proc4]{ListenerSet}} which contains the
    listeners for clients of the engine's messages.}
  \item{statistics}{Object of class \code{list} containing
      \code{\link[Peanut]{Statistic}} objects to be run on every update
      cycle.}
  \item{dburi}{A character scalar giving the login information for the
    mongo database.  See \code{\link[mongo]{makeDBuri}}.}
  \item{sslops}{Options for SSL connections to database.  See \code{\link[mongolite]{ssl_options}}.}
  \item{eadbname}{The name for the EA database.}
  \item{admindbname}{The name of the admin database used to check for
    shutdown requests.}
  \item{processN}{The number of records to process before stopping.  The
    default value \code{Inf} runs the process until the active flag is
    cleared.}
  \item{waittime}{The amout of time (in seconds) to wait before checking
    again for new evidence sets when the evidence set queue is empty.}
  \item{profModel}{The name of the proficiency model (its ID in the
    warehouse manifest).}
  \item{histNodes}{A character vector giving the names of the nodes for which history will automaticall be recorded.}
  \item{errorRestart}{A character scalar describing how to handle
    errors.  The default, "checkNoScore" will continue scoreing to try
    to find additional errors, but will not report statistics; the
    "scoreAvailable" option reports the scores based on the evidence
    sets which do not produce errors.  The "stopProcessing" option
    immediately stops processing.}
  \item{srcol}{A character scalar giving the name of the database backing the student record set.  Ignored if       \code{srs} is specified.}
  \item{mongoverbose}{A flag.  If true, extra debugging information from database calls is generated.}
  \item{srs}{A \code{\linkS4class{StudentRecordSet}} object for storing the student records.}
  \item{manifestCol}{The name of the column containing the manifest data, ignored if \code{manifestDB} is supplied.}
  \item{manifestDB}{A \code{\link[mongo]{JSONDB}} the database where manifest information is cached.}
  \item{evidenceCol}{The name of the column containing the evidence sets, ignored if \code{evidenceQueue} is supplied.}
  \item{evidenceQueue}{A \code{\link[Proc4:MessageQueue-class]{MessageQueue}} where the evidence sets exist.}
  \item{histcol}{The name of the column into which history data should be stored, ignored if \code{histNodesDB} is supplied.}
  \item{histNodesDB}{A \code{\link[mongo]{JSONDB}} database where history information is stored.}
  \item{statcol}{The name of the column into which statistics should be stored, ignored if \code{statDB} is supplied.}
  \item{statDB}{A \code{\link[mongo]{JSONDB}} database where statistics are stored.}
  \item{admincol}{The name of the column in the administrative database where engine status information is stored, ignored if \code{adminDB} is supplied.}
  \item{adminDB}{A \code{\link[mongo]{JSONDB}} where status information about the engine is stored.}
  \item{\dots}{Extra arguments are ignored.  This allows arguments for
    other engine versions to be set in the parameters and ignored.}
}
\details{

  This creates an uninitialized \code{\linkS4class{BNEngine}},
  specifically a \code{\linkS4class{BNEngineMongo}}.

  The \code{app}, \code{warehouse}, and \code{listenerSet} arguments need to be supplied,
  for most of the rest, the default arguments work.

  In particular, most of the \dQuote{db} arguments are built using the default arguments.
  The \code{\link[mongo]{makeDBuri}} function provides a useful
  shorthand for calculating the \code{dburi} field.

}
\value{

  An object of calls \code{\linkS4class{BNEngineMongo}} which is capable
  of scoring student models.

}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapter
  13.


}
\author{Russell Almond}
\note{

  Much of this information comes from the \dQuote{config.json} file, with the \code{dburi}, \code{eadbname},
  \code{admindbname}, and \code{sslops} arguments come from the \dQuote{EA.ini} file.

}
\seealso{

    Classes:
  \code{\linkS4class{BNEngine}}, \code{\linkS4class{BNEngineNDB}}

  Constituent parts:
  \code{\link{StudentRecordSet}}, \code{\link[Peanut]{PnetWarehouse}}
  \code{\link[Proc4]{ListenerSet}}

  Setup Functions:
  \code{\link{loadManifest}},
  \code{\link{setupDefaultSR}},
  \code{\link{configStats}},
  \code{\link{baselineHist}},

  Main Loop Functions:
  \code{\link{mainLoop}},
  \code{\link{accumulateEvidence}},
  \code{\link{handleEvidence}},
  \code{\link{getRecordForUser}},
  \code{\link{logEvidence}},
  \code{\link{updateSM}},
  \code{\link{updateStats}},
  \code{\link{updateHist}},
  \code{\link{announceStats}},

}
\examples{

%PNetica%\dontrun{
\dontrun{%Mongo%
## Requires database setup, also PNetica
library(RNetica)  ## Must load to setup Netica DLL
app <- "ecd://epls.coe.fsu.edu/EATest"
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)

config.dir <- file.path(library(help="Peanut")$path, "auxdata")
net.dir <- file.path(library(help="PNetica")$path,"testnets")

netman <- read.csv(file.path(config.dir, "Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
stattab <- read.csv(file.path(config.dir, "Mini-PP-Statistics.csv"),
                    as.is=TRUE)

Nethouse <- PNetica::BNWarehouse(netman,session=sess,
             address=net.dir)

cl <- new("CaptureListener")
listeners <- list("cl"=cl)
ls <- ListenerSet(sender= paste("EAEngine[",basename(app),"]"),
                  dbname="EARecords", dburi=makeDBuri(host="localhost"),
                  listeners=listeners,
                  colname="Messages")


eng <- newBNEngineMongo(app=app,warehouse=Nethouse,
                     listenerSet=ls,
                     dburi=makeDBuri(host="localhost"),
                     dbname="EARecords",profModel="miniPP_CM",
                     histNodes="Physics")

## Standard initialization methods.
loadManifest(eng,netman)
eng$setHistNodes("Physics")
configStats(eng,stattab)
setupDefaultSR(eng)

}%Mongo%
%PNetica%}

}
\keyword{ graphs }
\keyword{ interface }
