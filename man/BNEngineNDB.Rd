\name{BNEngineNDB}
\alias{BNEngineNDB}
\alias{newBNEngineNDB}
\title{Creates a Bayes net engine not attached to a database.}
\description{

  The \code{\linkS4class{BNEngineNDB}} is a
  \code{\linkS4class{BNEngine}} which is not attached to the database.
  In particular, it cannot store student records, so it cannot maintain
  state between scoring sessions without extenal help.


}
\usage{
newBNEngineNDB(app = "default", warehouse, listenerSet = NULL,
  manifest = data.frame(), processN = Inf, waittime = 0.25,
  profModel = character(), statmat = data.frame(),
  evidenceQueue = new("ListQueue",app, list()),
  activeTest = "EAActive",
  errorRestart=c("checkNoScore","stopProcessing","scoreAvailable"),
  srs =StudentRecordSet(app=app,warehouse=warehouse, db=MongoDB(noMongo=TRUE)),
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
  \item{manifest}{A data frame providing a manifest for the
    \code{\link[Peanut]{PnetWarehouse}}.}
  \item{processN}{The number of records to process before stopping.  The
    default value \code{Inf} runs the process until the active flag is
    cleared.}
  \item{waittime}{The amout of time (in seconds) to wait before checking
    again for new evidence sets when the evidence set queue is empty.}
  \item{profModel}{The name of the proficiency model (its ID in the
    warehouse manifest).}
  \item{statmat}{A data.frame describing the statistics.  See
    \code{\link{configStats}}.}
  \item{evidenceQueue}{A object of class \code{\link[Proc4]{MessageQueue-class}}
    containing evidence sets to be processed.}
  \item{activeTest}{The pathname for the file whose existance will be
    used to determine when the engine should shut down.}
  \item{errorRestart}{A character scalar describing how to handle
    errors.  The default, "checkNoScore" will continue scoreing to try
    to find additional errors, but will not report statistics; the
    "scoreAvailable" option reports the scores based on the evidence
    sets which do not produce errors.  The "stopProcessing" option
    immediately stops processing.}
  \item{srs}{A \code{\linkS4class{StudentRecordSet}} object used to
    manage student records.}
  \item{\dots}{Extra arguments are ignored.  This allows arguments for
    other engine versions to be set in the parameters and ignored.}
}
\details{

  This creates an uninitialized \code{\linkS4class{BNEngine}},
  specifically a \code{\linkS4class{BNEngineNDB}}.

}
\value{

  An object of calls \code{\linkS4class{BNEngineNDB}} which is capable
  of scoring student models.

}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapter
  13.


}
\author{Russell Almond}
\seealso{

  Classes:
  \code{\linkS4class{BNEngine}}, \code{\linkS4class{BNEngineMongo}}

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
## Requires database setup, also PNetica
library(RNetica)  ## Must load to setup Netica DLL
appid <- "ecd://epls.coe.fsu.edu/EITest"
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

ls <- ListenerSet(sender= paste("EAEngine[",appid,"]"),
                  listeners=listeners)

eng <- newBNEngineNDB(app=appid,warehouse=Nethouse,
                     listenerSet=ls,manifest=netman,
                     profModel="miniPP_CM",
                     histNodes="Physics",
                     statmat=stattab,
                     activeTest="EAActive.txt")

## Standard initialization methods.
loadManifest(eng,netman)
eng$setHistNodes("Physics")
configStats(eng,stattab)
setupDefaultSR(eng)

%PNetica%}

}
\keyword{ graphs }
\keyword{ interface }
