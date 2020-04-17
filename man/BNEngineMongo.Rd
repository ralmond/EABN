\name{BNEngineMongo}
\alias{BNEngineMongo}
\title{Creates a Bayes Net Engine attached to a Mongo database.}
\description{

  The \code{\linkS4class{BNEngineMongo}} is a
  \code{\linkS4class{BNEngine}} which is attached to a
  \code{\link[Proc4]{MongoDB-class}} database, which hold both the queue and
  the \code{\linkS4class{StudentRecordSet}}.  

}
\usage{
BNEngineMongo(app = "default", warehouse, listenerSet = NULL,
dburi = "mongodb://localhost", dbname = "EARecords", processN = Inf,
admindbname = "Proc4", waittime = 0.25, profModel = character(),
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
  \item{dburi}{A character scalar giving the login information for the
    mongo database.  See \code{\link[Proc4]{makeDBuri}}.}
  \item{dbname}{The name for the EA database.}
  \item{processN}{The number of records to process before stopping.  The
    default value \code{Inf} runs the process until the active flag is
    cleared.}
  \item{admindbname}{The name of the admin database used to check for
    shutdown requests.}
  \item{waittime}{The amout of time (in seconds) to wait before checking
    again for new evidence sets when the evidence set queue is empty.}
  \item{profModel}{The name of the proficiency model (its ID in the
    warehouse manifest).}
  \item{\dots}{Extra room in case we later think of more things we
    should add.}
}
\details{

  This creates an uninitialized \code{\linkS4class{BNEngine}},
  specifically a \code{\linkS4class{BNEngineMongo}}.

  The \code{\link[Proc4]{makeDBuri}} function provides a useful
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

  The database connections are not created right away, so it
  is important to use the class-based functions, \code{manifestdb()},
  \code{statdb()}, \code{evidenceSets()}, \code{histNodesdb()},
  \code{studentRecords()}, and \code{admindb()} rather than accessing
  the fields directly.

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
app <- "ecd://epls.coe.fsu.edu/EITest"
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


eng <- BNEngineMongo(app=app,warehouse=Nethouse,
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
