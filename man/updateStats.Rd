\name{updateStats}
\alias{updateStats}
\alias{announceStats}
\title{Recalculates statistics for changed student model.}
\description{

  When the student model of a \code{\linkS4class{StudentRecord}}
  changes, because the function \code{\link{updateSM}} has been run, the
  statistics need to be recalculated.  The function \code{updateStats}
  recalculates the statistics.  The function \code{announceStats} lets
  the listeners know that new statistics are available for this user.

}
\usage{
updateStats(eng, rec, debug = 0)
announceStats(eng, rec)
}
\arguments{
  \item{eng}{A \code{\linkS4class{BNEngine}} controlling the operation.}
  \item{rec}{A \code{\linkS4class{StudentRecord}}, particularly, one that
    has just been updated via a call to \code{\link{updateSM}}.}
  \item{debug}{An integer flag.  If the value is greater than 1, there
    will be a call \code{\link[utils]{recover}} inside of the call.}
}
\details{

  The \code{\linkS4class{BNEngine}} contains a number of
  \code{\link[Peanut]{Statistic}} objects.  Every time the student model
  (\code{\link{sm}}) of the \code{\linkS4class{StudentRecord}} changes,
  the \code{\link{stats}} of the record need to be updated as well.

  The function \code{updateStats} simply loops through the statistic
  collection and calculates the new values.  The corresponding field of
  the student record is then updated.

  The function \code{announceStats} takes the new statistic values and
  generates a \code{\link[Proc4]{P4Message}} containing the new
  statistics.  This is sent to all of the \code{\link[Proc4]{Listener}}
  objects in the \code{\link[Proc4]{ListenerSet}} attached to the
  engine.

  The function \code{\link{stats}} returns the latest statistics from
  the student record.

}
\value{

  The function \code{updateStats} returns the updates
  \code{\linkS4class{StudentRecord}} object.

  The function \code{announceStats} is called for its side effects.  Its
  return value should not be used.
}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapters
  5 and 13. 


}
\author{Russell Almond}
\seealso{

  Classes:
  \code{\linkS4class{BNEngine}}, \code{\link[Proc4]{ListenerSet}}
  \code{\linkS4class{StudentRecord}}, \code{\link[Peanut]{Statistic}},
  \code{\link[Proc4]{P4Message}}

  Functions in EABN:
  \code{\link{accumulateEvidence}}, \code{\link{updateHist}},
  \code{\link{updateSM}}, \code{\link{stats}}

  Peanut Functions:
  \code{\link[Peanut]{calcStat}}

  Proc4Functions
  \code{\link[Proc4]{notifyListeners}}


}
\examples{
%PNetica%\dontrun{
## Requires PNetica
library(PNetica)  ## Must load to setup Netica DLL
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

ls <- ListenerSet(sender= paste("EAEngine[",app,"]"),
                  dburi="", listeners=listeners)

eng <- BNEngineNDB(app=app,warehouse=Nethouse,
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

sr0 <- getRecordForUser(eng,"S1")

eap0 <- stat(sr0,"Physics_EAP")

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))


sr1 <- updateRecord(sr0,e1)
sr1 <- updateSM(eng,sr1,e1)
sr1 <- updateStats(eng,sr1)
eap1 <- stat(sr1,"Physics_EAP")

## This should have changed.
stopifnot(abs(eap1-eap0) > .001)

announceStats(eng,sr1)
## Look at the resulting message.
cl$lastMessage()
details(cl$lastMessage())
stopifnot(uid(cl$lastMessage())=="S1",context(cl$lastMessage())=="PPcompEM")
%PNetica%}
}
\keyword{ manip }
\keyword{ graph }
\keyword{ interface }
