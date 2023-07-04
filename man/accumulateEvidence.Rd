\name{accumulateEvidence}
\alias{accumulateEvidence}
\alias{handleEvidence}
\title{Merge evidence from an evidence set with the student record.}
\description{

  The function \code{accumulateEvidence} combines the evidence in the
  \code{\linkS4class{EvidenceSet}} with the exiting beliefs in the
  \code{\linkS4class{StudentRecord}}, updating the student record.  The
  function \code{handleEvidence} is a wrapper around this which takes
  care of finding and updating the evidence sets.

}
\usage{
accumulateEvidence(eng, rec, evidMess, debug = 0)
handleEvidence(eng, evidMess, srser = NULL, debug = 0)
}
\arguments{
  \item{eng}{The \code{\linkS4class{BNEngine}} which controls the
    process.}
  \item{rec}{The \code{\linkS4class{StudentRecord}} which will be updated.}
  \item{srser}{A serialized version of the student record for the
    no-database version of the model.}
  \item{evidMess}{An \code{\linkS4class{EvidenceSet}} which has the
    evidence to be incorporated.}
  \item{debug}{An integer flag.  If greater than 1, then
    \code{\link[utils]{recover}()} will be called at strategic places
    during the processing to allow inspection of the process.}
}
\details{

  The function \code{accumulateEvidence} performs the following steps:
  \enumerate{
    \item{Update the student record to associate it with the new
      evidence (\code{\link{updateRecord}}).}
    \item{Update the student model with the new evidence
      (\code{\link{updateSM}}).}
    \item{Update the statistics for the new student model
      (\code{\link{updateStats}}).}
    \item{Update the history for the new evidence
      (\code{\link{updateHist}}).}
    \item{Announce the availablity of new statistics
      (\code{\link{announceStats}}).}
    \item{Save the updated student record (\code{\link{saveSR}}).}
  }

  The function \code{handleEvidence} is a wrapper around
  \code{accumulateEvidence} which finds the student record.  Note for
  \code{\link{BNEngineNDB}}, it is expected that the student record will
  be passed in as a serialized object (see
  \code{\link{getRecordForUser}}).  It performs the following steps:
  \enumerate{
    \item{Fetch the student record for the \code{\link[Proc4]{uid}}
      associated with the evidence set
      (\code{\link{getRecordForUser}}).}
    \item{Mark the evidence as belonging to this student record
      (\code{\link{logEvidence}}).}
    \item{Update the record by calling \code{accumulateEvidence}.}
    \item{Mark the evidence as processed (\code{\link[Proc4]{markAsProcessed}}).}
  }
  If an error is encountered, then the error message is added to the
  evidence set.


}
\value{

  The modified \code{\linkS4class{StudentRecord}} which was just
  processed.  If an error occurs during the call to
  \code{accumulateEvidence} both function will return an object of class
  \code{try-error} instead of the student record.

}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapters
  5 and 13.


}
\author{Russell Almond}
\section{Logging, Error Handling and Debugging}{

  The functions \code{handleEvidence}, \code{accumulateEvidence} and
  many of the functions they call use the
  \code{\link[futile.logger]{flog.logger}} protocol.  The default
  logging level of INFO will give messages in response to the
  announcements and warnings when an error occur.  The DEBUG and TRACE
  levels will provide more information about the details of the update
  algorithm.

  The body of \code{accumulateEvidence} is wrapped in
  \code{\link[Proc4]{withFlogging}} which captures and logs errors.
  This function returns an object of class \code{try-error} when an
  error occurs.  Although \code{handleEvidence} does not use the
  flogging error handler, it will still pass on the \code{try-error} if
  one is generated.

  The \code{debug} argument can be used to pause execution.  Basically,
  \code{\link[utils]{recover}()} will be called between every step.  This
  only happens in interactive mode as it just does not make sense in
  batch model.

}
\section{Known Bugs}{

  There is a bug in version 5.04 of Netica which causes the absorbNodes
  function when called with a node that does not have display
  information to generate an internal Netica error.  This has been fixed
  with version 6.07, which is currently in beta release (Linux only).

  To work around, make sure that either all nodes do not have display
  information, or that all do.

}
\seealso{

  Classes:
  \code{\linkS4class{BNEngine}}
  \code{\linkS4class{BNEngineMongo}}, \code{\linkS4class{BNEngineNDB}}
  \code{\linkS4class{StudentRecord}},
  \code{\linkS4class{EvidenceSet}}


  Main Loop Functions:
  \code{\link{mainLoop}},
  \code{\link{getRecordForUser}},
  \code{\link{logEvidence}},
  \code{\link{updateRecord}},
  \code{\link{updateSM}},
  \code{\link{updateStats}},
  \code{\link{updateHist}},
  \code{\link{announceStats}},
  \code{\link{saveSR}}


}
\examples{

%PNetica%\dontrun{
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

ls <- ListenerSet(sender= paste("EAEngine[",app,"]"),
                  db=MongoDB(noMongo=TRUE), listeners=listeners)

eng <- newBNEngineNDB(app=app,warehouse=Nethouse,
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

e1 <- logEvidence(eng,sr0,e1)
sr1 <- accumulateEvidence(eng,sr0,e1)
stopifnot(!is(sr1,'try-error'))
stopifnot(m_id(sr1)!=m_id(sr0),sr1@prev_id==m_id(sr0))
stopifnot(seqno(sr1)==1L, seqno(e1)==1L)

eap1 <- stat(sr1,"Physics_EAP")
stopifnot(abs(eap1-eap0) > .001)
stopifnot(nrow(history(sr1,"Physcis"))==2L)

## handle Evidence.
sr1.ser <- as.json(sr1)
e2 <- EvidenceSet(uid="S2",app="Test",context="PPconjEM",
                  obs=list("ConjunctiveObs"="Wrong"))

sr2 <- handleEvidence(eng,e2,jsonlite::fromJSON(sr1.ser))
stopifnot(!is(sr2,'try-error'))
eap2 <- stat(sr2,"Physics_EAP")
stopifnot(uid(sr2)==uid(sr1),
          m_id(sr1)==sr2@prev_id,
          nrow(history(sr2,"Physics"))==3L,
          abs(eap1-eap2) > .001)

## <<HERE>> Need test with Mongo engine.

%PNetica%}


}
\keyword{ manip }
\keyword{ graphs }
\keyword{ interface }
