\name{fetchNextEvidence}
\alias{fetchNextEvidence}
\alias{markProcessed}
\title{Fetches evidence from the evidence stream and marks it as pocessed.}
\description{

  The \code{\linkS4class{BNEngine}} processes a queue of evidence
  objects (either in a database or in a list).  The function
  \code{fetchNextEvidence} fetches the oldest unprocessed
  \code{\linkS4class{EvidenceSet}} from the queue.  The function
  \code{markProcessed} marks the evidence set as processed.

}
\usage{
fetchNextEvidence(eng)
markProcessed(eng, eve)
}
\arguments{
  \item{eng}{A \code{\linkS4class{BNEngine}} which is the event handling
    system.}
  \item{eve}{A \code{\linkS4class{EvidenceSet}} which has been processed.}
}
\details{

  For the \code{\linkS4class{BNEngineMongo}} the
  \code{\linkS4class{EvidenceSet}}s reside in a collection.  The
  \code{fetchNextEvidence} fetches the oldest unprocessed record from
  the collection.  The \code{markProcessed} updates the record in the
  database to indicate that it has been processed.

  For the \code{\linkS4class{BNEngineNDB}} the queue is an in-memory
  list.  The \code{fetchNextEvidence} fetches the pops the next element
  from the list, and \code{markProcessed} marks it as processed, but
  otherwise does nothing.

}
\value{

  Both functions return the \code{\linkS4class{EvidenceSet}}.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{BNEngine}}, \code{\linkS4class{BNEngineMongo}},
  \code{\linkS4class{BNEngineNDB}}, \code{\linkS4class{EvidenceSet}}

  \code{\link{mainLoop}}, \code{\link{handleEvidence}}

}
\examples{

%\dontrun{
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


e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))
e1@"_id" <- "_E1"


e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                  obs=list("Attempts"=2,"Duration"=38.3))
e2@"_id" <- "_E2"


evidence(eng) <- list(e1,e2)

eve1 <- fetchNextEvidence(eng)
stopifnot(m_id(eve1)==m_id(e1))

eve1 <- markProcessed(eng,eve1)
stopifnot(eve1@processed)

stopifnot(length(evidence(eng))==1L)

eve2 <- fetchNextEvidence(eng)
stopifnot(m_id(eve2)==m_id(e2))

%}


}
\keyword{ interface }
\keyword{ manip }

