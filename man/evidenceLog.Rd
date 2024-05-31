\name{evidenceLog}
\alias{evidenceLog}
\title{Extracts the Evidence Log from a Student Record}
\description{

  The evidence log is a list of \code{\linkS4class{EvidenceLog}}
  objects which describe for each evidence message accumulated,
  which observables were and were not used.

}
\usage{
evidenceLog(sr)
}
\arguments{
  \item{sr}{An object of class \code{\linkS4class{StudentRecord}}.}
}
\value{
  A list (possibly empty) of \code{\linkS4class{EvidenceLog}} items.
}
\author{Russell Almond}
\seealso{
  \code{\linkS4class{EvidenceLog}}, \code{\linkS4class{StudentRecord}},
  \code{\link{logEvidence}}
}
\examples{
\dontrun{
  app <- "ecd://epls.coe.fsu.edu/EITest"
  wh <- local_warehouses()

  cl <- new("CaptureListener")
  listeners <- list("cl"=cl)

  ls <- Proc4::ListenerSet(sender= paste("EAEngine[",app,"]"),
                    db=mongo::MongoDB(noMongo=TRUE),
                    listeners=listeners)

  eng <- newBNEngineNDB(app=app,warehouse=wh$Nethouse,
                        listenerSet=ls,manifest=wh$netman,
                        profModel="miniPP_CM",
                        histNodes="Physics",
                        statmat=stattab,
                        activeTest="EAActive.txt")

  ## Standard initialization methods.
  loadManifest(eng,wh$netman)
  eng$setHistNodes("Physics")
  configStats(eng,wh$statmat)
  setupDefaultSR(eng)

  sr0 <- newSR(eng$studentRecords(),"Test1")

  e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                    obs=list("CompensatoryObs"="Right"))

  e1 <- logEvidence(eng,sr0,e1)

  evidenceLog(sr0)
}
}
\keyword{ manip }


