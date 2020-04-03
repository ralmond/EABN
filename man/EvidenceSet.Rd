\name{EvidenceSet}
\alias{EvidenceSet}
\title{Creates an Evidence Set Message}
\description{

  An \code{\linkS4class{EvidenceSet}} is a
  \code{\link[Proc4]{P4Message}} which contains observable variables for
  the Bayes net engine.  It provides the observavbles associated with a
  single scoring context.

}
\usage{
EvidenceSet(uid, context, timestamp = Sys.time(), obs = list(), app =
"default", mess = "Accumulate", sender = "EI", processed = FALSE) 
}
\arguments{
  \item{uid}{A character scalar giving unique identifier for the student/player.}
  \item{context}{A character scalar giving a unique identifier for the
    scoring context (often game level or task).}
  \item{timestamp}{The time at which the evidence was recorded (POSIXt
    format). }
  \item{obs}{A named list giving the observable variables.  The names
    and legal values correspond to the \code{context} and \code{app}
    values. }
  \item{app}{A character scalar giving the globally unique identifier of
    the application.}
  \item{mess}{A character scalar giving the message associated with the
    observables. (Part of the Proc 4 procotol).}
  \item{sender}{A character scalar giving the identity of the process
    which created the message.  This will usually be an evidence
    identification process.}
  \item{processed}{A flag that is set when the evidence set has been
    processed.} 
}
\details{

  Aside from the \code{\link{seqno}} field, this is pretty much a
  generic \code{\link[Proc4]{P4Message}}.  The \code{data} of the
  \code{P4Message} is the \code{\link{observables}} value fo the
  \code{\linkS4class{EvidenceSet}}. 

}
\value{

  An object of class \code{\link{EvidenceSet}}. 
}
\author{Russell Almond}
\seealso{

  Class:  \code{\linkS4class{EvidenceSet}}
  Methods:  \code{\link{observables}}, \code{\link{seqno}},
  \code{\link{parseEvidence}}

  Using classes:  \code{\linkS4class{StudentRecord}}
}
\examples{

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))

e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                  obs=list("Attempts"=2,"Duration"=38.3))




}
\keyword{ classes }
\keyword{ interface }
