\name{observables}
\alias{observables}
\title{Access parts of an evidence set message.}
\description{

  The function \code{observables} access the list of observables
  contained in this \code{\linkS4class{EvidenceSet}}.  The function \code{seqno} access the
  order in which the evidence sets were incorporated into the student
  record. 

}
\usage{
observables(x)
}
\arguments{
  \item{x}{An \code{\linkS4class{EvidenceSet}} object.}
}
\details{

  The \code{observables} function access the \code{data} field of the
  underlying \code{\link[Proc4]{P4Message}}.  This should be a named
  list of values that the \code{\linkS4class{BNEngine}} knows how to
  process.


}
\value{

  The function \code{observables} returns a named list of observable
  values.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{EvidenceSet}}, \code{\link{EvidenceSet}}
  \code{\linkS4class{StudentRecord}}, \code{\link{handleEvidence}}
  \code{\link[Proc4]{P4Message}}

}
\examples{

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))

e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                  obs=list("Attempts"=2,"Duration"=38.3))

stopifnot(all.equal(observables(e1),
                    list("CompensatoryObs"="Right")))

stopifnot(all.equal(observables(e2)$Attempts,2))

stopifnot(is.na(seqno(e1)))
seqno(e1) <- 1
stopifnot(seqno(e1)==1L)




}
\keyword{ manip }

