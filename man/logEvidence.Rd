\name{logEvidence}
\alias{logEvidence}
\alias{seqno}
\alias{seqno<-}
\title{Sets an evidence set into a sequence.}
\description{

  A \code{\linkS4class{StudentRecord}} differs from the baseline student
  record according to how many \code{\linkS4class{EvidenceSet}} objects
  have been incorporated into the estimate.  This function marks the
  evidence set with a sequence number.

}
\usage{
logEvidence(eng, rec, evidMess)
seqno(x)
seqno(x) <- value
}
\arguments{
  \item{eng}{A \code{\linkS4class{BNEngine}} which is currently not used
    (could later be used to save the evidence to a database).}
  \item{rec}{A \code{\linkS4class{StudentRecord}} into which the
    evidence will be incorporated.}
  \item{evidMess}{A \code{\linkS4class{EvidenceSet}} whose sequence
    number will be updated.}
  \item{x}{An \code{\linkS4class{EvidenceSet}} object.}
  \item{value}{An integer giving a new sequence number.}
}
\details{

  The \code{seqno} field is an optional ordering used to track the order
  in which evidence sets were incorporated into the student model.  The
  \code{logEvidence} function currently sets it to one more than the
  last sequence number for the student record.
  


}
\value{

  The \code{logEvidence} function returns the modified
  \code{\linkS4class{EvidenceSet}}.  

  The function \code{seqno} returns an integer (or \code{NA} if has not
  been set).

}
\author{Russell Almond}
\note{

  This is an experimental facility related to retraction of evidence.
  It may change in future implementations.

}
\seealso{

  \code{\linkS4class{BNEngine}},
  \code{\linkS4class{EvidenceSet}}, \code{\link{EvidenceSet}}
  \code{\linkS4class{StudentRecord}}, \code{\link{handleEvidence}}
  \code{\link[Proc4]{P4Message}}

}
\examples{

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))

e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                  obs=list("Attempts"=2,"Duration"=38.3))

stopifnot(is.na(seqno(e1)))
seqno(e1) <- 1
stopifnot(seqno(e1)==1L)

%% <<HERE>>

}
\keyword{ manip}

