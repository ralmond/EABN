\name{logEvidence}
\alias{updateRecord}
\alias{logEvidence}
\alias{seqno}
\alias{seqno<-}
\alias{evidence}
\alias{evidence<-}
\title{Handle the relationship between evidence sets and student records.}
\description{

  A \code{\linkS4class{StudentRecord}} differs from the baseline student
  record according to how many \code{\linkS4class{EvidenceSet}} objects
  have been incorporated into the estimate.  These functions tie and
  student record and evidence set together.

}
\usage{
logEvidence(eng, rec, evidMess)
seqno(x)
seqno(x) <- value
evidence(x)
evidence(x) <- value
}
\arguments{
  \item{eng}{A \code{\linkS4class{BNEngine}} which is currently not used
    (could later be used to save the evidence to a database).}
  \item{rec}{A \code{\linkS4class{StudentRecord}} into which the
    evidence will be incorporated.}
  \item{evidMess}{A \code{\linkS4class{EvidenceSet}} which will be
    associated with the student record.}
  \item{x}{An \code{\linkS4class{EvidenceSet}} object.}
  \item{value}{For \code{seqno(x) <- value}, an integer giving a new
    sequence number.  For \code{evidence(x) <- value}, a character
    vector giving the sequence of evidence ID.}
}
\details{

  There are several fields in the \code{\linkS4class{StudentRecord}}
  class which need to be updeated in the face of new evidence.
  \describe{
    \item{context and timestamp}{These needs to be set to the values in
      the new evidence message.}
    \item{seqno}{This needs to be incremented.}
    \item{evidence}{The new evidence needs to be prepended to this
      list.}
    \item{prev_id and "_id"}{The prev_id needs to point to the old field
      and the "_id" is set to NA (it will be updated on save).}
  }

  In the case of the \code{\linkS4class{BNEngineMongo}}, the IDs in
  question are the database ids for these objects so that they can be
  easily found.  The function \code{\link[mongo]{m_id}} For the
  \code{\linkS4class{BNEngineNDB}} case presumably some external system
  is issuing IDs to evidence sets and student records.

  The \code{evidence} field of a \code{\linkS4class{StudentRecord}} is a
  list of IDs (\code{\link[mongo]{m_id}}) for the accumulated evidence.

  The \code{seqno} field is an optional ordering used to track the order
  in which evidence sets were incorporated into the student model.  The
  value of \code{seqno} gives the number of evidence sets incorporated
  into the recrod.

  The \code{logEvidence} function sets the sequence number of the
  evidence message to one more than the last sequence number for the
  student record.  If no \code{\link[mongo]{m_id}} exists for the record
  (no database mode), then one is generated by concatenating the
  \code{\link[Proc4]{uid}} and the \code{seqno}.

}
\value{

  The \code{updateRecord} returns a new
  \code{\linkS4class{StudentRecord}} object, which points back to the
  old one.

  The \code{logEvidence} function returns the modified
  \code{\linkS4class{EvidenceSet}}.

  The function \code{seqno} returns an integer (or \code{NA} if has not
  been set).

  The function \code{evidence} returns a character vector giving the IDs
  (\code{\link[mongo]{m_id}}) of the encorpated evidence sets.

}
\author{Russell Almond}
\note{

  This is largely untested code for future fast retraction of evidence.

  The \code{prev_id} field of the \code{\linkS4class{StudentRecord}}
  should leave a trace of previous student records in the database,
  including old serialized models.  This should allow the scoring engine
  to quickly jump back in time.

  The \code{evidence} field provides a list of the
  \code{\link[mongo]{m_id}}s of all the incorporated evidence sets.
  This should enable one or more evidence sets to be replaced and the
  student model to be recalculated.

}
\seealso{

  \code{\linkS4class{BNEngine}},
  \code{\linkS4class{EvidenceSet}}, \code{\link{EvidenceSet}}
  \code{\linkS4class{StudentRecord}}, \code{\link{handleEvidence}}
  \code{\link[Proc4]{P4Message}}

}
\examples{
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)
Nethouse <- PNetica::BNWarehouse(sess=sess)

recset <- StudentRecordSet(warehouse=Nethouse,db=MongoDB(noMongo=TRUE))

sr0 <-
    StudentRecord("S1","*baseline*",as.POSIXct("2020-03-30 09:00:00"))
seqno(sr0) <- 0
sr0 <- saveSR(recset,sr0) # Sets the m_id

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))

e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                  obs=list("Attempts"=2,"Duration"=38.3))

stopifnot(is.na(seqno(e1)), seqno(sr0)==0L)
stopifnot(length(evidence(sr0))==0L)

e1 <- logEvidence(NULL,sr0,e1)
stopifnot(seqno(e1)==1L,!is.na(m_id(e1)))

sr1 <- updateRecord(sr0,e1)
stopifnot(is.na(m_id(sr1)),sr1@prev_id==m_id(sr0))
sr1 <- saveSR(recset,sr1)    # Sets the m_id

stopifnot(length(evidence(sr1))==1L,any(m_id(e1)==evidence(sr1)))
stopifnot(context(sr1)==context(e1),timestamp(sr1)==timestamp(e1))

e2 <- logEvidence(NULL,sr1,e2)
stopifnot(seqno(e2)==2L,!is.na(m_id(e2)))

sr2 <- updateRecord(sr1,e2)
stopifnot(is.na(m_id(sr2)),sr2@prev_id==m_id(sr1))
sr2 <- saveSR(recset,sr2) # Sets the m_id

stopifnot(length(evidence(sr2))==2L,any(m_id(e2)==evidence(sr2)))
stopifnot(context(sr2)==context(e2),timestamp(sr2)==timestamp(e2))


}
\keyword{ manip}

