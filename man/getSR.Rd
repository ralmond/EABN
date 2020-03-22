\name{getSR}
\alias{getSR}
\alias{newSR}
\alias{saveSR}
\alias{clearSRs}
\title{Save and retrieve student records from a record set.}
\description{

  A \code{\linkS4class{StudentRecordSet}} is a collection of
  \code{\linkS4class{StudentRecord}} objects.  The function \code{getSR}
  fetches one from the collection (if it exists) or creates a new one.
  The function \code{newSR} creates a new one.  The function
  \code{saveSR} saves the student record, and \code{clearSRs} clears out
  the saved student records.

}
\usage{
getSR(srs, uid, ser = "")
newSR(srs, uid)
saveSR(srs, rec)
clearSRs(srs)
}
\arguments{
  \item{srs}{The \code{\linkS4class{StudentRecordSet}} in question.}
  \item{uid}{A character scalar giving the unique identifier for the
    student. }
  \item{ser}{A serialized version of the student record.  Used to
    extract the student record in database-free mode.  This should
    either be a list which is the output of
    \code{\link[jsonlite]{fromJSON}} or \code{NULL}.}
  \item{rec}{A \code{\linkS4class{StudentRecord}} to be saved.}
}
\details{

  The student record set can either be attached to a database (the
  \code{dburi} field passed to \code{\link{StudentRecordSet}} is
  non-empty, or not.  In the database mode, recrods are saved in the
  database, so that they can be retrieved across sessions.  In the
  database-free mode, the serialized student record (if it exists)
  should be passed into the \code{getSR} function.

  The functions operate as follows:

  \describe{
    \item{\code{getSR}}{If the \code{ser} argument is not \code{NULL}, then the
      serialized student record is used to fetch the student record.
      Otherwise, the database (if it exists) is searched for a student
      record with the proper application and user ids.  Then
      \code{\link{fetchSM}} is called to fetch the student model. If
      both of those methods fail, \code{newSR} is called to create a new
      record.} 
    \item{\code{newSR}}{This creates a new
      \code{\linkS4class{StudentRecrod}} from the \code{defaultSR} field
      of the student record set (see \code{\link{setupDefaultSR}}).  The
      function \code{saveSR} is called to save the new record.}
    \item{\code{saveSR}}{If the database exists, the student record is
      saved to the database.  Otherwise, nothing is done.}
    \item{\code{clearSRs}}{In database mode, it clears the database.
    Otherwise, nothing is done.}
  }

}
\value{

  The functions \code{getSR}, \code{newSR} and \code{saveSR} return the
  student record.

  The function \code{clearSRs} returns the student record set (its
  argument). 

}
\author{Russell Almond}
\seealso{

  Classes:
  \code{\linkS4class{EAEngine}}, \code{\linkS4class{StudentRecordSet}},
  \code{\linkS4class{StudentRecord}}

  Functions:
  \code{\link{handleEvidence}}, \code{\link{setupDefaultSR}},
  \code{\link{fetchSM}}, \code{\link{StudentRecordSet}}

}
\examples{

##  <<Here>>
}
\keyword{ interface }
\keyword{ database }
