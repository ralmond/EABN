\name{getRecordForUser}
\alias{getRecordForUser}
\title{Gets or makes the student record for a given student.}
\description{

  The \code{\linkS4class{BNEninge}} contains a
  \code{\linkS4class{StudentRecordSet}}, which is a collection of 
  \code{\linkS4class{StudentRecord}} objects.  The function
  \code{getRecordForUser} fetches one from the collection (if it exists)
  or creates a new one.

}
\usage{
getRecordForUser(eng, uid, srser = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eng}{The \code{\linkS4class{BNEngine}} in question.}
  \item{uid}{A character scalar giving the unique identifier for the
    student. }
  \item{smser}{A serialized version of the student record.  Used to
    extract the student record in database-free mode.  This should
    either be a list which is the output of
    \code{\link[jsonlite]{fromJSON}} or \code{NULL}.}

}
\details{

  The student record set can either be attached to a database (the
  \code{dburi} field passed to \code{\link{StudentRecordSet}} is
  non-empty, or not.  In the database mode, recrods are saved in the
  database, so that they can be retrieved across sessions.  In the
  database-free mode, the serialized student record (if it exists)
  should be passed into the \code{getRecordForUser} function.

  If no student record is available for the \code{uid}, then a new one
  is created by cloning the default student record (see
  \code{\link{setupDefaultSR}}).  

  This function mostly just calls \code{\link{getSR}} on the
  \code{\linkS4class{StudentRecordSet}}; however, if a new record is
  generated, then \code{\link{announceStats}} is called to advertise the
  baseline statistics for the new user.



}
\value{

  The \code{\linkS4class{StudentRecord}} object is returned.

}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapter
  13. 

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{BNEngine}}, \code{\linkS4class{StudentRecordSet}},
  \code{\linkS4class{StudentRecord}}

  \code{\link{handleEvidence}}, \code{\link{setupDefaultSR}},
  \code{\link{fetchSM}},  \code{\link{getSR}}

}
\examples{

##  <<Here>>

}
\keyword{ manip }

