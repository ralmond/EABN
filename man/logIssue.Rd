\name{logIssue}
\alias{logIssue}
\alias{logIssue,StudentRecord,ANY-method}
\alias{logIssue,StudentRecord,character-method}
\alias{getIssues}
\alias{getIssues,StudentRecord-method}
\title{Manage error messages associated with a StudentRecord.}
\description{

  The function \code{logIssue()} adds an issue to a
  \code{\linkS4class{StudentRecord}}.  The function \code{getIssues()}
  returns a list of issues.

}
\usage{
logIssue(sr, issue)
\S4method{logIssue}{StudentRecord,ANY}(sr,issue)
\S4method{logIssue}{StudentRecord,character}(sr,issue)
getIssues(sr)
\S4method{getIssues}{StudentRecord}(sr)
}
\arguments{
  \item{sr}{A \code{\linkS4class{StudentRecord}} object to be examined
    or modified.}
  \item{issue}{An issue to be logged.  This should be a character object
    or something which can be coerced to a character object.}
}
\details{

  The idea is to be able to log error messages and warning which occur
  when processing evidence for this person.  These are converted to
  strings, so they can be saved 

}
\value{

  The function \code{getIssues()} returns a character vector containing
  the encountered issues.

  The function \code{logIssue()} returns the modified student record.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{StudentRecord}}, \code{\link[Proc4]{markAsError}}
}
\examples{

sr0 <-
    StudentRecord("S1","*baseline*",as.POSIXct("2020-03-30 09:00:00"))

sr0 <- logIssue(sr0,"Test Issue")
err <- simpleError("Another test error.")
sr0 <- logIssue(sr0,err)
getIssues(sr0)

}
\keyword{ attribute }
\keyword{ error }

