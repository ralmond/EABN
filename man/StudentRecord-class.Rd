\name{StudentRecord-class}
\Rdversion{1.1}
\docType{class}
\alias{StudentRecord-class}
\alias{app,StudentRecord-method}
\alias{as.jlist,StudentRecord,list-method}
\alias{context,StudentRecord-method}
\alias{evidence,StudentRecord-method}
\alias{evidence<-,StudentRecord-method}
\alias{histNames,StudentRecord-method}
\alias{history,StudentRecord,character-method}
\alias{seqno,StudentRecord-method}
\alias{seqno<-,StudentRecord-method}
\alias{show,StudentRecord-method}
\alias{sm,StudentRecord-method}
\alias{sm<-,StudentRecord-method}
\alias{stat,StudentRecord,character-method}
\alias{statNames,StudentRecord-method}
\alias{stats,StudentRecord-method}
\alias{timestamp,StudentRecord-method}
\alias{toString,StudentRecord-method}
\alias{uid,StudentRecord-method}

\title{Class \code{"StudentRecord"}}
\description{

  This is a wrapper for the Bayesian network information for a
  particular student.  It contains a local copy of the Bayesian
  network. 

}
\section{Objects from the Class}{
Objects can be created by calls to the function
\code{\link{StudentRecord}, uid, context, timestamp, smser, sm, stats,
  hist, evidence)}.

}
\section{Slots}{
  \describe{
    \item{\code{_id}:}{Object of class \code{"character"} The
      \code{\link[mongolite]{mongo}} ID of the object, empty character
      if it has not been saved in the database. If Mongo is not being
      used, this field can be used for other kinds of IDs.}
    \item{\code{app}:}{Object of class \code{"character"} that gives the
      identifier for the application this record is used with.}
    \item{\code{uid}:}{Object of class \code{"character"} which is the
      unique identifier for the user (student, player). }
    \item{\code{context}:}{Object of class \code{"character"} which
      identifies the scoring context (scoring window). }
    \item{\code{evidence}:}{Object of class \code{"character"} giving
      the IDs of the evidence sets applied to this student model. }
    \item{\code{timestamp}:}{Object of class \code{"POSIXt"} giving the
      timestamp of the last evidence set applied to this model. }
    \item{\code{sm}:}{Object of class \code{"\link[Peanut]{Pnet}"}, the
      actual student model (or \code{NULL} if it is not yet built).}
    \item{\code{smser}:}{Object of class \code{"list"} the serialized
      student model. }
    \item{\code{seqno}:}{Object of class \code{"integer"} a sequence
      number, that is the number of evidence sets applied. }
    \item{\code{stats}:}{Object of class \code{"list"} the most recent
      statistics generated from this model. }
    \item{\code{hist}:}{Object of class \code{"list"} list of history lists for
      the designed history variables.  There is one element for each
      history variable.}
    \item{\code{prev_id}:}{Object of class \code{"character"} the Mongo
      ID of the previous student model.}
  }
}
\section{Methods}{
  \describe{
    \item{app}{\code{signature(x = "StudentRecord")}: returns the
      application id associated with this record. }
    \item{as.jlist}{\code{signature(obj = "StudentRecord", ml =
        "list")}: serialized the record as JSON }
    \item{context}{\code{signature(x = "StudentRecord")}: return the
      context (scoring window) identifier asociated with the last
      processed evidence set. }
    \item{evidence}{\code{signature(x = "StudentRecord")}: returns the
      ids of the aborbed evidence sets. }
    \item{evidence<-}{\code{signature(x = "StudentRecord")}: sets the
      ids of the aborbed evidence sets.}
    \item{histNames}{\code{signature(sr = "StudentRecord")}: returns the
      names of the history variables.}
    \item{history}{\code{signature(sr = "StudentRecord", name =
        "character")}: returns the history list for the variable.}
    \item{seqno}{\code{signature(x = "StudentRecord")}: returns the
      sequence number for this record. }
    \item{seqno<-}{\code{signature(x = "StudentRecord")}: sets the
      sequence number for this record. }
    \item{show}{\code{signature(object = "StudentRecord")}: prints the
      record. }
    \item{sm}{\code{signature(x = "StudentRecord")}: returns the Bayes
      net (\code{\link[Peanut]{Pnet}}) associated with this record. }
    \item{sm<-}{\code{signature(x = "StudentRecord", value="ANY")}: sets
      the Bayes net (\code{\link[Peanut]{Pnet}}) associated with this
      record. } 
    \item{stat}{\code{signature(sr = "StudentRecord", name =
        "character")}: returns the current value of the named
      statistics.  }
    \item{statNames}{\code{signature(sr = "StudentRecord")}: returns the
      names of the statistics.}
    \item{stats}{\code{signature(x = "StudentRecord")}: returns all of
      the statistics. }
    \item{timestamp}{\code{signature(x = "StudentRecord")}: returns the
      timestamp of the last absorbed evidence set. }
    \item{toString}{\code{signature(x = "StudentRecord")}: creates a
      printed representation. }
    \item{uid}{\code{signature(x = "StudentRecord")}: returns the ID for
      the student/player.}
  }
}
\references{

  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (2015) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 13.

}
\author{Russell Almond}

\seealso{

  \code{\link{StudentRecord}}, \code{\link{EvidenceSet}},
  \code{\link{StudentRecordSet}} 

}
\examples{
showClass("StudentRecord")


}
\keyword{classes}
\keyword{graph}
