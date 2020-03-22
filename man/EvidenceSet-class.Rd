\name{EvidenceSet-class}
\Rdversion{1.1}
\docType{class}
\alias{EvidenceSet-class}
\alias{as.jlist,EvidenceSet,list-method}
\alias{observables,EvidenceSet-method}
\alias{seqno,EvidenceSet-method}
\alias{seqno<-,EvidenceSet-method}
\alias{show,EvidenceSet-method}
\alias{toString,EvidenceSet-method}

\title{Class \code{"EvidenceSet"}}
\description{

  An \code{EvidenceSet} is a collection of observables that comes from a
  particular context (scoring window, task).  It also has information
  about where it appears in the sequence of evidence that is recorded
  about a student.  It is an extension of the
  \code{\link[Proc4]{P4Message}} class.

}
\section{Objects from the Class}{
Objects can be created calls to the function
\code{\link{EvidenceSet}(uid, context, timestamp, obs, app, mess, sender)}.
}
\section{Slots}{
  \describe{
    \item{\code{seqno}:}{Object of class \code{"integer"} which contains
      the order in which this object was processed.}
    \item{\code{_id}:}{Object of class \code{"character"} which contains
      the database ID.}
    \item{\code{app}:}{Object of class \code{"character"} which gives a
      guid for the application. }
    \item{\code{uid}:}{Object of class \code{"character"} which gives an
      id for the student. }
    \item{\code{context}:}{Object of class \code{"character"} which
      gives an id for the scoring context.}
    \item{\code{sender}:}{Object of class \code{"character"} which gives
      an ID for the source of the evidence.}
    \item{\code{mess}:}{Object of class \code{"character"} which gives a
      message about what is contained in the message.}
    \item{\code{timestamp}:}{Object of class \code{"POSIXt"} which tells
      when the evidence was collected.}
    \item{\code{processed}:}{Object of class \code{"logical"} which is a
      flag to tell of the evidence has been incorporated into the
      \code{\linkS4class{StudentRecord}}.}
    \item{\code{pError}:}{Object of class \code{"ANY"} which contains
      processing error. }
    \item{\code{data}:}{Named list which contains the
      evidence.}
  }
}
\section{Extends}{
Class \code{"\link[Proc4]{P4Message}"}, directly.
}
\section{Methods}{
  \describe{
    \item{as.jlist}{\code{signature(obj = "EvidenceSet", ml = "list")}:
      This is a helper function used in serialization.  See
      \code{\link[Proc4]{as.json}}.}
    \item{observables}{\code{signature(x = "EvidenceSet")}: returns a
      named list of observables (the \code{data}) field. }
    \item{seqno}{\code{signature(x = "EvidenceSet")}: returns the
      sequence number. }
    \item{seqno<-}{\code{signature(x = "EvidenceSet")}: sets the
      sequence number. }
    \item{show}{\code{signature(object = "EvidenceSet")}: prints a
      summary of the evidence set. }
    \item{toString}{\code{signature(x = "EvidenceSet")}: provides a
      summary string for the evidence set. }
  }
}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{StudentRecord}}, \code{\link{accumulateEvidence}},
  \code{\link{handleEvidence}}, \code{\link{logEvidence}},

  \code{\link{parseEvidence}}, \code{\link{seqno}}, \code{\link{observables}}

}
\examples{
showClass("EvidenceSet")
}
\keyword{classes}
