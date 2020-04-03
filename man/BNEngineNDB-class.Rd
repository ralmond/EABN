\name{BNEngineNDB-class}
\Rdversion{1.1}
\docType{class}
\alias{BNEngineNDB-class}
\alias{evidence,BNEngineNDB-method}
\alias{evidence<-,BNEngineNDB-method}

\title{Class \code{"BNEngineNDB"}}
\description{

  A \code{\linkS4class{BNEngine}} instance which is \emph{not} connected
  to a database.

}
\section{Extends}{
Class \code{"\linkS4class{BNEngine}"}, directly.

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. 

}
\section{Methods}{
  \describe{
    \item{evidence}{\code{signature(x = "BNEngineNDB")}: Returns list of
      \code{\linkS4class{EvidenceSet}}s in the queue.}
    \item{evidence}{\code{signature(x = "BNEngineNDB", value="list")}:
      Sets the list of \code{\linkS4class{EvidenceSet}}s in the queue.}
  }
}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapter
  13. 

}
\author{Russell Almond}
\note{

  The assumption of this engine is that the serialized student model
  will be passed in along with the evidence and will be returned along
  with the updated statistics.

}
\seealso{
  Classes:
  \code{\linkS4class{BNEngine}}, \code{\linkS4class{BNEngineMongo}}

  Constituent parts:
  \code{\link{StudentRecordSet}}, \code{\link[Peanut]{PnetWarehouse}}
  
  Setup Functions:
  \code{\link{loadManifest}},
  \code{\link{setupDefaultSR}},
  \code{\link{configStats}},
  \code{\link{baselineHist}},

  Main Loop Functions:
  \code{\link{mainLoop}},
  \code{\link{accumulateEvidence}},
  \code{\link{handleEvidence}},
  \code{\link{getRecordForUser}},
  \code{\link{logEvidence}},
  \code{\link{updateSM}},
  \code{\link{updateStats}},
  \code{\link{updateHist}},
  \code{\link{announceStats}},
}
\examples{
showClass("BNEngineNDB")
}
\keyword{classes}
\section{Activation}{

  When the \code{\link{mainLoop}} runs out of evidence sets to process,
  it checks the \code{is.active()} method of the engine.  If this
  returns, false, it stops.

  For the \code{BNEngineNDB} this checks the existance of the file
  \code{activeTest}.  If this file exists, the engine is considered
  active.  Deleting this file will cause the engine to become inactive
  and stop after it finishes processing existing events.


}
\section{Fields}{
  \describe{
    \item{\code{app}:}{Object of class \code{character} giving an
      globally unique identifier for the application }
    \item{\code{srs}:}{Object of class
      \code{\linkS4class{StudentRecordSet}} of \code{NULL} giving the
        student record set for the application.}
    \item{\code{profModel}:}{Object of class \code{character} giving the
      name of the proficiency model (for the default student record) in
      the warehouse manifest.}
    \item{\code{listenerSet}:}{Object of class \code{ListenerSet} giving
      a set of listeners who will listen for new statistics.}
    \item{\code{statistics}:}{Object of class \code{list} containing
      \code{\link[Peanut]{Statistic}} objects to be run on every update
      cycle.}
    \item{\code{histNodes}:}{Object of class \code{character} giving the
      names of the nodes in the proficiency model whose history will be
      recorded. }
    \item{\code{warehouseObj}:}{Object of class
      \code{\link[Peanut]{PnetWarehouse}} 
      which stores the Bayes nets, both evidence models and student
      models are stored here.}
    \item{\code{waittime}:}{Object of class \code{numeric} giving the
      time in seconds the main event loop should wait before checking
      again for messages. }
    \item{\code{processN}:}{Object of class \code{numeric} giving the
      number of times that the main loop should run before stopping.  If
      \code{Inf}, then the main loop will run without stopping.}
    \item{\code{manifest}:}{Object of class \code{data.frame} which
      provides the manifest for the \code{\link[Peanut]{PnetWarehouse}} }
    \item{\code{histnodes}:}{Object of class \code{character} which
      gives the names of the nodes for whom history will be recorded. }
    \item{\code{evidenceQueue}:}{A \code{list} of
      \code{\linkS4class{EvidenceSet}} events to be processed. }
    \item{\code{statmat}:}{Object of class \code{data.frame} which gives
      the descriptions of the \code{\link[Peanut]{Statistic}} objects to
      be used with the net.}
    \item{\code{activeTest}:}{A pathname to the file whose existance
      will be checked to determine whether or not the engine should be
      considered active.}
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{studentRecords()}:}{ Returns the
      \code{\linkS4class{StudentRecordSet}} associated with this engine. }
    \item{\code{fetchStats()}:}{ Fetches the statistics marked in the
      database configuration. }
    \item{\code{activate()}:}{ Creates the \code{activeTest} file to
      activate the engine.}
    \item{\code{fetchStats()}:}{ Fetches the statistics or information in
      the \code{statmat} field.}
    \item{\code{initialize(app, warehouse, listeners, profModel,
        waittime, statistics, histNodes, evidenceQueue, processN,
        statmat, ...)}:}{ Initializes this class }
    \item{\code{saveManifest(manifest)}:}{ This sets the internal
      manifest field. }
    \item{\code{fetchManifest()}:}{ This returns the internal manifest field. }
    \item{\code{fetchNextEvidence()}:}{ This returns the first evidence set
      from the \code{evidenceQueue} field, and removes that element from
      the queue.}
    \item{\code{saveStats(statmat)}:}{ This saves the statistic table to
      the internal field.}
    \item{\code{isActivated()}:}{ This checks for the existance of the
      field in the \code{activeTets} field.}
    \item{\code{evidenceSets()}:}{ This returns NULL }
    \item{\code{show()}:}{ This produces a printable summary. }
  }

The following methods are inherited (from the corresponding class): 
evidenceSets ("BNEngine"), stats ("BNEngine"), setProcessed
("BNEngine"), setManifest ("BNEngine"), activate ("BNEngine"),
isActivated ("BNEngine"), saveManifest ("BNEngine"), setHistNodes
("BNEngine"), studentRecords ("BNEngine"), saveStats ("BNEngine"),
fetchNextEvidence ("BNEngine"), setError ("BNEngine"), getHistNodes
("BNEngine"), warehouse ("BNEngine"), show ("BNEngine"), fetchManifest
("BNEngine"), fetchStats ("BNEngine") 
}
