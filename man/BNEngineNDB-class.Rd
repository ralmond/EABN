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

  At the start of each iteration of the \code{\link{mainLoop}}, it
  checks \code{eng$shouldHalt()} method. If this returns \code{TRUE},
  then execution is immediately halted.  When the queue is empty, it
  checks the \code{eng$stopWhenFinished()} method.  If this returns
  true, then the main loop also terminates.

  In the no database version, the process communicates with the rest of
  the system by checking the file referenced in the \code{activeTest}
  field.  The \code{eng$activate()} creates this file with the extension
  \sQuote{.running}.  Renaming the file to have the extension
  \code{.finish} will cause \code{eng$stopWhenFinished()} to return
  true, that is the \code{\link{mainLoop}} will finish when the queue is
  empty.  Renaming the file to have the extension \code{.halt} will
  cause \code{eng$shouldHalt()} to return true, and \code{mainLoop} will
  stop when it finishes processing the current event.


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
    \item{\code{activate()}:}{Creates the \code{activeTest} to indicate
      that the process is running.}
    \item{\code{deactivate()}:}{Deletes the \code{activeTest} file to
      indicate that the process is no longer running.}
    \item{\code{shouldHalt()}:}{This function checks the
      \code{activeTest} file to see whether or not the flag is set to
      cause the process to halt after processing the current record.. }
    \item{\code{stopWhenFinished()}:}{ This function checks the 
      \code{activeTest} file database to see whether or not the flag is
      set to cause the process to stop when the event queue is empty. } 
    \item{\code{studentRecords()}:}{ Returns the
      \code{\linkS4class{StudentRecordSet}} associated with this engine. }
    \item{\code{fetchStats()}:}{ Fetches the statistics marked in the
      database configuration. }
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
