\name{BNEngine-class}
\Rdversion{1.1}
\docType{class}
\alias{BNEngine-class}
\alias{app,BNEngine-method}
\alias{notifyListeners,BNEngine-method}
\alias{fetchNextEvidence,BNEngine-method}
\alias{markProcessed,BNEngine-method}
\alias{BNEngine}
\title{Class \code{"BNEngine"}}
\description{

  A generic engine for handling evidence messages
  (\code{\linkS4class{EvidenceSet}} objects).
  
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\section{Methods}{
  \describe{
    \item{app}{\code{signature(x = "BNEngine")}: Returns the guid
      identifying the application that this engine is handling. }
    \item{notifyListeners}{\code{signature(sender = "BNEngine")}:
      Notifies other processes that student records have been updated.  }
    \item{fetchNextEvidence}{\code{signature(eng = "BNEngine")}: Returns
      the next unprocessed \code{\linkS4class{EvidenceSet}} in the queue.}
    \item{markProcessed}{\code{signature(eng = "BNEngine", eve =
        "EvidenceSet")}: marks the \code{eve} argument as processed.}
  }
}
\details{

  This is the basic class for running the evidence accumulation
  process.  This is actually an abstract class, there are two
  subclasses:  \code{\linkS4class{BNEngineMongo}}, which uses the Mongo
  database to store student records and as a message queue, and
  \code{\linkS4class{BNEngineNDB}}, which operates without a database.
  Note that the \code{BNEngine} constructor generates an error.

  The following functions form the core of the Engine Protocol:
  
  \describe{
    \item{\code{\link{loadManifest}}}{This loads the network manifest
      for the \code{\link[Peanut]{PnetWarehouse}}. }
    \item{\code{\link{setupDefaultSR}}}{Sets up the default Student
      Record (used for creating new student records)}
    \item{\code{\link{configStats}}}{Configures the statistics that are
      reported in the main loop.}
    \item{\code{\link{baselineHist}}}{Sets up the baselines for histories.}

    \item{\code{\link{mainLoop}}}{This runs through a queue of messages,
      handling the evidence. }
    \item{\code{\link{handleEvidence}}}{Handles evidence from one
      scoring context and one user.}
    \item{\code{\link{accumulateEvidence}}}{Does the actual work of
      processing the evidence.}
    \item{\code{\link{getRecordForUser}}}{Fetches the student record for
      a user, essentially a call to \code{\link{getSR}}. }
    \item{\code{\link{logEvidence}}}{Logs the evidence as part of the
      student record.}
    \item{\code{\link{updateSM}}}{Updates the student model for the new
      evidence.}
    \item{\code{\link{updateStats}}}{Calculates new statistics for the
      revised student model.}
    \item{\code{\link{updateHist}}}{Updates the history for the revised
      student model.}
    \item{\code{\link{announceStats}}}{Updates other processes about the
      existance of updated statistics.}
  }

}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapter
  13. 

}
\author{Russell Almond}
\seealso{

  Subclasses:
  \code{\linkS4class{BNEngineMongo}}, \code{\linkS4class{BNEngineNDB}}

  Constituent parts:
  \code{\linkS4class{StudentRecordSet}}, \code{\link[Peanut]{PnetWarehouse}}
  
  Setup Functions:
  \code{\link{loadManifest}},
  \code{\link{setupDefaultSR}},
  \code{\link{configStats}},
  \code{\link{baselineHist}},

  Main Loop Functions:
  \code{\link{mainLoop}},
  \code{\link{handleEvidence}},
  \code{\link{getRecordForUser}},
  \code{\link{logEvidence}},
  \code{\link{accumulateEvidence}},
  \code{\link{updateRecord}},
  \code{\link{updateSM}},
  \code{\link{updateStats}},
  \code{\link{updateHist}},
  \code{\link{announceStats}},
  
    
}
\examples{
showClass("BNEngine")
}
\keyword{classes}
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
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{isActivated()}:}{ This function checks the database to
      see whether or not the flag is set to cause the process to stop. }
    \item{\code{setHistNodes(nodenames)}:}{ Sets the names of the
      history nodes.  Note this should be called before the call to
      \code{\link{baselineHist}} or the history nodes will not be set
      properly in the default student record. }
    \item{\code{fetchNextEvidence()}:}{ Fetches the next evidence set to
      be handled.}
    \item{\code{setError(mess, e)}:}{ Adds an error flag to an evidence
      set that generated an error. }
    \item{\code{getHistNodes()}:}{ Retrieves the history nodes.}
    \item{\code{saveStats(statmat)}:}{ Updates the set of statitics
      associated with this engine. }
    \item{\code{studentRecords()}:}{ Fetches the
      \code{\linkS4class{StudentRecordSet}} associated with the engine.
      Note:  This method should be called instead of the raw field as it
      will initialize the field if it is not set up yet.}
    \item{\code{fetchStats()}:}{ Fetches statistic objects from the
      database.  }
    \item{\code{stats()}:}{Returns the set of
      \code{\link[Peanut]{Statistic}} objects associate with the engine. }
    \item{\code{activate()}:}{Sets the flag to indicate that the process
      is running.}
    \item{\code{fetchManifest()}:}{ Fetches the network manifest from
      the database. }
    \item{\code{setManifest(manifest)}:}{ Sets the manifest for the
      \code{\link[Peanut]{PnetWarehouse}}. }
    \item{\code{saveManifest(manifest)}:}{ Saves the network manifest to
      the database.}
    \item{\code{show()}:}{ Providse a printed representation of the
      database. }
    \item{\code{setProcessed(mess)}:}{ Sets an evidence set message as
      processed.  }
    \item{\code{warehouse()}:}{ Returns the
      \code{\link[Peanut]{PnetWarehouse}} associated with this
      engine. Again, this function should be called in preference to
      directly accessing the field as it forces initialization when necessary.}
    \item{\code{evidenceSets()}:}{ A reference to the collection of
      evidence sets. }
  }
}
