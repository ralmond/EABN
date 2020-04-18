\name{BNEngineMongo-class}
\Rdversion{1.1}
\docType{class}
\alias{BNEngineMongo-class}

\title{Class \code{"BNEngineMongo"}}
\description{

  A Bayes net engine hooked to a Mongo database.

}
\section{Extends}{
Class \code{"\linkS4class{BNEngine}"}, directly.

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. 

}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapter
  13. 


}
\author{Russell Almond}
\note{

  The database connections are not created right away, so it
  is important to use the class-based functions, \code{manifestdb()},
  \code{statdb()}, \code{evidenceSets()}, \code{histNodesdb()},
  \code{studentRecords()}, and \code{admindb()} rather than accessing
  the fields directly.


}
\seealso{
  Classes:
  \code{\linkS4class{BNEngine}}, \code{\linkS4class{BNEngineNDB}}

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
showClass("BNEngineMongo")
}
\keyword{classes}
\section{Activation}{

  When the \code{\link{mainLoop}} runs out of evidence sets to process,
  it checks the \code{is.active()} method of the engine.  If this
  returns, false, it stops.

  For the \code{BNEngineMongo} this checks the \dQuote{AuthorizedApps}
  collection in the database to see if the current \code{app} is
  active.  It returns the value of the \code{active} field for the app's
  record. 

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
    \item{\code{dburi}:}{Object of class \code{character} giving the URI
      for the mongo database.}
    \item{\code{dbname}:}{Object of class \code{character} giving the
      name of the database to be used. }
    \item{\code{manifestDB}:}{Object of class
      \code{\link[Proc4]{MongoDB-class}} giving the collection used to store
      the manifest.  This object may not be initialized so it should be
      accessed through the class-based function \code{manifestdb()}.}
    \item{\code{evidenceDB}:}{Object of class
      \code{\link[Proc4]{MongoDB-class}} accessing the evidence set
      collection. This object may not be initialized so it should be
      accessed through the class-based function \code{evidenceSets()}.}
    \item{\code{statDB}:}{Object of class \code{MongoDB} giving the
      statistics to use. This object may not be initialized so it should be
      accessed through the class-based function \code{statdb()}.}
    \item{\code{histNodesDB}:}{Object of class \code{MongoDB} giving the
      history nodes.  This object may not be initialized so it should be
      accessed through the class-based function \code{histNodesdb()}.}
    \item{\code{admindbname}:}{Object of class \code{character} giving name
      admin (Proc4) database, used for various listeners and the
      \code{is.active()} method.}
    \item{\code{adminDB}:}{Object of class \code{MongoDB} giving the link
      to the admin database. This object may not be initialized so it should be
      accessed through the class-based function \code{admindb()}.}
  }
}
\section{Methods}{
  \describe{
    \item{\code{statdb()}:}{ Returns the database contianing the
      statistic objects. }
    \item{\code{studentRecords()}:}{ Returns the
      \code{\linkS4class{StudentRecordSet}} associated with this engine. }
    \item{\code{fetchStats()}:}{ Fetches the statistics marked in the
      database configuration. }
    \item{\code{activate()}:}{ Sets the field in the Proc4 database used
      to indicate that this application is active. }
    \item{\code{initialize(app, warehouse, listeners, username,
        password, host, port, dbname, P4dbname, profModel, waittime,
        statistics, histNodes, processN, ...)}:}{ initializes the class.
      Note that some initialization is done in the various \code{XXXdb()}
      functions, so these should be called instead of directly accessing
      the fields.}
    \item{\code{manifestdb()}:}{ Returns the
      \code{\link[Proc4]{MongoDB-class}} handle to the manifest information
      collection.  }
    \item{\code{admindb()}:}{ Returns the \code{\link[Proc4]{MongoDB-class}}
      handle to the \dQuote{AuthorizedApps} collection.}
    \item{\code{histNodesdb()}:}{ Returns the
      \code{\link[Proc4]{MongoDB-class}} handle to the hist nodes collection. }
    \item{\code{saveManifest(manifest)}:}{ Saves the current
      \code{\link[Peanut]{PnetWarehouse}} manifest to the
      \code{manifestdb()} collection } 
    \item{\code{fetchManifest()}:}{ Retrieves the saved manifest from
      the \code{manifestdb()} collection. }
    \item{\code{fetchNextEvidence()}:}{ Retrieves the next
      \code{\linkS4class{EvidenceSet}} from the \code{evidenceSets()}
      collection.  Returns \code{NULL} if there are not unprocessed
      evidence sets. }
    \item{\code{saveStats(statmat)}:}{ Saves the update statistic
      definitions to the \code{statdb()} collection. }
    \item{\code{setHistNodes(nodenames)}:}{ Saves the history nodes to
      the \code{histNodesdb()} collection. }
    \item{\code{isActivated()}:}{ Checks to see if the active flag is set. }
    \item{\code{setError(mess, e)}:}{ Added an error message to an
      evidence set. }
    \item{\code{evidenceSets()}:}{ Returns a
      \code{\link[Proc4]{MongoDB-class}} handle to the collection/queue of
      evidence sets. }
    \item{\code{getHistNodes()}:}{ Fetches the history nodes from the
      \code{histNodesdb()} collection. }
    \item{\code{show()}:}{ Provides a printed representation of the engine. }
  }

The following methods are inherited (from the corresponding class):
evidenceSets ("BNEngine"), getHistNodes ("BNEngine"), stats ("BNEngine"), setProcessed ("BNEngine"), setManifest ("BNEngine"), activate ("BNEngine"), isActivated ("BNEngine"), saveManifest ("BNEngine"), studentRecords ("BNEngine"), saveStats ("BNEngine"), fetchNextEvidence ("BNEngine"), warehouse ("BNEngine"), show ("BNEngine"), setHistNodes ("BNEngine"), setError ("BNEngine"), fetchManifest ("BNEngine"), fetchStats ("BNEngine")
}
