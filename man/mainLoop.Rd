\name{mainLoop}
\alias{mainLoop}
\title{This function loops through the processing of evidence sets.}
\description{

  The \code{mainLoop} is used when the \code{\linkS4class{BNEngine}} is
  used as a server.  It checks the queue (database or internal list),
  for unprocessed \code{\linkS4class{EvidenceSet}} objects, and calls
  \code{\link{handleEvidence}} on them in the order of their
  \code{\link[Proc4]{timestamp}}s.  As a server, this is potentially an
  infinite loop, see details for ways of gracefully terminating the loop.


}
\usage{
mainLoop(eng, N=NULL)
}
\arguments{
  \item{eng}{An \code{\linkS4class{BNEngine}} which will handle the
    evidence sets.}
  \item{N}{If supplied, this should be an integer.  The loop will then
    handle that many cycles before quitting.}
}
\details{

  The \code{\linkS4class{BNEngineMongo}} class uses the EvidenceSets
  collection in the database as a queue.  All events have a
  \code{\link[Proc4]{processed}} field which is set to true when the
  evidence set is processed.  The function \code{\link{fetchNextEvidence}}
  fetches the oldest unprocessed evidence set.

  The \code{\linkS4class{BNEngineNDB}} has an internal \code{eventQueue}
  field which is a list of evidence setsn.  The \code{\link{fetchNextEvidence}}
  simply fetches the first evidence set in the queue.

  The \code{mainLoop} function iterates over the following steps.
  \enumerate{
    \item{Fetch the oldest unprocessed Event: \code{eve <-
        \link{fetchNextEvidence}()}.}
    \item{Process the evidence set: \code{out <-
        \link{handleEvidence}(eng,eve)}. 
      (Note: this expression will always return.  If it generates an error, the 
      error will be logged and an object of class \code{try-error} will be
      returned.)} 
    \item{Mark the event as processed: \code{\link{markProcessed}(eve)}.}
  }

  At its simplest level, the funciton produces an infinite loop over
  these three statements, with some additional steps related to
  logging and control.

  First, if the event queue is empty, the process sleeps for a time
  given by \code{eng$waittime} and then checks the queue again.  At the
  same time, it checks status of the active flag for the process using
  the \code{eng$stopWhenFinished()} call.  If this returns true and the
  queue is empty, processing will terminate.

  To facilitate testing, the field \code{eng$processN} can be set to a
  finite value.  This number is decremented at every cycle, and when
  it reaches 0, the \code{mainLoop} is terminated, whether or not
  their are any remaining events to be processed.  Setting
  \code{eng$processN} to an infinite value, will result in an infinite
  loop that can only be stopped by using the active flag (or
  interrupting the process).


}
\section{Activation}{

  When the loop begins, it calls the \code{eng$activate()} method to
  mark the engine as active.  When the loop finishes (outside of the
  main try/catch block, so it should always return), it calls the
  \code{eng$deactivate()} method to signal that the engine has
  terminated.

  External processes can signal the engine through the
  \code{eng$shouldHalt()} and \code{eng$stopWhenFinished()}.  The former
  is checked every iteration, and the main loop halts when it becomes
  true.  This allows for an immediate stop when needed.  The latter is
  checked only when the queue is empty and details whether or not the
  process should continue to wait for more messages in the queue.

  \bold{Database Engine.} For the Mongo engine
  (\code{\linkS4class{BNEngineMongo}}) the 
  communication channel is the \code{AuthorizedApps} collection in the
  administrative database.  In particular, the \code{EAsignal} field is
  read by both methods.  The \code{eng$activate()} method changes the
  value of that field to \dQuote{Running}.  Changing the value of the
  field to \dQuote{Halt} will cause the \code{eng$shouldHalt()} to be
  true triggering a halt before processing the next evidence set.
  Changing the value of that field to \dQuote{Finish} will
  \code{eng$stopWhenFinished()} to be true, causing the loop to stop
  then the queue is empty.

  The following command issues from the Mongo shell
  will shut down the server for an application containing the string
  "appName" as part of its name (note \dQuote{Halt} could be replaced
  with \dQuote{finish}).
  
  \code{db.AuthorizedApps.update(\{app:\{$regex:"appName"\}\},
    \{$set:\{"EAsignal":"Halt"\}\});}

  \bold{No Database Engine.} For the Mongo engine
  (\code{\linkS4class{BNEngineMongo}}) the 
  communication channel is a file named \code{activeTest}.  The
  name (extension) of this file is changed to produce the signals.
  The \code{eng$activate()} method creates it with the extension
  \code{.running}.  Changing the extension to  \code{.finish} or
  \code{.halt} will send the appropriate signal.  The
  \code{eng$deactive()} method removes the file.


}
\value{

  There is no return value.  The function is used entirely for its side
  effects. 

}
\author{Russell Almond}
\note{

  Currently, when running in server model (i.e., with
  \code{eng$processN} set to infinity), there are two ways of stopping
  the engine:  a clean stop after all events are processed using the
  \code{active} flag, and an immediate stop, possibly mid cycle, by
  killing the server process.  It became apparent during testing that
  there was a need for a graceful but immediate stop, i.e., a stop after
  processing the current event.  This should appear in later versions.

}

\seealso{

  \code{\linkS4class{BNEngine}}, \code{\linkS4class{BNEngineMongo}},
  \code{\linkS4class{BNEngineNDB}}

  \code{\link{fetchNextEvidence}}, \code{\link{handleEvidence}}

  
}
\examples{

\dontrun{
## From EABN.R script
  app <- "ecd://epls.coe.fsu.edu/P4test"
  loglevel <- "DEBUG"

source("/usr/local/share/Proc4/EAini.R")
flog.appender(appender.file(logfile))
flog.threshold(loglevel)

sess <- NeticaSession(LicenseKey=NeticaLicenseKey)
startSession(sess)
listeners <- lapply(names(EA.listenerSpecs),
                    function (ll) do.call(ll,EA.listenerSpecs[[ll]]))
names(listeners) <- names(EA.listenerSpecs)

eng <- do.call(BNEngineMongo,
              c(EAeng.params,list(session=sess,listeners=listeners),
                EAeng.common))
loadManifest(eng)
configStats(eng)
setupDefaultSR(eng)

## Activate engine (if not already activated.)
eng$activate()
mainLoop(eng)
## Wait for cows to come home.

}
}
\keyword{ database }
\keyword{ manip }

