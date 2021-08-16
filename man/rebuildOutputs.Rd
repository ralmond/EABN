\name{rebuildOutputs}
\alias{rebuildOutputs}
\title{Reconstructs the statistic and history outputs from the log files.}
\description{

  The last step of the runner (\code{\link{doRunrun}}) is to build a
  number of output files from the logged output.  This function reruns
  those last calculations.

}
\usage{
rebuildOutputs(appid, EA.config, EAeng.local, outdir)
}
\arguments{
  \item{appid}{A character string giving the global unique identifier for
    the application being run.  This is normally formatted like a URL,
    and \code{basename(app)} is used as a short name.}
  \item{EA.config}{A named list containing the configuration details.  See the
    \sQuote{Configuration} section below.}
  \item{EAeng.local}{A named list containing additional parameters for the
    engine constructor.  The intention that these are local
    configuraiton paramete (e.g., database names and passwords) as
    opposed to more global information.  Note this must have an element
    named \dQuote{dburi} which gives the URI for the database, or which
    should be blank if the no database engine is to be used.}
  \item{outdir}{The pathname of the directory to which output files
    will be written.}
}
\details{

  This is for simply regenerating the output files.  It performs only
  two steps:
    \enumerate{
    \item{Configure the listeners (to find databases for statistics).}
    \item{Build (and register) the statistics and history files.}
  }

  This function rebuilds the listerners and finds the statistics and
  history log files based on the \code{EA.config} and \code{EA.local}
  arguments.  See \code{\link{doRunrun}} for the fields of the
  arguments. 

}
\section{Data Files}{

  If the value of \code{EA.config$statListener} is not null, then the
  final statistic values for all users are put into a table which is
  exported (to the file \code{EA.config$statfile}).  

  If the value of \code{EA.config$histListener} is not null, then the
  history of all statistic values for all users are put into a table
  which is exported (to the file \code{EA.config$histfile}).  

  Both the statfile and histfile are registered using the
  \code{\link[Proc4]{ListenerSet}$registerOutput} method.

}
\value{

  Value is undefined.  Called for its side effects.
}
\author{Russell Almond}
\note{

  This rebuilds the output file from the database.  Does not work if the
  statistics were not recorded in the database.

}
\seealso{

  \code{\link{doBuild}}, \code{\link{doRunrun}}

  \code{\linkS4class{BNEninge}}, 
  \code{\link[Proc4]{ListenerSet}},
  \code{\link{configStats}},
  \code{\link{buildHistMat}}

}
\examples{
## This example is in:
## This example is in:
file.path(help(package="EABN")$path,"conf","EARebuildOutputs.R")
\dontrun{
library(R.utils)
library(EABN)

appStem <- cmdArg("app",NULL)
if (FALSE) {
  appStem <- "userControl"
}

source("/usr/local/share/Proc4/EAini.R")

EA.config <- fromJSON(file.path(config.dir,"config.json"),FALSE)

app <- as.character(Proc4.config$apps[appStem])
if (length(app)==0L || any(app=="NULL")) {
  stop("Could not find app for ",appStem)
}

rebuildOutput(app,EA.config,EAeng.local,outdir)

}

}
\keyword{ interface }
