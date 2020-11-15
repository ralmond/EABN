\name{buildHistMat}
\alias{buildHistMat}
\alias{buildAppHist}
\title{Builds history matrixes showing changes in Bayes nets over time.}
\description{

  After each scoring context is processed, the EA process outputs a set
  of statistics to the AS process. This file scans through the logs of
  those messages to build a data frame showing the history of the
  recorded statistics over time.  The function \code{buildHistMat}
  builds a matrix for a single user, and the function
  \code{buildAppHist} loops over all users in the database.

}
\usage{
buildHistMat(col, app, uid)
buildAppHist(col, app)
}
\arguments{
  \item{col}{The \code{\link[mongolite]{mongo} database collection which
      contains the logged statistics.}}
  \item{app}{A character value identifying the application to build the
    history for.  The long guid for the application is required here.}
  \item{uid}{A character value ident;yfing the user for whom to build
    the history.}
}
\details{

  The statistics must be logged in a database collection for this to
  work.  Note that the \code{\link{configStats}} function will determine
  which values are logged.

}
\value{

  A \code{data.frame} object with the following fields:
  \describe{
    \item{app}{The guid of the application.}
    \item{uid}{The identifier for the user.}
    \item{context}{An identifier for the scoring context (e.g., game
      level).}
    \item{timestamp}{The timestamp for the last event from the scoring
      context.}
    \item{\dots}{Other fields correspoonding to the statsitics.}
  }

}
\author{Russell Almond}
\seealso{

  \code{\link[CPTtools]{woeBal}}, \code{\link[CPTtools]{EAPBal}}
  \code{\link[CPTtools]{ListenerSet}}

}
\examples{
\dontrun{
## excerpt from doRunrun
if (!is.null(EA.config$histListener)) {
  hl <- eng$listenerSet$listeners[[EA.config$histListener]]
  if (is.null(hl)) {
    flog.warn("History listener \%s not found, skipping building history file.",
               EA.config$histListener)
  } else {
    hist <- buildAppHist(hl$messdb(),appid)
    if (isTRUE(nrow(hist) > 0L)) {
      hist$app <- basename(hist$app)
      fname <- gsub("<app>",sappid,EA.config$histfile)

      write.csv(hist,file.path(outdir,fname))
      EAeng.params$listenerSet$registerOutput(fname,file.path(outdir,fname),
                                              appid,"EA")
    } else {
      flog.warn("No records in history file.")
    }
  }
}
}
}
\keyword{ database }

