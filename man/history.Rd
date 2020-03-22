\name{history}
\alias{history}
\alias{histNames}
\title{Retrieves node histories from a Student Record}
\description{

  A history is a \code{data.frame} whose rows correspond to
  \code{\linkS4class{EvidenceSet}} objects and whose columns correspond
  to the states of a \code{\link[Peanut]{Pnode}}.  Each row is a
  probability distribution, and they show the changes to the
  probabilities over time.

  The function \code{history} returns the
  history for a single node in a given
  \code{\linkS4class{StudentRecord}}.  The function \code{histNames}
  returns the names of the nodes for which the record has history
  information. 

}
\usage{
history(sr, name)
histName(sr)
}
\arguments{
  \item{sr}{A \code{\linkS4class{StudentRecord}} whose history is to be
    accessed.}
  \item{name}{The name of the node whose history is requested.}
}
\details{

  When the student record is first initialized, the function 
  \code{\link{baselineHist}} is called to setup \dQuote{*BASELINE*}
  values for each of the history nodes identified by the
  \code{\linkS4class{BNEngine}}.  These are \code{data.frame} objects
  giving the prior marginal distributions for each of the identified
  variables. 

  After the student model is updated in response to evidence (see
  \code{\link{handleEvidence}}, the \code{\link{updateHist}} function is
  called to add a new row to each of the data frames.

  The \code{histNames} function returns the names of the history nodes
  being tracked by a student model.  The \code{history} function returns
  the history for a node.

}
\value{

  The function \code{histNames} returns a list of node names.  These are
  suitable for the \code{name} argument of the \code{history} function.

  The function \code{history} returns a data frame with rows
  corresponding to evidence sets and columns corresponding to states of
  the variables.  Each row is a marginal probability distribution.
  
}
\author{Russell Almond}
\note{

  These are designed to work with the functions
  \code{\link[CPTtools]{woeHist}} and \code{\link[CPTtools]{woeBal}} in
  the \code{\link[CPTtools]{CPTtools-package}}.  

}
\seealso{

  \code{\linkS4class{StudentRecord}} for student records.

  \code{\link{baselineHist}} and \code{\link{updateHist}} for history
  construction.
  
  \code{\link{BNEngine}} for specifying the history nodes.

  \code{\link[CPTtools]{woeHist}} and \code{\link[CPTtools]{woeBal}} for
  applications. 


}
\examples{
## dsr <- updateStats(eng,dsr)
statmat <- read.csv(file.path(config.dir,"Mini-PP-Statistics.csv"),
                    stringsAsFactors=FALSE)
rownames(statmat) <- statmat$Name
statlist <- sapply(statmat$Name,function (st)
    Statistic(statmat[st,"Fun"],statmat[st,"Node"],st))
names(statlist) <- statmat$Name 
dsr@stats <- lapply(statlist,
                    function (stat) calcStat(stat,sm(dsr)))
names(dsr@stats) <- names(statlist)
stat(dsr,"Physics_EAP")
stat(dsr,"Physics_Margin")

## dsr <- baselineHist(eng,dsr)

dsr@hist <- lapply(c("Physics"),
                     function (nd)
                     EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
names(dsr@hist) <- "Physics"
stopifnot(histName(dsr))=="Physics")
history(dsr,"Physics")


}
\keyword{ manip }
\keyword{ graphs }
