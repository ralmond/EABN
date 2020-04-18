\name{configStats}
\alias{configStats}
\title{Configures the Statistic Objects for the BNEninge}
\description{

  As part of the scoring cycle, the \code{\linkS4class{BNEngine}}
  calculates the values of certain statistics of the student model.
  This function sets up those statistics.

}
\usage{
configStats(eng, statmat = data.frame())
}
\arguments{
  \item{eng}{The \code{\linkS4class{BNEngine}} to be configured.}
  \item{statmat}{A data frame containing the statistic descriptions,
    see details.}
}
\details{

  A \code{\link[Peanut]{Statistic}} is a functional that is applied to
  the student model (\code{\link{sm}}) of a
  \code{\linkS4class{StudentRecord}}. At the end of the evidence
  processing cycle, the function \code{\link{updateStats}} is called to
  calculate new values for the specified statistics.

  The \code{statmat} argument should be a \code{data.frame} with three
  columns (all of mode character):
  \describe{
    \item{Name}{This column gives an identifier for the statistic used
      in the output message.}
    \item{Fun}{This column gives the name of a function (see
      \code{\link[Peanut]{Statistic}} for a list of possible values)
      which calculates the statistic value.  }
    \item{Node}{This gives the name of a node in the competency model
      which is the focus of the statistic.}
  }
    
  If the \code{statmat} argument is not supplied, then a default value
  based on the engine type is used.  For the
  \code{\linkS4class{BNEngineMongo}} this data frame is taken from a
  table in the database.  For the \code{\linkS4class{BNEngineNDB}} the
  default \code{statmat} is stored in a field in the engine.

}
\value{

  The modified engine argument is returned.
  
}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapter
  13. 

}
\author{Russell Almond}
\seealso{

  Classes:
  \code{\linkS4class{BNEngine}}, \code{\link[Peanut]{Statistic}}
  \code{\link{updateStats}}, \code{\link{announceStats}}
  
}
\examples{

%PNetica%\dontrun{
## Requires PNetica
library(RNetica)  ## Must load to setup Netica DLL
app <- "ecd://epls.coe.fsu.edu/EITest"
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)

config.dir <- file.path(library(help="Peanut")$path, "auxdata")
net.dir <- file.path(library(help="PNetica")$path,"testnets")

netman <- read.csv(file.path(config.dir, "Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
stattab <- read.csv(file.path(config.dir, "Mini-PP-Statistics.csv"),
                    as.is=TRUE)

Nethouse <- PNetica::BNWarehouse(netman,session=sess,
             address=net.dir)

cl <- new("CaptureListener")
listeners <- list("cl"=cl)

ls <- ListenerSet(sender= paste("EAEngine[",app,"]"),
                  dburi="", listeners=listeners)

eng <- BNEngineNDB(app=app,warehouse=Nethouse,
                     listenerSet=ls,manifest=netman,
                     profModel="miniPP_CM",
                     histNodes="Physics",
                     activeTest="EAActive.txt")

## Standard initialization methods.                     
configStats(eng,stattab)
stats <- eng$stats()
stopifnot(all(sapply(stats,StatName)==stattab$Name),
          all(sapply(stats,function(s) s@fun)==stattab$Fun),
          all(sapply(stats,function(s) s@node)==stattab$Node))

%PNetica%}
}
\keyword{ manip }

