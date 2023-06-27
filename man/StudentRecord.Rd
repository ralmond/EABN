\name{StudentRecord}
\alias{StudentRecord}
\title{Constructor for \code{StudentRecord} object}
\description{

  This is the constructor for a \code{\linkS4class{StudentRecord}}
  object.  Basically, this is a wrapper around the studnet model for the
  appropriate user, with meta-data about the evidence that has been
  absorbed.

}
\usage{
StudentRecord(uid, context = "", timestamp = Sys.time(), smser = list(),
sm = NULL, stats = list(), hist = list(), evidence = character(),
app = "default", seqno = -1L, prev_id = NA_character_)
}
\arguments{
  \item{uid}{A user identifier for the student/player.}
  \item{context}{An identifer for the scoring context/window.}
  \item{timestamp}{Timestamp of the last evidence set absorbed for this
    user. }
  \item{smser}{A serialized Bayesian network (see
    \code{\link[Peanut]{WarehouseUnpack}}). }
  \item{sm}{A \code{\link[Peanut]{Pnet}} containing the student model
    (or \code{NULL} if it has not been initialized.}
  \item{stats}{A list of statistics calculated for the model.}
  \item{hist}{A list of node histories for the measured nodes.}
  \item{evidence}{A character vector of ids for the absorbed evidence sets.}
  \item{app}{A guid (string) identifying the application.}
  \item{seqno}{A sequence number, basically a count of absorbed evidence
    sets.}
  \item{prev_id}{The database ID of the previous student model.}
}
\value{
  An object of class \code{\linkS4class{StudentRecord}}.
}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{StudentRecord}}

}
\examples{

%PNetica%\dontrun{#Requires PNetica
library(PNetica)

##Start with manifest
sess <- RNetica::NeticaSession()
RNetica::startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
config.dir <- file.path(library(help="Peanut")$path, "auxdata")
netman1 <- read.csv(file.path(config.dir,"Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
net.dir <- file.path(library(help="PNetica")$path, "testnets")
Nethouse <- PNetica::BNWarehouse(manifest=netman1,session=sess,key="Name",
                       address=net.dir)

dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                     context="*Baseline*")
sm(dsr) <- WarehouseSupply(Nethouse,"miniPP_CM")
PnetCompile(sm(dsr))

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
history(dsr,"Physics")

## Serialization and unserialization
dsr.ser <- as.json(dsr)

dsr1 <- parseStudentRecord(fromJSON(dsr.ser))
dsr1 <- fetchSM(dsr1,Nethouse)

### dsr and dsr1 should be the same.
stopifnot(
 app(dsr)==app(dsr1),
 uid(dsr)==uid(dsr1),
 context(dsr)==context(dsr1),
 all.equal(timestamp(dsr),timestamp(dsr1)),
 all.equal(seqno(dsr),seqno(dsr1)),
 all.equal(stats(dsr),stats(dsr1),tolerance=.0002),
 all.equal(history(dsr,"Physics"),history(dsr1,"Physics")),
 PnetName(sm(dsr)) == PnetName(sm(dsr))
)

%PNetica%}
}
\keyword{ graph }

