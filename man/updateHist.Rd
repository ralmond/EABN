\name{updateHist}
\alias{updateHist}
\alias{baselineHist}
\title{Update the node history in a student record}
\description{

  The \code{\linkS4class{StudentRecord}} object can track the history of
  zero or more \code{\link[Peanut]{Pnode}} in the student model
  (\code{\link{sm}}). The history is a data frame with columns
  corresponding to the states of the variables and the rows
  corresponding to the \code{\linkS4class{EvidenceSet}}s absorbed into
  the student record.  The function \code{updateHist} add a new row to
  each history corresponding to the evidence set.  The function
  \code{baselineHist} creates the initial row.

}
\usage{
updateHist(eng, rec, evidMess, debug = 0)
baselineHist(eng, rec)
}
\arguments{
  \item{eng}{The \code{\linkS4class{BNEngine}} controlling the operation.}
  \item{rec}{The \code{\linkS4class{StudentRecord}} which will be updated.}
  \item{evidMess}{The \code{\linkS4class{EvidenceSet}} which has just been
    added to the student model using \code{\link{updateSM}}.}
  \item{debug}{An integer flag.  If bigger than 1, then a call to
    \code{\link[utils]{recover}} will be made inside the function call.}
}
\details{

  A history tracks a single node in the student model as it changes in
  response to the incomming evidence sets.  The history for a node is
  data frame with columns representing variable states and rows
  representing evidence sets (evidence from different scoring windows or
  tasks).

  The function \code{baselineHist} is called as part of
  \code{\link{setupDefaultSR}}.  This initializes a history data frame
  for each node in the \code{histNodes} field of the
  \code{\linkS4class{BNEngine}}.  It inserts a first row, which is
  always given the name \dQuote{*Baseline*}.  The values in the first
  row are the marginal distribution of those nodes
  (\code{\link[Peanut]{PnodeMargin}}). 

  The function \code{updateHist} adds row to each history table.  The
  name of the row corresponds to the \code{\link[Proc4]{context}} field
  of the \code{\linkS4class{EvidenceSet}}.  The value is the curent
  marginal distribution for the history nodes.

  The function \code{\link{history}} retrieves the history.  The functions
  \code{\link[CPTtools]{woeHist}} and \code{\link[CPTtools]{woeBal}} in
  the \code{\link[CPTtools]{CPTtools-package}} describe possible
  applications for the history function.

}
\value{

  Both functions return the modified \code{\linkS4class{StudentRecord}}

}
\references{

  Madigan, Mosurski and Almond, (1997).  Graphical explanation in belief
  networks.  \emph{Journal of Computational and Graphical Statistics,}
  \bold{6}, 160--181.

  Almond, Kim, Shute and Ventura (2013).  Debugging the evidence chain.
  \emph{Proceeings of the 2013 UAI Application Workshops (UAI2013AW).}
  1--10.  CEUR workshop proceedings, vol 1024.
  \url{http://ceur-ws.org/Vol-1024/paper-01.pdf} 

}
\author{Russell Almond}
\note{

  With the Netica implementation, the student model needs to be compiled
  (\code{\link[Peanut]{PnetCompile}(sm(rec))}) before the
  \code{baselineHist} function is run.

  This is probably true of \code{updateHist} as well, but
  \code{\link{updateSM}} recompiles the network.
}
\seealso{

  Classes:
  \code{\linkS4class{BNEngine}}, \code{\linkS4class{EvidenceSet}}
  \code{\linkS4class{StudentRecord}}


  Functions in EABN:
  \code{\link{accumulateEvidence}}, \code{\link{updateStats}},
  \code{\link{updateSM}}, \code{\link{history}}

  Peanut Functions:
  \code{\link[Peanut]{PnodeMargin}}

  CPTtools Functions
  \code{\link[CPTtools]{woeHist}}, \code{\link[CPTtools]{woeBal}}


}
\examples{

%PNetica%\dontrun{
## Requires PNetica
library(PNetica)  ## Must load to setup Netica DLL
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
                  db=MongoDB(noMongo=TRUE), listeners=listeners)

eng <- newBNEngineNDB(app=app,warehouse=Nethouse,
                     listenerSet=ls,manifest=netman,
                     profModel="miniPP_CM",
                     histNodes="Physics",
                     statmat=stattab,
                     activeTest="EAActive.txt")

## Standard initialization methods.                     
loadManifest(eng,netman)
eng$setHistNodes(character())
configStats(eng,stattab)
setupDefaultSR(eng)



sr1 <- getRecordForUser(eng,"S1")
history(sr1,"Physics")
stopifnot(is.null(history(sr1,"Physics")))

## Now set up history.
eng$setHistNodes("Physics")
PnetCompile(sm(sr1))
sr1 <- baselineHist(eng,sr1)  
history(sr1,"Physics")
stopifnot(nrow(history(sr1,"Physics"))==1L)

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))
sr1 <- updateSM(eng,sr1,e1)
sr1 <- updateHist(eng,sr1,e1)

e2 <- EvidenceSet(uid="S1",app="Test",context="PPconjEM",
                  obs=list("ConjunctiveObs"="Wrong"))
sr1 <- updateSM(eng,sr1,e2)
sr1 <- updateHist(eng,sr1,e2)

history(sr1,"Physics")
stopifnot(nrow(history(sr1,"Physcis"))==3L)
woeHist(history(sr1,"Physics"),pos="High",neg=c("Medium","Low"))

%PNetica%}

}
\keyword{ manip }
\keyword{ graph }
