\name{updateSM}
\alias{updateSM}
\title{Updates the Student model with additional evidence.}
\description{

  This function is the core of the \code{EABN} algorithm.  It finds and
  attaches the evidence model to the student model, enters the findings
  from the evidence message, and then detaches the evidence model,
  leaving the student model updated.

}
\usage{
updateSM(eng, rec, evidMess, debug = 0)
}
\arguments{
  \item{eng}{The \code{\linkS4class{BNEngine}}  supervising the
    opeeration.}
  \item{rec}{The \code{\linkS4class{StudentRecord}} for the student in
    question.}
  \item{evidMess}{The \code{\linkS4class{EvidenceSet}} containing the
    new evidence.}
  \item{debug}{An integer describing how much debugging to do.  If set
    to a number greater than 1, it will issue a call to
    \code{\link[utils]{recover}} at various stages to aid in debugging
    models.} 
}
\details{

  The update algorithm performs the following step:
  \enumerate{
    \item{Finds the evidence model by name according to the
      \code{context} field of the \code{\linkS4class{EvidenceSet}}.  See 
      \code{\link[Peanut]{WarehouseSupply}}.}
    \item{Adjoins the \code{\link{sm}} of the student record with the
      evidence model and compiles the modified network.    See
      \code{\link[Peanut]{PnetAdjoin}} and
      \code{\link[Peanut]{PnetCompile}}.}
    \item{Loops over the \code{\link{observables}} in the evidence set,
      if they correspond to nodes in the evidence model, then
      instantiate their values using
      \code{\link[Peanut]{PnodeEvidence}}.}
    \item{Detatch the evidence model and recompile the network.  See
      \code{\link[Peanut]{PnetDetach}}.} 
  }

}
\value{

  The updated student record is returned.

}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapters
  5 and 13. 


}
\author{Russell Almond}
\seealso{

  Classes:
  \code{\linkS4class{BNEngine}}, \code{\link[Peanut]{PnetWarehouse}},
  \code{\linkS4class{StudentRecord}}, \code{\linkS4class{EvidenceSet}},
  \code{\link[Peanut]{Pnet}}

  Functions in EABN:
  \code{\link{accumulateEvidence}}, \code{\link{updateHist}},
  \code{\link{updateStats}}, \code{\link{getRecordForUser}}


  Peanut Functions:
  \code{\link[Peanut]{WarehouseSupply}},
  \code{\link[Peanut]{PnetAdjoin}}, \code{\link[Peanut]{PnetCompile}},
  \code{\link[Peanut]{PnetDetach}}, \code{\link[Peanut]{PnodeEvidence}}


}
\examples{

%\dontrun{
## Requires database setup, also PNetica
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
                     statmat=stattab,
                     activeTest="EAActive.txt")

## Standard initialization methods.                     
loadManifest(eng,netman)
eng$setHistNodes("Physics")
configStats(eng,stattab)
setupDefaultSR(eng)

sr1 <- getRecordForUser(eng,"S1")
PnetCompile(sm(sr1))
eap1 <- PnodeEAP(sm(sr1),PnetFindNode(sm(sr1),"Physics"))

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))

sr1a <- updateSM(eng,sr1,e1)
eap1a <- PnodeEAP(sm(sr1),PnetFindNode(sm(sr1),"Physics"))
## This should have changed.
stopifnot(abs(eap1-eap1a) > .001)

%}


}
\keyword{ graphs }
\keyword{ manip }
