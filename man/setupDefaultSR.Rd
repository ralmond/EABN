\name{setupDefaultSR}
\alias{setupDefaultSR}
\title{Set up the Default Student Record for an StudentRecordSet}
\description{

  The default student record is a field associated with a
  \code{\linkS4class{StudentRecordSet}} which provides a template
  student record for a student just staring the assessment.  The
  \code{setupDefaultSR} function needs to be called at the start of
  every scoring session to initialize the \code{defaultSR} field of the
  student record set.

}
\usage{
setupDefaultSR(eng)
}
\arguments{
  \item{eng}{A \code{\linkS4class{BNEngine}} which contains the student
    record details.}
}
\details{

  This function creates a new \code{\linkS4class{StudentRecord}} object
  with the special \code{uid} \dQuote{*DEFAULT*} and the special
  \code{\link{context}} ID \dQuote{*Baseline*}.  The student model is
  actually the competency or proficiency model:  the baseline student
  model giving the population distribution of the the measured
  proficiencies.  This is fetched by name from the
  \code{\link[Peanut]{PnetWarehouse}} attached to the engine; the name
  is given in the \code{profModel} field of the \code{eng}.

  Setting up a default student record actually takes a number of steps:
  \enumerate{
    \item{The student record set (\code{eng$studentRecrods()}) is
      cleared by calling \code{\link{clearSRs}}.}
    \item{A new blank student record (\code{uid="*DEFAULT*"}) is
      created.}
    \item{The \code{\link{sm}} field of the new student record is
      initialized to the proficiency model.}
    \item{The student model is compiled
      (\code{\link[Peanut]{PnetCompile}}).}
    \item{The baseline statisics are calculated
      (\code{\link{updateStats}}).} 
    \item{The baseline history is set
      (\code{\link{baselineHist}}).}
    \item{The default student record is saved in the \code{defaultSR}
      field of the \code{\linkS4class{StudentRecordSet}} and in the
      database (\code{\link{saveSR}}).}
    \item{The baseline statistics are announced
      (\code{\link{announceStats}}).}
  }
}
\value{

  This function is called for its side effects.  

}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapter
  13. 

}
\author{Russell Almond}
\seealso{

  Classes:
  \code{\linkS4class{BNEngine}}, \code{\linkS4class{StudentRecord}},
  \code{\link{StudentRecordSet}}, \code{\link[Peanut]{PnetWarehouse}}

  Functions:
  \code{\link{clearSRs}}, \code{\link[Peanut]{PnetCompile}},
  \code{\link{updateStats}}, \code{\link{baselineHist}},
  \code{\link{saveSR}}, \code{\link{announceStats}}

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

defrec <- eng$studentRecords()$defaultSR

stopifnot(uid(defrec)=="*DEFAULT*", app(defrec)==app(eng),
          context(defrec)=="*Baseline*",
          PnetName(sm(defrec))==eng$profModel)

%}
}
\keyword{ manip }

