\name{parseStudentRecord}
\alias{parseStudentRecord}
\alias{as.jlist,StudentRecord,list-method}
\title{Covert Student Records to/from JSON}
\description{

  The \code{\link[mongo]{as.json}} function takes an
  \code{\linkS4class{StudentRecord}} (among other objects) and turns it
  into JSON.  The function \code{parseStudentRecord} takes the list produced
  as the output to \code{\link[jsonlite]{fromJSON}} and turns it back
  into an \code{\linkS4class{StudentRecord}} object.


}
\usage{
parseStudentRecord(rec)
\S4method{as.jlist}{StudentRecord,list}(obj, ml, serialize=TRUE)
}
\arguments{
  \item{rec}{A list which comes from running
    \code{\link[jsonlite]{fromJSON}}  on a JSON string, or database
    extraction method.}
  \item{obj}{The object being serialized; usually \code{attributes(obj)}.}
  \item{ml}{A list of fields of the object.}
  \item{serialize}{A logical flag. If true,
    \code{\link[jsonlite]{serializeJSON}} is used to protect the
    \code{data} field (and other objects which might contain complex R
    code.}
}
\details{

  See the description for \code{\link[mongo]{as.json}} for more
  description of the general JSON conversion prototocl.

  The \code{\linkS4class{StudentRecord}} contains a
  \code{\link[Peanut]{Pnet}} field in the student model.  This takes
  some post-processing to to properly restore.

  The \code{as.jlist} method for the \code{\linkS4class{StudentRecord}}
  serializes the \code{\link{sm}} field using the
  \code{\link[Peanut]{PnetSerialize}} method.  This produces a slob
  (string large object) which is stored in the \code{smser} field of the
  \code{\linkS4class{StudentRecord}}.

  The \code{parseStudentRecord} function restores the \code{smser}
  field, but not the \code{sm} field.  This must be done in the context
  of the \code{\linkS4class{StudentRecordSet}}, or equivalently the
  \code{\link[Peanut]{PnetWarehouse}}, which is currently managing
  the networks.  To finish the process, call \code{\link{fetchSM}} to
  restore the student model network.

}
\value{

  The function \code{parseStudentRecord} returns a student record object
  with the student model not yet initialized.

  The \code{as.jlist} method returns a list which can be passed to
  \code{\link[jsonlite]{toJSON}} to produce legible JSON from the R
  object.
}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{StudentRecord}}, \code{\link[mongo]{as.json}},
  \code{\link[mongo]{getOneRec}}, \code{\link[mongo]{getManyRecs}}

  \code{\link{fetchSM}}, \code{\link[Peanut]{PnetSerialize}}

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
eng$setHistNodes("Physics")
configStats(eng,stattab)
setupDefaultSR(eng)

recset <- eng$studentRecords()

sr0 <- getRecordForUser(eng,"S1")
eap0 <- stat(sr0,"Physics_EAP")

sr0.ser <- as.json(sr0)
sr0a <- parseStudentRecord(jsonlite::fromJSON(sr0.ser))
sr0a <- fetchSM(sr0a,recset$warehouse())
## This should relink to the same student model
stopifnot(sm(sr0a)==sm(sr0),abs(eap0-stat(sr0a,"Physics_EAP")) <.0001)

## Next add some evidence and test again.

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))

e1 <- logEvidence(eng,sr0,e1)
sr1 <- accumulateEvidence(eng,sr0,e1)
eap1 <- stat(sr1,"Physics_EAP")
sr1.ser <- as.json(sr1)

## Force delete student model to make sure that it is properly
## recovered.
WarehouseFree(Nethouse,PnetName(sm(sr1)))
stopifnot(!is.active(sm(sr1)))  # No longer active.

sr1a <- parseStudentRecord(jsonlite::fromJSON(sr1.ser))
sr1a <- fetchSM(sr1a,recset$warehouse())
eap1a <- stat(sr1a,"Physics_EAP")
stopifnot(all(evidence(sr1)==evidence(sr1a)),
          abs(eap1-eap1a) <.001)

%PNetica%}

}
\keyword{ interface }

