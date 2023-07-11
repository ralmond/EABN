\name{getSR}
\alias{getSR}
\alias{newSR}
\alias{saveSR}
\alias{clearSRs}
\title{Save and retrieve student records from a record set.}
\description{

  A \code{\linkS4class{StudentRecordSet}} is a collection of
  \code{\linkS4class{StudentRecord}} objects.  The function \code{getSR}
  fetches one from the collection if it exists.
  The function \code{newSR} creates a new one.  The function
  \code{saveSR} saves the student record, and \code{clearSRs} clears out
  the saved student records.

}
\usage{
getSR(srs, uid, ser = "")
newSR(srs, uid, timestamp = Sys.time())
saveSR(srs, rec)
clearSRs(srs)
}
\arguments{
  \item{srs}{The \code{\linkS4class{StudentRecordSet}} in question.}
  \item{uid}{A character scalar giving the unique identifier for the
    student. }
  \item{ser}{A serialized version of the student record.  Used to
    extract the student record in database-free mode.  This should
    either be a list which is the output of
    \code{\link[jsonlite]{fromJSON}} or \code{NULL}.}
  \item{rec}{A \code{\linkS4class{StudentRecord}} to be saved.}
  \item{timestamp}{A \code{POSIXt} datetime indicating the last
    modification date of the record.}
}
\details{

  The student record set can either be attached to a database (the
  \code{dburi} field passed to \code{\link{StudentRecordSet}} is
  non-empty, or not.  In the database mode, recrods are saved in the
  database, so that they can be retrieved across sessions.  In the
  database-free mode, the serialized student record (if it exists)
  should be passed into the \code{getSR} function.

  The functions operate as follows:

  \describe{
    \item{\code{getSR}}{If the \code{ser} argument is not \code{NULL}, then the
      serialized student record is used to fetch the student record.
      Otherwise, the database (if it exists) is searched for a student
      record with the proper application and user ids.  Then
      \code{\link{fetchSM}} is called to fetch the student model. If
      both of those methods fail, it returns \code{NULL}.}
    \item{\code{newSR}}{This creates a new
      \code{\linkS4class{StudentRecord}} from the \code{defaultSR} field
      of the student record set (see \code{\link{setupDefaultSR}}).  The
      function \code{saveSR} is called to save the new record.}
    \item{\code{saveSR}}{If the database exists, the student record is
      saved to the database.  Otherwise, if no
      \code{\link[mongo]{m_id}} exists for the record one is created
      from the \code{\link[Proc4]{uid}} and \code{\link{seqno}}.}
    \item{\code{clearSRs}}{In database mode, it clears the database.
      Otherwise, nothing is done.}
  }

}
\value{

  The functions \code{getSR}, \code{newSR} and \code{saveSR} return the
  student record or \code{NULL} if the record was not found or created.

  The function \code{clearSRs} returns the student record set (its
  argument).

}
\author{Russell Almond}
\seealso{

  Classes:
  \code{\linkS4class{BNEngine}}, \code{\linkS4class{StudentRecordSet}},
  \code{\linkS4class{StudentRecord}}

  Functions:
  \code{\link{handleEvidence}}, \code{\link{setupDefaultSR}},
  \code{\link{fetchSM}}, \code{\link{StudentRecordSet}}

}
\examples{

\dontrun{
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

tr1 <- newSR(eng$studentRecords(),"Test1")
PnetCompile(sm(tr1))
stopifnot(uid(tr1)=="Test1",abs(stat(tr1,"Physics_EAP")) < .0001)
stopifnot(is.na(m_id(tr1)))  # id is NA as it has not been saved yet.

tr1 <- saveSR(eng$studentRecords(),tr1)
m_id(tr1)
stopifnot(!is.na(m_id(tr1)))  # Now set

sr0 <- getRecordForUser(eng,"S1")

eap0 <- stat(sr0,"Physics_EAP")

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))

e1 <- logEvidence(eng,sr0,e1)
sr1 <- accumulateEvidence(eng,sr0,e1)
stopifnot(m_id(sr1)!=m_id(sr0),sr1@prev_id==m_id(sr0))
stopifnot(seqno(sr1)==1L, seqno(e1)==1L)

eap1 <- stat(sr1,"Physics_EAP")
stopifnot(abs(eap1-eap0) > .001)
stopifnot(nrow(history(sr1,"Physcis"))==2L)

sr1.ser <- as.json(sr1)
WarehouseFree(Nethouse,PnetName(sm(sr1))) # Delete student model to
                                           # force restore.

sr1a <- getSR(eng$studentRecords(),"S1",fromJSON(sr1.ser))
PnetCompile(sm(sr1a))
eap1a <- stat(sr1a,"Physics_EAP")
stopifnot(abs(eap1-eap1a) < .001)
stopifnot(nrow(history(sr1a,"Physcis"))==2L)


}

##  <<Here>> Need test with Mongo implementation
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
                  db=mongo::MongoDB("Messages","EARecords", makeDBuri()),
                  registryDB=mongo::MongoDB("OutputFiles","Proc4",makeDBuri()),
                  listeners=listeners)

eng <- newBNEngineMongo(app=app,warehouse=Nethouse,
                     listenerSet=ls,
                     profModel="miniPP_CM",
                     histNodes="Physics",
                     dburi=makeDBuri(),
                     dbname="EARecords",admindbname="Proc4")



## Standard initialization methods.
loadManifest(eng,netman)
eng$setHistNodes("Physics")
configStats(eng,stattab)
setupDefaultSR(eng)

}
\keyword{ interface }
\keyword{ database }
