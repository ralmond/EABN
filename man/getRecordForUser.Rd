\name{getRecordForUser}
\alias{getRecordForUser}
\title{Gets or makes the student record for a given student.}
\description{

  The \code{\linkS4class{BNEngine}} contains a
  \code{\linkS4class{StudentRecordSet}}, which is a collection of 
  \code{\linkS4class{StudentRecord}} objects.  The function
  \code{getRecordForUser} fetches one from the collection (if it exists)
  or creates a new one.

}
\usage{
getRecordForUser(eng, uid, srser = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eng}{The \code{\linkS4class{BNEngine}} in question.}
  \item{uid}{A character scalar giving the unique identifier for the
    student. }
  \item{srser}{A serialized version of the student record.  Used to
    extract the student record in database-free mode.  This should
    either be a list which is the output of
    \code{\link[jsonlite]{fromJSON}} or \code{NULL}.}

}
\details{

  The student record set can either be attached to a database (the
  \code{dburi} field passed to \code{\link{StudentRecordSet}} is
  non-empty, or not.  In the database mode, recrods are saved in the
  database, so that they can be retrieved across sessions.  In the
  database-free mode, the serialized student record (if it exists)
  should be passed into the \code{getRecordForUser} function.

  If no student record is available for the \code{uid}, then a new one
  is created by cloning the default student record (see
  \code{\link{setupDefaultSR}}).  

  This function mostly just calls \code{\link{getSR}} on the
  \code{\linkS4class{StudentRecordSet}}; however, if a new record is
  generated, then \code{\link{announceStats}} is called to advertise the
  baseline statistics for the new user.



}
\section{Warning}{

  Calling this multiple times will not return the same student record.
  In particular, the student model associated with the old version of
  the record could be replaced with a new version, rendering the student
  model in the old records inactive.  Be careful when dealing with old
  records.
  
  
}
\value{

  The \code{\linkS4class{StudentRecord}} object is returned.

}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapter
  13. 

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{BNEngine}}, \code{\linkS4class{StudentRecordSet}},
  \code{\linkS4class{StudentRecord}}

  \code{\link{handleEvidence}}, \code{\link{setupDefaultSR}},
  \code{\link{fetchSM}},  \code{\link{getSR}}

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
stattab <- read.csv(file.path(config.dir, "Mini-PP-Statistics.csv"),
                    as.is=TRUE)
Nethouse <- PNetica::BNWarehouse(manifest=netman1,session=sess,key="Name",
                       address=net.dir)


cl <- new("CaptureListener")
listeners <- list("cl"=cl)

ls <- ListenerSet(sender= "EAEngine[Test]",
                  dburi="", listeners=listeners)

eng <- BNEngineNDB(app="Test",warehouse=Nethouse,
                     listenerSet=ls,manifest=netman,
                     profModel="miniPP_CM",
                     histNodes="Physics",
                     statmat=stattab,
                     activeTest="EAActive.txt")

## Standard initialization methods.                     
loadManifest(eng,netman1)
eng$setHistNodes("Physics")
configStats(eng,stattab)
setupDefaultSR(eng)

sr0a <- getRecordForUser(eng,"Student1")
sr0 <- getRecordForUser(eng,"Student1")
## This is announcing twice, so not quite working with NDB engine.

stopifnot(is.active(sm(sr0)),!is.active(sm(sr0a)))
stopifnot(all.equal(stats(sr0),stats(sr0a)))
eap0<- stat(sr0,"Physics_EAP")


e1 <- EvidenceSet(uid="Student1",app="Test",context="PPcompEM",
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
sr1a <- getRecordForUser(eng,"Student1",fromJSON(sr1.ser))
#PnetCompile(sm(sr1a))
eap1a <- stat(sr1a,"Physics_EAP")
stopifnot(abs(eap1-eap1a) < .001)
stopifnot(nrow(history(sr1a,"Physcis"))==2L)


##  <<Here>>  Need test with Mongo engine

%PNetica%}
}
\keyword{ manip }

