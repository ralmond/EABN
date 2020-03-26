\name{evidence}
\alias{evidence}
\title{Access the IDs for the evidence sets associated with a student
  model.}
\description{

  A give \code{\linkS4class{StudentRecord}} differs from the default
  student record (\code{\link{setupDefaultSR}}) by the sets of evidence
  that it has accumulated.  The \code{evidence} function returns a list
  of database IDs for the \code{\linkS4class{EvidenceSet}} objects which
  have been incorporated into the student record.

}
\usage{
evidence(x)
evidence(x) <- value
}
\arguments{
  \item{x}{A \code{\linkS4class{StudentRecord}} whose evidence sequence
    is to be accessed.}
  \item{value}{A character vector giving the database IDs of the
    evidence sets.}
}
\details{

  The purpose of this field is to allow for retraction of evidence.
  A particular evidence set can always be retracted by replaying all of
  the prior evidence accumulation.  This would allow a particular
  evidence set (or sets) to be retracted or replaced.

}
\value{

  A character vector giving the database IDs of various evidence sets.
}
\author{Russell Almond}
\note{

  The protocol was designed for use with the
  \code{\linkS4class{BNEngineMongo}}, which is attached to a database.
  In particular, it uses the \code{\link[Proc4]{m_id}} function, which
  returns the Mongo database ID.

  This could work in other contexts as well, but the \code{_id} field
  would need to be mapped into some other key for the evidence sets.

}
\seealso{

  \code{\link[Proc4]{P4Message}}, \code{\link[Proc4]{m_id}}
  \code{\linkS4class{StudentRecord}}, \code{\linkS4class{EvidenceSet}}
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

sr1 <- getRecordForUser(eng,"Student1")

## <<HERE>>

}
\keyword{ database }

