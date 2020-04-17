\name{fetchSM}
\alias{fetchSM}
\alias{unpackSM}
\title{Fetches student model from database or JSON}
\description{

  The function \code{fetchSM} retrieves the student model from a
  \code{\link[Peanut]{PnetWarehouse}} or if not there, attempts to
  recreate it from a serialized version.  The function \code{unpackSM}
  does this unpacking.

}
\usage{
fetchSM(sr, warehouse)
unpackSM(sr, warehouse)
}
\arguments{
  \item{sr}{An object of class \code{\linkS4class{StudentRecord}} whose
    student model we wish to retrieve.}
  \item{warehouse}{A \code{\link{PnetWarehouse}} which stores the
    student models.}
}
\details{

  The \code{\linkS4class{StudentRecord}} object has two fields related to
  student models:  \code{sm} and \code{smser}.  The former contains the
  actual student model or \code{NULL} if it has not yet been initialized
  or restored from the database.  The latter contains a character string
  which contains a serialized version of the student model.  In
  particular, it is this serialized student model which is stored in the
  database, not the actual student model.

  The function \code{fetchSM} is used to set the \code{sm} field.  It
  checks the following places in order:
  \enumerate{
    \item{It looks in the \code{warehouse} for a student net for the
      given \code{\link{uid}} for the record.}
    \item{It calls \code{unpackSM} to unpack the serialized record.}
  }

  The function \code{unpackSM} is wrapper for the function
  \code{\link[Peanut]{WarehouseUnpack}}.  
    
}
\value{

  The function \code{fetchSM} returns the modified
  \code{\linkS4class{StudentRecord}}.

  The function \code{unpackSM} returns the student model (a
  \code{\link[Peanut]{Pnet}}).  

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{StudentRecord}}

  \code{\link[Peanut]{PnetWarehouse}},
    \code{\link[Peanut]{WarehouseUnpack}}
  
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

## dsr <- baselineHist(eng,dsr)
dsr@hist <- lapply(c("Physics"),
                     function (nd)
                     EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
names(dsr@hist) <- "Physics"

pnodenames <- names(PnetPnodes(sm(dsr)))


## Serialization and unserialization
dsr.ser <- as.json(dsr)

dsr1 <- parseStudentRecord(fromJSON(dsr.ser))
stopifnot(is.null(sm(dsr1)))
## at this point, SM has not yet been restored.


## It is there in the serial field
net1 <- unpackSM(dsr1,Nethouse)
stopifnot(all.equal(pnodenames,names(PnetPnodes(net1))))
dsr1 <- fetchSM(dsr1,Nethouse)
stopifnot(all.equal(pnodenames,names(PnetPnodes(sm(dsr1)))))

## Try this again, but first delete net from warehouse,
## So we are sure we are building it from serialized version.
WarehouseFree(Nethouse,PnetName(sm(dsr)))

dsr1 <- parseStudentRecord(fromJSON(dsr.ser))
stopifnot(is.null(sm(dsr1)))
## at this point, SM has not yet been restored.


## It is there in the serial field
net1 <- unpackSM(dsr1,Nethouse)
stopifnot(all.equal(pnodenames,names(PnetPnodes(net1))))
dsr1 <- fetchSM(dsr1,Nethouse)
stopifnot(all.equal(pnodenames,names(PnetPnodes(sm(dsr1)))))

%PNetica%}
}
\keyword{ graphs }
\keyword{ interface }
