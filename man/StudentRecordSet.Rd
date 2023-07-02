\name{StudentRecordSet}
\alias{StudentRecordSet}
\title{Constructor for \dQuote{StudentRecordSet} class}
\description{

  A \code{\linkS4class{StudentRecordSet}} is a collection of collection
  of \code{\linkS4class{StudentRecord}} objects.  It is always connected
  to a \code{\link[Peanut]{PnetWarehouse}} and could be connected to a
  database as well.

}
\usage{
StudentRecordSet(app = "default", warehouse = NULL,
                 db=MongoDB("StudentRecords","EARecords"),...)
}
\arguments{
  \item{app}{A character scalar providing a guid for the application.}
  \item{warehouse}{An object of type \code{\link[Peanut]{PnetWarehouse}}
    that contains already built student models.}
  \item{db}{A \code{\link[mongo::JSONDB-class]{JSONDB}} object which store the student
    records. }
  \item{\dots}{Other arguments for future extensions.}
}
\details{

  A \code{StudentRecordSet} is a collection of student recrods.  It
  contains a \code{\link[Peanut]{PnetWarehouse}} which contains the
  student models and possibly a database containing the student records.

  The \code{StudentRecordSet} operates in two modes, depending on the
  value of \code{db}.  If \code{db} references a
  \code{\link[mongo]{MongoDB-class}} database, then the
  \code{StudentRecordSet} set will save student records (including
  serialized Bayes nets) to the database and restore them on demmand.
  This facilitates scoring across several sessions.

  If the \code{db} argument has the \code{noMongo} flag, no
  database connection will be created.  Instead, the calls to the
  \code{\link{getSR}} function should pass in a serialized version of
  the student record function.  If no serialized record is available, a
  new record will be created.

}
\value{
  An object of class \code{\linkS4class{StudentRecordSet}}.
}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{StudentRecordSet}}, \code{\linkS4class{StudentRecord}}, 
  \code{\link{getSR}}, \code{\link{saveSR}}, \code{\link{newSR}},
  \code{\link{clearSRs}}
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

## Setup to test without Mongo
SRS <- StudentRecordSet(app="Test",warehouse=Nethouse,
                        dburi="")
stopifnot(is.null(SRS$recorddb()))

## Setup default SR
dsr <- StudentRecord("*DEFAULT*",app="Test",
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

dsr@hist <- lapply(c("Physics"),
                     function (nd)
                     EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
names(dsr@hist) <- "Physics"

SRS$defaultSR <- dsr
saveSR(SRS, dsr)

## Make a new Student Record for a student.
sr1 <- newSR(SRS,"S1")
stopifnot(uid(sr1)=="S1",app(sr1)==app(dsr),
          all.equal(stats(dsr),stats(sr1),.0002))


sr1a <- getSR(SRS,"S1")

clearSRs(SRS)

%PNetica%}
}
\keyword{ class }

