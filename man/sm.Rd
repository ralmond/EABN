\name{sm}
\alias{sm}
\alias{sm<-}
\title{Access the student model (Pnet) associated with a studnet record}
\description{

  A characteristic of the \code{EABN} model is that each code
  \code{\link{StudentRecord}} is associated with a \emph{student
  model}--a \code{\link[Peanut]{Pnet}} which tracks our knowledge about
  the student's knowledge skills and abilities.  The function \code{sm}
  accesses the net.

}
\usage{
sm(x)
sm(x) <- value
}
\arguments{
  \item{x}{An object of class \code{\linkS4class{StudentRecord}} whose
    student model will be accessed.}
  \item{value}{A \code{\link[Peanut]{Pnet}} object which will be the
  new student model.}
}
\value{

  The function \code{sm} returns an object which implements the
  \code{\link[Peanut]{Pnet}} protocol, or none is the student model has
  not been generated.

  The setter version returns the student record.

}
\author{Russell Almond}
\seealso{

  \code{\link{fetchSM}}, \code{\link{unpackSM}},
  \code{\link{setupDefaultSR}} 
}
\examples{

library(PNetica)

##Start with manifest
sess <- NeticaSession()
startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
config.dir <- file.path(library(help="Peanut")$path, "auxdata")
netman1 <- read.csv(file.path(config.dir,"Mini-PP-Nets.csv"),
                    row.names=1, stringsAsFactors=FALSE)
net.dir <- file.path(library(help="PNetica")$path, "testnets")
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name",
                       address=net.dir)

dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                     context="*Baseline*")
sm(dsr) <- WarehouseSupply(Nethouse,"miniPP_CM")
PnetCompile(sm(dsr))

}
\keyword{ graphs }
\keyword{ manip }
