\name{loadManifest}
\alias{loadManifest}
\title{Loads the mainifest for the compentency and evidence models in
  the BNEngine}
\description{

  This sets the manifest of networks used in the scoring engine.  In
  particular, it sets the \code{\link[Peanut]{WarehouseManifest}} of the
  \code{\link[Peanut]{PnetWarehouse}} associated with a
  \code{\linkS4class{BNEngine}}. 

}
\usage{
loadManifest(eng, manifest = data.frame())
}
\arguments{
  \item{eng}{A \code{\linkS4class{BNEngine}} whose manifest is to be
    set.}
  \item{manifest}{A dataframe containing a network manifest (see
    \code{\link[Peanut]{BuildNetManifest}}). If missing, then the
    manifest will be retrieved from the database or other cached source.}
}
\details{

  The \code{\linkS4class{BNEngine}} requires a proficiency or competency 
  model (which is used to build student models) and a collection of
  evidence models (one for each scoring context) which are all expressed
  as \code{\link[Peanut]{Pnet}}s.  The \code{manifest} is basically a
  table of which evidence model networks go with which scoring
  contexts.  The proficienty model usually serves as the hub in the
  hub-and-spoke framework.  (In fact, if the \code{profModel} argument
  is not supplied when the \code{\linkS4class{BNEngine}} is built, the
  engine will look for a network which has no hub in the manifest.

  In fact, the manifest is part of the
  \code{\link[Peanut]{PnetWarehouse}} which is a field of the engine.
  It should have the format associate with manifests described in
  \code{\link[Peanut]{WarehouseManifest}}.  Note that the Bayes nets
  should have already been built, so the the warehouse should point to
  where they can be loaded from the filesystem on demand.

  For the \code{\linkS4class{BNEngineMongo}}, the default manifest is
  located in a table in the database.  If no manifest is supplied, then
  the manifest is read from the database.  For the
  \code{\linkS4class{BNEngineNDB}}, the manifest must be specified
  manually when the engine is contructed (or when \code{loadManifest} is
  called). 


}
\value{

  This function returns the engine argument.
  
}
\references{

  Almond, Mislevy, Steinberg, Yan and Williamson (2015).  \emph{Bayesian
    Networks in Educational Assessment}.  Springer.  Especially Chapter
  13. 

}
\author{Russell Almond}
\note{

  The \code{loadManifest} call is part of the initialization sequence
  for the \code{\linkS4class{BNEngine}}.  However, if the manifest is
  loaded into the \code{\link[Peanut]{PnetWarehouse}} as it is built, it
  is really redundant.

}
\seealso{

  Classes:  \code{\linkS4class{BNEngine}},
  \code{\linkS4class{BNEngineMongo}}, \code{\linkS4class{BNEngineNDB}},
  \code{\link[Peanut]{PnetWarehouse}}

  Functions:
  \code{\link[Peanut]{WarehouseManifest}},
  \code{\link[Peanut]{BuildNetManifest}}

}
\examples{
%\dontrun{
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

## Deliberately build warehouse without empty manifest.
Nethouse <- PNetica::BNWarehouse(session=sess,
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

stopifnot(nrow(WarehouseManifest(eng$warehouse())) == 0L)

## Standard initialization methods.                     
loadManifest(eng,netman)
stopifnot(nrow(WarehouseManifest(eng$warehouse())) == 5L)

%PNetica%}
}
\keyword{ interface }
\keyword{ manip }
