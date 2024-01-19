## These functions setup a default test environment, useful for testing.

local_warehouses <- function(config.dir=file.path(library(help="Peanut")$path, "auxdata"),
                             net.dir=file.path(library(help="PNetica")$path, "testnets"),
                             netFilename="Mini-PP-Nets.csv", nodeFilename="Mini-PP-Nodes.csv",
                             statFilename="Mini-PP-Statistics.csv",
                             env=parent.frame()) {

  sess <- RNetica::NeticaSession()
  RNetica::startSession(sess)
  withr::defer(RNetica::stopSession(sess),envir=env)

  ## BNWarehouse is the PNetica Net Warehouse.
  ## This provides an example network manifest.
  netman1 <- read.csv(file.path(config.dir,netFilename),
                      row.names=1, stringsAsFactors=FALSE)
  Nethouse <- PNetica::BNWarehouse(manifest=netman1,session=sess,key="Name",
                                   address=net.dir)
  nodeman1 <- read.csv(file.path(config.dir,nodeFilename),
                      row.names=1, stringsAsFactors=FALSE)
  Nodehouse <- PNetica::NNWarehouse(manifest=netman1,session=sess,key=c("Model","Name"))

  statmat <- read.csv(file.path(config.dir,statFilename),
                      stringsAsFactors=FALSE)
  rownames(statmat) <- statmat$Name
  statlist <- sapply(statmat$Name,function (st)
    Peanut::Statistic(statmat[st,"Fun"],statmat[st,"Node"],st))
  names(statlist) <- statmat$Name

  list(sess=sess,netman=netman1,Nethouse=Nethouse,nodeman=nodeman1,Nodehouse=Nodehouse,
       statmat=statmat,statlist=statlist)
}
