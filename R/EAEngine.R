setClassUnion("BayesNet",c("NeticaBN","NULL"))

BNEngine <-
  setRefClass("BNEngine",
              c(
                  app = "character",
                  dburi="character",
                  dbname="character",
                  manifestDB = "MongoDB",
                  evidenceDB = "MongoDB",
                  profModel = "BayesNet",
                  srDB = "MongoDB",
                  listenerSet="ListenerSet",
                  warehouseObj="BNWarehouse"
              ),
              methods = list(
                  initialie = function(app,session,listeners=list(),
                     username="",password="",host="localhost",
                     port="",dbname="EARecords",manifest=data.frame(),...) {

                    ## Setup DB URI
                    security <- ""
                    if (nchar(username) > 0L) {
                      if (nchar(password) > 0L)
                        security <- paste(username,password,sep=":")
                      else
                        security <- username
                    }
                    if (nchar(port) > 0L)
                      host <- paste(host,port,sep=":")
                    else
                      host <- host
                    if (nchar(security) > 0L)
                      host <- paste(security,host,sep="@")
                    dburl <- paste("mongodb:/",host,sep="/")
                    flog.info("Connecting to database %s/%s\n",dburl,dbname)
                    ls <- ListenerSet(sender= paste("EAEngine[",app,"]"),
                                      dbname=dbname,
                                      dburi=dburl,
                                      listeners=listeners,
                                      colname="Messages",
                                      ...)
                    bnw <- BNWarehouse(manifest,session,key="Name")
                    callSuper(app=app,dburi=dburl,dbname=dbname,
                              manifestDB=NULL, evidenceDB=NULL,
                              srDB=NULL,listenerSet=ls,
                              warehouseObj=bnw)
                  },
                  manifestdb = function() {
                    if (is.null(manifestDB)) {
                      manifestDB <<- mongo("Manifest",dbname,dburi)
                    }
                    manifestDB
                  },
                  evidenceSets = function() {
                    if (is.null(evidenceDB)) {
                      evidenceDB <<- mongo("EvidenceSets",dbname,dburi)
                    }
                    evidenceDB
                  },
                  studentRecords = function() {
                    if (is.null(srDB)) {
                      srDB <<- mongo("StudentRecords",dbname,dburi)
                    }
                    srDB
                  },
                  warehouse = function() {
                    warehouseObj
                  },
                  show = function() {
                    methods::show(paste("<EABN: ",app,">"))
                  }))


EAEngine <- function(session,app="default",listeners=list(),
                     username="",password="",host="localhost",
                     port="",dbname="EARecords",manifest=data.frame(),
                     ...) {
  new("EAEngine",app=app,session=session,listeners=listeners,username=username,
      password=password,host=host,port=port,dbname=dbname,...)
}

## Listener notification.
setMethod("notifyListeners","EAEngine",
           function(sender,mess) {
             sender$listenerSet$notifyListeners(mess)
           })


loadManifest <- function(eng,manifest=data.frame()) {
  if (missing(manifest)) {
    manifest <- eng$manifestdb()$find(sprintf('{ "app":"%s"}',app))
  } else {
    eng$manifestdb()$remove(sprintf('{ "app":"%s"}',app))
    manifest$app <- app(eng)
    eng$manifestdb()$insert(manifest)
  }
  WarehouseManifest(eng$warehouseObj) <- manifest
  manifest
}

