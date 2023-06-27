setClassUnion("SRSorNull",c("StudentRecordSet","NULL"))

BNEngine <-
  setRefClass("BNEngine",
              c(
                  app = "character",
                  srs = "SRSorNull",
                  profModel = "character",
                  listenerSet="NullListenerSet",
                  statistics="list",
                  histNodes="character",
                  warehouseObj="MTWarehouse",
                  waittime="numeric",
                  processN="numeric",
                  evidenceQueue = "MessageQueue",
                  ## These fields are included as they are related to the
                  ## configuration.
                  manifestFile="character",
                  statFile="character",
                  errorRestart="character",
                  sender="character"
              ),
              methods = list(
                  initialize = function(app=character(),profModel=character(),
                                        statistics=list(),histnodes=character(),
                                        warehouse=NULL,waittime=.25,
                                        processN=Inf,
                                        errorRestart="checkNoScore",
                                        listenerSet=NULL,
                                        srs=NULL,evidenceQueue=NULL,
                                        sender="EABN",...) {
                      if (is.null(evidenceQueue))
                        evidenceQueue=new("ListQueue",app)
                      callSuper(app=app,warehouseObj=warehouse,
                                srs=srs,listenerSet=listenerSet,
                                statistics=statistics,
                                histNodes=histNodes,profModel=profModel,
                                waittime=waittime, processN=processN,
                                errorRestart=errorRestart[1],
                                evidenceQueue=evidenceQueue,
                                ...)
                  },
                  stats = function() {
                    if (length(statistics) == 0L)
                      configStats(.self)
                    statistics
                  },
                  fetchStats = function() {
                    stop("Abstract method.")
                  },
                  saveStats = function(stats) {
                    stop("Abstract method.")
                  },
                  evidenceSet = function() {
                    evidenceQueue
                  },
                  fetchNextEvidence = function() {
                    evidenceSet()$fetchNextMessage()
                  },
                  getHistNodes = function() {
                    histNodes
                  },
                  setHistNodes = function(nodenames) {
                    if (!is.character(nodenames))
                      stop("Expected nodenames to be a character expression.")
                    histNodes <<- nodenames
                  },
                  studentRecords = function () {
                    if (is.null(srs))
                      stop("Student record set not initialized.")
                    srs
                  },
                  warehouse = function () {
                    if (is.null(warehouseObj))
                      stop("Warehouse not initialized.")
                    if (nrow(WarehouseManifest(warehouseObj))==0L) {
                      loadManifest(.self)
                    }
                    warehouseObj
                  },
                  setManifest = function(manifest) {
                    if (nrow(manifest) > 0L) {
                      ## warehouse()         # Warehouse is now pre-installed.
                      WarehouseManifest(warehouseObj) <<- manifest
                    }
                  },
                  fetchManifest = function() {
                    stop("Abstract method.")
                  },
                  saveManifest = function(manif) {
                    stop("Abstract method.")
                  },
                  activate = function() {
                  },
                  isActivated = function() {
                    TRUE
                  },
                  deactivate = function() {
                  },
                  shouldHalt = function() {
                    FALSE
                  },
                  stopWhenFinished = function() {
                    TRUE
                  },
                  show = function() {
                    methods::show(paste("<EABN: ",app,">"))
                  },
                  getRestart = function() {
                    errorRestart
                  },
                  setRestart = function(newRestart=c("checkNoScore",
                                                     "stopProcessing",
                                                     "scoreAvailable"))
                    errorRestart <<- newRestart[1]
                  ))

## warehouseObj <<- BNWarehouse(manifest=data.frame(),
##                              session=session,
##                              address=netDirectory,
##                              key="Name")


BNEngine <- function(app="default",profModel=character(),
                     statistics=list(),histnodes=character(),
                     warehouse=NULL,waittime=.25,
                     processN=Inf,
                     errorRestart="checkNoScore",
                     listenerSet=NULL,
                     srs=NULL,evidenceQueue=NULL,
                     ...) {
  stop("BNEngine now abstract, use BNMongoEngine or BNSQLEngine.")
}

setMethod("app","BNEngine",function (x) x$app)


## Listener notification.
setMethod("notifyListeners","BNEngine",
           function(sender,message) {
             if (!is.null(sender$listenerSet))
               sender$listenerSet$notifyListeners(message)
           })

setMethod("fetchNextMessage","BNEngine",
          function(queue) fetchNextMessage(queue$evidenceSet()))

setMethod("markAsProcessed",c("BNEngine","P4Message"),
          function(col,mess) markAsProcessed(col$evidenceSet(),mess))

setMethod("markAsError",c("BNEngine","P4Message"),
          function(col,mess,e) markAsError(col$evidenceSet(),mess,e))





