setClassUnion("NullRecordSet",c("StudentRecordSet","NULL"))


BNEngine <-
  setRefClass("BNEngine",
              c(
                  app = "character",
                  srs = "NullRecordSet",
                  profModel = "character",
                  listenerSet="NullListenerSet",
                  statistics="list",
                  histNodes="character",
                  warehouseObj="PnetWarehouse",
                  waittime="numeric",
                  processN="numeric",
                  ## These fields are included as they are related to the
                  ## configuration.
                  manifestFile="character",
                  statFile="character",
                  errorRestart="character"
              ),
              methods = list(
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
                  evidenceSets = function() {
                    stop("Abstract method.")
                  },
                  setProcessed= function (mess) {
                    mess@processed <- TRUE
                    saveRec(mess,evidenceSets())
                    mess
                  },
                  setError= function (mess,e) {
                    markAsError(mess,evidenceSets(),e)
                    mess
                  },
                  fetchNextEvidence = function() {
                    stop("Abstract Method")
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
                    stop("Abstract method.")
                    srs
                  },
                  warehouse = function () {
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
                  setRestart = function(newRestart=c("stopProcessing","checkNoScore","scoreAvailable"))
                    errorRestart <<- newRestart
                  ))

## warehouseObj <<- BNWarehouse(manifest=data.frame(),
##                              session=session,
##                              address=netDirectory,
##                              key="Name")


BNEngine <- function(app="default",session,listenerSet=NULL,
                     waittime=.25, profModel=character(),
                     ...) {
  stop("BNEngine now abstract, use BNMongoEngine or BNSQLEngine.")
}

setMethod("app","BNEngine",function (x) x$app)


## Listener notification.
setMethod("notifyListeners","BNEngine",
           function(sender,mess) {
             if (!is.null(sender$listenerSet))
               sender$listenerSet$notifyListeners(mess)
           })

setGeneric("fetchNextEvidence",
           function (eng) standardGeneric("fetchNextEvidence"))
setMethod("fetchNextEvidence","BNEngine",
          function(eng) eng$fetchNextEvidence())

setGeneric("markProcessed",
           function(eng,eve) standardGeneric("markProcessed"))
setMethod("markProcessed","BNEngine",
          function(eng,eve) eng$setProcessed(eve))





