setClassUnion("NullRecordSet",c("StudentRecordSet","NULL"))


BNEngine <-
  setRefClass("BNEngine",
              c(
                  app = "character",
                  session="Psession",
                  srs = "NullRecordSet",
                  profModel = "character",
                  listenerSet="ListenerSet",
                  statistics="list",
                  histNodes="character",
                  warehouseObj="PnetWarehouse",
                  waittime="numeric",
                  processN="numeric"
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
                  saveStats = function(statmat) {
                    stop("Abstract method.")
                  },
                  evidenceSets = function() {
                    stop("Abstract method.")
                  },
                  setProcessed= function (mess) {
                    mess@processed <- TRUE
                    saveRec(mess,evidenceSets())
                  },
                  setError= function (mess,e) {
                    markAsError(mess,evidenceSets(),e)
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
                    warehouse()         # Initialize Warehouse
                    WarehouseManifest(warehouseObj) <<- manifest
                  },
                  fetchManifest = function() {
                    stop("Abstract method.")
                  },
                  saveManifest = function(manifest) {
                    stop("Abstract method.")
                  },
                  isActivated = function() {
                    TRUE
                  },
                  activate = function() {
                  },
                  show = function() {
                    methods::show(paste("<EABN: ",app,">"))
                  }))

## warehouseObj <<- BNWarehouse(manifest=data.frame(),
##                              session=session,
##                              address=netDirectory,
##                              key="Name")


BNEngine <- function(app="default",warehouse,listeners=list(),
                     username="",password="",host="localhost",
                     port="",dbname="EARecords",processN=Inf,
                     P4dbname="Proc4", waittime=.25, profModel=character(),
                     ...) {
  stop("BNEngine now abstract, use BNMongoEngine or BNSQLEngine.")
}

setMethod("app","BNEngine",function (x) x$app)


## Listener notification.
setMethod("notifyListeners","BNEngine",
           function(sender,mess) {
             sender$listenerSet$notifyListeners(mess)
           })






