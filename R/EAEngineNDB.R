## Bayesian network engine with no database connection.

## Must input statmat

BNEngineNDB <-
  setRefClass("BNEngineNDB",
              c(
                  manifest = "data.frame",
                  histnodes = "character",
                  warehouseObj="PnetWarehouse",
                  netDirectory="character",
                  evidenceQueue="list",
                  statmat="data.frame",
                  activeTest="character"
              ),
              contains="BNEngine",
              methods = list(
                  initialize =
                    function(app="default",warehouse=NULL, listenerSet=NULL,
                             manifest=data.frame(),profModel=character(),
                             statistics=list(), histNodes=character(),
                             evidenceQueue=list(),waittime=0,
                             processN=Inf,statmat=data.frame(),
                             activeTest="EAActive.txt",
                             ...) {
                      if (is.null(warehouse))
                        stop("Null warehouse.")
                      callSuper(app=app,warehouseObj=warehouse,
                                srs=NULL,listenerSet=listenerSet,
                                warehouseObj=NULL,
                                evidenceQueue=evidenceQueue,
                                statistics=statistics,statmat=statmat,
                                histNodes=histNodes,profModel=profModel,
                                waittime=waittime, processN=processN,
                                activeTest=activeTest,
                                ...)
                  },
                  fetchStats = function() {
                    statmat
                  },
                  saveStats = function(stats) {
                    stats$app <- app
                    statmat <<- statmat
                  },
                  ## Here
                  evidenceSets = function() {
                    NULL
                  },
                  fetchNextEvidence = function() {
                    if (length(evidenceQueue) == 0L)
                      return(NULL)
                    es <- evidenceQueue[1]
                    evidenceQueue <<- evidenceQueue[-1]
                    es
                  },
                  studentRecords = function() {
                    if (is.null(srs)) {
                      srs <<- StudentRecordSet(app=app,warehouse=warehouse(),
                                              dburi="")
                    }
                    srs
                  },
                  fetchManifest = function() {
                    manifest
                  },
                  saveManifest = function(manif) {
                    manif$app <- app(eng)
                    manifest <<- manif
                  },
                  P4db = function () {
                    NULL
                  },
                  isActivated = function() {
                    file.exists(activeTest)
                  },
                  activate = function() {
                    file.create(activeTest)
                  },
                  show = function() {
                    methods::show(paste("<EABN: ",app,", No DB>"))
                  }))

BNEngineNDB <- function(app="default",warehouse, listenerSet=NULL,
                     manifest=data.frame(),processN=Inf,
                     waittime=.25, profModel=character(),
                     statmat=data.frame(),
                     netDirectory=".",activeTest="EAActive.txt",...) {
  new("BNEngineNDB",app=app,warehouse=warehouse,
      listenerSet=listenerSet,manifest=manifest,processN=processN,
      waittime=waittime,profModel=profModel,
      netDirectory=netDirectory,activeTest=activeTest,...)
}
