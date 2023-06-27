## Bayesian network engine with no database connection.

## Must input statmat

BNEngineNDB <-
  setRefClass("BNEngineNDB",
              c(
                  manifest = "data.frame",
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
                             activeTest="EAActive",
                             errorRestart="checkNoScore",
                             srs=NULL,
                             ...) {
                      callSuper(app=app,warehouse=warehouse,
                                srs=srs,listenerSet=listenerSet,
                                evidenceQueue=evidenceQueue,
                                statistics=statistics,statmat=statmat,
                                histNodes=histNodes,profModel=profModel,
                                waittime=waittime, processN=processN,
                                activeTest=activeTest,
                                errorRestart=errorRestart[1],
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
                    es <- evidenceQueue[[1]]
                    evidenceQueue <<- evidenceQueue[-1]
                    es
                  },
                  studentRecords = function() {
                     srs
                  },
                  fetchManifest = function() {
                    manifest
                  },
                  saveManifest = function(manif) {
                    manif$app <- app
                    manifest <<- manif
                  },
                  P4db = function () {
                    NULL
                  },
                  shouldHalt = function() {
                    file.exists(paste(activeTest,"halt",sep="."))
                  },
                  stopWhenFinished = function() {
                    !file.exists(paste(activeTest,"running",sep="."))
                  },
                  activate = function() {
                    file.create(paste(activeTest,"running",sep="."))
                  },
                  isActivated = function() {
                    locks <- list.files(dirname(activeTest),
                               pattern=paste(basename(activeTest),"*",sep="."))
                    return (length(locks) > 0L)
                  },
                  deactivate = function() {
                    tryCatch(file.remove(paste(activeTest,
                                               c("running","finish","halt"),
                                               sep=".")),
                             warning=function(w){})
                  },
                  show = function() {
                    methods::show(paste("<EABN: ",app,", No DB>"))
                  }))

BNEngineNDB <- function(app="default",warehouse, listenerSet=NULL,
                     manifest=data.frame(),processN=Inf,
                     waittime=.25, profModel=character(),
                     statmat=data.frame(),evidenceQueue=list(),
                     activeTest="EAActive",
                     errorRestart=c("checkNoScore", "stopProcessing",
                                    "scoreAvailable"),
                     srs=StudentRecordSet(app=app,warehouse=warehouse,
                                          db=MongoDB(noMongo=TRUE)),
                     ...) {
  ## Removed ... from new, so we can silently drop unused arguments.
  if (is.null(warehouse)) stop("Warehouse must be supplied.")
  if (is.null(srs)) stop("Student record set must be supplied.")
  new("BNEngineNDB",app=app,warehouse=warehouse,
      listenerSet=listenerSet,manifest=manifest,processN=processN,
      waittime=waittime,profModel=profModel,
      activeTest=activeTest,errorRestart=errorRestart[1],srs=srs,
      ...)
}

setMethod("evidence","BNEngineNDB",
          function(x) x$evidenceQueue)
setMethod("evidence<-","BNEngineNDB",
          function(x,value) {
            if (!is.list(value) ||
                !all(sapply(value,is,"EvidenceSet")))
              stop("Expected a list of evidence sets.")
            x$evidenceQueue<-value
            x})

