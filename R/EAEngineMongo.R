BNEngineMongo <-
  setRefClass("BNEngineMongo",
              c(
                  dburi="character",
                  dbname="character",
                  manifestDB = "ANY",
                  evidenceDB = "ANY",
                  statDB = "ANY",
                  histNodesDB = "ANY",
                  admindbname="character",
                  adminDB="ANY"
              ),
              contains="BNEngine",
              methods = list(
                  initialize =
                    function(app="default",warehouse=NULL, listenerSet=NULL,
                             dburi="mongodb://localhost",
                             dbname="EARecords",admindbname="Proc4",
                             profModel=character(),waittime=.25,
                             statistics=list(), histNodes=character(),
                             processN=Inf,errorRestart="checkNoScore",
                             ...) {
                      if (is.null(warehouse))
                        stop("Null warehouse.")
                      flog.info("Connecting to database %s/%s\n",dburi,dbname)
                      callSuper(app=app,dburi=dburi,dbname=dbname,
                                warehouse=warehouse,statDB=NULL,
                                manifestDB=NULL, evidenceDB=NULL,
                                srs=NULL,listenerSet=listenerSet,
                                histNodesDB=NULL,
                                admindbname=admindbname,adminDB=NULL,
                                statistics=statistics,
                                histNodes=histNodes,profModel=profModel,
                                waittime=waittime, processN=processN,
                                errorRestart=errorRestart[1], 
                               ...)
                  },
                  manifestdb = function() {
                    if (is.null(manifestDB)) {
                      manifestDB <<- mongo("Manifest",dbname,dburi)
                    }
                    manifestDB
                  },
                  statdb = function() {
                    if (is.null(statDB)) {
                      statDB <<- mongo("Statistics",dbname,dburi)
                    }
                    statDB
                  },
                  fetchStats = function() {
                    statdb()$find(buildJQuery(app=app))
                  },
                  saveStats = function(stats) {
                    statdb()$remove(buildJQuery(app=app))
                    stats$app <- app
                    statdb()$insert(stats)
                  },
                  evidenceSets = function() {
                    if (is.null(evidenceDB)) {
                      evidenceDB <<- mongo("EvidenceSets",dbname,dburi)
                    }
                    evidenceDB
                  },
                  setError= function (mess,e) {
                    markAsError(mess,evidenceSets(),e)
                  },
                  fetchNextEvidence = function() {
                    getOneRec(buildJQuery(app=app,processed=FALSE),
                              evidenceSets(),parseEvidence,
                              sort = c(timestamp = 1))
                  },
                  histNodesdb = function() {
                    if (is.null(histNodesDB)) {
                      histNodesDB <<- mongo("histNodes",dbname,dburi)
                    }
                    histNodesDB
                  },
                  getHistNodes = function() {
                    if (length(histNodes) == 0L) {
                      histNodes <<-
                        histNodesdb()$find(buildJQuery(app=app))$Nodes[[1]]
                    }
                    histNodes
                  },
                  setHistNodes = function(nodenames) {
                    if (!is.character(nodenames))
                      stop("Expected nodenames to be a character expression.")
                    histNodes <<- nodenames
                    oldrec <- histNodesdb()$find(buildJQuery(app=app))
                    nodestring <- paste('"',nodenames,'"', sep="", collapse=",")
                    if (is.null(oldrec) || nrow(oldrec)==0L)
                      histNodesdb()$insert(
                                       sprintf('{"app":"%s", "Nodes":[%s]}',
                                               app, nodestring))
                    else
                      histNodesdb()$update(buildJQuery(app=app),
                                           sprintf('{"$set":{"Nodes":[%s]}}',
                                                   nodestring))
                    histNodes
                  },
                  studentRecords = function() {
                    if (is.null(srs)) {
                      srs <<- StudentRecordSet(app=app,warehouse=warehouse(),
                                              dbname=dbname,dburi=dburi)
                    }
                    srs
                  },
                  fetchManifest = function() {
                    manifestdb()$find(buildJQuery(app=app))
                  },
                  saveManifest = function(manif) {
                    manifestdb()$remove(buildJQuery(app=app))
                    manif$app <- app
                    manifestdb()$insert(manif)
                  },
                  admindb = function () {
                    if(is.null(adminDB))
                      adminDB <<- mongo("AuthorizedApps",admindbname,dburi)
                    adminDB
                  },
                  activate = function() {
                    if (length(admindb()$find(buildJQuery(app=app)))==0L) {
                      admindb()$insert(buildJQuery(app=app,
                                                   appStem=basename(app),
                                                   EAactive=TRUE,
                                                   EAsignal="running"))
                    } else {
                      admindb()$update(buildJQuery(app=app),
                                    '{"$set":{"EAactive":true,"EAsignal":"running"}}')
                    }
                  },
                  deactivate = function() {
                    if (length(admindb()$find(buildJQuery(app=app)))==0L) {
                      admindb()$insert(buildJQuery(app=app,
                                                   appStem=basename(app),
                                                   EAactive=FALSE))
                    } else {
                      admindb()$update(buildJQuery(app=app),
                                    '{"$set":{"EAactive":false}}')
                    }
                  },
                  isActivated = function() {
                    rec <- admindb()$find(buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(FALSE)
                    return (isTRUE(as.logical(rec$EAactive)))
                  },
                  shouldHalt = function() {
                    rec <- admindb()$find(buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(FALSE)
                    if (toupper(rec$EAsignal)=="HALT") return(TRUE)
                    FALSE
                  },
                  stopWhenFinished = function() {
                    rec <- admindb()$find(buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(TRUE)
                    if (toupper(rec$EAsignal)!="RUNNING") return(TRUE)
                    FALSE
                  },
                  show = function() {
                    methods::show(paste("<EABN: ",app,", DB:", dbname,">"))
                  }))



BNEngineMongo <- function(app="default",warehouse,listenerSet=NULL,
                     dburi="mongodb://localhost", dbname="EARecords",
                     processN=Inf,
                     admindbname="Proc4", waittime=.25, profModel=character(),
                     errorRestart=c("checkNoScore", "stopProcessing",
                       "scoreAvailable"),
                     ...) {
  ## Drop ... from new() so we can quietly delete unused arugments.
  new("BNEngineMongo",app=app,listenerSet=listenerSet,
      warehouse=warehouse,dburi=dburi, dbname=dbname,processN=processN,
      admindbname=admindbname,waittime=waittime,profModel=profModel,
      errorRestart=errorRestart[1])
}

