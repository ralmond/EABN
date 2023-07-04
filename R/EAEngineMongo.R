BNEngineMongo <-
  setRefClass("BNEngineMongo",
              c(
                  manifestDB = "JSONDB",
                  statDB = "JSONDB",
                  histNodesDB = "JSONDB",
                  adminDB="JSONDB",
                  dbname="character"
              ),
              contains="BNEngine",
              methods = list(
                  initialize =
                    function(app="default",warehouse=NULL, listenerSet=NULL,
                             dbname="EARecords",
                             manifestDB=MongoDB("Manifest",dbname),
                             evidenceQueue=new("MongoQueue",app=app,
                                               messDB=MongoDB("EvidenceSets",dbname),
                                               builder=Proc4::buildMessage),
                             histNodesDB=MongoDB("histNodes",dbname),
                             statDB=MongoDB("Statistics",dbname),
                             adminDB=MongoDB("AuthorizedApps","Proc4"),
                             profModel=character(),waittime=.25,
                             statistics=list(), histNodes=character(),
                             processN=Inf,errorRestart="checkNoScore",
                             srs=NULL,
                             ...) {
                      callSuper(app=app,
                                warehouse=warehouse,
                                statDB=statDB,
                                manifestDB=manifestDB,
                                evidenceQueue=evidenceQueue,
                                srs=srs,
                                listenerSet=listenerSet,
                                histNodesDB=histNodesDB,
                                adminDB=adminDB,
                                statistics=statistics,
                                histNodes=histNodes,profModel=profModel,
                                waittime=waittime, processN=processN,
                                errorRestart=errorRestart[1],
                               ...)
                  },
                  manifestdb = function() {
                    manifestDB
                  },
                  statdb = function() {
                    statDB
                  },
                  fetchStats = function() {
                    mdbFind(statdb(),buildJQuery(app=app))
                  },
                  saveStats = function(stats) {
                    mdbRemove(statdb(),buildJQuery(app=app))
                    stats$app <- app
                    mdbInsert(statdb(),stats)
                  },
                  histNodesdb = function() {
                    histNodesDB
                  },
                  getHistNodes = function() {
                    if (length(histNodes) == 0L) {
                      histNodes <<-
                        mdbFind(histNodesdb(),buildJQuery(app=app))$Nodes[[1]]
                    }
                    histNodes
                  },
                  setHistNodes = function(nodenames) {
                    if (!is.character(nodenames))
                      stop("Expected nodenames to be a character expression.")
                    histNodes <<- nodenames
                    oldrec <- mdbFind(histNodesdb(),buildJQuery(app=app))
                    nodestring <- paste('"',nodenames,'"', sep="", collapse=",")
                    if (is.null(oldrec) || nrow(oldrec)==0L)
                      mdbInsert(histNodesdb(),
                                sprintf('{"app":"%s", "Nodes":[%s]}',
                                        app, nodestring))
                    else
                      mdbUpdate(histNodesdb(),
                                buildJQuery(app=app),
                                sprintf('{"$set":{"Nodes":[%s]}}',
                                        nodestring))
                    histNodes
                  },
                  fetchManifest = function() {
                    mdbFind(manifestdb(),buildJQuery(app=app))
                  },
                  saveManifest = function(manif) {
                    mdbRemove(manifestdb(),buildJQuery(app=app))
                    manif$app <- app
                    mdbInsert(manifestdb(),manif)
                  },
                  admindb = function () {
                      adminDB
                  },
                  activate = function() {
                    if (length(mdbFind(admindb(),buildJQuery(app=app)))==0L) {
                      mdbInsert(admindb(),
                                buildJQuery(app=app,
                                            appStem=basename(app),
                                            EAactive=TRUE,
                                            EAsignal="running"))
                    } else {
                      mdbUpdate(admindb(),
                                buildJQuery(app=app),
                                '{"$set":{"EAactive":true,"EAsignal":"running"}}')
                    }
                  },
                  deactivate = function() {
                    if (length(mdbFind(admindb(),buildJQuery(app=app)))==0L) {
                      mdbInsert(admindb(),
                                buildJQuery(app=app,
                                            appStem=basename(app),
                                            EAactive=FALSE))
                    } else {
                      mdbUpdate(admindb(),
                                buildJQuery(app=app),
                                '{"$set":{"EAactive":false}}')
                    }
                  },
                  isActivated = function() {
                    rec <- mdbFind(admindb(),buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(FALSE)
                    return (isTRUE(as.logical(rec$EAactive)))
                  },
                  shouldHalt = function() {
                    rec <- mdbFind(admindb(),buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(FALSE)
                    if (toupper(rec$EAsignal)=="HALT") return(TRUE)
                    FALSE
                  },
                  stopWhenFinished = function() {
                    rec <- mdbFind(admindb(),buildJQuery(app=app),limit=1)
                    if (length(rec)==0L) return(TRUE)
                    if (toupper(rec$EAsignal)!="RUNNING") return(TRUE)
                    FALSE
                  },
                  show = function() {
                    methods::show(paste("<EABN: ",app,", DB:", dbname,">"))
                  }))



newBNEngineMongo <- function(app="default",warehouse,
                          listenerSet=NULL,
                          processN=Inf,
                          statistics=list(),
                          dburi="mongodb://localhost",
                          sslops=mongolite::ssl_options(),
                          eadbname="EARecords",
                          admindbname="Proc4", waittime=.25,
                          profModel=character(),
                          histNodes=character(),
                          errorRestart=c("checkNoScore", "stopProcessing",
                                         "scoreAvailable"),
                          srcol="StudentRecords",
                          mongoverbose=FALSE,
                          srs=StudentRecordSet(app=app,warehouse=warehouse,
                                               db=MongoDB(srcol,eadbname,dburi,
                                                          verbose=mongoverbose,
                                                          options=sslops)),
                          manifestCol="Manifest",
                          manifestDB=MongoDB(manifestCol,eadbname,dburi,
                                             verbose=mongoverbose,
                                             options=sslops),
                          evidenceCol="EvidenceSets",
                          evidenceQueue=new("MongoQueue",app=app,
                                            messDB=MongoDB(evidenceCol,
                                                           eadbname,dburi,
                                                           verbose=mongoverbose,
                                                           options=sslops),
                                            builder=Proc4::buildMessage),

                          histcol="histNodes",
                          histNodesDB=MongoDB(histcol,eadbname,dburi,
                                              verbose=mongoverbose,
                                              options=sslops),
                          statcol="Statistics",
                          statDB=MongoDB(statcol,eadbname,dburi,
                                         verbose=mongoverbose,
                                         options=sslops),
                          admincol="AuthorizedApps",
                          adminDB=MongoDB(admincol,admindbname,dburi,
                                          verbose=mongoverbose,
                                          options=sslops),
                          ...) {
  ## Drop ... from new() so we can quietly delete unused arguments.
  if (is.null(warehouse)) stop("Warehouse must be supplied.")
  if (is.null(srs)) stop("Student record set must be supplied.")
  new("BNEngineMongo",app=app,dbname=eadbname,
      warehouse=warehouse,processN=processN,waittime=waittime,
      profModel=profModel, errorRestart=errorRestart[1],
      statDB=statDB,
      manifestDB=manifestDB,
      evidenceQueue=evidenceQueue,
      srs=srs,
      listenerSet=listenerSet,
      histNodesDB=histNodesDB,
      adminDB=adminDB,
      statistics=statistics,
      histNodes=histNodes)
}

