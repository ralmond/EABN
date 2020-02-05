BNMongoEngine <-
  setRefClass("BNMongoEngine",
              c(
                  dburi="character",
                  dbname="character",
                  manifestDB = "MongoDB",
                  evidenceDB = "MongoDB",
                  statDB = "MongoDB",
                  histNodesDB = "MongoDB",
                  P4dbname="character",
                  p4db="MongoDB",
                  warehouseObj="PnetWarehouse",
                  netDirectory="character",
              ),
              contains="BNEngine",
              methods = list(
                  initialize =
                    function(app="default",warehouse=NULL, listeners=list(),
                             username="",password="", host="localhost",
                             port="",dbname="EARecords",P4dbname="Proc4",
                             profModel=character(),waittime=.25,
                             statistics=list(), histNodes=character(),
                             processN=Inf,
                             ...) {
                      if (is.null(warehouse))
                        stop("Null warehouse.")
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
                                        dbname=dbname, dburi=dburl,
                                        listeners=listeners,
                                        colname="Messages",...)
                      callSuper(app=app,dburi=dburl,dbname=dbname,
                                warehouseObj=warehouse,statDB=NULL,
                                manifestDB=NULL, evidenceDB=NULL,
                                srs=NULL,listenerSet=ls,
                                warehouseObj=NULL,histNodesDB=NULL,
                                P4dbname=P4dbname,p4db=NULL,
                                statistics=statistics,
                                histNodes=histNodes,profModel=profModel,
                                waittime=waittime, processN=processN,
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
                    statdb()$find(buildJQuery(app=app(eng)))
                  },
                  saveStats = function(statmat) {
                    statdb()$remove(buildJQuery(app=app(eng)))
                    statmat$app <- app(eng)
                    statdb()$insert(statmat)
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
                    manifestdb()$find(buildJQuery(app=app(eng)))
                  },
                  saveManifest = function(manifest) {
                    manifestdb()$remove(buildJQuery(app=app(eng)))
                    manifest$app <- app(eng)
                    manifestdb()$insert(manifest)
                  },
                  P4db = function () {
                    if(is.null(p4db))
                      p4db <<- mongo("AuthorizedApps",P4dbname,dburi)
                    p4db
                  },
                  isActivated = function() {
                    rec <- P4db()$find(buildJQuery(app=app(eng)),limit=1)
                    if (length(rec)==0L) return(FALSE)
                    rec$active
                  },
                  activate = function() {
                    if (length(P4db()$find(buildJQuery(app=app(eng))))==0L) {
                      P4db()$insert(buildJQuery(app=app(eng),active=TRUE))
                    } else {
                      P4db()$update(buildJQuery(app=app(eng)),
                                    '{"$set":{"active":true}}')
                    }
                  },
                  show = function() {
                    methods::show(paste("<EABN: ",app,">"))
                  }))



BNMongoEngine <- function(app="default",session=NULL,listeners=list(),
                     username="",password="",host="localhost",
                     port="",dbname="EARecords",processN=Inf,
                     P4dbname="Proc4", waittime=.25, profModel=character(),
                     netDirectory=".",...) {
 new("BNMongoEngine",app=app,session=session,listeners=listeners,username=username,
     password=password,host=host,port=port,dbname=dbname,processN=processN,
     P4dbname=P4dbname,waittime=waittime,profModel=profModel,
     netDirectory=netDirectory,...)
}

