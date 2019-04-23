
BNEngine <-
  setRefClass("BNEngine",
              c(
                  app = "character",
                  dburi="character",
                  dbname="character",
                  session="BNSession",
                  manifestDB = "MongoDB",
                  evidenceDB = "MongoDB",
                  statDB = "MongoDB",
                  histNodesDB = "MongoDB",
                  srs = "StudentRecordSet",
                  profModel = "character",
                  listenerSet="ListenerSet",
                  statistics="list",
                  histNodes="character",
                  warehouseObj="EmptyWarehouse"
              ),
              methods = list(
                  initialize =
                    function(app="default",session=NULL, listeners=list(),
                             username="",password="", host="localhost",
                             port="",dbname="EARecords",
                             profModel=character(),
                             statistics=list(), histNodes=character(),
                             ...) {

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
                    srset <- StudentRecordSet(app=app,dbname=dbname,
                                              dburi=dburl)
                    callSuper(app=app,dburi=dburl,dbname=dbname,
                              session=session,statDB=NULL,
                              manifestDB=NULL, evidenceDB=NULL,
                              srs=srset,listenerSet=ls,
                              warehouseObj=NULL,histNodesDB=NULL,
                              statistics=statistics,
                              histNodes=histNodes,
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
                  evidenceSets = function() {
                    if (is.null(evidenceDB)) {
                      evidenceDB <<- mongo("EvidenceSets",dbname,dburi)
                    }
                    evidenceDB
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
                        histNodesdb()$ind(buildJQuery(app=tapp))$Nodes[[1]]
                    }
                    histNodes
                  }
                  setHistNodes = function(nodenames) {
                    if (!is.character(nodenames))
                      stop("Expected nodenames to be a character expression.")
                    histNodes <<- nodenames
                    histNodedb()$insert(sprintf('{"app":"%s", "Nodes":[%s]}',
                                                app,
                                                paste('"',nodenames,'"',sep="",
                                                      collapse=",")))
                    histNodes
                  },
                  studentRecords = function() {srs},
                  warehouse = function() {
                    if (is.null(warehouseObj)) {
                      warehouseObj <<- BNWarehouse(manifest=data.frame(),
                                                   session=session,
                                                   key="Name")
                      loadManifest(.self)
                    }
                    warehouseObj
                  },
                  setManifest = function(manifest) {
                    if (is.null(warehouseObj)) {
                      warehouseObj <<-BNWarehouse(manifest=manifest,
                                                  session=session,
                                                  key="Name")
                    } else {
                      warehouseObj <<- WarehouseManifest(warehouseObj,
                                                         manifest)
                    }
                  },
                  show = function() {
                    methods::show(paste("<EABN: ",app,">"))
                  }))


BNEngine <- function(app="default",session=NULL,listeners=list(),
                     username="",password="",host="localhost",
                     port="",dbname="EARecords",
                     ...) {
 new("BNEngine",app=app,session=session,listeners=listeners,username=username,
      password=password,host=host,port=port,dbname=dbname,...)
}

setMethod("app","BNEngine",function (x) x$app)



## Listener notification.
setMethod("notifyListeners","BNEngine",
           function(sender,mess) {
             sender$listenerSet$notifyListeners(mess)
           })

#######################################################
### Manifest Manipulation

loadManifest <- function(eng,manifest=data.frame()) {
  if (missing(manifest)) {
    manifest <- eng$manifestdb()$find(buildJQuery(app=app(eng)))
  } else {
    eng$manifestdb()$remove(buildJQuery(app=app(eng)))
    manifest$app <- app(eng)
    eng$manifestdb()$insert(manifest)
  }
  eng$setManifest(manifest)
  eng
}



################################
## Setup Default Student Record

## Fetch ProfModel
## Setup Histories
## Calc Initial Stats
## Log Initial Stats

setupDefaultSR <- function (eng) {
  dsr <- StudentRecord("*DEFAULT*",app=app(eng),context="*Baseline*")
  if (length(eng$profModel) > 0L) {
    flog.info("Using proficieny model %s.",eng$profModel)
    dsr@sm <- WarehouseFetch(eng$warehouse(),eng$profModel)
    if (is.null(dsr@sm))
      flog.warn("Proficiency Model %s not found.",eng$profModel)
  }
  if (is.null(dsr@sm)) {
    flog.debug("No proficiency model named, trying to get from manifest.")
    manf <-WarehouseManifest(eng$warehouse())
    pMod <-manf$Name[manf$Hub==""]
    flog.info("Using proficiency model %s from warehouse.",pMod)
    dsr@sm <- WarehouseFetch(eng$Warehouse(),pMod)
    if (is.null(dsr@sm))
      flog.error('Proficiency Model "%s" and backup "%s" not found.',
                 eng$profModel,pMod)
  }
  if (is.null(dsr@sm)) stop("Proficiency Model not found.")
  dsr <- updateStats(eng,dsr)
  dsr <- baselineHist(eng,dsr)
  eng$srs$defaultSR <- dsr
}


################
## Big Update Function

createDefaultSR <- function(eng){}

logEvidence <- function (eng,rec,evidMess) {}

accumulateEvidence <- function(eng,rec,evidMess) {
}

#############################################
## Statistics

configStats <- function(eng,statmat=data.frame()) {
  if (missing(statmat)) {
    statmat <- eng$statdb()$find(buildJQuery(app=app(eng)))
  } else {
    eng$statdb()$remove(buildJQuery(app=app(eng)))
    statmat$app <- app(eng)
    eng$statdb()$insert(statmat)
  }
  registerStats(eng,statmat)
  eng
}

## Statmat is a data.frame with columns name
registerStats <- function(eng,statmat) {
  rownames(statmat) <- statmat$Name
  eng$statistics <- sapply(statmat$Name,function (st)
    Statistic(statmat[st,"Fun"],statmat[st,"Node"],st))
  names(eng$statistics) <- statmat$Name
  eng
}


updateStats <- function(eng,rec) {
  rec@stats <- lapply(eng$statistics,
                      function (stat) calcStat(stat,sm(rec)))
  names(rec@stats) <- sapply(eng$statistics,name)
  rec
}

announceStats <- function(eng,rec) {
  mess <- P4Message(uid(rec),context(rec),sender="EABN",
                    mess="Statistics",timestamp=timestamp(rec),
                    details=stats(rec),app=eng$app)
  notifyListeners(eng,mess)
}

##########################################
## History

baselineHist <- function(eng,rec) {
  rec@hist <- lapply(eng$getHistNodes(),
                     function (nd) uphist(sm(rec),nd,NULL,"*Baseline*"))
  names(rec@hist) <- eng$getHistNodes()
  rec
}

uphist <- function (sm,vname,past,eventname) {
  node <- PnetFindNode(sm,vname)
  marg <- PnodeMargin(sm,node)
  hist <- rbind(past,marg)
  rownames(hist)[nrow(hist)] <- eventname
  hist
}

updateHist <- function(eng,rec,evidMess) {
  eventname <- toString(evidMess)
  rec@hist <- lapply(eng$getHistNodes(), function (nd)
    uphist(sm(rec),nd,history(rec,nd),eventname))
  names(rec@hist) <- eng$getHistNodes()
  rec
}


