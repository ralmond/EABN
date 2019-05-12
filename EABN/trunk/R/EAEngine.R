setClassUnion("NullRecordSet",c("StudentRecordSet","NULL"))


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
                  P4dbname="character",
                  p4db="MongoDB",
                  srs = "NullRecordSet",
                  profModel = "character",
                  listenerSet="ListenerSet",
                  statistics="list",
                  histNodes="character",
                  warehouseObj="EmptyWarehouse",
                  waittime="numeric",
                  processN="numeric"
              ),
              methods = list(
                  initialize =
                    function(app="default",session=NULL, listeners=list(),
                             username="",password="", host="localhost",
                             port="",dbname="EARecords",P4dbname="Proc4",
                             profModel=character(),waittime=.25,
                             statistics=list(), histNodes=character(),
                             processN=Inf,
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
                                        dbname=dbname, dburi=dburl,
                                        listeners=listeners,
                                        colname="Messages",
                                        ...)
                      callSuper(app=app,dburi=dburl,dbname=dbname,
                                session=session,statDB=NULL,
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
                  stats = function() {
                    if (length(statistics) == 0L)
                      configStats(.self)
                    statistics
                  },
                  evidenceSets = function() {
                    if (is.null(evidenceDB)) {
                      evidenceDB <<- mongo("EvidenceSets",dbname,dburi)
                    }
                    evidenceDB
                  },
                  setProcessed= function (mess) {
                    mess@processed <- TRUE
                    saveRec(mess,evidenceSets())
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
                    if (is.null(oldrec))
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
                    warehouse()         # Initialize Warehouse
                    WarehouseManifest(warehouseObj) <<- manifest
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


BNEngine <- function(app="default",session=NULL,listeners=list(),
                     username="",password="",host="localhost",
                     port="",dbname="EARecords",processN=Inf,
                     P4dbname="Proc4", waittime=.25, profModel=character(),
                     ...) {
 new("BNEngine",app=app,session=session,listeners=listeners,username=username,
     password=password,host=host,port=port,dbname=dbname,processN=processN,
     P4dbname=P4dbname,waittime=waittime,profModel=profModel,...)
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
  eng$studentRecords()                  #Make sure initialized
  dsr <- StudentRecord("*DEFAULT*",app=app(eng),context="*Baseline*")
  ## If an old record exists, clear it out.
  if (eng$srs$recorddb()$count(buildJQuery(app=app(dsr),uid=uid(dsr))) > 0L)
    eng$srs$recorddb()$remove(buildJQuery(app=app(dsr),uid=uid(dsr)))
  if (length(eng$profModel) > 0L) {
    flog.info("Using proficieny model %s.",eng$profModel)
    dsr@sm <- WarehouseSupply(eng$warehouse(),eng$profModel)
    if (is.null(dsr@sm))
      flog.warn("Proficiency Model %s not found.",eng$profModel)
  }
  if (is.null(dsr@sm)) {
    flog.debug("No proficiency model named, trying to get from manifest.")
    manf <-WarehouseManifest(eng$warehouse())
    pMod <-manf$Name[manf$Hub==""]
    flog.info("Using proficiency model %s from warehouse.",pMod)
    dsr@sm <- WarehouseSupply(eng$warehouse(),pMod)
    if (is.null(dsr@sm))
      flog.error('Proficiency Model "%s" and backup "%s" not found.',
                 eng$profModel,pMod)
  }
  if (is.null(dsr@sm)) stop("Proficiency Model not found.")
  CompileNetwork(dsr@sm)                #Replace with abstract version later.
  dsr <- updateStats(eng,dsr)
  dsr <- baselineHist(eng,dsr)
  eng$srs$defaultSR <- dsr
  saveSR(eng$srs,dsr)
  announceStats(eng,dsr)
}

getRecordForUser <- function(eng,uid) {
  rec <- getSR(eng$studentRecords(),uid)
  if (is.null(rec))
    stop("Could not find or generate student record for ",uid)
  if (isTRUE(seqno(rec)==0L))
    announceStats(eng,rec)
  rec
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
  rec@stats <- lapply(eng$stats(),
                      function (stat) calcStat(stat,sm(rec)))
  names(rec@stats) <- sapply(eng$stats(),name)
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



################
## Big Update Function

logEvidence <- function (eng,rec,evidMess) {
  seqno(evidMess) <- seqno(rec)+1L
  evidMess
}

accumulateEvidence <- function(eng,rec,evidMess) {
  withFlogging({
    rec1 <- StudentRecord(app=app(eng),uid=uid(rec),
                          context=context(evidMess),
                          timestamp=timestamp(evidMess),
                          evidence=c(evidence(rec),m_id(evidMess)),
                          sm=sm(rec),stats=stats(rec),hist=rec@hist,
                          seqno=seqno(evidMess),prev_id=m_id(rec))
    rec1 <- updateSM(eng,rec1,evidMess)
    rec1 <- updateStats(eng,rec1)
    rec1 <- updateHist(eng,rec1,evidMess)
    announceStats(eng,rec1)
    rec1 <- saveSR(eng$studentRecords(),rec1)
    rec1
  },evidence=evidMess,
  context=sprintf("Proccesing %s for user %s, seqno %d",
                  uid(evidMess),context(evidMess),seqno(evidMess)))
}

updateSM <- function (eng,rec,evidMess) {
  manf <-WarehouseManifest(eng$warehouse())
  emName <-manf[manf$Title==context(evidMess),"Name"]
  flog.debug("Evidence Model for level %s is %s",context(evidMess),
             paste(emName, collapse=", "))
  if (length(emName) != 1L) {
    flog.warn("No evidence model for context %s",context(evidMess))
    stop("No evidence model for context ",context(evidMess))
  }
  em <- WarehouseSupply(eng$warehouse(),emName)
  if (is.null(em)) {
    flog.error("No evidence model net for context %s",context(evidMess))
    stop("No evidence model net for context %s",context(evidMess))
  }
  obs <- AdjoinNetwork(sm(rec),em)
  CompileNetwork(sm(rec))

  for (oname in names(observables(evidMess))) {
    if(!is.null(obs[[oname]])) {
      flog.trace("Processing observable %s.")
      oval <- observables(evidMess)[[oname]]
      if (is.null(oval) || is.na(oval)) {
        flog.trace("Observable %s is null/NA, skipping.", oname)
      } else if (is.numeric(oval)) {
        NodeValue(obs[[oname]]) <- oval
      } else {
        ## Need to check for capitalization issues.
        ov1 <- oval
        sts <- NodeStates(obs[[oname]])
        if (!(ov1 %in% sts)) {
          ov1 <- sts[toupper(sts)==toupper(ov1)]
        }
        if (length(oval) > 0L) {
          flog.trace("Setting observable %s to %s.",oname,ov1)
          NodeFinding(obs[[oname]]) <- ov1
        } else {
          flog.warn("Processing observables for %s for %s:",context(evidMess),
                    uid(evidMess))
          flog.warn("Observable %s has unknown value %s, skipping.",oname,oval)
        }
      }
    } else {
      flog.trace("Skipping observable %s:  not a node.")
    }
  }
  AbsorbNodes(obs)
  CompileNetwork(sm(rec))
  rec
}

handleEvidence <- function (eng, evidMess) {
  uid <- uid(evidMess)
  context <- context(evidMess)
  flog.debug("Processing Record for user %s, context: %s",uid,context)
  rec <- getRecordForUser(eng,uid)
  evidMess <- logEvidence(eng,rec,evidMess)
  if (interactive() && FALSE) recover()
  out <- accumulateEvidence(eng,rec,evidMess)
  if (interactive() && FALSE) recover()
  eng$setProcessed(evidMess)
  if (is(out,'try-error')) {
    flog.warn("Processing %s for user %s generated error: %s",
              context,uid,toString(out))
    eng$setError(evidMess,out)
  }
  out
}

mainLoop <- function(eng) {
  withFlogging({
    flog.info("Evidence AccumulationEngine %s starting.", app(eng))
    active <- eng$isActivated()
    while (active) {
      eve <- eng$fetchNextEvidence()
      if (is.null(eve)) {
        ## Queue is empty, wait and check again.
        Sys.sleep(eng$waittime)
        ## Check for deactivation signal.
        active <- eng$isActivated()
      } else {
        handleEvidence(eng,eve)
        eng$setProcessed(eve)
        eng$processN <- eng$processN -1
        active <- eng$processN > 0
      }
    }
  flog.info("EA Engine %s was deactivated.",
            app(eng))
  },
  context=sprintf("Running EA Application %s",app(eng)))
  flog.info("Application Engine %s stopping.",app(eng))
}





