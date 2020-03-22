#######################################################
### Manifest Manipulation

loadManifest <- function(eng,manifest=data.frame()) {
  if (missing(manifest)) {
    manifest <- eng$fetchManifest()
  } else {
    eng$saveManifest(manifest)
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
  clearSRs(eng$studentRecords())
  if (length(eng$profModel) > 0L) {
    flog.info("Using proficieny model %s.",eng$profModel)
    sm(dsr) <- WarehouseSupply(eng$warehouse(),eng$profModel)
    if (is.null(dsr@sm))
      flog.warn("Proficiency Model %s not found.",eng$profModel)
  }
  if (is.null(sm(dsr))) {
    flog.debug("No proficiency model named, trying to get from manifest.")
    manf <-WarehouseManifest(eng$warehouse())
    pMod <-manf$Name[manf$Hub==""]
    flog.info("Using proficiency model %s from warehouse.",pMod)
    sm(dsr) <- WarehouseSupply(eng$warehouse(),pMod)
    if (is.null(sm(dsr)))
      flog.error('Proficiency Model "%s" and backup "%s" not found.',
                 eng$profModel,pMod)
  }
  if (is.null(sm(dsr))) stop("Proficiency Model not found.")
  PnetCompile(sm(dsr))
  dsr <- updateStats(eng,dsr)
  dsr <- baselineHist(eng,dsr)
  eng$srs$defaultSR <- dsr
  saveSR(eng$srs,dsr)
  announceStats(eng,dsr)
}

getRecordForUser <- function(eng,uid,srser=NULL) {
  rec <- getSR(eng$studentRecords(),uid,srser)
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
    statmat <- eng$fetchStats()
  } else {
    eng$saveStats(statmat)
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


updateStats <- function(eng,rec,debug=0) {
  rec@stats <- lapply(eng$stats(),
                      function (stat) calcStat(stat,sm(rec)))
  names(rec@stats) <- sapply(eng$stats(),StatName)
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

uphist <- function (sm,vname,past,eventname, debug=0) {
  node <- PnetFindNode(sm,vname)
  marg <- PnodeMargin(sm,node)
  hist <- rbind(past,marg)
  rownames(hist)[nrow(hist)] <- eventname
  if (interactive() && debug>2) utils::recover()
  hist
}

updateHist <- function(eng,rec,evidMess, debug=0) {
  eventname <- toString(evidMess)
  rec@hist <- lapply(eng$getHistNodes(), function (nd)
    uphist(sm(rec),nd,history(rec,nd),eventname, debug))
  names(rec@hist) <- eng$getHistNodes()
  rec
}



################
## Big Update Function

logEvidence <- function (eng,rec,evidMess) {
  seqno(evidMess) <- seqno(rec)+1L
  evidMess
}

accumulateEvidence <- function(eng,rec,evidMess, debug=0) {
  withFlogging({
    rec1 <- StudentRecord(app=app(eng),uid=uid(rec),
                          context=context(evidMess),
                          timestamp=timestamp(evidMess),
                          evidence=c(evidence(rec),m_id(evidMess)),
                          sm=sm(rec),stats=stats(rec),hist=rec@hist,
                          seqno=seqno(evidMess),prev_id=m_id(rec))
    rec1 <- updateSM(eng,rec1,evidMess, debug)
    if (interactive() && debug>1) utils::recover()
    rec1 <- updateStats(eng,rec1, debug)
    if (interactive() && debug>1) utils::recover()
    rec1 <- updateHist(eng,rec1,evidMess, debug)
    if (interactive() && debug>1) utils::recover()
    announceStats(eng,rec1)
    rec1 <- saveSR(eng$studentRecords(),rec1)
    rec1
  },evidence=evidMess,
  context=sprintf("Proccesing %s for user %s, seqno %d",
                  context(evidMess),uid(evidMess),seqno(evidMess)))
}

updateSM <- function (eng,rec,evidMess, debug=0) {
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
  obs <- PnetAdjoin(sm(rec),em)
  names(obs) <- sapply(obs,PnodeName)   #Use the (long) truenames!
  PnetCompile(sm(rec))
  flog.trace("Evidence:",details(evidMess),capture=TRUE)
  if (interactive() && debug>1) utils::recover()

  for (oname in names(observables(evidMess))) {
    if(!is.null(obs[[oname]])) {
      flog.trace("Processing observable %s.",oname)
      oval <- observables(evidMess)[[oname]]
      if (is.null(oval) || is.na(oval) || length(oval)==0L) {
        flog.trace("Observable %s is null/NA, skipping.", oname)
      } else {
        flog.trace("Setting observable %s to %s",oname,as.character(oval))
        PnodeEvidence(obs[[oname]]) <- oval
      }
    } else {
      flog.trace("Skipping observable %s:  not a node.",oname)
    }
  }
  if (flog.threshold()=="TRACE") {
    for (ob in obs) {
      flog.trace("Observable %s has value %s.",PnodeName(ob),
                 PnodeEvidence(ob))
    }
  }
  if (interactive() && debug>0) utils::recover()
  PnetDetach(sm(rec),em)
  PnetCompile(sm(rec))
  rec
}

handleEvidence <- function (eng, evidMess, srser=NULL, debug=0) {
  uid <- uid(evidMess)
  context <- context(evidMess)
  flog.debug("Processing Record for user %s, context: %s",uid,context)
  rec <- getRecordForUser(eng,uid,srser)
  evidMess <- logEvidence(eng,rec,evidMess)
  if (interactive() && debug>1) utils::recover()
  out <- accumulateEvidence(eng,rec,evidMess,debug)
  if (interactive() && debug>1) utils::recover()
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
