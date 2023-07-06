

trimTable <- function (tab, lastcol="Description") {
  nlcol <- which(colnames(tab)==lastcol)
  result <- tab[,1:nlcol]
  ## Need this as leading/trailing ws in column names is invisible
  ## in Google sheets (& M$ Excel)
  names(result) <- trimws(names(result),whitespace="[ \t\r\n.]")
  result
}

doBuild <- function (sess, EA.tables,  config.dir,
                     override=FALSE) {

  netdir <- ifelse(!is.null(EA.tables$netdir),EA.tables$netdir,"nets")
  tabdir <- ifelse(!is.null(EA.tables$netdir),EA.tables$tabdir,"tables")

  locks <- list.files(file.path(config.dir,netdir),pattern=".lock$")
  if (length(locks) > 0L) {
    flog.warn("Configuration directory locked by other applications: %s",
              paste(locks,collapse=", "))
    if (override==FALSE) stop("Directory locked.")
  }
  flog.trace("Locking directory.")
  file.create(file.path(config.dir,netdir,"netbuilder.lock"))
  on.exit ({
    flog.trace("Unlocking directory.")
    file.remove(file.path(config.dir,netdir,"netbuilder.lock"))
  })

  ## TODO -- Replace this with calls to googlesheets
  tid <- EA.tables$TableID
  flog.info("Downloading spreadsheets from %s",tid)
  if (!is.null(tid) && nchar(tid)>0L) {
    script <- ifelse(!is.null(EA.tables$downloadScript),
                 EA.tables$downloadScript,"download.sh")
    status <- system2(file.path(config.dir,tabdir,script),
                      c(file.path(config.dir,tabdir),tid,
                      stderr=TRUE,stdout=TRUE))
    if (!is.null(attr(status,"status"))) {
      flog.error("Got error when loading spreadsheet.")
      flog.error("Error:",status,capture=TRUE)
      return (NULL)
    }
  }

  flog.info("Loading tables into R.")
  templateURL <- file.path(config.dir,tabdir,"%s.csv")

  stattab <- withFlogging({
    trimTable(read.csv(sprintf(templateURL,EA.tables$StatName),
                       stringsAsFactors=FALSE,strip.white=TRUE),"Node")
  }, context=sprintf("Loading file %s.csv.",EA.tables$StatName))

  netman <- withFlogging({
    trimTable(read.csv(sprintf(templateURL,EA.tables$NetsName),
                       stringsAsFactors=FALSE,strip.white=TRUE))
  }, context=sprintf("Loading file %s.csv.",EA.tables$NetsName))

  nodeman <- withFlogging({
    tab1 <- trimTable(read.csv(sprintf(templateURL,EA.tables$NodesName),
                               stringsAsFactors=FALSE,strip.white=TRUE),
                      "UpperBound")
  }, context=sprintf("Loading file %s.csv.",EA.tables$NodesName))

  Omega <- withFlogging({
    trimTable(read.csv(sprintf(templateURL,EA.tables$OmegaName),
                       stringsAsFactors=FALSE,strip.white=TRUE),"PriorWeight")
  }, context=sprintf("Loading file %s.csv.",EA.tables$OmegaName))

  QQ <- withFlogging({
    trimTable(read.csv(sprintf(templateURL,EA.tables$QName),
                       stringsAsFactors=FALSE,strip.white=TRUE),"PriorWeight")
  }, context=sprintf("Loading file %s.csv.",EA.tables$QName))

  if (is(stattab,'try-error') || is(netman,'try-error') ||
      is(nodeman,'try-error') || is(Omega,'try-error') ||
      is(QQ,'try-error')) {
    flog.fatal("Failed to read one or more table, giving up.")
    return(NULL)
  }

  flog.info("Building Models.")
  Nethouse <- PNetica::BNWarehouse(manifest=netman,session=sess,
                                   address=file.path(config.dir,netdir),
                                   key="Name")
  Nodehouse <- PNetica::NNWarehouse(manifest=nodeman,
                                    key=c("Model","NodeName"),
                                    session=sess)

  profModel <- EA.tables$profModel
  if (is.null(profModel)) {
    profModel <- na.omit(netman$Hub)[1]
  }
  contex <- sprintf("Building proficiency model: %s.",profModel)
  flog.info(contex)
  if (any(is.na(Omega$Node))) {
    flog.error("Missing node name in rows: ",
               which(is.na(Omega$Node))+1,capture=TRUE)
  }
  CM <- withFlogging({
    CM <- WarehouseSupply(Nethouse,profModel)
    Omega2Pnet(Omega,CM,Nodehouse,override=EA.tables$profOverride)
  },context=contex)
  if (is(CM,'try-error')) {
    flog.fatal("Failed to build competentcy model, giving up.")
    return(NULL)
  }

  contex <- ("Building Evidence Models.")
  flog.info(contex)
  if (any(is.na(QQ$Model))) {
    flog.error("Missing model name in rows: ",
               which(is.na(QQ$Model))+1,capture=TRUE)
  }
  if (any(is.na(QQ$Node))) {
    flog.error("Missing node name in rows: ",
               which(is.na(QQ$Node))+1,capture=TRUE)
  }
  withFlogging(
      Qmat2Pnet(QQ,Nethouse,Nodehouse,override=EA.tables$emOverride),
      context=contex)

  flog.info("Writing nets.")
  withFlogging({
    manifestFile <- ifelse(!is.null(EA.tables$manifestFile),
                           EA.tables$manifestFile, "NetManifest.csv")
    write.csv(netman,file.path(config.dir,netdir,manifestFile))

    for (name in netman$Name) {
      if (nchar(name)>0L) {
        net <- WarehouseSave(Nethouse,name)
      }
    }
    statFile <- ifelse(!is.null(EA.tables$statFile),
                       EA.tables$statFile, "StatisticList.csv")
    write.csv(stattab,file.path(config.dir,netdir,statFile))
  }, context="Writing Nets.")

}

doRunrun <- function (appid, sess, EA.config,  EAeng.local, config.dir,
                      outdir=config.dir, override = FALSE, logfile="",
                      noprep=FALSE) {

  netdir <- ifelse(!is.null(EA.config$netdir),EA.config$netdir,"nets")
  sappid <- basename(appid)
  dburi <- EAeng.local$dburi
  if (!is.null(dburi) && dburi == "") dburi <- NULL
  sslops <- EAeng.local$ssloptions
  if (is.null(sslops)) sslops <- mongolite::ssl_options()
  dbname <- EAeng.local$dbname
  if (is.null(dbname)) dbname<-"EARecords"
  admindbname <- EAeng.local$admindbname
  if (is.null(admindbname)) admindbname<-"Proc4"
  mongoverbose <- EAeng.local$mongoverbose
  if (is.null(mongoverbose)) mongoverbose <- FALSE

  flog.info("Building and configuring engine.")

  EAeng.params <- EA.config$EAEngine
  EAeng.params$dburi <- dburi
  if (is.null(dburi)) {
    EAeng.params$activeTest <- EAeng.local$activeTest
    srsDB <- mongo::MongoDB(noMongo=TRUE)
  } else {
    cnms <- EA.config$colnames
    EAeng.params$sslops <- sslops
    EAeng.params$eadbname <- dbname
    EAeng.params$admindbnam <- admindbname
    EAeng.params$mongoverbose <- mongoverbose
    srscol <- ifelse(is.null(cnms$srec),"StudentRecords",cnms$srec)
    srDB <- mongo::MongoDB(srscol,dbname,dburi,mongoverbose,options=sslops)
    EAeng.params$statcol <- ifelse(is.null(cnms$stats),"Statistics",cnms$stats)
    EAeng.params$manifestCol <- ifelse(is.null(cnms$manifest),"Manifest",cnms$manifest)
    EAeng.params$evidenceCol<- ifelse(is.null(cnms$evidence),"EvidenceSets",cnms$evidence)
    EAeng.params$histcol <- ifelse(is.null(cnms$hist),"histNodes",cnms$hist)
    EAeng.params$admincol- ifelse(is.null(cnms$stat),"AuthorizedApps",cnms$stat)

  }

  ## Force to character, JSON leave as list.
  EAeng.params$histNodes <- as.character(EAeng.params$histNodes)

  if (is.null(EA.config$listners)) {
    EAeng.params$listenerSet <- NULL # Listener set not used
  } else {
    EAeng.params$listenerSet <-
      withFlogging({
        buildListenerSet(sender= sub("<app>",sappid,EA.config$sender),
                        EA.config$listeners,appid,
                        EA.config$colnames$listenerSetLog,
                        dburi,sslops,EA.config$colnames$registry,
                        admindbname,mongoverbose=FALSE)
      }, context="Building listener set.")
    if (is(EAeng.params$listenerSet,'try-error')) {
      flog.fatal("Could not build listener set: %s",EAeng.params$listenerSet)
      stop(EAeng.params$listenerSet)
    }
    if (nchar(logfile)>0L) {
      registerOutput(EAeng.params$listenerSet,basename(logfile),logfile,
                     appid,"EA","log")
    }
  }
  netman <- read.csv(file.path(config.dir,netdir,
                             EA.config$EAEngine$manifestFile),
                   stringsAsFactors=FALSE,strip.white=TRUE)
  EAeng.params$warehouse <-
    withFlogging({
      PNetica::BNWarehouse(manifest=netman,
                           session=sess,key="Name",
                           address=file.path(config.dir,netdir))
    },context="Building Network Warehouse")
  flog.debug("Warehouse is a %s.",class(EAeng.params$warehous))

  EAeng.params$app <- appid
  EAeng.params$srs <- StudentRecordSet(appid,EAeng.params$warehouse,
                                       db=srsDB)
  eng <- withFlogging({
    do.call(ifelse(is.null(dburi),newBNEngineNDB,newBNEngineMongo),
            EAeng.params)
  }, context="Constructing Engine.")
  if (is(eng,'try-error')) {
    flog.fatal("Could not build engine: %s.",eng)
    stop(eng)
  }

  if (eng$isActivated()) {
    flog.warn("Engine for application %s already active.",sappid)
    if (!isTRUE(override)) stop("Application ",sappid," already active.")
  }


  status <- withFlogging({
    loadManifest(eng,netman)
    stattab <- read.csv(file.path(config.dir,netdir,
                                  EA.config$EAEngine$statFile),
                        stringsAsFactors=FALSE,strip.white=TRUE)
    configStats(eng,stattab)
  }, context="Configuring engine.")
  ## Currently continuing anyway.  Is this the right thing to do?


  if (!is.null(dburi) && !noprep) {
    flog.info("Preparing Database.")

    ## Clean Messages
    if (isTRUE(EA.config$filter$doRemove)) {
      cleanMessageQueue(eng$evdienceSets(),EA.config$filter$remove)
    }
    ## Clearing Student Records
    if (isTRUE(EA.config$SRreset)) {
      doClearSRS(eng$studentRecords())
    }

    ## Import
    data.dir <- EA.config$dataDir
    if (is.null(data.dir)) data.dir <- config.dir
    importMessages(eng$evidenceSets(),EA.config$importFile,data.dir)

    ## Purging Unused messages
    if (isTRUE(EA.config$filter$doPurge)) {
      cleanMessageQueue(eng$evidenceSet(),EA.config$filter$purge)
    }

    ## Setting Processed flag.
    if (isTRUE(EA.config$filter$doReprocess)) {
      resetProcessedMessages(eng$evidenceSets(),EA.config$filter$reprocess)
    }
  }
  setupDefaultSR(eng)

  if (!is.null(EA.config$listenerReset) && !noprep) {
    doResetListeners(eng$listenerSet,
                    as.character(EA.config$listenerReset),
                    appid)
  }

  if (EA.config$limitNN=="ALL") {
    es <- eng$evidenceSets()
    ## If this is an empty queue, then this should return 0
    eng$processN <- es$count()
  } else {
    eng$processN <- as.numeric(EA.config$limitNN)
  }


  flog.info("Beginning EA for application %s.",basename(appid))
  if (is.finite(eng$processN)) {
    flog.info("%d messages queued.",eng$processN)
  } else {
    flog.info("Running in server mode.")
  }

  file.create(file.path(config.dir,netdir,paste(sappid,"lock",sep=".")))
  withr::defer({
    file.remove(file.path(config.dir,netdir,paste(sappid,"lock",sep=".")))
    eng$deactivate()
  })
  if (eng$processN > 0)
    withFlogging(mainLoop(eng))

  rebuildOutputs(eng$listenerSet,appid,EA.config,outdir)

  invisible(eng)
}


rebuildOutputs <- function (listenerSet, appid, EA.config,  outdir) {

  sappid <- basename(appid)
  listeners <- listenerSet$listners

  if (!is.null(EA.config$statListener)) {
    sl <- listeners[[EA.config$statListener]]
    if (is.null(sl)) {
      flog.warn("Stat listener %s not found, skipping building stat file.",
                 EA.config$statListener)
    } else {
      sdat <- listenerDataTable(sl,NULL,appid)
      if (!is.null(sdat)) {
        fname <- gsub("<app>",sappid,EA.config$statfile)
        write.csv(sdat,file.path(outdir,fname))
        listenerSet$registerOutput(fname,file.path(outdir,fname),
                                                appid,"EA")
      }
    }
  }
  if (!is.null(EA.config$histListener)) {
    hl <- listeners[[EA.config$histListener]]
    if (is.null(hl)) {
      flog.warn("History listener %s not found, skipping building history file.",
                 EA.config$histListener)
    } else {
      hist <- buildAppHist(hl$messdb(),appid)
      if (isTRUE(nrow(hist) > 0L)) {
        hist$app <- basename(hist$app)
        fname <- gsub("<app>",sappid,EA.config$histfile)

        write.csv(hist,file.path(outdir,fname))
        listenerSet$registerOutput(fname,file.path(outdir,fname),
                                                appid,"EA")
      } else {
        flog.warn("No records in history file.")
      }

    }
  }
}



buildHistMat <- function (col, app, uid) {
  stat1 <- mdbFind(col,buildJQuery(app=app, uid=uid))
  stat1d <- lapply(stat1$data,function (d) flattenStats(parseData(d)))
  data.frame(stat1[,c("app","uid","context","timestamp")],
             do.call(rbind,stat1d))
}

buildAppHist <- function (col, app) {
  uids <- col$distinct("uid",buildJQuery(app=app))
  do.call(rbind,
          lapply(uids,function(u) buildHistMat(col, app, u)))
}

doClearSRS <- function (srs) {
  flog.debug("Clearing old student records.")
  status <- withFlogging({
    srs$clearAll(TRUE)
    ##Clear default, as we will set
    ## it back up in a moment.
  },context = "Clearning old student records.")
  if (is(status,'try-error')) {
    flog.fatal("Error during database clearing: %s.",
               status)
    stop(status)
  }
}

doResetListeners <- function (ls,whichOnes,appid) {
  flog.info("Reseting Listners.")
  status <- withFlogging({
    resetListeners(ls,whichOnes, appid)
  }, context="Resetting Listeners.")
  if (is(status,'try-error')) {
    flog.fatal("Error while resetting listeners.",status)
    stop(status)
  }
}



