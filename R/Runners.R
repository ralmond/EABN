

trimTable <- function (tab, lastcol="Description") {
    nlcol <- which(colnames(tab)==lastcol)
    result <- tab[,1:nlcol]
    ## Need this as leading/trailing ws in column names is invivible in Google sheets (& M$ Excel)
    names(result) <- trimws(names(result),whitespace="[ \t\r\n.]")
    result
}

doBuild <- function (sess, EA.tables,  config.dir, override=FALSE) {

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
    trimTable(read.csv(sprintf(templateURL,EA.tables$NodesName),
                       stringsAsFactors=FALSE,strip.white=TRUE),"UpperBound")
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
                      outdir=config.dir, override = FALSE, logfile="") {

  netdir <- ifelse(!is.null(EA.config$netdir),EA.config$netdir,"nets")
  sappid <- basename(appid)
  dburi <- EAeng.local$dburi

  flog.info("Building and configuring engine.")
  listeners <- lapply(EA.config$listeners, buildListener,appid,dburi)
  names(listeners) <- sapply(listeners,listenerName)


  EAeng.params <- c(EA.config$EAEngine,
                    EAeng.local[setdiff(names(EAeng.local),
                                        names(EA.config$EAEngine))])
  ## Force to character, JSON leave as list.
  EAeng.params$histNodes <- as.character(EAeng.params$histNodes)
  EAeng.params$listenerSet <-
    withFlogging({
      ListenerSet(sender= sub("<app>",sappid,EA.config$sender),
                  dbname=EAeng.local$dbname, dburi=EAeng.local$dburi,
                  listeners=listeners,admindbname=EAeng.local$admindbname,
                  colname=EA.config$lscolname)
    }, context="Building listener set.")
  if (is(EAeng.params$listenerSet,'try-error')) {
    flog.fatal("Could not build listener set: %s",EAeng.params$listenerSet)
    stop(EAeng.params$listenerSet)
  }
  if (nchar(logfile)>0L) {
    EAeng.params$listenerSet$registerOutput(basename(logfile),logfile,
                                              appid,"EA","log")
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
  EAeng.params$app <- appid
  eng <- withFlogging({
    do.call(ifelse(EAeng.local$dburi=="",BNEngineNDB,BNEngineMongo),
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
  },context="Configuring engine.")
  ## Currently continuing anyway.  Is this the right thing to do?

  if (dburi != "") {
    flog.info("Preparing Database.")

    ## Clearning
    if (isTRUE(EA.config$filter$doRemove)) {
      flog.debug("Removing old evidence sets.")
      remquery <- EA.config$filter$remove
      status <- withFlogging({
        if (!is.null(names(remquery)))
          remquery <- list(remquery)      #Single query make it multiple.
        for (rq in remquery) {
          rquery <- do.call(buildJQuery,c(list(app=appid),rq))
          flog.trace("Removing %s",rquery)
          eng$evdienceSets()$remove(rquery)
        }
      }, context=sprintf("removing old evidence sets: %s.",
                         paste(remquery,collapse=", ")))
      if (is(status,'try-error')) {
        flog.fatal("Error during database event set removal: %s.",
                   status)
        stop(status)
      }
    }
    ## Clearing Student Records
    if (isTRUE(EA.config$SRreset)) {
      flog.debug("Clearing old student records.")
      status <- withFlogging({
        eng$studentRecords()$clearAll(TRUE)   #Clear default, as we will set
                                        #it back up in a moment.
      },context = "Clearning old student records.")
      if (is(status,'try-error')) {
        flog.fatal("Error during database clearing: %s.",
                   status)
        stop(status)
      }
    }

    ## Import
    for (fil in EA.config$importFile) {
      flog.info("Importing from file  %s.", fil)
      impf <- file.path(config.dir,fil)
      if (!file.exists(impf)) {
        flog.warn("File %s does not exist, skipping import.",
                  EA.config$importFile)
      } else {
        status <-
          system2("mongoimport",c("--jsonArray",
                                  "-d",EAeng.local$dbname,
                                  "-c","EvidenceSets",
                                  impf), stdout=TRUE, stderr=TRUE)
        if (!is.null(attr(status,"status"))) {
          flog.fatal("Got error when loading import file.")
          flog.fatal("Error:",status,capture=TRUE)
          stop(status)
        }
      }
    }

    ## Purging New Evidence sets.
    if (isTRUE(EA.config$filter$doPurge)) {
      flog.debug("Purging unwanted evidence sets.")
      purquery <- EA.config$filter$purge
      status <- withFlogging({
        if (!is.null(names(purquery)))
          purquery <- list(purquery)      #Single query make it multiple.
        for (pq in purquery) {
          pquery <- do.call(buildJQuery,c(list(app=appid),pq))
          flog.trace("Purging %s",pquery)
          eng$evdienceSets()$remove(pquery)
        }
      }, context=sprintf("Purging new evidence sets: %s.",
                         paste(remquery,collapse=", ")))
      if (is(status,'try-error')) {
        flog.fatal("Error during evidence set purging: %s.",
                   status)
        stop(status)
      }
    }

    ## Setting Processed flag.
    if (isTRUE(EA.config$filter$doReprocess)) {
      flog.debug("Clearing processed flags.")
      repquery <- EA.config$filter$reprocess
      status <- withFlogging({
        if (!is.null(names(repquery)))
          repquery <- list(repquery)      #Single query make it multiple.
        for (rq in repquery) {
          rquery <- do.call(buildJQuery, c(list(app=appid),rq))
          flog.trace("Reprocessing %s",rquery)
          eng$evidenceSets()$update(rquery,'{"$set":{"processed":false}}',
                                    multiple=TRUE)
        }
      }, context=sprintf("Clearing Processed Flag: %s.",
                         paste(repquery,collapse=", ")))
      if (is(status,'try-error')) {
        flog.fatal("Error while clearing processed: %s.",
                   status)
        stop(status)
      }
    }
  }
  setupDefaultSR(eng)
  flog.info("Reseting Listners.")
  if (!is.null(EA.config$listenerReset)) {
    status <- withFlogging({
      resetListeners(eng$listenerSet,as.character(EA.config$listenerReset),
                     appid)
    }, context="Resetting Listeners.")
    if (is(status,'try-error')) {
      flog.fatal("Error while resetting listeners.",status)
      stop(status)
    }
  }

  if (EA.config$limitNN=="ALL") {
    es <- eng$evidenceSets()
    if (!is.null(es)) {
      eng$processN <- es$count(buildJQuery(app=appid,processed=FALSE))
    } else {
      ## Want zero here as in DB-less mode we just want to exit
      ## returning the engine.  
      eng$processN <- 0 
    }
  } else {
    eng$processN <- as.numeric(EA.config$limitNN)
  }


  flog.info("Beginning EA for application %s.",basename(appid))
  if (is.finite(eng$processN)) {
    flog.info("%d messages queued.",eng$processN)
  } else {
    flog.info("Running in server mode.")
  }
  tryCatch({
    file.create(file.path(config.dir,netdir,paste(sappid,"lock",sep=".")))
    if (eng$processN > 0)
      withFlogging(mainLoop(eng))
  },finally={
    file.remove(file.path(config.dir,netdir,paste(sappid,"lock",sep=".")))
    eng$deactivate()
  })

  if (!is.null(EA.config$statListener)) {
    sl <- eng$listenerSet$listeners[[EA.config$statListener]]
    if (is.null(sl)) {
      flog.warn("Stat listener %s not found, skipping building stat file.",
                 EA.config$statListener)
    } else {
      stat1 <- sl$messdb()$find(buildJQuery(app=appid))
      if (isTRUE(nrow(stat1) > 0L)) {
        sdat <- data.frame(stat1[,c("app","uid","context","timestamp")],
                           do.call(cbind,stat1$data))
        sdat$app <- basename(sdat$app)
        fname <- gsub("<app>",sappid,EA.config$statfile)

        write.csv(sdat,file.path(outdir,fname))
        EAeng.params$listenerSet$registerOutput(fname,file.path(outdir,fname),
                                                appid,"EA")
      } else {
        flog.warn("No records in statistics file.")
      }

    }
  }
  if (!is.null(EA.config$histListener)) {
    hl <- eng$listenerSet$listeners[[EA.config$histListener]]
    if (is.null(hl)) {
      flog.warn("History listener %s not found, skipping building history file.",
                 EA.config$histListener)
    } else {
      hist <- buildAppHist(hl$messdb(),appid)
      if (isTRUE(nrow(hist) > 0L)) {
        hist$app <- basename(hist$app)
        fname <- gsub("<app>",sappid,EA.config$histfile)

        write.csv(hist,file.path(outdir,fname))
        EAeng.params$listenerSet$registerOutput(fname,file.path(outdir,fname),
                                                appid,"EA")
      } else {
        flog.warn("No records in history file.")
      }

    }
  }
  invisible(eng)
}


rebuildOutputs <- function (appid, EA.config,  EAeng.local, outdir) {

  sappid <- basename(appid)
  dburi <- EAeng.local$dburi

  flog.info("Building and configuring engine.")
  listeners <- lapply(EA.config$listeners, buildListener,appid,dburi)
  names(listeners) <- sapply(listeners,listenerName)
  listenerSet <-
    withFlogging({
      ListenerSet(sender= sub("<app>",sappid,EA.config$sender),
                  dbname=EAeng.local$dbname, dburi=EAeng.local$dburi,
                  listeners=listeners,admindbname=EAeng.local$admindbname,
                  colname=EA.config$lscolname)
    }, context="Building listener set.")

  if (!is.null(EA.config$statListener)) {
    sl <- listeners[[EA.config$statListener]]
    if (is.null(sl)) {
      flog.warn("Stat listener %s not found, skipping building stat file.",
                 EA.config$statListener)
    } else {
      stat1 <- sl$messdb()$find(buildJQuery(app=appid))
      if (isTRUE(nrow(stat1) > 0L)) {
        sdat <- data.frame(stat1[,c("app","uid","context","timestamp")],
                           do.call(cbind,stat1$data))
        sdat$app <- basename(sdat$app)
        fname <- gsub("<app>",sappid,EA.config$statfile)

        write.csv(sdat,file.path(outdir,fname))
        listenerSet$registerOutput(fname,file.path(outdir,fname),
                                                appid,"EA")
      } else {
        flog.warn("No records in statistics file.")
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
  stat1 <- col$find(buildJQuery(app=app, uid=uid))
  stat1d <- lapply(stat1$data,function (d) flattenStats(parseData(d)))
  data.frame(stat1[,c("app","uid","context","timestamp")],
             do.call(rbind,stat1d))
}

buildAppHist <- function (col, app) {
  uids <- col$distinct("uid",buildJQuery(app=app))
  do.call(rbind,
          lapply(uids,function(u) buildHistMat(col, app, u)))
}
