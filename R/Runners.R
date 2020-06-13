

trimTable <- function (tab, lastcol="Description") {
    nlcol <- which(colnames(tab)==lastcol)
    tab[,1:nlcol]
}

doBuild <- function (sess, EA.tables,  config.dir, override=FALSE) {

  netdir <- ifelse(!is.null(EA.tables$netdir),EA.tables$netdir,"nets")
  tabdir <- ifelse(!is.null(EA.tables$netdir),EA.tables$tabdir,"tables")

  locks <- list.files(file.path(config.dir,netdir),pattern=".lock")
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

  stattab <- read.csv(sprintf(templateURL,EA.tables$StatName),
                        stringsAsFactors=FALSE,strip.white=TRUE)
  stattab <- trimTable(stattab,"Node")
  netman <- read.csv(sprintf(templateURL,EA.tables$NetsName),
                     stringsAsFactors=FALSE,strip.white=TRUE)
  netman <- trimTable(netman)
  nodeman <- read.csv(sprintf(templateURL,EA.tables$NodesName),
                      stringsAsFactors=FALSE,strip.white=TRUE)
  nodeman <- trimTable(nodeman,"UpperBound")
  Omega <- read.csv(sprintf(templateURL,EA.tables$OmegaName),
                    stringsAsFactors=FALSE,strip.white=TRUE)
  Omega <- trimTable(Omega,"PriorWeight")
  QQ <- read.csv(sprintf(templateURL,EA.tables$QName),
                 stringsAsFactors=FALSE,strip.white=TRUE)
  QQ <- trimTable(QQ,"PriorWeight")


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
  flog.info("Building proficiency model: %s.",profModel)
  if (any(is.na(Omega$Node))) {
    flog.error("Missing node name in rows: ",
               which(is.na(Omega$Node))+1,capture=TRUE)
  }
  CM <- WarehouseSupply(Nethouse,profModel)
  CM <- Omega2Pnet(Omega,CM,Nodehouse,override=TRUE)

  flog.info("Building Evidence Models.")
  if (any(is.na(QQ$Model))) {
    flog.error("Missing model name in rows: ",
               which(is.na(QQ$Model))+1,capture=TRUE)
  }
  if (any(is.na(QQ$Node))) {
    flog.error("Missing node name in rows: ",
               which(is.na(QQ$Node))+1,capture=TRUE)
  }
  Qmat2Pnet(QQ,Nethouse,Nodehouse)

  flog.info("Writing nets.")

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

}

doRunrun <- function (app, sess, EA.config,  EAeng.local, config.dir,
                      outdir=config.dir, override = FALSE) {

  netdir <- ifelse(!is.null(EA.config$netdir),EA.config$netdir,"nets")
  sapp <- basename(app)
  dburi <- EAeng.local$dburi

  flog.info("Building and configuring engine.")
  listeners <- lapply(EA.config$listeners, buildListener,app,dburi)
  names(listeners) <- sapply(listeners,listenerName)

  EAeng.params <-
    c(EA.config$EAEngine,
      EAeng.local[setdiff(names(EAeng.local),names(EA.config$EAEngine))])
  EAeng.params$listenerSet <-
    ListenerSet(sender= sub("<app>",sapp,EA.config$sender),
                dbname=EAeng.local$dbname, dburi=EAeng.local$dburi,
                listeners=listeners,
                colname=EA.config$lscolname)
  netman <- read.csv(file.path(config.dir,netdir,
                               EA.config$EAEngine$manifestFile),
                     stringsAsFactors=FALSE,strip.white=TRUE)
  EAeng.params$warehouse <-
    PNetica::BNWarehouse(manifest=netman,
                         session=sess,key="Name",
                         address=file.path(config.dir,netdir))
  EAeng.params$app <- app
  eng <- do.call(ifelse(EAeng.local$dburi=="",BNEngineNDB,BNEngineMongo),
                 EAeng.params)
  if (eng$isActivated()) {
    flog.warn("Enging for application %s already active.",sapp)
    if (!isTRUE(override)) stop("Application ",sapp," already active.")
  }


  loadManifest(eng,netman)
  stattab <- read.csv(file.path(config.dir,netdir,
                                 EA.config$EAEngine$statFile),
                       stringsAsFactors=FALSE,strip.white=TRUE)
  configStats(eng,stattab)


  flog.info("Preparing Database.")
  if (dburi != "") {
    if (isTRUE(EA.config$filter$doRemove)) {
      flog.debug("Clearing old evidence sets.")
      remquery <- EA.config$filter$remove
      if (!is.null(names(remquery)))
        remquery <- list(remquery)      #Single query make it multiple.
      for (rq in remquery) {
        rquery <- do.call(buildJQuery,c(list(app=app),rq))
        flog.trace("Removing %s",rquery)
        eng$evdienceSets()$remove(rquery)
      }
    }
    if (isTRUE(EA.config$SRreset)) {
      flog.debug("Clearing old student records.")
      eng$studentRecords()$clearAll(TRUE)   #Clear default, as we will set
                                        #it back up in a moment.
    }
    for (fil in EA.config$importFile) {
      flog.info("Importing from file  %s.", fil)
      impf <- file.path(config.dir,fil)
      if (!file.exists(impf)) {
        flog.warn("File %s does not exist, skipping import.",
                  EA.confg$importFile)
      } else {
        status <-
          system2("mongoimport",c("--jsonArray",
                                  "-d",EAeng.local$dbname,
                                  "-c","EvidenceSets",
                                  impf), stdout=TRUE, stderr=TRUE)
        if (!is.null(attr(status,"status"))) {
          flog.error("Got error when loading import file.")
          flog.error("Error:",status,capture=TRUE)
        }
      }
    }
    if (isTRUE(EA.config$filter$doPurge)) {
      flog.debug("Purging unwanted evidence sets.")
      purquery <- EA.config$filter$purge
      if (!is.null(names(purquery)))
        purquery <- list(purquery)      #Single query make it multiple.
      for (pq in purquery) {
        pquery <- do.call(buildJQuery,c(list(app=app),pq))
        flog.trace("Purging %s",pquery)
        eng$evdienceSets()$remove(pquery)
      }
    }
    if (isTRUE(EA.config$filter$doReprocess)) {
      flog.debug("Clearing reprocessed flags.")
      repquery <- EA.config$filter$reprocess
      if (!is.null(names(repquery)))
        repquery <- list(repquery)      #Single query make it multiple.
      for (rq in repquery) {
        rquery <- do.call(buildJQuery, c(list(app=app),rq))
        flog.trace("Reprocessing %s",pquery)
        eng$evidenceSets()$update(rquery,'{"$set":{"processed":false}}',
                                  multiple=TRUE)
      }
    }
  }
  setupDefaultSR(eng)
  flog.info("Reseting Listners.")
  if (!is.null(EA.config$listenerReset)) {
    resetListeners(eng$listenerSet,as.character(EA.config$listenerReset),app)
  }

  if (EA.config$limitNN=="ALL") {
    eng$processN <- eng$evidenceSets()$count(buildJQuery(app=app,
                                                         processed=FALSE))
  } else {
    eng$processN <- as.numeric(EA.config$limitNN)
  }


  flog.info("Beginning EA for application %s.",basename(app))
  if (is.finite(eng$processN)) {
    flog.info("%d messages queued.",eng$processN)
  } else {
    flog.info("Running in server mode.")
  }
  tryCatch({
    file.create(file.path(config.dir,netdir,paste(sapp,"lock",sep=".")))
    mainLoop(eng)
  },finally={
    file.remove(file.path(config.dir,netdir,paste(sapp,"lock",sep=".")))
  })

  if (!is.null(EA.config$statListener)) {
    sl <- listeners[[EA.config$statListener]]
    if (is.null(sl)) {
      flog.warn("Stat listener %s not found, skipping building stat file.",
                 EA.config$statListener)
    } else {
      stat1 <- sl$messdb()$find(buildJQuery(app=app))
      sdat <- data.frame(stat1[,c("app","uid","context","timestamp")],
                         flattenStats(stat1$data))
      sdat$app <- basename(sdat$app)
      fname <- gsub("<app>",sapp,EA.config$statfile)
      write.csv(stat1,file.path(outdir,fname))
    }
  }

}

