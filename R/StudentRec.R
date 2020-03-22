### Student Record ---  This is a wrapper for the Bayes net.
### For now, assume 1 SM per record


## This doesn't work because Pnet is an abstract class.
## setClassUnion("PnetOrNull",c("Pnet","NULL"))

setClass("StudentRecord",
         slots=c("_id"="character",
                 app="character",
                 uid="character",
                 context="character",
                 evidence="character",
                 timestamp="POSIXt",      #Date of last update.
                 sm="ANY",                #Damn Package Locking Anyway!
                 smser="list",
                 seqno="integer",
                 stats="list",
                 hist="list",
                 prev_id="character"))

setMethod("app","StudentRecord", function(x) x@app)
setMethod("uid","StudentRecord", function(x) x@uid)
setMethod("context","StudentRecord", function(x) x@context)
setMethod("timestamp","StudentRecord", function(x) x@timestamp)

setMethod("seqno","StudentRecord", function(x) x@seqno)
setMethod("seqno<-","StudentRecord", function(x,value) {
  x@seqno <- value
  x
})


setGeneric("stats",function(x) standardGeneric("stats"))
setGeneric("statNames",function (sr) standardGeneric("statNames"))
setGeneric("histNames",function (sr) standardGeneric("histNames"))
setGeneric("stat",function (sr,name) standardGeneric("stat"))
setGeneric("history",function (sr,name) standardGeneric("history"))

setMethod("stats","StudentRecord", function(x) x@stats)
setMethod("statNames","StudentRecord", function (sr) names(stats(sr)))
setMethod("stat",c("StudentRecord","character"),
          function (sr, name) stats(sr)[[name]])
setMethod("histNames","StudentRecord", function (sr) names(sr@hist))
setMethod("history",c("StudentRecord","character"),
          function (sr, name) sr@hist[[name]])


setGeneric("sm",function(x) standardGeneric("sm"))
setMethod("sm","StudentRecord", function(x) x@sm)
setGeneric("sm<-",function(x,value) standardGeneric("sm<-"))
setMethod("sm<-","StudentRecord", function(x,value) {
  if (!is.null(value) && !is.Pnet(value))
    stop("Must be a 'Pnet' or null.")
  x@sm <- value
  x
  })
fetchSM <- function (sr, warehouse) {
  sm(sr)<- WarehouseFetch(warehouse,as.legal.name(warehouse,uid(sr)))
  if (is.null(sm(sr)) || !is.valid(warehouse,sm(sr))) {
    sm(sr) <- unpackSM(sr,warehouse)
  }
  sr
}

unpackSM <- function (sr, warehouse) {
  if (length(sr@smser)==0L) {
    flog.error("No serialized verison of sm for %s",uid(sr))
    stop("No serialized version of sm to unpack.")
  }
  WarehouseUnpack(warehouse,sr@smser)
}

StudentRecord <- function(uid,context="",timestamp=Sys.time(),
                          smser=list(),sm=NULL,stats=list(),hist=list(),
                          evidence=character(),
                          app="default",seqno=-1L, prev_id=NA_character_) {
  new("StudentRecord",app=app,uid=uid,context=context,
      timestamp=timestamp,smser=smser,evidence=evidence,
      sm=sm,stats=stats,hist=hist,
      seqno=seqno,"_id"=NA_character_,
      prev_id=prev_id)
}

setGeneric("evidence",function(x) standardGeneric("evidence"))
setMethod("evidence","StudentRecord", function(x) x@evidence)
setGeneric("evidence<-",function(x,value) standardGeneric("evidence<-"))
setMethod("evidence<-","StudentRecord",
          function(x,value) {
            x@evidence <- value
            x})

setMethod("toString","StudentRecord", function(x, ...) {
  paste('StudentRecord:{uid:',x@uid,', context:',x@context,
        ', seqno:',x@seqno,'}')
})
setMethod("show","StudentRecord",function(object) {
  cat(toString(object),"\n")
})

setMethod("as.jlist",c("StudentRecord","list"), function(obj,ml,serialize=TRUE) {
  ml$"_id" <- NULL
  ml$class <- NULL
  ml$app <- unboxer(ml$app)
  ml$uid <- unboxer(ml$uid)
  if (!is.null(ml$context) && length(ml$context)==1L)
    ml$context <- unboxer(ml$context)
  ml$timestamp <- unboxer(ml$timestamp) # Auto_unbox bug.
  ## Serialize SM if necessary.
  if (!is.null(obj@sm)) {
    smo <- PnetSerialize(obj@sm)
    smo$data <- base64_enc(smo$data)
    ml$sm <- smo
  } else {
    ml$sm <- NULL
  }
  ml$smser <- NULL
  ## Normalize Evidence Sets
  ml$evidence <- NULL
  if (length(obj@evidence)>0L) {
    ml$evidence <- obj@evidence
  }
  ## Normalize Prev_id
  ml$"prev_id" <- NULL
  if (!is.na(obj@"prev_id")) {
    ml$prev <- unboxer(obj@"prev_id")
  }
  ml$seqno <- NULL
  if (!is.na(obj@seqno)) {
    ml$seqno <- unboxer(obj@seqno)
  }
  ml$stats <- unparseStats(ml$stats)
  ml$hist <- unparseData(ml$hist,TRUE)
  ml
})

parseStudentRecord <- function (rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  if (is.null(rec$prev_id)) rec$prev_id <- NA_character_
  if (is.null(rec$seqno)) rec$seqno <- NA_integer_
  else rec$seqno <- as.integer(rec$seqno)
  if (!is.null(rec$sm)) {
    smo <- list()
    smo$name <- rec$sm[["name"]]
    smo$data <- base64_dec(rec$sm[["data"]])
    smo$factory <- rec$sm[["factory"]]
  } else {
    smo <- NULL
  }

  slist <- parseStats(rec$stats)
  hist <- parseData(rec$hist)
  new("StudentRecord","_id"=ununboxer(rec$"_id"),
      app=as.character(rec$app),
      context=as.character(rec$context),
      uid=as.character(rec$uid),
      timestamp=as.POSIXlt(ununboxer(rec$timestamp)),
      evidence=as.character(rec$evidence),
      sm=NULL,smser=smo,stats=slist,hist=hist,
      seqno=rec$seqno,
      prev_id=as.vector(rec$prev_id))
}


unparseStats <- function (slist,flatten=FALSE) {
  res <- lapply(slist,function (s)
    if (length(s)==1L)
      unboxer(s)
    else
      lapply(as.list(s),unboxer)
    )
  if (flatten)
    res <- as.list(flatten(as.data.frame(res)))
  res
}

stats2json <- function (slist, flatten=FALSE) {
  toJSON(unparseStats(slist,flatten))
}

strsplit2 <- function (labels, splitchar=".", fixed=TRUE) {
  llist <- strsplit(labels,splitchar,fixed)
  tail <- rep("",length(labels))
  head <- rep("",length(labels))
  for (i in 1L:length(llist)) {
    parts <- llist[[i]]
    lp <- length(parts)
    if (lp > 1L) {
      head[i] <- paste(parts[1:(lp-1)],collapse=".")
      tail[i] <- parts[lp]
    } else
      head[i] <- parts
  }
  list(head=head,tail=tail)
}

unflattenNames <- function (slist) {
  nlist <- strsplit2(names(slist),".",fixed=TRUE)
  snames <- unique(nlist$head)
  if (length(snames)==length(slist)) return (slist) #Nothing to do
  res <- list()
  resnames <- character()
  for (sname in snames) {
    ii <- which(nlist$head==sname)
    if (length(ii)==1L) {
      if (length(slist[[ii]]) > 1L)
        res <- c(res,list(slist[[ii]]))
      else
        res <- c(res,slist[[ii]])
      resnames <- c(resnames,names(slist)[ii])
    } else {
      vals <- sapply(slist[ii],identity)
      names(vals) <- nlist$tail[ii]
      res <- c(res,list(vals))
      resnames <- c(resnames,sname)
    }
  }
  names(res) <- resnames
  res
}

parseStats <- function (slist) {
  res <- lapply(slist,function(s) {
    if (length(s) == 1L) {
      s[[1]]
    } else if (is.list(s)) {
      if (is.character(s[[1]]))
        s1 <- as.character(s)
      else if (as.numeric(s[[1]]))
        s1 <- as.numeric(s)
      else
        s1 <- s
      names(s1) <- names(s)            #Damn R anyway! it should keep names.
      s1
    } else {
      s
    }
  })
  unflattenNames(res)
}

###########
## Evidence Lists

setGeneric("evidence",function(x) standardGeneric("evidence"))
setMethod("evidence","StudentRecord", function(x) x@evidence)



markEvidence <- function (rec, evidMess) {
  rec@prev_id <- m_id(rec)
  rec@"_id" <- NA_character_
  rec@evidence <- c(m_id(evidMess),rec@evidence)
  rec@seqno <- rec@seqno+1
  rec
}





#############################################################
## SRSet mainenance

## Get SR
## Three cases:
## 1) SR for uid in warehouse -- return
## 2) SR for uid not in warehouse, but in DB -- fetch, cache and return.
## 3) No SR for uid -- clone Prof Model and save.

setClassUnion("SRorNull",c("StudentRecord","NULL"))

StudentRecordSet <-
  setRefClass("StudentRecordSet",
              fields=c(app="character",
                       dbname="character",
                       dburi="character",
                       db="MongoDB",
                       warehouse="PnetWarehouse",
                       defaultSR="SRorNull"),
              methods = list(
                  initialize =
                    function(app="default",dbname="EARecords",
                             dburi="mongodb://localhost:271017",
                             db = NULL,warehouse=NULL,
                             ...)
                      callSuper(app=app,db=db,dbname=dbname,dburi=dburi,
                                warehouse=warehouse,defaultSR=NULL,
                                ...),
                  recorddb = function () {
                    if (is.null(db) && length(dburi)>0L && nchar(dburi) >0L) {
                      db <<- mongo("StudentRecords",dbname,dburi)
                    }
                    db
                  },
                  clearAll = function(clearDefault=FALSE) {
                    flog.info("Clearing Student Records for %s",app)
                    ## Save reference to proficiency model
                    profModel <- NULL
                    if (!is.null(defaultSR))
                      profModel <- PnetName(sm(defaultSR))

                    ClearWarehouse(warehouse)

                    if (clearDefault) {
                      defaultSR <<- NULL
                    } else if (!is.null(profModel)) {
                      sm(defaultSR) <<- WarehouseFetch(warehouse,profModel)
                      PnetCompile(sm(defaultSR))
                    }

                    if (!is.null(recorddb())) {
                      if (clearDefault)
                        recorddb()$remove(buildJQuery(app=app))
                      else
                        recorddb()$remove(buildJQuery(app=app,
                                                      uid=c("ne"="*DEFAULT*")))
                    }
                  }
              ))


StudentRecordSet <- function(app="default",warehouse=NULL,
                             dburi="mongodb://localhost:271017",
                             dbname="EIRecords",
                             ...)
  new("StudentRecordSet",app=app, dbname=dbname, dburi=dburi,
      db=NULL, warehouse=warehouse, ...)



setMethod("app","StudentRecordSet", function(x) x$app)
defaultSR <- function(x) x$defaultSR

setGeneric("getSR", function (srs,uid,ser="") standardGeneric("getSR"))
setGeneric("saveSR", function (srs,rec) standardGeneric("saveSR"))
setGeneric("newSR", function (srs,uid) standardGeneric("newSR"))
setGeneric("clearSRs", function(srs) standardGeneric("clearSRs"))


## Student Record Methods
setMethod("getSR", c("StudentRecordSet","ANY"),
function (srs,uid,ser=NULL) {
  if (length(ser) > 0L) {
    rec <- parseStudentRecord(ser)
  } else if (!is.null(srs$recorddb())) {
    rec <- getOneRec(buildJQuery(app=app(srs),uid=uid),srs$recorddb(),
                     parseStudentRecord)
  } else {
    rec <- NULL
  }
  if (is.null(rec)) {
    rec <- newSR(srs,uid)
  } else {
    rec <- fetchSM(rec,srs$warehouse)
  }
  rec
})

setMethod("saveSR", c("StudentRecordSet","ANY"), function (srs,rec) {
  if (!is.null(srs$recorddb()))
    saveRec(rec,srs$recorddb())
})

setMethod("newSR", c("StudentRecordSet","ANY"), function (srs,uid) {
  flog.debug("Making new student record for  %s", uid)
  dsr <- srs$defaultSR
  oldnet <- WarehouseFetch(srs$warehouse,as.legal.name(srs$warehouse,uid))
  if (isTRUE(is.valid(srs$warehouse,oldnet)))
    WarehouseFree(srs$warehouse,PnetName(oldnet))
  rec <- StudentRecord(uid=uid,context(dsr),timestamp=Sys.time(),
                       sm=WarehouseCopy(srs$warehouse,sm(dsr),
                                        as.legal.name(srs$warehouse,uid)),
                       stats=stats(dsr),hist=dsr@hist,app=app(srs),
                       seqno=0L)
  saveRec(rec,srs$recorddb())
  rec
})

setMethod("clearSRs", c("StudentRecordSet"), function (srs) {
  if (!is.null(srs$recorddb()))
    if (srs$recorddb()$count(buildJQuery(app=app(srs))) > 0L)
      srs$recorddb()$remove(buildJQuery(app=app(srs)))
  invisible(srs)
})


