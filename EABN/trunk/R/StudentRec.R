### Student Record ---  This is a wrapper for the Bayes net.
### For now, assume 1 SM per record and 2 Hard code dependency on
### RNetica.

## This doesn't work because Pnet is an abstract class.
## setClassUnion("PnetOrNull",c("Pnet","NULL"))
setClassUnion("BayesianNetwork",c("NeticaBN","NULL"))
setClassUnion("BNSession",c("NeticaSession","NULL"))
setClassUnion("EmptyWarehouse",c("BNWarehouse","NULL"))

setClass("StudentRecord",
         slots=c("_id"="character",
                 app="character",
                 uid="character",
                 context="character",
                 evidence="character",
                 timestamp="POSIXt",      #Date of last update.
                 sm="BayesianNetwork",
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
fetchSM <- function (sr, warehouse) {
  sr@sm <- WarehouseFetch(warehouse,as.IDname(uid(sr),"S"))
  if (is.null(sm(sr)) || !is.active(sm(sr))) {
    sr@sm <- unpackSM(sr,warehouse)
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
                          app="default",seqno=-1L) {
  new("StudentRecord",app=app,uid=uid,context=context,
      timestamp=timestamp,smser=smser,evidence=evidence,
      sm=sm,stats=stats,hist=hist,
      seqno=seqno,"_id"=NA_character_,
      prev_id=NA_character_)
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
    ml$sqno <- unboxer(obj@seqno)
  }
  ml$stats <- unparseStats(ml$stats)
  ml$hist <- lapply(ml$hist,unparseHist)
  ml
})

parseStudentRecord <- function (rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  if (is.null(rec$prev_id)) rec$prev_id <- NA_character_
  if (is.null(rec$seqno)) rec$seqno <- NA_integer_
  else rec$seqno <- as.integer(ununboxer(rec$seqno))
  if (!is.null(rec$sm)) {
    smo <- rec$sm
    smo$name <- smo$name[[1]]
    smo$data <- base64_dec(smo$data[[1]])
    smo$factory <- smo$factory[[1]]
  } else {
    smo <- NULL
  }

  slist <- parseStats(rec$stats)
  hist <- lapply(rec$hist,parseHist)
  new("StudentRecord","_id"=ununboxer(rec$"_id"),
      app=ununboxer(rec$app), context=ununboxer(rec$context),
      uid=ununboxer(rec$uid),
      timestamp=as.POSIXlt(ununboxer(rec$timestamp)),
      evidence=as.character(rec$evidence),
      sm=NULL,smser=smo,stats=slist,hist=hist,
      seqno=rec$seqno,
      prev_id=ununboxer(rec$prev_id))
}


unparseStats <- function (slist) {
  lapply(slist,function (s)
    if (length(s)==1L)
      unboxer(s)
    else
      lapply(as.list(s),unboxer)
    )
}
stats2json <- function (slist) {
  toJSON(unparseStats(slist))
}
parseStats <- function (slist) {
  lapply(slist,function(s) do.call(c,as.list(s)))
}

unparseHist <- function(histo) {
  list(rownames=rownames(histo),colnames=colnames(histo),
       data=as.vector(histo))
}

parseHist <- function(hlist) {
  rnames <- hlist$rownames
  cnames <- hlist$colnames
  matrix(hlist$data,length(rnames),length(cnames),
         dimnames=list(Events=rnames,States=cnames))
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
                       warehouse="EmptyWarehouse",
                       defaultSR="SRorNull"),
              methods = list(
                  initialize =
                    function(app="default",dbname="EIRecords",
                             dburi="mongodb://localhost:271017",
                             db = NULL,warehouse=NULL,
                             ...)
                      callSuper(app=app,db=db,dbname=dbname,dburi=dburi,
                                warehouse=warehouse,defaultSR=NULL,...),
                  recorddb = function () {
                    if (is.null(db)) {
                      db <<- mongo("StudentRecords",dbname,dburi)
                    }
                    db
                  },
                  clearAll = function(clearDefault=FALSE) {
                    flog.info("Clearing Student Records for %s",app)
                    if (clearDefault)
                      recorddb()$remove(buildJQuery(app=app))
                    else
                      recorddb()$remove(buildJQuery(app=app,
                                                    uid=c("ne"="*DEFAULT*")))
                  }
                  ))

setMethod("app","StudentRecordSet", function(x) x$app)
defaultSR <- function(x) x$defaultSR



## Student Record Methods
getSR <- function (srs,uid) {
  rec <- getOneRec(buildJQuery(app=app(srs),uid=uid),srs$recorddb(),
                      parseStudentRecord)
  if (is.null(rec)) {
    rec <- newSR(srs,uid)
  } else {
    rec <- fetchSM(rec,srs$warehouse)
  }
  rec
}

saveSR <- function (srs,rec) {
  saveRec(rec,srs$recorddb())
}

newSR <- function (srs,uid) {
  flog.debug("Making new student record for  %s", uid)
  dsr <- srs$defaultSR
  rec <- StudentRecord(uid=uid,context(dsr),timestamp=Sys.time(),
                      sm=CopyNetworks(sm(dsr),as.IDname(uid,"S")),
                      stats=stats(dsr),hist=dsr@hist,app=app(srs),
                      seqno=0L)
  saveRec(rec,srs$recorddb())
  rec
}



