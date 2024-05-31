###  Evidence -- A set of evidence coming from the EI process.
###  This roughly follow a combination of the xAPI format.

setClass("EvidenceSet",
         slots=c(seqno="integer"),
         contains="P4Message")

EvidenceSet <- function(uid,context,timestamp=Sys.time(),
                        obs=list(),app="default",mess="Accumulate",
                        sender="EI", processed=FALSE) {
  new("EvidenceSet",app=app,uid=uid,context=context,mess=mess,
      timestamp=as.POSIXct(timestamp),
      data=obs,sender=sender,"_id"=c(oid=NA_character_),
      seqno=NA_integer_ , processed=processed,
      pError="")
}

setGeneric("seqno",function(x) standardGeneric("seqno"))
setMethod("seqno","EvidenceSet", function(x) x@seqno)
setGeneric("seqno<-",function(x,value) standardGeneric("seqno<-"))
setMethod("seqno<-","EvidenceSet", function(x,value) {
  x@seqno <- as.integer(value)
  x
})


setMethod("toString","EvidenceSet", function(x, ...) {
  paste('EvidenceSet:{ uid:',x@uid,', context:',x@context,
        ', seqno:',x@seqno,'}')
})
setMethod("show","EvidenceSet",function(object) {
  cat(toString(object),"\n")
})

setGeneric("observables",function(x) standardGeneric("observables"))
setMethod("observables","EvidenceSet", function(x) x@data)

setMethod("as.jlist",c("EvidenceSet","list"), function(obj,ml,serialize=TRUE) {
  ## Call Next Method
  as.p4jlist <- getMethod("as.jlist",c("P4Message","list"))
  ml <- do.call(as.p4jlist,list(obj,ml,serialize))
  ## Additional work
  if (is.na(ml$seqno)) {
    ml$seqno <- NULL
  } else {
    ml$seqno <- unboxer(ml$seqno)
  }
  ml
})

setMethod("parse.jlist", c("EvidenceSet","list"),
 function (class,rec) {
  rec <- cleanMessageJlist(rec)
  if (is.null(rec$seqno)) rec$seqno <- NA_integer_
  rec$"_id" <- ununboxer(rec$"_id")
  if (is.null(rec$"_id")) rec$"_id" <- c(oid=NA_character_)
  rec$data <- parseData(ununboxer(rec$data))
  rec
})

### Evidence Logs

## Need a way of actually logging which observables are and are not used to
## facilitate debugging.

setClass("EvidenceLog",
         slots=c(eid="character",
                 context="character",
                 used="list",
                 ignored="list"))

EvidenceLog <- function (eid,context,used=list(),ignored=list())
  new("EvidenceLog",eid=eid,context=context,
      used=used,ignored=ignored)


eid <- function(el) {el@eid}

setMethod("context","EvidenceLog",function(x) {x@context})

setMethod("observables","EvidenceLog", function (x) {
  list(used=x@used,ignored=x@ignored)
  })

setGeneric("useObs", function (x,name,value) standardGeneric("useObs"))
setGeneric("ignoreObs", function (x,name,value) standardGeneric("ignoreObs"))

setMethod("useObs",c("EvidenceLog"), function (x,name,value) {
  obs <- list(value)
  names(obs) <- name
  x@used <- c(x@used,obs)
  x
})

setMethod("ignoreObs",c("EvidenceLog"), function (x,name,value) {
  obs <- list(value)
  names(obs) <- name
  x@ignored <- c(x@ignored,obs)
  x
})


setMethod("as.jlist",c("EvidenceLog","list"),function(obj,ml,serialize=TRUE) {
  ml$eid <- unboxer(ml$eid)
  ml$context <- unboxer(ml$context)
  ml$used <- unboxer(ml$used)
  ml$ignored <- unboxer(ml$ignored)
  callNextMethod(obj,ml,serialize)
})


parseEvidenceLog <- function (rec) {
  rec$eid <- ununboxer(rec$eid)
  rec$context <- ununboxer(rec$context)
  rec$used <- as.list(ununboxer(rec$used))
  rec$ignored <- as.list(ununboxer(rec$ignored))
  rec
}

setMethod("parse.jlist",c("EvidenceLog","list"),function(class,rec) {
  parseEvidenceLog(rec)
})
