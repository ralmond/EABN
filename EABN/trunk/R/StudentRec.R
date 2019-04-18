### Student Record ---  This is a wrapper for the Bayes net.
### For now, assume 1 SM per record and 2 Hard code dependency on
### RNetica.

## This doesn't work because Pnet is an abstract class.
## setClassUnion("PnetOrNull",c("Pnet","NULL"))

setClass("StudentRecord",
         slots=c(evidence_id="character",
                 evidence="list",
                 sm="ANY",
                 seqno="integer",
                 prev="ANY",prev_id="character"),
         contains="P4Message")

setMethod("app","StudentRecord", function(x) x@app)
setMethod("uid","StudentRecord", function(x) x@uid)
setMethod("context","StudentRecord", function(x) x@context)
setMethod("timestamp","StudentRecord", function(x) x@timestamp)


setGeneric("sm",function(x) standardGeneric("sm"))

setGeneric("stats",function(x) standardGeneric("stats"))

setMethod("sm","StudentRecord", function(x) x@sm)
setMethod("stats","StudentRecord", function(x) x@data)

StudentRecord <- function(uid,context="",timestamp=Sys.time(),
                          evidence=list(),evidence_id=character(),
                          sm=NULL,stats=list(),
                          app="default") {
  new("StudentRecord",app=app,uid=uid,context=context,
      timestamp=timestamp,evidence=evidence,sm=sm,stats=stats,
      seqno=NA_integer_,prev=NULL,"_id"=NA_character_,
      prev_id=NA_character_)
}

setGeneric("evidence",function(x) standardGeneric("evidence"))
setMethod("evidence","StudentRecord", function(x) x@evidence)

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
  if (!is.null(x@sm)) {
    smo <- PnetSerialize(x@sm)
    smo$data <- base64_enc(smo$data)
    ml$sm <- smo
  } else {
    srl$sm <- NULL
  }
  ## Normalize Evidence Sets
  ml$evidence <- NULL
  ml$"evidence_id" <- NULL
  if (length(x@"evidence_id")>0L) {
    ml$evidence <- unboxer(x@"evidence_id")
  } else if (length(x@evidence) > 0L) {
    ml$evidence <- sapply(x@evidence, function (e) e@"_id")
  }
  ## Normalize Prev_id
  ml$prev <- NULL
  ml$"prev_id" <- NULL
  if (!is.na(x@"prev_id")) {
    ml$prev <- unboxer(x@"prev_id")
  } else if (!is.null(x@prev)) {
    ml$prev <- unboxer(x@prev@"_id")
  }
  ml

parseStudentRecord <- function (rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  if (is.null(rec$evidence_id)) rec$evidence_id <- NA_character_
  if (is.null(rec$prev_id)) rec$prev_id <- NA_character_
  if (is.null(rec$seqno)) rec$seqno <- NA_integer_
  elist <- list()
  smo <- NULL  ## HERE
  slist <- list()
  prev <- NULL
  new("StudentRecord","_id"=ununboxer(rec$"_id"),
      app=ununboxer(rec$app), context=ununboxer(rec$context),
      uid=ununboxer(rec$uid),
      timestamp=as.POSIXlt(ununboxer(rec$timestamp)),
      evidence_id=ununboxer(rec$evidence_id),evidence=elist,
      sm=smo,stats=slist,prev=prev,prev_id=ununboxer(rec$prev_id))
  }

###########
## Evidence Lists

setGeneric("evidence",function(x) standardGeneric("evidence"))
setMethod("evidence","StudentRecord", function(x) x@evidence)

