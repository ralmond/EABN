### Student Record ---  This is a wrapper for the Bayes net.
### For now, assume 1 SM per record and 2 Hard code dependency on
### RNetica.

setClassUnion("PnetOrNull",c("Pnet","NULL"))

setClass("StudentRecord",
         slots=c("_id"="string",       #Mongo ID
                 app="character",       #Application ID
                 uid="character",       #User (student) ID
                 context="string",      #If context is calculated by PP
                 timestamp="POSIXt",      #When action took place.
                 evidence_id="character",
                 evidence="list",
                 sm="PnetOrNull",
                 stats="numeric",
                 seqno="integer",
                 prev="ANY",prev_id="character"))        #More details.
setMethod("app","StudentRecord", function(x) x@app)
setMethod("uid","StudentRecord", function(x) x@uid)
setMethod("context","StudentRecord", function(x) x@context)
setMethod("timestamp","StudentRecord", function(x) x@timestamp)
setGeneric("sm",function(x) standardGeneric("sm"))
setMethod("sm","StudentRecord", function(x) x@sm)
setGeneric("stats",function(x) standardGeneric("stats"))
setMethod("stats","StudentRecord", function(x) x@stats)

StudentRecord <- function(uid,context,timestamp=Sys.time(),
                          evidence=list(),evidence_id=character(),
                          sm=NULL,stats=numeric(),
                          app="default",context="") {
  new("StudentRecord",app=app,uid=uid,context=context
      timestamp=timetimestamp,evidence=evidence,sm=sm,stats=stats,
      seqno=NA_integer_,prev=NULL,_id=NA_character_,
      prev_id=NA_character_)
}

setMethod("toString","StudentRecord", function(x, ...) {
  paste('StudentRecord:{uid:',x@uid,', context:',x@context,
        ', seqno:',x@seqno,'}')
})
setMethod("show","StudentRecord",function(object) {
  cat(toString(object),"\n")
})

setMethod("as.json","EvidenceSet", function(x) {
  srl <- attributes(x)
  srl$"_id" <- NULL
  if (is.na(srl$seqno)) srl$seqno <- NULL
  srl$class <-NULL
  ## Serialize SM if necessary.
  if (!is.null(x@sm)) {
    smo <- PnetSerialize(x@sm)
    smo$data <- base64_enc(smo$data)
    srl$sm <- smo
  }
  ## Normalize Evidence Sets
  srl$evidence <- NULL
  srl$"evidence_id" <- NULL
  if (!is.null(x@"evidence_id")) {
    srl$evidence <- x@"evidence_id"
  } else if (length(x@evidence) > 0L) {
    srl$evidence <- sapply(x@evidence, function (e) e@"_id")
  }

  ## Normalize Previod
  srl$prev <- NULL
  srl$"prev_id" <- NULL
  if (!is.na(x@"prev_id")) {
    srl$prev <- x@"prev_id"
  } else if (!is.null(x@prev)) {
    srl$prev <- x@prev@"_id"
  }
  ## Now serialize
  raw <- toJSON(esl,auto_unbox=TRUE,POSIXt="mongo")
  ## Timestamp is not unboxed.  Need to do that manually.
  sub('"timestamp":\\[(.*)\\]','"timestamp":\\1',raw)
  })


setGeneric("evidence",function(x) standardGeneric("evidence"))
setMethod("evidence","StudentRecord", function(x) x@evidence)
