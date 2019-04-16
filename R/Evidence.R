###  Evedence -- A set of evidence coming from the EI process.
###  This roughy follow a combination of the xAPI format.

setClass("EvidenceSet",
         slots=c(seqno="integer"),
         contains="P4Message")

EvidenceSet <- function(uid,context,timestamp=Sys.time(),
                        obs=list(),app="default",mess="Accumulate") {
  new("EvidenceSet",app=app,uid=uid,context=context,mess=mess,
      timestamp=timestamp,obs=obs,"_id"=NA_character_,
      seqno=NA_integer_ )
}

setGeneric("seqno",function(x) standardGeneric("seqno"))
setMethod("seqno","Event", function(x) x@seqno)



setMethod("toString","EvidenceSet", function(x, ...) {
  paste('EvidenceSet:{ uid:',x@uid,', context:',x@context,
        ', seqno:',x@seqno,'}')
})
setMethod("show","EvidenceSet",function(object) {
  cat(toString(object),"\n")
})

setMethod("as.jlist",c("EvidenceSet","list"), function(obj,ml,serialize=TRUE) {
  ## Call Next Method
  as.p4jlist <- getMethod("as.jlist",c("P4Message","list"))
  ml <- do.call(as.p4jlist,list(obj,ml,serialize))
  ## Additional work
  if (is.na(ml$seqno)) {
    ml$seqno <- NULL
  } else {
    ml$seqno <- unbox(ml$seqno)
  ml
  })

parseEvidence<- function (rec) {
  if (is.null(rec$"_id")) rec$"_id" <- NA_character_
  names(rec$"_id") <- "oid"
  if (is.null(rec$app)) rec$app <- "default"
  if (is.null(rec$seqno)) rec$seqno <- NA_integer_
  if (is.null(rec$timestamp)) rec$timestamp <- Sys.time()
  new("EvidenceSet","_id"=rec$"_id", app=as.vector(rec$app),
      uid=as.vector(rec$uid),
      context=as.vector(rec$context),mess=as.vector(rec$mess),
      timestamp=as.POSIXlt(ununboxer(rec$timestamp)),
      data=parseData(ununboxer(rec$data)),
      seqno=as.vector(rec$seqno))
}


