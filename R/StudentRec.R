### Student Record ---  This is a wrapper for the Bayes net.
### For now, assume 1 SM per record and 2 Hard code dependency on
### RNetica.

## This doesn't work because Pnet is an abstract class.
## setClassUnion("PnetOrNull",c("Pnet","NULL"))
setClassUnion("BayesianNetwork",c("NeticaBN","NULL"))
setClassUnion("BNSession",c("NeticaSession","NULL"))
setClassUnion("EmptyWarehouse",c("BNWarehouse","NULL"))

setClass("StudentRecord",
         slots=c(evidence="list",
                 sm="BayesianNetwork",
                 seqno="integer",
                 hist="list",
                 prev_id="character"),
         contains="P4Message")

setMethod("app","StudentRecord", function(x) x@app)
setMethod("uid","StudentRecord", function(x) x@uid)
setMethod("context","StudentRecord", function(x) x@context)
setMethod("timestamp","StudentRecord", function(x) x@timestamp)


setGeneric("sm",function(x) standardGeneric("sm"))
setGeneric("stats",function(x) standardGeneric("stats"))
setGeneric("statNames",function (sr) standardGeneric("statNames"))
setGeneric("histNames",function (sr) standardGeneric("histNames"))
setGeneric("stat",function (sr,name) standardGeneric("stat"))
setGeneric("history",function (sr,name) standardGeneric("history"))

setMethod("sm","StudentRecord", function(x) x@sm)
setMethod("stats","StudentRecord", function(x) x@data)
setMethod("statNames","StudentRecord", function (sr) names(stats(sr)))
setMethod("stat",c("StudentRecord","character"),
          function (sr, name) stats(sr)[[name]])
setMethod("histNames","StudentRecord", function (sr) names(sr@hist))
setMethod("history",c("StudentRecord","character"),
          function (sr, name) sr@hist[[name]])


StudentRecord <- function(uid,context="",timestamp=Sys.time(),
                          evidence=list(),evidence_id=character(),
                          sm=NULL,stats=list(),hist=list(),
                          app="default") {
  new("StudentRecord",app=app,uid=uid,context=context,
      timestamp=timestamp,evidence=evidence,sm=sm,data=stats,
      hist=hist,
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
    ml$sm <- NULL
  }
  ## Normalize Evidence Sets
  ml$evidence <- NULL
  if (length(x@evidence)>0L) {
    ml$evidence <- unboxer(x@evidence)
  }
  ## Normalize Prev_id
  ml$prev <- NULL
  ml$"prev_id" <- NULL
  if (!is.na(x@"prev_id")) {
    ml$prev <- unboxer(x@"prev_id")
  }
  ml$hist <- serializeJSON(ml$hist)
  ml
})

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
  hist <- unserializeJSON(rec$hist)
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
                       defaultSR="SRorNull"),
              methods = list(
                  initialize =
                    function(app="default",dbname="EIRecords",
                             dburi="mongodb://localhost:271017",
                             db = NULL,
                             ...)
                      callSuper(app=app,db=db,dbname=dbname,dburi=dburi,...)
              ))


## Student Record Methods
StudentRecordSet$methods(
             recorddb = function () {
               if (is.null(db)) {
                 db <<- mongo("States",dbname,dburi)
               }
               db
             },
             getSR = function (uid) {
               result <- getOneRec(buildJQuery(app=app,uid=uid),recorddb(),
                                   parseStudentRecord)
               if (is.null(result)) {
                 result <- getOneRec(buildJQuery(app=app,uid="*DEFAULT*"),
                                                 recorddb(),parseStudentRecord)
                 result@uid <- uid
                 result@timestamp <- Sys.time()
                 result@"_id" <- NA_character_

               }
               result
             },
             saveStatus = function (state) {
               saveRec(state,recorddb())
             },
             newStudent = function (uid) {
               rec <- getStatus(uid)
               if (!is.null(rec)) {
                 flog.debug("Found existing student record for  %s", uid)
                 return (rec)
               }
               rec <- getStatus("*DEFAULT*")
               if(!is.null(rec)) {
                 flog.debug("Found default student record for  %s", uid)
                 rec@uid <- uid
                 rec@timestamp <- Sys.time()
                 saveRec(rec,recorddb())
                 return(rec)
               }
               flog.debug("Making blank student record for  %s", uid)
               rec <- Status(uid=uid,context="*INITIAL*",timestamp=Sys.time(),
                             app=app)
               rec <- saveRec(rec,recorddb())
               rec
             },
             clearAll = function(clearDefault=FALSE) {
               flog.info("Clearing Student Records for %s",app)
               if (clearDefault)
                 recorddb()$remove(buildJQuery(app=app))
               else
                 recorddb()$remove(buildJQuery(app=app,uid=c("ne"="*DEFAULT*")))
             }
             )

