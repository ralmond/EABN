###  Evedence -- A set of evidence coming from the EI process.
###  This roughy follow a combination of the xAPI format.

setClass("EvidenceSet",
         slots=c("_id"="character",    #Mongo ID
                 app="character",       #Application ID
                 uid="character",       #User (student) ID
                 context="character",      #If context is calculated by PP
                 mess="character",      #Action Identifier
                 timestamp="POSIXt",      #When action took place.
                 obs="list",              #More details.
                 seqno="integer"))
setGeneric("app",function(x) standardGeneric("app"))
setMethod("app","EvidenceSet", function(x) x@app)
setGeneric("uid",function(x) standardGeneric("uid"))
setMethod("uid","EvidenceSet", function(x) x@uid)
setGeneric("mess",function(x) standardGeneric("mess"))
setMethod("mess","EvidenceSet", function(x) x@mess)
setGeneric("context",function(x) standardGeneric("context"))
setMethod("context","EvidenceSet", function(x) x@context)
setGeneric("timestamp",function(x) standardGeneric("timestamp"))
setMethod("timestamp","EvidenceSet", function(x) x@timestamp)
setGeneric("details",function(x) standardGeneric("details"))
setMethod("details","EvidenceSet", function(x) x@obs)

EvidenceSet <- function(uid,context,timestamp=Sys.time(),
                        obs=list(),app="default",mess="Accumulate") {
  new("EvidenceSet",app=app,uid=uid,context=context,mess=mess,
      timestamp=timestamp,obs=obs,"_id"=NA_character_,
      seqno=NA_integer_ )
}

setMethod("toString","EvidenceSet", function(x, ...) {
  paste('EvidenceSet:{ uid:',x@uid,', context:',x@context,
        ', seqno:',x@seqno,'}')
})
setMethod("show","EvidenceSet",function(object) {
  cat(toString(object),"\n")
})




setGeneric("as.json",function(x) standardGeneric("as.json"))
setMethod("as.json","EvidenceSet", function(x) {
  esl <- attributes(x)
  esl$"_id" <- NULL
  if (is.na(esl$seqno)) esl$seqno <- NULL
  esl$class <-NULL
  esl$timestamp <- unbox(esl$timestamp) # Auto_unbox bug.
  raw <- toJSON(esl,auto_unbox=TRUE,POSIXt="mongo")
  ## Timestamp is not unboxed.  Need to do that manually.
  ## sub('"timestamp":\\[(.*)\\]','"timestamp":\\1',raw)
  raw
  })

dquote <- function (str) {paste('"',str,'"',sep="")}

saveES <- function (es, col) {
  if (is.na(es@"_id")) {
    ## Insert
    jso <- as.json(es)
    col$insert(jso)
    it <- col$iterate(jso,'{"_id":true}',limit=1)
    es@"_id" <- it$one()$"_id"
  } else {
    ## Replace
    col$update(paste('{"_id":{"$oid":"',es@"_id",'"}}',sep=""),
               paste('{"$set":',as.json(es),'}',sep=""))
  }
  es
}


getESbyID <- function(id,col) {
  it <- col$iterate(paste('{"_id":{"$oid":"',id,'"}}',sep=""),
                    '{}',limit=1)
  rec <- it$one()
  if (is.null(rec)) return(rec)
  parseEvidence(rec)
}



parseEvidence<- function (rec) {
  new("EvidenceSet","_id"=rec$"_id", app=rec$app, uid=rec$uid,
      context=rec$context,mess=rec$mess,
      timestamp=rec$timestamp,
      obs=parseObs(rec$obs),
      seqno=rec$seqno)
}

parseObs <- function (obsObj) {
  ##Not sure if we need further processing.
  obsObj
}

buildESquery <- function (uid,context,mess,before,after,
                          timestamp=NULL,seqno=NA_integer_,
                          app="default") {
  query <- '{'
  ## app
  if (length(app)> 1L) {
    app <- paste('{"$in":',toJSON(app),'}',sep="")
  }
  query <- paste(query,'"app":"',app,'"',sep="")
  ## uid
  if (missing(uid)) {
    stop("Must specify user/student id.")
  }
  if (length(uid) > 1L) {
    uid <- paste('{"$in":',toJSON(uid),'}',sep="")
  } else {
    uid <- toJSON(uid,auto_unbox=TRUE)
  }
  query <- paste(query,', "uid":',uid,sep="")
  ## Context
  if (!missing(context)) {
    if (length(context) > 1L) {
      context <- paste('{"$in":',toJSON(context),'}',sep="")
    } else {
      context <- toJSON(context,auto_unbox=TRUE)
    }
    query <- paste(query,', "context":',context,sep="")
  }
  ## Mess(age)
  if (!missing(mess)) {
    if (length(mess) > 1L) {
      mess <- paste('{"$in":',toJSON(mess),'}',sep="")
    } else {
      mess <- toJSON(mess,auto_unbox=TRUE)
    }
    query <- paste(query,', "mess":',mess,sep="")
  }
  ## Timestamp -- can be either single time or before or after range.
  if (!missing(timestamp)) {
    timestamp <- paste('"$in:"',toJSON(timestamp,POSIXt="mongo"))
  } else {
    lt <- NULL
    if (!missing(before) && is(before,"POSIXt")) {
      if (length(before) > 1L) {
        stop("Before must be a single POSIXt or integer.")
      }
      ## Need to strip array marks off
      lt <- paste('"$lt":',toJSON(unbox(before),POSIXt="mongo"))
    }
    gt <- NULL
    if (!missing(after) && is(after,"POSIXt")) {
      if (length(after) > 1L) {
        stop("After must be a single POSIXt or integer.")
      }
      ## Need to strip array marks off
      gt <- paste('"$gt":',toJSON(unbox(after),POSIXt="mongo"))
    }
    timestamp <- paste(c(lt,gt),collapse=",")
  }
  if (nchar(timestamp) > 0L) {
    query <- paste(query,', "timestamp":{',timestamp,'}',sep="")
  }

  ## seqno -- can be either single number or before and after range.
  if (!missing(seqno)) {
    ## This produces sloppy code when seqno is a single number, but
    ## requires fewer branches.
    seqno <- paste('"$in:"',toJSON(seqno))
  } else {
    lt <- NULL
    if (!missing(before) && is.numeric(before)) {
      if (length(before) > 1L) {
        stop("Before must be a single POSIXt or integer.")
      }
      ## Need to strip array marks off
      lt <- paste('"$lt":',toJSON(unbox(before)))
    }
    gt <- NULL
    if (!missing(after) && is.numeric(after)) {
      if (length(after) > 1L) {
        stop("After must be a single POSIXt or integer.")
      }
      ## Need to strip array marks off
      gt <- paste('"$gt":',toJSON(unbox(after)))
    }
    seqno <- paste(c(lt,gt),collapse=",")
  }
  if (nchar(seqno) > 0L) {
    query <- paste(query,', "seqno":{',seqno,'}',sep="")
  }

  #### Return as string.
  paste(query,'}',sep="")
}


getESone <- function(query,col) {
  it <- col$iterate(query,'{}',sort='{"timestamp":-1}',limit=1)
  rec <- it$one()
  if (is.null(rec)) return(rec)
  parseEvidence(rec)
}

getESmany <- function(query,sort=-1,col) {
  n <- col$count(query)
  result <- vector("list",n)
  it <- col$iterate(query,'{}',
                    sort=paste('{"timestamp":',sort,'}',sep=""))
  nn <- 1
  while (!is.null(rec <- it$one())) {
    result[[nn]] <- parseEvidence(rec)
    nn <- nn +1
  }
  result
}

