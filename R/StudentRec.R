### Student Record ---  This is a wrapper for the Bayes net.
### For now, assume 1 SM per record and 2 Hard code dependency on
### RNetica.

## This doesn't work because Pnet is an abstract class.
## setClassUnion("PnetOrNull",c("Pnet","NULL"))

setClass("StudentRecord",
         slots=c("_id"="character",       #Mongo ID
                 app="character",       #Application ID
                 uid="character",       #User (student) ID
                 context="character",      #If context is calculated by PP
                 timestamp="POSIXt",      #When action took place.
                 evidence_id="character",
                 evidence="list",
                 sm="ANY",
                 stats="list",
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

StudentRecord <- function(uid,context="",timestamp=Sys.time(),
                          evidence=list(),evidence_id=character(),
                          sm=NULL,stats=list(),
                          app="default") {
  new("StudentRecord",app=app,uid=uid,context=context,
      timestamp=timestamp,evidence=evidence,sm=sm,stats=stats,
      seqno=NA_integer_,prev=NULL,"_id"=NA_character_,
      prev_id=NA_character_)
}

setMethod("toString","StudentRecord", function(x, ...) {
  paste('StudentRecord:{uid:',x@uid,', context:',x@context,
        ', seqno:',x@seqno,'}')
})
setMethod("show","StudentRecord",function(object) {
  cat(toString(object),"\n")
})

setMethod("as.json","StudentRecord", function(x) {
  srl <- attributes(x)
  srl$"_id" <- NULL
  if (is.na(srl$seqno)) srl$seqno <- NULL
  srl$class <-NULL
  ## Serialize SM if necessary.
  if (!is.null(x@sm)) {
    smo <- PnetSerialize(x@sm)
    smo$data <- base64_enc(smo$data)
    srl$sm <- smo
  } else {
    srl$sm <- NULL
  }
  ## Normalize Evidence Sets
  srl$evidence <- NULL
  srl$"evidence_id" <- NULL
  if (length(x@"evidence_id")>0L) {
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
  ## Fix for auto_unbox bug
  ## Turns out I want finer control on the auto-unbox, so will use
  ## manual unboxes.
  srl$app <- unbox(srl$app)
  srl$uid <- unbox(srl$uid)
  srl$context <- unbox(srl$context)
  srl$timestamp <- unbox(srl$timestamp)
  srl$seqno <- unbox(srl$seqno)
  srl$prev <- unbox(srl$prev)
  srl$sm <- lapply(srl$sm,unbox)
  ## Saves name data
  srl$stats <- lapply(srl$stats,
                      function (s) lapply(s,unbox)) #Saves name data.
  ## Now serialize
  toJSON(srl,POSIXt="mongo")
  })


parseSR<- function (rec) {
  smo <- rec$sm
  if (!is.null(smo)) {
    smo$data <- base64_dec(smo$data)
  }
  if (is.null(rec$seqno)) rec$seqno <- NA_integer_
  if (is.null(rec$prev_id)) rec$prev_id <- NA_character_
  if (is.null(rec$"evidence") || length(rec$evidence)==0L)
    rec$"evidence" <- character()
  new("StudentRecord","_id"=rec$"_id", app=rec$app, uid=rec$uid,
      context=rec$context,timestamp=rec$timestamp,
      "evidence_id"=as.character(rec$evidence), sm=PnetUnserialize(smo),
      stats=parseStats(rec$stats),seqno=rec$seqno,prev_id=rec$prev)
}

parseStats <- function (statsList) {
  ##Need to convert back from list to numeric/character
  for (i in 1:length(statsList)) {
    stat <- statsList[[i]]
    if (all(sapply(stat,is.character))) {
      stat <- as.character(stat)
      names(stat) <- names(statsList[[i]])
    }
    if (all(sapply(stat,is.numeric))) {
      if (all(sapply(stat,is.integer))) {
        stat <- as.integer(stat)
      } else {
        stat <- as.numeric(stat)
      }
      names(stat) <- names(statsList[[i]])
    }
    ## May need an extra step here to decode stats which
    ## are not one of the primative vector types.
    statsList[[i]] <- stat
  }
  statsList
}


saveSR <- function (sr, col) {
  if (is.na(sr@"_id")) {
    ## Insert
    jso <- as.json(sr)
    col$insert(jso)
    it <- col$iterate(jso,'{"_id":true}',limit=1)
    sr@"_id" <- it$one()$"_id"
  } else {
    ## Replace
    col$update(paste('{"_id":{"$oid":"',sr@"_id",'"}}',sep=""),
               paste('{"$set":',as.json(sr),'}',sep=""))
  }
  sr
}

getSRbyID <- function(id,col) {
  it <- col$iterate(paste('{"_id":{"$oid":"',id,'"}}',sep=""),
                    '{}',limit=1)
  rec <- it$one()
  if (is.null(rec)) return(rec)
  parseSR(rec)
}

buildSRquery <- function (uid,context,before,after,
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


getSRone <- function(query,col) {
  it <- col$iterate(query,'{}',sort='{"timestamp":-1}',limit=1)
  rec <- it$one()
  if (is.null(rec)) return(rec)
  parseEvidence(rec)
}

getSRmany <- function(query,sort=-1,col) {
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



###########
## Evidence Lists

setGeneric("evidence",function(x) standardGeneric("evidence"))
setMethod("evidence","StudentRecord", function(x) x@evidence)


