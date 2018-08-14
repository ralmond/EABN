### This file is mostly to test connections to the database and make
### sure we can read and write objects.
library(jsonlite)
library(mongolite)

pwds <- do.call(rbind,read_json("~/.Proc4.json"))
cred <- pwds[pwds[,"user"]=="EAP",]

ear.es <- mongo("EvidenceSets","EARecords",
                url=paste("mongodb://",
                          cred$user,":",
                          cred$pwd,
                          "@127.0.0.1:27017/EARecords",sep=""),
                verbose=TRUE)

es1 <- EvidenceSet("Phred","Task1",obs=list(isCorrect=TRUE,selection="B"))
es1@seqno <- 1L
es2 <- EvidenceSet("Phred","Task2",obs=list(isCorrect=FALSE,selection="D"))
es2@seqno <- 2L
es3 <- EvidenceSet("Fred","Task1",obs=list(isCorrect=FALSE,selection="C"))
es3@seqno <- 1L

## Saving it back recovers the _id
es1 <- saveES(es1,ear.es)
es2 <- saveES(es2,ear.es)
es3 <- saveES(es3,ear.es)

## Test of retrieve by ID
es1a <- getESbyID(es1@"_id",ear.es)
stopifnot(all.equal(details(es1a),details(es1)))


## query string examples
cat(buildESquery("Phred"),"\n")
cat(buildESquery("Phred","Task1"),"\n")
cat(buildESquery("Phred",c("Task1","Task2")),"\n")
cat(buildESquery(c("Phred","Fred"),c("Task1","Task2")),"\n")
cat(buildESquery("Phred",mess="Accumulate"),"\n")
cat(buildESquery("Phred",mess=c("Accumulate","Retract")),"\n")

cat(buildESquery("Phred",timestamp=Sys.time()),"\n")
cat(buildESquery("Phred",after=Sys.time()),"\n")
cat(buildESquery("Phred",before=Sys.time()),"\n")
cat(buildESquery("Phred",after=Sys.time()-as.difftime(1,units="hours"),
                 before=Sys.time()),"\n")

cat(buildESquery("Phred",seqno=2),"\n")
cat(buildESquery("Phred",seqno=2:3),"\n")
cat(buildESquery("Phred",before=3),"\n")
cat(buildESquery("Phred",after=2),"\n")
cat(buildESquery("Phred",before=3,after=2),"\n")

es2c <- getESone(buildESquery("Phred"),ear.es)
es3c <- getESone(buildESquery("Fred"),ear.es)

Phred.es <- getESmany(buildESquery("Phred"),col=ear.es)
Fred.es <- getESmany(buildESquery("Fred"),col=ear.es)
Phred.esr <- getESmany(buildESquery("Phred"),col=ear.es,sort=1)


### Need test of save/replace mode.

