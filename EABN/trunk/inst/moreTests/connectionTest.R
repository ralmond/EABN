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


################
## Student Records

ear.sr <- mongo("StudentRecords","EARecords",
                url=paste("mongodb://",
                          cred$user,":",
                          cred$pwd,
                          "@127.0.0.1:27017/EARecords",sep=""),
                verbose=TRUE)

MySession <- NeticaSession(SessionName="MySession")
startSession(MySession)

emsmMotif <-ReadNetworks(file.path(library(help="RNetica")$path,
                                   "sampleNets","EMSMMotif.dne"),
                         session=MySession)
NetworkAllNodes(emsmMotif)
CompileNetwork(emsmMotif)

Bobby_sm <- CopyNetworks(emsmMotif,"Bobby_sm","no_visual")

sr0 <- StudentRecord(app="emsm",uid="Bobby",context="*baseline*",
                     sm = Bobby_sm)

NetworkAllNodes(Bobby_sm)
CompileNetwork(Bobby_sm)
sr0@stats <- list(Skill1=NodeBeliefs(Bobby_sm$nodes$Skill1),
                  Skill2=NodeBeliefs(Bobby_sm$nodes$Skill2),
                  Skill3=NodeBeliefs(Bobby_sm$nodes$Skill3))
sr0 <- saveSR(sr0,ear.sr)

sr0a <- getSRbyID(sr0@"_id",ear.sr)
sr0@seqno <- 0L
sr0 <- saveSR(sr0,ear.sr)
sr0a <- getSRbyID(sr0@"_id",ear.sr)
stopifnot(sr0a@seqno==0L)

eb1 <- EvidenceSet(app="emsm",uid="Bobby",context="Task1a",
                   mess="New Observables",
                   obs=list(Obs1a1="Right",Obs1a2="Wrong"))
eb1@seqno <- 1L

eb1 <- saveES(eb1,ear.es)


sr1 <- StudentRecord(app="emsm",uid="Bobby",context="Task1",
                     sm = sr1@sm,evidence=list(eb1))
sr1@prev <- sr0
sr1@seqno <- 1L
NetworkAllNodes(sr1@sm)
CompileNetwork(sr1@sm)
NodeFinding(sr1@sm$nodes$Obs1a1) <- "Right"
NodeFinding(sr1@sm$nodes$Obs1a2) <- "Wrong"
sr1@stats <- list(Skill1=NodeBeliefs(Bobby_sm$nodes$Skill1),
                  Skill2=NodeBeliefs(Bobby_sm$nodes$Skill2),
                  Skill3=NodeBeliefs(Bobby_sm$nodes$Skill3))

sr1 <- saveSR(sr1,ear.sr)
sr1a <- getSRbyID(sr1@"_id",ear.sr)
NetworkAllNodes(sr1a@sm)
CompileNetwork(sr1a@sm)
stopifnot(all(abs(NodeBeliefs(sm(sr1a)$nodes$Skill1) -
                  stats(sr1a)$Skill1) < .0001))

## Check to make sure we can go backwards.
sr0b <- getSRbyID(sr0@"_id",ear.sr)
NetworkAllNodes(sm(sr0b))
CompileNetwork(sm(sr0b))
stopifnot(all(abs(NodeBeliefs(sm(sr0b)$nodes$Skill1) -
                  stats(sr0b)$Skill1) < .0001))


## Check to make sure we can go forwards.
sr1b <- getSRbyID(sr1@"_id",ear.sr)
NetworkAllNodes(sm(sr1b))
CompileNetwork(sm(sr1b))
stopifnot(all(abs(NodeBeliefs(sm(sr1b)$nodes$Skill1) -
                  stats(sr1b)$Skill1) < .0001))
