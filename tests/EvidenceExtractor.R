library(EABN)
eviddb <- try(mongolite::mongo("EvidenceSets","EARecords","mongodb://localhost"))
if (FALSE) {
#ev1 <- getOneRec('{}',eviddb,parseEvidence)

evidSet <- mongo::getManyRecs(eviddb,'{"app":"ecd://epls.coe.fsu.edu/P4test"}',
                       parseEvidence)

allfields <- sapply(evidSet, function (s) names(Proc4::details(s)))
ufields <- unique(do.call(c,allfields))
nfields <- c("uid","context","timestamp","app",ufields[ufields!="trophyHall"])

N <- length(evidSet)
obsDF <- lapply(nfields, function(n) rep(NA,N))
names(obsDF) <- nfields
obsDF <- as.data.frame(obsDF)
obsDF$timestamp <- rep(NA,N)


for (n in 1:N) {
  obsDF[n,"uid"] <- uid(evidSet[[n]])
  obsDF[n,"context"] <- context(evidSet[[n]])
  obsDF[n,"timestamp"] <- toString(timestamp(evidSet[[n]]))
  obsDF[n,"app"] <- basename(app(evidSet[[n]]))
  obs <- Proc4::details(evidSet[[n]])
  for (oname in names(obs)) {
    if (oname %in% names(obsDF)) {
      val <- obs[[oname]]
      if (oname=="agentsUsed")
        val <- paste(val,collapse=", ")
      if (length(val) > 0L)
        obsDF[n,oname] <- val
    }
  }
}
write.csv(obsDF,"Observables2019-05-08.csv")
}
