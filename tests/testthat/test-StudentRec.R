test_that("StudentRecord", {

  wh <- local_warehouses()
  dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                     context="*Baseline*")
  sm(dsr) <- Peanut::WarehouseSupply(wh$Nethouse,"miniPP_CM")
  Peanut::PnetCompile(sm(dsr))

  ## dsr <- updateStats(eng,dsr)

  dsr@stats <- lapply(wh$statlist,
                      function (stat) Peanut::calcStat(stat,sm(dsr)))
  names(dsr@stats) <- names(wh$statlist)
  stat(dsr,"Physics_EAP")
  stat(dsr,"Physics_Margin")

  ## dsr <- baselineHist(eng,dsr)

  dsr@hist <- lapply(c("Physics"),
                     function (nd)
                     EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
  names(dsr@hist) <- "Physics"
  history(dsr,"Physics")

  ## Serialization and unserialization
  dsr.ser <- mongo::as.json(dsr)

  dsr1 <- parseStudentRecord(jsonlite::fromJSON(dsr.ser))
  dsr1 <- fetchSM(dsr1,wh$Nethouse)

  ## dsr and dsr1 should be the same.
  expect_equal(app(dsr),app(dsr1))
  expect_equal(uid(dsr),uid(dsr1))
  expect_equal(context(dsr),context(dsr1))
  ## problems with timezones
  ## all.equal(timestamp(dsr),timestamp(dsr1)),
  expect_equal(seqno(dsr),seqno(dsr1))
  expect_equal(stats(dsr),stats(dsr1),tolerance=.0002)
  expect_equal(history(dsr,"Physics"),history(dsr1,"Physics"))
  expect_equal(Peanut::PnetName(sm(dsr)), Peanut::PnetName(sm(dsr)))

})

test_that("toString(StudentRecord) & show()", {

})


test_that("app(StudentRecord)", {

})

test_that("uid(StudentRecord)", {

})

test_that("context(StudentRecord)", {

})

test_that("timestamp(StudentRecord)", {

})

test_that("seqno(StudentRecord)", {
  ## Getter & Setter
})

test_that("stats(StudentRecord), stat() & statNames()", {

  stats <- list(Physics_EAP=0,EnergyTransfer_EAP=.15,
                Physics_Margin=c(High=1/3,Medium=1/3,
                                 Low=1/3))

  dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                       context="*Baseline*",stats=stats)

  expect_equal(stats,stats(dsr),tolerance=.0002)

  expect_equal(statNames(dsr),names(stats))

  expect_equal(stat(dsr,"Physics_Margin"),stats[[3]],tolerance=.0002)


})

test_that("hist(StudentRecord), history() & histNames()", {

  wh <- local_warehouses()

  dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                       context="*Baseline*")
  sm(dsr) <- Peanut::WarehouseSupply(wh$Nethouse,"miniPP_CM")
  Peanut::PnetCompile(sm(dsr))
  ## dsr <- updateStats(eng,dsr)

  dsr@stats <- lapply(wh$statlist,
                      function (stat) Peanut::calcStat(stat,sm(dsr)))
  names(dsr@stats) <- names(wh$statlist)

  ## dsr <- baselineHist(eng,dsr)

  dsr@hist <- lapply(c("Physics"),
                     function (nd)
                       EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
  names(dsr@hist) <- "Physics"
  expect_equal(histNames(dsr),"Physics")
  history(dsr,"Physics")
  skip("Need to check this.")

})



test_that("evidenceLog(StudentRecord)", {
  ## Getter & Setter
})


test_that("useObs(StudentRecord) & ignoreObs(StudentRecord)", {

})

test_that("getIssues(StudentRecord) & logIssues()", {

  sr0 <-
    StudentRecord("S1","*baseline*",as.POSIXct("2020-03-30 09:00:00"))

  sr0 <- logIssue(sr0,"Test Issue")
  err <- simpleError("Another test error.")
  sr0 <- logIssue(sr0,err)
  getIssues(sr0)
  skip("Need to check")
})

test_that("sm(StudentRecord)", {
  ## Getter and Setter
  wh <- local_warehouses()

  dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                       context="*Baseline*")
  sm(dsr) <- Peanut::WarehouseSupply(wh$Nethouse,"miniPP_CM")
  Peanut::PnetCompile(sm(dsr))
  skip("Need to finish test")

})

test_that("fetchSM(StudentRecord)", {

  wh <- local_warehouses()

  dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                       context="*Baseline*")
  sm(dsr) <- Peanut::WarehouseSupply(wh$Nethouse,"miniPP_CM")
  Peanut::PnetCompile(sm(dsr))

  ## dsr <- updateStats(eng,dsr)
  dsr@stats <- lapply(wh$statlist,
                      function (stat) Peanut::calcStat(stat,sm(dsr)))
  names(dsr@stats) <- names(wh$statlist)

  ## dsr <- baselineHist(eng,dsr)
  dsr@hist <- lapply(c("Physics"),
                     function (nd)
                       EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
  names(dsr@hist) <- "Physics"

  pnodenames <- names(Peanut::PnetPnodes(sm(dsr)))


  ## Serialization and unserialization
  dsr.ser <- mongo::as.json(dsr)

  dsr1 <- parseStudentRecord(jsonlite::fromJSON(dsr.ser))
  expect_null(sm(dsr1))
  ## at this point, SM has not yet been restored.


  ## It is there in the serial field
  net1 <- unpackSM(dsr1,wh$Nethouse)
  expect_equal(pnodenames,names(Peanut::PnetPnodes(net1)))
  dsr1 <- fetchSM(dsr1,wh$Nethouse)
  expect_equal(pnodenames,names(Peanut::PnetPnodes(sm(dsr1))))

  ## Try this again, but first delete net from warehouse,
  ## So we are sure we are building it from serialized version.
  Peanut::WarehouseFree(wh$Nethouse,Peanut::PnetName(sm(dsr)))

  dsr1 <- parseStudentRecord(jsonlite::fromJSON(dsr.ser))
  expect_null(sm(dsr1))
  ## at this point, SM has not yet been restored.


  ## It is there in the serial field
  net1 <- unpackSM(dsr1,wh$Nethouse)
  expect_equal(pnodenames,names(Peanut::PnetPnodes(net1)))
  dsr1 <- fetchSM(dsr1,wh$Nethouse)
  expect_equal(pnodenames,names(Peanut::PnetPnodes(sm(dsr1))))

})

test_that("unpackSM(StudentRecord)", {

})

test_that("evidence(StudentRecord)", {
  ## Getter & Setter
})


test_that("as.jlist(StudentRecord)", {

})


test_that("parseStudentRecord(StudentRecord)", {

})

test_that("unparseStats", {

})

test_that("stats2json", {

})

test_that("strsplit2", {

})

test_that("unflatten", {

})

test_that("parseStats", {

  stats <- list(Physics_EAP=0,EnergyTransfer_EAP=.15,
                Physics_Margin=c(High=1/3,Medium=1/3,
                               Low=1/3))
  stats2json(stats)

  stats1 <- parseStats(mongo::ununboxer(unparseStats(stats)))
  expect_equal(stats,stats1,tolerance=.0002)

  stats2json(stats,flatten=TRUE)

  stats2 <- parseStats(mongo::ununboxer(unparseStats(stats,flatten=TRUE)))
  expect_equal(stats,stats2,tolerance=.0002)

})

test_that("unflattenNames", {

})


test_that("updateRecord", {

})

test_that("StudentRecordSet", {

})

test_that("StudentRecordSet", {

})

test_that("app(StudentRecordSet)", {
  wh <- local_warehouses()
  ## Setup to test without Mongo
  SRS <- StudentRecordSet(app="Test",warehouse=wh$Nethouse,
                          db=mongo::MongoDB(noMongo=TRUE))
  stopifnot(!mdbAvailable((SRS$recorddb())))

  ## Setup default SR
  dsr <- StudentRecord("*DEFAULT*",app="Test",
                       context="*Baseline*")
  sm(dsr) <- Peanut::WarehouseSupply(wh$Nethouse,"miniPP_CM")
  Peanut::PnetCompile(sm(dsr))

  ## dsr <- updateStats(eng,dsr)
  dsr@stats <- lapply(wh$statlist,
                      function (stat) Peanut::calcStat(stat,sm(dsr)))
  names(dsr@stats) <- names(wh$statlist)

  dsr@hist <- lapply(c("Physics"),
                     function (nd)
                       EABN:::uphist(sm(dsr),nd,NULL,"*Baseline*"))
  names(dsr@hist) <- "Physics"

  SRS$defaultSR <- dsr
  saveSR(SRS, dsr)

  ## Make a new Student Record for a student.
  sr1 <- newSR(SRS,"S1")
  expect_equal(uid(sr1),"S1")
  expect_equal(app(sr1),app(dsr))
  expect_equal(stats(dsr),stats(sr1),tolerance=.0002)


  sr1a <- getSR(SRS,"S1")

  clearSRs(SRS)

})

test_that("getSR(StudentRecordSet) No Mongo", {
  ## Requires PNetica

  app <- "ecd://epls.coe.fsu.edu/EITest"
  wh <- local_warehouses()

  cl <- new("CaptureListener")
  listeners <- list("cl"=cl)

  ls <- Proc4::ListenerSet(sender= paste("EAEngine[",app,"]"),
                    db=mongo::MongoDB(noMongo=TRUE), listeners=listeners)

  eng <- newBNEngineNDB(app=app,warehouse=wh$Nethouse,
                        listenerSet=ls,manifest=wh$netman,
                        profModel="miniPP_CM",
                        histNodes="Physics",
                        statmat=stattab,
                        activeTest="EAActive.txt")

  ## Standard initialization methods.
  loadManifest(eng,wh$netman)
  eng$setHistNodes("Physics")
  configStats(eng,wh$statmat)
  setupDefaultSR(eng)

  tr1 <- newSR(eng$studentRecords(),"Test1")
  Peanut::PnetCompile(sm(tr1))
  expect_equal(uid(tr1),"Test1")
  expect_lt(abs(stat(tr1,"Physics_EAP")), .0001)
  expect_true(is.na(mongo::m_id(tr1)))  # id is NA as it has not been saved yet.

  tr1 <- saveSR(eng$studentRecords(),tr1)
  mongo::m_id(tr1)
  expect_false(is.na(mongo::m_id(tr1)))  # Now set

  sr0 <- getRecordForUser(eng,"S1")

  eap0 <- stat(sr0,"Physics_EAP")

  e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                    obs=list("CompensatoryObs"="Right"))

  e1 <- logEvidence(eng,sr0,e1)
  sr1 <- accumulateEvidence(eng,sr0,e1)
  if (is(sr1,'try-error')) fail(sr1)
  expect_false(mongo::m_id(sr1)==mongo::m_id(sr0))
  expect_equal(sr1@prev_id,mongo::m_id(sr0))
  expect_equal(seqno(sr1),1L)
  expect_equal(seqno(e1),1L)

  eap1 <- stat(sr1,"Physics_EAP")
  expect_gt(abs(eap1-eap0), .001)
  expect_equal(nrow(history(sr1,"Physcis")),2L)

  sr1.ser <- mongo::as.json(sr1)
  Peanut::WarehouseFree(Nethouse,Peanut::PnetName(sm(sr1))) # Delete student model to
                                        # force restore.

  sr1a <- getSR(eng$studentRecords(),"S1",mongolite::fromJSON(sr1.ser))
  Peanut::PnetCompile(sm(sr1a))
  eap1a <- stat(sr1a,"Physics_EAP")
  expect_lt(abs(eap1-eap1a), .001)
  expect_equal(nrow(history(sr1a,"Physcis")),2L)

})


test_that("getSR(StudentRecordSet) Mongo", {
  skip_if_not(MongoAvailable)
  wh <- local_warehouses()
  app <- "ecd://epls.coe.fsu.edu/EITest"
  cl <- new("CaptureListener")
  listeners <- list("cl"=cl)

  ls <- Proc4::ListenerSet(sender= paste("EAEngine[",app,"]"),
                    db=mongo::MongoDB("Messages","EARecords",
                                      mongo::makeDBuri()),
                    registryDB=mongo::MongoDB("OutputFiles",
                                              "Proc4",
                                              mongo::makeDBuri()),
                    listeners=listeners)

  eng <- newBNEngineMongo(app=app,warehouse=wh$Nethouse,
                          listenerSet=ls,
                          profModel="miniPP_CM",
                          histNodes="Physics",
                          dburi=mongo::makeDBuri(),
                          dbname="EARecords",admindbname="Proc4")

  ## Standard initialization methods.
  loadManifest(eng,wh$netman)
  eng$setHistNodes("Physics")
  configStats(eng,wh$statmat)
  setupDefaultSR(eng)
  skip("Test not finished")

})

test_that("revertSM(StudentRecordSet)", {

})

test_that("saveSR(StudentRecordSet)", {

})

test_that("newSR(StudentRecordSet)", {

})

test_that("clearSRs(StudentRecordSet)", {

})



