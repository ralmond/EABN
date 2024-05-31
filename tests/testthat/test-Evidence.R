test_that("Evidence Set Creation",{

  e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                    obs=list("CompensatoryObs"="Right"))
  expect_equal(uid(e1),"S1")
  expect_equal(app(e1),"Test")
  expect_equal(context(e1),"PPcompEM")
  expect_equal(observables(e1),list(CompensatoryObs="Right"))

  e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                    obs=list("Attempts"=2,"Duration"=38.3))
  expect_equal(uid(e2),"S1")
  expect_equal(app(e2),"Test")
  expect_equal(context(e2),"PPdurAttEM")
  expect_equal(observables(e2),list(Attempts=2,Duration=38.3))

})

test_that("Evidence Set seqno", {

  e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                    obs=list("CompensatoryObs"="Right"))
  expect_true(is.na(seqno(e1)))
  seqno(e1) <- 10
  expect_equal(seqno(e1),10)

})

test_that("Evidence Set printing", {

  e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                    obs=list("CompensatoryObs"="Right"))
  expect_equal(toString(e1),
               "EvidenceSet:{ uid: S1 , context: PPcompEM , seqno: NA }")

  e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                    obs=list("Attempts"=2,"Duration"=38.3))
  seqno(e2) <- 3
  expect_equal(toString(e2),
               "EvidenceSet:{ uid: S1 , context: PPdurAttEM , seqno: 3 }")

})

test_that("Evidence Set timestamps", {

  e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                    obs=list("CompensatoryObs"="Right"))
  expect_lt(difftime(Sys.time(),timestamp(e1)),
                   as.difftime(10,unit="mins"))

  atime <- as.POSIXlt("2024-03-17 04:06:07 EDT")
  e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                    timestamp=atime,
                    obs=list("Attempts"=2,"Duration"=38.3))
  expect_equal(timestamp(e2),as.POSIXct(atime))

})

test_that("Evidence Set as.jlist", {
  e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                    obs=list("CompensatoryObs"="Right"))

  e1j <- mongo::as.json(e1)
  e1r <- mongo::parse.json(e1j)
  expect_equal(e1r,e1, ignore_attr=c("tzone","waldo_opts"))

  e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                    timestamp=as.POSIXlt("2024-03-17 04:05:06 EDT"),
                    obs=list("Attempts"=2,"Duration"=38.3))
  e2j <- mongo::as.json(e2)
  e2r <- mongo::parse.json(e2j)
  expect_equal(e2r,e2, ignore_attr=c("tzone","waldo_opts"))


})

test_that("Evidence Set json parse/deparse ", {

})

test_that("Evidence Log Entry, eid, context", {

  el <- EvidenceLog("e1","t1",list(a=1,b="foo"),list(c=3,d="bar"))
  expect_equal(eid(el),"e1")
  expect_equal(context(el),"t1")
  expect_equal(observables(el)$used,list(a=1,b="foo"))
  expect_equal(observables(el)$ignored,list(c=3,d="bar"))
})

test_that("Evidence Log Entry useObs", {
  el <- EvidenceLog("e2","t2")
  expect_equal(observables(el)$used,list())
  expect_equal(observables(el)$ignored,list())

  el <- useObs(el,"a",TRUE)
  expect_equal(observables(el)$used,list(a=TRUE))
  expect_equal(observables(el)$ignored,list())

  el <- useObs(el,"b","good")
  expect_equal(observables(el)$used,list(a=TRUE,b="good"))
  expect_equal(observables(el)$ignored,list())


})

test_that("Evidence Log Entry ignoreObs", {

  el <- EvidenceLog("e3","t3")
  expect_equal(observables(el)$used,list())
  expect_equal(observables(el)$ignored,list())

  el <- ignoreObs(el,"a",FALSE)
  expect_equal(observables(el)$used,list())
  expect_equal(observables(el)$ignored,list(a=FALSE))

  el <- ignoreObs(el,"b","bad")
  expect_equal(observables(el)$ignored,list(a=FALSE,b="bad"))
  expect_equal(observables(el)$used,list())


})

test_that("Evidence Log Entry as.json", {

  el <- EvidenceLog("e1","t1",list(a=1,b="foo"),list(c=3,d="bar"))
  elj <- mongo::as.json(el)
  elr <- mongo::parse.json(elj)
  expect_equal(elr,el)

  el0 <- EvidenceLog("e1","t1")
  elj0 <- mongo::as.json(el0)
  elr0 <- mongo::parse.json(elj0)
  expect_equal(elr0,el0)

})

