
testdir <- "/home/ralmond/Projects/EABN/inst"
mantab <- read.csv(file.path(testdir,"sampleAssessment",
                             "PPsubsetManifest.csv"),as.is=TRUE)
rownames(mantab) <- mantab$Name

app <- "ecd://epls.coe.fsu.edu/P4test"
col <- mongo("manfred")
mantab$app <- "ecd://epls.coe.fsu.edu/P4test"
col$insert(mantab)
col$find(sprintf('{ "app":"%s"}',app))
