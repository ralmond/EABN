library(EABN)
app <- "ecd://epls.coe.fsu.edu/PhysicsPlayground/Sp2019/EAConfig"
source("/usr/local/share/Proc4/EAini.R")

sess <- NeticaSession(LicenseKey=NeticaLicenseKey)
startSession(sess)

mantab <- read.csv(file.path(config.dir, manifestFile),as.is=TRUE)
if (is.null(mantab$Name)) stop ("No names in manifest file",
                                file.path(config.dir, manifestFile))

badnames <- !sapply(mantab$Name,is.IDname)
if (any(badnames))
  stop ("Illegal Netica names in manifest:",
        paste(mantab$Name[badnames],collapse=", "))

if (is.null(mantab$Pathname)) stop ("No pathnames in manifest file",
                                file.path(config.dir, manifestFile))

rownames(mantab) <- mantab$Name
mantab$Pathname <- file.path(config.dir,mantab$Pathname)

PPNethouse <- BNWarehouse(manifest=mantab,session=sess,key="Name")

PPnets <- lapply(mantab$Name,function(nm) WarehouseSupply(PPNethouse,nm))
names(PPnets) <- mantab$Name

PPCM <- PPnets[[1]]
PPEMs <- PPnets[-1]

PPnodelist <- NetworkAllNodes(PPCM)
for (n in 1:length(PPEMs)) {
  PPnodelist <- c(PPnodelist,
                  lapply(NetworkAllNodes(PPEMs[[n]]),as.Pnode))
}
names(PPnodelist) <- NULL


PPnodeman <- BuildNodeManifest(PPnodelist)

