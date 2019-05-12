## These are application generic parameters
EAeng.common <- list(host="localhost",username="EA",password="secret",
                     profModel="PPOrangeNodes",
                     dbname="EARecords",P4dbname="Proc4",waittime=.25)
appstem <- basename(app)

## These are for application specific parameters
EAeng.params <- list(app=app)


logfile <- file.path("/usr/local/share/Proc4/logs",
                     paste("EA_",appstem,"0.log",sep=""))

trophy2json <- function(dat) {
  paste('{', '"trophyHall"', ':','[',
        paste(
            paste('{"',names(dat$trophyHall),'":"',dat$trophyHall,'"}',
                  sep=""), collapse=", "), '],',
        '"bankBalance"', ':', dat$bankBalance, '}')
}

EA.listenerSpecs <-
  list("InjectionListener"=list(sender=paste("EA",appstem,sep="_"),
            dbname="ASRecords",dburi="mongodb://localhost",
            colname="Statistics",messSet="Statistics"),
       "UpdateListener"=list(dbname="Proc4",dburi="mongodb://localhost",
            colname="Statistics",targetField="data",
            messSet=c("Statistics"),
            jsonEncoder="stats2json"))

NeticaLicenseKey <- ""

config.dir <- "/home/ralmond/ownCloud/Projects/NSFCyberlearning/EvidenceAc/Sp2019nets"
manifestFile <- "PPManifest.csv"
statFile <- "StatisticList.csv"

histNodes <-c("Physics","Torque","Energy","LinearMomentum","ForceAndMotion")
histNodes <-c("Physics")

