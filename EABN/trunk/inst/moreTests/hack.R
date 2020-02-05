ear.hn <- mongo("histNodes","EARecords")
tapp <- "ecd://epls.coe.fsu.edu/P4test"

nodes <- c("Physics","Energy")
paste('"',nodes,'"',sep="",collapse=",")
sprintf('{"app":"%s", "Nodes":[%s]}',tapp,
        paste('"',nodes,'"',sep="",collapse=","))
ear.hn$insert(sprintf('{"app":"%s", "Nodes":[%s]}',tapp,
                      paste('"',nodes,'"',sep="",collapse=",")))

ear.hn$find(buildJQuery(app=tapp),)

