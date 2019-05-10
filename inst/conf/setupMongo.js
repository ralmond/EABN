// This file contains javascript to set up the collections expected by
// the EA process

// Before running this script, create passwords for the Proc4
// processes you will use.
// You should put these in a file called ".Proc4.js" in your home
// directory.  It should look like:
// var pwds = [
//     {"user":"EAP","pwd":"Secret1"},
//     {"user":"ASP","pwd":"Secret2"},
//     {"user":"EIP","pwd":"Secret3"},
//     {"user":"C4","pwd":"Secret4"},
// ];
// Then load that file.  Change the next line
// To reflect the name of that path.
load("/usr/local/share/Proc4/Proc4.js")

eapUser = pwds.filter(function(u) {return u.user == "EAP";})[0];

con = new Mongo();
db=con.getDB("EARecords");                 
db.auth(eapUser.user,eapUser.pwd);
db.createCollection("Observables", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","uid","context","timestamp","seqno"],
            properties: {
                _id: {
                    bsonType: "string",
                    description: "app@uid@seqno",
                },
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                uid: {
                    bsonType: "string",
                    description: "User (studnet) ID (string)"
                },
                context: {
                    bsonType: "string",
                    description: "Context (task) ID (string)"
                },
                sender: {
                    bsonType: "string",
                    description: "Who posted this message."
                },
                timestamp: {
                    bsonType: "date",
                    description: "Timestamp"
                },
                mess: {
                    bsonType: "string",
                    description: "Instructions to EAP"
                },
                data: {
                    bsonType: "object",
                    description: "Named list of evidence."
                },
                processed: {
                    bsonType: "bool",
                    description: "Has this record been processed?"
                }
            }
        }
    },
    validationAction: "warn"
})
db.EvidenceSets.createIndex( { app:1, uid: 1, timestamp:-1});
db.createCollection("StudentRecords", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","uid","context","timestamp","seqno"],
            properties: {
                _id: {
                    bsonType: "string",
                    description: "app@uid@seqno",
                },
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                uid: {
                    bsonType: "string",
                    description: "User (studnet) ID (string)"
                },
                context: {
                    bsonType: "string",
                    description: "Context (task) ID (string)"
                },
                timestamp: {
                    bsonType: "Date",
                    description: "Timestamp"
                },
                evidence: {
                    bsonType: "array",
                    description: "List of evidence set ids."
                },
                sm: {
                    bsonType: "object",
                    description: "Serialized student model."
                },
                stats: {
                    bsonType: "object",
                    description: "Statistics about student."
                },
                hist: {
                    bsonType: "object",
                    description: "Statistics about student."
                },
                prev: {
                    bsonType: "string",
                    description: "Reference to previous student model."
                },
                seqno: {
                    bsonType: "int",
                    description: "Sequence number for this step for student."
                }
            }
        }
    },
    validationAction: "warn"
})
db.StudentRecords.createIndex( { app:1, uid: 1, timestamp:-1});
db.createCollection("Manifest", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","Name","Hub","Pathname",],
            properties: {
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                Name: {
                    bsonType: "string",
                    description: "Model Name"
                },
                Hub: {
                    bsonType: "string",
                    description: "Name of Proficiency Model, or null for PM"
                },
                Pathname: {
                    bsonType: "string",
                    description: "Pathname for Network."
                },
                Description: {
                    bsonType: "string",
                    description: "Documentation string for network."
                },
            }
        }
    },
    validationAction: "warn"
})
db.Manifest.createIndex( { app:1, Name:1 })
db.createCollection("Statistics", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","Name","Fun","Node",],
            properties: {
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                Name: {
                    bsonType: "string",
                    description: "Model Name"
                },
                Node: {
                    bsonType: "string",
                    description: "Name of node in Proficiency Model"
                },
                Fun: {
                    bsonType: "string",
                    description: "Name of calculation Function."
                },
                Doc: {
                    bsonType: "string",
                    description: "Documentation string for network."
                },
            }
        }
    },
    validationAction: "warn"
})
db.Manifest.createIndex( { app:1, Name:1 })
db.createCollection("Messages", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","uid","timestamp"],
            properties: {
                _id: {
                    bsonType: "string",
                    description: "app@uid@seqno",
                },
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                uid: {
                    bsonType: "string",
                    description: "User (studnet) ID (string)"
                },
                context: {
                    bsonType: "string",
                    description: "Context (task) ID (string)"
                },
                sender: {
                    bsonType: "string",
                    description: "Who posted this message."
                },
                mess: {
                    bsonType: "string",
                    description: "Instructions to PP/ASP"
                },
                timestamp: {
                    bsonType: "date",
                    description: "Timestamp"
                },
                data: {
                    bsonType: "object",
                    description: "Named list of statistics or WOEs."
                }
            }
        }
    },
    validationAction: "warn"
})
db.Messages.createIndex( { app:1, uid: 1, timestamp:-1});
db.createCollection("histNodes", {
    validator: {
        $jsonSchema: {
            bsonType: "object",
            required: ["app","Nodes",],
            properties: {
                app: {
                    bsonType: "string",
                    description: "Application ID (string)"
                },
                Nodes: {
                    bsonType: "array",
                    description: "Names of nodes in Proficiency Model"
                },
            }
        }
    },
    validationAction: "warn"
})
