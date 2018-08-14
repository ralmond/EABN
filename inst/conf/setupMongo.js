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
load("/home/ralmond/.Proc4.js")

eapUser = pwds.filter(function(u) {return u.user == "EAP";})[0];

con = new Mongo();
db=con.getDB("EARecords");                 
db.auth(eapUser.user,eapUser.pwd);
db.createCollection("EvidenceSets", {
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
                    bsonType: "date",
                    description: "Timestamp"
                },
                mess: {
                    bsonType: "string",
                    description: "Instructions to EAP"
                },
                obs: {
                    bsonType: "object",
                    description: "Named list of evidence."
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
