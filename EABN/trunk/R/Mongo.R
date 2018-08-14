### Functions for doing queries to the EAP mongo database.

getLatestSR<- function (eapsrs,app,uid) {
  it <- eapsrs$iterate(paste("{ app:\"",app,"\", uid:\"",uid,"\"}"),
                       sort('{timestamp:-1}',limit=1))
  rec <- it$one();                      #Only one record (limit=1)
  if (is.null(rec)) return(NULL)
  new("StudentRecord",_id=rec$_id, app=rec$app, uid=rec$uid,
      context=rec$context,
      evidence_ids=rec$evidence,
      timestamp=strptime(rec$timestamp, "%Y-%m-%dT%H:%M:%SZ", 'UTC'),
      sm=rec$sm, stats=parseStats(rec$stats),
      seqno=rec$seqno, prev_id =rec$prev)
}



