\name{parseEvidence}
\alias{parseEvidence}
\alias{as.jlist,EvidenceSet,list-method}
\title{Convert EvidenceSet objects to and from JSON}
\description{

  The \code{\link[Proc4]{as.json}} function takes an
  \code{\linkS4class{EvidenceSet}} (among other objects) and turns it
  into JSON.  The function \code{parseEvidence} takes the list produced
  as the output to \code{\link[jsonlite]{fromJSON}} and turns it back
  into an \code{\linkS4class{EvidenceSet}} object.  

}
\usage{
parseEvidence(rec)
\S4method{as.jlist}{EvidenceSet,list}(obj, ml, serialize=TRUE)
}
\arguments{
  \item{rec}{A list which comes from running
    \code{\link[jsonlite]{fromJSON}}  on a JSON string, or database
    extraction method.}
  \item{obj}{The object being serialized; usually \code{attributes(obj)}.}
  \item{ml}{A list of fields of the object.}
  \item{serialize}{A logical flag. If true,
    \code{\link[jsonlite]{serializeJSON}} is used to protect the
    \code{data} field (and other objects which might contain complex R
    code.} 

}
\details{

  See the description for \code{\link[Proc4]{as.json}} for more
  description of the JSON conversion prototocl.

  The \code{parseEvidence} method is designed to be used with the
  \code{\link[Proc4]{getOneRec}} and \code{\link[Proc4]{getManyRecs}}
  functions for fetching information from the database.
}
\value{

  The function \code{parseEvidence} returns an object of class
  \code{\linkS4class{EvidenceSet}}.

  The \code{as.jlist} method returns a list which can be passed to
  \code{\link[jsonlite]{toJSON}} to produce legible JSON from the R
  object. 
}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{EvidenceSet}}, \code{\link[Proc4]{as.json}},
  \code{\link[Proc4]{getOneRec}}, \code{\link[Proc4]{getManyRecs}}

}
\examples{

e1 <- EvidenceSet(uid="S1",app="Test",context="PPcompEM",
                  obs=list("CompensatoryObs"="Right"))

e2 <- EvidenceSet(uid="S1",app="Test",context="PPdurAttEM",
                  obs=list("Attempts"=2,"Duration"=38.3))

e1.ser <- as.json(e1)
e1a <- parseEvidence(fromJSON(e1.ser))
e2.ser <- as.json(e2)
e2a <- parseEvidence(fromJSON(e2.ser))

stopifnot(all.equal(e1,e1a), all.equal(e2,e2a))

}
\keyword{ interface }

