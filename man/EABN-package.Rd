\name{EABN-package}
\alias{EABN-package}
\alias{EABN}
\docType{package}
\title{
\packageTitle{EABN}
}
\description{
\packageDescription{EABN}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{EABN}
\packageIndices{EABN}

The heart of this package is the \code{\linkS4class{BNEngine}} class, which comes in two flavors:
 \code{\linkS4class{BNEngineMongo} and  \code{\linkS4class{BNEngineNDB}}} (no database).
 In the database flavour, the message queue is a collection in the databse.  In the NDB flavour,
 the calling program must implement its own message queue.

The function \code{\link{doBuild}} builds

}
\author{
\packageAuthor{EABN}

Maintainer: \packageMaintainer{EABN}
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
}
