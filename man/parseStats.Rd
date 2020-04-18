\name{parseStats}
\alias{parseStats}
\alias{unparseStats}
\alias{stats2json}
\title{Functions for (un)serializing stats from student records.}
\description{

  The functions \code{unparseStats} and \code{stats2json} serialize the
  statistics as a JSON record.  The function \code{parseStats} reverses
  the process.

}
\usage{
parseStats(slist)
unparseStats(slist, flatten=FALSE)
stats2json(slist, flatten=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{slist}{A list of statistics.  For \code{parseStats} this should
    be the output of \code{\link[jsonlite]{fromJSON}}.  For the others,
    this is just a list of statistic values.}
  \item{flatten}{If true, then vector-valued statistics (i.e.,
    \code{\link[Peanut]{PnodeMargin}}, will have their values flattened
    into scalars.  If not they will be left as vectors.}
}
\details{

  The function \code{unparseStats} massages the list of statistics so it
  will be output in clean JSON (in particular, using
  \code{\link[Proc4]{unboxer}} to make sure scalars appear as scalars
  and not vectors). The function \code{stats2json} is just
  \code{\link[jsonlite]{toJSON}(unparseStats(slist)).} 

  If \code{flatten} is true, then vector value statstics will be
  flattened.  For example, if the statistic \dQuote{Physics_Margin} has
  three values with labels \dQuote{High}, \dQuote{Medium}, and
  \dQuote{Low}, then it will be replaced with three statistics with the
  names \dQuote{Physics_Margin.High}, \dQuote{Physics_Margin.Medium},
  and \dQuote{Physics_Margin.Low}.

  The function \code{parseStatistics} is designed to reverse the
  process. 

}
\value{

  The function \code{unparseStats} returns a list which is ready to be
  passed to \code{\link[jsonlite]{toJSON}}.  In particular, scalars are
  marked using \code{\link[Proc4]{unboxer}}.

  The function \code{stats2json} returns a string containing the JSON.

  The function \code{parseStats} returns a list of statistics values.
  this is suitable for being set to the \code{\link{stats}} field of the
  \code{\linkS4class{StudentRecord}} object.

}
\author{Russell Almond}
\note{

  When using \code{flatten=TRUE}, avoid periods, \sQuote{.}, in the
  names of statistics, as this marker is used to recreate the nested
  structure in \code{parseStats}.
}
\seealso{

  \code{\link[Proc4]{ParseMessage}} gives general information about how
  the parsing/unparsing protocol works.

  \code{\link[Peanut]{Statistic}} gives a list of available statistics.

  \code{\linkS4class{StudentRecord}} talks about the statitic fields of
  the student records.

}
\examples{

stats <- list(Physics_EAP=0,EnergyTransfer_EAP=.15,
              Physics_Margin=c(High=1/3,Medium=1/3,
                               Low=1/3))
stats2json(stats)

stats1 <- parseStats(ununboxer(unparseStats(stats)))
stopifnot(all.equal(stats,stats1,tolerance=.0002))

stats2json(stats,flatten=TRUE)

stats2 <- parseStats(ununboxer(unparseStats(stats,flatten=TRUE)))
stopifnot(all.equal(stats,stats2,tolerance=.0002))


}
\keyword{ interface }

