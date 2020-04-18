\name{stat}
\alias{stat}
\alias{stats}
\alias{statNames}
\title{Access statistics from a Student Record}
\description{

  These functions access the \code{stats} field of a
  \code{\linkS4class{StudentRecord}} object.  The function \code{stat}
  accesses a single statistics and \code{stats} returns all of the
  statistics.  The function \code{statNames} returns the names of the
  available statistics.

}
\usage{
stat(sr, name)
stats(x)
statNames(sr)
}
\arguments{
  \item{sr, x}{A \code{\linkS4class{StudentRecord}} object whose
    statsitics are to be accessed.}
  \item{name}{A character object giving the name of the specific
    statististic to access.}
}
\value{

  The function \code{stat} returns the value of a single statistic,
  which could be numeric, character or something else.

  The function \code{stats} returns a named list of statistics.

  The function \code{statNames} returns a character vector.

}
\author{Russell Almond}
\seealso{

  \code{\linkS4class{StudentRecord}} for the student record class.

  \code{\link[Peanut]{Statistic}} for statistic objects which return the
  statistics.  

}
\examples{

stats <- list(Physics_EAP=0,EnergyTransfer_EAP=.15,
              Physics_Margin=c(High=1/3,Medium=1/3,
                               Low=1/3))

dsr <- StudentRecord("*DEFAULT*",app="ecd://epls.coe.fsu.edu/P4Test",
                     context="*Baseline*",stats=stats)

stats(dsr)
stopifnot(all.equal(stats,stats,tolerance=.0002))

statNames(dsr)
stopifnot(all(statNames(dsr)==names(stats)))

stat(dsr,"Physics_Margin")
stopifnot(all.equal(stat(dsr,"Physics_Margin"),stats[[3]],tolerance=.0002))




}
\keyword{ manip }

