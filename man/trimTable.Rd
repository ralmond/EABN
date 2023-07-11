\name{trimTable}
\alias{trimTable}
\title{Trims empty columns from tables.}
\description{

  Downloaded spreadsheets sometimes contain empty columns at the end.
  This function removes all of the columns after the give column.
  
}
\usage{
trimTable(tab, lastcol = "Description")
}
\arguments{
  \item{tab}{A matrix, data frame or tibble to be trimmed).
}
  \item{lastcol}{
    The name of the last column to keep.  Any column to the right of
    this one will be discarded.}
}
\value{
  The first several columns of the table.
}
\author{Russell Almond}
\seealso{
  \code{\link[utils]{read.csv}}
}
\examples{

dat <- data.frame(One=1:3,Two=4:6,Three=7:9,10:12)
trimmed <- trimTable(dat,"Three")
stopifnot (ncol(trimmed)==3L)

}
\keyword{ manip }

