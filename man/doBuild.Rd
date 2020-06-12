\name{doBuild}
\alias{doBuild}
\title{Build or rebuild the Bayes nets for a scoring engine.}
\description{

  This function downloads the table specifications from the internet and
  rebuilds the Bayesian networks for a partilar scoring application.  It
  takes the information from the \dQuote{tables} subdirectory (under
  \code{config.dir} and builds the nets in the \dQuote{nets}
  subdirectory. 

}
\usage{
doBuild(sess, EA.tables, config.dir, override = FALSE)
}
\arguments{
  \item{sess}{A \code{\link[RNetica]{NeticaSession}} object used to
    build the Bayes nets.}
  \item{EA.tables}{A list containing configuration details.  See the
    \sQuote{Configuration} section below.}
  \item{config.dir}{The pathname of the directory that contains the
    tables and the nets subdirectories.}
  \item{override}{A logical flag.  If true, the code will ignore locks
    and rebuild the nets anyway.}
}
\details{
  
  This program applies the scripts from the \code{\link{Peanut-package}}
  to rebuild the nets.  It assumes the existance of five tables which
  describe the scoring model:
  \describe{
    \item{Nets.csv}{Manifest of all networks. See
      \code{\link[Peanut]{NetWarehouse}}.}
    \item{Nodes.csv}{Manifest of all nodes in all networks. See
      \code{\link[Peanut]{NodeWarehouse}}.}
    \item{Omega.csv}{Description of the competency model.  See
      \code{\link[Peanut]{Omega2Pnet}}.}
    \item{Q.csv}{Description of the evidence model.  See
      \code{\link[Peanut]{Qmat2Pnet}}.}
    \item{Statistics.csv}{A description of the statistics being used.  See
      \code{\link{configStats}}.}
  }

  These are expected to reside is the \dQuote{tables} subdirectory of
  the \code{config.dir} and have the names described above (although
  these details can be overriden by the configuration, see
  \sQuote{Configuration} below).

  The following steps are followed in the rebuilding.
  \enumerate{

    \item{The tables (CSV files) are downloaded from internet sources
      (see Downloading Tables below) into the \dQuote{tables} directory.}
    \item{The tables are loaded into R and a
      \code{\link[Peanut]{NetWarehouse}} and
      \code{\link[Peanut]{NodeWarehouse}} are built for the models.}
    \item{The \code{\link[Peanut]{Omega2Pnet}} script is run to build
      the proficiency model.}
    \item{The \code{\link[Peanut]{Qmat2Pnet}} script is run to build the
      evidence models.}
    \item{The nets are written out the \dQuote{nets} subdirectory of
      \code{config.dir}.  The net manifest is written to the
      subdirectory in the file \dQuote{NetManifest.csv} and the
      statistic list is written in the file
      \dQuote{StatisticList.csv}. These values can be overrided with the
      configuration.}
  }
}
\section{Configuration}{

  There are a large number of parameters which can be configured.  These
  are passed in through the \code{EA.tables} argument, which is a list
  of parameters.  The intention is that this can be read in from a JSON
  file (using \code{\link[jsonlite]{fromJSON}}).  In the current
  implementation, the \code{EA.tables} parameter set is a sub-object of
  the larger \code{EA.config} parameter set.

  The following fields are available:
  \describe{
    \item{netdir}{This is the name of the subdirectory of
      \code{config.dir} in which the constructed nets will be saved.
      Default value is \dQuote{nets}.}
    \item{tabdir}{This is the name of the subdirectory of
      \code{config.dir} in which the network specification tables are
      found.  The default value is \dQuote{tables}.}
    \item{TableID}{This is a parameter passed to the download script to
      identify the place from which the tables should be downloaded.
      The intent is for this to be a Google Sheets ID such as,
      \dQuote{16LcEuCspZjiBoZ3-Y1R3jxi1COXmh9vuTa9GwH1A_7Q}. }
    \item{downloadScript}{This is the name of the script which is run to
      download the tables.  The default value is \dQuote{download.sh}.
      See the Downloading Tables section below.}
    \item{NetsName}{This is the name (less the .csv extension) of the
      file containing the network manifest.  The default value is
      \dQuote{Nets}.} 
    \item{NodesName}{This is the name (less the .csv extension) of the
      file containing the node manifest. The default value is
      \dQuote{Nodes}.} 
    \item{OmegaName}{This is the name (less the .csv extension) of the
      file containing the Omega matrix (Proficiency model
      specification).  The default value is \dQuote{Omega}.}
    \item{QName}{This is the name (less the .csv extension) of the
      file containing the Q matrix (Evidence model
      specification). The default value is \dQuote{Q}.}
    \item{StatName}{This is the name (less the .csv extension) of the
      file containing the statistic list. The default value is
      \dQuote{Statistics}.} 
    \item{profModel}{This is the name of the proficiency model.  If no
      value is supplied, the value is inferred from the first
      non-missing value of the \dQuote{Hub} column in the network manifest.}
    \item{manifestFile}{The name of the  file in which the list of
      available networks is output.  The default value is
      \dQuote{PPManifest.csv}.} 
    \item{statFile}{The name of the file (in the \dQuote{nets}
      directory) in which statistics list is output.  The default value
      is \dQuote{StatisticList.csv}.} 
  }
}
\section{Downloading Tables}{

  The complete specification is given in five different tables.  This
  can be represented a five different sheets (pages) on a typical
  spreadsheet program.  In various projects it has been useful to create
  a Google Sheets document with these five pages which can be accessed
  by the project team.  Thus, one team member can make changes and the
  other download it.  (This would probably work with a different
  document collaboration system, but this has not been tested.)

  Google Sheets are identified by a long string in the URL.  This is the
  \dQuote{TableID} field in the \code{EA.tables} configuration list.
  (In theory, this could be replaced by an appropriate identifier if
  something other than Google Sheets was used.)  The script
  \dQuote{download.sh} (the name can be overriden in the configuration)
  is called using \code{\link[base]{system2}} with the \dQuote{table}
  directory path and the \dQuote{TableID} as arguments.  It then
  downloads the tables.

  The bash implementation for use with Google sheets is to first define
  a \code{BASEURL} variable:
  \code{BASEURL="https://docs.google.com/spreadsheets/d/$2"}, and then
  to call \code{curl} to download the sheets, e.g.,
  \code{curl "${BASEURL}/gviz/tq?tqx=out:csv&sheet={Nets}" >Nets.csv}.

  In theory, the sheets could be downloaded directly from the URLs using
  \code{read.csv}, however, there were issues with that solution.  This
  solution also allows the \code{download.sh} script to take care of any
  authentication which needs to be done (as the Google APIs here are a
  moving target).
}
\section{Locking}{

  It is probably a bad idea to rebuild the nets which a different
  incarnation is using the net directory to score.  It is almost
  certainly a bad idea for two different programs to rebuild the nets in
  the same directory at the same time.

  To prevent such clashes, the \code{\link{doRunrun}} function adds a
  file with the extension \code{.lock} to the directory when it is
  scoring.  The \code{doBuild} function adds the file
  \code{netbuilder.lock} while it is rebuilding the nets.

  If when \code{doBuild} starts, if a \code{.lock} file is found in the
  \dQuote{nets} directory, it issues an warning, and unless the
  \code{override} parameter is set to \code{TRUE} it stops.  Use the
  override only with extreme caution.

}
\section{Logging}{

  Logging is done through the \code{\link{futile.logger}{flog.logger}}
  mechanism. This allows logs to be save to a file.

}
\references{
  Almond, R. G. (2010). \sQuote{I can name that Bayesian network in two
  matrixes.} \emph{International Journal of Approximate Reasoning.}
  \bold{51}, 167-178.

  Almond, R. G. (presented 2017, August). Tabular views of Bayesian
  networks. In John-Mark Agosta and Tomas Singlair (Chair), \emph{Bayeisan
    Modeling Application Workshop 2017}. Symposium conducted at the
  meeting of Association for Uncertainty in Artificial Intelligence,
  Sydney, Australia. (International) Retrieved from \url{http://bmaw2017.azurewebsites.net/}

}
\value{

  This function is invoked for its side effects, which are stored in the
  \dQuote{nets} subdirectory of the \code{config.dir} directory.
  
}
\author{Russell Almond}
\seealso{

  \code{\link{doRunrun}}, \code{\link{configStats}}

  \code{\link[Peanut]{NetWarehouse}},
  \code{\link[Peanut]{NodeWarehouse}},
  \code{\link[Peanut]{Omega2Pnet}},
  \code{\link[Peanut]{Qmat2Pnet}},

}
\examples{

\dontrun{
## Set up config.dir, logpath and  NeticaLicenseKey
source("/usr/local/share/Proc4/EAini.R")

EA.config <- fromJSON(file.path(config.dir,"config.json"),FALSE)

EA.tables <- EA.config$Tables
EA.tables$netdir <- EA.config$netdir

sess <- RNetica::NeticaSession(LicenseKey=NeticaLicenseKey)
startSession(sess)

flog.appender(appender.file(file.path(logpath,
                            sub("<app>","builder",EA.config$logname))))
flog.threshold(EA.config$loglevel)

doBuild(sess,EA.tables,config.dir)
}
}
\keyword{ interface }
\keyword{ manip }
