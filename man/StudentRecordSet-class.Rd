\name{StudentRecordSet-class}
\Rdversion{1.1}
\docType{class}
\alias{StudentRecordSet-class}
\alias{app,StudentRecordSet-method}
\alias{getSR,StudentRecordSet-method}
\alias{newSR,StudentRecordSet-method}
\alias{saveSR,StudentRecordSet-method}
\alias{clearSRs,StudentRecordSet-method}

\title{Class \code{"StudentRecordSet"}}
\description{

This class provides a collection of student records.  Optionally, it can
be hitched to a database so that student can be saved and restored
across scoring sessions.

}
\section{Extends}{

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. 

}
\details{

  The \code{StudentRecordSet} exists to hold a collection of
  \code{\linkS4class{StudentRecord}} objects.  If, when constructed, the
  record set is passed information about a database, the record set is
  stored in the database.  If not, it is merely stored in memory.  The
  database version, in particular, allows restoring the object from
  memory.  The primary key for the student record in the database is the
  \code{app} ID (which is a field in the record set) and the \code{uid}
  which is passed through the \code{\link{getSR}} method.

  The method \code{\link{getSR}} takes different arguments based on
  which version is passed.  In particular, the \code{ser} argument
  allows a serialized (JSON) version of the data to be passed in.
  In particular, \code{\link{getSR}} will do one of the following
  things (in order of priority):
  \enumerate{
    \item{If the \code{ser} argument is supplied, the student record
      will be restored from this.}
    \item{If the \code{StudentRecordSet} is connected to a database,
      then the student record is restored from information in the
      database, based on the \code{uid} argument and the \code{app}
      field.}
    \item{A new student record is created for the \code{uid}.}
  }

  The record set also contains a link to a \code{\link[Peanut]{PnetWarehouse}}
  which it uses to try and find the \code{\link[Peanut]{Pnet}}
  associated with the \code{\linkS4class{StudentRecord}}.  If the
  \code{Pnet} already exists in the warehouse, it is just connected to
  the fetched record.  If not, then it is restored from a serialized
  version either from the passed in serialized record, or from the
  serialized \code{Pnet} in the database.

}
\section{Methods}{
  \describe{
    \item{app}{\code{signature(x = "StudentRecordSet")}: Returns the
      application this record set is associated with. }
    \item{getSR}{\code{signature(srs = "StudentRecordSet",
        uid="ANY",ser="character")}: Returns the student record for the
      specified ID. If \code{ser} is supplied it should be a json list
      object containing the student record.} 
    \item{newSR}{\code{signature(srs = "StudentRecordSet",
        uid="character")}: Creates a new Student Record for the
      specified ID by cloning the default student record. }
    \item{saveSR}{\code{signature(srs = "StudentRecordSet")}: If
      connected to a database, the SR is saved to the database. }
    \item{clearSR}{\code{signature(srs = "StudentRecordSet")}: If
      connected to a database, the SR in the database are cleared. }
  }
}
\author{Russell Almond}

\seealso{

  \code{\link{StudentRecordSet}} for the constructor.
  \code{\linkS4class{StudentRecord}} for the contained objects.

  \code{\link[Peanut]{PnetWarehouse}} and \code{\link[Peanut]{Pnet}} for
  information about the contained Bayesian networks.
  
  \code{\linkS4class{BNEngine}} for the engine that holds it.
}
\examples{
showClass("StudentRecordSet")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{app}:}{Object of class \code{character} which contains
      the application identifier }
    \item{\code{dbname}:}{Object of class \code{character} which
      contains the name of the database.}
    \item{\code{dburi}:}{Object of class \code{character} containing the
      URI for connecting to the database, for example
      \dQuote{mongodb://localhost:271017}. To create a record set not
      connecteded to the database, set this value to \code{character()}.}
    \item{\code{db}:}{Object of class \code{MongoDB} a connection to the
      database or \code{NULL} if the object is not connected to the
      database.  Users should call the \code{recorddb()} function rather
      than access this field directly.}
    \item{\code{warehouse}:}{Object of class
      \code{\link[Peanut]{PnetWarehouse}} which contains already loaded
      nets. } 
    \item{\code{defaultSR}:}{Object of class
      \code{\linkS4class{StudentRecord}} or \code{NULL}.  This is the
      default student record which is cloned to create new studnet records. }
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{initialize(app, dbname, dburi, db, warehouse,
        ...)}:}{Initializes the student record set. }
    \item{\code{recorddb()}:}{Returns the database handle (if connected
      to a database) or \code{NULL} if not connected to a database.
      Note that this initializes the database the first time it is
      called, so it should be called rather than accessing the \code{db}
      field directly.}
    \item{\code{clearAll(clearDefault=FALSE)}:}{Clears all records from
      the database and the warehouse.  If \code{clearDefault==FALSE},
      then the default record is not cleared. }
  }
}
