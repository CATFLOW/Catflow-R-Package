\name{write.CATFLOW.IN}
\alias{write.CATFLOW.IN}

\title{
Write main input file for CATFLOW
}
\description{
Writes the main input file 'CATFLOW.IN' which specifies the control files to be 
proceeded by CATFLOW.
}
\usage{
write.CATFLOW.IN(control.files, project.path = NULL, flag = 2 )
}

\arguments{
  \item{control.files}{
Name(s) of the control file(s) to be included in the project control file
}
  \item{project.path}{
Path to the direcory for the project control file, if not to be produced in the current directory
}
 \item{flag}{
Global Manning roughness for simulation (default is 2)
}
}
\details{
The project control file produced holds each control file name, followed by the \code{flag}
number, separated by 20 space characters.
}
\value{
No value is returned. A short message indicates success.
}

\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}

\seealso{
\code{\link{write.control}}
}
\examples{
  \dontrun{
  # some names of potential control files (just examples)
  control.files <- c("test.1strun.in","test.2ndrun.in")
  
  #  create a main input file with these control files in a temp dir
  tempwd <- tempdir()
  write.CATFLOW.IN(control.files, project.path=tempwd)
  
  # display the file
  file.show(file.path(tempwd, "CATFLOW.IN"))
 
  }
}


\keyword{ utilities}
