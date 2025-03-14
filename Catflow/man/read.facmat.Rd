\name{read.facmat}
\alias{read.facmat}

\title{
Read matrix file for CATFLOW
}
\description{
Reads a file for a node-wise specification of values for a CATFLOW simulation. 
Examples of values are multipliers to scale saturated hydraulic conductivity, 
or initial conditions in terms of matric potential or soil moisture.
}
\usage{
read.facmat(input.file)
}

\arguments{
  \item{input.file}{
Name of file to read
}
}
\details{
The function 'read.facmat' is used for reading a file in CATFLOW-specific format. 
}
\value{
A matrix of values from the input file.
}

\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}

\seealso{
\code{\link{write.facmat}}
}
\examples{
  # Example discretization: eta (vertical), xsi (lateral)
   eta <- 1:6
    xsi <- 1:11
   
  # Multipliers for some exponential decrease of saturated hydraulic conductivity with depth
   exp.dec <- exp(seq(0, -3, length=length(eta) ))
    exp.dec <- rep(round(exp.dec,3), length(xsi) ) 
  
  # Write temporary ks multiplier file
  kstemp <- tempfile()
  write.facmat(output.file=kstemp, fac=exp.dec)
  
  file.show(kstemp)
  
  # read in again for comparison
  read.facmat(kstemp)
 
}

\keyword{ utilities}
