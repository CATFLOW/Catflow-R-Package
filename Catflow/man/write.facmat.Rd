\name{write.facmat}
\alias{write.facmat}

\title{
Write matrix file for CATFLOW
}
\description{
Writes a file for a node-wise specification of values for a CATFLOW simulation. 
Examples of values are multipliers to scale saturated hydraulic conductivity, 
or initial conditions in terms of matric potential or soil moisture.
}
\usage{
write.facmat <- function(output.file, eta, xsi, header = NULL, numh = 1, fac = 1)
}

\arguments{
  \item{output.file}{
Name of file to produce (if missing, no file is generated)
}
  \item{eta}{
Vertical discretization of slope profile (eta vector)
}
  \item{xsi}{
Lateral discretization of slope profile (xsi vector)
}
  \item{numh}{
ID of hillslope (integer)
}
  \item{headr}{
Header/First line in output file. Defaults to "paste(-1000-numh, length(eta), length(xsi))".
}
  \item{fac}{
Value that is to be assigned to all nodes. Either a single number, or a matrix of dimensions dim(fac) = c(length(et), length(xs)). 
}
}
\details{
The function 'write.facmat' is used for its side-effect: writing a file in CATFLOW-specific format. 
}
\value{
A matrix with the specified value 'fac' is returned invisibly.
}
\references{
Maurer, T. and E. Zehe (2007) \emph{CATFLOW User Guide and Program Documentation (Version Catstat)}
}
\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}

\seealso{
\code{\link{read.facmat}}
}
\examples{
\dontrun{
  # Example discretization: eta (vertical), xsi (lateral)
  eta <- 1:6
  xsi <- 1:11
 
  # Temp dir and files
  tempwd <- tempdir()
  ksfile <- file.path(tempwd, "ksmult.dat")
  inifile <- file.path(tempwd, "soilhyd.ini")
   
  # Dummy multipliers for scaling saturated hydraulic conductivity: all one
  write.facmat(output.file = ksfile, eta, xsi)
  file.show(ksfile)
      
  # Initial conditions: Uniform Psi (soilhyd.ini)
  write.facmat(output.file="soilhyd.ini",
                eta=eta, xsi=xsi, numh=1, 
                headr=paste("PSI   ", 0,  1, length(eta), length(xsi), 1),
                fac = 0.8)

  file.show("soilhyd.ini")
    
  # Multipliers for some exponential decrease of saturated hydraulic conductivity with depth
  exp.dec <- exp(seq(0, -3, length=length(eta) ))
   exp.dec <- rep(round(exp.dec,3), length(xsi) ) 
  
  write.facmat(output.file=ksfile, 
               eta, xsi,
               fac=exp.dec)
  file.show(ksfile)
   }
}

\keyword{ utilities }