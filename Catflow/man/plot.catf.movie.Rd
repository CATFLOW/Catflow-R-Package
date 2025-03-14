\name{plot.catf.movie}
\alias{plot.catf.movie}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot CATFLOW results sequentially 
}
\description{
Plots the results of a CATFLOW simulation for a hillslope profile in chronological
sequence, either on the screen or to a pdf file.

}
\usage{
plot.catf.movie(resultmat, geof, 
                sel, begindate, outputPath, filename, 
                SCREENPLOT = FALSE, delayTime = 1, 
                pdf_width = 10, pdf_height = 7,
                lowercut = 0, uppercut = 1, lencut =  8,
                colorsForCuts,
                plotlab = TRUE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resultmat}{
List with a list of simulated values and a vector of simulation times,
e.g., read with \code{read.catf.resultmat}.
}
  \item{geof}{
Simulation geometry as a list with the components \code{hko} and \code{sko}, which
are the matrices of local vertical and horizontal coordinates, respectively. 
These are for example returned by \code{read.geofile}.
}
  \item{sel}{
Vector for selection of columns to plot (optional).
}
  \item{begindate}{
Start date given as a suitable date-time object, or date string in the format "\%d.\%m.\%Y \%H:\%M:\%S" for labelling the plots.
If missing, the simulation time is used as title.
}
  \item{SCREENPLOT}{
Logical if plotting should occur on the screen (default:FALSE), or to a pdf file.
}
  \item{delayTime}{
Delay time between subsequent plot when plotting on the screen.
}
  \item{outputPath}{
Path for output when plotting to a file (optional).
}
  \item{filename}{
Name for pdf file when plotting to a file; with or without extension.
}
 \item{ pdf_width}{
Width of pdf file when plotting to a file.
}
\item{ pdf_height}{
Height of pdf file when plotting to a file.
}
  \item{lowercut}{
Lower value for color coding values
}
  \item{uppercut}{
Upper value for color coding values
}
  \item{lencut}{
Length of the color coding (number of breaks)
}
  \item{colorsForCuts}{
Color palette for color coding (defaults to c("pink",rev(brewer.pal(length(cuts),"Blues"))))
}
  \item{plotlab}{
Logical indicating if a legend is to be plotted
}}
\details{
The function \code{plot.catf.movie} is used for its side-effect: generating a 
chronological sequence of plots to visualise CATFLOW results.
The plots are either displayed on the screen (if \code{SCREENPLOT = TRUE}), or saved
as a pdf file.


If \code{begindate} is missing, the simulation time in seconds is used in the title.

If \code{filename} is missing, the simulation results are plotted on the screen only.
If \code{outputPath} is missing, the current directory is used.
 
}
\value{
No value is returned.
}

\author{
Jan \enc{Wienhöfer}{Wienhoefer}
}

\seealso{
\code{\link{read.geofile}}, \code{\link{read.catf.resultmat}},
\code{\link{node2poly}}
}

\examples{
 % # JW  file.path(.libPaths(), "Catflow", "Catflow-TEST", "in", "test.geo")
 #geometry                                                  
  geof <- read.geofile(system.file("Catflow-TEST/in/test.geo", package = "Catflow"))
 #relative saturation 
  res <- read.catf.resultmat(system.file("Catflow-TEST/out/relsat.out", package = "Catflow") )   
 
 \dontrun{
 # plot on screen  
  plot.catf.movie(res, geof,  sel =12:21, 
                  SCREENPLOT = TRUE,
                  lencut=5,
                  colorsForCuts = c("pink",brewer.pal(8,"Blues")))   
   

 # plot to file  
  tempwd <- tempdir()
 
  plot.catf.movie(res, geof, 
  filename = "test.pdf",
  outputPath = tempwd,                                
  lencut=10,
  colorsForCuts = c("pink",brewer.pal(8,"Blues")))   
  
   # open pdf
   shell.exec(file.path(tempwd, "test.pdf"))
  }  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities}
