#' Plot the geometry of a catflow object
#' 
#' @param geom A catflow geometry object as returned by \code{\link{make.geometry}}
#' @param zooming Logical. If TRUE, the plot is interactive and zooming is enabled. If FALSE, the plot is static.
#' @param ... Additional arguments passed to \code{\link{matplot}} and \code{\link{matlines}}
#' @return NULL 
#' @export
#' @examples
#' \dontrun{        
#'  library(Catflow)
#'  geom <- make.geometry(make.output = FALSE)     
#'  plot.catf.geometry(geom) 
#' }
#' @importFrom graphics matplot matlines
#' @importFrom grDEvices x11 dev.interactive
#' @export 



plot.catf.geometry <- function(geom,  zooming = TRUE, ...) {

  x <- geom$sko
  y <- geom$hko
 
  # When using RStudio/VS Code or other IDEs with own graphics devices,
  # plot functions with zoom do not work properly
  # workaround: open new interactive window         
  
  #-------------------------------------------------------------------------------
  # Plot function to call with zoom  
      plofun <- function(plottitle , ...) {
    matplot(x, y, type = "l", pch = "", main = plottitle, ...)
    matlines(t(x), t(y), type = "l", pch = "",...)
}
    
 ## -----------------------------------------------------------------------
 ## Plot the geometry
 ## -----------------------------------------------------------------------
  if(interactive() && zooming) {
   if (!dev.interactive()) dev.new()
   try(zoom(plofun, plottitle = "Model geometry (zoom enabled)", asp = 1,...))
  } else {
    plofun(plottitle ="Model geometry (zoom disabled)",...)
  }
  # -----------------------------------------------------------------------
}
