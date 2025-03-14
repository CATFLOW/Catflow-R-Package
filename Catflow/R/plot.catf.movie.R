plot.catf.movie <-
function( resultmat,               # values and  simulation times -> read.catf.resultmat()
          geof,                    # geometry
          sel ,                    # selection of columns
          begindate ,              # "%d.%m.%Y %H:%M:%S", Time object, or NA / NULL
          outputPath ,             # When plotting to file ( SCREENPLOT = FALSE), 
          filename  ,              # you can set path and name of pdf file
          SCREENPLOT = FALSE,       # Plot on screen, or files (TRUE/FALSE)
          delayTime = 1,         # Delay time in sec. (for slower/faster plotting)
          pdf_width=10,
          pdf_height=7,

          lowercut = 0,            # color cuts: lower, upper, length
          uppercut = 1,
          lencut =  8,
     colorsForCuts,                # palette
     plotlab = TRUE                # plot.legend
          )
##########

  ####
{
old.par <- par(no.readonly = TRUE) # all par settings which
if(SCREENPLOT)  on.exit(par(old.par))

if(missing(begindate)) begindate <- NULL    # some default date

# make output directory
if(missing(outputPath)) outputPath <- "."   # current directory
if(!SCREENPLOT) if(!file_test("-d", outputPath)) dir.create(outputPath)


if(is.null(begindate) ) {  useTime <- TRUE
 } else  { useTime <- FALSE
           if(timeBased(begindate)) { begindate <- as.POSIXct(begindate) 
           } else {                     begindate <- as.POSIXct(strptime(begindate, "%d.%m.%Y %H:%M:%S")) 
           }
          }

if(missing(filename)) {
        if(!SCREENPLOT) warning("No filename specified! -> Plotting on screen")
        SCREENPLOT <- TRUE
        }

# one single pdf
if(!SCREENPLOT) { outfile <- paste(outputPath, "/", filename, ".pdf", sep = "")
                          outfile <-  sub("pdf.pdf", "pdf", outfile)
                  pdf(file = outfile, width = pdf_width, height = pdf_height)                               
                  par(oma = old.par$oma)
}


 #Set the colors. One more color then cuts is needed!
 cuts <-  seq(from = lowercut, to = uppercut, length.out = lencut)
  if(missing(colorsForCuts)) colorsForCuts <- c("pink",rev(brewer.pal(length(cuts),"Blues")))

      ### find significant number of digits to round off  # no longer used
      # cuts <- unique(round(cuts,digits =  nchar( gsub("0\\.","", lowercut) )))

 ###

val <- resultmat[[1]]
simtim <- resultmat[[2]]

numbersteps <- length(simtim)

ynum <- nrow(geof$hko)
xnum <- ncol(geof$hko)

### polygon representation of nodes
if(missing(sel)) { 
    geom <- node2poly(xv = geof$sko, yv = geof$hko)
} else  { # select columns ('bereich')
    bereich <- sel
    geom <- node2poly(xv = geof$sko, yv = geof$hko, sel = bereich)
    val <- lapply(val, function(x) x[, bereich] )
  }


## loop over timesteps    
for (j in 1:numbersteps) {

  if(useTime) { pic.ID <- paste(simtim[[j]], "s") 
   } else       pic.ID <- format(begindate + simtim[[j]] , "%Y-%m-%d %H:00")
 
 # # draw geometry and color coded values -for all nodes
#    # empty plot
   plot(range(geom[[1]], na.rm = TRUE), range(geom[[2]], na.rm = TRUE),
        typ = "n", main = pic.ID, xlab = "hor [m]", ylab = "vert [m]")
  # plot borders of slope geometry (-> geof)
     lines(geof$sko[1, ], geof$hko[1, ], t = "l", lwd = 2, col = 8)
     lines(geof$sko[ynum, ], geof$hko[ynum, ], t = "l", lwd = 2, col = 8)
     lines(geof$sko[, 1], geof$hko[, 1], t = "l", lwd = 2, col = 8)
     lines(geof$sko[, xnum], geof$hko[, xnum], t = "l", lwd = 2, col = 8)

  #if (SCREENPLOT)  #bringToTop()
  

 ### Color.codes: ## color coding - scale values in color space - specific palette

   colors2 <- colorsForCuts[findInterval(val[[j]],cuts, rightmost.closed = TRUE)+1]

  polygon(geom[[1]], geom[[2]], col = colors2, border = NA)

   if(plotlab)
   {
    legendtext <- c( paste("<",cuts[1]),
         paste(cuts[-length(cuts)],"-",cuts[-1],sep = " "),
          paste(">",cuts[length(cuts)]) )

    # check if an outer margin is specified at the right - if yes, plot legend there
     if(old.par$oma[4]>0)
        { par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
          plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
        }
    legend("topright", legendtext, pch = 22, pt.bg = colorsForCuts, bty = "n", cex = 0.9, inset = c(0.05, 0.15))
    }
    par(old.par)

   if (SCREENPLOT) Sys.sleep(delayTime)

} # end of loop over timesteps

if (! SCREENPLOT)  {dev.off()
                    print(paste("Generated", outfile))}

return(invisible(NULL))  
}

