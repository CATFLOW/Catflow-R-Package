plot.catf.bal <-
function(bilanz, interact = TRUE, ylim, stay = F, ...)
{  
 if(is.character(bilanz)) bilanz <- read.catf.balance(bilanz, plottin = FALSE, 
                     differences = FALSE) 
 
 if(! is.data.frame(bilanz)) stop("'plot.catf.bal': Either a data frame or a file name of a balance file must be given!")
 
 
 Nied <- bilanz[, 14]
 Auffeuchtung <- bilanz[, 5]
 Senken <- bilanz[, 6]
 Rechtrand <- bilanz[, 9]
 Unterrand <- bilanz[, 10]
 Linkrand <- bilanz[, 11]
 Ofa <- bilanz[, 12]
 BodenVerd <- bilanz[, 17]
 Interzep <- bilanz[, 16]
 Transp <- bilanz[, 18]
 # Totale <- Nied-Auffeuchtung+Senken+Unterrand+Rechtrand-Ofa-BodenVerd-Interzep 
 Totale <- Nied -Auffeuchtung -Transp +Unterrand +Rechtrand + Linkrand -Ofa -BodenVerd -Interzep 
 if(missing(ylim)) ylim = range(c(bilanz[, c(5, 12, 14, 16, 17)], 
                  -bilanz[, c(9, 10, 11, 6)], Totale))

  opa <- par(oma = c(0,0,0,9.5) )
  
  plot(bilanz[, 3]/86400, Nied, typ = "l", , ylab = "[m3]", xlab = "Time [d]", ylim = ylim, ...)
 lines(bilanz[, 3]/86400, Ofa,  col = "blue")
 lines(bilanz[, 3]/86400, -Rechtrand,  col = "darkblue", lty = 2)
 lines(bilanz[, 3]/86400, -Unterrand,  col = "brown")
 lines(bilanz[, 3]/86400, -Linkrand, col = "grey" , lty = 2)
 lines(bilanz[, 3]/86400, Auffeuchtung,  col = "lightblue")
 lines(bilanz[, 3]/86400, BodenVerd,  col = "green")
 lines(bilanz[, 3]/86400, Transp, col = "darkgreen" , lty = 3)
 
 lines(bilanz[, 3]/86400, Totale,  col = "red", lty = 2) 
 lines(bilanz[,3]/86400, bilanz[,4], col = 6)
 
 par(opa)
 
 opa <- par(fig = c(0, 1, 0, 1), oma = c(0,0,0,1), mar=c(0,0,0,0), new =TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  
 legend("right", c("Precipitation", "Surface runoff", "Right bound. flux",
        "Left bound. flux",
         "Lower bound. flux", "Soil moist.", "Soil evap.", "Transpiration", 
         "Total bal.","Internal bal."), 
   title ="Legend",
   col = c(1, "blue", "darkblue", "grey", "brown", "lightblue", "green", "darkgreen", "red", 6), 
    lty = c(1, 1, 2, 2, rep(1, 3), 3, 2, 1), bty = "o"    
    )    # , ...

 par(opa)   
    
 if(interact && .Platform$OS.type == "windows") bringToTop(stay = stay)
 
return(invisible(data.frame(time = bilanz[, 3], total = Totale) ) ) 
}
 
