plot.catf.bal <-
function(bilanz, ylim, unit = "m3", on.top = FALSE, stay = FALSE, ...)
{  
 if(is.character(bilanz)) bilanz <- read.catf.balance(bilanz, plottin = FALSE, 
                     differences = FALSE) 
 
 if(! is.data.frame(bilanz)) stop("'plot.catf.bal': Either a data frame or a file name of a balance file must be given!")
 
 Time <- bilanz[, 3]
 Internbil <- bilanz[, 4]                  ## == - Oberrand + Auffeuchtung - Senken - Unterrand - Rechtrand   #  positive when  Out > In 
 Auffeuchtung <- bilanz[, 5]
 Senken <- bilanz[, 6]
 Bilrand <- bilanz[, 7]
 Oberrand <- bilanz[, 8]
 Rechtrand <- bilanz[, 9]
 Unterrand <- bilanz[, 10]
 Linkrand <- bilanz[, 11]
 Ofa <- bilanz[, 12]
 Nied <- bilanz[, 14]
 Nied_mm <- bilanz[, 15]
 Interzep <- bilanz[, 16]
 BodenVerd <- bilanz[, 17]
 Transp <- bilanz[, 18]

#.. Calculations

 surfbal <- - Nied +( Oberrand + Ofa + Interzep)        # positive when  Out>  In 
  
  ## OLD logic, albeit now with correct components
  # Totale <- -Nied + Auffeuchtung  - (Senken +Unterrand +Rechtrand + Linkrand) +Ofa +Interzep 
  # --> equals surfbal + Internbil

 # calculate Surface area from Precipitation data 
 area <-  Nied/(Nied_mm/1000)                     

 if(unit =="mm")
  {  unitfac <- 1000/area[length(Nied)]
 } else unitfac <- 1

  
 #.. Plotting

 if(missing(ylim)) ylim = range(c(Nied, Auffeuchtung, Ofa, Interzep, BodenVerd, surfbal, Internbil), 
                  -c(Rechtrand, Unterrand, Linkrand, Senken) )*unitfac

  opa <- par(oma = c(0,0,0,9.5) )
  
 plot(Time/86400, Nied*unitfac, typ = "l", , ylab = unit, xlab = "Time [d]", ylim = ylim, ...)
 lines(Time/86400, Ofa*unitfac,  col = "blue")
 lines(Time/86400, -Rechtrand*unitfac,  col = "darkblue", lty = 2)
 lines(Time/86400, -Unterrand*unitfac,  col = "brown")
  # lines(Time/86400, -Linkrand*unitfac, col = "grey" , lty = 2)
 lines(Time/86400, Auffeuchtung*unitfac,  col = "lightblue")
 lines(Time/86400, Interzep*unitfac,  col = "green")
 lines(Time/86400, -Senken*unitfac, col = "darkgreen" , lty = 3)
 
 lines(Time/86400, surfbal*unitfac,  col = "red", lty = 2)
 lines(bilanz[,3]/86400, Internbil*unitfac, col = 6)
 
 par(opa)
 
 opa <- par(fig = c(0, 1, 0, 1), oma = c(0,0,0,1), mar=c(0,0,0,0), new =TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  
 legend("right", c("Precipitation", "Surface runoff", "Right bound. flux",
        "Left bound. flux",
         "Lower bound. flux", "Soil moist.", "Interception", "Evapotranspiration", 
         "Surface bal.","Internal bal."),
   title ="Legend",
   col = c(1, "blue", "darkblue", "grey", "brown", "lightblue", "green", "darkgreen", "red", 6), 
    lty = c(1, 1, 2, 2, rep(1, 3), 3, 2, 1), bty = "o"    
    )    # , ...

 par(opa)   
    
 if(on.top && .Platform$OS.type == "windows") bringToTop(stay = stay)
 
return(invisible(data.frame(time = Time, "Surf.bal" = surfbal, "Totale" =  surfbal + Internbil) ) ) 
}
 
