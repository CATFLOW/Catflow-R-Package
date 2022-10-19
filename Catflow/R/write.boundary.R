write.boundary <-
function( timeser,
         irbtyp,                             # ID for type of boundary condition
         output.file,
         start.time = "01.01.2004 00:00:00", # dd.mm.yyyy hh:mm:ss,
         time.unit = "h"                     # d(ay), h(our), m(inute), s(econds)
     ) 
{ 
  # Knickdat currently only working for one column with boundary values
  # else: use timeser directly
  onecol <- TRUE
  if(is.zoo(timeser)){
      if(!is.null(dim(timeser))) onecol <- FALSE
    } else  if(ncol(timeser)>2)  onecol <- FALSE
    
 faktor.t <- switch(time.unit, "d" = 86400, "h" = 3600, "m" = 60, "s" = 1)
       # convert time.unit to seconds
 
 # header
  zeile1 <- paste(start.time, ".00", sep = "")
  zeile2 <- paste("# Startzeit   [", time.unit ,"] -> [s]", sep = "")        
 
 if(is.zoo(timeser)) { val <- coredata(timeser)
            tim <- as.numeric(index(timeser) - start(timeser))/faktor.t
            timeser <- cbind(tim, val)
  } else   val <- timeser[,-1]                 # first column is time stamp
        
# # Index
 if(onecol){
  #Differenz zum Vorgaenger
  diff1 = c( 1, abs(diff(val)) )

  #Differenz zum nachfolger
  diff2 <- c(abs(diff(val)), 1)

  #   points of interest
  poi <- which(diff1>0 | diff2>0)

 #gewaehlte Datenpunkte
 knickdat <- rbind(timeser[poi,1],timeser[poi,2])

 knickdat <- rbind(knickdat[1,], irbtyp, knickdat[2,])
 } else
 {  # "Knickdat" direkt für RB-Zeitreihenmit >1 Spalte, es wird einfach zeitreihe verwendet.
knickdat <- t(cbind(timeser[,1], irbtyp, timeser[,-1])  )
 }
 
 write( c(zeile1 , faktor.t), ncolumns = 2, output.file, sep = " ")
 write( zeile2, output.file, append = T)
 write(format(knickdat, width = c(8,8,4)), output.file, ncolumns = ncol(timeser)+1, sep = " ", append = T)
            
print(paste("Generated boundary condition file '", output.file, "'.",sep = "") )         

return(invisible())
}
