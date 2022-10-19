read.precip <-
function( file.nam = "./in/Klima/P_DB_knick_04.dat", 
   GMT.off = -3600, 
   timzon = "GMT", 
   plotting = TRUE)
{ 
 hdr <- readLines(file.nam, n = 2)
 prec <- read.table(file.nam, skip = 2)
 
 hdr <- strsplit(hdr, "[[:space:]]{2,}")
 hdr[[2]] <- hdr[[2]][grep("[",hdr[[2]], fixed = TRUE)]
 hdr[[2]] <- sapply(strsplit(hdr[[2]]," -> ", fixed = TRUE), function(x) x[1])
 
 colnames(prec) <- hdr[[2]]
 
 # timeseries object
 
 tim <- as.POSIXct(strptime( hdr[[1]][1], "%d.%m.%Y %H:%M:%S"), tz = timzon) + 
          GMT.off  + prec[,1] * as.numeric(hdr[[1]][2]) 
 prec <- zoo(prec[,2], order.by = tim)
   if(plotting){
     plot(prec, t = "s", 
         main = strsplit(file.nam, "/")[[1]][
           length(strsplit(file.nam, "/")[[1]])],
             xlab = "", ylab = paste("Precipitation", hdr[[2]][2])) 
     }
  
return(prec)
}

