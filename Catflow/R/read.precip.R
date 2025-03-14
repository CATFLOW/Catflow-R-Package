read.precip <-
function( filename, 
   GMT.off = -3600, 
   timzon = "GMT", 
   plotting = TRUE)
{ 
 hdr <- readLines(filename, n = 2)
 prec <- read.table(filename, skip = 2)
 
  hdr <- strsplit(hdr, " ", fixed = TRUE)
  # ectract numeric values from header
   ## --> start date, start time, time conversion factor, rain rate conversion factor
  hdr[[1]] <- grep("[0-9]", hdr[[1]], value = TRUE) [1:4] 
 
  # Extract textual info 
  orig.units <-  hdr[[2]][grep("[",hdr[[2]], fixed = TRUE)]        # 1: orig time, 2: seconds, 3 orig rate, 4 m/s
  
 
# 
# hdr <- strsplit(hdr, "[[:space:]]{2,}")
# hdr[[2]] <- hdr[[2]][grep("[",hdr[[2]], fixed = TRUE)]
# hdr[[2]] <- sapply(strsplit(hdr[[2]]," -> ", fixed = TRUE), function(x) x[1])
# 
 colnames(prec) <- orig.units[c(1,3)] 
 
 # timeseries object
                                        
 tim <- as.POSIXct(strptime( paste( hdr[[1]][1],hdr[[1]][2]), "%d.%m.%Y %H:%M:%S"), tz = timzon) + 
          GMT.off  + prec[,1] * as.numeric(hdr[[1]][3]) 
 prec <- zoo(prec[,2], order.by = tim)
   if(plotting){
     plot(prec, t = "s", 
         main = strsplit(filename, "/")[[1]][
           length(strsplit(filename, "/")[[1]])],
             xlab = "", ylab = paste("Precipitation",  orig.units[3])) 
     }
  
return(prec)
}

