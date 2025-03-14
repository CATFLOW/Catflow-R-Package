read.climate <-
function(file.nam, 
     GMT.off = -3600, 
     timzon = "GMT", 
     plotting = TRUE,
     ...)
{ opa <- par(no.readonly = TRUE)
  
  hdr <- readLines(file.nam, n = 3)[-1]
  clim <- read.table(file.nam, skip = 3,...)

  hdr <- strsplit(hdr, " ", fixed = TRUE)
  hdr[[1]] <- grep("[0-9]", hdr[[1]], value = TRUE) [1:3] # start date, start time, and time conversion factor
   hdr[[2]] <- hdr[[2]][grep("[",hdr[[2]], fixed = TRUE)]
    hdr[[2]] <- strsplit(hdr[[2]]," -> ", fixed = TRUE)[[1]][1]
   
  colnames(clim) <- unlist(strsplit("Time,GlobRad,NetRad,Temp,RelHum,vWind,dirWind",","))

# timeseries object
 
  tim <- as.POSIXct(strptime( paste( hdr[[1]][1],hdr[[1]][2]), "%d.%m.%Y %H:%M:%S"), tz = timzon) + 
               GMT.off   + clim[,1] * as.numeric(hdr[[1]][3])
  clim <- zoo(clim[,-1], order.by = tim)
  if(plotting) plot (clim,...)
 
 par(opa) 
return(invisible(clim))
}

