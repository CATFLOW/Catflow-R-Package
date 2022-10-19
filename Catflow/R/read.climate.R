read.climate <-
function(file.nam = "./in/Klima/climate_04.dat", 
     GMT.off = -3600, 
     timzon = "GMT", 
     plotting = TRUE,
     ...)
{ opa <- par(no.readonly = TRUE)
  
  hdr <- readLines(file.nam, n = 3)[-1]
  clim <- read.table(file.nam, skip = 3,...)

  hdr <- strsplit(hdr, " ", fixed = TRUE)
  hdr[[1]] <- grep("[0-9]", hdr[[1]], value = TRUE) [1:2] # start date, and time conversion
   hdr[[2]] <- hdr[[2]][grep("[",hdr[[2]], fixed = TRUE)]
    hdr[[2]] <- strsplit(hdr[[2]]," -> ", fixed = TRUE)[[1]][1]
   
  colnames(clim) <- unlist(strsplit("Time,GlobRad,NetRad,Temp,RelHum,vWind,dirWind",","))

# timeseries object
 
  tim <- as.POSIXct(strptime( hdr[[1]][1], "%d.%m.%Y %H:%M:%S"), tz = timzon) + 
               GMT.off   + clim[,1] * as.numeric(hdr[[1]][2])
  clim <- zoo(clim[,-1], order.by = tim)
  if(plotting) plot (clim,...)
 
 par(opa) 
return(invisible(clim))
}

