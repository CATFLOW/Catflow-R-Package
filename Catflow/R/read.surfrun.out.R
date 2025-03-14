read.surfrun.out <-           # JW  check output across versions, check conversion
function(filename,
         GMT.off = -3600, 
         timzon = "GMT", 
         plotting = TRUE)
  {
  start.date <- read.table(filename, nrows = 1)  # currently unused
  
  hdr <- read.table(filename, nrows = 1, skip = 1)
  
  qo <- read.table(filename, skip = 2)
 
  # old version: 5 columns, new version: six columns
 
 if(ncol(qo) == 6)
 { names(qo) <- c("ihg", "time", "time+dt","Neff", "q", "cum.q")
   qo <- qo[, c("time", "time+dt","Neff", "q", "cum.q","ihg" )]
  } else {
   names(qo) <- c("ihg", "time", "Neff", "q", "cum.q")
   qo <- qo[, c("time", "Neff", "q", "cum.q","ihg" )]
  }

  # convert with given factors: all columns but last (slope ID)
  qconv <- qo 
  for( i in 1:(ncol(qo)-1) ) qconv[,i] <- hdr[[i]] * qo[,i] 


 ## convert to zoo
 timevec <- as.POSIXct(strptime( paste( start.date[1], start.date[2]), "%d.%m.%Y %H:%M:%S"), 
                       tz = timzon) +  GMT.off  + qo$time 
 
 qo <- zoo(qo, timevec)
  qconv <- zoo(qconv, timevec)
                       
 # Plotting (optional)
  if(plotting){
     plot(qo[,c("q","cum.q","Neff")], type = "s", 
         main = strsplit(filename, "/")[[1]][
           length(strsplit(filename, "/")[[1]])]) 
     }
 
  
  return(list("orig" = qo, "converted" = qconv)) 
}

