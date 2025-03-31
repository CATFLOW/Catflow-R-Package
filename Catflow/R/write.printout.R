write.printout <-
   function(output.file = "printout.prt",
            start.time = "01.01.2004 00:00:00", # dd.mm.yyyy hh:mm:ss, or Time object
            end.time, # = "01.01.2005 00:00:00",
            length.prt,
            intervall,
            time.unit = "h", # d(ay), h(our), m(inute), s(econds)
            flag = 1, # All nodes or Surface nodes: 1 | 0
            first.time = 0) # 1: dump all; 0:dump for surface nodes
   {
      #
      if (missing(end.time) & missing(intervall) & missing(length.prt)) {
         stop("Error in write.printout() - check time sequence arguments!")
      }

      if (!missing(end.time) & !missing(intervall) & !missing(length.prt)) {
         warning("write.printout() - end.time is not considered, because length and interval are set")
      }


      if (inherits(start.time, c("POSIXct", "Date"))) { # check if time object and convert to string
         start.time <- strftime(start.time, "%d.%m.%Y %H:%M:%S")
      }

      if (inherits(end.time, c("POSIXct", "Date"))) { # check if time object and convert to string
         end.time <- strftime(end.time, "%d.%m.%Y %H:%M:%S")
      }

      # header
      faktor <- switch(time.unit,
         "d" = 86400,
         "h" = 3600,
         "m" = 60,
         "s" = 1
      )

      zeile1a <- paste(start.time, ".00", sep = "")
      zeile2 <- paste("# Startzeit   [", time.unit, "] -> [s]", sep = "")
      zeile3 <- "# 	1: dump all; 0:dump for surface nodes"

      if (missing(intervall)) {
         laenge <- as.numeric(difftime(strptime(end.time, "%d.%m.%Y %H:%M:%S"),
            strptime(start.time, "%d.%m.%Y %H:%M:%S"),
            units = "secs"
         ))
         intervall <- round(laenge / (length.prt - 1) / faktor, 1)
      }

      if (missing(length.prt)) {
         laenge <- as.numeric(difftime(strptime(end.time, "%d.%m.%Y %H:%M:%S"),
            strptime(start.time, "%d.%m.%Y %H:%M:%S"),
            units = "secs"
         ))
         zeitvec <- seq(first.time, laenge / faktor + 1, by = intervall)
      } else {
         zeitvec <- seq(first.time, by = intervall, length.out = length.prt)
      }

      ###
      # Verknuepfen von Zeitvektor und Schalter fuer Ausgabe aller Knoten / an Oberflaeche
      ausgabe <- rbind(round(zeitvec, 2), flag)


      # in Datei schreiben
      write(c(zeile1a, faktor), ncolumns = 2, output.file, sep = "\t")
      write(c(zeile2, zeile3), output.file, append = T)
      write(ausgabe, output.file, ncolumns = 2, sep = "\t", append = T)

      print(paste("Generated printout times file '", basename(output.file), "'.", sep = ""))

      return(invisible())
   }
