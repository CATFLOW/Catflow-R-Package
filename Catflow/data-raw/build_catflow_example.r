### R code from vignette source 'catflow.Rnw'
### Encoding: ISO8859-1



###################################################
### code chunk number 2: prelim
###################################################

require(splines)
require(deSolve)
require(zoo)


# library("Catflow")                                    ## Henne-Ei :)
invisible(sapply(list.files("D:/R_work/CATF/Catflow-R-Package/Catflow/R", full.names=T), source, echo=F))

setwd("D:/R_work/CATF/Catflow-R-Package")

exdir <- file.path(".","Catflow", "inst", "Catflow-TEST")

if(!file_test("-d", exdir )) dir.create(exdir)


###################################################
### code chunk number 3: slopelineDecl
###################################################
# northing of slope line 
  north <- seq(1,11, length=20)  
# easting of slope line
  east  <- seq(2, 8, length=20)
# elevation of at points of slope line
  elev  <- approx(c(8,5),n=20)$y + sin((0:19)/2)/5 
# width of slope at points of slope line (here: uniform) 
  slopewidth <- rep(1,20)  


####################################################
#### code chunk number 4: slopelineFig
####################################################
    #opa <- par(mfrow=c(2,1), mar=c(4,4,1,1), lend=1)
    #plot(north, east, t="b", xlab="Northing", ylab="Easting", 
    #      ylim=c(1,9), xlim=c(0.5,11.5))           
    #lines(north,east, col="yellow", lwd=30)
    #lines(north,east, t="b")     
    #legend("bottomr", c("slope line","slope area"), pch=c(21,NA), lty=1, lwd=c(1,30), col=c(1,"yellow"), bty="n")
    # legend("topl", "Map", lty=0, pch=NA, bty="n")
    #
    #plot(sqrt((north-1)^2 + (east-2)^2), elev, ylab="Elevation [m]",
    #xlab="Distance along slope line [m]", t="b",         
    #ylim=c(4,9), xlim=c(-.5,12.5) )
    # legend("bottomleft", "Profile", lty=0, pch=NA, bty="n")
    #par(opa)


###################################################
### code chunk number 5: slopelistDecl
###################################################
simple.slope <- list( xh = north,
                      yh = east,
                      zh = elev,
                      bh = slopewidth,
                      tot.area = 12 ,
                      htyp = 1, 
                      dyy = 2,
                      xsi = seq(0,1,length = 21),
                      eta = seq(0,1,length = 11),
                      out.file="test.geo"      )                    


###################################################
### code chunk number 6: makeGeom
###################################################
  
  test.geom <- make.geometry(simple.slope, plotting=FALSE, project.path = file.path(exdir, "in") )


###################################################
### code chunk number 7: plotGeom
###################################################
opa <- par(mar=c(4,4,1,1))
with(test.geom, matplot(sko,hko, t="l", pch="", ylab="Elevation [m]",
xlab="Distance along slope line [m]"))
with(test.geom, matlines(t(sko),t(hko), t="l", pch=""))
par(opa)


###################################################
### code chunk number 8: facmat
###################################################
  attach(test.geom)   # attach the geometry to make 'eta' and 'xsi' available
  write.facmat(output.file=file.path(exdir, "in" , "ksmult.dat"))
  write.facmat(output.file=file.path(exdir, "in" , "thsmult.dat"))


###################################################
### code chunk number 9: soilID
###################################################
# Initial conditions: Uniform Psi (0.8 m) 
  write.facmat(output.file=file.path(exdir, "in" , "soilhyd.ini"),
               headr=paste("PSI   ", 0,  1, length(eta), length(xsi), 1),
               fac = 0.8)
# Soil type IDs: 
   write.facmat(output.file=file.path(exdir, "in" , "soils.bod"),
               headr= paste("BODEN",  length(eta), length(xsi), 1),
               fac = matrix(c(rep(1, ceiling(length(eta)/2)),rep(2,floor(length(eta)/2)) ), 
                            nrow = length(eta), ncol = length(xsi))  ) 


###################################################
### code chunk number 10: rain
###################################################
# some artificial rainfall record
  raindat <- data.frame("hours" = seq(0,48, by=0.5),
                        "precip" = c(rep(0,30), 1, rep(3,4), rep(2,3), 
                                      rep(0,25), rep(1,4), rep(0,30)) ) 
 plot(raindat, t="s", ylab="Precip. [mm/h]")
  write.precip(raindat, file.path(exdir, "in" , "TEST.rain.dat"), 
                start.time= "01.01.2004 00:00:00" )


###################################################
### code chunk number 11: clima
###################################################
 climadat <- data.frame(
              "hours" = seq(0,48, by=0.5),
              "GlobRad" =  ifelse(0 + 800 * sin((seq(0,48, by=0.5) - 8)*pi/12) > 0,
                                  0 + 800 * sin((seq(0,48, by=0.5) - 8)*pi/12),  0),
              "NetRad" = NA ,
              "Temp" = 4 +  sin((seq(0,48, by=0.5) - 12)*pi/12)  ,
              "RelHum" = 70 + 10* sin((seq(0,48, by=0.5))*pi/12) ,
              "vWind"  =  rlnorm(97, 0,1) ,
              "dirWind" = runif(97, 0, 359) 
              )
 write.climate(climadat, file.path(exdir, "in" , "TEST.clima.dat"), 
                  start.time= "01.01.2004 00:00:00" )


###################################################
### code chunk number 12: printout
###################################################
 write.printout(output.file = file.path(exdir, "in" , "printout.prt"), 
                 start.time = "01.01.2004 00:00:00", 
                 end.time = "03.01.2004 00:00:00", 
                 intervall = 0.5, time.unit = "h",
                 flag = 1)


###################################################
### code chunk number 13: surfPob
###################################################
  write.surface.pob(output.file = file.path(exdir, "in" , "surface.pob"), 
                    xs = xsi, lu = 33, 
                    windid = rep(1,4))  


###################################################
### code chunk number 14: otherFiles
###################################################
# macro.file = "profil.mak"
 cat(paste("1  0  2", "ari", "0.00 1.00 0.00 1.00 1  1.00 1.00 ", sep="\n"), 
   file = file.path(exdir, "in" , "profil.mak") )
# cv.file = "cont_vol.cv" 
 cat(paste("1", "0.8   0.9   0.98   1.0",sep="\n"), 
   file = file.path(exdir, "in" , "cont_vol.cv")  )
# bc.file = "boundary.rb"
 cat(paste("L", "1  0", "0. 1. 0",   " ",
           "R", "1  0", "0. 1. -10", " ",
           "T", "1  0", "0. 1. -99 "," ",  
           "B", "1  0", "0. 1. 0",   " ",
           "S", "1  0", "0. 1. 0. 1. -99",   " ",
           "M", 0 , sep="\n"),
   file = file.path(exdir, "in" , "boundary.rb")  )  

###################################################
### code chunk number 15: control
###################################################                                     
 write.control(output.file = "TEST.example.in",project.path = exdir,    
 start.date = "01.01.2004 00:00:00.00", end.date = "03.01.2004 00:00:00",
 slope.in.list = list( slope1 = list( geo.file = "test.geo", soil.file = "soils.bod", 
                                 ks.fac = "ksmult.dat", ths.fac = "thsmult.dat", 
                                 macro.file = "profil.mak", cv.file = "cont_vol.cv", 
                                 ini.file = "soilhyd.ini",print.file = "printout.prt", 
                                 surf.file = "surface.pob", bc.file = "boundary.rb")) )


###################################################
### code chunk number 16: mainFile
###################################################
 write.CATFLOW.IN("TEST.example.in", project.path = exdir)



###################################################
### code chunk number 17: globalIn
###################################################

###################################################
### code chunk number 18: windsec
###################################################
 cat(paste("4", "240   0.81", " 50   0.78", " 80   0.97", "220   0.94", sep="\n"), 
  file = file.path(exdir, "in" , "winddir.def") 
  )
  
###################################################
### code chunk number 19: soiltype
###################################################
 cat(paste("2", "1 Loamy Sand, porosity 0.55, bulk dens 1 g/cm3",
           "1  800  1. 1.  1e-4   0.5 0.34 0.11  20. 0.70  0.050   1. 1. 1.",           
           "4.05e-5  0.55 0.06 12.40 2.28 -6.00 8.00 1000.00 0.80",
           "0. 0. 0.", "0. 0. 0.", "0. 0. 0.", 
           "2 Sandy Clay Loam (30% S, 40 % U; 30 % T)",
           "1  800  1. 1.  1e-4   0.5 0.34 0.11  20. 0.70  0.050   1. 1. 1.",
           "3.42e-6 0.48 0.08 0.96 1.5 -6.00 8.00 1200.00 0.80",
           "0. 0. 0.", "0. 0. 0.", "0. 0. 0.", sep="\n") ,
  file = file.path(exdir, "in" , "soils.def")
    )       
  
###################################################
### code chunk number 20: timeser
###################################################
 cat(paste("PREC", "1", "in/TEST.rain.dat", "",
           "BC", "0", "", "SINKS","0", "", "SOLUTE", "0", "",
           "LAND-USE", "in/landuse/lu_ts.dat", "", 
           "CLIMATE", "1", "in/TEST.clima.dat", "", sep="\n"),
  file = file.path(exdir, "in" , "timeser.def")      
    )

###################################################
### code chunk number 21: landuse
###################################################
 if(!file_test("-d",file.path(exdir, "in" , "landuse"))) dir.create(file.path(exdir, "in" , "landuse"))
# pointer to land-use parameters
 cat(paste("3", "coniferous forest", "in/landuse/conif.par", sep ="             "),
   file = file.path(exdir, "in" , "landuse", "lu_file.def")
   ) 
# time-series of land-use parameters
 cat(paste("01.01.2004 00:00:00.00", "in/landuse/lu_set1.dat", 
            "01.01.2005 00:00:00.00", sep="\n"), 
  file = file.path(exdir, "in" , "landuse", "lu_ts.dat")
  )       
# parameters of land-use type 'coniferous forest'
 cat(paste(
      paste("10", "KST", "MAK", "BFI", "BBG", "TWU", "PFH", 
            "PALB", "RSTMIN", "WP_BFW", "F_BFW", sep= "   "),
      "0.    3.     1.    5.    0.95   5.0    5.0     0.15    1.    1.      1.",
      paste(c("1  ","366"), 
            "2.     1.     1.     1.0   1.0    1.0     1.0   546.    0.05    30.",
       sep="    ", collapse="\n"), sep="\n"), 
  file = file.path(exdir, "in" , "landuse", "conif.par")
   )
# pointer to surface node attributes
 cat(paste(1, "33  3    %coniferous forest", sep = "\n"),
  file = file.path(exdir, "in" , "landuse", "lu_set1.dat")
  )


###################################################
### code chunk number 22: CATFLOW
###################################################



### copy CATFLOW exe
file.copy("d:/R_work/CATF/Catflow-R-Package/Catflow/data-raw/CATFLOW.exe",
          exdir)
          
## Run CATFLOW
  owd <- setwd(exdir)
   system("CATFLOW")   

  # Delete unneccessary resultfiles
  del.files(direct=".")
  file.remove(c("angles.dat","angles_2.dat","boden.dat"))



#.. TIDY
  detach(test.geom)
  setwd(owd)        
