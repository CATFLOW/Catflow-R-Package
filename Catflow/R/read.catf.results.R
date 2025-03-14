read.catf.results <-
function(resdir,
     result.files.mat = c("psi.out", "relsat.out", "theta.out", 
     "fl_xsi.out", "fl_eta.out", "senken.out"),
     balance.file , 
     surfrun.file ,
     evapo.file ,
     target.dir ) 
{
 if(missing(target.dir)) {saving <- FALSE
    } else saving <- TRUE

# generate filenames to read from resdir argument
files2read <- as.list(file.path(resdir, result.files.mat ))


# read all files and return list
res <- lapply(files2read, function(x) { erg <- read.catf.resultmat(x)
                    gc()
                    return(erg)}
                    ) 
 names(res) <- result.files.mat
 
 ## check if single or multiple hillslopes
  laenge <- lapply(res, length)
 
if( all(laenge == 2))  # single slope
{  # extract time - check if equal in all files
   simtim <- sapply(res, function(x) x[["time"]])
 
   if(ncol(simtim)>1)
   {
   ind <- combn(1:ncol(simtim),2, simplify = FALSE) 
   ind <- sapply(ind, function(x, y = simtim) all.equal(y[,x[1]], y[,x[2]]))
   
   stopifnot("Time steps not equal in out.files!" = all(ind))   # stops if not unique time vector in all files
   }
   
   # keep only values, not time in results list
   for(i in 1:length(res)) res[[i]] <- res[[i]][[1]]
  
   # add time vector
   res$time <- simtim[,1]

 } else {  # several slopes
    simtim <- lapply(res, function(x) sapply(x, function(y) y$time))
     # check if all are equal within each variable (equality between hillslopes)
     same_timestamp <- sapply(simtim, function(x) all(apply(x,1 , function(row) all(row == row[1]))))
     
    # check for equality between variables
    if(all(same_timestamp)) {
       all_timestamps <- sapply(simtim, function(x) x[,1])
       all_timestamps_same <- apply(all_timestamps, 1 , function(row) all(row == row[1]))

       }
 
   # one common time vector
    if( all(same_timestamp) & all(all_timestamps_same) ){   simtim <- simtim[[1]][,1]
     } else { stop(paste("Several slopes, but Time steps not equal in out.files!"))
     }
  
   res <- lapply(res, function(x) lapply(x, function(y) y[[1]]))
   res$time <- simtim
  }

# balance file
if(!missing(balance.file)) {
  res$balance <- read.catf.balance( file.path(resdir,balance.file)) }

# surface runoff
if(!missing(surfrun.file)) {
  res$surf.runoff <- read.surfrun.out( file.path(resdir,surfrun.file)) }
  
if(!missing(evapo.file)) {
  res$evapo <- read.evapo.out( file.path(resdir,evapo.file)) }
  

 # save results
 if(saving) 
 { filnam <- file.path(target.dir, paste(rev(unlist(strsplit(resdir,"/")))[1],"obj",sep = ".") )
  save(res, file = filnam) }
  
 gc()
 
return(res) 
}

