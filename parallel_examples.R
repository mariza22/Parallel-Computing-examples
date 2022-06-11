#################### 1st example ###########################

wait.then.square           <- function(xx) Sys.sleep(5)
system.time( lapply(1:2, wait.then.square))                                                # serial computing 

## Define example function and the global variable 
system.time(serial.output <- lapply(1:2,function(xx) {return( wait.then.square(xx) )}) )

library(parallel)

start_time                <- Sys.time()
cl                        <- makeCluster( detectCores())                                   # we use all the available cores 
par.setup                 <- parLapply( cl, 1:length(cl),function(xx) {c(library(MASS))} ) # all the libraries we use

clusterExport(cl,"wait.then.square")
par.output                <- parLapply(cl,1:2,function(xx) return(wait.then.square(xx)) )  # all the functions we use
stopCluster(cl)
Sys.time() - start_time

################## 2nd example  #######################

# functions that we use

wait.then.square  <- function(xx){
  Sys.sleep(1);
  xx^2 } 
                                       
(a.global.variable <- Diagonal(3) )

require(parallel)
library(parallel)
require(Matrix)
library(Matrix)

## Step 1: Create a cluster of child processes 
cl                        <- makeCluster( 4 )

## Step 2: Load the necessary R package(s)
par.setup                 <- parLapply( cl, 1:length(cl), function(xx) {require(Matrix)})

## Step 3: Distribute the necessary R objects 
clusterExport( cl, c('wait.then.square', 'a.global.variable') )

## Step 4: Do the computation
system.time(par.output   <- parLapply(cl, 1:4, function(xx) { return( wait.then.square(xx) + a.global.variable)}))

## Step 5: Remember to stop the cluster!
stopCluster(cl)
#################### 3rd example ###########
########### Application to my code ########

########### Parallel Computing ##########################
########### for all the athletes ######## 

library(parallel)

total_athl            <- length(sort(unique(data_performance$ID)))              # 100m
cl                    <- makeCluster(3) # detectCores() - 1 
par.setup             <- parLapply( cl, 1:length(cl),function(xx) {c(library(MASS))} )

clusterExport( cl, c( "bf_ind_L", "log_llh_ind_L", "sample_ind", "sample_ind", "compute_Delta", "compute_LA", #all the functions that are used throughout the code
                      "extract_llh", "generate_temperature", "logit_e", "inv_logit_e", "update_temp_in_LA", "update_LA", "ASI",
                      "make_data","alg_par","make_hyper_par","EM","data_global", "data_performance","test"))#"bf_g_L","log_llh_g_L",
start_time2           <- Sys.time()

athletes              <- parLapply(cl, 1:total_athl, function( ID = xx, EM = EM1, data_global1 = data_global, data_ID=data_performance$ID, data_Perf = data_performance$Mark,
                                                               splines = test$datamat) { ASI(ID, EM, data_global1, data_ID, data_Perf, splines)} )
stopCluster(cl)
Sys.time()- start_time2
                                       
