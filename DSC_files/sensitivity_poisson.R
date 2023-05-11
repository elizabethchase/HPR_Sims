library(tidyverse);
library(HPR);

rm(list=ls())

array_id_offset = 0;
array_id <- array_id_offset + as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'));  
write_to_folder = "/home/ecchase/sensitivity_poisson/";
setwd("/home/ecchase")

set.seed(array_id)

n_list <- c(30, 100, 500)
param_list <- c(1, 2, 3)

fullcombs <- expand.grid(n_list, param_list)

rowid <- 1 + array_id%%nrow(fullcombs)

alphamean_list <- c(-99, 0, 10)
alphasd_list <- c(-99, 0.05, 50)
c_list <- c(0.01, 1, 0.0001)

alpha_mean <- alphamean_list[fullcombs$Var2[rowid]]
alpha_sd <- alphasd_list[fullcombs$Var2[rowid]]

if (alpha_mean==-99){
  alpha_mean <- NULL
}
if (alpha_sd==-99){
  alpha_sd <- NULL
}

c <- c_list[fullcombs$Var2[rowid]]
n <- fullcombs[rowid, 1]

#Generating data:
X <- as.matrix(seq(from = 0, to = 10, length = n), ncol = 1);
myfunc <- function(x){(x <= 2)*0 + (x > 2 & x <= 5)*6 + (x > 5 & x <= 6)*1 + (x > 6 & x <= 8)*3 + (x > 8)*10};
y <- rpois(n, lambda = myfunc(X[,1]));
true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1]))

mymodel <- hpr(y = y, X = X, family = "poisson", intercept_mean = alpha_mean);
mycurvefits <- get_preds(mymodel, alpha = 0.05)
mycurvefits$x <- X[,1]
mycurvefits$n <- n
mycurvefits$param <- paste0("alpha_mean = ", alpha_mean)
mycurvefits$truth <- true_curve$truth

mycompperf <- get_diagnostics(mymodel)
mycompperf$n <- n
mycompperf$param <- paste0("alpha_mean = ", alpha_mean)

finaldat <- mycurvefits
finalcomp <- mycompperf

mymodel <- hpr(y = y, X = X, family = "poisson", intercept_sd = alpha_sd);
mycurvefits <- get_preds(mymodel, alpha = 0.05)
mycurvefits$x <- X[,1]
mycurvefits$n <- n
mycurvefits$param <- paste0("alpha_sd = ", alpha_sd)
mycurvefits$truth <- true_curve$truth

mycompperf <- get_diagnostics(mymodel)
mycompperf$n <- n
mycompperf$param <- paste0("alpha_sd = ", alpha_sd)

finaldat <- rbind(finaldat, mycurvefits)
finalcomp <- rbind(finalcomp, mycompperf)

mymodel <- hpr(y = y, X = X, family = "poisson", c = c);
mycurvefits <- get_preds(mymodel, alpha = 0.05)
mycurvefits$x <- X[,1]
mycurvefits$n <- n
mycurvefits$param <- paste0("c = ", c)
mycurvefits$truth <- true_curve$truth

mycompperf <- get_diagnostics(mymodel)
mycompperf$n <- n
mycompperf$param <- paste0("c = ", c)

finaldat <- rbind(finaldat, mycurvefits)
finalcomp <- rbind(finalcomp, mycompperf)

save(list=c("finaldat", "finalcomp"), 
     file = paste0(write_to_folder, "hpr_dat", array_id, ".RData"))