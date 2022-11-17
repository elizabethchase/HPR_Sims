bigstep: R( X <- as.matrix(sample(seq(0, 10, by = 0.03), size = n), ncol = 1);
            myfunc <- function(x){((x <= 2)*0 + (x > 2 & x <= 5)*6 + (x > 5 & x <= 6)*1 + (x > 6 & x <= 8)*3 + (x > 8)*10)};
            y <- rpois(n, lambda = myfunc(X[,1]));
            true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1])))
  n: 100
  $preds: X
  $outcome: y
  $truth: true_curve
  $func: myfunc

smooth: R(  X <- as.matrix(sample(seq(0, 10, by = 0.03), size = n), ncol = 1);
            myfunc <- function(x){abs(sin(x))*10};
            y <- rpois(n, lambda = myfunc(X[,1]));
            true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1])))
  n: 100
  $preds: X
  $outcome: y
  $truth: true_curve
  $func: myfunc
  
HPR_obs: R( library(HPR);
        mymodel <- hpr(y = y, X = X, family = "poisson");
        mycurvefits <- get_preds(mymodel, alpha = 0.05);
        mycompperf <- get_diagnostics(mymodel))
  X: $preds
  y: $outcome
  $curve_fit: mycurvefits
  $comp_perf: mycompperf
  
HPR_five: R( library(HPR);
        mymodel <- hpr(y = y, X = X, family = "poisson", X_aug = list(seq(0, 10, by = 0.5)));
        mycurvefits <- get_preds(mymodel, alpha = 0.05);
        mycompperf <- get_diagnostics(mymodel))
  X: $preds
  y: $outcome
  $curve_fit: mycurvefits
  $comp_perf: mycompperf
  
HPR_one: R( library(HPR);
        mymodel <- hpr(y = y, X = X, family = "poisson", X_aug = list(seq(0, 10, by = 0.1)));
        mycurvefits <- get_preds(mymodel, alpha = 0.05);
        mycompperf <- get_diagnostics(mymodel))
  X: $preds
  y: $outcome
  $curve_fit: mycurvefits
  $comp_perf: mycompperf
  
mad_obs: R( library(dplyr);
        mergedat <- left_join(y_true, yhat, by = c("x"));
        mergedat$mad <- abs(mergedat$Median - mergedat$truth);
        mean_abs_diff <- mean(mergedat$mad))
  yhat: $curve_fit
  y_true : $truth
  $metric: mean_abs_diff

mad_aug: R( library(dplyr);
        mergedat <- left_join(yhat, y_true, by = c("x"));
        subdat <- mergedat[is.na(mergedat$truth),];
        subdat$truth <- true_func(subdat$x);
        subdat$mad <- abs(subdat$Median - subdat$truth);
        mean_abs_diff <- mean(subdat$mad))
  yhat: $curve_fit
  true_func: $func
  y_true : $truth
  $metric: mean_abs_diff
  
width_obs: R( library(dplyr);
          mergedat <- left_join(y_true, yhat, by = c("x"));
          mergedat$width <- mergedat$Upper - mergedat$Lower;
          cred_width <- mean(mergedat$width))
  yhat: $curve_fit
  y_true : $truth
  $metric: cred_width
  
width_aug: R( library(dplyr);
        mergedat <- left_join(yhat, y_true, by = c("x"));
        subdat <- mergedat[is.na(mergedat$truth),];
        subdat$width <- subdat$Upper - subdat$Lower;
        cred_width <- mean(subdat$width))
  yhat: $curve_fit
  true_func: $func
  y_true : $truth
  $metric: cred_width

cover_obs: R( library(dplyr);
          mergedat <- left_join(y_true, yhat, by = c("x"));
          mergedat$cover <- as.numeric(mergedat$truth <= mergedat$Upper & mergedat$truth >= mergedat$Lower);
          cred_cover <- mean(mergedat$cover))
  yhat: $curve_fit
  y_true : $truth
  $metric: cred_cover
  
cover_aug: R( library(dplyr);
        mergedat <- left_join(yhat, y_true, by = c("x"));
        subdat <- mergedat[is.na(mergedat$truth),];
        subdat$truth <- true_func(subdat$x);
        subdat$cover <- as.numeric(subdat$truth <= subdat$Upper & subdat$truth >= subdat$Lower);
        cred_cover <- mean(subdat$cover))
  yhat: $curve_fit
  true_func: $func
  y_true : $truth
  $metric: cred_cover

div: R( mydiv <- comp_table$Divergences/comp_table$Num_Samples)
  comp_table: $comp_perf
  $metric: mydiv

time: R(  mytime <- comp_table$Time)
  comp_table: $comp_perf
  $metric: mytime
 
rhat: R(  myrhat <- comp_table$RHat/comp_table$Num_Param)
  comp_table: $comp_perf
  $metric: myrhat
  
min_samp_bulk: R( my_samp_bulk <- comp_table$Min_Ess_Bulk)
  comp_table: $comp_perf
  $metric: my_samp_bulk
  
min_samp_tail: R( my_samp_tail <- comp_table$Min_Ess_Tail)
  comp_table: $comp_perf
  $metric: my_samp_tail
  
treedepth: R( mytreedepth <- comp_table$Max_Treedepth/comp_table$Num_Samples)
  comp_table: $comp_perf
  $metric: mytreedepth
  
DSC:
  define:
    simulate: bigstep, smooth
    analyze: HPR_obs, HPR_five, HPR_one 
    score: mad_obs, mad_aug, width_obs, width_aug, cover_obs, cover_aug, div, time, rhat, min_samp_bulk, min_samp_tail, treedepth
  run: simulate * analyze * score