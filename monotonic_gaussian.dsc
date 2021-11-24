bigstep: R( X <- as.matrix(seq(from = 0, to = 10, length = n), ncol = 1);
            myfunc <- function(x){(x <= 2)*0 + (x > 2 & x <= 5)*6 + (x > 5 & x <= 6)*10 + (x > 6 & x <= 8)*12 + (x > 8)*20};
            y <- rnorm(n, mean = myfunc(X[,1]), sd = sd);
            true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1])))
  n: 75
  sd: 1
  $preds: X
  $outcome: y
  $truth: true_curve

smooth: R(  X <- as.matrix(seq(from = 0, to = 10, length = n), ncol = 1);
            myfunc <- function(x){qlogis((x/11) + 0.01)};
            y <- rnorm(n, mean = myfunc(X[,1]), sd = sd);
            true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1])))
  n: 75
  sd: 0.5
  $preds: X
  $outcome: y
  $truth: true_curve
  
HPR: R( library(HPR);
        mymodel <- hpr(y = y, X = X, family = "gaussian");
        mycurvefits <- get_f(mymodel, alpha = 0.05);
        mycompperf <- get_diagnostics(mymodel))
  X: $preds
  y: $outcome
  $curve_fit: mycurvefits
  $comp_perf: mycompperf
  
HPR_exp: R( library(HPR);
        mymodel <- hpr(y = y, X = X, family = "gaussian", monotonic_terms = 1, monotonic_approach = "exp");
        mycurvefits <- get_f(mymodel, alpha = 0.05);
        mycompperf <- get_diagnostics(mymodel))
  X: $preds
  y: $outcome
  $curve_fit: mycurvefits
  $comp_perf: mycompperf
  
HPR_abs: R( library(HPR);
        mymodel <- hpr(y = y, X = X, family = "gaussian", monotonic_terms = 1, monotonic_approach = "abs");
        mycurvefits <- get_f(mymodel, alpha = 0.05);
        mycompperf <- get_diagnostics(mymodel))
  X: $preds
  y: $outcome
  $curve_fit: mycurvefits
  $comp_perf: mycompperf
  
mad: R( library(dplyr);
        mergedat <- left_join(yhat, y_true, by = c("x"));
        mergedat$mad <- abs(mergedat$Median - mergedat$truth);
        mean_abs_diff <- mean(mergedat$mad))
  yhat: $curve_fit
  y_true : $truth
  $metric: mean_abs_diff

width: R( library(dplyr);
          mergedat <- left_join(yhat, y_true, by = c("x"));
          mergedat$width <- mergedat$Upper - mergedat$Lower;
          cred_width <- mean(mergedat$width))
  yhat: $curve_fit
  y_true : $truth
  $metric: cred_width

cover: R( library(dplyr);
          mergedat <- left_join(yhat, y_true, by = c("x"));
          mergedat$cover <- as.numeric(mergedat$truth <= mergedat$Upper & mergedat$truth >= mergedat$Lower);
          cred_cover <- mean(mergedat$cover))
  yhat: $curve_fit
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
    analyze: HPR, HPR_exp, HPR_abs
    score: mad, width, cover, div, time, rhat, min_samp_bulk, min_samp_tail, treedepth
  run: simulate * analyze * score
  