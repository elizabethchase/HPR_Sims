bigstep: R( X <- as.matrix(seq(from = 0, to = 10, length = n), ncol = 1);
            myfunc <- function(x){(x <= 2)*0 + (x > 2 & x <= 5)*6 + (x > 5 & x <= 6)*1 + (x > 6 & x <= 8)*3 + (x > 8)*10};
            y <- rbinom(n, prob = myfunc(X[,1])/10, size = 1);
            true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1])/10))
  n: 150
  $preds: X
  $outcome: y
  $truth: true_curve
  
smooth: R(  X <- as.matrix(seq(from = 0, to = 10, length = n), ncol = 1);
            myfunc <- function(x){abs(sin(x))};
            y <- rbinom(n, prob = myfunc(X[,1]), size = 1);
            true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1])))
  n: 150
  $preds: X
  $outcome: y
  $truth: true_curve
  
joinpoint: R( X <- as.matrix(seq(from = 0, to = 10, length = n), ncol = 1);
              myfunc <- function(x){(1.5*x)*(x < 2) + (16 - 5*x)*(x >= 2 & x < 3) + (x >= 3 & x < 6) + (10 - x)*(x >= 6 & x < 9) + (-44 + 5*x)*(x >= 9)};
              y <- rbinom(n, prob = myfunc(X[,1])/6, size = 1);
              true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1])/6))
  n: 150
  $preds: X
  $outcome: y
  $truth: true_curve
  
lineflat: R( X <- as.matrix(seq(from = 0, to = 10, length = n), ncol = 1);
            myfunc <- function(x){(x <= 3)*x + (x > 3)*3};
            y <- rbinom(n, prob = myfunc(X[,1])/3, size = 1);
            true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1])/3))
  n: 150
  $preds: X
  $outcome: y
  $truth: true_curve

impulse: R( X <- as.matrix(seq(from = 0, to = 10, length = n), ncol = 1);
            myfunc <- function(x){(x > 0 & x < 3)*exp(-x) + (x == 3)*1 + (x > 3 & x < 7)*exp(-(x-3)) + (x == 7)*1 + (x > 7)*exp(-(x-7))};
            y <- rbinom(n, prob = myfunc(X[,1]), size = 1);
            true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1])))
  n: 150
  $preds: X
  $outcome: y
  $truth: true_curve

HPR: R( library(HPR);
        mymodel <- hpr(y = y, X = X, family = "binomial");
        mycurvefits <- get_preds(mymodel, alpha = 0.05);
        mycurvefits$x <- X[,1];
        mycompperf <- get_diagnostics(mymodel))
  X: $preds
  y: $outcome
  $curve_fit: mycurvefits
  $comp_perf: mycompperf
  
GPR: R( library(mgcv);
        start.time <- Sys.time();
        gp_lin <- gam(y ~ s(X[,1], bs = "gp"), family = "binomial");
        gp_fit_lin <- predict(gp_lin, newdata = data.frame("x"= X[,1]), type = "link", se.fit = TRUE);
        end.time <- Sys.time();
        time.taken <- as.numeric(difftime(end.time, start.time, units="secs"));
        mycurvefits <- data.frame("Median" = plogis(gp_fit_lin$fit), "Lower" = plogis(gp_fit_lin$fit - 1.96*gp_fit_lin$se.fit), "Upper" = plogis(gp_fit_lin$fit + 1.96*gp_fit_lin$se.fit), "x" = X[,1]);
        mycompperf <- data.frame("Time" = time.taken, "Divergences" = NA, "Max_Treedepth" = NA, "RHat" = NA, "Min_Ess_Bulk" = NA, "Min_Ess_Tail" = NA, "Num_Param" = NA, "Num_Samples" = NA))
  X: $preds
  y: $outcome
  $curve_fit: mycurvefits
  $comp_perf: mycompperf
  
Adspline: R( library(mgcv);
        start.time <- Sys.time();
        gp_lin <- gam(y ~ s(X[,1], bs = "ad"), family = "binomial");
        gp_fit_lin <- predict(gp_lin, newdata = data.frame("x"= X[,1]), type = "link", se.fit = TRUE);
        end.time <- Sys.time();
        time.taken <- as.numeric(difftime(end.time, start.time, units="secs"));
        mycurvefits <- data.frame("Median" = plogis(gp_fit_lin$fit), "Lower" = plogis(gp_fit_lin$fit - 1.96*gp_fit_lin$se.fit), "Upper" = plogis(gp_fit_lin$fit + 1.96*gp_fit_lin$se.fit), "x" = X[,1]);
        mycompperf <- data.frame("Time" = time.taken, "Divergences" = NA, "Max_Treedepth" = NA, "RHat" = NA, "Min_Ess_Bulk" = NA, "Min_Ess_Tail" = NA, "Num_Param" = NA, "Num_Samples" = NA))
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
  
pointwise_bias: R( library(dplyr);
        mergedat <- left_join(yhat, y_true, by = c("x"));
        mergedat$bias <- mergedat$Median - mergedat$truth;
        pw_bias <- select(mergedat, x, bias))
  yhat: $curve_fit
  y_true : $truth
  $metric: pw_bias
  
pointwise_width: R( library(dplyr);
        mergedat <- left_join(yhat, y_true, by = c("x"));
        mergedat$width <- mergedat$Upper - mergedat$Lower;
        pw_width <- select(mergedat, x, width))
  yhat: $curve_fit
  y_true : $truth
  $metric: pw_width
  
pointwise_cover: R( library(dplyr);
          mergedat <- left_join(yhat, y_true, by = c("x"));
          mergedat$cover <- as.numeric(mergedat$truth <= mergedat$Upper & mergedat$truth >= mergedat$Lower);
          pw_cover <- select(mergedat, x, cover))
  yhat: $curve_fit
  y_true : $truth
  $metric: pw_cover
  
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
    simulate: bigstep, smooth, joinpoint, lineflat, impulse
    analyze: HPR, GPR, Adspline
    score: mad, width, cover, pointwise_bias, pointwise_width, pointwise_cover, div, time, rhat, min_samp_bulk, min_samp_tail, treedepth
  run: simulate * analyze * score