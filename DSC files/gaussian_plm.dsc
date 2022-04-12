presim: R( library(dplyr);
            lin_cov <- chol(diag(x= 1-lin_rho, nrow = p_lin) + lin_rho);
            nonlin_cov <- chol(diag(x= 1-nonlin_rho, nrow = p_nonlin) + nonlin_rho);
            lin_pred <- matrix(c(rnorm(n, mean = 67, sd = 7), rnorm(n, mean = 0, sd = 1), rnorm(n, mean = 130, sd = 20)), nrow = n)%*%lin_cov;
            nonlin_pred <- matrix(c(rnorm(n, mean = 45, sd = 12), rnorm(n, mean = 80, sd = 20)), nrow = n)%*%nonlin_cov;
            mydata <- data.frame("Height" = lin_pred[,1], "Sex" = as.numeric(lin_pred[,2] > 24), "Weight" = lin_pred[,3], "Age" = nonlin_pred[,1], "Income" = nonlin_pred[,2]))
  n: 100
  lin_rho: 0.35
  nonlin_rho: 0.2
  p_lin: 3
  p_nonlin: 2
  $my_data: mydata

bigstep: R( library(dplyr);
            myfunc <- function(x){(x < 35)*0 + (x >= 35 & x < 55)*5 + (x >= 55 & x < 65)*6 + (x >= 65 & x < 80)*8 + (x >= 80)*10};
            mydata$Visits_noint <- (mydata$Weight)*beta_vec[1] + mydata$Income*beta_vec[2] + mydata$Sex*beta_vec[3] + mydata$Height*beta_vec[4] + myfunc(mydata$Age);
            truey <- mydata$Visits_noint;
            y <- rnorm(n, mean = mydata$Visits_noint, sd = sd);
            X <- matrix(mydata$Age, ncol = 1);
            Z <- dplyr::select(mydata, Weight, Income, Sex, Height);
            true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1]));
            beta_table <- data.frame("Variable" = c("Weight", "Income", "Sex", "Height"), "beta" = beta_vec))
  n: 100
  sd: 0.5
  mydata: $my_data
  beta_vec: c(0.05, 0.1, 5, 0)
  $preds: X
  $outcome: y
  $lin_preds: Z
  $truth: true_curve
  $true_betas: beta_table
  $true_y: truey

smooth: R( library(dplyr);
            myfunc <- function(x){(x^2)/100};
            mydata$Visits_noint <- (mydata$Weight)*beta_vec[1] + mydata$Income*beta_vec[2] + mydata$Sex*beta_vec[3] + mydata$Height*beta_vec[4] + myfunc(mydata$Age);
            truey <- mydata$Visits_noint;
            y <- rnorm(n, mean = mydata$Visits_noint, sd = sd);
            X <- matrix(mydata$Age, ncol = 1);
            Z_df <- dplyr::select(mydata, Weight, Income, Sex, Height);
            true_curve <- data.frame("x" = X[,1], "truth" = myfunc(X[,1]));
            beta_table <- data.frame("Variable" = c("Weight", "Income", "Sex", "Height"), "beta" = beta_vec))
  n: 100
  sd: 0.5
  mydata: $my_data
  beta_vec: c(0.05, 0.1, 5, 0)
  $preds: X
  $outcome: y
  $lin_preds: Z_df
  $truth: true_curve
  $true_betas: beta_table
  $true_y: truey
  
HPR: R( library(HPR);
        mymodel <- hpr(y = y, X = X, Z = as.matrix(Z), family = "gaussian");
        mybetafits <- get_betas(mymodel, alpha = 0.05, var_names = c("Weight", "Income", "Sex", "Height"));
        mycurvefits <- get_f(mymodel, alpha = 0.05);
        mypredfits <- get_preds(mymodel, alpha = 0.05);
        mycompperf <- get_diagnostics(mymodel))
  X: $preds
  y: $outcome
  Z: $lin_preds
  $curve_fit: mycurvefits
  $beta_fit: mybetafits
  $pred_fit: mypredfits
  $comp_perf: mycompperf
  
GPR: R( library(mgcv);
        newdata <- Z;
        newdata$y <- y;
        newdata$x <- X[,1];
        start.time <- Sys.time();
        gp_lin <- gam(y ~ s(x, bs = "gp") + Weight + Income + Sex + Height, family = "gaussian", data = newdata);
        gp_fit_ys <- predict(gp_lin, newdata = newdata, type = "link", se.fit = TRUE);
        gp_fit_fs <- predict(gp_lin, newdata = newdata, type = "terms", se.fit = TRUE);
        end.time <- Sys.time();
        time.taken <- as.numeric(difftime(end.time, start.time, units="secs"));
        mycurvefits <- data.frame("Median" = gp_fit_fs$fit[,"s(x)"], "Lower" = gp_fit_fs$fit[,"s(x)"] - 1.96*gp_fit_fs$se.fit[,"s(x)"], "Upper" = gp_fit_fs$fit[,"s(x)"] + 1.96*gp_fit_fs$se.fit[,"s(x)"], "x" = X[,1]);
        mybetafits <- data.frame("Median" = summary(gp_lin)$p.coeff[-1], "Lower" = summary(gp_lin)$p.coeff[-1] - 1.96*summary(gp_lin)$se[2:5], "Upper" = summary(gp_lin)$p.coeff[-1] + 1.96*summary(gp_lin)$se[2:5], "Variable" = c("Weight", "Income", "Sex", "Height"));
        mypredfits <- data.frame("Median" = gp_fit_ys$fit, "Lower" = gp_fit_ys$fit - 1.96*gp_fit_ys$se.fit, "Upper" = gp_fit_ys$fit + 1.96*gp_fit_ys$se.fit, "x" = X[,1]);
        mycompperf <- data.frame("Time" = time.taken, "Divergences" = NA, "Max_Treedepth" = NA, "RHat" = NA, "Min_Ess_Bulk" = NA, "Min_Ess_Tail" = NA, "Num_Param" = NA, "Num_Samples" = NA))
  X: $preds
  y: $outcome
  Z: $lin_preds
  $curve_fit: mycurvefits
  $beta_fit: mybetafits
  $pred_fit: mypredfits
  $comp_perf: mycompperf
  
Pspline: R( library(gamlss);
            newdata <- Z;
            newdata$y <- y;
            newdata$x <- X[,1];
            start.time <- Sys.time();
            mymodel <- gamlss(y ~ pb(x, control = pb.control(order = 1)) + Weight + Income + Sex + Height, data = newdata);
            preds_ys <- predict(mymodel, se.fit = TRUE);
            preds_fs <- predict(mymodel, se.fit = TRUE, type = "terms");
            end.time <- Sys.time();
            time.taken <- as.numeric(difftime(end.time, start.time, units="secs"));
            mycurvefits <- data.frame("Median" = preds_fs$fit[,"pb(x, control = pb.control(order = 1))"], "Lower" = preds_fs$fit[,"pb(x, control = pb.control(order = 1))"] - 1.96*preds_fs$se.fit[,"pb(x, control = pb.control(order = 1))"], "Upper" = preds_fs$fit[,"pb(x, control = pb.control(order = 1))"] + 1.96*preds_fs$se.fit[,"pb(x, control = pb.control(order = 1))"], "x" = X[,1]);
            mybetafits <- data.frame("Median" = summary(mymodel)[3:6, 1], "Lower" = summary(mymodel)[3:6, 1] - 1.96*summary(mymodel)[3:6, 2], "Upper" = summary(mymodel)[3:6, 1] + 1.96*summary(mymodel)[3:6, 2], "Variable" = c("Weight", "Income", "Sex", "Height"));
            mypredfits <- data.frame("Median" = preds_ys$fit, "Lower" = preds_ys$fit - 1.96*preds_ys$se.fit, "Upper" = preds_ys$fit + 1.96*preds_ys$se.fit, "x" = X[,1]);
            mycompperf <- data.frame("Time" = time.taken, "Divergences" = NA, "Max_Treedepth" = NA, "RHat" = NA, "Min_Ess_Bulk" = NA, "Min_Ess_Tail" = NA, "Num_Param" = NA, "Num_Samples" = NA))
  X: $preds
  y: $outcome
  Z: $lin_preds
  $curve_fit: mycurvefits
  $beta_fit: mybetafits
  $pred_fit: mypredfits
  $comp_perf: mycompperf
  
mad_adj: R( library(dplyr);
        mergedat <- left_join(yhat, f_true, by = c("x"));
        adj_factor <- mean(mergedat$Median);
        mergedat$Median <- mergedat$Median - adj_factor;
        mergedat$truth2 <- scale(mergedat$truth, scale = FALSE);
        mergedat$mad <- abs(mergedat$Median - mergedat$truth2);
        mean_abs_diff <- mean(mergedat$mad))
  yhat: $curve_fit
  f_true : $truth
  $metric: mean_abs_diff
  
pred_diff: R( my_pred_diff <- mean(abs(pred_hat$Median - y_true)))
  pred_hat: $pred_fit
  y_true : $true_y
  $metric: my_pred_diff
  
pred_cover: R( my_pred_cover <- mean(as.numeric(y_true >= pred_hat$Lower & y_true <= pred_hat$Upper)))
  pred_hat: $pred_fit
  y_true : $true_y
  $metric: my_pred_cover
  
beta1_bias: R(  beta1_bias <- abs(beta_true$beta[1] - beta_hat$Median[1]))
  beta_hat: $beta_fit
  beta_true : $true_betas
  $metric: beta1_bias
  
beta2_bias: R(  beta2_bias <- abs(beta_true$beta[2] - beta_hat$Median[2]))
  beta_hat: $beta_fit
  beta_true : $true_betas
  $metric: beta2_bias
  
beta3_bias: R(  beta3_bias <- abs(beta_true$beta[3] - beta_hat$Median[3]))
  beta_hat: $beta_fit
  beta_true : $true_betas
  $metric: beta3_bias
  
beta4_bias: R(  beta4_bias <- abs(beta_true$beta[4] - beta_hat$Median[4]))
  beta_hat: $beta_fit
  beta_true : $true_betas
  $metric: beta4_bias

width: R( library(dplyr);
          mergedat <- left_join(yhat, f_true, by = c("x"));
          mergedat$width <- mergedat$Upper - mergedat$Lower;
          cred_width <- mean(mergedat$width))
  yhat: $curve_fit
  f_true : $truth
  $metric: cred_width

cover_adj: R( library(dplyr);
          mergedat <- left_join(yhat, f_true, by = c("x"));
          adj_factor <- mean(mergedat$Median);
          mergedat$Lower <- mergedat$Lower - adj_factor;
          mergedat$Upper <- mergedat$Upper - adj_factor;
          mergedat$truth2 <- scale(mergedat$truth, scale = FALSE);
          mergedat$cover <- as.numeric(mergedat$truth2 <= mergedat$Upper & mergedat$truth2 >= mergedat$Lower);
          cred_cover <- mean(mergedat$cover))
  yhat: $curve_fit
  f_true : $truth
  $metric: cred_cover
  
beta1_cover: R(  beta1_cover <- as.numeric(beta_true$beta[1] >= beta_hat$Lower[1] & beta_true$beta[1] <= beta_hat$Upper[1]))
  beta_hat: $beta_fit
  beta_true : $true_betas
  $metric: beta1_cover
  
beta2_cover: R(  beta2_cover <- as.numeric(beta_true$beta[2] >= beta_hat$Lower[2] & beta_true$beta[2] <= beta_hat$Upper[2]))
  beta_hat: $beta_fit
  beta_true : $true_betas
  $metric: beta2_cover
  
beta3_cover: R(  beta3_cover <- as.numeric(beta_true$beta[3] >= beta_hat$Lower[3] & beta_true$beta[3] <= beta_hat$Upper[3]))
  beta_hat: $beta_fit
  beta_true : $true_betas
  $metric: beta3_cover
  
beta4_cover: R(  beta4_cover <- as.numeric(beta_true$beta[4] >= beta_hat$Lower[4] & beta_true$beta[4] <= beta_hat$Upper[4]))
  beta_hat: $beta_fit
  beta_true : $true_betas
  $metric: beta4_cover
  
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
    presimulate: presim
    simulate: bigstep, smooth
    analyze: HPR, GPR, Pspline
    score: mad_adj, pred_diff, pred_cover, beta1_bias, beta2_bias, beta3_bias, beta4_bias, width, cover_adj, beta1_cover, beta2_cover, beta3_cover, beta4_cover, div, time, rhat, min_samp_bulk, min_samp_tail, treedepth
  run: presimulate * simulate * analyze * score