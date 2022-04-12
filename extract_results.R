library(dscrutils)
library(tidyverse)
setwd("/home/ecchase/")

binomial_results <- dscquery(dsc.outdir = "binomial_basic", 
                             targets = c("simulate", "analyze", "score", "score.metric"))
save(list = c("binomial_results"), file = "binomial_results.RData")

binomial_aug_results <- dscquery(dsc.outdir = "binomial_augment", 
                                 targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(binomial_aug_results, "binomial_aug_results.csv")

poisson_results <- dscquery(dsc.outdir = "poisson_basic", 
                     targets = c("simulate", "analyze", "score", "score.metric"))
save(list = c("poisson_results"), file = "poisson_results.RData")
                            
poisson_aug_results <- dscquery(dsc.outdir = "poisson_augment", 
                                targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(poisson_aug_results, "poisson_aug_results.csv")

gaussian_results <- dscquery(dsc.outdir = "gaussian_basic", 
                             targets = c("simulate", "analyze", "score", "score.metric"))
save(list = c("gaussian_results"), file = "gaussian_results.RData")

gaussian_aug_results <- dscquery(dsc.outdir = "gaussian_augment", 
                                 targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(gaussian_aug_results, "gaussian_aug_results.csv")

monotonic_results <- dscquery(dsc.outdir = "monotonic_gaussian", 
                              targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(monotonic_results, "monotonic_results.csv")

plm_results <- dscquery(dsc.outdir = "gaussian_plm", 
                        targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(plm_results, "plm_results.csv")

bin_plm_results <- dscquery(dsc.outdir = "binomial_plm", 
                            targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(bin_plm_results, "bin_plm_results.csv")

pois_plm_results <- dscquery(dsc.outdir = "poisson_plm", 
                             targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(pois_plm_results, "pois_plm_results.csv")