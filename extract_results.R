library(dscrutils)
library(tidyverse)
setwd("/home/ecchase/")

binomial_results <- dscquery(dsc.outdir = "binomial_basic", targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(binomial_results, "binomial_results.csv")

binomial_aug_results <- dscquery(dsc.outdir = "binomial_augment", targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(binomial_aug_results, "binomial_aug_results.csv")

poisson_results <- dscquery(dsc.outdir = "poisson_basic", targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(poisson_results, "poisson_results.csv")

poisson_aug_results <- dscquery(dsc.outdir = "poisson_augment", targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(poisson_aug_results, "poisson_aug_results.csv")

gaussian_results <- dscquery(dsc.outdir = "gaussian_basic", targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(gaussian_results, "gaussian_results.csv")

gaussian_aug_results <- dscquery(dsc.outdir = "gaussian_augment", targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(gaussian_aug_results, "gaussian_aug_results.csv")

monotonic_results <- dscquery(dsc.outdir = "monotonic_gaussian", targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(monotonic_results, "monotonic_results.csv")

plm_results <- dscquery(dsc.outdir = "gaussian_plm", targets = c("simulate", "analyze", "score", "score.metric"))
write_csv(plm_results, "plm_results.csv")
