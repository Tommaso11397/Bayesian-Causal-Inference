# All the code is a joint effort with Flavio Argentieri


# This is to run the Stan Model

setwd("C:/Users/Utente/Desktop/University/Bayesian Statistics/Lavoro di Gruppo/Bayesian Potential Outcomes/Code and Data")
rm(list=ls())


#install.packages(c("MASS", "dplyr", "bayesplot", "rstanarm", "tidyverse","ggmcmc"))

library(parallel)
library(MASS)
library(StanHeaders)
library(ggplot2)
library(rstan)
library(dplyr)
library(bayesplot)
library(rstanarm, options(mc.cores = parallel::detectCores()))   
library(tidyverse)
library(ggmcmc)

individual_level_variables <- read.csv("individual_level_variables.csv", sep = ";")
school_level_variables <- read.csv("school_level_variables.csv", sep = ";")



N <- 5135
K <- 5
J <- 108
L <- 12

y <- individual_level_variables[,8]
w <- individual_level_variables[,2]
x <- individual_level_variables[,3:7]
u <- school_level_variables[,2:13]

ordered_groups <- read.csv("ordered_groups.csv", sep = ";")
jj <- c(1:5135)
for (n in c(1:5135)) {
  jj[n] <- ordered_groups[n,2]
}


stan_data <- list(N = N, K = K, J = J, L = L, x = as.matrix(x), y = y, g = jj, u = as.matrix(u), w = w)

Trial <- stan(file = "Cholensky Decomposition.stan",
                data = stan_data,
                iter = 1000, chains = 4, warmup = 500, seed = 69,
                sample_file = file.path("mysimulation.csv"),
                control = list(adapt_delta = 0.95, max_treedepth = 12),
                cores = 8)

#csvfiles <- c("mysimulation_1.csv", "mysimulation_2.csv", "mysimulation_3.csv",
              "mysimulation_4.csv")
#Trial <- read_stan_csv(csvfiles)

# fit_mod_joint <- sflist2stanfit(fit_mod1, fit_mod2)

# expose_stan_functions(".stan", includes = NULL,
#                      show_compiler_warnings = FALSE)


param <- c("effect", "effect_fs", "effect_qte25", "effect_qte50", "effect_qte75", "alpha")
print(Prova_1, pars=param, probs = c(0.1, 0.5, 0.9), digits = 3)

################################################################################
################################################################################

# Set your working directory here

setwd("C:/Users/Utente/Desktop/University/Bayesian Statistics/Lavoro di Gruppo/Bayesian Potential Outcomes/Code and Data")
rm(list=ls())

# If you have not installed the required packages uncomment the next line

# install.packages(c("MASS", "StanHeaders", "dplyr", "bayesplot", "rstanarm", "tidyverse", "ggplot2"))

library(MASS)
library(StanHeaders)
library(ggplot2)
library(rstan)
library(dplyr)
library(bayesplot)
library(rstanarm)   
library(tidyverse)


# This is a function we will use to compare plots

compare_plots <- function(plot_1, plot_2, ncol = 2, ...) {
  bayesplot_grid(
    plot_1, plot_2, 
    grid_args = list(ncol = ncol),
    ...
  )
}



# Here we clean the environment

rm(list = "fit_mod5", "Vectorized", "x", "u", "individual_level_variables", 
   "school_level_variables", "ordered_groups", "stan_data", "w", "y", "K", "J",
   "csvfiles", "n", "N", "jj", "L")



# This is the list of treatment effects of interest

param <- c("effect", "effect_fs", "effect_qte25", "effect_qte50", "effect_qte75")

#Duflo, Dupas & Kremer result

mte_duflo <- 0.119


################################################################################

# Now we start creating the Figures

################################################################################

# First, we show how the Vectorized model performs poorly

# This is the Vectorized Model with divergences

load("Vectorized final.RData")

post_Vectorized_std <- as.array(Vectorized)

# Trace Plot for Vectorized centered model

color_scheme_set("viridis")
mcmc_trace(post_Vectorized_std, pars = param,
           np = nuts_params(Vectorized)) +
  ggplot2::labs(title = "Trace Plots (25.15% divergencies)", 
                subtitle = "Vectorized Model")

################################################################################

# Energy Plot and Autocorrelation plot for Vectorized model


mcmc_nuts_energy(nuts_params(Vectorized), 
                 log_posterior(Vectorized), binwidth = 5) +
  ggplot2::labs(title = "Energy Plot", 
                subtitle = "Vectorized Model")

mcmc_acf(post_Vectorized_std, pars = c("effect", "effect_fs"), lags=50) +
  ggplot2::labs(title = "Autocorrelation Plot", 
                subtitle = "Vectorized Model")

# ESS for the Vectorized model (this will be used at the end, it will not print
# the graph here)

neff_Vectorized <- mcmc_neff(neff_ratio(Vectorized, pars = param))  +
  ggplot2::labs(title = "Vectorized Uncentered Model", 
                subtitle = "Effective Sample Size")

rm("Vectorized", "csvfiles", "post_Vectorized_std")

################################################################################
################################################################################


## Now we look at the Cholesky model and see how the performance improves

load("Cholesky.RData")
post_Cholesky_std <- as.array(Cholesky)


# Trace plots

color_scheme_set("viridis")
mcmc_trace(post_Cholesky_std, pars = param,
           np = nuts_params(Cholesky)) +
  ggplot2::labs(title = "Trace Plots", 
                subtitle = "Cholesky Model")


################################################################################

# Energy plots

mcmc_nuts_energy(nuts_params(Cholesky), 
                 log_posterior(Cholesky), binwidth = 5) +
  ggplot2::labs(title = "Energy Plot", 
                subtitle = "Cholesky Model")


################################################################################

# Autocorrelation plots

mcmc_acf(post_Cholesky_std, 
         pars = c("effect", "effect_fs"), lags=50) +
  ggplot2::labs(title = "Autocorrelation Plot", 
                subtitle = "Cholesky Model") 

################################################################################

# ESS (it will not print it now)

neff_Cholesky <- mcmc_neff(neff_ratio(Cholesky, pars = param))  +
  ggplot2::labs(title = "Cholesky Model", 
                subtitle = "Effective Sample Size")


rm("Cholesky", "csvfiles", "post_Cholesky_std")
################################################################################
################################################################################


# Now we consider the model with Cholesky and QR reparametrization 

load("Cholesky_QR final.RData")
post_Cholesky_QR_std <- as.array(Cholesky_QR)


################################################################################

# Trace plot

mcmc_trace(post_Cholesky_QR_std, pars = param,
           np = nuts_params(Cholesky_QR)) +
  ggplot2::labs(title = "Trace Plots", 
                subtitle = "Cholesky and QR Model")

################################################################################

# Energy plot

mcmc_nuts_energy(nuts_params(Cholesky_QR), 
                 log_posterior(Cholesky_QR), binwidth = 5) +
  ggplot2::labs(title = "Energy Plot", 
                subtitle = "Cholesky and QR Model")

# Autocorrelation plots


mcmc_acf(post_Cholesky_QR_std, 
         pars = c("effect", "effect_fs"), lags=50) +
  ggplot2::labs(title = "Autocorrelation Plot", 
                subtitle = "Cholesky and QR Model") 

################################################################################

neff_Cholesky_QR <- mcmc_neff(neff_ratio(Cholesky_QR, pars = param))  +
  ggplot2::labs(title = "Cholesky and QR Model", 
                subtitle = "Effective Sample Size")


compare_plots(plot_1 = neff_Vectorized, plot_2 = neff_Cholesky, ncol=1)

compare_plots(plot_1 = neff_Cholesky, plot_2 = neff_Cholesky_QR, ncol=1)

# Cholesky QR has surprisingly slightly lower ESS

################################################################################
################################################################################

## We have finished the diagnostic, now let's see the results, in this case
## we only consider the Cholesky QR model, since results are stable


# Interval plots

mcmc_intervals(post_Cholesky_QR_std, pars = param, 
               prob = 0.8, prob_outer = 0.99, point_est = "mean") +
  vline_at(mte_duflo, linetype = "dashed", color = "red", size = 1) + 
  ggplot2::labs(title = "Main Treatment Effects - Cholensky and QR Model", 
                subtitle = "with means and 80% intervals (Duflo et al. mean treatment effect in red)")


################################################################################

# Area plots

mcmc_areas(post_Cholensky_QR_std, pars = param, 
           prob = 0.8, prob_outer = 0.99, point_est = "mean")  +
  vline_at(mte_duflo, linetype = "dashed", color = "red", size = 1) + 
  ggplot2::labs(title = "Main Treatment Effects - Cholensky and QR Model", 
                subtitle = "Posterior Distribution of treatment effects (Duflo et al. mean treatment effect in red)")

################################################################################

# Histogram by chain plot

mcmc_hist_by_chain(post_Cholesky_QR_std, pars = c("effect", "effect_fs"), binwidth = 0.005) +
  vline_at(mte_duflo, linetype = "dashed", color = "red", size = 1) + 
  ggplot2::labs(title = "Population and Finite Sample Treatment Effects - Cholensky and QR Model", 
                subtitle = "Histograms of effects by chain (Duflo et al. mean treatment effect in red)")


################################################################################

# Density by chain overlayed plot

mcmc_dens_overlay(post_Cholesky_QR_std, pars = c("effect", "effect_fs"), color_chains = TRUE) +
  vline_at(mte_duflo, linetype = "dashed", color = "red", size = 1,) + 
  ggplot2::labs(title = "Population and Finite Sample Treatment Effects - Cholensky and QR Model", 
                subtitle = "Density of effects by chain (Duflo et al. mean treatment effect in red)")



################################################################################
################################################################################
################################################################################

# Put in the Appendix


mcmc_nuts_treedepth(nuts_params(Vectorized), log_posterior(Vectorized)) +
  ggplot2::labs(title = "Vectorized model", 
                subtitle = "Tree Depth Plot")

mcmc_nuts_treedepth(nuts_params(Cholensky), log_posterior(Cholensky)) +
  ggplot2::labs(title = "Cholensky model", 
                subtitle = "Tree Depth Plot")

mcmc_nuts_treedepth(nuts_params(Cholensky_QR), log_posterior(Cholensky_QR)) +
  ggplot2::labs(title = "Cholensky and QR model", 
                subtitle = "Tree Depth Plot")