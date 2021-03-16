#
# Initial settings
#

# Load packages
library(tidyverse)
library(readxl)
library(emmeans)
library(exact2x2)
library(patchwork)
library(colorspace)

# Helper function for data import
import_fun <- function(sheet, cols) {
  read_xlsx(
    path = "Data/data_EUPert_IgG.xlsx",
    sheet = sheet,
    range = cell_cols(cols),
    col_types = "text",
    # Function to replace spaces and forward slashes by underscores
    .name_repair = function(x) {
      x %>%
        str_replace_all(" ", "_") %>%
        str_replace_all("/", "_")
    })
}

# Define link function for logistic regression that corrects for specificity and sensitivity
# https://stackoverflow.com/questions/15931403/modify-glm-function-to-adopt-user-specified-link-function-in-r
logit_sp_se <- function(sp = 1, se = 1) {
  linkfun  <- function(mu)  log((1 - mu - sp)/(mu - se))
  linkinv  <- function(eta) (se*exp(eta) - sp + 1)/(exp(eta) + 1)
  mu.eta   <- function(eta) exp(eta)*(se + sp - 1)/(exp(eta) + 1)^2
  #validmu  <- function(mu) all(is.finite(mu)) && all(mu > (1 - sp) & mu < se)
  valideta <- function(eta) TRUE
  structure(
    list(
      linkfun = linkfun,
      linkinv = linkinv,
      mu.eta = mu.eta,
      # valuemu = validmu,
      valideta = valideta,
      name = "logit_sp_se"),
    class = "link-glm")
}

# Two-sided exact 2x2 test for two independent samples
# data1, data2 = two tibbles to compare with given x and n (sim = FALSE) or p_sim (sim = TRUE)
exact_2x2_test <- function(data1, data2, sim = FALSE) {
  # Init
  n_rec <- nrow(data1)
  p_value <- rep(NA, n_rec)
  # Loop over records
  for (i in 1:n_rec) {
    # Skip if n = 0
    if (data1[i, ]$n == 0 | data2[i, ]$n == 0) next
    # Use counts or simulations?
    if (!sim) {
      # Counts: extract x and to construct 2x2 table
      tab <- matrix(
        data = c(
          with(data1[i, ], c(x, n - x)),
          with(data2[i, ], c(x, n - x))),
        nrow = 2, ncol = 2)
      # p-value by exact 2x2 test using minlike method
      p_value[i] <- exact2x2(tab, tsmethod = "minlike")$p.value
    } else {
      # Simulations: extract p_sim
      p1_sim <- data1[i, ]$p_sim[[1]]
      p2_sim <- data2[i, ]$p_sim[[1]]
      # Difference
      pdiff_sim <- p1_sim - p2_sim
      # p-value by simulation
      p_value[i] <- 2*min(c(mean(pdiff_sim >= 0), mean(pdiff_sim < 0)))
    }
  }
  # Return output
  return(p_value)
}

# Sample true prevalence from posterior distribution
# that corrects for specificity and sensitivity
# Use Jeffrey's prior as default. This ensures finite values
p_sim <- function(x, n, sp = 1, se = 1, alpha_prior = 0.5, beta_prior = 0.5) {
  # Set seed for reproducibility
  set.seed(1)
  # Posterior parameters
  alpha_post <- alpha_prior + x
  beta_post  <- beta_prior  + n - x
  # Sample from posterior. These are the uncorrected prevalences
  p_est_post <- rbeta(n = 100000, alpha_post, beta_post)
  # Apply the correction. These are the true prevalences
  p_true_post <- (p_est_post + sp - 1)/(se + sp - 1)
  p_true_post <- pmax(0, pmin(p_true_post, 1))
  # Return samples
  return(p_true_post)
}
