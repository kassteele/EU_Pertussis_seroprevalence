#
# Seroincidence calculations PT
#

# Load packages
library(seroincidence)

# Prepare data
# Only a selection of countries FI, NL and NO
PT_data <- eupert_complete_data %>%
  filter(Country %in% c("FI", "NL", "NO")) %>%
  filter(Gender == "Total") %>%
  filter(Age_group != "Total") %>%
  select(Age_group, Country, IgG = IgG_PT_IU_ml) %>%
  droplevels() %>%
  mutate(Age_group_Country = interaction(Age_group, Country, sep = "_")) %>%
  arrange(Age_group_Country)

#
# Seroincidence
#

# Visual check for parameter settings for censoring and 'negatives'
plot(PT_data %>% pull(IgG), log = "y")
abline(h = 0.850, col = 2, lwd = 2) # Censoring limit
abline(h = exp(log(1) + 1.96*c(-1, 0, 1)*0.5), lty = 2, col = 2, lwd = 2) # Negatives

# Teunis personal communication: use model 2:
# - seroconversion is instantaneous
# - decay proceeds as a power function
PT_fit <- estimateSeroincidence(
  data = PT_data,
  antibodies = "IgG",
  strata = "Age_group_Country",
  params = pertussisIgGPTParams2,
  censorLimits = list(IgG = 0.850),
  par0 = list(IgG = c(log(1), 0.5)),
  start = log(7/365.25),
  numCores = 20)
summary(PT_fit)

#
# Time to level PT = 100
#

# Simulate power decay of PT
n_t <- 100
n_sim <- 3000
t <- seq(from = 0, to = 2000, length = n_t)
y1    <- pertussisIgGPTParams2$IgG$y1[1:n_sim]
alpha <- pertussisIgGPTParams2$IgG$alpha[1:n_sim]
r     <- pertussisIgGPTParams2$IgG$r[1:n_sim]
y <- matrix(0, n_t, n_sim)
for (i in 1:n_sim) {
  y[, i] <- y1[i]*(1 + (r[i] - 1)*y1[i]^(r[i] - 1)*alpha[i]*t)^(-1/(r[i] - 1))
}

# Visualize power decay of PT
matplot(
  x = t, y = y, type = "l", lty = 1, col = adjustcolor(1, alpha = 0.1), log = "y", ylim = c(1, 5000))
matlines(
  x = t,
  y = y %>% apply(MARGIN = 1, FUN = quantile, probs = c(0.025, 0.5, 0.975)) %>% t(),
  lty = 1, lwd = 2, col = 2)
abline(h = 100, lty = 2,lwd = 2)

# Calculate time to level PT = 100
# Use the inverse of the power decay function
y100 <- 100
t100 <- (y1^(-r)*(y100/y1)^(-r)*y100-y1^(-r+1))/(alpha*(r-1))
t100 <- t100[t100 > 0]
quantile(t100, prob = c(0.025, 0.5, 0.975))

# Calculate seroprevalence PT >= 100
# given the sero incidence and time to PT = 100
lambda <- rnorm(
  n = length(t100),
  mean = summary(PT_fit)$Results[6, 1],
  sd = (summary(PT_fit)$Results[6, 3] - summary(PT_fit)$Results[6, 2])/(2*1.96))
p <- (t100/365.25)*lambda
quantile(100*p, probs = c(0.025, 0.5, 0.975))
