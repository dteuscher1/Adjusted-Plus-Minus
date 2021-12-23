library(tidyverse)
points <- read.csv("Data/point_diff_2019_updated.csv", header = TRUE)



# set seed for replicability
set.seed(8675309)

# create a N x k matrix of covariates
X <- points %>% 
    dplyr::select(-point_diff, -game_id, -home_possession) %>% 
    as.matrix()
y <- points$point_diff
N <- nrow(X)

dat <- list(N=500, K=ncol(X), y=y[1:500], X=X[1:500,])

### Run the model and examine results
library(rstan)
fit <- stan(model_code = readLines("stan_model.stan"),
            data = dat,
            iter = 5000,
            warmup = 2500,
            thin = 1,
            chains = parallel::detectCores())

# summary
print(fit, pars=c('beta', 'sigma'), digits=3, prob=c(0.025, 0.5, 0.975))
