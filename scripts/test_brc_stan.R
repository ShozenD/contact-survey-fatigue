library(readr)
library(data.table)
library(cmdstanr)
library(reshape2)
library(ggplot2)

data <- read_rds("data/silver/covimod-wave-20.rds")

stan_data <- data
stan_data$A <- 85
stan_data$A2 <- stan_data$A*(stan_data$A + 1)/2
stan_data$Xhsgp <- scale(make_nn_lowertri_idxset(85))
stan_data$sym_from_lowertri_idxset <- make_sym_from_lowertri_idx(85)
stan_data$S <- make_S_matrix(20, 20)
stan_data$hatGamma <- 1
stan_data$hatKappa <- 1
stan_data$hatEta <- 1

age_idxset <- stan_data$age_idxset
age_idxset[,1] <- ifelse(age_idxset[,1] > 85, 85, age_idxset[,1])
stan_data$age_idxset <- age_idxset

stan_data$M1 <- 20
stan_data$M2 <- 20
stan_data$C1 <- 1.5
stan_data$C2 <- 1.5

job_idx <- rep(0, stan_data$Npart)
for (i in 1:nrow(stan_data$jobD)) {
  if (sum(stan_data$jobD[i,]) > 0) {
    job_idx[i] <- which(stan_data$jobD[i,] > 0, arr.ind = TRUE)
  }
}
stan_data$job_idx <- job_idx

model <- cmdstan_model("stan_models/tests/test_brc.stan", compile = TRUE)

fit <- model$sample(stan_data,
                    fixed_param = TRUE,
                    chains = 1,
                    iter_warmup = 0,
                    iter_sampling = 1)

po <- fit$draws("Y", format = "matrix")
po <- reshape2::melt(po)

po <- fit$draws("lambda_obs", format = "matrix")
dim(po)
po <- reshape2::melt(po)

df_grid <- expand.grid(x = 0:84, y = 0:84)
df_grid$z <- po$value

ggplot(df_grid, aes(x, y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_bw() +
  coord_equal()

stan_data$age_strata_map
