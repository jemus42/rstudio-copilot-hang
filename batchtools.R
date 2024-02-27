library(batchtools)

# Number of replicates, multiplied by 6 for total number of jobs
n_repls <- 1000
# Depending on local hardware
n_cpus <- 3

unlink("registry", recursive = TRUE)
reg <- makeExperimentRegistry("registry", seed = 1, conf.file = NA)
reg$cluster.functions <- makeClusterFunctionsMulticore(ncpus = n_cpus)

# Some toy data and algorithm
sim_norm <- function(data, job, n, p, beta) {
  x <- replicate(p, rnorm(n))
  y <- x %*% rep(beta, p) + rnorm(n)
  train <- data.frame(y = y, x = x)
  x <- replicate(p, rnorm(n))
  y <- x %*% rep(beta, p) + rnorm(n)
  test <- data.frame(y = y, x = x)
  list(train = train, test = test)
}

lm_wrapper <- function(data, job, instance, ...) {
  fit <- lm(y ~ ., data = instance$train, ...)
  pred <- predict(fit, instance$test)
  sqrt(mean((pred - instance$test$y)^2))
}

addProblem("norm", fun = sim_norm)
addAlgorithm("lm", fun = lm_wrapper)

prob_design <- list(norm = expand.grid(n = c(50, 100), p = 10, beta = c(0.25, 0.5, 1)))
algo_design <- list(lm = expand.grid())

addExperiments(prob_design, algo_design, repls = n_repls)

submitJobs()

# Rename log files to have .log_XYZ ending to simulate batchtools/SLURM array jobs with chunks just in case that matters
log_files = fs::dir_ls("registry/logs/")
log_files_new = fs::path_ext_set(log_files, paste0("log_", seq_along(log_files) %% 200))
fs::file_move(log_files, log_files_new)
