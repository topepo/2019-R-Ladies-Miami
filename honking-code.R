library(tidymodels)

# Used devtools::install_github("tidymodels/tune", ref = "honk-honk")
# FYI this branch induces a false 5% error rate to demonstrate the 
# communication of bad news :-)

data("Chicago", package = "modeldata")

chi_rec <-
  recipe(ridership ~ ., data = Chicago) %>%
  # Create holiday indicators
  step_holiday(date) %>%
  # Make _factors_ for day, month, and year
  step_date(date) %>%
  # We don't need the original date column
  step_rm(date) %>%
  # Make all factors into indicator variables
  step_dummy(all_nominal()) %>%
  # Remove any columns with a single value
  step_zv(all_predictors()) %>%
  # We wil be using methods that require the predictors to be in the same 
  # units/scale. First, we normalize the stations to have mean = 0 and sd = 1.
  # `stations` is included in  the modeldata package and is a list of the 
  # station predictors.
  step_normalize(one_of(!!stations)) %>% 
  # Convert the stations to principal components. 
  step_pca(one_of(!!stations), num_comp = tune()) %>% 
  # But wait! Our model will be using distances, so now _everything_ should be
  # on the same scale (inlcuding the PCA values).
  step_normalize(all_predictors())

knn_mod <- nearest_neighbor(neighbors = tune()) %>% 
  # The "engine" is the method for 
  # estimating parameters. For KNN
  set_engine("kknn") %>% 
  # The mode is the type of model (we have a
  # numeric outcome)
  set_mode("regression")

time_resamples <-
  rolling_origin(
    Chicago,
    initial = 364 * 15,
    assess = 7 * 2,
    skip = 7 * 2,
    cumulative = FALSE
  )

# library(doMC)
# registerDoMC(cores = parallel::detectCores())

set.seed(2132)
grid_results <-
  knn_mod %>%
  tune_grid(
    chi_rec,
    resamples = time_resamples,
    grid = 20,
    control = control_grid(verbose = TRUE)
  )

