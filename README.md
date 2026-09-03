# BKT: Bayesian Knowledge Tracing in R

`BKT` fits Bayesian Knowledge Tracing models to student response sequences. It supports model fitting, parameter inspection, prediction, evaluation, cross-validation, parameter fixing, and several BKT variants.

The package is based on the ideas implemented by [pyBKT](https://github.com/CAHLR/pyBKT).

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("Feng-Ji-Lab/bkt")
```

Then load the package:

```r
library(BKT)
```

## Included example data

The package includes `simulation_data_50`, a small simulated dataset containing 50 students and 494 response records:

```r
data("simulation_data_50", package = "BKT")
head(simulation_data_50)
```

The required columns are:

- `order_id`: response order within a student sequence
- `correct`: `1` for a correct response and `0` for an incorrect response
- `student_id`: student identifier
- `skill_name`: skill identifier

`BKT` recognizes `student_id` automatically, so no column mapping is needed for the included simulation data.

## Fit a model

Create and fit a model directly from the included data frame:

```r
library(BKT)
data("simulation_data_50", package = "BKT")

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
fitted_model <- fit(model, data = simulation_data_50)

params(fitted_model)
```

The returned parameter table contains the estimated learning, guessing, slipping, and prior probabilities for the `mathematic` skill.

## Prediction and evaluation

Use a fitted model to generate response and knowledge-state probabilities:

```r
predictions <- predict_bkt(fitted_model, data = simulation_data_50)
head(predictions)
```

The result contains the original data plus:

- `correct_predictions`: predicted probability of a correct response
- `state_predictions`: predicted probability associated with the latent knowledge state

The default evaluation metric is root mean squared error:

```r
evaluate(fitted_model, data = simulation_data_50)
```

A custom metric can also be supplied:

```r
mae <- function(true_vals, pred_vals) {
  mean(abs(true_vals - pred_vals))
}

evaluate(fitted_model, data = simulation_data_50, metric = mae)
```

## Cross-validation

Run cross-validation without downloading an external dataset:

```r
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

cv_results <- crossvalidate(
  model,
  data = simulation_data_50,
  folds = 2,
  parallel = FALSE
)

cv_results
```

## Forgetting model

Set `forgets = TRUE` when fitting to estimate a forgetting probability:

```r
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
fitted_with_forgetting <- fit(
  model,
  data = simulation_data_50,
  forgets = TRUE
)

params(fitted_with_forgetting)
```

## Fix model parameters

Use `set_coef()` to initialize a parameter and `fixed` to keep it unchanged during fitting:

```r
model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
model <- set_coef(
  model,
  list(mathematic = list(prior = 0.5))
)

fitted_fixed <- fit(
  model,
  data = simulation_data_50,
  skills = "mathematic",
  fixed = list(mathematic = list(prior = TRUE))
)

params(fitted_fixed)
```

## Custom column names

For a data frame with different column names, provide a `defaults` mapping. This example renames the included simulation data in memory:

```r
custom_data <- simulation_data_50
names(custom_data) <- c("sequence", "answer", "learner", "skill")

model <- bkt(
  seed = 42,
  num_fits = 1,
  parallel = FALSE,
  defaults = list(
    order_id = "sequence",
    correct = "answer",
    user_id = "learner",
    skill_name = "skill"
  )
)

fitted_custom <- fit(model, data = custom_data)
params(fitted_custom)
```

Alternatively, a CSV file can be supplied with `data_path`. In-memory data frames are preferable for reproducible examples because they do not require downloads or temporary files.

## Model variants

The supported variants are enabled through `fit()`:

- `multilearn = TRUE`: separate learning rates by learning resource
- `multiprior = TRUE`: separate prior probabilities by group
- `multipair = TRUE`: learning rates based on consecutive resource pairs
- `multigs = TRUE`: separate guess and slip rates by item

Variant data must contain the corresponding class column, supplied through `defaults` when its name differs from the variant argument. For example:

```r
variant_data <- simulation_data_50
variant_data$item <- paste0("item_", variant_data$order_id %% 2 + 1)

model <- bkt(
  seed = 42,
  num_fits = 1,
  parallel = FALSE,
  defaults = list(multigs = "item")
)

fitted_multigs <- fit(
  model,
  data = variant_data,
  multigs = TRUE
)

params(fitted_multigs)
```

## Simulate data

New BKT response sequences can be generated locally:

```r
set.seed(42)

simulated_data <- simulate_bkt_data(
  prior = 0.2,
  guess = 0.1,
  slip = 0.1,
  learn = 0.3,
  num_students = 5,
  min_questions = 5,
  max_questions = 10
)

head(simulated_data)
```

The function returns a data frame by default. Use `output_file` only when a CSV file is explicitly needed.

## License

`BKT` is licensed under the MIT License.
