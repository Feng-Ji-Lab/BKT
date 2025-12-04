# ============================================================================
# BKT Regression Checklist Script
# ----------------------------------------------------------------------------
# Each test case has three parts:
#   1) Introduction
#   2) Case code (can be run from a clean R session)
#   3) Historical results (to be filled in after running)
#
# Recommended usage:
#   - Keep this script under version control.
#   - After modifying the BKT package, run selected cases or the whole file.
#   - Update "Historical results" sections with date, BKT version, and notes.
# ============================================================================


# ============================================================================
# Test Case 01: Basic model creation and internal data
# ----------------------------------------------------------------------------
# Introduction:
#   This case checks that a BKT model object can be created and that the
#   built-in dataset `simulation_data_50` is available and has a sane structure.
# ============================================================================

# --- Case 01: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 01 --------------\n")

devtools::load_all(".")

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)
# print(model)

data("simulation_data_50", package = "BKT")
print(head(simulation_data_50))
str(simulation_data_50)

# --- Case 01: Historical results -------------------------------------------
# order_id correct student_id skill_name
# 1        1       0          1 mathematic
# 2        2       0          1 mathematic
# 3        3       1          1 mathematic
# 4        4       1          1 mathematic
# 5        5       1          1 mathematic
# 6        6       1          1 mathematic
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 02: Fit on ct.csv for a single skill
# ----------------------------------------------------------------------------
# Introduction:
#   This case checks that `fetch_dataset()` can download ct.csv from GitHub
#   and that `fit()` runs successfully for the skill
#   "Plot non-terminating improper fraction". It also inspects the parameters.
# ============================================================================

# --- Case 02: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 02 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path = "ct.csv",
    skills = "Plot non-terminating improper fraction"
)

param_df <- params(result)
print(head(param_df))

# --- Case 02: Historical results -------------------------------------------
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.222837
# 2 Plot non-terminating improper fraction guesses default 0.002951
# 3 Plot non-terminating improper fraction   slips default 0.322440
# 4 Plot non-terminating improper fraction   prior default 0.737934
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 03: Save and load model (persistence)
# ----------------------------------------------------------------------------
# Introduction:
#   This case verifies that `save_model()` writes a model to disk and that
#   `load_model()` restores a functionally equivalent model. The parameters
#   from the loaded model are compared to the original.
# ============================================================================

# --- Case 03: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 03 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path = "ct.csv",
    skills = "Plot non-terminating improper fraction"
)
original_params <- params(result)
print(head(original_params))

model_file <- "model_saved_case03.pkl"
save_model(result, model_file)

model2 <- bkt()
model2 <- load_model(model2, model_file)
loaded_params <- params(model2)
print(head(loaded_params))

# Quick sanity check: compare column names and head
print(head(all(names(original_params) == names(loaded_params))))
print(head(original_params))
print(head(loaded_params))

# --- Case 03: Historical results -------------------------------------------
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.222837
# 2 Plot non-terminating improper fraction guesses default 0.002951
# 3 Plot non-terminating improper fraction   slips default 0.322440
# 4 Plot non-terminating improper fraction   prior default 0.737934
# ℹ Loading BKT
# File already exists: ./ct.csv
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.222837
# 2 Plot non-terminating improper fraction guesses default 0.002951
# 3 Plot non-terminating improper fraction   slips default 0.322440
# 4 Plot non-terminating improper fraction   prior default 0.737934
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.222837
# 2 Plot non-terminating improper fraction guesses default 0.002951
# 3 Plot non-terminating improper fraction   slips default 0.322440
# 4 Plot non-terminating improper fraction   prior default 0.737934
# [1] TRUE
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.222837
# 2 Plot non-terminating improper fraction guesses default 0.002951
# 3 Plot non-terminating improper fraction   slips default 0.322440
# 4 Plot non-terminating improper fraction   prior default 0.737934
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.222837
# 2 Plot non-terminating improper fraction guesses default 0.002951
# 3 Plot non-terminating improper fraction   slips default 0.322440
# 4 Plot non-terminating improper fraction   prior default 0.737934
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 04: Evaluate model with RMSE and custom MAE
# ----------------------------------------------------------------------------
# Introduction:
#   This case checks that `evaluate()` computes the default metric (RMSE)
#   and also works with a custom metric function (MAE).
# ============================================================================

# --- Case 04: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 04 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path = "ct.csv",
    skills = "Plot non-terminating improper fraction"
)

training_rmse <- evaluate(result, data_path = "ct.csv")
cat("Training RMSE:", training_rmse, "\n")

mae <- function(true_vals, pred_vals) {
    mean(abs(true_vals - pred_vals))
}

training_mae <- evaluate(result, data_path = "ct.csv", metric = mae)
cat("Training MAE:", training_mae, "\n")

# --- Case 04: Historical results -------------------------------------------
# Training RMSE: 0.4807593
# Training MAE: 0.4632686
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 05: Predict on as.csv and inspect predictions
# ----------------------------------------------------------------------------
# Introduction:
#   This case checks that `predict_bkt()` works on the as.csv dataset and
#   that we can subset predictions for the skill "Box and Whisker" with
#   the expected columns.
# ============================================================================

# --- Case 05: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 05 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/as.csv",
    "."
)

result <- fit(
    model,
    data_path = "as.csv",
    forgets = TRUE,
    skills = "Box and Whisker"
)

save_model(result, "model_case05.pkl")

model2 <- bkt()
model2 <- load_model(model2, "model_case05.pkl")

preds_df <- predict_bkt(model2, data_path = "as.csv")
cat("Prediction data frame (first rows):\n")
print(head(preds_df))

box_and_whisker_preds <- subset(
    preds_df,
    skill_name == "Box and Whisker",
    select = c("user_id", "correct", "correct_predictions", "state_predictions")
)
cat("Box and Whisker predictions (first rows):\n")
print(head(box_and_whisker_preds))

# --- Case 05: Historical results -------------------------------------------
# Prediction data frame (first rows):
#        order_id assignment_id user_id assistment_id problem_id original correct
# 3958   21617623        263599      14         53412      93383        1       0
# 94048  21617623        263599      14         53412      93383        1       0
# 172412 21617623        263599      14         53412      93383        1       0
# 3959   21617632        263599      14         53436      93407        1       1
# 94049  21617632        263599      14         53436      93407        1       1
# 172413 21617632        263599      14         53436      93407        1       1
# ...
# Box and Whisker predictions (first rows):
#   user_id correct correct_predictions state_predictions
# 1   64525       1           0.6934731         0.2998926
# 2   64525       1           0.8005197         0.1158210
# 3   70363       0           0.6934731         0.2998926
# 4   70363       1           0.5549912         0.5380185
# 5   70363       0           0.7348004         0.2288283
# 6   70363       1           0.5902264         0.4774299
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 06: Cross-validation on ct.csv
# ----------------------------------------------------------------------------
# Introduction:
#   This case checks that `crossvalidate()` runs with a small number of folds
#   (e.g., 2) and returns a result without errors. It also reports elapsed time.
# ============================================================================

# --- Case 06: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 06 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

start_time <- Sys.time()
cv_result <- crossvalidate(
    model,
    data_path = "ct.csv",
    skills = "Plot non-terminating improper fraction",
    folds = 2,
    parallel = FALSE
)
end_time <- Sys.time()

elapsed <- difftime(end_time, start_time, units = "secs")
cat("Cross-validation result (head):\n")
print(head(cv_result))
cat("Elapsed time:", round(elapsed, 2), "seconds\n")

# --- Case 06: Historical results -------------------------------------------
# Cross-validation result (head):
#                                    skill        dummy
# 1 Plot non-terminating improper fraction 0.483331....
# Elapsed time: 4.29 seconds
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 07: Fit with forgets = TRUE vs FALSE
# ----------------------------------------------------------------------------
# Introduction:
#   This case checks that both `forgets = TRUE` and `forgets = FALSE` runs
#   complete successfully, and compares the parameter tables.
# ============================================================================

# --- Case 07: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 07 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

res_forgets <- fit(
    model,
    data_path = "ct.csv",
    forgets = TRUE,
    skills = "Plot non-terminating improper fraction"
)

res_noforget <- fit(
    model,
    data_path = "ct.csv",
    forgets = FALSE,
    skills = "Plot non-terminating improper fraction"
)

p_forgets <- params(res_forgets)
p_noforget <- params(res_noforget)

cat("Parameters with forgets = TRUE (head):\n")
print(head(p_forgets))
cat("Parameters with forgets = FALSE (head):\n")
print(head(p_noforget))

# --- Case 07: Historical results -------------------------------------------
# Parameters with forgets = TRUE (head):
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.224056
# 2 Plot non-terminating improper fraction forgets default 0.000222
# 3 Plot non-terminating improper fraction guesses default 0.002461
# 4 Plot non-terminating improper fraction   slips default 0.322116
# 5 Plot non-terminating improper fraction   prior default 0.737541
# Parameters with forgets = FALSE (head):
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.222837
# 2 Plot non-terminating improper fraction guesses default 0.003102
# 3 Plot non-terminating improper fraction   slips default 0.322343
# 4 Plot non-terminating improper fraction   prior default 0.737753
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 08: Fixed prior = 0.5
# ----------------------------------------------------------------------------
# Introduction:
#   This case fixes the prior for "Plot non-terminating improper fraction"
#   at 0.5 and checks that the fitted model respects this fixed value.
# ============================================================================

# --- Case 08: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 08 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

model <- set_coef(
    model,
    list("Plot non-terminating improper fraction" = list("prior" = 0.5))
)

result <- fit(
    model,
    forgets = TRUE,
    data_path = "ct.csv",
    skills = "Plot non-terminating improper fraction",
    fixed = list(
        "Plot non-terminating improper fraction" = list("prior" = TRUE)
    )
)

p_fixed_prior <- params(result)
cat("Parameters with fixed prior = 0.5 (head):\n")
print(head(p_fixed_prior))

# Simple check: ensure prior is 0.5
prior_rows <- subset(p_fixed_prior, param == "prior")
if (nrow(prior_rows) == 0) {
    stop("No 'prior' rows found in params(result).")
}

# Coerce to numeric in case 'value' is stored as character
prior_values <- suppressWarnings(as.numeric(prior_rows$value))

if (any(is.na(prior_values))) {
    stop("Cannot convert prior values to numeric.")
}

if (!all(abs(prior_values - 0.5) < 1e-8)) {
    stop("Fixed prior is not 0.5 as expected.")
} else {
    cat("Check passed: all prior values are 0.5.\n")
}

# --- Case 08: Historical results -------------------------------------------
# Parameters with fixed prior = 0.5 (head):
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.236929
# 2 Plot non-terminating improper fraction forgets default 0.001697
# 3 Plot non-terminating improper fraction guesses default 0.135015
# 4 Plot non-terminating improper fraction   slips default 0.227079
# 5 Plot non-terminating improper fraction   prior default 0.500000
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 09: Fixed learns and forgets
# ----------------------------------------------------------------------------
# Introduction:
#   This case sets learns and forgets to 0.25 for the skill
#   "Plot non-terminating improper fraction" and fixes them during fitting.
#   It checks whether the fitted model keeps these values unchanged.
# ============================================================================

# --- Case 09: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 09 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

model <- set_coef(
    model,
    list("Plot non-terminating improper fraction" = list(
        "learns" = c(0.25),
        "forgets" = c(0.25)
    ))
)

cat("Initial parameters before fitting (head):\n")
print(head(params(model)))

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path = "ct.csv",
    forgets = TRUE,
    skills = "Plot non-terminating improper fraction",
    fixed = list(
        "Plot non-terminating improper fraction" = list(
            "learns" = TRUE,
            "forgets" = TRUE
        )
    )
)

p_fixed_lf <- params(result)
cat("Parameters after fitting with learns and forgets fixed (head):\n")
print(head(p_fixed_lf))

# --- Case 09: Historical results -------------------------------------------
# Initial parameters before fitting (head):
#                                    skill  param   class    value
# 1 Plot non-terminating improper fraction learns default 0.250000
# File already exists: ./ct.csv
# Parameters after fitting with learns and forgets fixed (head):
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.250000
# 2 Plot non-terminating improper fraction forgets default 0.250000
# 3 Plot non-terminating improper fraction guesses default 0.165527
# 4 Plot non-terminating improper fraction   slips default 0.121181
# 5 Plot non-terminating improper fraction   prior default 0.507612
# ============================================================================

# --- Case 10: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 10 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

model <- set_coef(
    model,
    list("Plot non-terminating improper fraction" = list(
        "guesses" = c(0.025),
        "slips" = c(0.025)
    ))
)

cat("Initial parameters (guesses and slips = 0.025; head):\n")
print(head(params(model)))

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path = "ct.csv",
    forgets = TRUE,
    skills = "Plot non-terminating improper fraction",
    fixed = list(
        "Plot non-terminating improper fraction" = list(
            "guesses" = TRUE,
            "slips"   = TRUE
        )
    )
)

p_fixed_gs <- params(result)
cat("Parameters after fitting with guesses and slips fixed (head):\n")
print(head(p_fixed_gs))

# --- Case 10: Historical results -------------------------------------------
# Initial parameters (guesses and slips = 0.025; head):
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction guesses default 0.025000
# 2 Plot non-terminating improper fraction   slips default 0.025000
# File already exists: ./ct.csv
# Parameters after fitting with guesses and slips fixed (head):
#                                    skill   param   class    value
# 1 Plot non-terminating improper fraction  learns default 0.392828
# 2 Plot non-terminating improper fraction forgets default 0.293042
# 3 Plot non-terminating improper fraction guesses default 0.025000
# 4 Plot non-terminating improper fraction   slips default 0.025000
# 5 Plot non-terminating improper fraction   prior default 0.521820
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 11: multilearn variant
# ----------------------------------------------------------------------------
# Introduction:
#   This case runs `fit()` with `multilearn = TRUE` and `forgets = TRUE`
#   to check that the multilearn variant works on ct.csv for the chosen skill.
# ============================================================================

# --- Case 11: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 11 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path = "ct.csv",
    multilearn = TRUE,
    forgets = TRUE,
    skills = "Plot non-terminating improper fraction"
)

cat("Parameters for multilearn variant (head):\n")
print(head(params(result)))

# --- Case 11: Historical results -------------------------------------------
#                                    skill  param         class    value
# 1 Plot non-terminating improper fraction learns RATIONAL1-007 0.086131
# 2 Plot non-terminating improper fraction learns RATIONAL1-014 0.441007
# 3 Plot non-terminating improper fraction learns RATIONAL1-100 0.000000
# 4 Plot non-terminating improper fraction learns RATIONAL1-101 0.136941
# 5 Plot non-terminating improper fraction learns RATIONAL1-117 0.429816
# 6 Plot non-terminating improper fraction learns RATIONAL1-118 0.536789
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 12: multigs variant
# ----------------------------------------------------------------------------
# Introduction:
#   This case runs `fit()` with `multigs = TRUE` on ct.csv for the skill
#   "Plot pi" to verify that the multigs variant works.
# ============================================================================

# --- Case 12: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 12 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path = "ct.csv",
    skills = c("Plot pi"),
    multigs = TRUE
)

cat("Parameters for multigs variant (head):\n")
print(head(params(result)))

# --- Case 12: Historical results -------------------------------------------
# 1 Plot pi  learns       default 0.986592
# 2 Plot pi guesses RATIONAL1-045 0.000000
# 3 Plot pi guesses RATIONAL1-063 0.000000
# 4 Plot pi guesses RATIONAL1-122 0.000000
# 5 Plot pi guesses RATIONAL1-156 0.000000
# 6 Plot pi guesses RATIONAL1-190 0.000000
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 13: multilearn + multigs variant
# ----------------------------------------------------------------------------
# Introduction:
#   This case runs `fit()` with both `multilearn = TRUE` and `multigs = TRUE`
#   plus `forgets = TRUE` for "Plot non-terminating improper fraction".
# ============================================================================

# --- Case 13: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 13 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path = "ct.csv",
    multilearn = TRUE,
    multigs = TRUE,
    forgets = TRUE,
    skills = "Plot non-terminating improper fraction"
)

cat("Parameters for multilearn + multigs variant (head):\n")
print(head(params(result)))

# --- Case 13: Historical results -------------------------------------------
# Parameters for multilearn + multigs variant (head):
#                                    skill  param         class    value
# 1 Plot non-terminating improper fraction learns RATIONAL1-007 0.416209
# 2 Plot non-terminating improper fraction learns RATIONAL1-014 0.278697
# 3 Plot non-terminating improper fraction learns RATIONAL1-100 0.000000
# 4 Plot non-terminating improper fraction learns RATIONAL1-101 0.201743
# 5 Plot non-terminating improper fraction learns RATIONAL1-117 0.369909
# 6 Plot non-terminating improper fraction learns RATIONAL1-118 0.708911
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 14: multipair variant
# ----------------------------------------------------------------------------
# Introduction:
#   This case runs `fit()` with `multipair = TRUE` on ct.csv for the skill
#   "Plot pi", verifying that the multipair variant works.
# ============================================================================

# --- Case 14: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 14 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path = "ct.csv",
    skills = c("Plot pi"),
    multipair = TRUE
)

cat("Parameters for multipair variant (head):\n")
print(head(params(result)))

# --- Case 14: Historical results -------------------------------------------
# Parameters for multipair variant (head):
#     skill  param                       class    value
# 1 Plot pi learns                     Default 0.363054
# 2 Plot pi learns RATIONAL1-063 RATIONAL1-237 1.000000
# 3 Plot pi learns RATIONAL1-254 RATIONAL1-063 1.000000
# 4 Plot pi learns RATIONAL1-284 RATIONAL1-254 1.000000
# 5 Plot pi learns RATIONAL1-063 RATIONAL1-122 1.000000
# 6 Plot pi learns RATIONAL1-122 RATIONAL1-284 1.000000
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 15: multipair + multigs variant
# ----------------------------------------------------------------------------
# Introduction:
#   This case runs `fit()` with `multipair = TRUE` and `multigs = TRUE`
#   plus `forgets = TRUE` for "Plot non-terminating improper fraction".
# ============================================================================

# --- Case 15: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 15 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path = "ct.csv",
    multigs   = TRUE,
    multipair = TRUE,
    forgets   = TRUE,
    skills    = "Plot non-terminating improper fraction"
)

cat("Parameters for multipair + multigs variant (head):\n")
print(head(params(result)))

# --- Case 15: Historical results -------------------------------------------
# Parameters for multipair + multigs variant (head):
#                                    skill  param                       class
# 1 Plot non-terminating improper fraction learns                     Default
# 2 Plot non-terminating improper fraction learns RATIONAL1-160 RATIONAL1-014
# 3 Plot non-terminating improper fraction learns RATIONAL1-117 RATIONAL1-287
# 4 Plot non-terminating improper fraction learns RATIONAL1-100 RATIONAL1-117
# 5 Plot non-terminating improper fraction learns RATIONAL1-014 RATIONAL1-100
# 6 Plot non-terminating improper fraction learns RATIONAL1-007 RATIONAL1-123
#      value
# 1 0.419322
# 2 0.674781
# 3 0.000000
# 4 0.000000
# 5 0.033139
# 6 0.999999
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 16: multiprior variant
# ----------------------------------------------------------------------------
# Introduction:
#   This case runs `fit()` with `multiprior = TRUE` on ct.csv for the skill
#   "Plot pi", verifying that the multiprior variant works.
# ============================================================================

# --- Case 16: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 16 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

fetch_dataset(
    model,
    "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
    "."
)

result <- fit(
    model,
    data_path  = "ct.csv",
    skills     = c("Plot pi"),
    multiprior = TRUE
)

cat("Parameters for multiprior variant (head):\n")
print(head(params(result)))

# --- Case 16: Historical results -------------------------------------------
# Parameters for multiprior variant (head):
#     skill   param    class    value
# 1 Plot pi  learns  default 0.634913
# 2 Plot pi guesses  default 0.010978
# 3 Plot pi   slips  default 0.030377
# 4 Plot pi   prior  0I891Gg 1.000000
# 5 Plot pi   prior 17116XP9 0.000000
# 6 Plot pi   prior 1712bs2B 0.000000
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 17: multiprior + multigs variant
# ----------------------------------------------------------------------------
# Introduction:
#   This case runs `fit()` with `multiprior = TRUE` and `multigs = TRUE`
#   plus `forgets = TRUE` for "Plot non-terminating improper fraction".
# ============================================================================

# --- Case 17: Code ----------------------------------------------------------
# rm(list = ls())

# if ("BKT" %in% loadedNamespaces()) {
#     try(unloadNamespace("BKT"), silent = TRUE)
# }

# cat("-------------- CASE 17 --------------\n")

# if (requireNamespace("devtools", quietly = TRUE)) {
#     devtools::load_all(".")
# } else {
#     library(BKT)
# }

# model <- bkt(seed = 42, num_fits = 1, parallel = FALSE)

# fetch_dataset(
#     model,
#     "https://raw.githubusercontent.com/CAHLR/pyBKT-examples/master/data/ct.csv",
#     "."
# )

# result <- fit(
#     model,
#     data_path  = "ct.csv",
#     multigs    = TRUE,
#     multiprior = TRUE,
#     forgets    = TRUE,
#     skills     = "Plot non-terminating improper fraction"
# )

# cat("Parameters for multiprior + multigs variant (head):\n")
# print(head(params(result)))

# --- Case 17: Historical results -------------------------------------------
# Date:
# BKT version:
# Notes:
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 18: defaults (column mapping) with custom simulation file
# ----------------------------------------------------------------------------
# Introduction:
#   This case checks that `defaults` can be used to map non-standard column
#   names in a custom simulation dataset. It assumes that the file
#   "simulation_data_500_need_defaults.csv" exists in the working directory.
# ============================================================================

# --- Case 18: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 18 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

if (!file.exists("simulation_data_500_need_defaults.csv")) {
    stop("simulation_data_500_need_defaults.csv not found in working directory.")
}

model <- bkt(
    seed = 42,
    num_fits = 1,
    parallel = FALSE,
    forgets = TRUE,
    defaults = list(
        order_id   = "r1",
        user_id    = "r3",
        correct    = "r2",
        skill_name = "r4"
    )
)

result <- fit(
    model,
    data_path = "simulation_data_500_need_defaults.csv"
)

cat("Parameters for defaults-mapped simulation data (head):\n")
print(head(params(result)))

# --- Case 18: Historical results -------------------------------------------
# Date:
# BKT version:
# Notes (file size, runtime, parameter sanity, etc.):
# ---------------------------------------------------------------------------


# ============================================================================
# Test Case 19: Simulation + prediction workflow with defaults
# ----------------------------------------------------------------------------
# Introduction:
#   This case completes a full workflow using a training simulation dataset
#   and a separate test dataset for prediction, both using `defaults` for
#   column mapping. It assumes that both
#   "simulation_data_500_need_defaults.csv" and
#   "simulation_data_500_need_defaults_test.csv" exist in the working
#   directory.
# ============================================================================

# --- Case 19: Code ----------------------------------------------------------
rm(list = ls())

if ("BKT" %in% loadedNamespaces()) {
    try(unloadNamespace("BKT"), silent = TRUE)
}

cat("-------------- CASE 19 --------------\n")

if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
} else {
    library(BKT)
}

if (!file.exists("simulation_data_500_need_defaults.csv")) {
    stop("simulation_data_500_need_defaults.csv not found in working directory.")
}

if (!file.exists("simulation_data_500_need_defaults_test.csv")) {
    stop("simulation_data_500_need_defaults_test.csv not found in working directory.")
}

model <- bkt(
    seed = 42,
    num_fits = 1,
    parallel = FALSE,
    forgets = TRUE,
    defaults = list(
        order_id   = "r1",
        user_id    = "r3",
        correct    = "r2",
        skill_name = "r4"
    )
)

result <- fit(
    model,
    data_path = "simulation_data_500_need_defaults.csv"
)

cat("Parameters from simulation training data (head):\n")
print(head(params(result)))

preds_df <- predict_bkt(
    result,
    data_path = "simulation_data_500_need_defaults_test.csv"
)

cat("Predictions on simulation test data (head):\n")
print(head(preds_df))

# --- Case 19: Historical results -------------------------------------------
# Parameters from simulation training data (head):
#        skill   param   class    value
# 1 mathematic  learns default 0.294188
# 2 mathematic guesses default 0.089263
# 3 mathematic   slips default 0.090359
# 4 mathematic   prior default 0.239617
# Predictions on simulation test data (head):
#   r1 r2  r3         r4 correct_predictions state_predictions
# 1  1  0 450 mathematic           0.2858398      7.603826e-01
# 2  2  1 450 mathematic           0.3481631      6.844136e-01
# 3  3  1 450 mathematic           0.8080376      1.238499e-01
# 4  4  1 450 mathematic           0.9017193      9.656585e-03
# 5  5  1 450 mathematic           0.9090878      6.747017e-04
# 6  6  1 450 mathematic           0.9096030      4.675903e-05
# ---------------------------------------------------------------------------
