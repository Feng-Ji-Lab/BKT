# =====================================================
# Predict test: hide each student's last response and
# evaluate predict_bkt on the held-out last attempts
# =====================================================

ensure_pkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

ensure_pkg("devtools")
ensure_pkg("peakRAM")
ensure_pkg("testthat")
ensure_pkg("pROC") # for AUC

suppressWarnings(try(unloadNamespace("BKT"), silent = TRUE))
rm(list = ls())

devtools::load_all("./")

set.seed(123)

prior <- 0.2
guess <- 0.1
slip <- 0.1
learn <- 0.3

num_students <- 500
min_questions <- 9
max_questions <- 13

prior <- 0.1
guess <- 0.01
slip  <- 0.01
learn <- 0.2

dat_full <- simulate_bkt_data(
    prior         = prior,
    guess         = guess,
    slip          = slip,
    learn         = learn,
    num_students  = num_students,
    min_questions = min_questions,
    max_questions = max_questions,
    output_file   = NULL
)

dat_full <- dat_full[order(dat_full$student_id, dat_full$order_id), ]

write.csv(dat_full, "dat_full_before.csv", row.names = FALSE)
cat("Saved original data to dat_full_before.csv\n")

last_idx <- unlist(
    tapply(seq_len(nrow(dat_full)), dat_full$student_id, function(idx) {
        idx[which.max(dat_full$order_id[idx])]
    })
)

holdout_last <- dat_full[last_idx, ]
train_dat <- dat_full[-last_idx, ]

write.csv(train_dat, "train_dat.csv", row.names = FALSE)
cat("Saved training data to train_dat.csv\n")

model <- bkt(
    seed = 42,
    num_fits = 5,
    parallel = FALSE,
    defaults = list(
        order_id   = "order_id",
        user_id    = "student_id",
        correct    = "correct",
        skill_name = "skill_name"
    )
)

fit_model <- fit(model, data = train_dat)
print(params(fit_model))

preds_df <- predict_bkt(fit_model, data = dat_full)

preds_df <- preds_df[order(as.numeric(preds_df$student_id), preds_df$order_id), ]

write.csv(preds_df, "dat_after_predict.csv", row.names = FALSE)
cat("Saved predicted data to dat_after_predict.csv\n")

last_idx_pred <- unlist(
    tapply(seq_len(nrow(preds_df)), preds_df$student_id, function(idx) {
        idx[which.max(preds_df$order_id[idx])]
    })
)

last_preds <- preds_df[last_idx_pred, c(
    "student_id",
    "order_id",
    "skill_name",
    "correct",
    "correct_predictions",
    "state_predictions"
)]

print(head(last_preds))

rmse_last <- sqrt(mean((last_preds$correct_predictions - last_preds$correct)^2))
mae_last <- mean(abs(last_preds$correct_predictions - last_preds$correct))

pred_class <- ifelse(last_preds$correct_predictions >= 0.5, 1, 0)
acc_last <- mean(pred_class == last_preds$correct)

roc_obj <- pROC::roc(
    response = last_preds$correct,
    predictor = last_preds$correct_predictions,
    quiet = TRUE
)
auc_last <- as.numeric(pROC::auc(roc_obj))

cat("\nHeld-out last attempts:\n")
cat("  RMSE =", rmse_last, "\n")
cat("  MAE  =", mae_last, "\n")
cat("  ACC  =", acc_last, "\n")
cat("  AUC  =", auc_last, "\n")
