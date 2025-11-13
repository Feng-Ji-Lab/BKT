# ============================================
# Benchmark BKT: time + memory + MSE/Bias/SD
# Units: Memory in MiB (1 MiB = 1024^2 bytes)
# Outputs:
#   - performance_summary.csv
#   - parameter_recovery_results.csv
# ============================================

# ---- Packages ----
ensure_pkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
ensure_pkg("peakRAM")
ensure_pkg("progress")
ensure_pkg("devtools")

# ---- Clean & load local code ----
suppressWarnings(try(unloadNamespace("BKT"), silent = TRUE))
rm(list = ls())

# Your generator + package
# source("simulation/generator.R") # defines simulate_bkt_data()
devtools::load_all("./") # loads your BKT package in dev mode

# ---- Settings ----
prior <- 0.2
guess <- 0.1
slip <- 0.1
learn <- 0.3
parallel <- TRUE
min_questions <- 8
max_questions <- 12

true_params <- c(learns = learn, guesses = guess, slips = slip, prior = prior)

num_simulations <- 2 # change if needed
num_students_values <- c(50, 500, 2000, 10000) # dataset sizes

# ---- Helpers ----
compute_metrics <- function(estimated, true_values) {
    estimated <- as.numeric(estimated)
    errs <- estimated - true_values
    list(MSE = errs^2, Bias = errs)
}

# ---- Storage ----
perf_summary <- data.frame(
    data_size = integer(),
    total_time_sec = numeric(),
    avg_time_per_iter_sec = numeric(),
    mean_total_RAM_MiB = numeric(),
    mean_peak_RAM_MiB = numeric(),
    p95_peak_RAM_MiB = numeric(),
    max_peak_RAM_MiB = numeric(),
    stringsAsFactors = FALSE
)

msebias_rows <- list() # will bind into a CSV at the end

# ======================================================
# Main loop over dataset sizes
# ======================================================
for (num_students in num_students_values) {
    cat("\n=== Running:", num_students, "students ===\n")

    mse_values <- matrix(
        0,
        nrow = num_simulations, ncol = 4,
        dimnames = list(NULL, names(true_params))
    )
    bias_values <- matrix(
        0,
        nrow = num_simulations, ncol = 4,
        dimnames = list(NULL, names(true_params))
    )

    iter_time_sec <- numeric(num_simulations) # seconds
    iter_total_ram <- numeric(num_simulations) # MiB
    iter_peak_ram <- numeric(num_simulations) # MiB

    pb <- progress_bar$new(
        format = "  Processing [:bar] :percent in :elapsed (eta: :eta)",
        total = num_simulations, clear = FALSE, width = 60
    )

    dataset_tic <- proc.time()[["elapsed"]]

    for (i in seq_len(num_simulations)) {
        pb$tick()
        # --- Generate one dataset ---
        dat <- simulate_bkt_data(
            prior = prior,
            guess = guess,
            slip = slip,
            learn = learn,
            num_students = num_students,
            min_questions = min_questions,
            max_questions = max_questions,
            output_file = NULL # don't write to disk for fairness
        )

        # Wrap a whole iteration (data gen + fit + params) in peakRAM
        prof <- peakRAM({
            # --- Fit model ---
            model <- bkt(
                num_fits = 5, parallel = parallel,
                defaults = list(
                    "order_id"   = "order_id",
                    "user_id"    = "student_id",
                    "correct"    = "correct",
                    "skill_name" = "skill_name"
                )
            )
            result <- fit(model, data = dat)

            est <- setNames(params(result)$value, params(result)$param)
            met <- compute_metrics(est[names(true_params)], true_params)
            mse_values[i, ] <- met$MSE
            bias_values[i, ] <- met$Bias
        })

        iter_time_sec[i] <- as.numeric(prof$Elapsed_Time_sec[1])
        iter_total_ram[i] <- as.numeric(prof$Total_RAM_Used_MiB[1])
        iter_peak_ram[i] <- as.numeric(prof$Peak_RAM_Used_MiB[1])
    }

    total_time <- proc.time()[["elapsed"]] - dataset_tic

    # ---- Aggregate MSE/Bias/SD over iterations ----
    avg_mse <- colMeans(mse_values)
    avg_bias <- colMeans(bias_values)
    # SD of estimates = SD of (estimate - true), since true is constant
    sd_est <- apply(bias_values, 2, stats::sd, na.rm = TRUE)

    # Collect rows for the final MSE/Bias/SD CSV (one row per size & parameter)
    for (p in names(true_params)) {
        msebias_rows[[length(msebias_rows) + 1]] <- data.frame(
            data_size = num_students,
            parameter = p,
            MSE = round(avg_mse[p], 6),
            Bias = round(avg_bias[p], 6),
            SD = round(sd_est[p], 6),
            stringsAsFactors = FALSE
        )
    }

    # ---- Performance aggregation ----
    size_perf <- data.frame(
        data_size             = num_students,
        total_time_sec        = round(total_time, 3),
        avg_time_per_iter_sec = round(mean(iter_time_sec, na.rm = TRUE), 4),
        mean_total_RAM_MiB    = round(mean(iter_total_ram, na.rm = TRUE), 3),
        mean_peak_RAM_MiB     = round(mean(iter_peak_ram, na.rm = TRUE), 3),
        p95_peak_RAM_MiB      = round(as.numeric(quantile(iter_peak_ram, 0.95, na.rm = TRUE)), 3),
        max_peak_RAM_MiB      = round(max(iter_peak_ram, na.rm = TRUE), 3)
    )

    perf_summary <- rbind(perf_summary, size_perf)

    # ---- Pretty print (per size) ----
    cat("\n--- Parameter Recovery (averaged over", num_simulations, "runs) ---\n")
    print(
        data.frame(
            Parameter = names(true_params),
            MSE = round(avg_mse[names(true_params)], 6),
            Bias = round(avg_bias[names(true_params)], 6),
            SD = round(sd_est[names(true_params)], 6),
            row.names = NULL
        ),
        row.names = FALSE
    )

    cat("\n--- Performance (this size, memory in MiB) ---\n")
    print(size_perf, row.names = FALSE)
}

# ======================================================
# Write outputs
# ======================================================
write.csv(perf_summary, file = "performance_summary.csv", row.names = FALSE)

msebias_df <- do.call(rbind, msebias_rows)
write.csv(msebias_df, file = "parameter_recovery_results.csv", row.names = FALSE)

cat(
    "\nResults written to:\n",
    "- performance_summary.csv (time + memory; memory in MiB)\n",
    "- parameter_recovery_results.csv (MSE/Bias/SD by size & parameter)\n"
)
