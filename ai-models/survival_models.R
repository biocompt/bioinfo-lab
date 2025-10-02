# Function to print help message
print_help <- function() {
  cat("Usage:\n")
  cat("  Rscript survival_models.R -i <input_file>\n\n")
  cat("Arguments:\n")
  cat("  -i   Path to input data file (TSV format)\n")
  cat("  -h   Show this help message\n")
}

# Function to parse command line arguments with flags
parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0 || "-h" %in% args) {
    print_help()
    quit(save = "no", status = 0)
  }
  arg_list <- list()
  for (i in seq(1, length(args), by = 2)) {
    flag <- args[i]
    value <- args[i + 1]
    if (flag == "-i") arg_list$input <- value
  }
  # Check required arguments
  if (is.null(arg_list$input)) {
    cat("Error: Missing required arguments.\n\n")
    print_help()
    quit(save = "no", status = 1)
  }
  return(arg_list)
}

args <- parse_args()
if (!dir.exists("models")) dir.create("models")

library(parallel)
library(qs2)
options(rf.cores = detectCores() - 2)

# 1. Load data and convert columns to class
surv_data <- read.table(args$input, header = TRUE, sep = "\t",
                        stringsAsFactors = TRUE)

# 2. Split into training and test set
source("scripts/split_data.R")
split <- split_survival_data(surv_data, time_col = "N_Days", status_col = "Status")

train_data_surv <- split$train
test_data_surv <- split$test

# 3. Build the model
source("scripts/build_models.R")
survival_model <- build_survival_model(train_data_surv, "N_Days", "Status")

model <- survival_model$model
qs_save(model, "models/initial_survival_model.qs2")

pred <- predict(model, test_data_surv, na.action = "na.impute")
metrics_initial <- compute_surv_metrics(model, test_data_surv, "initial")

# 4. Reduce variables with importance and improve initial model
library(varPro)
imp <- cv.varpro(Surv(N_Days, Status) ~ ., train_data_surv, rmst = c(500, 1000, 1500))
vars_relevantes <- imp$imp.conserve$variable

reduced_model <- improve_surv_model(train_data_surv, "N_Days", "Status", vars_relevantes)
reduced_model <- reduced_model$model
qs_save(reduced_model, "models/reduced_survival_model.qs2")

pred_reduced <- predict(reduced_model, test_data_surv, na.action = "na.impute")
metrics_imp <- compute_surv_metrics(reduced_model, test_data_surv, "improved")

metrics_all <- rbind(metrics_initial, metrics_imp)
write.table(metrics_all, file = "models/metrics_all_survival.tsv", sep = "\t", quote = FALSE, row.names = TRUE)