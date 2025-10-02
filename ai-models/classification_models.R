# Function to print help message
print_help <- function() {
  cat("Usage:\n")
  cat("  Rscript classification_models.R -i <input_file>\n\n")
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

# ----------------------------------------------------------------------------------#

library(parallel)
library(qs2)
options(rf.cores = detectCores() - 2)

# 1. Load data and convert columns to factors
class_data <- read.table(args$input, header = TRUE, sep = "\t",
                         row.names = 1, stringsAsFactors = TRUE)

# 2. Split into training and test sets
source("scripts/split_data.R")

split <- split_class_data(class_data, "diagnosis")
train_data_class <- split$train
test_data_class <- split$test

# 3. Build the initial model
source("scripts/build_models.R")
initial_class_model <- build_classification_model(train_data_class, test_data_class, "diagnosis")

initial_model <- initial_class_model$model
qs_save(initial_model, 'models/initial_class_model.qs2')

pred <- predict(initial_model, test_data_class, na.action = "na.impute")
metrics_initial <- compute_class_metrics(initial_model, test_data_class, "diagnosis", "initial")

# 4. Reduce variables by importance and improve the initial model
library(varPro)
imp <- cv.varpro(diagnosis ~ ., data = train_data_class)
relevant_vars <- imp$imp.conserve$variable

model_imp <- improve_class_model(train_data_class, test_data_class, "diagnosis", relevant_vars)

model_reduced <- model_imp$model
qs_save(model_reduced, 'models/reduced_class_model.qs2')

pred_reduced <- predict(model_reduced, test_data_class, na.action = "na.impute")
metrics_imp <- compute_class_metrics(model_reduced, test_data_class, "diagnosis", "improved")

# Save results in the models directory
metrics_all <- rbind(metrics_initial, metrics_imp)
write.table(metrics_all, file = "models/metrics_all_classification.tsv", sep = "\t", quote = FALSE, row.names = TRUE)