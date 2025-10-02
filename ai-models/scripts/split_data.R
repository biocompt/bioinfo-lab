library(caret, quietly = T, warn.conflicts = F)
library(Hmisc, quietly = T, warn.conflicts = F)

split_survival_data <- function(data,
                                time_col,
                                status_col,
                                p_train = 0.7,
                                n_time_bins = 4,
                                seed = 1234) {
  set.seed(seed)
  
  # 1) Creamos los cortes de tiempo en N grupos por cuantiles
  #    cut2 asegura que cada bin tenga tamaño similar
  time_vals <- data[[time_col]]
  time_bin  <- cut2(time_vals, g = n_time_bins)
  
  # 2) Definimos la variable de estratificación
  strata <- interaction(data[[status_col]], time_bin, drop = TRUE)
  
  # 3) Usamos createDataPartition para un split estratificado
  train_idx <- createDataPartition(
    y    = strata,
    p    = p_train,
    list = FALSE
  )
  
  # 4) Devolvemos listas con train/test (sin la columna time_bin estratificadora)
  train <- data[ train_idx, , drop = FALSE]
  test  <- data[-train_idx, , drop = FALSE]
  list(train = train, test = test)
}

split_class_data <- function(data,
                             y_col,
                             p_train = 0.7,
                             seed = 1234) {
  set.seed(seed)
  
  # 3) Usamos createDataPartition para un split estratificado
  train_idx <- createDataPartition(
    y    = data[[y_col]],
    p    = p_train,
    list = FALSE
  )
  
  # 4) Devolvemos listas con train/test (sin la columna time_bin estratificadora)
  train <- data[ train_idx, , drop = FALSE]
  test  <- data[-train_idx, , drop = FALSE]
  list(train = train, test = test)
}