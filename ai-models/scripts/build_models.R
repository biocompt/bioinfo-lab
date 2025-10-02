library(randomForestSRC, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr,         quietly = TRUE, warn.conflicts = FALSE)

build_survival_model <- function(data,
                                 time_col,
                                 status_col,
                                 ntree_set     = c(500, 1000, 1500, 2000),
                                 split     = c("logrank", "logrankscore", "bs.gradient"),
                                 samptype_set  = c("swor", "swr")) {
  # 1) Definir el grid de hiperparámetros
  tune_grid <- expand.grid(
    ntree    = ntree_set,
    samptype = samptype_set,
    splitrule = split,
    stringsAsFactors = FALSE
  )
  
  # 2) Crear la fórmula en base a los nombres que recibe la función
  surv_formula <- as.formula(
    paste0("Surv(", time_col, ", ", status_col, ") ~ .")
  )
  
  best_cindex <- -Inf
  best_brier  <-  Inf
  best_fit    <-  NULL
  res_list    <- vector("list", nrow(tune_grid))
  
  # 3) Búsqueda en grid
  for (i in seq_len(nrow(tune_grid))) {
    params <- tune_grid[i, ]
    
    sampsize_fn <- if (params$samptype == "swor") {
      function(x) min(x * 0.632, 300)
    } else {
      function(x) x
    }
    
    # 4) Ajustar el bosque pidiendo Brier OOB
    fit <- rfsrc(
      formula     = surv_formula,
      data        = data,
      ntree       = params$ntree,
      bootstrap   = "by.root",
      samptype    = params$samptype,
      sampsize    = sampsize_fn,
      importance  = "permute",
      splitrule = params$splitrule,
      save.memory = TRUE,
      na.action   = "na.impute",
      seed        = 1234
    )
    
    # 5) Métricas OOB
    oob_err    <- tail(fit$err.rate, 1)
    cindex_oob <- 1 - oob_err
    brier_vec  <- get.brier.survival(fit, cens.model = "rfsrc")$brier.score
    brier_oob  <- mean(brier_vec$brier.score, na.rm = TRUE)
    crps_std   <- get.brier.survival(fit, cens.model = "rfsrc")$crps.std
    
    res_list[[i]] <- data.frame(
      params,
      cindex_OOB = cindex_oob,
      brier_OOB  = brier_oob,
      crps_STD   = crps_std
    )
    
    # 6) Actualizar mejor modelo
    if (cindex_oob > best_cindex ||
        (cindex_oob == best_cindex && brier_oob < best_brier)) {
      best_cindex <- cindex_oob
      best_brier  <- brier_oob
      best_fit    <- fit
    }
  }
  
  # 7) Resultados
  tune_results <- bind_rows(res_list)
  list(
    model        = best_fit,
    tune_results = tune_results
  )
}

library(dplyr)
library(randomForestSRC)

build_classification_model <- function(data, test,
                                       y_col,
                                       ntree_set     = c(500, 1000, 1500, 2000),
                                       split         = c("gini", "auc", "entropy"),
                                       samptype_set  = c("swor", "swr")) {
  
  # 1) Grid de hiperparámetros
  tune_grid <- expand.grid(
    ntree     = ntree_set,
    samptype  = samptype_set,
    splitrule = split,
    stringsAsFactors = FALSE
  )
  
  # 2) Fórmula
  formula <- as.formula(paste0(y_col, " ~ ."))
  
  # 3) Inicializar métricas "mejores"
  best_err_M  <- Inf       # menor error en clase M
  best_auc    <- -Inf      # mayor AUC
  best_brier  <- Inf       # menor Brier
  best_fit    <- NULL
  
  res_list <- vector("list", nrow(tune_grid))
  
  # 4) Búsqueda en grid
  for (i in seq_len(nrow(tune_grid))) {
    params <- tune_grid[i, ]
    
    sampsize_fn <- if (params$samptype == "swor") {
      function(x) min(x * 0.632, 300)
    } else {
      function(x) x
    }
    
    # 4.1) Ajuste del modelo
    fit <- rfsrc(
      formula     = formula,
      data        = data,
      ntree       = params$ntree,
      bootstrap   = "by.root",
      samptype    = params$samptype,
      sampsize    = sampsize_fn,
      importance  = "permute",
      splitrule   = params$splitrule,
      save.memory = TRUE,
      na.action   = "na.impute",
      seed        = 1234
    )
    
    # 4.2) Predicción y métricas sobre test
    pred        <- predict(fit, test, na.action = "na.impute")
    auc         <- get.auc(test[[y_col]], pred$predicted)
    brier       <- get.brier.error(test[[y_col]], pred$predicted)
    class_errs  <- data.frame(get.confusion(test[[y_col]], pred$predicted))$class.error
    err_B       <- class_errs[1]
    err_M       <- class_errs[2]
    
    # 4.3) Guardar resultados parciales
    res_list[[i]] <- tibble::tibble(
      ntree      = params$ntree,
      samptype   = params$samptype,
      splitrule  = params$splitrule,
      auc        = auc,
      brier      = brier,
      err_B      = err_B,
      err_M      = err_M
    )
    
    # 4.4) Comparación lexicográfica:
    #     1) err_M ↓, 2) auc ↑, 3) brier ↓
    is_better <- (
      (err_M <  best_err_M) ||
        (err_M == best_err_M && auc   >  best_auc) ||
        (err_M == best_err_M && auc   == best_auc && brier < best_brier)
    )
    
    if (is_better) {
      best_err_M <- err_M
      best_auc   <- auc
      best_brier <- brier
      best_fit   <- fit
    }
  }
  
  # 5) Devolver mejor modelo y todos los resultados
  tune_results <- bind_rows(res_list)
  list(
    model        = best_fit,
    tune_results = tune_results
  )
}


improve_surv_model <- function(data,
                          time_col,
                          status_col,
                          predictors,
                          ntree_set     = c(500, 1000, 1500, 2000),
                          split          = c("logrank", "logrankscore", "bs.gradient"),
                          samptype_set   = c("swor", "swr")) {
  # 1) Grid de hiperparámetros
  tune_grid <- expand.grid(
    ntree     = ntree_set,
    samptype  = samptype_set,
    splitrule = split,
    stringsAsFactors = FALSE
  )
  
  # 2) Construir la fórmula Survival ~ predictors
  rhs <- paste(predictors, collapse = " + ")
  surv_formula <- as.formula(
    paste0("Surv(", time_col, ", ", status_col, ") ~ ", rhs)
  )
  
  best_cindex <- -Inf
  best_brier  <-  Inf
  best_fit    <-  NULL
  res_list    <- vector("list", nrow(tune_grid))
  
  # 3) Grid search
  for (i in seq_len(nrow(tune_grid))) {
    params <- tune_grid[i, ]
    
    sampsize_fn <- if (params$samptype == "swor") {
      # .632 del n, tope 300
      function(n) min(n * 0.632, 300)
    } else {
      # todo el n
      function(n) n
    }
    
    # 4) Ajustar RSF pidiendo métricas OOB
    fit <- rfsrc(
      formula     = surv_formula,
      data        = data,
      ntree       = params$ntree,
      bootstrap   = "by.root",
      samptype    = params$samptype,
      sampsize    = sampsize_fn,
      importance  = "permute",
      splitrule   = params$splitrule,
      save.memory = TRUE,
      na.action   = "na.impute",
      seed        = 1234
    )
    
    # 5) Extraer métricas OOB
    oob_err    <- tail(fit$err.rate, 1)
    cindex_oob <- 1 - oob_err
    brier_out  <- get.brier.survival(fit, cens.model = "rfsrc")$brier.score
    brier_oob  <- mean(brier_out$brier.score, na.rm = TRUE)
    crps_std   <- get.brier.survival(fit, cens.model = "rfsrc")$crps.std
    
    res_list[[i]] <- data.frame(
      params,
      cindex_OOB = cindex_oob,
      brier_OOB  = brier_oob,
      crps_STD   = crps_std
    )
    
    # 6) Actualizar mejor modelo (primero C, luego Brier)
    if (cindex_oob > best_cindex ||
        (cindex_oob == best_cindex && brier_oob < best_brier)) {
      best_cindex <- cindex_oob
      best_brier  <- brier_oob
      best_fit    <- fit
    }
  }
  
  # 7) Devolver resultados
  tune_results <- bind_rows(res_list)
  list(
    model        = best_fit,
    tune_results = tune_results
  )
}

improve_class_model <- function(data,
                                test_data,
                                y_col,
                                predictors,
                                ntree_set     = c(500, 1000, 1500, 2000),
                                split          = c("gini", "auc", "entropy"),
                                samptype_set   = c("swor", "swr")) {
  
  # 1) Grid de hiperparámetros
  tune_grid <- expand.grid(
    ntree     = ntree_set,
    samptype  = samptype_set,
    splitrule = split,
    stringsAsFactors = FALSE
  )
  
  # 2) Fórmula de clasificación usando sólo los predictors elegidos
  rhs <- paste(predictors, collapse = " + ")
  cls_formula <- as.formula(
    paste0(y_col, " ~ ", rhs)
  )
  
  # 3) Inicializar métricas "mejores" para comparación lexicográfica
  best_err_M  <- Inf     # minimizar error de la clase M
  best_auc    <- -Inf    # maximizar AUC
  best_brier  <- Inf     # minimizar Brier
  best_fit    <- NULL
  
  res_list <- vector("list", nrow(tune_grid))
  
  # 4) Grid search
  for (i in seq_len(nrow(tune_grid))) {
    params <- tune_grid[i, ]
    
    # función de sampsize según swor / swr
    sampsize_fn <- if (params$samptype == "swor") {
      function(n) min(n * 0.632, 300)
    } else {
      function(n) n
    }
    
    # 4.1) Ajustar el RF de clasificación
    fit <- rfsrc(
      formula     = cls_formula,
      data        = data,
      ntree       = params$ntree,
      bootstrap   = "by.root",
      samptype    = params$samptype,
      sampsize    = sampsize_fn,
      importance  = "permute",
      splitrule   = params$splitrule,
      save.memory = TRUE,
      na.action   = "na.impute",
      seed        = 1234
    )
    
    # 4.2) Predicción sobre test y cálculo de métricas
    pred        <- predict(fit, test_data, na.action = "na.impute")
    auc_val     <- get.auc(test_data[[y_col]], pred$predicted)
    brier_val   <- get.brier.error(test_data[[y_col]], pred$predicted)
    errs        <- data.frame(get.confusion(test_data[[y_col]], pred$predicted))$class.error
    err_B       <- errs[1]
    err_M       <- errs[2]
    
    # 4.3) Guardar resultados en la lista
    res_list[[i]] <- tibble::tibble(
      ntree      = params$ntree,
      samptype   = params$samptype,
      splitrule  = params$splitrule,
      AUC        = auc_val,
      Brier      = brier_val,
      err_B      = err_B,
      err_M      = err_M
    )
    
    # 4.4) Comparación lexicográfica
    #   1) err_M ↓
    #   2) AUC ↑
    #   3) Brier ↓
    is_better <- (
      (err_M <  best_err_M) ||
        (err_M == best_err_M && auc_val >  best_auc) ||
        (err_M == best_err_M && auc_val == best_auc && brier_val < best_brier)
    )
    
    if (is_better) {
      best_err_M <- err_M
      best_auc   <- auc_val
      best_brier <- brier_val
      best_fit   <- fit
    }
  }
  
  # 5) Devolver: el mejor modelo y la tabla con todas las combinaciones
  tune_results <- bind_rows(res_list)
  list(
    model        = best_fit,
    tune_results = tune_results
  )
}


# Función que calcula C-index, mean BS y CRPS.std para un RSF y lo devuelve como df de 1 fila
compute_surv_metrics <- function(model, test_data, model_name,
                                 time_col = "N_Days", status_col = "Status") {
  # 1) Predecir sobre test
  pred <- predict(model, test_data, na.action = "na.impute")
  
  # 2) C-index puro
  pe    <- get.cindex(test_data[[time_col]], test_data[[status_col]], pred$predicted)
  cidx  <- 1 - pe
  
  # 3) Brier Score puntual (media OOB / test)
  bs_obj   <- get.brier.survival(pred, cens.model = "rfsrc")
  mean_bs  <- mean(bs_obj$brier.score$brier.score, na.rm = TRUE)
  
  # 4) CRPS estandarizado
  crps_std <- bs_obj$crps.std
  
  # 5) Devuelve 1 fila con rowname = model_name
  df <- data.frame(
    C_index   = cidx,
    mean_BS   = mean_bs,
    CRPS_std  = crps_std,
    row.names = model_name,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  return(df)
}

compute_class_metrics <- function(model, test_data, y_col, model_name) {
  # 1) Predecir sobre test
  pred <- predict(model, test_data, na.action = "na.impute")
  
  # 2) Calcular AUC
  auc_val <- get.auc(test_data[[y_col]], pred$predicted)
  
  # 3) Calcular Brier score
  brier_val <- get.brier.error(test_data[[y_col]], pred$predicted)
  
  # 4) Extraer errores por clase de la matriz de confusión
  conf_df   <- data.frame(get.confusion(test_data[[y_col]], pred$predicted))
  class_err <- conf_df$class.error
  err_B     <- class_err[1]
  err_M     <- class_err[2]
  
  # 5) Devolver data.frame de 1 fila con rowname = model_name
  out <- data.frame(
    AUC     = auc_val,
    Brier   = brier_val,
    err_B   = err_B,
    err_M   = err_M,
    row.names = model_name,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  return(out)
}