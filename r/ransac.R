suppressWarnings(suppressPackageStartupMessages({
  library(gridExtra)
  library(pryr)
  library(ggplot2)
}))

gen_dummy_data <- function(x, y, noise_ratio = 0.5) {
  if(length(x) != length(y)) {
    stop("Length of x and y have to be the same.")
  }
  n <- length(x)
  trend <- data.frame(x = x, y = y)
  noise <- data.frame(x = runif(noise_ratio*n, min(x), max(x)), y = runif(noise_ratio*n, min(y), max(y)))
  df <- rbind(trend, noise)
  df <- df[sample(1:nrow(df), nrow(df)), , drop = FALSE]
  return(df)
}

#' Random sample consensus (RANSAC) 
#' 
#' @param data dataframe with target column and model matrix columns
#' @param y_col name of target column
#' @param model model function to fit
#' @param model_args list, arguments to the model
#' @param n the minimum number of data values required to fit the model
#' @param k the maximum number of iterations allowed in the algorithm
#' @param t a threshold value for determining when a data point fits a model
#' @param d the number of close data values required to assert that a model fits well to data
#' 
#' @return a list of output contains best fitted model, inliers, outliers 
#' 
RANSAC <- function(data, y_col, model, model_args, n = 2, k = NA, t = NA, d = NA, verbose = FALSE) {
  
  default_ratio = 0.7 
  
  model_args_all <- c(model_args, data = list(data))
  model_all <- do.call(model, args = model_args_all)
  err_abs <- abs(data[, y_col] - predict(model_all, newdata = data[, names(df) != "y", drop=FALSE]))
  
  if(is.na(t)) {
    t <- quantile(err_abs, 0.5, names = FALSE)
  }
  if(is.na(d)) {
    d <- nrow(data)*default_ratio
  }
  if(is.na(k)) {
    k <- as.integer(log(1 - 0.99) / log(1 - default_ratio^n))
  }
  
  mae_best <- mean(err_abs)
  ind <- 1:nrow(data)
  inliers <- c()
  i = 1
  
  if(verbose) {
    print("fsadfas")
    progress_pct = round(quantile(seq(1, k, length.out = 10), probs = seq(0.1, 1, length.out = 10)), 0)
    cat(sprintf("Begin RANSAC algoritm with parameters:\nn = %s\nk = %s\nt = %s \nd = %s\n\n", n, k, t, d))
  }
  
  while(i <= k) {
    
    if(verbose && (i %in% progress_pct) ) {
      print("fsadfas")
      cat(sprintf("Completion percentage: %s\n", names(progress_pct)[which(i == progress_pct)]))
    }
    
    inliers_case <- sample(ind, n)
    model_args_case <- c(model_args, data = list(data[inliers_case, , drop=FALSE]))
    model_case <- do.call(model, model_args_case)
    
    predict_args_case = list(
      object = model_case,
      newdata = data[-inliers_case, , drop=FALSE]
    )
    yhat <- do.call(predict, predict_args_case)
    y <- data[-inliers_case, y_col]
    res <- abs(yhat - y)
    
    inliers_case <- c(inliers_case, names(res)[which(res < t)])
    if(length(inliers_case) > d) {
      data_inliers <- data[inliers_case, , drop=FALSE]
      model_inliers_args <- c(model_args, data = list(data_inliers))
      model_inliers <- do.call(model, model_inliers_args)
      
      predict_inliers_args_case = list(
        object = model_inliers,
        newdata = data_inliers
      )
      mae_inliers <- mean(abs(data_inliers[, y_col] - do.call(predict, predict_inliers_args_case)))
      if(mae_inliers < mae_best) {
        model_best <- model_inliers
        mae_best <- mae_inliers
        inliers <- inliers_case
      }
    }
    i = i + 1
  }

  if(!exists("model_best")) {
    
    warning("Final model could not be found, using all data to fit the model. ")
    model_best <- model_all
  } else {
    model_best["call"] <- sprintf("Model with formula with %s", model_args[["formula"]])  
  }
  inliers_df = data[sort(inliers), , drop = FALSE]
  outliers_df = data[setdiff(ind, sort(inliers)), , drop = FALSE]
  if(verbose) {
    cat(sprintf("%s (%.1f%%) inliers out of %s total data points have been used. \n", length(inliers), length(inliers) / nrow(data) * 100, nrow(data)))
  }
  return(list(model = model_best, inliers = inliers_df, outliers = outliers_df))
}

seed = 123
m = 100
x <- 1:m
y <- x + rnorm(m, 0, 3)
noise_ratio = c(0.25, 0.5, 1, 2, 3, 4, 5, 8, 10)
df_list <- list()
set.seed(seed)
do.call(partial(grid.arrange, nrow = 3), 
        lapply(noise_ratio, function(r) {
          df <- gen_dummy_data(x, y, noise_ratio = r)
          df_list[[as.character(r)]] <<- df
          ggplot() + 
            geom_point(data = df, aes(x, y)) + 
            ggtitle(sprintf("noise_ratio = %s", r))
        })
)

plot_list <- list()
for(r in noise_ratio) {
  
  args_list = list(
    data = df_list[[as.character(r)]],
    y_col = "y",
    model = lm,
    model_args = list(formula = "y ~ x"),
    n = 5,
    k = 500,
    t = NA,
    d = m / (1 + r),
    verbose = FALSE
  )
  
  set.seed(seed)
  fit <- RANSAC(data = args_list[["data"]], 
                y_col = args_list[["y_col"]], 
                model = args_list[["model"]], 
                model_args = args_list[["model_args"]], 
                n = args_list[["n"]], 
                k = args_list[["k"]],
                t = args_list[["t"]],
                d = args_list[["d"]],
                verbose = args_list[["verbose"]]
  )
  
  p <- ggplot() +
    geom_point(data = args_list[["data"]], aes(x = x, y = y), size = 0.6) +
    stat_smooth(data = args_list[["data"]], aes(x = x, y = y), method = "lm", alpha = 0, color = "blue", size = 1.2) +
    geom_point(data = fit[["inliers"]], aes(x, y), color = "red", size = 0.6) +
    stat_smooth(data = fit[["inliers"]], aes(x = x, y = y), method = "lm", alpha = 0, color = "green", size = 1.2) +
    ggtitle(sprintf("noise_ratio = %s", r))
  plot_list[[as.character(r)]] <- p
}

do.call(partial(grid.arrange, nrow = 3), lapply(plot_list, function(x) {x}))



