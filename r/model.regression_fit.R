#-----------------------------------------------------------------
# regression model and prediction functions snippet
#
# TODO: Create an R package that contains ransac related functions
#-----------------------------------------------------------------

#############################################################
# broken stick fn
#############################################################
model.break_vector <- function(x, bp) {
  vec_lt <- ifelse(x < bp, bp - x, 0)
  vec_gt <- ifelse(x < bp, 0, x - bp)
  return(list(vec_lt = vec_lt, vec_gt = vec_gt))
}

model.search_break_point_obj_fn <- function(bp, data, model = lm) {
  data <- as.data.frame(data)
  y_var <- data[, 1]
  x_var <- data[, 2]
  x_var_broken <- model.break_vector(x_var, bp)
  fit <- model(y_var ~ x_var_broken$vec_lt + x_var_broken$vec_gt)
  sigma <- summary(fit)$sigma
  return(sigma)
}

model.search_break_point <- function(obj_fn, interval, ...) {
  optimize(obj_fn, interval, ...)
}

broken_stick_fn.model_fn <- function(data, formula_str, bp = NULL, verbose = F, ...) {
  
  data <- as.data.frame(data)
  orig_col_names <- colnames(data)[1:2]
  colnames(data)[1:2] <- c("y", "x")
  
  if(is.null(bp)) {
    bp <- model.search_break_point(..., data = data)$minimum
  }
  
  x_broken <- model.break_vector(data[, "x"], bp)
  data$x1 <- x_broken$vec_lt
  data$x2 <- x_broken$vec_gt
  lm_fit <- lm(as.formula(formula_str), data = data)
  
  if(verbose) {
    message("column names are: ", orig_col_names[1], " , ", orig_col_names[3:4])
    message("model fit is: ")
    print(summary(lm_fit))
  }
  return(list(data = data, model_fit = lm_fit, col_names = orig_col_names, break_point = bp, model = "broken_stick_fn"))
}

broken_stick_fn.predict_fn <- function(model_output, newdata) {
  if(!names(newdata) %in%  model_output$col_names) {message("newdata does not have the required columns")}
  newdata <- as.data.frame(newdata)
  
  x_broken <- model.break_vector(newdata[,1], model_output$break_point)
  newdata$x1 <- x_broken$vec_lt
  newdata$x2 <- x_broken$vec_gt
  y <- predict(model_output$model_fit, newdata = newdata)
  return(list(predicted = y))
}

#############################################################
# simple linear regression fn 
# (This can be merged to multiple linear regression but just keep it)
#############################################################
simple_linear_fn.model_fn <- function(data, formula_str, verbose = F, ...) {
  
  if(ncol(data) != 2) {stop("number of column of data using slm must be 2")}
  data <- as.data.frame(data)
  orig_col_names <- colnames(data)
  colnames(data) <- c("y", "x")
  
  lm_fit <- lm(as.formula(formula_str), data = data, ...)
  
  if(verbose) {
    message("column names are: ", orig_col_names[1], " , ", orig_col_names[2])
    message("model fit is: ")
    print(summary(lm_fit))
  }
  return(list(data = data, model_fit = lm_fit, col_names = orig_col_names, model = "simple_linear_fn"))
}

simple_linear_fn.predict_fn <- function(model_output, newdata) {
  ind <- which(names(newdata) %in% model_output$col_names)
  if(length(ind) == 0) { message("newdata does not have the required columns"); return(NULL)}
  names(newdata)[ind] <- "x"
  y <- predict(model_output$model_fit, newdata = newdata)
  return(list(predicted = y))
}

#############################################################
# linear regression fn (generalizes both simple and multiple)
#############################################################
linear_fn.model_fn <- function(data, formula_str, verbose = F, ...) {
  
  data <- as.data.frame(data)
  orig_col_names <- colnames(data)
  colnames(data) <- c("y", c(paste0("x", 1:(ncol(data)-1))))
  
  lm_fit <- lm(as.formula(formula_str), data = data, ...)
  
  if(verbose) {
    message("column names are: ", orig_col_names[1], " , ", orig_col_names[-1])
    message("model fit is: ")
    print(summary(lm_fit))
  }
  return(list(data = data, model_fit = lm_fit, col_names = orig_col_names, model = "linear_fn"))
}

linear_fn.predict_fn <- function(model_output, newdata) {
  ind <- match(names(newdata), model_output$col_names) %>% (function(x) {x <- x[!is.na(x)];x})
  ind <- ind - 1
  if(length(ind) == 0) { message("newdata does not have the required columns"); return(NULL)}
  names(newdata)[ind] <- paste0("x", (ind))
  
  y <- predict(model_output$model_fit, newdata = newdata)
  return(list(predicted = y))
}

#############################################################
# power fn
#############################################################
power_fn.model_fn <- function(data, initial_guess = NULL, verbose = F, ...) {
  
  data <- as.data.frame(data)
  orig_col_names <- colnames(data)[1:2]
  colnames(data)[1:2] <- c("y", "x")
  
  if(is.null(initial_guess)) {
    inverse_fit <- lm(log(y) ~ log(x), data = data)
    initial_guess = list(a = exp(coef(inverse_fit)[1]), b = coef(inverse_fit)[2])
  }
  nls_fit <- stats::nls(y ~ a * x^b, start = initial_guess, data = data, trace = FALSE)
  
  if(verbose) {
    message("column names are: ", orig_col_names[1], " , ", orig_col_names[2])
    message("model fit is: ")
    print(summary(nls_fit))
  }
  return(list(data = data, model_fit = nls_fit, col_names = orig_col_names, model = "power_fn"))
}

power_fn.predict_fn <- function(model_output, newdata) {
  ind <- which(names(newdata) %in% model_output$col_names)
  if(length(ind) == 0) { message("newdata does not have the required columns"); return(NULL)}
  names(newdata)[ind] <- "x"
  
  a <- coef(model_output$model_fit)[1]
  b <- coef(model_output$model_fit)[2]
  # c <- coef(model_output$model_fit)[3]
  x <- newdata[, model_output$col_names[2]]
  y <-  a * x^b 
  return(list(predicted = y))
}

#############################################################
# log fn
#############################################################
log_fn.model_fn <- function(data, start = NULL, verbose = F, ...) {
  
  data <- as.data.frame(data)
  orig_col_names <- colnames(data)[1:2]
  colnames(data)[1:2] <- c("y", "x")
  
  if(is.null(start)) {start <-  list(a = 1, b = 1, c = 0)}
  
  nls_fit <- stats::nls(y ~ a*log(b*x + c), data = data, start = start, ...)
  
  if(verbose) {
    message("column names are: ", orig_col_names[1], " , ", orig_col_names[2])
    message("model fit is: ")
    print(summary(nls_fit))
  }
  return(list(data = data, model_fit = nls_fit, col_names = orig_col_names, model = "log_fn_fn"))
}

log_fn.predict_fn <- function(model_output, newdata) {
  ind <- which(names(newdata) %in% model_output$col_names)
  if(length(ind) == 0) { message("newdata does not have the required columns"); return(NULL)}
  names(newdata)[ind] <- "x"  
  
  a <- coef(model_output$model_fit)[1]
  b <- coef(model_output$model_fit)[2]
  c <- coef(model_output$model_fit)[3]
  x <- newdata[, model_output$col_names[2]]
  y <- a*log(b*x + c)
  return(list(predicted = y))
}

#############################################################
# fitting summary functions
#############################################################
model.regression_accuracy <- function(y, yhat) {
  resid <- yhat - y
  num_rows <- which(!is.na(resid))
  resid <- resid[num_rows]
  yhat <- yhat[num_rows]
  y <- y[num_rows]
  mse <- mean(resid^2)
  data.frame(accuracy_mae = mean(abs(resid)),
             accuracy_mape = mean(abs(resid/y)), 
             accuracy_rmse = sqrt(mse), 
             accuracy_r2 = 1 - mse/mean((y - mean(y))^2), 
             bias_me = mean(resid), 
             bias_mpe = mean(resid/y))
}

model.summary <- function(model_output, make_plot = T) {
  fitted_data <- data.frame(model_output$data, pred = fitted(model_output$model_fit))
  summary <- data.frame(y = model_output$col_names[1], 
                        x = paste(model_output$col_names[-1], collapse = ", "), 
                        model = model_output$model,
                        model.regression_accuracy(fitted_data$y, fitted_data$pred))
  
  if(make_plot){
    x_vars <- grep("x", names(fitted_data), value = T)
    if(model_output$model == "broken_stick_fn") {x_vars = "x"}
    lapply(1:length(x_vars), function(x) {
      p <- ggplot() + 
        geom_point(data = fitted_data, aes_string(x = x_vars[x], y = "y")) + 
        geom_line(data = fitted_data, aes_string(x = x_vars[x], y = "pred"), col = "red") +
        ylab(model_output$col_names[1]) + 
        xlab(model_output$col_names[x+1])
      print(p)
    })
  }
  return(summary)
}

#############################################################
# curve fit wrapper function
#############################################################
model.curve_fit_wrapper <- function(data, y_vars, fit_method_map, make_plot = F) {
  model_list <- list()
  model_summary <- data.frame()
  for(y_var in y_vars) {
    ## fitting parameters
    predictor <- FIT_METHOD_MAP[[as.character(city_id)]][[y_var]][["predictor"]]
    fit_method <- FIT_METHOD_MAP[[as.character(city_id)]][[y_var]][["model_fn"]][["fit_method"]]
    model_fn <- MODEL_FIT_MAP[[fit_method]][["model_fn"]]
    params <- FIT_METHOD_MAP[[as.character(city_id)]][[y_var]][["model_fn"]][["params"]]
    params[["data"]] <- data[, c(y_var, predictor)]
    
    ## fiting summary
    fit <- do.call(model_fn, params)
    fit_summary <- model.summary(fit, make_plot = make_plot)
    model_list[[y_var]] <- fit
    model_summary <- rbind(model_summary, fit_summary)
    logger(sprintf("finished curve fit of %s", y_var))
  }
  return(list(model_list = model_list, model_summary = model_summary))
}

#############################################################
# model fit mapping
#############################################################
MODEL_FIT_MAP <- list(
  "broken_stick_fn" = list(
    model_fn = broken_stick_fn.model_fn,
    predict_fn = broken_stick_fn.predict_fn),
  
  "simple_linear_fn" = list(
    model_fn = simple_linear_fn.model_fn,
    predict_fn = simple_linear_fn.predict_fn),
  
  "log_fn" = list(
    model_fn = log_fn.model_fn,
    predict_fn = log_fn.predict_fn),
  
  "power_fn" = list(
    model_fn = power_fn.model_fn,
    predict_fn = power_fn.predict_fn),
  
  "linear_fn" = list(
    model_fn = linear_fn.model_fn,
    predict_fn = linear_fn.predict_fn)
)
