#' Title
#'
#' @param df
#' @param include_intercept
#' @param model_type
#'
#' @return
#' @export
#'
#' @examples

fit_rolling_regression <- function(df_list, include_intercept = FALSE, model_type = "OLS") {
  # Validate input
  if (model_type != "OLS") {
    stop("Currently, only OLS model type is supported.")
  }

  # Initialize a list to store errors for each fold
  list_errors <- list()

  # Process each data frame in the list
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]
    # Identify columns
    value_sales_cols <- grep("^value_sales_[ABCDEFG]", names(df), value = TRUE)
    training_data <- df[df$indicate_train_test == 'train', ]
    testing_data <- df[df$indicate_train_test == 'test', ]

    # Initialize error collection for this dataframe
    errors <- setNames(numeric(length(value_sales_cols)), value_sales_cols)

    # Fit models and make predictions
    for (col in value_sales_cols) {
      # Define formula
      formula <- as.formula(paste(col, "~ value_sales_0_sum", ifelse(include_intercept, "", " - 1")))

      # Fit model
      if (model_type == "OLS") {
        model <- lm(formula, data = training_data)
      }

      # Make predictions
      predictions <- predict(model, newdata = testing_data)

      # Calculate relative error
      actual <- testing_data[[col]]
      rel_error <- sum(actual - predictions) / sum(predictions)
      errors[col] <- rel_error
    }

    # Store errors for this dataframe
    list_errors[[i]] <- errors
  }

  # Convert list of errors to a data frame
  error_df <- do.call(rbind, list_errors)
  rownames(error_df) <- paste("Fold", seq_along(df_list))

  # Return the data frame of errors
  return(error_df)
}


