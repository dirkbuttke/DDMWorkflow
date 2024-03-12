#' Split a given data frame into folds for building confidence intervals
#'
#' A detailed description of what the function does.
#'
#' @param df The data frame to be used to split into folds.
#' @param train Number of training weeks.
#' @param test Number of test weeks.
#' @return Test.
#' @export
#' @examples
#' # Examples of how to use the function:
#' result <- your_function_name(sample_param1, sample_param2)

calculate_df_folds <- function(df, train, test) {
  folds <- list()
  n <- nrow(df)
  fold_size <- train + test  # Total size of each fold

  start <- n - fold_size + 1
  if (start < 1) start <- 1

  while (start > 0) {
    end <- start + fold_size - 1
    if (end > n) end <- n
    folds[[length(folds) + 1]] <- df[start:end, ]

    start <- start - test  # Move to the next fold by the size of the test set
    if (start < 1 && length(folds) > 0) break
  }

  return(rev(folds))
}
