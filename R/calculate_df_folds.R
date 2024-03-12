#' Split a given data frame into folds for building confidence intervals
#'
#' A detailed description of what the function does.
#'
#' @param df The data frame to be used to split into folds.
#' @param train Number of training weeks.
#' @param test Number of test weeks.
#' @return A list of data frames representing the individual folds with training and test weeks.
#' @export
#' @examples
#' # Examples of how to use the function:
#' data <- data.frame(week = 1:10, sales = runif(10, 100, 1000))
#' result <- calculate_df_folds(data, 2, 2)
#' result

calculate_df_folds <- function(df, train, test) {
  folds <- list()
  n <- nrow(df)
  fold_size <- train + test  # Total size of each fold

  start <- n - fold_size + 1
  if (start < 1) start <- 1

  while (start > 0) {
    end <- start + fold_size - 1
    if (end > n) end <- n
    fold_df <- df[start:end, ]

    # Add an indicator column
    fold_df$Indicator <- c(rep("test", min(test, nrow(fold_df))),
                           rep("train", max(0, nrow(fold_df) - test)))

    folds[[length(folds) + 1]] <- fold_df

    start <- start - test  # Move to the next fold by the size of the test set
    if (start < 1 && length(folds) > 0) break
  }

  return(rev(folds))
}
