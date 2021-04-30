impute_missing <- function(data,n_imputations,
                           treated_column_name, outcome_column_name,
                           impute_with_treatment, impute_with_outcome) {

  treatment_ind <- which(colnames(data) == treated_column_name)
  outcome_ind <- which(colnames(data) == outcome_column_name)

  pred_mat <- matrix(1, nrow = ncol(data), ncol = ncol(data))
  diag(pred_mat) <- 0

  if (!impute_with_treatment) {
    pred_mat[, treatment_ind] <- 0
  }
  if (!impute_with_outcome) {
    pred_mat[, outcome_ind] <- 0
  }

  pred_mat[c(treatment_ind, outcome_ind), ] <- 0
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("The bart package must be installed")
  }
  mice_out <-
    mice::mice(data, m = n_imputations,
               predictorMatrix = pred_mat, printFlag = FALSE)

  imputed_data <- mice::complete(mice_out, action = 'all')
  imputed_data<-data.frame(imputed_data)
  colnames(imputed_data) <- colnames(data)
  return(imputed_data)
}

handle_missing <-
  function(data,name,missing_data,
           treated_column_name, outcome_column_name,
           impute_with_treatment, impute_with_outcome) {

    to_drop_data <- is.na(data[[outcome_column_name]]) |
      is.na(data[[treated_column_name]])

    if (any(to_drop_data)) {
      message(paste("Found missingness in ", name, " in treatment and/or outcome;. Corresponding rows will be dropped."))
      data <- data.frame(data[!to_drop_data, ])
    }

    if (all(to_drop_data)) {
      stop(paste("Dropping all rows in ",name," due to missingnessin treatment and/or outcome."))
    }

    if (missing_data == 'none') {
      is_missing <- FALSE
      if (sum(is.na(data)) > 0) {
        stop(paste("Found missingness in ", name, " but was told to assume there was none.Please either change `missing_data` or supply ", name, " without missingness."))
      }
    }
    else if (missing_data == 'drop') {
      data <- data[complete.cases(data), ]
    }
    else if (missing_data == 'impute') {
      is_missing <- FALSE
      if (sum(is.na(data)) > 0) {
        message(paste("Starting imputation of ", name), appendLF = FALSE)
        data <- impute_missing(data,  1,
                               treated_column_name, outcome_column_name,
                               impute_with_treatment, impute_with_outcome)
        message(paste("Finished imputation of ", name), appendLF = FALSE)
      }
      else {
        message('No missing data found; skipping imputation.')
      }
    }

    rownames(data) <- NULL
    return(data)
  }
