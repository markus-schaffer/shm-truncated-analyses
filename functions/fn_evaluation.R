# Title: Evaluation function 

# Purpose : This function calculate the normalized root mean square error
# (NRMSE) and the Pearson Correlation coefficient (PCC)

# Author: M. Schaffer
# Contact details: msch@build.aau.dk


eval_criteria <- list(
  nrmse = function(actual, predicted) {
    if (all(is.na(actual)) | sum(actual, na.rm = TRUE) == 0) {
      return(NA_real_)
    } else {
      b <- ((is.na(actual) + is.na(predicted)) == 0)

      ((predicted[b] - actual[b])^2)|>
        mean()|>
        sqrt() / mean(actual[b])
    }
  },
  corr = function(actual, predicted) {
    b <- ((is.na(actual) + is.na(predicted)) == 0)
    if (all(is.na(actual)) | sum(!is.na(actual)) < 2 | sd(actual[b], na.rm = TRUE) == 0 | sd(predicted[b], na.rm = TRUE) == 0) {
      return(NA_real_)
    } else {
      abs(cor(actual[b], predicted[b]))
    }
  }
)
 