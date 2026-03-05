#' Format a synthdid object
#' @param x The object to format
#' @param ... Additional arguments (currently ignored).
#' @method format synthdid_estimate
#' @export
format.synthdid_estimate = function(x, ...) {
  info = summary(x, fast=TRUE)
  d = as.list(info$dimensions)
  sprintf('synthdid: %1.3f +- %1.3f. Effective N0/N0 = %1.1f/%d~%1.1f. Effective T0/T0 = %1.1f/%d~%1.1f. N1,T1 = %d,%d.',
    c(x), 1.96*info$se,
    d$N0.effective, d$N0, d$N0.effective/d$N0,
    d$T0.effective, d$T0, d$T0.effective/d$T0,
    d$N1, d$T1)
}


#' Format a synthdid_multi object
#' @param x The object to format
#' @param ... Additional arguments (currently ignored).
#' @method format synthdid_estimate_multi
#' @export
format.synthdid_estimate_multi <- function(x, ...) {
  x <- createEstimateList(x)

  # Initialize a vector to collect formatted strings
  formatted_strings <- c(sprintf('synthdid estimates\n============ \n\n'))

  for (k in 1:length(x)) {
    info <- summary(x[[k]], fast = TRUE)
    d <- as.list(info$dimensions)

    # Collect each formatted string into the vector
    formatted_strings <- c(formatted_strings,
                           sprintf('estimate %d: %1.3f +- %1.3f. \n',
                                   k, c(x[[k]]), 1.96 * info$se))
  }

  formatted_strings <- c(formatted_strings,
                         sprintf('\n Effective N0/N0 = %1.1f/%d~%1.1f. Effective T0/T0 = %1.1f/%d~%1.1f. N1,T1 = %d,%d.',
                                 d$N0.effective, d$N0, d$N0.effective / d$N0,
                                 d$T0.effective, d$T0, d$T0.effective / d$T0,
                                 d$N1, d$T1))



  # Return the collection of formatted strings
  return(formatted_strings)
}

