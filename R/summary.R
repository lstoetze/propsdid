#' Outputs a table of important synthetic controls and their corresponding weights, sorted by weight.
#' The table is truncated to exclude synthetic controls that do not matter for any estimate ---
#' for each estimate, the truncated controls may have total weight no larger that 1-mass.
#' @param estimates, a list of estimates output by synthdid_estimate. Or a single estimate.
#' @param sort.by, the index of the estimate to sort by. Defaults to 1.
#' @param mass, which controls the length of the table. Defaults to 0.9.
#' @param weight.type, 'omega' for units, 'lambda' for time periods
#' @export synthdid_controls
synthdid_controls = function(estimates, sort.by = 1, mass = .9, weight.type = 'omega') {
  if (class(estimates) == 'synthdid_estimate') { estimates = list(estimates) }
  if (is.null(names(estimates))) { names(estimates) = sprintf('estimate %d', 1:length(estimates)) }
  if (!weight.type %in% c('omega', 'lambda')) { stop('weight.type must be "omega" or "lambda"') }
  weights = do.call(cbind, lapply(estimates, function(est) { attr(est, 'weights')[[weight.type]] }))
  if (is.null(dim(weights))) { dim(weights) = c(length(weights), 1) }

  Y = attr(estimates[[1]], 'setup')$Y
  o = rev(order(weights[, sort.by]))
  tab = weights[o, , drop = FALSE]
  rownames(tab) = if(weight.type == 'omega') { rownames(Y)[o] } else { colnames(Y)[o] }
  colnames(tab) = names(estimates)

  # truncate table to retain a weight sum of at least mass for each unit
  tab.len = max(apply(tab, 2, function(col) { Position(function(x) { x >= mass }, cumsum(col), nomatch=nrow(tab)) }))
  tab[1:tab.len, , drop=FALSE]
}

#' Outputs a table of important synthetic controls and their corresponding weights, sorted by weight.
#' The table is truncated to exclude synthetic controls that do not matter for any estimate ---
#' for each estimate, the truncated controls may have total weight no larger that 1-mass.
#' @param estimates, a list of estimates output by synthdid_estimate. Or a single estimate.
#' @param sort.by, the index of the estimate to sort by. Defaults to 1.
#' @param mass, which controls the length of the table. Defaults to 0.9.
#' @param weight.type, 'omega' for units, 'lambda' for time periods
#' @export synthdid_controls
synthdid_controls = function(estimates, sort.by = 1, mass = .9, weight.type = 'omega') {
  if (class(estimates) == 'synthdid_estimate') { estimates = list(estimates) }
  if (is.null(names(estimates))) { names(estimates) = sprintf('estimate %d', 1:length(estimates)) }
  if (!weight.type %in% c('omega', 'lambda')) { stop('weight.type must be "omega" or "lambda"') }
  weights = do.call(cbind, lapply(estimates, function(est) { attr(est, 'weights')[[weight.type]] }))
  if (is.null(dim(weights))) { dim(weights) = c(length(weights), 1) }

  Y = attr(estimates[[1]], 'setup')$Y
  o = rev(order(weights[, sort.by]))
  tab = weights[o, , drop = FALSE]
  rownames(tab) = if(weight.type == 'omega') { rownames(Y)[o] } else { colnames(Y)[o] }
  colnames(tab) = names(estimates)

  # truncate table to retain a weight sum of at least mass for each unit
  tab.len = max(apply(tab, 2, function(col) { Position(function(x) { x >= mass }, cumsum(col), nomatch=nrow(tab)) }))
  tab[1:tab.len, , drop=FALSE]
}


#' Summarize a synthdid object
#' @param object The object to summarize
#' @param weight.digits The number of digits to use when displaying weights (omega, lambda)
#' @param fast Be fast but less accurate, e.g. jackknife instead of bootstrap se estimate
#' @param ... Additional arguments (currently ignored).
#' @method summary synthdid_estimate
#' @export
summary.synthdid_estimate = function(object, weight.digits=3, fast=FALSE, ...) {
  N0 = attr(object, 'setup')$N0
  T0 = attr(object, 'setup')$T0
  list(estimate = c(object),
    se = sqrt(if(fast) { vcov(object, method = 'jackknife') } else { vcov(object) }),
    controls = round(synthdid_controls(object, weight.type='omega'),  digits=weight.digits),
    periods  = round(synthdid_controls(object, weight.type='lambda'), digits=weight.digits),
    dimensions = c( N1 = nrow(Y(object))-N0, N0 = N0, N0.effective = round(1 / sum(omega(object)^2),  weight.digits),
		    T1 = ncol(Y(object))-T0, T0 = T0, T0.effective = round(1 / sum(lambda(object)^2), weight.digits)))
}

#' Summarize a synthdid_multi object
#' @param object The object to summarize
#' @param weight.digits The number of digits to use when displaying weights (omega, lambda)
#' @param fast Be fast but less accurate, e.g. jackknife instead of bootstrap se estimate
#' @param ... Additional arguments (currently ignored).
#' @method summary synthdid_estimate_multi
#' @export
summary.synthdid_estimate_multi = function(object,fast=FALSE,  ...) {
  N0 = attr(object, 'setup')$N0
  N = dim(attr(object, "setup")$Y)[1]
  T0 = attr(object, 'setup')$T0
  T = dim(attr(object, "setup")$Y)[2]
  K = attr(object, "setup")$K
  list(estimate = c(object),
       se = sqrt(if(fast) { vcov(object, method = 'jackknife') } else { vcov(object, ...) }),
       #controls = round(synthdid_controls(object, weight.type='omega'),  digits=weight.digits),
       #periods  = round(synthdid_controls(object, weight.type='lambda'), digits=weight.digits),
       dimensions = c(K = K, N1 = N-N0, N0 = N0,
                        T1 = T-T0, T0 = T0))
}

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
  
  formatted_strings <- c("synthdid estimates\n============\n")
  
  for (k in 1:length(x)) {
    info <- summary(x[[k]], fast = TRUE)
    d <- as.list(info$dimensions)
    
    formatted_strings <- c(
      formatted_strings,
      sprintf("estimate %d: %1.3f +- %1.3f.",
              k, c(x[[k]]), 1.96 * info$se)
    )
  }
  
  formatted_strings <- c(
    formatted_strings,
    sprintf(
      "\nEffective N0/N0 = %1.1f/%d~%1.1f. Effective T0/T0 = %1.1f/%d~%1.1f. N1,T1 = %d,%d.",
      d$N0.effective, d$N0, d$N0.effective / d$N0,
      d$T0.effective, d$T0, d$T0.effective / d$T0,
      d$N1, d$T1
    )
  )
  
  # Collapse correctly into one string
  paste(formatted_strings, collapse = "\n")
}





#' Print a synthdid object
#' @param x The object to print
#' @param ... Additional arguments (currently ignored).
#' @method print synthdid_estimate
#' @export
print.synthdid_estimate = function(x, ...) { cat(format(x, ...), "\n") }


#' Print a synthdid_multi object
#' @param x The object to print
#' @param ... Additional arguments (currently ignored).
#' @method print synthdid_estimate
#' @export
print.synthdid_estimate_multi = function(x, ...) {
  cat(format(x, ...), "\n")
}







