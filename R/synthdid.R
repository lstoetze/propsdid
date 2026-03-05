#' A function mapping a numeric vector to a (presumably sparser) numeric vector of the same shape to
#' be passed onto synthdid_estimate.
#' @param v a vector
sparsify_function = function(v) { v[v <= max(v)/4] = 0; v/sum(v) }

#' Computes the synthetic diff-in-diff estimate for an average treatment effect on a treated block.
#'
#' See 'Synthetic Difference in Differences' by Arkhangelsky et al. This implements Algorithm 1.
#' @param Y the observation matrix.
#' @param N0 the number of control units (N_co in the paper). Rows 1-N0 of Y correspond to the control units.
#' @param T0 the number of pre-treatment time steps (T_pre in the paper). Columns 1-T0 of Y correspond to pre-treatment time steps.
#' @param X an optional 3-D array of time-varying covariates. Shape should be N X T X C for C covariates.
#' @param noise.level, an estimate of the noise standard deviation sigma. Defaults to the standard deviation of first differences of Y.
#' @param eta.omega  determines the tuning parameter zeta.omega = eta.omega * noise.level. Defaults to the value (N_tr T_post)^(1/4).
#' @param eta.lambda analogous for lambda.  Defaults to an 'infinitesimal' value 1e-6.
#' @param zeta.omega if passed, overrides the default zeta.omega = eta.omega * noise.level. Deprecated.
#' @param zeta.lambda analogous for lambda.
#' @param omega.intercept Binary. Use an intercept when estimating omega.
#' @param lambda.intercept Binary. Use an intercept when estimating lambda.
#' @param weights a list with fields lambda and omega. If non-null weights$lambda is passed,
#'        we use them instead of estimating lambda weights. Same for weights$omega.
#' @param update.omega If true, solve for omega using the passed value of weights$omega only as an initialization.
#'        If false, use it exactly as passed. Defaults to false if a non-null value of weights$omega is passed.
#' @param update.lambda  Analogous.
#' @param min.decrease Tunes a stopping criterion for our weight estimator. Stop after an iteration results in a decrease
#' 		        in penalized MSE smaller than min.decrease^2.
#' @param max.iter A fallback stopping criterion for our weight estimator. Stop after this number of iterations.
#' @param sparsify A function mapping a numeric vector to a (presumably sparser) numeric vector of the same shape, which must sum to one.
#'                  If not null, we try to estimate sparse weights via a second round of Frank-Wolfe optimization
#'                  initialized at sparsify( the solution to the first round ).
#' @param max.iter.pre.sparsify Analogous to max.iter, but for the pre-sparsification first-round of optimization.
#'     		                Not used if sparsify=NULL.
#' @return An average treatment effect estimate with 'weights' and 'setup' attached as attributes.
#'         'weights' contains the estimated weights lambda and omega and corresponding intercepts,
#'         as well as regression coefficients beta if X is passed.
#'         'setup' is a list describing the problem passed in: Y, N0, T0, X.
#' @export synthdid_estimate
synthdid_estimate <- function(Y, N0, T0, X = array(dim = c(dim(Y), 0)),
                              noise.level = sd(apply(Y[1:N0,1:T0], 1, diff)),
                              eta.omega = ((nrow(Y)-N0)*(ncol(Y)-T0))^(1/4), eta.lambda = 1e-6,
                              zeta.omega  = eta.omega  * noise.level,  zeta.lambda = eta.lambda * noise.level,
                              omega.intercept = TRUE, lambda.intercept = TRUE,
                              weights = list(omega = NULL, lambda = NULL),
                              update.omega = is.null(weights$omega), update.lambda = is.null(weights$lambda),
                              min.decrease = 1e-5 * noise.level, max.iter = 1e4,
                              sparsify = sparsify_function,
                              max.iter.pre.sparsify = 100) {
  stopifnot(nrow(Y) > N0, ncol(Y) > T0, length(dim(X)) %in% c(2, 3), dim(X)[1:2] == dim(Y), is.list(weights),
            is.null(weights$lambda) || length(weights$lambda) == T0, is.null(weights$omega) || length(weights$omega) == N0,
            !is.null(weights$lambda) || update.lambda, !is.null(weights$omega) || update.omega)
  if (length(dim(X)) == 2) { dim(X) = c(dim(X), 1) }
  if (is.null(sparsify)) { max.iter.pre.sparsify = max.iter }
  N1 = nrow(Y) - N0
  T1 = ncol(Y) - T0

  if (dim(X)[3] == 0) {
    weights$vals = NULL
    weights$lambda.vals = NULL
    weights$omega.vals = NULL
    if (update.lambda) {
      Yc = collapsed.form(Y, N0, T0)
      lambda.opt = sc.weight.fw(Yc[1:N0, ], zeta = zeta.lambda, intercept = lambda.intercept, lambda=weights$lambda,
                                min.decrease = min.decrease, max.iter = max.iter.pre.sparsify)
      if(!is.null(sparsify)) {
        lambda.opt = sc.weight.fw(Yc[1:N0, ], zeta = zeta.lambda, intercept = lambda.intercept, lambda=sparsify(lambda.opt$lambda),
                                  min.decrease = min.decrease, max.iter = max.iter)
      }
      weights$lambda = lambda.opt$lambda
      weights$lambda.vals = lambda.opt$vals
      weights$vals = lambda.opt$vals
    }
    if (update.omega) {
      Yc = collapsed.form(Y, N0, T0)
      omega.opt = sc.weight.fw(t(Yc[, 1:T0]), zeta = zeta.omega, intercept = omega.intercept, lambda=weights$omega,
                               min.decrease = min.decrease, max.iter = max.iter.pre.sparsify)
      if(!is.null(sparsify)) {
        omega.opt = sc.weight.fw(t(Yc[, 1:T0]), zeta = zeta.omega, intercept = omega.intercept, lambda=sparsify(omega.opt$lambda),
                                 min.decrease = min.decrease, max.iter = max.iter)
      }
      weights$omega = omega.opt$lambda
      weights$omega.vals = omega.opt$vals
      if (is.null(weights$vals)) { weights$vals = omega.opt$vals }
      else { weights$vals = pairwise.sum.decreasing(weights$vals, omega.opt$vals) }
    }
  } else {
    Yc = collapsed.form(Y, N0, T0)
    Xc = apply(X, 3, function(Xi) { collapsed.form(Xi, N0, T0) })
    dim(Xc) = c(dim(Yc), dim(X)[3])
    weights = sc.weight.fw.covariates(Yc, Xc, zeta.lambda = zeta.lambda, zeta.omega = zeta.omega,
                                      lambda.intercept = lambda.intercept, omega.intercept = omega.intercept,
                                      min.decrease = min.decrease, max.iter = max.iter,
                                      lambda = weights$lambda, omega = weights$omega, update.lambda = update.lambda, update.omega = update.omega)
  }

  X.beta = contract3(X, weights$beta)
  estimate = t(c(-weights$omega, rep(1 / N1, N1))) %*% (Y - X.beta) %*% c(-weights$lambda, rep(1 / T1, T1))

  class(estimate) = 'synthdid_estimate'
  attr(estimate, 'estimator') = "synthdid_estimate"
  attr(estimate, 'weights') = weights
  attr(estimate, 'setup') = list(Y = Y, X = X, N0 = N0, T0 = T0)
  attr(estimate, 'opts') = list(zeta.omega = zeta.omega, zeta.lambda = zeta.lambda,
                                omega.intercept = omega.intercept, lambda.intercept = lambda.intercept,
                                update.omega = update.omega, update.lambda = update.lambda,
                                min.decrease = min.decrease, max.iter=max.iter)
  return(estimate)
}

#' Computes the Synthetic Difference‑in‑Differences and Synthethic Control estimator for 
#' multivariate proportional outcomes when treatment occurs on a block.
#'
#' This function implements the common‑weights SDID estimator described in 
#' Stoetzer and Bogatyrev, "Estimating Treatment Effects on Proportions with Synthetic Controls". 
#' It constructs a synthetic control using a shared set of unit and time weights 
#' across all outcomes, ensuring that the counterfactual respects the 
#' compositional structure of the data.
#' @param Y the observations 3d-array, with N observations, T Time-points, and K proportions.
#' @param N0 the number of control units (N_co in the paper). Rows 1-N0 of Y correspond to the control units.
#' @param T0 the number of pre-treatment time steps (T_pre in the paper). Columns 1-T0 of Y correspond to pre-treatment time steps.
#' @param X an optional 3-D array of time-varying covariates. Shape should be N X T X C for C covariates.
#' @param noise.level, an estimate of the noise standard deviation sigma. Defaults to the standard deviation of first differences of Y.
#' @param eta.omega  determines the tuning parameter zeta.omega = eta.omega * noise.level. Defaults to the value (N_tr T_post K)^(1/4).
#' @param eta.lambda analogous for lambda.  Defaults to an 'infinitesimal' value 1e-6.
#' @param zeta.omega if passed, overrides the default zeta.omega = eta.omega * noise.level. Deprecated.
#' @param zeta.lambda analogous for lambda.
#' @param omega.intercept Binary. Use an intercept when estimating omega.
#' @param lambda.intercept Binary. Use an intercept when estimating lambda.
#' @param weights a list with fields lambda and omega. If non-null weights$lambda is passed,
#'        we use them instead of estimating lambda weights. Same for weights$omega.
#' @param update.omega If true, solve for omega using the passed value of weights$omega only as an initialization.
#'        If false, use it exactly as passed. Defaults to false if a non-null value of weights$omega is passed.
#' @param update.lambda  Analogous.
#' @param min.decrease Tunes a stopping criterion for our weight estimator. Stop after an iteration results in a decrease
#' 		        in penalized MSE smaller than min.decrease^2.
#' @param max.iter A fallback stopping criterion for our weight estimator. Stop after this number of iterations.
#' @param sparsify A function mapping a numeric vector to a (presumably sparser) numeric vector of the same shape, which must sum to one.
#'                  If not null, we try to estimate sparse weights via a second round of Frank-Wolfe optimization
#'                  initialized at sparsify( the solution to the first round ).
#' @param max.iter.pre.sparsify Analogous to max.iter, but for the pre-sparsification first-round of optimization.
#'     		                Not used if sparsify=NULL.
#' @return An average treatment effect estimate with 'weights' and 'setup' attached as attributes.
#'         'weights' contains the estimated weights lambda and omega and corresponding intercepts,
#'         as well as regression coefficients beta if X is passed.
#'         'setup' is a list describing the problem passed in: Y, N0, T0, X.
#' @export synthdid_estimate
synthdid_estimate_multivar <- function(Y, N0, T0,
                              noise.level = sd(apply(Y[1:N0,1:T0,], 1, diff)),
                              eta.omega = ((dim(Y)[1]-N0)*(dim(Y)[2]-T0)*dim(Y)[3])^(1/4), eta.lambda = 1e-6,
                              zeta.omega  = eta.omega  * noise.level,  zeta.lambda = eta.lambda * noise.level,
                              omega.intercept = TRUE, lambda.intercept = TRUE,
                              weights = list(omega = NULL, lambda = NULL),
                              update.omega = is.null(weights$omega), update.lambda = is.null(weights$lambda),
                              min.decrease = 1e-5 * noise.level, max.iter = 1e4,
                              sparsify = sparsify_function,
                              max.iter.pre.sparsify = 100) {

  stopifnot(length(dim(Y))==3,
            dim(Y)[1] > N0, dim(Y)[2] > T0, is.list(weights),
            is.null(weights$lambda) || length(weights$lambda) == T0,
            is.null(weights$omega) || length(weights$omega) == N0,
            !is.null(weights$lambda) || update.lambda,
            !is.null(weights$omega) || update.omega)

  if (is.null(sparsify)) { max.iter.pre.sparsify = max.iter }

  N1 = dim(Y)[1] - N0
  T1 = dim(Y)[2] - T0
  K = dim(Y)[3]

  weights$lambda.vals = NULL
  weights$omega.vals = NULL

  # Calculate Lambda Weights (Time)
  if (update.lambda) {
    Yc = collapsed.form.3d(Y, N0, T0)
    lambda.opt = sc.weight.fw(Yc[1:N0, , ], zeta = zeta.lambda, intercept = lambda.intercept, lambda=weights$lambda,
                              min.decrease = min.decrease, max.iter = max.iter.pre.sparsify)
    if (!is.null(sparsify)) {
      lambda.opt = sc.weight.fw(Yc[1:N0, , ], zeta = zeta.lambda, intercept = lambda.intercept, lambda=sparsify(lambda.opt$lambda),
                                min.decrease = min.decrease, max.iter = max.iter)
    }
    weights$lambda = lambda.opt$lambda
    weights$lambda.vals = lambda.opt$vals
    weights$vals = lambda.opt$vals
  }

  # Calculate Omega Weights (Units)
  if (update.omega) {
    Yc = collapsed.form.3d(Y, N0, T0)
    Yct = aperm(Yc[, 1:T0, ], c(2, 1, 3))

    omega.opt = sc.weight.fw(Yct, zeta = zeta.omega, intercept = omega.intercept, lambda=weights$omega,
                             min.decrease = min.decrease, max.iter = max.iter.pre.sparsify)
    if (!is.null(sparsify)) {
      omega.opt = sc.weight.fw(Yct, zeta = zeta.omega, intercept = omega.intercept, lambda=sparsify(omega.opt$lambda),
                               min.decrease = min.decrease, max.iter = max.iter)
    }
    weights$omega = omega.opt$lambda
    weights$omega.vals = omega.opt$vals
    if (is.null(weights$vals)) {
      weights$vals = omega.opt$vals
    } else {
      weights$vals = pairwise.sum.decreasing(weights$vals, omega.opt$vals)
    }
  }

  # Estimate Effects
  estimate <- NULL
  for(k in 1:K){
    Yk <- Y[,,k]
    estimate[k] = t(c(-weights$omega, rep(1 / (nrow(Yk) - N0), nrow(Yk) - N0))) %*% Yk %*% c(-weights$lambda, rep(1 / (ncol(Yk) - T0), ncol(Yk) - T0))
  }

  class(estimate) = 'synthdid_estimate_multi'
  attr(estimate, 'estimator') = "synthdid_estimate_multi"
  attr(estimate, 'weights') = weights
  attr(estimate, 'setup') = list(Y = Y, N0 = N0, T0 = T0, K=K)
  attr(estimate, 'opts') = list(zeta.omega = zeta.omega, zeta.lambda = zeta.lambda,
                                omega.intercept = omega.intercept, lambda.intercept = lambda.intercept,
                                update.omega = update.omega, update.lambda = update.lambda,
                                min.decrease = min.decrease, max.iter=max.iter)
  return(estimate)
}

#' Estimates Synthetic Control models with common weights for multivariate 
#' proportional outcomes.
#'
#' This function implements the common‑weights Synthetic Control estimator 
#' introduced in *Estimating Treatment Effects on Proportions with Synthetic 
#' Controls* (Stoetzer & Bogatyrev). It applies the standard SC setup but 
#' enforces a shared set of unit and time weights across all outcomes, ensuring 
#' that the counterfactual respects the compositional structure of the data.
#'
#' @param Y the observation matrix.
#' @param N0 the number of control units. Rows 1-N0 of Y correspond to the control units.
#' @param T0 the number of pre-treatment time steps. Columns 1-T0 of Y correspond to pre-treatment time steps.
#' @param method specifies what method should be used: sdid (synthetic difference in difference), sc (synthetic control), or did (difference in difference)
#' @param prop_dat specifies if outcome is multivariate (TRUE default) or univariate (FALSE)
#' @param ... additional options for synthdid_estimate
#' @return an object like that returned by synthdid_estimate
#' @export sc_estimate
sc_estimate = function(Y, N0, T0,
                       method="did",porp_dat = TRUE,...) {

  if (!method %in% c('did', 'sc', "sdid")) { stop('weight.type must be "sdid", "sc", or "did"') }

  if(method == "did"){
    if(isFALSE(porp_dat)){
      estimate = synthdid_estimate(Y, N0, T0,
                                   weights = list(lambda = rep(1 / T0, T0), omega = rep(1 / N0, N0)), ...)
      attr(estimate, 'method') = "did"
      return(estimate)
    } else {
      estimate = synthdid_estimate_multivar(Y, N0, T0,
                                            weights = list(lambda = rep(1 / T0, T0), omega = rep(1 / N0, N0)), ...)
      attr(estimate, 'method') = "did"
      return(estimate)
    }
  }

  if(method == "sc"){
    if(isFALSE(porp_dat)){
      estimate = synthdid_estimate(Y, N0, T0,eta.omega =  1e-6,
                                   weights = list(lambda = rep(0, T0)), omega.intercept = FALSE, ...)
      attr(estimate, 'method') = "sc"
      return(estimate)
    } else {
      estimate = synthdid_estimate_multivar(Y, N0, T0,eta.omega =  1e-6,
                                            weights = list(lambda = rep(0, T0)), omega.intercept = FALSE, ...)
      attr(estimate, 'method') = "sc"
      return(estimate)
    }
  }


  if(method == "sdid"){
    if(isFALSE(porp_dat)){
      estimate = synthdid_estimate(Y, N0, T0)
      attr(estimate, 'method') = "sdid"
      return(estimate)
    } else {
      estimate = synthdid_estimate_multivar(Y, N0, T0)
      attr(estimate, 'method') = "sdid"
      return(estimate)
    }
  }


}



#' Computes a placebo variant of our estimator using pre-treatment data only
#' @param estimate, as output by synthdid_estimate
#' @param treated.fraction, the fraction of pre-treatment data to use as a placebo treatment period
#'        Defaults to NULL, which indicates that it should be the fraction of post-treatment to pre-treatment data
#' @export synthdid_placebo
synthdid_placebo = function(estimate, treated.fraction = NULL) {
  setup = attr(estimate, 'setup')
  opts = attr(estimate, 'opts')
  weights = attr(estimate, 'weights')
  X.beta = contract3(setup$X, weights$beta)
  estimator = attr(estimate, 'estimator')

  if (is.null(treated.fraction)) { treated.fraction = 1 - setup$T0 / ncol(setup$Y) }
  placebo.T0 = floor(setup$T0 * (1 - treated.fraction))

  do.call(estimator, c(list(Y=setup$Y[, 1:setup$T0], N0=setup$N0, T0=placebo.T0, X=setup$X[, 1:setup$T0,]), opts))
}

#' Outputs the effect curve that was averaged to produce our estimate
#' @param estimate, as output by synthdid_estimate
#' @export synthdid_effect_curve
synthdid_effect_curve = function(estimate) {
  setup = attr(estimate, 'setup')
  weights = attr(estimate, 'weights')
  X.beta = contract3(setup$X, weights$beta)
  N1 = nrow(setup$Y) - setup$N0
  T1 = ncol(setup$Y) - setup$T0

  tau.sc = t(c(-weights$omega, rep(1 / N1, N1))) %*% (setup$Y - X.beta)
  tau.curve = tau.sc[setup$T0 + (1:T1)] - c(tau.sc[1:setup$T0] %*% weights$lambda)
  tau.curve
}
