contract3 = function(X, v) {
  stopifnot(length(dim(X)) == 3, dim(X)[3] == length(v))
  out = array(0, dim = dim(X)[1:2])
  if (length(v) == 0) { return(out) }
  for (ii in 1:length(v)) {
    out = out + v[ii] * X[, , ii]
  }
  return(out)
}

# a Frank-Wolfe step for \\Ax - b||^2 + eta * ||x||^2 with x in unit simplex.
fw.step = function(A, x, b, eta, alpha = NULL) {
  Ax = A %*% x
  half.grad = t(Ax - b) %*% A + eta * x
  i = which.min(half.grad)
  if (!is.null(alpha)) {
    x = x * (1 - alpha)
    x[i] = x[i] + alpha
    return(x)
  } else {
    d.x = -x; d.x[i] = 1 - x[i]
    if (all(d.x == 0)) { return(x) }
    d.err = A[, i] - Ax
    step = -t(c(half.grad)) %*% d.x / (sum(d.err^2) + eta * sum(d.x^2))
    constrained.step = min(1, max(0, step))
    return(x + constrained.step * d.x)
  }
}



# a Frank-Wolfe solver for synthetic control weights using exact line search
sc.weight.fw = function(Y, zeta, intercept = TRUE, lambda = NULL, min.decrease = 1e-3, max.iter = 1000) {
  T0 = dim(Y)[2] - 1
  N0 = dim(Y)[1]
  if (is.null(lambda)) { lambda = rep(1 / T0, T0) }

  # Check if Y is a 3D array or 2D matrix
  if (length(dim(Y)) == 3) {
    K <- dim(Y)[3]
    if (intercept) {
      for (k in 1:K) {
        Y[, , k] <- apply(Y[, , k], 2, function(col) col - mean(col))
      }
    }
    Y <- do.call(rbind, lapply(1:K, function(k) Y[, , k]))
  } else if (length(dim(Y)) == 2 && intercept) {
    Y <- apply(Y, 2, function(col) col - mean(col))
  }

  t = 0
  vals = rep(NA, max.iter)
  A <- Y[, 1:T0, drop = FALSE]
  b <- as.vector(Y[, (T0 + 1)])

  eta = N0 * Re(zeta^2)

  while (t < max.iter && (t < 2 || vals[t - 1] - vals[t] > min.decrease^2)) {
    t = t + 1
    lambda.p = fw.step(A, lambda, b, eta)
    lambda = lambda.p
    err = Y %*% c(lambda, -1)
    vals[t] = Re(zeta^2) * sum(lambda^2) + sum(err^2) / length(b)
  }
  list(lambda = lambda, vals = vals)
}



