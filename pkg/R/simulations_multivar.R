#' Simulates panel data based on a latent factor model for proportional data
#'
#' @param N Integer, the total number of units in the study.
#' @param T Integer, the total number of time periods.
#' @param R Integer, the dimension of latent factors affecting both units and time periods.
#' @param tau Numeric, the true treatment effect to be simulated across treated units.
#' @param treated_n Integer, the number of units to be treated from the treatment start time onwards.
#' @param treat_t Integer, the time point at which treatment starts, with all prior periods being pre-treatment.
#' @param additive Logical, indicating whether the systematic component should be additive (`TRUE`) or multiplicative (`FALSE`).
#' @param ratiosd_GU Numeric, ratio between standard deviation of unit factors and time factors
#' @return A list containing:
#'         - `dat`: a data frame in long format that can be passed on to panel.array function
#'         - `Y`: the outcome matrix with dimensions N x T, where outcomes for each unit and time period are simulated.
#'         - `W`: the treatment assignment matrix with dimensions N x T, indicating treatment status for each unit and period based on the predefined logic.
#'         - `L`: the systematic component matrix derived from latent factors, affecting the outcome.
#'         - `E`: the idiosyncratic error matrix, representing random effects not captured by the systematic component.
#'         Additional components such as `Gamma` (latent unit factors) and `Upsilon` (latent time factors) are also returned for further analysis.
#'
#' @examples
#' simulated_data <- simulateDGP_multi(N = 100, T = 10, R = 2, tau = 1.5, treated_n = 50, treat_t = 5, additive = FALSE)
#'
#' @export
#' @importFrom stats rnorm matrix
#'

simulateDGP_multi <- function(N = 50, T = 10, R = 2, K=3, 
                              tau = c(1.5,-1.5,0,0), 
                              treated_n = 25, treat_t = 5, 
                              additive = FALSE,
                              ratiosd_GU = 1) {
  
  # Generate Latent Factors
  Gamma <- matrix(rnorm(N * R, sd=ratiosd_GU), ncol = R) # Unit factors
  Upsilon <- matrix(rnorm(T * R), ncol = R) # Time factors
  lambda <-  rnorm(K) # factor loadings
  
  # Choose systematic component model in terms of latent factors
  if(additive) {
    
    L <- matrix(NA, ncol=T, nrow=N)
    
    for(t in 1:T){
      for(i in 1:N){
        L[i,t] <- Gamma[i,] %*% Upsilon[t,]
      }
    }
    
  } else {
    L <- Gamma %*% t(Upsilon) # Multiplicative form
  }
  
  
  # Generate treatment assignment based on L and reorder L
  # such that first control than treatment units
  W_ind <- randomize_treatment_based_on_L(L = L, N = N, N1 = treated_n, lambda = 1) # Ensure lambda or adjust function as needed
  L <- rbind(L[W_ind==1,], L[W_ind==0,]) 

  # From treat_t onwards, assign treatment to selected units
  W <- matrix(0, ncol=T, nrow=N)
  W[1:treated_n, (treat_t:T)] <- 1
  
  # Define outcome array
  Y <- array(NA,dim=c(N,T,K))
  E <- array(NA,dim=c(N,T,K))
  
  # Loop over proportions
  for(k in 1:K){
  
    # Proportion specific systemic component (multiplied with factor loading)
    Lk <- lambda[k] * L    
    
    # Simulate Outcome Matrix
    E[,,k] <- matrix(rnorm(N * T), nrow = N, ncol=T) # Idiosyncratic errors
    Y[,,k] <- exp(L + tau[k] * W + E[,,k]) # Outcome matrix
  
  }
  
  # Proportions 
  Ysum <- rowSums(Y, dims = 2)
  for(k in 1:K){
    for(i in 1:N){
      for(t in 1:T){
        Y[i,t,k] <- Y[i,t,k]/Ysum[i,t]
  }}}
  

  # Long format
  dat <- reshape2::melt(Y,value.name = "y")
  colnames(dat)[1:3] <- c("i","t","k")
  dat$d <- ifelse(dat$t >= treat_t & dat$i <= treated_n,1,0)
  dat$treated <- ifelse(dat$i <= treated_n,1,0)

  
  # Return the simulated data and parameters
  list(dat=dat,  Y = Y, W = W, L = L, E = E, Gamma = Gamma, Upsilon = Upsilon, tau = tau)
}


#' Randomize Treatment Assignment Based on Systematic Component
#'
#' This function assigns treatment to units based on a systematic component \(L\) with modulated strength.
#' It calculates probabilities of treatment assignment using a logistic function, where the influence
#' of \(L\) on these probabilities is adjusted by a strength parameter (\(\lambda\)). The function ensures
#' that the number of treated units does not exceed a specified cap (N1), and if the initial assignment
#' results in no treated units, it assigns treatment to at least one unit.
#'
#' @param L The matrix representing the systematic component influencing treatment assignment, where
#'        each row corresponds to a unit and columns represent different characteristics or time periods.
#' @param N The total number of units to consider for treatment assignment.
#' @param N1 The maximum number of units that can be assigned to treatment.
#' @param lambda The strength parameter that modulates the influence of the systematic component \(L\)
#'        on the probability of treatment assignment. A higher value of \(\lambda\) results in a stronger
#'        influence. Default is 1.
#'
#' @return A binary vector of length N, with ones indicating assignment to treatment and zeros indicating
#'         control. The number of ones does not exceed N1, ensuring the cap on treated units is respected.
#'
#' @examples
#' N <- 50  # Number of units
#' N1 <- 25  # Maximum number of treated units
#' lambda <- 2  # Strength of influence
#' # Assuming L is a previously defined matrix representing the systematic component
#' treatment_assignment <- randomize_treatment_based_on_L(L, N, N1, lambda)
#' 
#' @export
#' @importFrom stats rbinom
#' @importFrom base matrix sample
randomize_treatment_based_on_L <- function(L, N, N1, lambda = 1) {
  # Compute a measure from L to influence treatment probabilities
  L_measure <- rowSums(L) # Example: row sums of L
  L_scaled <- scale(L_measure) # Standardize
  
  # Calculte aim pi and use log-odds for constant
  aim_p <- N1/N
  const <- log(aim_p/(1-aim_p))
  
  # Transform the measure into probabilities using a logistic function with strength parameter lambda
  pi <- 1 / (1 + exp(- const -lambda * L_scaled))
  
  # Simulate treatment assignment based on computed probabilities
  assignment_sim <- rbinom(N, 1, pi)
  
  # Adjusting the number of treated units to meet the cap
  index_as <- which(assignment_sim == 1)
  
  if (sum(assignment_sim) > N1){
    index_pert <- sample(index_as,N1)
    assignment_sim <- rep(0,N)
    assignment_sim[index_pert] <- 1
  } else if (sum(assignment_sim) < N1) {
    index_cont <- 1:N
    index_as <- index_cont[-index_as]
    index_pert <- sample(index_as,(N-N1))
    assignment_sim <- rep(1,N)
    assignment_sim[index_pert] <- 0
  }  else if (sum(assignment_sim) == 0){
    index_pert <- sample(1:N,N1)
    assignment_sim <- rep(0,N)
    assignment_sim[index_pert] <- 1
  }
  
  
  return(assignment_sim)
}


