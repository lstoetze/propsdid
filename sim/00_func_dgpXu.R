# Own attempt to implement simulation from Xu Generalized Synthetic Control Method 
dgp_xu <- function(N = 100, T = 10, T0 = 8, N_tr = 50, w = 0.5, true_att=c(1,1)) {
  # Load necessary library
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package 'MASS' is required but not installed.")
  }
  
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Package 'reshape2' is required but not installed.")
  }
  
  if(length(true_att) != T-T0){
    stop("Porived true Atts need to be length of treatment periods")
  }
  
  # Generate factors
  f <- MASS::mvrnorm(n = T, mu = c(0, 0), Sigma = diag(2))
  
  # Treatment indicator
  D <- matrix(0, nrow = N, ncol = T)
  D[1:N_tr, (T0+1):T] <- 1
  
  # Factor loadings and unit fixed effects
  lambda <- matrix(runif(N * 2, min = -sqrt(3), max = sqrt(3)), nrow = N)
  lambda[1:N_tr, ] <- matrix(runif(N_tr * 2, min = sqrt(3) - 2*w*sqrt(3), max = 3*sqrt(3) - 2*w*sqrt(3)), nrow = N_tr)
  alpha <- runif(N, min = -sqrt(3), max = sqrt(3))
  alpha[1:N_tr] <- runif(N_tr, min = sqrt(3) - 2*w*sqrt(3), max = sqrt(3)*sqrt(3) - 2*w*sqrt(3))
  
  # Time fixed effects
  xi <- rnorm(T, mean = 0, sd = 1)
  
  # Simulate covariates, errors, and treatment effect
  x <- array(dim = c(N, T, 2))
  eta <- array(rnorm(N * T * 2), dim = c(N, T, 2))
  epsilon <- matrix(rnorm(N * T), nrow = N)
  
  # Treatment effect over time
  delta <- matrix(0, nrow = N, ncol = T)
  if (N_tr > 0) {
    for(t in 1:length(t_period)){
      delta[1:N_tr, (T0+1):T] <- MASS::mvrnorm(n=N_tr,mu=true_att, Sigma=diag(length(true_att)))
    }
  }
  
  # Generate observed covariates based on factor loadings and factors
  for (i in 1:N) {
    for (t in 1:T) {
      x[i, t, ] <- 1 + c(lambda[i, ] %*% f[t, ]) + lambda[i, 1] + lambda[i, 2] + f[t, 1] + f[t, 2] + eta[i, t, ]
    }
  }
  
  # Generate outcome variable Y
  Y <- matrix(0, nrow = N, ncol = T)
  for (i in 1:N) {
    for (t in 1:T) {
      Y[i, t] <- D[i, t] * delta[i, t] + x[i, t, 1] * 1 + x[i, t, 2] * 3 + lambda[i, ] %*% f[t, ] + alpha[i] + xi[t] + 5 + epsilon[i, t]
    }
  }
  
  # Long format
  dat <- reshape2::melt(Y,value.name = "y")
  colnames(dat)[1:2] <- c("i","t")
  dat$d <- ifelse(dat$t > T0 & dat$i <= N_tr,1,0)
  dat$treated <- ifelse(dat$i <= N_tr,1,0)
  
  # Return a list containing the outcome matrix Y, treatment indicator D, and covariates x
  return(list(dat=dat, Y = Y, D = D, x = x))
}

# Example usage
set.seed(123) # For reproducibility
result <- dgp_xu(N = 100, T = 10, T0 = 8, N_tr = 50, w = 0.5, true_att=c(1,2))


library(propsdid)

panel_dat <- panel.matrices(result$dat)
res_did <- sc_estimate(panel_dat$Y,panel_dat$N0, panel_dat$T0,
            method="did")

plot(res_did)
res_sdid <-  sc_estimate(panel_dat$Y,panel_dat$N0, panel_dat$T0,
            method="sdid")


plot(res_sdid)


# Xu Generalized Synthetic Control Method function from Dataverse 
simulate<-function(Ntr, Nco, T0, p, r, m=0, w=1, D.sd=1, att=c(1:10),
                   beta=NULL, mu=0, fixF=FALSE, fixL=FALSE, fsize=1,
                   FE=0, seed=NULL,unif=FALSE,AR1=0) {
  
  ## p, number of covariates (p=0, no covariates)
  ## Ntr, Nco, N
  ## T0, T
  ## r
  ## m: number of Z
  ## overlap w = [0,1], w==1 means complete overlap
  ## D.sd: heterogeneity
  ## beta: true coefficients for X
  ## att: average treatment effect
  ## fsize: relative importance of factors vs. covariates
  ## trend: importance of a linear time trend 
  ## FE: to include unit and time fixed effects
  ## fixF: factors as given
  ## fixL: loadings as given
  ## AR1: Autoregressive(1) coefficient
  
  if (is.null(seed)==FALSE) {
    set.seed(seed)
  }
  N<-Ntr+Nco
  T<-T0+length(att)
  
  
  Tr<-1:Ntr   # treatment
  Co<-(Ntr+1):N # control
  pre<-1:T0
  pst<-c(1:T)[-pre]
  
  ## ###########################
  ## Data generating process
  ## ###########################
  
  rr<-m + r # observed and unobserved
  
  ## loadings: get 10 (for the construction of X), use the first 1:r columns
  ss<-sqrt(3) # to ensure variance =1
  if (rr > 0) {
    if (fixL==FALSE) {
      lambda<-matrix(runif(N*rr,min=-ss,max=ss),N,rr)  # loadings     
    } else {
      lambda<-L.source[c(c(1:Ntr),c(501:(500+Nco))),1:rr]
    } 
    lambda[1:Ntr,] <- lambda[1:Ntr,]+(1-w)*2*ss # overlap of loadings, w determines the shift of the uniform distribution
  }
  
  ## factors
  if (fixF==FALSE) { # F not given
    factor<-matrix(rnorm(T*rr),T,rr)   # factors
  } else {
    if (unif==FALSE) {
      factor<-F.source[(dim(F.source)[1]-T+1):dim(F.source)[1],1:rr]
      # always use the last T rows, first rr columns
    }  else { # uniform distribution 
      factor<-F.u.source[(dim(F.u.source)[1]-T+1):dim(F.u.source)[1],1:rr] 
    }
  } 
  
  ## fixed effects
  if (FE==1) {
    ## unit fixed effects
    if (fixL==FALSE) {
      alpha<-runif(N,min=-ss,max=ss)
    } else {
      alpha <- L.source[c(c(1:Ntr),c(501:(500+Nco))),20] # the last column
    }
    alpha[1:Ntr]<-alpha[1:Ntr]+(1-w)*2*ss    
    ## time fixed effects
    if (fixF==FALSE) {
      xi<-rnorm(T,0,1) 
    } else {
      if (unif==FALSE) {
        xi<-F.source[(dim(F.source)[1]-T+1):dim(F.source)[1],20] # the 20th (last) column
      }  else {
        xi<-F.u.source[(dim(F.u.source)[1]-T+1):dim(F.u.source)[1],20] # the 20th (last) column
      }
      
    }  
  }
  
  ## error
  e <- matrix(rnorm(T*N),T,N) # disturbances
  
  ## time varying covariates: always generated by the same first two factors
  truebeta<-beta
  if (p!=0) {
    X<-array(0,dim=c(T,N,p))    # regressor matrix, must be T by N  by  p 
    for (j in 1:p) {
      X[,,j] <- matrix(rnorm(T*N),T,N)  +
        0.5 * factor[,1:2] %*% t(lambda[,1:2]) +
        0.25 * matrix(1,T,2) %*% t(lambda[,1:2]) +
        0.25 * factor[,1:2] %*% matrix(1,2,N)+1
    }
  }  
  
  ## treatment assignment
  D<-cbind(rbind(matrix(0,T0,Ntr),matrix(1,(T-T0),Ntr)),matrix(0,T,Nco)) 
  
  ## the treatment effect
  eff<-matrix(c(rep(0,T0),att),T,N)+rbind(matrix(0,T0,N),matrix(rnorm((T-T0)*N,0,D.sd),(T-T0),N))
  
  ## outcome variable
  Y0<- e  +  matrix(mu,T,N) # error + grand mean  
  if (r>0) {
    Y0 <- Y0 + fsize*factor%*%t(lambda)
  } 
  if (FE==1) {
    Y0 <- Y0 + matrix(alpha,T,N,byrow=TRUE) + matrix(xi,T,N,byrow=FALSE)
  }
  if (p!=0) { # covariates
    for (k in 1:p) {
      Y0<-Y0+X[,,k]*truebeta[k]
    }
  }
  Y1 <- Y0 + eff
  if (AR1>0) {
    for (t in 2:T) {
      Y0[t,]<-Y0[(t-1),]*AR1 + Y0[t,]
      Y1[t,]<-Y1[(t-1),]*AR1 + Y1[t,]
    }
    eff.acc<-Y1-Y0 # accumulative effect, T*N matrix
  }  
  Y<-(matrix(1,T,N)-D)*Y0+D*Y1 
  if (AR1>0) {
    Y.lag<-matrix(NA,T,N); Y.lag[2:T,]<-Y[1:(T-1),]
  }
  
  ## substract error
  Y.bar <- Y - e
  
  ## panel structure
  panel<-as.data.frame(cbind(rep(101:(100+N),each=T),rep(1:T,N),
                             c(Y),c(Y0),c(Y1),c(D),c(eff),c(e),c(Y.bar),
                             rep(mu,T*N),rep(1,T*N),
                             c(rep(1,T*Ntr),rep(0,T*Nco))))
  cname<-c("id","time","Y","Y0","Y1","D","eff","error","Ybar","mu","X0","treat")
  if (p!=0) {
    for (i in 1:p) {
      panel<-cbind(panel,c(X[,,i]))
      cname<-c(cname,paste("X",i,sep=""))
    }
  }
  if (rr > 0) {
    for (i in 1:rr) {
      panel<-cbind(panel,rep(factor[,i],N))
      cname<-c(cname,paste("F",i,sep=""))
    }
  }
  if (m > 0) {
    for (i in 1:m) {
      panel<-cbind(panel,rep(lambda[,i],each=T))
      cname<-c(cname,paste("Z",i,sep=""))
    }
  }
  if (r > 0) {
    for (i in 1:r) {
      panel<-cbind(panel,rep(lambda[,(m+i)],each=T))
      cname<-c(cname,paste("L",i,sep=""))
    }
  } 
  if (FE==1) {
    panel<-cbind(panel,rep(alpha,each=T),rep(xi,N))
    cname<-c(cname,"alpha","xi")
  }  
  if (AR1>0) {
    panel<-cbind(panel,c(Y.lag),c(eff.acc))
    cname<-c(cname,"Y_lag","eff_acc")
  } 
  colnames(panel)<-cname
  return(panel)
}


r=2
w=0.4 # 80% overlap
T0 <-8
Nco<-40
Ntr<-20

panel<-simulate(Ntr=Ntr,Nco=Nco,T0=T0,p=0,r=2,m=0,w=w,D.sd=1,beta=c(1,3),AR1=0.2,
                mu=5,att=c(2),fsize=1,FE=1,fixF=FALSE, fixL=FALSE)

library(tidyverse)
panel_est <- panel %>% 
  mutate(d = ifelse(treat ==1 & time > T0,1,0)) %>%
  select(id,time,Y,d) %>%
  as.data.frame() %>%
  panel.matrices(.,unit = "id",time = "time",outcome = "Y",treatment = "d")

res_did <- sc_estimate(panel_est$Y,panel_est$N0, panel_est$T0,
            method="did")
  
res_sdid <- sc_estimate(panel_est$Y,panel_est$N0, panel_est$T0,
            method="sdid")

plot(res_did)
