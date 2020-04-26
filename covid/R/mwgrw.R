# Helper functions for MWGRW
# Log-likelihood for the i-th subject

f = function(x,yi, Xi, Z, betat, Sigma_gammat){
  
  # Zi is a subset of the columns of Xi, handle the case where Z is dim 1
  Zi = NULL
  if(is.vector(Xi)){
    Zi = Xi[Z]
  }else{
    Zi = Xi[,Z]
  }
  
  # Calculate lambdai
  lambdai = exp(Xi%*%betat + Zi%*%matrix(x, ncol = 1))
  
  # Return log-likelihood for the i-th subject
  return(sum(dpois(yi, lambdai, log = T)) + dmvnorm(x,sigma = Sigma_gammat, log = T))
}

# Proposal function
g.sim = function(Sigma_gammat){
  rmvnorm(1, sigma = Sigma_gammat)
}

# RW density
h.sim = function(){
  rnorm(1,0,0.1)
}

# MH ratio
R = function(xt,x, f, yi, Xi, Z, betat, Sigma_gammat){
  return(exp(f(x, yi, Xi, Z, betat, Sigma_gammat) - f(xt,yi, Xi, Z, betat, Sigma_gammat)))
}

# MWGRW sampler
mwg.rw.sampler = function(yi, Xi, Z, betat, Sigma_gammat, M, prev.gamma.i = NULL){
  
  # Get dimension of gamma
  q = ncol(Sigma_gammat)
  x.indep.chain = matrix(0, M, q)
  
  # Either start fresh with a new draw from g or use the last value of the previous chain
  if(is.null(prev.gamma.i)){
    x.indep.chain[1,] = g.sim(Sigma_gammat)
  }else{
    x.indep.chain[1,] = prev.gamma.i    
  }
  accept = rep(0,q)
  
  for(i in 1:(M-1)){
    
    # Call current value xt
    xt = x.indep.chain[i,]
    
    # Using univariate proposal and using MH at each gibbs step
    for(j in 1:q){  
      x = xt 
      
      # Update one dimension using random walk
      x[j] = x[j] + h.sim()
      
      # Calculate MH ratio
      r = min(R(xt, x, f, yi, Xi, Z, betat, Sigma_gammat),1)
      
      # Accept/Reject based on MH criteria
      keep = rbinom(1, 1, r)
      if(keep == 1){
        x.indep.chain[i+1,] = x
        xt = x
        accept[j] = accept[j] + 1
      }else{
        x.indep.chain[i+1,] = xt
      }
    }
  }
  return(list(gammai = x.indep.chain, ar = accept/M))
}

# E-Step
e.step= function(y, X, Z, ID, betat, Sigma_gammat, M, n, ni, sampler, burn.in = 200,prev.gamma = NULL){
  
  # Initialize Q-function
  Qfunction = 0
  
  # Matrix to hold the chains for each country, used in prediction
  gamma = matrix(0, n*M, ncol(Sigma_gammat))
  
  # Vector to hold offset values Zi %*% gammai, yaug, Xaug in M step
  N = sum(ni)#ni*n
  offset = yaug = rep(0, N*M)
  Xaug = matrix(0, nrow = N*M, ncol = ncol(X))
  
  # Loop over countries
  for(i in 1:n){
    # Get country level data for country i
    yi = y[which(ID == i)]
    Xi = X[which(ID == i),]
    
    # Generate chain for each country, using either previous chain without burn in or
    # start from new chain and remove the burn in. Then save the chain
    if(is.null(prev.gamma)){
      chain = sampler(yi = yi, Xi = Xi,Z = Z,betat = betat, Sigma_gammat = Sigma_gammat, M = M + burn.in)
      gammai = chain$gammai[-c(1:burn.in),]
    }else{
      chain = sampler(yi = yi, Xi = Xi,Z = Z,betat = betat, Sigma_gammat = Sigma_gammat, M = M, prev.gamma = prev.gamma[i,]) 
      gammai = chain$gammai
    }
    
    # Create augmented versions of each vector for easier calctions
    # Handle cases for when Zi has 1 column and when Xi has 1 row
    aug = rep(1:ni[i],M)#rep(1:ni, M)
    yi_aug = yi[aug]
    Xi_aug = NULL
    if(is.vector(Xi)){
      Xi_aug = matrix(Xi,nrow=1)[aug,]
    }else{
      Xi_aug = Xi[aug,]
    }
    Zi_aug = NULL
    Zi = NULL
    if(is.vector(Xi)){
      Zi = Xi[Z]
    }else{
      Zi = Xi[,Z]
    }
    if(is.vector(Zi)){
      Zi_aug = matrix(matrix(Zi,ncol=length(Zi))[aug,], ncol=length(Zi))
    }else{
      Zi_aug = Zi[aug,]
    }
    
    # Create augmented version of gammai, with each country having ni observations
    augg = rep(1:M, each = ni[i])
    gammai_aug = gammai[augg,]
    
    # calculate Q function for country i
    XBaug = Xi_aug%*%betat
    Zgammaaug = rowSums(Zi_aug * gammai_aug)
    lambdai_aug = exp(XBaug + Zgammaaug)
    Qi = sum(dpois(yi_aug, lambda = lambdai_aug, log = T)) + sum(dmvnorm(gammai_aug, sigma = Sigma_gammat, log = T))
    
    # Divide by chain length
    Qi = Qi/M
    Qfunction = Qfunction + Qi
    
    # Calculate the location to store offset, yaug, xaug
    if(i == 1){
      a = 1 
    }else{ 
      a = M*sum(ni[1:i-1]) + 1 
    }
    b = a+M*ni[i]-1
    offset[a:b] = Zgammaaug
    yaug[a:b] = yi_aug
    Xaug[a:b,] = Xi_aug
    
    # Calculate location to store gammai
    a = (i-1)*M + 1
    b = i*M
    gamma[a:b,] = gammai
  }
  
  return(list(Qfunction = Qfunction, gamma = gamma, offset = offset, yaug = yaug, Xaug = Xaug))
}


#' Estimation of a Generalized Linear Mixed Model of COVID-19 data using a Metropolis-within-Gibbs Random Walk 
#' 
#' This function computes the estimates of a Poisson generalized linear mixed model
#' with a log link for COVID-19 data from the Johns Hopkins CSSE. Other data included
#' comes from the World Bank and the Global Health Secuiry Index. The response is the
#' number of new cases, while the predictors include an intercept, time, time^2, GHS score,
#' percentage of population over age 65, and percentage of population living in urban areas.
#' Random effects can be adjusted but is defaulted to intercept and time.
#' 
#' @param beta An intial vector of length 6 to be the starting values of beta for the MCEM algorithm. Default values avaliable.
#' @param Sigma_gamma An intial matrix of size qxq, where q is the dimension of the random effects. Default values are 2x2. Default random effects would require a 2x2 matrix.
#' @param M Chain length for each country in the MCEM algorithmn. Defaults to 1000.
#' @param Z A vector that contains the subset of columns of X to be random effects. Defaults to intercept and time only.
#' @param control A list of control parameters for the MCEM algorithm. tol is the convergence tolerance, and maxit is the maximum number of iterations
#' @param verbose A boolean indicating if output should be printed at each iteration
#'
#' @return A list containing: beta - final estimates of beta, Sigma_gamma - final estimates of Sigma_gamma, gamma - chains from the final iteration of the EM algorithm, and time - time the MCEM algorithm took to run.
#' 
#' 
#' @export
mwgrw <- function(beta = NA, Sigma_gamma = NA, M=1000, Z=c(1,2), control=list(tol=10^-5,maxit=1000), verbose= TRUE){
  # Read in data
  dat = covid2
  # Remove NA data
  dat <- dat %>% mutate(day2 = day^2) %>% drop_na(GHS_Score) %>% drop_na(AgeGEQ65) %>% drop_na(UrbanPop)
  
  # Modify china new_cases day 0 since it was NA previously
  dat[402,5]=548
  dat$ID <- dat %>% group_indices(Country.Region)
  
  # Remove countries with less than 5 cases
  for (i in 1:max(dat$ID)) {
    if (sum(dat$ID==i) < 5) {
      dat<- dat[!(dat$ID==i),]
    }
  }
  dat$ID <- dat %>% group_indices(Country.Region)
  
  # Create unique country list
  order = unique(dat$Country.Region)
  # Number of unique countries
  n = length(order)
  # Assign IDs to each county and generate ni vector, which is the number of observations for each country
  ID = as.numeric()
  ni = as.numeric()
  index = 1
  for(i in 1:length(order)){
    # Generate ID
    count = length(which(dat$Country.Region==order[i]))
    ID[index:(index+count-1)] = rep(i,count)
    index = index + count
    # Generate ni
    ni[i] = count
  }
  # Add intercept
  dat = dat %>% add_column(int = 1)
  # Generate the design and response matricies
  X = unname(as.matrix(dat %>% select(int, day, day2, GHS_Score, AgeGEQ65, UrbanPop)))
  y = unname(as.matrix(dat %>% select(new_cases)))
  # The Z matrix is defined in the parameters
  
  # Assign starting values if not specified
  if(is.na(beta)){
    suppressWarnings(fit.glmm <- summary(glmm1 <- glmer(new_cases ~ day + day2 + GHS_Score + AgeGEQ65 
                                       + UrbanPop + (day | Country.Region), data = dat, family = poisson)))
    beta = as.vector(fit.glmm$coefficients[,1])
  }
  if(is.na(Sigma_gamma)){
    Sigma_gamma = diag(c(12,0.1))
  }

  ## Set initial parameters
  tol = control$tol; maxit = control$maxit; iter = 0
  eps = Inf; qfunction = -10000 # using Qfunction for convergence
  prev.gamma = NULL
  final_beta = beta; final_Sigma_gamma = Sigma_gamma; final_gamma = NULL
  
  ## Chain length M for E-step
  start = Sys.time()
  while(eps > tol & iter < maxit){
    
    ## Save old Q-function
    qfunction0 = qfunction
    
    ## Obtain last chain value (Mth value) for each obs if iter > 0
    if(iter > 0){
      prev.gamma = gamma[seq(M,nrow(gamma), by = M),]
    }
    
    # Start E-step
    estep = e.step(y = y, X = X, Z = Z, ID = ID, betat = beta, Sigma_gammat = Sigma_gamma, M = M, n = n, ni = ni, sampler = mwg.rw.sampler, prev.gamma = prev.gamma)
    gamma = estep$gamma
    qfunction = estep$Qfunction
    offset = estep$offset
    yaug = estep$yaug
    Xaug = estep$Xaug
    
    # Calculate relative change in qfunction from prior iteration
    eps  = abs(qfunction-qfunction0)/abs(qfunction0)
    
    # Start M-step
    # Maximize Sigma_gamma
    Sigma_gamma = t(gamma) %*% gamma/(n*M)
    
    # Poisson GLM with corresponding weights and offset to maximize beta
    aug = rep(1:n,each = M)
    fit = glm(yaug ~ Xaug -1, 
              family = poisson(), 
              weights = rep(1/M, nrow(Xaug)), 
              offset = offset,
              start = beta
    )
    beta = as.vector(fit$coefficients)
    # Update iterator
    iter = iter + 1
    if(iter == maxit) warning("Iteration limit reached without convergence")
    # Print results if verbose
    if(verbose){
      cat(sprintf("Iter: %d Qf: %.3f g11: %f g12: %f g22: %f beta0: %.3f beta1:%.3f beta2:%.3f beta3:%.3f beta4 :%.3f
                beta5:%.3f eps:%f\n",iter, qfunction,diag(Sigma_gamma)[1],Sigma_gamma[1,2],  diag(Sigma_gamma)[2], 
                  beta[1],beta[2], beta[3], beta[4], beta[5], beta[6], eps))
      
    }
    # Save results to export
    final_beta = beta; final_Sigma_gamma = Sigma_gamma; final_gamma = gamma
  }
  end = Sys.time()
  return(list(beta = final_beta, Sigma_gamma = final_Sigma_gamma, gamma = final_gamma, time = (end-start)))
}