library(tidyverse)
library(stats)
library(lme4)
library(mvtnorm)
set.seed(1)
setwd("/nas/longleaf/home/euphyw/Desktop/covid19-project")
f = function(x, yi,Xi,betat,Sigma_gammat) {
    Zi = Xi[,1:2]
    lambdai = exp(Xi%*%betat + Zi %*% matrix(x, ncol=1) )
    lli = sum(dpois(yi,lambdai,log=T)) + dmvnorm(x,sigma= Sigma_gammat, log=T)
    return (lli)
}

g = function(x, Sigma_gammat) {
    dmvnorm(x, sigma=Sigma_gammat,log=T)
}

## Proposal function MVN(0,Sigma)
g.sim = function(Sigma_gammat) {
    rmvnorm(1,sigma=Sigma_gammat)
}


# rw density
h.sim = function(var = 1){
    rnorm(1, mean = 0, sd = sqrt(var))
}

# new MH ratio function (proposal density cancels out due to symmetr\boldsymbol{y})
R = function(xt,x, f, yi, Xi, betat, Sigma_gammat){
    # log numerator - log denominator
    logR = f(x, yi,Xi,betat,Sigma_gammat) - f(xt, yi,Xi,betat,Sigma_gammat)
    R = exp(logR)
    return(R)
}


adaptive.mwg.rw.sampler = function(yi, Xi, betat, Sigma_gammat, M, prev.gamma.i = NULL, b = 50){
    
    # get dimension of gammai
    q = ncol(Sigma_gammat)
    
    # initialize the chain vector
    x.indep.chain = matrix(0, M, q)
    
    
    # initialize proposal variance
    prop.var = rep(1, q)
    
    if(is.null(prev.gamma.i)){
        # Simulate initial draw from original proposal density g
        x.indep.chain[1,] = g.sim(Sigma_gammat)
    }else{
        # if last value from previous chain avail, start there
        x.indep.chain[1,] = prev.gamma.i  
        
    }
    
    #intialize batch index
    batch = 0
    
    # now start chain
    accept = b.accept = rep(0,q)
    
    
    for(i in 1:(M-1)){
        
        # set the value at current iteration of the chain to variable xt
        xt = x.indep.chain[i,]
        
        #looping over chains drawing univariate proposal
        # conditions unknown here so performing MH at each gibbs step
        for(j in 1:q){  
            
            # set propsal equal to previous
            x = xt 
            
            # only update the jth component
            # proposal variance is based on vector
            x[j] = x[j] + h.sim(var = prop.var[j])
            
            # calculate MH ratio
            r = min(
                R(xt, x, f, yi, Xi, betat, Sigma_gammat),
                1
            )
            
            # Generate draw from bernoulli(p).
            # Alternatively, can directly compare ratio to 
            # a U(0,1) draw as we did with Rejection Sampling
            keep = rbinom(1, 1, r)
            
            if(keep == 1){
                # if keep = 1, then set next iteration equal to then proposal
                x.indep.chain[i+1,] = x
                
                # reset xt
                xt = x
                
                #  update number of acceptances
                accept[j] = accept[j] + 1
                b.accept[j] = b.accept[j] + 1 
            }else{
                # otherwise, carry over value from the current iteration
                x.indep.chain[i+1,] = xt
            }
        }
        # end of a single gibbs cycle
        
        # if at end of batch
        if(floor(i/b) == ceiling(i/b)){
            
            # increment for proposal variance
            delta.b = min(0.01, 1/sqrt(i))
            
            # loop over proposal density variance vector
            for(j in 1:q){
                if(b.accept[j]/b > 0.44){
                    # if greater, add to variance
                    prop.var[j] = log(sqrt(prop.var[j])) + delta.b        
                }else{
                    # otherwise, subtract
                    prop.var[j] = log(sqrt(prop.var[j])) - delta.b
                }
            }
            
            # tranform back from log sqrt scale
            prop.var = exp(prop.var)^2
            # reset batch counter
            b.accept = rep(0, q)
            
            # increment batch index
            batch = batch + 1
        } 
        
    }
    # end chain 
    
    return(list(gammai = x.indep.chain, ar = accept/M, prop.var = prop.var))
}

e.step = function(y, X, betat, Sigma_gammat, M , n, sampler, burn.in=200, prev.gamma= NULL) {
    
    # initialization
    Qfunction = 0
    gamma = matrix(0, n*M, ncol(Sigma_gammat))
    ar = matrix(0, n, 2)
    
    nc = sum(as.vector(table(dat$Country.Region)))
    # offset Zi %*% gammai
    N = n*nc
    offset=yaug = rep(0,N*M)
    Xaug = matrix(0 , nrow=N*M, ncol=ncol(X))
    
    # loop over observations
    for(i in 1:n){
        
        if (i == 1) {ni.prev=0}
        subject.prev=which(dat$ID < i)
        ni.prev=length(subject.prev)
        # subject i indices
        subjecti = which(dat$ID == i)
        ni = length(subjecti)
        # grab subject i data
        yi = y[subjecti]
        Xi = X[subjecti,]
        
        # create chain of length M per observation 
        if(is.null(prev.gamma)){
            # if no previous chain available
            # start from scratch and remove burn.in
            chain = sampler(
                yi = yi, 
                Xi = Xi,
                betat = betat, 
                Sigma_gammat = Sigma_gammat,
                M = M + burn.in)
            gammai = chain$gammai[-c(1:burn.in),]
        }else{
            # if chain available from previous EM
            # restart this chain from last draw in previous chain
            chain = sampler(
                yi = yi, 
                Xi = Xi,
                betat = betat, 
                Sigma_gammat = Sigma_gammat,
                M = M, # no burn in
                prev.gamma = prev.gamma[i,]
            ) 
            gammai = chain$gammai
        }
        
        ar[i,] = chain$ar
        
        # create augmented versions for Q function calculation
        # total length is (n*ni*M) rows
        aug = rep(1:ni, M)
        yi_aug = yi[aug]
        Xi_aug = Xi[aug,]
        Zi_aug = Xi_aug[,1:2]
        
        # create augmented version of gammai to aid vectorization
        # repeated ni times per replicated subject 
        # total length is (n*ni*M) rows
        augg = rep(1:M, each = ni)
        gammai_aug = gammai[augg,]
        
        # calculate Q function for subject i:  poisson portion (n*ni*M)
        XBaug = Xi_aug%*%betat
        Zgammaaug = rowSums(Zi_aug * gammai_aug)
        lambdai_aug = exp(XBaug + Zgammaaug)
        Qi = sum(dpois(yi_aug, lambda = lambdai_aug, log = T))
        
        # calculate Q function for subject i:  MVN portion (n*M)
        Qi = Qi + sum(dmvnorm(gammai_aug, sigma = Sigma_gammat, log = T))
        
        # divide by M
        Qi = Qi/M
        
        # add to overall Q estimate
        Qfunction = Qfunction + Qi
        
        # save offset, yaug, xaug for later
        a = M*ni.prev + 1
        b = M*ni+ a-1
        offset[a:b] = Zgammaaug
        yaug[a:b] = yi_aug
        Xaug[a:b,] = Xi_aug
        
        # save gammai for later
        a = (i-1)*M + 1
        b = i*M
        gamma[a:b,] = gammai
    }
    
    return(list(Qfunction = Qfunction, gamma = gamma, ar = ar, offset = offset, yaug = yaug, Xaug = Xaug))
}

## EM
## set initial parameters
tol = 10^-5
maxit = 10
iter = 0
eps = Inf
qfunction = -10000 # using Qfunction for convergence
prev.gamma = NULL

dat <- readRDS("dat2.rds")
dat <- dat %>% mutate(day2 = day^2) %>% drop_na(AgeGEQ65) %>% drop_na(UrbanPop)  %>% drop_na(GHS_Score)
dat$ID <- dat %>% group_indices(Country.Region)

for (i in 1:max(dat$ID)) {
    if (sum(dat$ID==i) < 5) {
        dat<- dat[!(dat$ID==i),]
    }
}

dat$ID <- dat %>% group_indices(Country.Region)
dat[dat$Country.Region=="China",][1,5] = 548

## starting values
fit.glmm <- summary(glmm1 <- glmer(new_cases ~ day + day2 + GHS_Score + AgeGEQ65 
                                   + UrbanPop + (day | Country.Region), data = dat, family = poisson))
beta = as.vector(fit.glmm$coefficients[,1])
Sigma_gamma =  diag(rep(1, 2))
X <- cbind(1,dat$day,dat$day2,dat$GHS_Score,dat$AgeGEQ65,dat$UrbanPop)
n <- max(dat$ID)

## fix chain length at 1000 in E-step
M = 1000
# M=10
start = Sys.time()
while(eps > tol & iter < maxit){
    
    ## save old qfunction
    qfunction0 = qfunction
    
    ## obtain last chain value (Mth value) for each obs if iter > 0
    if(iter > 0){
        prev.gamma = gamma[seq(M,nrow(gamma), by = M),]
    }
    
    ## E-step
    estep = e.step(y = dat$new_cases, X = X, betat = beta, Sigma_gammat = Sigma_gamma, M = M, n = n,sampler = adaptive.mwg.rw.sampler, prev.gamma = prev.gamma)
    gamma = estep$gamma
    qfunction = estep$Qfunction
    offset = estep$offset
    yaug = estep$yaug
    Xaug = estep$Xaug
    
    ## Calculate relative change in qfunction from prior iteration
    eps  = abs(qfunction-qfunction0)/abs(qfunction0)
    
    ## Start M-step
    
    # s2gamma, MLE for sigma^2 from normal with mean 0, averaged over M
    # closed form derived from Q function approximation
    Sigma_gamma = t(gamma) %*% gamma/(n*M)
    
    aug = rep(1:n,each = M)
    fit = glm(yaug ~ Xaug -1, 
              family = poisson(), 
              weights = rep(1/M, nrow(Xaug)), 
              offset = offset,
              # use starting value from previous step
              start = beta
    )
    beta = as.vector(fit$coefficients)
    
    ## update iterator
    iter = iter + 1
    if(iter == maxit) warning("Iteration limit reached without convergence")
    
    ## print out info to keep track
    cat(sprintf("Iter: %d Qf: %.3f g11: %f g12: %f g22: %f beta0: %.3f beta1:%.3f beta2:%.3f beta3:%.3f beta4 :%.3f
                    beta5:%.3f eps:%f\n",iter, qfunction,diag(Sigma_gamma)[1],Sigma_gamma[1,2],  diag(Sigma_gamma)[2], 
                beta[1],beta[2], beta[3], beta[4], beta[5], beta[6], eps)
    , file = "/nas/longleaf/home/euphyw/Desktop/covid19-project/GLMM_amwg_LL.txt", append = TRUE)
    
}

