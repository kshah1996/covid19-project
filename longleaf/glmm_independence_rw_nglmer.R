library(tidyverse)
library(stats)
library(lme4)
library(mvtnorm)
set.seed(1)
setwd("/nas/longleaf/home/euphyw/Desktop/covid19-project")

### function for the log likelihood for ith subject 
f = function(x,yi, Xi, betat, Sigma_gammat){
    
    # here we assume random effect matrix Zi for subject i same as Xi
    #Zi = Xi
    
    # Zi is a subset of the columns of Xi
    Zi = NULL
    if(is.vector(Xi)){
        Zi = Xi[Z]
    }else{
        Zi = Xi[,Z]
    }
    
    # calculate lambdai
    lambdai = exp(Xi%*%betat + Zi%*%matrix(x, ncol = 1))
    
    # sum across repeated observations for poisson portion
    lli = sum(dpois(yi, lambdai, log = T)) 
    
    # dont forget to include the MVN log likelihood
    lli = lli  + dmvnorm(x,sigma = Sigma_gammat, log = T)
    
    return(lli)
}

### log proposal density function
g = function(x, Sigma_gammat){
    dmvnorm(x, sigma = Sigma_gammat, log = T)
}

### proposal function, MVN(0, Sigma)
g.sim = function(Sigma_gammat){
    rmvnorm(1, sigma = Sigma_gammat)
}

### calculate MH ratio given f and g, x is the proposal, xt is the current value from the chain
R = function(xt,x, f, g, yi, Xi, betat, Sigma_gammat){
    # log numerator - log denominator
    logR = ( f(x, yi, Xi, betat, Sigma_gammat) + g(xt, Sigma_gammat) ) - ( f(xt,yi, Xi, betat, Sigma_gammat) + g(x , Sigma_gammat) )
    R = exp(logR)
    return(R)
}

mh.independence.sampler = function(yi, Xi, betat, Sigma_gammat, M, prev.gamma.i = NULL){
    
    # get dimension of gammai
    q = ncol(Sigma_gammat)
    
    # initialize the chain vector
    x.indep.chain = matrix(0, M, q)
    
    if(is.null(prev.gamma.i)){
        # Simulate initial draw from proposal density g
        x.indep.chain[1,] = g.sim(Sigma_gammat)
    }else{
        # if last value from previous chain avail, start there
        x.indep.chain[1,] = prev.gamma.i    
    }
    
    
    # now start chain
    accept = 0
    for(i in 1:(M-1)){
        
        # set the value at current iteration of the chain to variable xt
        xt = x.indep.chain[i,]
        
        # draw a proposal from the proposal density
        x = g.sim(Sigma_gammat)
        
        # calculate MH ratio 
        r = min(
            R(xt, x, f, g, yi, Xi, betat, Sigma_gammat),
            1
        )
        
        # Generate draw from bernoulli(p).
        # Alternatively, can directly compare ratio to 
        # a U(0,1) draw as we did with Rejection Sampling
        keep = rbinom(1, 1, r)
        
        if(keep == 1){
            # if keep = 1, then set next iteration equal to then proposal
            x.indep.chain[i+1,] = x
            #  update number of acceptacnes
            accept = accept + 1
        }else{
            # otherwise, carry over value from the current iteration
            x.indep.chain[i+1,] = xt
        }
    }
    
    return(list(gammai = x.indep.chain, ar = accept/M))
}

e.step= function(y, X, ID, betat, Sigma_gammat, M, n, ni, sampler, burn.in = 200,prev.gamma = NULL){
    
    # initialize Q-function
    Qfunction = 0
    
    # matrix to hold chains from each subject
    gamma = matrix(0, n*M, ncol(Sigma_gammat))
    
    # vector to hold subject acceptance rates
    q = ncol(Sigma_gammat)
    ar = matrix(0, n, q)
    
    # vector to hold offset values Zi %*% gammai, yaug, Xaug in M step
    N = sum(ni)#ni*n
    offset = yaug = rep(0, N*M)
    Xaug = matrix(0, nrow = N*M, ncol = ncol(X))
    
    # loop over observations
    for(i in 1:n){
        
        # subject i indices
        subjecti = which(ID == i)
        
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
        
        # handle cases for when Zi has 1 column and when Xi has 1 row
        aug = rep(1:ni[i],M)#rep(1:ni, M)
        yi_aug = yi[aug]
        Xi_aug = NULL
        if(is.vector(Xi)){
            Xi_aug = matrix(Xi,nrow=1)[aug,]
        }else{
            Xi_aug = Xi[aug,]
        }
        #Xi_aug = Xi[aug,]#tryCatch({Xi[aug,]},error=function(e){})
        #Zi_aug = Xi_aug
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
        
        # create augmented version of gammai to aid vectorization
        # repeated ni times per replicated subject 
        # total length is (n*ni*M) rows
        augg = rep(1:M, each = ni[i])#rep(1:M, each = ni)
        gammai_aug = gammai[augg,]
        
        # calculate Q function for subject i:  poisson portion (n*ni*M)
        XBaug = Xi_aug%*%betat
        Zgammaaug = rowSums(Zi_aug * gammai_aug)
        #Zgammaaug = tryCatch({rowSums(Zi_aug * gammai_aug)},error=function(e){print(Zi)})#print(Zi_aug); print(gammai_aug)})
        lambdai_aug = exp(XBaug + Zgammaaug)
        Qi = sum(dpois(yi_aug, lambda = lambdai_aug, log = T))
        
        # calculate Q function for subject i:  MVN portion (n*M)
        Qi = Qi + sum(dmvnorm(gammai_aug, sigma = Sigma_gammat, log = T))
        
        # divide by M
        Qi = Qi/M
        
        # add to overall Q estimate
        Qfunction = Qfunction + Qi
        
        # save offset, yaug, xaug for later
        #a = (i-1)*M*ni + 1
        #b = i*M*ni
        # calculate the location to store offset, yaug, xaug
        if(i == 1){
            a = 1 
        }else{ 
            a = M*sum(ni[1:i-1]) + 1 
        }
        b = a+M*ni[i]-1
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

# more data processing
dat = readRDS("dat2.rds")
# remove na data
dat <- dat %>% mutate(day2 = day^2) %>% drop_na(GHS_Score) %>% drop_na(AgeGEQ65) %>% drop_na(UrbanPop)
# modify china new_cases day 0 since it was NA previously
dat[402,5]=548
dat$ID <- dat %>% group_indices(Country.Region)

for (i in 1:max(dat$ID)) {
    if (sum(dat$ID==i) < 5) {
        dat<- dat[!(dat$ID==i),]
    }
}

dat$ID <- dat %>% group_indices(Country.Region)

# unique country list
order = unique(dat$Country.Region)
# number of unique countries
n = length(order)
# assign IDs to each county and generate ni vector
ID = as.numeric()
ni = as.numeric()
index = 1
for(i in 1:length(order)){
    #generate ID
    count = length(which(dat$Country.Region==order[i]))
    ID[index:(index+count-1)] = rep(i,count)
    index = index + count
    #generate ni
    ni[i] = count
}
# add intercept
dat = dat %>% add_column(int = 1)
# generate the design and response matricies
X = unname(as.matrix(dat %>% select(int, day, day2, GHS_Score, AgeGEQ65, UrbanPop)))
y = unname(as.matrix(dat %>% select(new_cases)))
# define columns of X that are in Z
Z = c(1,2)

#glmm1 = glmer(new_cases ~ day + day2 + GHS_Score + AgeGEQ65 + UrbanPop + (day | Country.Region), data = dat, family = poisson)
#beta = as.vector(fixef(glmm1))
#Sigma_gamma =  diag(rep(1, 2))
# start values
beta = c(-0.5,0.4,0,0,0,0)
Sigma_gamma = diag(c(12,0.1))


## set initial parameters
tol = 10^-5
maxit = 1000
iter = 0
eps = Inf
qfunction = -10000 # using Qfunction for convergence
prev.gamma = NULL

## starting values
#beta = as.vector(glm(y ~ X-1, family = poisson())$coef)
#Sigma_gamma =  diag(rep(1, 5))

## fix chain length at 1000 in E-step
M = 1000

start = Sys.time()
while(eps > tol & iter < maxit){
    
    ## save old qfunction
    qfunction0 = qfunction
    
    ## obtain last chain value (Mth value) for each obs if iter > 0
    if(iter > 0){
        prev.gamma = gamma[seq(M,nrow(gamma), by = M),]
    }
    
    ## E-step
    estep = e.step(y = y, X = X, ID = ID, betat = beta, Sigma_gammat = Sigma_gamma, M = M, n = n, ni = ni, sampler = mh.independence.sampler, prev.gamma = prev.gamma)
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
        , file = "/nas/longleaf/home/euphyw/Desktop/covid19-project/longleaf/glmm_independence_rw_nglmer.txt", append = TRUE)
}
end = Sys.time()
print(end - start)