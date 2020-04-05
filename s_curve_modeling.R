# Basic S-curve modeling, without taking other countries' information into account

library(tidyverse)
library(stats)
library(lme4)
library(nlme)

dat <- readRDS("dat.rds")
dat_old <- readRDS("dat_old.rds")

# Works well when a country has finished its curve
china <- filter(dat, Country.Region == "China")
fit <- nls(total_cases ~ SSlogis(day, Asym, xmid, scal), data = china)
plot(china$total_cases ~ china$day)
lines(seq(0, 80, length.out = 100), predict(fit, newdata = data.frame(day = seq(0, 80, length.out = 100))))

# Does not work well when a country has an incomplete curve
# E.g. China, but using only its first 10 days for future predictions
china2 <- filter(dat, Country.Region == "China" & day < 10)
fit2 <- nls(total_cases ~ SSlogis(day, Asym, xmid, scal), data = china2)
plot(china$total_cases ~ china$day)
lines(seq(0, 80, length.out = 100), predict(fit2, newdata = data.frame(day = seq(0, 80, length.out = 100))))


# Random Effects Attempt (will need to convert from linear to generalized)

# Random intercept and slope
fm2 <- lmer(log(total_cases) ~ day + (day | Country.Region), data = dat)
summary(fm2)
ranef(fm2) # Country estimates of random effect variations

## Poisson GLMM

glmm1 <- glmer(total_cases ~ day + (day | Country.Region), data = dat, family = poisson)
ranef(glmm1)

# NL Mixed Effects
library(nlme)
data <- groupedData(total_cases ~ day | Country.Region, data=dat)
#initVals <- getInitial(y ~ SSlogis(t, Asym, xmid, scal), data = data)
baseModel<- nlme(y ~ SSlogis(day, Asym, xmid, scal),
                 data = data,
                 fixed = list(Asym ~ 1, xmid ~ 1, scal ~ 1),
                 random = Asym + xmid + scal ~ 1,
                 start = c(Asym=50000, xmid = 20, scal=0.1)
)

## NLS
southkorea <- filter(dat, Country.Region == "Korea, South")
usa <- filter(dat, Country.Region == "US")
ggplot(data = southkorea, aes(x = day, y = new_cases)) + geom_line()

bellcurve.model <- function(d, mu, var, x) {
  f <- d*exp(-((x - mu)^2) / (2*var))
  return(f)
}

m1 <- nls(new_cases ~ bellcurve.model(d, mu, var, x = day), start = list(d = 800, mu = 15, var = 25), data = southkorea, trace = TRUE, lower = list(d = 1, mu = 1, var = 0))
plot(southkorea$day, southkorea$new_cases)
lines(southkorea$day, predict(m1))

m2 <- nls(new_cases ~ bellcurve.model(d, mu, var, x = day), start = list(d = 5000, mu = 15, var = 25), data = filter(dat, Country.Region == "US"), trace = TRUE, algorithm = "port", lower = list(d = 1, mu = 1, var = 0))
plot(usa$day, usa$new_cases)
lines(usa$day, predict(m2))

# NLME
ctrl <- nlmeControl(maxIter=50, msMaxIter = 50)
baseModel <- nlme(new_cases ~ bellcurve.model(d, mu, var, x = day), 
                  data = dat, 
                  fixed = list(d ~ 1, mu ~ 1, var ~ 1),
                  random = d + mu + var ~ 1|Country.Region,
                  start = list(fixed = c(1000, 20, 20)),
                  na.action = na.omit,
                  control=ctrl)

nestedModel <- update(baseModel, fixed = list(d ~ 1, mu ~ 1, var ~ 1), start = fixef(baseModel))
nestedModel2 <- update(nestedModel, fixed = list(d ~ 1, mu ~ 1, var ~ 1), start = fixef(nestedModel))
nestedModel3 <- update(nestedModel2, fixed = list(d ~ 1, mu ~ 1, var ~ 1), start = fixef(nestedModel2))

preds <- predict(nestedModel2)
preds_dat <- data.frame(country = attr(preds, "names"), pred = preds)

## SAEMIX
library(saemix)
saemix.data <- saemixData(name.data = dat,
                          name.group = "Country.Region",
                          name.predictors = "day",
                          name.response = "new_cases")

bellcurve.model1 <- function(psi, id, x) {
  t <- x[,1]
  d <- psi[id,1]
  mu <- psi[id,2]
  var <- psi[id,3]
  f <- d*exp(-((t - mu)^2) / (2*var))
  return(f)
}

saemix.model <- saemixModel(model = bellcurve.model1,
                            psi0 = c(d = 800, mu = 20, var = 20))

saemix.options <- list(map=TRUE, fim=TRUE, ll.is=FALSE, displayProgress=FALSE, seed=632545)
saemix.fit1    <- saemix(saemix.model, saemix.data, saemix.options)








model1cpt<-function(psi,id,xidep) { 
  dose<-xidep[,1]
  tim<-xidep[,2]  
  ka<-psi[id,1]
  V<-psi[id,2]
  CL<-psi[id,3]
  k<-CL/V
  ypred<-dose*ka/(V*(ka-k))*(exp(-k*tim)-exp(-ka*tim))
  return(ypred)
}

saemix.model<-saemixModel(model=model1cpt,
                          description="One-compartment model with first-order absorption", 
                          psi0=matrix(c(1.,20,0.5,0.1,0,-0.01),ncol=3, byrow=TRUE,
                                      dimnames=list(NULL, c("ka","V","CL"))),transform.par=c(1,1,1),
                          covariate.model=matrix(c(0,1,0,0,0,0),ncol=3,byrow=TRUE),fixed.estim=c(1,1,1),
                          covariance.model=matrix(c(1,0,0,0,1,0,0,0,1),ncol=3,byrow=TRUE),
                          omega.init=matrix(c(1,0,0,0,1,0,0,0,1),ncol=3,byrow=TRUE),error.model="constant")