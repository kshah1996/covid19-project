## more data processing
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

glmm1 <- glmer(new_cases ~ day + day2 + GHS_Score + AgeGEQ65 + UrbanPop + (day | Country.Region), data = dat, family = poisson)
fix_glmer <- fixef(glmm1)

gamma <- read.table("longleaf/glmm_mwg_rw_gamma_1.txt", header = F, skip = 1)
gamma2 <- as.matrix(gamma[,2:3])
M <- 1000
mean(gamma2[(49*M+1):(50*M),1])
mean(gamma2[(49*M+1):(50*M),2])

fix_mwg <- c(0.844,0.202,-0.005,0.028,0.010,-0.001)





##### China
country = 21
country_name = 'China'

ran_glmer <- c((ranef(glmm1)$Country.Region[country,])[[1]],(ranef(glmm1)$Country.Region[country,])[[2]],0,0,0,0)
ran_mwg <- c(mean(gamma2[((country-1)*M+1):(country*M),1]),mean(gamma2[((country-1)*M+1):(country*M),2]),0,0,0,0)
coef_glmer <- fix_glmer + ran_glmer
coef_mwg <- fix_mwg + ran_mwg

dat2 <- dat %>% 
    filter(Country.Region==country_name) %>% 
    mutate(model_glmer=exp(coef_glmer[1]+coef_glmer[2]*day+coef_glmer[3]*day^2+coef_glmer[4]*GHS_Score+coef_glmer[5]*AgeGEQ65+coef_glmer[6]*UrbanPop)) %>% 
    mutate(model_mwg=exp(coef_mwg[1]+coef_mwg[2]*day+coef_mwg[3]*day^2+coef_mwg[4]*GHS_Score+coef_mwg[5]*AgeGEQ65+coef_mwg[6]*UrbanPop))
ggplot(data=dat2)+
    geom_line(aes(x=day,y=new_cases))