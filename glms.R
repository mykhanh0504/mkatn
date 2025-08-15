url <- 'https://raw.githubusercontent.com/niamhmimnagh/FGLM01---Introduction-to-Generalised-Linear-Models/refs/heads/main/data/plant_biomass.txt'
plant_biomass <- read.table(url,header=TRUE,sep="\t")
head(plant_biomass)

table(plant_biomass$plot_id)

range(plant_biomass$Rainfall_mm)
range(plant_biomass$Biomass_gm2)


# Visualize relationship between rainfall and biomass ---------------------
#ctrl shift r

plot(plant_biomass$Rainfall_mm,plant_biomass$Biomass_gm2)
abline(lm(plant_biomass$Biomass_gm2~plant_biomass$Rainfall_mm))


# Fit linear model using lm() ---------------------------------------------
fit_lm <- lm(Biomass_gm2~Rainfall_mm,data=plant_biomass)
summary(fit_lm)

# for a 1mm increase in rainfall, we expect to see an increase of 0.58+/-0.04 g/m2 of plant biomass (p<0.001)
# rainfall accounts for 84% of variation indata


# Fit the same model using the glm function -------------------------------
fit_glm <- glm(Biomass_gm2~Rainfall_mm,
               family=gaussian(link='identity'),
               data=plant_biomass)
summary(fit_glm)

coef(fit_lm)
coef(fit_glm)


# Plot fitted values from both models -------------------------------------
plot(fitted(fit_lm),fitted(fit_glm))

# Diagnostic checks -------------------------------------------------------
plot(fitted(fit_glm),resid(fit_glm))

# QQ plot, compares quantiles of residuals to quantiles of theoretical norm dis
qqnorm(resid(fit_glm))

# Predicting new values ---------------------------------------------------
url <- 'https://raw.githubusercontent.com/niamhmimnagh/FGLM01---Introduction-to-Generalised-Linear-Models/refs/heads/main/data/plant_biomass_new.txt'
plant_biomass_new <- read.table(url,header=T,sep='\t')
head(plant_biomass_new)

pred_glm <- predict(fit_glm,newdata=plant_biomass_new,
                    type='link',se.fit=TRUE)

fit_mean <- pred_glm$fit
se_mean <- pred_glm$se.fit
plot(fit_mean,plant_biomass_new$Biomass_gm2,
     xlab='fitted',ylab='observed')


# bernoulli model of disease ----------------------------------------------

url <- 'https://raw.githubusercontent.com/niamhmimnagh/FGLM01---Introduction-to-Generalised-Linear-Models/refs/heads/main/data/disease_presence.txt'
disease_presence <- read.table(url,header=TRUE,sep='\t')
head(disease_presence)

plot(disease_presence$Age,disease_presence$Disease)

# fit a logistic glm - glm with binary data
fit <- glm(Disease~Age,
           family=binomial(link='logit'),
           data=disease_presence)
summary(fit)

#intercept -2.35, estimated log-odds of disease for an animal that is 0yo

range(disease_presence$Age)

#convert this to a probability = exp(log-odds)/(1+exp(log-odds))
exp(-2.35)/(1+exp(-2.35))

#estimate for age = 0.317 >>> for each additional year, log odds increase by 0.317
exp(0.317)
#for each additional year, odds of an animal having disease increase by a factor of 1.37

#drop in deviance tells us that model with age fits better than the intercept models
#strong evidence that age explains part of variation in disease status
#lower aic means model has a better fit

# predict probability of disease at specific ages
ages_of_interest <- data.frame(Age=c(2,5,8,12))
disease_prediction <- predict(fit,newdata=ages_of_interest,
                              type ='link',se.fit=TRUE)
predictions <- plogis(disease_prediction$fit)


# poisson distribution for count data -------------------------------------
url <- 'https://raw.githubusercontent.com/niamhmimnagh/FGLM01---Introduction-to-Generalised-Linear-Models/refs/heads/main/data/birds.txt'
birds <- read.table(url,header=TRUE,sep='\t')
head(birds)

plot(birds$Nests,birds$VegDensity)

#fit poisson glm
fit_poisson <- glm(Nests~VegDensity,
                   family=poisson(link='log'),
                   data=birds)
summary(fit_poisson)
range(birds$VegDensity)

#intercept = log of expected number of nests when veg density = 0%
#because 0% veg density doesn't exist in dataset, intercept is extrapolated

#estimate for vegden=0.02, for each 1% increase in vegden, log of expected numbers of nests increases by 0.02
#incident rate ratios
exp(0.02)
# 1.02 the mean number of nest is multiplied by 1.02 for every 1% increase in vegden (2% increase)

#drop in deviance suggests vegden explains a portion of variance in nest count


# incidence rate ratios ---------------------------------------------------

irr <- exp(coef(fit_poisson)[2])

# predictions for specific veg densities ----------------------------------

new_sites <- data.frame(VegDensity=c(10,30,60,90))
predictions <- predict(fit_poisson,newdata=new_sites,type='link',se.fit=TRUE)
exp(predictions$fit)


# check mean and variance -------------------------------------------------
pearson_chisq <- sum(residuals(fit_poisson,type='pearson')^2)
dispersion <- pearson_chisq/df.residual(fit_poisson) # number of observations - number of parameters you're estimating

# the closer to 1 the dispersion is, the better (anything less than 1.5 is alright)
# 1 means mean and variance are the same
# >1.5 overdispersion, switch to neg binomial? zero-inflated neg binomial