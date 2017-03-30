##################################################################################################################################
##################################################################################################################################
## Script to analyse occupation as a function of geographical variables using logistic regression. 
## Original script: May 9, 2012 (PJ & GL)
## -----  
## Revised - March 3, 2016 by PJ, then edited by AB March 30
## ----- 
##################################################################################################################################
##################################################################################################################################

## -----Clean workspace
rm(list=ls(all=TRUE))
gc(T, verbose=F)

## ----- Install  packages 
require(car)  ## for vif (variance inflation test)
require(AICcmodavg) ## for AIC model selection
require(MASS) ## for stepAIC
require(ROCR) ## for AUC
library(coefplot2) ## for coefficient plot.
library(caret)
library(e1071)

## Set base directory
# setwd("C:/Users/burkea/Desktop/R workshop")   # change as needed  # A changer si n?cessaire

## Set file name
fileName <- "Test_Iberia.csv"

## ----- Read in data
dat <- read.csv(fileName, header=T) 

## ----- View/check data
head(dat)


##################################################################################################
###  ----- Standardization of predictors

###  ----- Standardization of predictors
dat2 <- as.data.frame(cbind(dat[,c(1:4)], (scale(dat[,-c(1:4)]))))
#dat2<- dat2[,] ## removal of extra non informative variables

# V?rification (make sure the vars are numerical)
str(dat2)

##################################################################################################################################
## ----- MODELLING
##################################################################################################################################

## ----- Define formulae
form0 <- formula(LGM~ 1)#intercept only model
form1<- formula(LGM~ X + Y, data=dat1,family=binomial)
form2<- formula(LGM~ elev + slope, data=dat1,family=binomial)
form3<- formula(LGM~ t_min_y, data=dat1,family=binomial)
form4<- formula(LGM~ elev + slope + p_min_spr, data=dat1,family=binomial)
form5<- formula(LGM~ p_min_spr + t_min_y, data=dat1,family=binomial)
form6<- formula(LGM~ t_avg_y + p_avg_y, data=dat1,family=binomial)
form7<- formula(LGM~ elev + slope + t_avg_y + p_avg_y, data=dat1,family=binomial)
form8<- formula(LGM~.) ## all variables for step-wise procedure
## all variables for step-wise procedure

##-----------------------------------------------------------------------------------------------------------------
## Using all of the data including the zeros, which cannot be described as necessarily "true" zeroes, we introduce an artificial level of certainty
## regarding absences. This can bias results (King and Zeng, 2000; Dixon et al.,2005), (also see http://www2.unil.ch/biomapper/Download/Chefaoui-EcoMod-2008.pdf)
## To reduce this uncertainy, we sample the # zeros which we now call pseudo absences. Good practice is 10x the number of presences. (=1000).
## We can bootstrap this if we want, but this 1000 samples shoul dbe sufficient. We will nonetheless need to report some sensitivity to the number of absences used. 
##-----------------------------------------------------------------------------------------------------------------

## ----- Tease apart presences and absences, then select a subset of absences - recombine presences (all) and absences (sample)
## ----- Tease apart presences and absences, then select a subset of absences - recombine presences (all) and absences (sample)
numAbsences <-350 ## 10x presences

Presences <- subset(dat2, dat2$LGM==1)
Absences <- subset(dat2, dat2$LGM==0)[sample(c(1:dim(dat2)[1]), numAbsences),]

## ----- Final data on which analyses will be run
dat3<- rbind(Presences, Absences)

	## ----- data table for forward selection (stepAIC)
dat4<-dat3[,c(2, 5, 6, 7, 8, 9, 10)]
	## ----- View/check data
head(dat4)


	## ----- Build models
	mod.0  <-  glm(form0, family=binomial, data=dat3)
	mod.1  <-  glm(form1, family=binomial, data=dat3)
	mod.2  <-  glm(form2, family=binomial, data=dat3)
	mod.3  <-  glm(form3, family=binomial, data=dat3)
	mod.4  <-  glm(form4, family=binomial, data=dat3)
	mod.5  <-  glm(form5, family=binomial, data=dat3)
	mod.6  <-  glm(form6, family=binomial, data=dat3)
	mod.7  <-  glm(form7, family=binomial, data=dat3)
	mod.8 <- stepAIC(glm(form8, family=binomial, data=dat4))
	
	## ----- Summarize AIC results, including weightings. Using modaicavg package.
	mods<- list(mod.0, mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8)
	modnames<- c("mod.0", "mod.1", "mod.2", "mod.3", "mod.4", "mod.5", "mod.6", "mod.7", "mod.8")
	aictab(mods, modnames, second.ord=T)

summary(mod.8)

## ---- Coefficient Plot for model 
coefplot2(mod.8, main="Model 8", col='blue', cex.pts=1.3, intercept=F)

## ---- list coefficients
 rownames(summary(mod.8)$coefficients)


## ---- Odds ratios and 95% CI
ORs<- exp(cbind(OR = coef(mod.8), confint(mod.8)))[-1,] ## Intercept OR shouldn't be interpreted.
ORs

## ----- Assess variance inflation (>5 is not good) 
vif(mod.8)

