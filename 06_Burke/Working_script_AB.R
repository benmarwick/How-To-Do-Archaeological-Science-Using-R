##################################################################################################################################
##################################################################################################################################
##script for Ben Marwick's workshop (R)
#SAA2017
##################################################################################################################################
##################################################################################################################################

## -----Clean workspace
# BM # rm(list=ls(all=TRUE))
# BM # gc(T, verbose=F)

## ----- Install  packages 
require(car)  ## for vif (variance inflation test)
require(AICcmodavg) ## for AIC model selection
require(MASS) ## for stepAIC
require(ROCR) ## for AUC
# BM # devtools::install_github("palday/coefplot2", subdir = "pkg")
library(coefplot2) ## for coefficient plot.
library(caret)
#library(e1071)

## Set base directory
# setwd("C:/Users/burkea/Desktop/R workshop")   # change as needed

## Set file name
fileName <- "Test_Iberia.csv"

## ----- Read in data
dat <- read.csv(fileName, header=T) 

## ----- View/check data
head(dat)
##################################################################################################
###  ----- Standardisation of predictors
dat1 <- as.data.frame(cbind(dat[,c(1:4)], (scale(dat[,-c(1:4)]))))

# V?rification (make sure the vars are numerical)
str(dat1)

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

##-----------------------------------------------------------------------------------------------------------------
## Using all of the data including the zeros, which cannot be described as necessarily "true" zeroes, we introduce an artificial level of certainty
## regarding absences. This can bias results (King and Zeng, 2000; Dixon et al.,2005), (also see http://www2.unil.ch/biomapper/Download/Chefaoui-EcoMod-2008.pdf)
## To reduce this uncertainy, we sample the # zeros which we now call pseudo absences. Good practice is 10x the number of presences. (=1000).
## We can bootstrap this if we want, but this 1000 samples shoul dbe sufficient. We will nonetheless need to report some sensitivity to the number of absences used. 
##-----------------------------------------------------------------------------------------------------------------


## ----- Tease apart presences and absences, then select a subset of absences - recombine presences (all) and absences (sample)
#numAbsences <-350 ## 10x presences

#Presences <- subset(dat1, dat1$LGM==1)
#Absences <- subset(dat1, dat1$LGM==0)[sample(c(1:dim(dat1)[1]), numAbsences),]

## ----- Final data on which analyses will be run
#dat2<- rbind(Presences, Absences)

	## ----- Build models
	mod.0  <-  glm(form0, family=binomial, data=dat1)
	mod.1  <-  glm(form1, family=binomial, data=dat1)
	mod.2  <-  glm(form2, family=binomial, data=dat1)
	mod.3  <-  glm(form3, family=binomial, data=dat1) # ?
	mod.4  <-  glm(form4, family=binomial, data=dat1)
	mod.5  <-  glm(form5, family=binomial, data=dat1)
	mod.6  <-  glm(form6, family=binomial, data=dat1)
	mod.7  <-  glm(form7, family=binomial, data=dat1)
	mod.8  <-  glm(form8, family=binomial, data=dat1)
	
	## ----- Summarize AIC results, including weightings. Using modaicavg package.
	mods<- list(mod.0, mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8)
	modnames<- c("mod.0", "mod.1", "mod.2", "mod.3", "mod.4", "mod.5", "mod.6", "mod.7", "mod.8")
	aictab(mods, modnames, second.ord=T)

	selected<-rownames(summary(mod.8)$coefficients)[-1] #can select any model
	
	paramsOut[xx,]<- c(selected, rep(0, 10-length(selected)))

##alternatively show summaries of selected models
#	summary(mod.3)

## ---- Coefficient Plot for selected model 
coefplot2(mod.8, main="Model 8", col='blue', cex.pts=1.3, intercept=F)

## ---- list coefficients
rownames(summary(mod.8)$coefficients)


## ---- Odds ratios and 95% CI
ORs<- exp(cbind(OR = coef(mod.8), confint(mod.8)))[-1,] ## Intercept OR shouldn't be interpreted.
ORs

## ----- Assess variance inflation (>5 is not good) 
vif(mod.8)

##-----------------------------------------------------------------------------------------------------------------
## ROC and AUC - Model performance testing!
##-----------------------------------------------------------------------------------------------------------------

## ----- Best model: model selected using automagic forward selection
## ----- Heuristic to decide how data should be slpit - % training and % testing. 
## ----- 1/[1+(p-1^1/2)] ; where p = # parameters in the model
numParams<-7 #change as needed
pctSplit<- 1/(1+sqrt(numParams-1)) ## =~0.3 with 6 = 70/30. good. 

## ----- Select # of samples from dat2 will be used to fit the already decided upon model.
numTest <- floor(0.7*dim(dat3)[1])  

## ----- Number of different data splits (70/30) we want to run.
reps<-100

## ----- Creat empty AUC Output table
AUCOut <- mat.or.vec(reps,1)

dev.new()
## ----- Run bootstrapped AUC analysis
for (ii in 1:reps){
  sampling <- sample(1:nrow(dat2),numTest) ## select smaple vector
  dat70 <- dat2[sampling,]  #training data
  dat30 <- dat2[-sampling,]  #testing data
  modelFormula<- formula(mod.8) # extract formula from model
  
  ## ----- 
  
  mod.x <- glm(modelFormula, data=dat70, family=binomial)
  pred <- predict(mod.x, newdata=dat30, type="response")
  pred.a <- prediction(pred, dat30[,1])
  perf <- performance(pred.a,"tpr","fpr")
  plot(perf, main=paste0("Reps: ",ii), col='red')
  abline(0,1, lty=2, col='grey')
  auc.a <- performance(pred.a,"auc")
  AUCOut[ii] <- auc.a@y.values
  
}

## ---- Organize outputs
AUCOut.Mean <- mean(as.numeric(AUCOut))
AUCOut.SE <- sqrt(var(as.numeric(AUCOut)))/sqrt(reps)
AUCSummary <- c(AUCOut.Mean,AUCOut.SE)
names(AUCSummary) <- c("AUC.Mean","AUC.SE")

## ---- see AUC summary object
AUCSummary

##prob is we can't determine whether the sites are being correctly predicted or not with greater than 50% as presence
p <- ifelse(predict(mod.8, newdata=dat2, type="response") > 0.5, 1, 0)
confusionMatrix(p, dat2$LGM)

##################################################################################################################################
## -------------------------------------------------------------------------------------------------------------------------------
## ---- Once satisfied with model and performance, predict over all locations for mapping surfaces. 
## -------------------------------------------------------------------------------------------------------------------------------
##################################################################################################################################

FullGrid <- subset(dat2, dat2$LGM==0)  ## all LGM=0 points
pred <- predict(mod.8, newdata=FullGrid, type="response") ## make prediction

## ----- This is the data you want to exprt to ARCGIS to make maps wth
SurfaceDat<- cbind(FullGrid[,c(3,4)], pred) ## combine coordinates and predicted values into a new table
write.table(SurfaceDat, "Test_Predict.txt", quote=F, row.names=F) ## export table.

## ----- This is the data you want to exprt to ARCGIS to make maps wth
SurfaceDat<- cbind(FullGrid[,c(1,2)], pred) ## combine coordinates and predicted values into a new table
write.table(SurfaceDat, "Iberia_Predict.csv", quote=F, row.names=F) ## export table.

## ----- In the mean tme, we can make a quick plot in R to look at the general pattern.
require(fields) 
dev.new()
quilt.plot(SurfaceDat$X, SurfaceDat$Y, SurfaceDat$pred, xlab="Longitude", ylab="Latitude", main="Model Prediction")


# done.

