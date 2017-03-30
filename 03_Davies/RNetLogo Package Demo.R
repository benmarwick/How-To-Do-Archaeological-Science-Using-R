#RNetLogo Demonstration
#Ben Davies, University of Auckland
#Prepared for Society for American Archaeology Annual Meeting 2017 

#This demonstration requires installation of NetLogo 6.0, and uses the NetLogo 
#model HMODEL, originally published here:
#Davies, Holdaway, and Fanning 2016, Modeling the palimpsest: an exploratory 
#agent-based model of surface archaeological deposit formation in a fluvial 
#arid Australian landscape. The Holocene 26(3):350-463 
#DOI: 10.1177/0959683615609754

#1) Install RNetLogo and other packages required for this tutorial
require(RNetLogo) 
require(spatstat)

#2) Open NetLogo, load the model, initialize it, and run it
NLStart("C:\\Program Files\\NetLogo 6.0\\app",gui=TRUE) #point this at NetLogo.jar directory
NLLoadModel("C:\\Users\\bdav0\\Desktop\\RSci\\hmodel.nlogo") #point this at the model .nlogo file to load
NLCommand("Setup") #runs the NetLogo setup command
NLCommand("set surface_stability 0") #sets the surface_stability parameter
NLDoCommand(2001,"Go") #runs the model for 2000 iterations

#3) Create dataframes for samples (n=100) of C14, OSL, and a null sample from total population
hearth_c14<- NLReport("[who] of n-of 100 hearths with [ hidden? = false and charcoal? = true ]")
hearth_osl<- NLReport("[who] of n-of 100 hearths with [ hidden? = false ]")
hearth_null<- NLReport("[who] of n-of 100 hearths")
c14_sample<-data.frame(ID=integer(),X=double(),Y=double(),age=integer())
osl_sample<-data.frame(ID=integer(),X=double(),Y=double(),age=integer())
null_sample<-data.frame(ID=integer(),X=double(),Y=double(),age=integer())
for (i in c(1:100)){
  c14_sample[i,]<-c(hearth_c14[i],NLReport(c(paste("[xcor] of hearth ",hearth_c14[i]),paste("[ycor] of hearth ",hearth_c14[i]),paste("[age] of hearth ",hearth_c14[i]))))
  osl_sample[i,]<-c(hearth_osl[i],NLReport(c(paste("[xcor] of hearth ",hearth_osl[i]),paste("[ycor] of hearth ",hearth_osl[i]),paste("[age] of hearth ",hearth_osl[i]))))
  null_sample[i,]<-c(hearth_null[i],NLReport(c(paste("[xcor] of hearth ",hearth_null[i]),paste("[ycor] of hearth ",hearth_null[i]),paste("[age] of hearth ",hearth_null[i]))))
}

#4) Plot the age samples by ascending order of age
plot(sort(null_sample$age),c(1:100),xlab="Years BP",ylab="Index by Age",main="",xlim=c(0,2000))
points(sort(osl_sample$age),c(1:100),pch=16,col="pink")
points(sort(c14_sample$age),c(1:100),pch=16)
abline(-11.11111111,0.055555555555555555555,lty=2)


#5) Quit NetLogo
NLQuit()

