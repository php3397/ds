library(readxl)
Cars <- read_excel("C:/Users/b60552/Desktop/p/ml/datasets/Cars.xlsx")
View(Cars)
#check model performance for all variables at once
model.car <- lm(MPG ~ VOL + HP + SP +WT,data = Cars)
summary(model.car)

# since vol and wt have pr >0.05, see which one of them is more 
modelv <- lm(MPG ~ VOL,data=Cars)
summary(modelv)
modelwt <- lm(MPG ~ WT,data=Cars)
summary(modelwt)

modelvolwt <- lm(MPG ~ WT+VOL,data=Cars)
summary(modelvolwt)

#when merged together, both vol wt , become significant and there is
#coliniearity problem and one of them have to be removed
#Removal techniques
#find influence values
influence.measures(model.car)
influenceIndexPlot(model.car)#index plot for influence measures
influencePlot(model.car) #A user friendly representation of above

#remove the influencial values and check model 
model.carremove <- lm(MPG~VOL+SP+HP+WT,data=Cars[-c(1,71,77),])
summary(model.carremove)

#variance inflation factor
vif(model.car)

#added variable plot 
avPlots(model.car)

final_model <- lm (MPG~log(VOL)+log(SP)+log(HP))
summary(model.carremove)

