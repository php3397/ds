claimants <- read.csv("C:/Users/b60552/Desktop/p/ml/datasets/claimants.csv")
attach(claimants)
summary(claimants)
head(claimants,10)
names(claimants)
str(claimants)


cor(claimants)
windows()
plot(claimants)
#CLMAGE+CLMINSUR+CLMSEX+LOSS+SEATBELT
fit <- glm(ATTORNEY ~ CLMAGE+CLMINSUR+CLMSEX+LOSS+SEATBELT,data=claimants,family = "binomial")
summary(fit)

#influence.measures(as.factor(SEATBELT))

probabilities <- predict(fit,type = "response",claimants)
dfprob <- as.data.frame(probabilities)
View(dfprob)
prob1 <- cbind(dfprob,claimants)

#check accuracy of logistic regression using confusion matrix
table(probabilities > 0.5)
confusion <- table(prob1$probabilities > 0.5,prob1$ATTORNEY)
#View(prob1)
confusion
#calculate accuracy
#acc = total correct prediction/overall prediction   773/1098 = 70%

accuracy <- sum(diag(confusion)/sum(confusion) )
accuracy

#ROC curve
library(ROCR)
#ROC curve :The ROC curve is created by plotting the true positive rate 
#(TPR) against the false positive rate (FPR) at various threshold settings.
#The true-positive rate is also known as sensitivity, recall or probability of detection[5] 


