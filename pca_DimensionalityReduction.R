mydata <- read.csv("C:/Users/b60552/Desktop/p/ml/datasets/Universities.csv", header=TRUE)
#help("princomp")

data <-mydata[,-1]
View(data)
attach(data)
cor(data)
summary(data)
pcaobj <- princomp(data,cor=TRUE)
summary(pcaobj)
str(pcaobj)
loadings(pcaobj)
windows()
plot(pcaobj)
pcaobj$scores[,1:3]
mydata <- cbind(mydata,pcaobj$scores[,1:3])
View(mydata)
dim(mydata)
###here ends the dimensionality reduction part##
#now with reduced columns inmplement clustering
newdata <- mydata[,8:10]
normalized_data <- scale(newdata)
dist1 <- dist(normalized_data,method = "euclidean")
dist1
#heirarchical implementation
fit1 <- hclust(dist1,method = "complete")
windows()
plot(fit1,hang=-2)
groups <- cutree(fit1,5)
groups
membership_1 <- as.matrix(groups)
View(membership_1)
final <- cbind(membership_1,mydata)
View(final)

x <- aggregate(final[,-c(2,9:11)],by=list(membership_1),FUN=mean)#cluster final
x
#
attach(final)
M1 <- lm(GradRate~ Comp.1+Comp.2+Comp.3,data=final)
summary(M1)
M2 <- lm(GradRate ~ final$SAT+final$Top10+final$Accept+final$Expenses+final$GradRate)
summary(M2)
 