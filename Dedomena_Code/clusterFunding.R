data <- read.csv(file="CLEANdataset.csv", header=TRUE, sep=",")
data<-as.data.frame(data)

#In order to cluster the given data, we need to divide the funding attribute
#into n tiers in order to get n clusters.
#Given other attributes of the dataset, the tier can then be predicted 
#in order to provide an estimate of the funding. 

###########################################################################
#creating a box plot of funding data
library(stringr)
options(scipen=999)
#boxplot(data$AmountInUSD,horizontal=TRUE,ylim=c(0,20000000))

x<-data$AmountInUSD
png(filename="D:/DA/fund_boxplot.png")
xout <- boxplot(x,range=1,horizontal=TRUE)$out
xin <- x[!(x %in% xout)]
noutl <- sum(xout<median(x))
nouth <- sum(xout>median(x))
boxplot(xin,horizontal=TRUE,ylim=c(min(xin)*1.15,max(xin)*1.15),main="Box plot of funding",xlab="Amount in USD")
text(x=max(xin)*1.1,y=1,labels=paste0(as.character(nouth)," >"))
dev.off()
q<-quantile(data$AmountInUSD)
q

###########################################################################
#diving the funding attribute into 5 tiers
#install.packages("dplyr")
library(dplyr)

q <- ntile(data$AmountInUSD, 5) 
data$tier<-q

print("TIER 1")
d<-data[data$tier==1,]
max(d$AmountInUSD)
min(d$AmountInUSD)

print("TIER 2")
d<-data[data$tier==2,]
max(d$AmountInUSD)
min(d$AmountInUSD)

print("TIER 3")
d<-data[data$tier==3,]
max(d$AmountInUSD)
min(d$AmountInUSD)

print("TIER 4")
d<-data[data$tier==4,]
max(d$AmountInUSD)
min(d$AmountInUSD)

print("TIER 5")
d<-data[data$tier==5,]
max(d$AmountInUSD)
min(d$AmountInUSD)

################################################################################
#text analysis to merge industry verticals under the same category
length(unique(data$IndustryVertical))
library(stringr)
#length(IV)[1] 516
fc<-c()
hc<-c()
ec<-c()
tc<-c()
gc<-c()
hoc<-c()
fac<-c()
bc<-c()
ec<-c()
vc<-c()
mc<-c()
bic<-c()
fic<-c()
fuc<-c()
for(i in 1:nrow(data)){
  iv1<-toString(data[i,]$IndustryVertical)
  if(is.na(str_locate(iv1,"health")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Health")[1,]["start"])==FALSE ||
     is.na(str_locate(iv1,"pharmacy")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Pharmacy")[1,]["start"])==FALSE ||
     is.na(str_locate(iv1,"doctor")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Doctor")[1,]["start"])==FALSE){
    hc<-c(hc,iv1)
  }
  else if(is.na(str_locate(iv1,"food")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Food")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"Restaurant")[1,]["start"])==FALSE || is.na(str_locate(iv1,"restaurant")[1,]["start"])==FALSE){
    fc<-c(fc,iv1)
  }
  else if(is.na(str_locate(iv1,"education")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Education")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"learning")[1,]["start"])==FALSE || is.na(str_locate(iv1,"E-learning")[1,]["start"])==FALSE ){
    ec<-c(ec,iv1)
  }
  else if(is.na(str_locate(iv1,"travel")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Travel")[1,]["start"])==FALSE){
    tc<-c(tc,iv1)
  }
  else if(is.na(str_locate(iv1,"grocery")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Grocery")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"grocers")[1,]["start"])==FALSE ){
    gc<-c(gc,iv1)
  }
  else if(is.na(str_locate(iv1,"hotel")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Hotel")[1,]["start"])==FALSE){
    hoc<-c(hoc,iv1)
  }
  else if(is.na(str_locate(iv1,"fashion")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Fashion")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"apparel")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Apparel")[1,]["start"])==FALSE){
    fac<-c(fac,iv1)
  }
  else if(is.na(str_locate(iv1,"beauty")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Beauty")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"spa")[1,]["start"])==FALSE){
    bc<-c(bc,iv1)
  }
  else if(is.na(str_locate(iv1,"ecommerce")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Ecommerce")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"eCommerce")[1,]["start"])==FALSE ||  is.na(str_locate(iv1,"E-Commerce")[1,]["start"])==FALSE ){
    ec<-c(ec,iv1)
  }
  else if(is.na(str_locate(iv1,"vehicle")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Vehicle")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"car")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Car")[1,]["start"])==FALSE){
    vc<-c(vc,iv1)
  }
  else if(is.na(str_locate(iv1,"mobile")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Mobile")[1,]["start"])==FALSE ){
    mc<-c(mc,iv1)
  }
  else if(is.na(str_locate(iv1,"big data")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Big data")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"analytics")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Analytics")[1,]["start"])==FALSE){
    bic<-c(bic,iv1)
  }
  else if(is.na(str_locate(iv1,"finance")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Finance")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"FinTech")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Financial")[1,]["start"])==FALSE){
    fic<-c(fic,iv1)
  }
  else if(is.na(str_locate(iv1,"home")[1,]["start"])==FALSE || is.na(str_locate(iv1,"Home")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"decor")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Decor")[1,]["start"])==FALSE ||
          is.na(str_locate(iv1,"furniture")[1,]["start"])==FALSE ||is.na(str_locate(iv1,"Furniture")[1,]["start"])==FALSE){
    fuc<-c(fuc,iv1)
  }
}

data["IndustryMain"]<-data["IndustryVertical"]
for(i in 1:nrow(data)){
  iv1<-toString(data[i,]$IndustryVertical)
  if(iv1 %in% hc){
    data[i,]$IndustryMain<-"Healthcare"
  }
  else if(iv1 %in% fc){
    data[i,]$IndustryMain<-"Food"
  }
  else if(iv1 %in% ec){
    data[i,]$IndustryMain<-"Education"
  }
  else if(iv1 %in% tc){
    data[i,]$IndustryMain<-"Travel"
  }
  else if(iv1 %in% gc){
    data[i,]$IndustryMain<-"Grocery"
  }
  else if(iv1 %in% hoc){
    data[i,]$IndustryMain<-"Hotel"
  }
  else if(iv1 %in% fac){
    data[i,]$IndustryMain<-"Fashion"
  }
  else if(iv1 %in% bc){
    data[i,]$IndustryMain<-"Beauty"
  }
  else if(iv1 %in% ec){
    data[i,]$IndustryMain<-"Ecommerce"
  }
  else if(iv1 %in% vc){
    data[i,]$IndustryMain<-"Vehicle"
  }
  else if(iv1 %in% mc){
    data[i,]$IndustryMain<-"Mobile"
  }
  else if(iv1 %in% bic){
    data[i,]$IndustryMain<-"Big Data and Analytics"
  }
  else if(iv1 %in% fic){
    data[i,]$IndustryMain<-"Finanace"
  }
  else if(iv1 %in% fuc){
    data[i,]$IndustryMain<-"Furniture"
  }
  
}
length(unique(data$IndustryMain))
#table(data$IndustryMain)
################################################################################


#assigning ranks to every attribute
rankdf<-data.frame()
cityRank <- factor(data[,"CityLocation"])
ranks <- rank(-table(cityRank), ties.method="first")
rankdf <- data.frame(category=cityRank, cityRank=ranks[as.character(cityRank)])

indRank <- factor(data[,"IndustryMain"])
ranks <- rank(-table(indRank), ties.method="first")
rankdf <- cbind(rankdf,data.frame(category=indRank, indRank=ranks[as.character(indRank)]))

subRank <- factor(data[,"SubVertical"])
ranks <- rank(-table(subRank), ties.method="first")
rankdf <- cbind(rankdf,data.frame(category=subRank, subRank=ranks[as.character(subRank)]))

typeRank <- factor(data[,"InvestmentType"])
ranks <- rank(-table(typeRank), ties.method="first")
rankdf <- cbind(rankdf,data.frame(category=typeRank, typeRank=ranks[as.character(typeRank)]))

rankdf$tier<-data$tier
rankdf$AmountInUSD<-data$AmountInUSD

#checking for na values
head(rankdf)
sum(is.na(rankdf$subRank))
sum(is.na(rankdf$typeRank))
sum(is.na(rankdf$cityRank))
sum(is.na(rankdf$indRank))

rankdf$mm<-data$mm
sum(is.na(rankdf$mm))

#dropping subtype due to large number of na values
rankdf$subRank<-NULL

#removing rows with na values
rankdf<-na.omit(rankdf)
nrow(rankdf)

###########################################################################
#plotting scatter plots of funding type, city, industry verical month against Amount 
#to see their correlation and to visualise linear regression best fit line

#funding type
cor(rankdf$AmountInUSD,rankdf$typeRank)
png(filename="D:/DA/amountVStype.png")
plot(AmountInUSD~typeRank,data=rankdf,main="Amount VS Funding type", ylab="Amount in USD", xlab="Funding type")
abline(lm(AmountInUSD~typeRank,data=rankdf),col="red")
dev.off()


#city
cor(rankdf$AmountInUSD,rankdf$cityRank)
png(filename="D:/DA/amountVScity.png")
plot(AmountInUSD~cityRank,data=rankdf,main="Amount VS City", ylab="Amount in USD", xlab="City")
abline(lm(AmountInUSD~cityRank,data=rankdf),col="red")
dev.off()

#industry vertical
cor(rankdf$AmountInUSD,rankdf$indRank)
png(filename="D:/DA/amountVSindustry.png")
plot(AmountInUSD~indRank,data=rankdf,main="Amount VS Industry Vertical", ylab="Amount in USD", xlab="Industry Vertical")
abline(lm(AmountInUSD~indRank,data=rankdf),col="red")
dev.off()

#month
cor(rankdf$AmountInUSD,rankdf$mm)
png(filename="D:/DA/amountVSmonth.png")
plot(AmountInUSD~mm,data=rankdf,main="Amount VS Month", ylab="Amount in USD", xlab="Month")
abline(lm(AmountInUSD~mm,data=rankdf),col="red")
dev.off()


###########################################################################
#MULTINOMIAL LOGISTIC REGRESSION
#install.packages("foreign")
#install.packages("nnet")
#install.packages("ggplot2")
#install.packages("reshape2")
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(caret)

#shuffling rows of the data frame for random sampling
set.seed(20)
rankdf <- rankdf[sample(nrow(rankdf)),]
#taking 70% training data
train<- 0.7*nrow(rankdf)
traindata<-rankdf[1:train,]
testdata<-rankdf[(train+1):nrow(rankdf),]

#with month
model<- multinom(tier~typeRank+indRank+cityRank+mm,family=binomial(link='logit'),data=traindata,MaxNWts =10000000)
summary(model)
#calculating Z score and p-Value for the variables in the model to find the significant attributes
z <- summary(model)$coefficients/summary(model)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p

#checking for fitted values
head(fitted(model))
p<-predict(model,testdata,type='class')
head(p)
library(caret)
cm<- confusionMatrix(p,testdata$tier)
accuracy <- cm$overall['Accuracy']*100
accuracy

#without month
model<- multinom(tier~typeRank+indRank+cityRank,family=binomial(link='logit'),data=traindata,MaxNWts =10000000)
summary(model)
#calculating Z score and p-Value for the variables in the model to find the significant attributes
z <- summary(model)$coefficients/summary(model)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p

#checking for fitted values
head(fitted(model))
p<-predict(model,testdata,type='class')
head(p)
library(caret)
cm<- confusionMatrix(p,testdata$tier)
accuracy <- cm$overall['Accuracy']*100
accuracy

#calculating rms error
rms<- 0
i<- 1
while(i<=nrow(testdata))
{
rms<-rms+((testdata[i,"tier"]-as.numeric(p[i]))*(testdata[i,"tier"]-as.numeric(p[i])))
i<-i+1
}
rms<-sqrt(rms/nrow(testdata))
rms

###########################################################################
#ORDINAL LOGISTIC REGRESSION
#install.packages("MASS")
#install.packages("Hmisc")
require(MASS)
require(Hmisc)

m <- polr(factor(tier) ~ typeRank+indRank+cityRank, data = traindata, Hess=TRUE)
summary(m)
head(predict(m, testdata, type = "p"))
p1 <- predict(m, testdata, type = "class")
cm<- confusionMatrix(p1,testdata$tier)
accuracy <- cm$overall['Accuracy']*100
accuracy

#calculating rms error
rms<- 0
i<- 1
while(i<=nrow(testdata))
{
rms<-rms+((testdata[i,"tier"]-as.numeric(p1[i]))*(testdata[i,"tier"]-as.numeric(p1[i])))
i<-i+1
}
rms<-sqrt(rms/nrow(testdata))
rms

###########################################################################
#LINEAR REGRESSION
#using linear regression to predict the tier
library(caret)
model2<-lm(tier~typeRank+indRank+cityRank+mm,data=traindata)
p2<-predict(model2,testdata,type='response')
AIC(model2)

model2<-lm(tier~typeRank+indRank+cityRank,data=traindata)
p2<-predict(model2,testdata,type='response')
AIC(model2)

p2<-as.numeric(floor(p2))
t<-table(factor(p2, levels=min(testdata$tier):max(testdata$tier)), factor(testdata$tier, levels=min(testdata$tier):max(testdata$tier)))
#accuracy
accuracy<-(sum(diag(t))/sum(t))*100
accuracy

p2<-as.numeric(round(p2))
t<-table(factor(p2, levels=min(testdata$tier):max(testdata$tier)), factor(testdata$tier, levels=min(testdata$tier):max(testdata$tier)))
#accuracy
accuracy<-(sum(diag(t))/sum(t))*100
accuracy

p2<-as.numeric(ceiling(p2))
t<-table(factor(p2, levels=min(testdata$tier):max(testdata$tier)), factor(testdata$tier, levels=min(testdata$tier):max(testdata$tier)))
#accuracy
accuracy<-(sum(diag(t))/sum(t))*100
accuracy

#calculating rms error
rms<- 0
i<- 1
while(i<=nrow(testdata))
{
rms<-rms+((testdata[i,"tier"]-as.numeric(p2[i]))*(testdata[i,"tier"]-as.numeric(p2[i])))
i<-i+1
}
rms<-sqrt(rms/nrow(testdata))
rms

#using linear regression to predict the amount
#model3<-lm(AmountInUSD~typeRank+indRank+cityRank+mm,data=traindata)
#p3<-predict(model3,testdata)
#calculating error
#p3<-as.numeric(p3)
#rms<- 0
#i<- 1
#while(i<=length(p3))
#{
#if(p3[i]>0) 
#{
#rms<-rms+((p3[i]-testdata$AmountInUSD[i])*(p3[i]-testdata$AmountInUSD[i]))
#}
#i<-i+1
#}
#rms<-sqrt(rms/length(p3))
#predicted values were negative, hence error was large. Dropping this model

###########################################################################
#MULTICLASS DECISION TREES
library(rpart)
fitTree <- rpart(factor(tier) ~ typeRank+indRank+cityRank,traindata)
newdata<-data.frame(testdata$tier,testdata$typeRank,testdata$indRank,testdata$cityRank)
names(newdata)<-c("tier","typeRank","indRank","cityRank")
p<-predict(fitTree,newdata,type="class")
cm<- confusionMatrix(p,testdata$tier)
accuracy <- cm$overall['Accuracy']*100
accuracy

#calculating rms error
rms<- 0
i<- 1
while(i<=nrow(testdata))
{
rms<-rms+((testdata[i,"tier"]-as.numeric(p[i]))*(testdata[i,"tier"]-as.numeric(p[i])))
i<-i+1
}
rms<-sqrt(rms/nrow(testdata))
rms
library(rpart.plot)
png(filename="D:/DA/decisionTree.png")
rpart.plot(fitTree)
dev.off()
###########################################################################
#CLUSTERING

names(traindata)
traindata["category"]<-NULL
traindata["category"]<-NULL
traindata["category"]<-NULL
traindata["category"]<-NULL
traindata["AmountInUSD"]<-NULL
traindata["mm"]<-NULL
names(traindata)

names(testdata)
testdata["category"]<-NULL
testdata["category"]<-NULL
testdata["category"]<-NULL
testdata["category"]<-NULL
testdata["AmountInUSD"]<-NULL
testdata["mm"]<-NULL
names(testdata)

set.seed(10)
fit<-kmeans(traindata[,"tier"],5,iter.max=200)
t<-table(fit$cluster,traindata$tier)

#every row is a cluster and every column is tier
#Label for each cluster
labels<-c()
for(i in 1:nrow(t)){
r<-t[i,]
index<-as.integer(which(r==max(r)))
labels[i]<-index
print(c("Cluster",i,"Label",index))
}
#Given cluster number, find its label
FindLabelGivenCluster<-function(clusternum){
return(labels[clusternum])
}

#Centers has the centers generated from k means clustering
centers<-fit$centers

#Computes eucleadian distance between two vectors
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#Compute distance between one row from test data to all rows from centers data
DistanceFromCenters<-function(v){
distance_vector<-c()
for(i in 1:nrow(centers)){
#for every row in centers
c<-centers[i,]
#compute eucledian distance between centers cluster and v
euc<-euc.dist(v,c)
#print(euc)
#append to distance vector
distance_vector<-c(distance_vector,euc)
}
#Distance vector contains distances of v from each cluster
return(distance_vector)
}

#Label test data
for(i in 1:nrow(testdata)){
row<-testdata[i,]
row<-row[,c("cityRank","indRank","typeRank")]
#compute distance vector
distance_vector<-DistanceFromCenters(row)
#Minimum distance
min_index<-which(distance_vector==min(distance_vector))
cluster_number<-min_index[1]
label<-FindLabelGivenCluster(cluster_number)
testdata[i,"pred"]<-label
}

head(testdata)

#calculating rms error
rms<- 0
i<- 1
while(i<=nrow(testdata))
{
rms<-rms+((testdata[i,"tier"]-testdata[i,"pred"])*(testdata[i,"tier"]-testdata[i,"pred"]))
i<-i+1
}
rms<-sqrt(rms/nrow(testdata))
rms

#plotting the clusters
#library(cluster)
#library(fpc)
#plotcluster(traindata$tier,fit$cluster)
###########################################################################

