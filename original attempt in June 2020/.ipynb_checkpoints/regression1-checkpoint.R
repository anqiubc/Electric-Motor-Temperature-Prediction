dataset=read.csv("4.csv",header=T)
dataset
attach(dataset)
cor(dataset)

library(ggplot2)
library(reshape2)
data_m <- melt(dataset)
head(data_m)
p <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p

p <- ggplot(data_m, aes(x=variable, y=value),color=variable) + 
  geom_violin(aes(fill=factor(variable))) + 
  theme(axis.text.x=element_text(angle=50,hjust=0.5, vjust=0.5)) +
  theme(legend.position="none")
p


ggplot(dataset,aes(x=pm))+geom_histogram(binwidth=0.5,fill="lightblue",colour="black")

model2=lm(pm~ambient+coolant+u_d+u_q+motor_speed+torque+i_d+i_q,data=dataset)
model2
summary(model2)

plot(fitted(model2),resid(model2))
abline(h=0)
plot(fitted(model2),sqrt(abs(resid(model2))))
abline(h=0)

summary(lm(sqrt(abs(residuals(model2)))~fitted(model2)))

qqnorm(resid(model2))
qqline(resid(model2))

require(lmtest)
dwtest(pm~ambient+coolant+u_d+u_q+motor_speed+torque+i_d+i_q,data=dataset)

n=length(residuals(model2))
plot(tail(residuals(model2),n-1)~head(residuals(model2),n-1),xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
summary(lm(tail(residuals(model2),n-1)~head(residuals(model2),n-1)))

library(faraway)
hatv=hatvalues(model2)
head(hatv)
halfnorm(hatv,ylab="leverages")

stud=rstudent(model2)
stud[which.max(abs(stud))]
qt(0.05/(33423*2),33418)
stud[abs(stud)>abs(qt(0.05/(33423*2),33418))]

cook=cooks.distance(model2)
sort(cook)
halfnorm(cook,10,ylab="Cook's distances")

library(faraway)
x=model.matrix(model2)[,-1]
e=eigen(t(x)%*%x)
e$val
sqrt(e$val[1]/e$val) 

require(faraway)
vif(x)

require(leaps)
b=regsubsets(pm~ambient+coolant+u_d+u_q+motor_speed+torque+i_d+i_q,data=dataset)
rs=summary(b)

rs$which

plot(2:9,rs$adjr2)

which.max(rs$adjr2)

require(leaps)
b=regsubsets(pm~ambient+coolant+u_d+u_q+motor_speed+torque+i_d+i_q,data=dataset)
rs=summary(b)

plot(2:9,rs$cp)
abline(0,1)

require(leaps)
b=regsubsets(pm~ambient+coolant+u_d+u_q+motor_speed+torque+i_d+i_q,data=dataset)
rs=summary(b)
AIC=33423*log(rs$rss/33423)+(2:9)*2
par(mfrow=c(1,1))
plot(AIC~I(1:8))

model21=lm(pm~ambient+coolant+u_d+u_q+motor_speed+torque+i_d+i_q)
summary(model21)

model22=lm(pm~ambient+u_d+u_q+motor_speed+torque+i_d+i_q)
summary(model22)

model3=lm(pm~ambient+u_q+motor_speed+torque+i_d+i_q)
summary(model3)

library(faraway)
x=model.matrix(model3)[,-1]
e=eigen(t(x)%*%x)
e$val
sqrt(e$val[1]/e$val) 

require(faraway)
vif(x)

model4=lm(pm~ambient+u_q+motor_speed+torque+i_d)
summary(model4)

library(faraway)
x=model.matrix(model4)[,-1]
e=eigen(t(x)%*%x)
e$val
sqrt(e$val[1]/e$val) 

require(faraway)
vif(x)

plot(fitted(model4),resid(model4))
abline(h=0)

qqnorm(resid(model4))
qqline(resid(model4))

require(lmtest)
dwtest(pm~ambient+u_q+motor_speed+torque+i_d,data=dataset)

n=length(residuals(model4))
plot(tail(residuals(model4),n-1)~head(residuals(model4),n-1),xlab=expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
summary(lm(tail(residuals(model4),n-1)~head(residuals(model4),n-1)))

cook=cooks.distance(model4)
sort(cook)
halfnorm(cook,3,ylab="Cook's distances")

#GLS
require(nlme)
model5=gls(pm~ambient+u_q+motor_speed+torque+i_d, corr=corAR1(),data=dataset)
summary(model5)

#Ridge regression
rmse=function(x,y) sqrt(mean((x-y)^2))
ridgedata=data.frame(scale(dataset[,9],center=T,scale=F),scale(dataset[,c(1,4,5,6,7,8)],center=T,scale=T))
names(ridgedata)=names(dataset[,c(9,1,4,5,6,7,8)])

set.seed(123)
index<-sample(2,nrow(ridgedata),replace=TRUE,prob=c(0.8,0.2))
traindata=ridgedata[index==1,]
testdata=ridgedata[index==2,]

library(MASS)
rgmod=lm.ridge(pm~ambient+u_q+motor_speed+torque+i_d+i_q,data=traindata,lambda=seq(0,1,0.01))
matplot(rgmod$lambda,type='l',coef(rgmod),xlab=expression(lambda),ylab=expression(hat(beta)))

select(rgmod)
abline(v=0.006)

ridgemodel=lm.ridge(pm~ambient+u_q+motor_speed+torque+i_d+i_q,data=traindata,lambda=0.006)
ypred=cbind(1,as.matrix(traindata[,-1]))%*%coef(ridgemodel)
rmse(ypred,traindata[,1])

ridgemodel2=lm.ridge(pm~ambient+u_q+motor_speed+torque+i_d+i_q,data=testdata,lambda=0.006)
ypred=cbind(1,as.matrix(traindata[,-1]))%*%coef(ridgemodel2)
rmse(ypred,testdata[,1])

#Lasso Regression
y=traindata[,1]
x=traindata[,-1]

require(lars)
lassomod=lars(as.matrix(x),as.matrix(y))
summary(lassomod)

plot(lassomod)

set.seed(123)
tcv=cv.lars(as.matrix(x),as.matrix(y))
t=tcv$index[which.min(tcv$cv)]
round(predict(lassomod,s=t,type="coef",mode="fraction")$coef,3)

trainx=as.matrix(traindata[,-1])
predlars=predict(lassomod,trainx,1,mode="fraction")
rmse(traindata$pm,predlars$fit)

testx=as.matrix(testdata[,-1])
predlars=predict(lassomod,testx,1,mode="fraction")
rmse(traindata$pm,predlars$fit)

require(msgps)

alasso=msgps(as.matrix(x),y,penalty="alasso",gamma=1,lambda=0)
summary(alasso)
plot(alasso)

ypred1=predict(alasso,as.matrix(x))

rmse(y,ypred1[,1])
rmse(y,ypred1[,2])
rmse(y,ypred1[,3])
rmse(y,ypred1[,4])

testx=as.matrix(testdata[,-1])
predmsgps=predict(alasso,testx)
rmse(testdata$pm,predmsgps[,1])
rmse(testdata$pm,predmsgps[,2])
rmse(testdata$pm,predmsgps[,3])
rmse(testdata$pm,predmsgps[,4])

#PCA
pcrdata=prcomp(as.matrix(x))
summary(pcrdata)

plot(pcrdata$sdev[1:6],type="l")

round(pcrdata$rot[,1:2],3)
matplot(1:6,pcrdata$rot[,1:2],type="l",xlab="x",ylab="",col=1)

pcr=lm(pm~pcrdata$x[,1:2],data=traindata)
summary(pcr)

require(pls)
pcrmod=pcr(pm~.,data=traindata,ncomp=6)

rmse(predict(pcrmod,ncomp=2),traindata$pm)

pcrmse=RMSEP(pcrmod,data=traindata)
plot(pcrmse,main="")

rmse(predict(pcrmod,ncomp=4),y)
rmse(predict(pcrmod,ncomp=6),y)

rmse(predict(pcrmod,testdata,ncomp=6),testdata$pm)

plot(ridgedata, main="scaled data")

y=ridgedata[,1]
x=ridgedata[,-1]
pcrdata=prcomp(as.matrix(x))

summary(pcrdata)

plot(pcrdata$x, main="after PCA")

scores <- pcrdata$x
variance <- (pcrdata$sdev)^2 

loadings <- pcrdata$rotation 
rownames(loadings) <- colnames(x) 

variance.per <- round(variance/sum(variance)*100,1) 

barplot(variance.per,main = "Sreen Plot",xlab = "Princioal Component",ylab = "percent Variation")
library(ggplot2)
pca.data <- data.frame(Sample=rownames(pcrdata$x),X=pcrdata$x[,1],Y=pcrdata$x[,2])
ggplot(data = pca.data,aes(x=X,y=Y,label=Sample))+
  geom_text()+
  xlab(paste("PC1 - ",variance.per[1],"%",sep = ""))+
  ylab(paste("PC2 - ",variance.per[2],"%",sep = ""))+
  theme_bw()+
  ggtitle("My PCA Graph")

round(loadings,2)

biplot(scores[, 1:2], loadings[, 1:2], cex=0.7) 

plot(scores[, 1], scores[, 2], xlab='PC 1', ylab='PC 2'
, type='n',xlim=c(min(scores[, 1:2]), max(scores[, 1:2])), ylim=c(min(scores[, 1:2]), max(scores[, 1:2])), las=1) 
text(scores[, 1],scores[, 2], rownames(scores), col='gray', cex=0.7) 
scale <- 5
arrows(0, 0, loadings[, 1]*scale, loadings[, 2]*scale, length=0.1, angle=20, col='red') 
labelScale <- 1.2 
text(loadings[, 1]*scale*labelScale, loadings[, 2]*scale* labelScale, rownames(loadings), col='red', cex=0.7) 

sd <- pcrdata$sdev 
plot(scores[,1]/sd[1], scores[, 2]/sd[2], xlab='PC 1', ylab='PC 2', type='n', las=1)
text(scores[,1]/sd[1],scores[, 2]/sd[2], rownames(scores), col='gray', cex=0.7) 
arrows(0, 0, loadings[, 1]*sd[1], loadings[, 2]*sd[2], length=0.1, angle=20, col='red') 
labelScale <- 1.2 
text(loadings[, 1]*sd[1]* labelScale, loadings[, 2]*sd[2]* labelScale, rownames(loadings), col='red', cex=0.7) 

correlations <- t(loadings) * sd

#Random Forest
library(randomForest)

dataset=dataset[,c(1:9)]

set.seed(100)
index=sample(2,nrow(dataset),replace=TRUE,prob=c(0.8,0.2))
train=dataset[index==1,]
test=dataset[index==2,]

n=length(names(train))
for(i in 1:n){
  mtry_fit=randomForest(pm~.,data=train,mtry=i)
  err=mean(mtry_fit$mse)
  print(err)
}

ntree_fit=randomForest(pm~.,data=train,mtry=4,ntree=1000)
plot(ntree_fit)

rf=randomForest(pm~.,data=train,mtry=4,ntree=400)
print(rf)

varImpPlot(rf)

forest_pred=predict(rf,newdata=test)

forest_rmse=sqrt(1/length(forest_pred)*sum((test$pm-forest_pred)**2))

