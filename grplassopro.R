### load dataset ###
credit <- read.csv("D:/DATA/German.csv")

### split dataset ###
set.seed(1024)
split <- sample(1000, 1000*(2/3))
cr_train <- credit[split,]
cr_test <- credit[-split,]

### group lasso regression ###
group <- factor(c(1,1,1,2,3,3,3,3,4,4,4,4,4,4,4,4,4,5,6,6,6,6,7,7,
                  7,7,8,9,9,9,10,10,11,12,12,12,13,14,14,15,15,16,
                  17,17,17,18,19,20), labels = c(1:20));group
library(grpreg)
ytr <- cr_train[,1]
xtr <- cr_train[,-1]

xtr$A2 <- scale(xtr$A2)
xtr$A5 <- scale(xtr$A5)
xtr$A13.1 <- scale(xtr$A13.1)
Xtr <- data.matrix(xtr)
fit4 <- grpreg(xtr, ytr, group, penalty="grLasso", family = 'binomial')
plot(fit4)
select(fit4,'BIC')
select(fit4,'AIC')
select(fit4,'GCV')
par(mfrow=c(1,3))
l4 <- fit4$lambda
xlim <- rev(range(l4))
plot(l4, select(fit4,"BIC")$IC, xlim=xlim, pch=19, type="o", ylab="BIC")
plot(l4, select(fit4,"AIC")$IC, xlim=xlim, pch=19, type="o",ylab="AIC")
plot(l4, select(fit4,"GCV")$IC, xlim=xlim, pch=19, type="o",ylab="GCV")
cvfit3 <- cv.grpreg(xtr, ytr, group, penalty="grLasso", family = 'binomial')
par(mfrow=c(1,1))
plot(cvfit3)
coef(cvfit3)
l3 <- cvfit3$lambda.min

p1 <- predict(cvfit3, X=Xtr, type="response", lambda = l3)










really <- cr_train$Y

test_log <- data.frame(really,p1)

pro <- seq(0.01,1,0.01)
p <- data.frame(matrix(NA,666,100))
for(i in 1:100){
  p[,i] =ifelse(p1<pro[i],'0','1')
  
  
}
res1 <- data.frame(really,p)

right1_11 <- data.frame(matrix(NA,666,100))
for(i in 1:666){
  for(j in 2:101){
    if(res1$really[i]=='1' & res1[,j][i]=='1'){
      right1_11[i,j]=1
    }
  }
}
right1_00 <- data.frame(matrix(NA,666,100))
for(i in 1:666){
  for(j in 2:101){
    if(res1$really[i]=='0' & res1[,j][i]=='0'){
      right1_00[i,j]=1
    }
  }
}
right1_00 <- right1_00[,-1]
l11 <- colSums(right1_11, na.rm = T)
l10 <- colSums(right1_00, na.rm = T)
TR11 <- (l11+l10)
TR1 <- (1/666)*TR11
which.max(TR1)
pro[45]
