### load dataset ###
credit <- read.csv("C:\\Users\\Armagan\\Downloads\\German.csv")

### split dataset ###
set.seed(1024)
split <- sample(1000, 1000*(2/3))
cr_train <- credit[split,]
cr_test <- credit[-split,]

### ridge regression ###
library(glmnet)
ytr <- cr_train[,1]
xtr <- cr_train[,-1]

xtr$A2 <- scale(xtr$A2)
xtr$A5 <- scale(xtr$A5)
xtr$A13.1 <- scale(xtr$A13.1)
Xtr <- data.matrix(xtr)
fit5 <- glmnet(Xtr, ytr, alpha=0, family = 'binomial')
plot(fit5)
plot(fit5,xvar = "lambda", label = T)
cvfit1 <- cv.glmnet(Xtr, ytr,alpha=0, family = 'binomial', type.measure = 'auc')
plot(cvfit1)
l1 <- c(cvfit1$lambda.min, cvfit1$lambda.1se)
coef(cvfit1)

p1 <- predict(cvfit1, Xtr,alpha=0, type="response", s="lambda.min")

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
pro[50]
