library(glmnet)
library(MASS)
set.seed(19873)
n <- 100
p <- 50
CovMatrix <- outer(1:p, 1:p, function(x,y) {.7^abs(x-y)})
x <- mvrnorm(n,rep(0,p),CovMatrix)
#y <- 10*rowSums(x[,1:2])+5*rowSums(x[,3:4])+rowSums(x[,5:14])
y <- 10*apply(x[,1:2],1,sum)+5*apply(x[,3:4],1,sum)+apply(x[,5:14],1,sum)
p <- exp(y)/(1+exp(y))
y <- rbinom(n,1,p)
trainRows <- sample(1:n,.66*n)
x.train <- x[trainRows, ]
x.test <- x[-trainRows, ]

y.train <- y[trainRows]
y.test <- y[-trainRows]

for (i in 0:10){
    assign(paste("fit",i,sep=""), cv.glmnet(x.train,y.train,type.measure="class", alpha=i/10,family="binomial"))
    fitt <- get(paste("fit",i,sep=""))
    assign(paste("Alpha",i/10,sep=""),1-sum(predict(fitt, s=fitt$lambda.1se, newx=x.test, type="class")==y.test)/length(y.test))
}

mscRate <- matrix(NA, 11,1)
rowname <- c()
for (i in 0:10){
    mscRate[i+1,1] <- get(paste("Alpha",i/10,sep=""))
    rowname <- rbind(rowname,paste("Alpha",i/10,sep=""))
}

rownames(mscRate) <- rowname
mscRate
minErrorNum <- apply(mscRate,2,which.min)
plot((0:10)/10,mscRate,type='b',xlab="alpha")

fit.lasso <- glmnet(x.train, y.train, family="binomial",alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="binomial",alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="binomial", alpha=minErrorNum/10)

par(mfrow=c(3,2))
plot(fit.lasso, xvar = "lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar = "lambda")
plot(get(paste("fit",minErrorNum,sep="")), main = "Elastic Net")
