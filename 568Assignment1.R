library(MASS)
data("birthwt", package = "MASS")
bwt <- with(birthwt, {
    race <- factor(race, labels = c("white", "black", "other"))
    ptd <- factor(ptl > 0)
    ftv <- factor(ftv)
    levels(ftv)[-(1:2)] <- "2+"
    data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})

# remove low weight 
allRowIndex <- c(1:nrow(bwt))
set.seed(1111)
removeLowIndex <- sample(allRowIndex[bwt$low==1],39)  ### set.seed?
bwtRemoveLow <- bwt[-removeLowIndex,]

# remove normal weight
set.seed(1232)
removeNormalIndex <- sample(allRowIndex[bwt$low==0],39)
bwtRemoveNormal <- bwt[-removeNormalIndex,]

# very small sample size
set.seed(1)
LowIndex <- sample(allRowIndex[bwt$low==1],20)
NormalIndex <- sample(allRowIndex[bwt$low==0],20)
bwtSmall <- bwt[c(LowIndex,NormalIndex),]

selectModel <- function(bwt,kfold){
    # split sample into kfold blocks
    n <- nrow(bwt)
    remainder <-  n%%kfold
    foldNum <- rep(floor(n/kfold),kfold)
    if (remainder>0) foldNum[1:remainder] <- foldNum[1:remainder]+1
    ind <- c()
    for (i in 1:kfold){
        temp <- rep(i,foldNum[i])
        ind <- c(ind,temp)
    }
    ind <- sample(ind)
    
    # criticle value
    #p <- sum(bwt$low==1)/n
    p <- 0.5

    # fit model
    ErrorRate <- matrix(NA,10,2)
    for (i in 1:kfold){
        fit0 <- glm(low ~ ., family = binomial, data = bwt, subset = (ind!=i))
        fit1AIC <- stepAIC(fit0, ~.)     
       # fit2AIC <- stepAIC(fit0, ~.^2 + I(scale(age)^2) + I(scale(lwt)^2) )
       # fit3AIC <- stepAIC(fit1AIC, ~.^2 + I(scale(age)^2) + I(scale(lwt)^2) )
        fit1BIC <- stepAIC(fit0, ~., k=log(nrow(bwt)))
       # fit2BIC <- stepAIC(fit0, ~.^2 + I(scale(age)^2) + I(scale(lwt)^2) , k=log(nrow(bwt)) )
       # fit3BIC <- stepAIC(fit1BIC, ~.^2 + I(scale(age)^2) + I(scale(lwt)^2), k=log(nrow(bwt)) )
     
        # predict
        prefit1AIC <- predict(fit1AIC, newdata=bwt, type = 'response')[ind==i]
        #prefit2AIC <- predict(fit2AIC, newdata=bwt, type = 'response')[ind==i]
        #prefit3AIC <- predict(fit3AIC, newdata=bwt, type = 'response')[ind==i]
        prefit1BIC <- predict(fit1BIC, newdata=bwt, type = 'response')[ind==i]
        #prefit2BIC <- predict(fit2BIC, newdata=bwt, type = 'response')[ind==i]
        #prefit3BIC <- predict(fit3BIC, newdata=bwt, type = 'response')[ind==i]

        # misclassification Rate
        preclass <- matrix(rep(0,sum(ind==i)*4),sum(ind==i),4)
        preclass[,1][prefit1AIC>p] <- 1
        #preclass[,3][prefit2AIC>p] <- 1
       # preclass[,3][prefit3AIC>p] <- 1
        preclass[,2][prefit1BIC>p] <- 1
        #preclass[,4][prefit2BIC>p] <- 1
       # preclass[,6][prefit3BIC>p] <- 1
        ErrorRate[i,] <- colSums(preclass==bwt$low[ind==i])
    }
    ER <- colSums(ErrorRate)/n
    return(ER)
}

Perfor <- c()
#########
set.seed(12344)
MSCRAll <- matrix(NA,100,4)
for (i in 1:100){
    ErrorRate <- selectModel(bwt,10)
    MSCRAll[i,] <- ErrorRate
}
write.table(MSCRAll, file="/home/ailanshi/Downloads/568_A1/All.csv",row.names=F,quote=F)
png(filename = "/home/ailanshi/Downloads/568_A1/All.png")
boxplot(MSCRAll[,1:2],main="all data",names=c("AIC","BIC"))
dev.off()
Perfor[1] <- sum(MSCRAll[,2]<=MSCRAll[,1])/100

set.seed(1233)
MSCRLow <- matrix(NA,100,4)
for (i in 1:100){
    ErrorRate <- selectModel(bwtRemoveLow,10)
    MSCRLow[i,] <- ErrorRate
}
write.table(MSCRLow, file="/home/ailanshi/Downloads/568_A1/Low.csv",row.names=F,quote=F)
png(filename = "/home/ailanshi/Downloads/568_A1/Low.png")
boxplot(MSCRLow[,1:2],main="Remove low",names=c("AIC","BIC"))
dev.off()
Perfor[2] <- sum(MSCRLow[,2]<=MSCRLow[,1])/100

set.seed(1233)
MSCRNormal <- matrix(NA,100,4)
for (i in 1:100){
    ErrorRate <- selectModel(bwtRemoveNormal,10)
    MSCRNormal[i,] <- ErrorRate
}
png(filename = "/home/ailanshi/Downloads/568_A1/Normal.png")
boxplot(MSCRNormal[,1:2],main="Remove Normal",names=c("AIC","BIC"))
dev.off()
Perfor[3] <- sum(MSCRNormal[,2]<=MSCRNormal[,1])/100

set.seed(1233)
MSCRSmall <- matrix(NA,100,4)
for (i in 1:100){
    ErrorRate <- selectModel(bwtSmall,10)
    MSCRSmall[i,] <- ErrorRate
}
png(filename = "/home/ailanshi/Downloads/568_A1/Small.png")
boxplot(MSCRSmall[,1:2],main="Small Sample Size",names=c("AIC","BIC"))
dev.off()
Perfor[4] <- sum(MSCRSmall[,2]<=MSCRSmall[,1])/100

# different split data, same method, boxplot
boxplot(MSCRAll[,1],MSCRLow[,1],MSCRNormal[,1],MSCRSmall[,1],main="AIC",names=c("All","-Low","-Normal","Small"),xlab="different split", ylab="misclassification rate")
boxplot(MSCRAll[,2],MSCRLow[,2],MSCRNormal[,2],MSCRSmall[,2],main="BIC",names=c("All","-Low","-Normal","Small"),xlab="different split", ylab="misclassification rate")
                                       
# for one same data, t-test
test = c()
test[1] <- t.test(MSCRAll[,1], MSCRAll[,2], alternative = c("two.sided"))$p.value
test[2] <- t.test(MSCRLow[,1], MSCRLow[,2], alternative = c("two.sided"))$p.value
test[3] <- t.test(MSCRNormal[,1], MSCRNormal[,2], alternative = c("two.sided"))$p.value
test[4] <- t.test(MSCRSmall[,1], MSCRSmall[,2], alternative = c("two.sided"))$p.value

# ANOVA, same method for different data split,
group <- as.factor(c(rep(1,100),rep(2,100),rep(3,100),rep(4,100)))
mscAIC <- c(MSCRAll[,1],MSCRLow[,1],MSCRNormal[,1],MSCRSmall[,1])
mscBIC <- c(MSCRAll[,2],MSCRLow[,2],MSCRNormal[,2],MSCRSmall[,2])
AICaov <- aov(mscAIC ~ group)
BICaov <- aov(mscBIC ~ group)
summary(AICaov)
summary(BICaov)


# BIC/AIC differential in different ratio of data
AllRatio <- MSCRAll[,2]/MSCRAll[,1]
LowRatio <- MSCRLow[,2]/MSCRLow[,1]
NormalRatio <- MSCRNormal[,2]/MSCRNormal[,1]
SmallRatio <- MSCRSmall[,2]/MSCRSmall[,1]
boxplot(AllRatio,LowRatio,NormalRatio,SmallRatio,main="BIC/AIC",names=c("All","-Low","-Normal","Small"),xlab="different split", ylab="BIC/AIC")
# anova
mscRatio <- c(AllRatio,LowRatio,NormalRatio,SmallRatio)
RatioAov <- aov(mscRatio ~ group)
summary(RatioAov)
        
