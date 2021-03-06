penalizedReg <- function(bwt,x,y,kfold,Interac = c("TRUE","FALSE")){
  ERateMin <- matrix(0,1,3)
  ERate1se <- matrix(0,1,3)
  ERateStep <- matrix(0,10,2)
  n <- nrow(bwt)
  ind <- sample(cut(seq(1,n),breaks=10,labels=F))
  
  # AIC,BIC criticle value
  p <- 0.5
  
  # Cross Validation
  for (i in 1:kfold){
    x.train <- x[-(ind==i),]
    x.test <- x[ind==i,]
    y.train <- y[-(ind==i)]
    y.test <- y[ind==i]
    SecERmin <- matrix(0,1,9)
    SecER1se <- matrix(0,1,9)
    for (j in 1:9){
      block <- sample(cut(seq(1,nrow(x.train)),breaks=10,labels=F))
      for (k in 1:10){
        #index <- which(block!=k, arr.ind=T)
        x.ttrain <- x.train[-(block==k),]
        x.ttest <- x.train[block==k,]
        y.ttrain <- y.train[-(block==k)]
        y.ttest <- y.train[block==k]
        fitt <- cv.glmnet(x.ttrain,y.ttrain,type.measure="class",alpha=j/10, family="binomial")
        SecERmin[1,j] <- sum(y.ttest!=predict(fitt,s=fitt$lambda.min, newx=x.ttest, type="class"))+SecERmin[1,j]
        SecER1se[1,j] <- sum(y.ttest!=predict(fitt,s=fitt$lambda.1se, newx=x.ttest, type="class"))+SecER1se[1,j]
      }         
    }
    # Elastic-net
    alphaMin <- which.min(SecERmin)/10
    alpha1se <- which.min(SecER1se)/10
    finalFitMin <- cv.glmnet(x.train, y.train,type.measure="class", alpha=alphaMin, family="binomial")
    finalFit1se <- cv.glmnet(x.train, y.train, type.measure="class", alpha=alpha1se, family="binomial")
    ERateMin[1,1] <- sum(y.test!=predict(finalFitMin, s=finalFitMin$lambda.min, newx=x.test, type = "class"))+ERateMin[1,1]
    ERate1se[1,1] <- sum(y.test!=predict(finalFit1se, s=finalFit1se$lambda.1se, newx=x.test, type="class"))+ERate1se[1,1]
    # Lasso
    LassoFit <- cv.glmnet(x.train, y.train, type.measure="class", alpha=1, family="binomial")
    ERateMin[1,2] <- sum(y.test!=predict(LassoFit, s=LassoFit$lambda.min, newx=x.test, type="class"))+ERateMin[1,2]
    ERate1se[1,2] <- sum(y.test!=predict(LassoFit, s=LassoFit$lambda.1se, newx=x.test, type="class"))+ERate1se[1,2]
    # Ridge
    RidgeFit <- cv.glmnet(x.train, y.train, type.measure="class", alpha=0, family="binomial")
    ERateMin[1,3] <- sum(y.test!=predict(RidgeFit, s=RidgeFit$lambda.min, newx=x.test, type="class"))+ERateMin[1,3]
    ERate1se[1,3] <- sum(y.test!=predict(RidgeFit, s=RidgeFit$lambda.1se, newx=x.test, type="class"))+ERate1se[1,3]
    
    # stepAIC and stepBIC
    fit0 <- glm(low ~ ., family = binomial, data = bwt, subset = (ind!=i))
    
    # with Interaction
    if (Interac==TRUE){
      # fit model
      fit2AIC <- stepAIC(fit0, ~.^2 + I(scale(age)^2) + I(scale(lwt)^2) )
      fit2BIC <- stepAIC(fit0, ~.^2 + I(scale(age)^2) + I(scale(lwt)^2) , k=log(nrow(bwt)) )
      # predict
      prefit2AIC <- predict(fit2AIC, newdata=bwt, type = 'response')[ind==i]
      prefit2BIC <- predict(fit2BIC, newdata=bwt, type = 'response')[ind==i]
    }
    # without interaction
    else{
      # fit model
      fit1AIC <- stepAIC(fit0, ~.)
      fit1BIC <- stepAIC(fit0, ~., k=log(nrow(bwt)))
      # predict
      prefit1AIC <- predict(fit1AIC, newdata=bwt, type = 'response')[ind==i]
      prefit1BIC <- predict(fit1BIC, newdata=bwt, type = 'response')[ind==i]
      # misclassification Rate
      preclass <- matrix(rep(0,sum(ind==i)*2),sum(ind==i),2)
      preclass[,1][prefit1AIC>p] <- 1
      preclass[,2][prefit1BIC>p] <- 1
      ERateStep[i,] <- colSums(preclass!=bwt$low[ind==i])
    }
    
  }   
  msMin <- ERateMin/n
  ms1se <- ERate1se/n
  msStep <- colSums(ERateStep)/n
  newlist <- list("msMin"=msMin, "ms1se"=ms1se, "msStep"=msStep)
  return (newlist)
}

library(MASS)
library(glmnet)
data("birthwt", package = "MASS")
bwt <- with(birthwt, {
  race <- factor(race, labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0)
  ftv <- factor(ftv)
  levels(ftv)[-(1:2)] <- "2+"
  data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})

# no interaction item
NoIntModel <- function(bwt,K,kfold){
  MSRate <- matrix(NA,K,8)
  set.seed(1233423)
  y <- bwt$low
  f <- as.formula(low  ~ .)
  x <- model.matrix(f,bwt)[,-1]
  for (k in 1:K){
    MSRate[k,] <- unlist(penalizedReg(bwt,x,y,kfold,Interac="FALSE"))
  }
  return (MSRate)
}    

# add interactions
IntModel <- function(bwt,K,kfold){
  set.seed(1233423)
  MscrMin <- matrix(NA,K,11)
  Mscr1se <- matrix(NA,K,11)
  f <- as.formula(low ~ .*.)
  y <- bwt$low
  x <- model.matrix(f,bwt)[,-1]
  for (k in 1:K){
    MscrMin[k,] <- penalizedReg(bwt,x,y,kfold)$msMin
    Mscr1se[k,] <- penalizedReg(bwt,x,y,kfold)$ms1se
  }
  Inte <- list("mscrMin"=MscrMin, "mscr1se"=Mscr1se)
  return (Inte)
}

NoInt <- NoIntModel(bwt,50,10)
Npng(filename = "/home/ailanshi/Downloads/568_A1/Nomin.png")
boxplot(NoInt$NmscrMin,main="Main Effects with min CV",names=c("Ridge","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","Lasso"))
dev.off()
png(filename = "/home/ailanshi/Downloads/568_A1/No1se.png")
boxplot(NoInt$Nmscr1se,main="Main Effects with 1se",names=c("Ridge","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","Lasso"))
dev.off()

Inte <- IntModel(bwt,100,10)
png(filename = "/home/ailanshi/Downloads/568_A1/Intemin.png")
boxplot(Inte$mscrMin,main="Add Interactions with min CV",names=c("Ridge","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","Lasso"))
dev.off()
png(filename = "/home/ailanshi/Downloads/568_A1/Inte1se.png")
boxplot(Inte$mscr1se,main="Add Interactions with 1se",names=c("Ridge","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","Lasso"))
dev.off()