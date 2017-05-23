library(glmnet)

Xtrain <- read.table("Xtrain.csv", sep = ",", header = T)
Ytrain = read.table("Ytrain.csv",sep=";",header=TRUE)
Xtest <- read.table("Xtest.csv", sep = ",",header=TRUE)

for(i in 1:4031){
  for(j in c(1:48,73:96)){
    if (Ytrain[i,j]==0){
      Ytrain[i,j]=1
    }
  }
}


#### Regression pour PM2_5
# Moyenne des heures
meanPM2_h = rep(6,length = 24)
for(k in 1:24){
  a = 0
  for(i in 0:23){c
    h = which(Xtrain[,2] == i)
    if (i+k<=24){
      a = a + sum(log(Ytrain[h,i+k]))
    }else{
      a = a + sum(log(Ytrain[h,24-i]))
    }
  }
  meanPM2_h[k] = a/length(Xtrain)
}

PM2 = rowMeans(log(Ytrain[,1:24]))
meanPM2 = mean(PM2)
diff = meanPM2_h - meanPM2

# Lasso
lasso_PM2 = cv.glmnet(as.matrix(Xtrain[,-1]),PM2, family = "gaussian", alpha = 1)
plot(lasso_PM2)
c_PM2<-coef(lasso_PM2,s=exp(-1.9),exact=TRUE)
ind_PM2<-which(c_PM2!=0)
length(ind_PM2)

# BIC
Xtrain_lasso_PM2 = Xtrain[,ind_PM2]
Xtest_lasso_PM2 = Xtrain[, ind_PM2]
choix <- regsubsets(PM2~.,data = Xtrain_lasso_PM2[,-1],nbest=1,nvmax=30 ) 
plot(choix,scale="bic")
sum = summary(choix)
ind_BIC = which(sum$which[16,]==TRUE)

Xtrain_BIC_PM2 = Xtrain_lasso_PM2[,ind_BIC]

# Regression 
Xtest_BIC_PM2 = Xtest_lasso_PM2[,ind_BIC]
X = as.matrix(Xtrain_BIC_PM2[,-1])
Y = PM2
fit_ridge_PM2 = glmnet(X,Y, alpha = 0)
plot(fit_ridge_PM2)
predi_PM2 = predict(fit_ridge_PM2, Xtest_BIC_PM2)

predi_lasso_PM2_24fois = replicate(24,predi_lasso_PM2)


# mse lasso avec le diff
for(k in 1:24){
  for(i in 0:23){
    h = which(Xtest[,2] == i)
    if (i+k<=24){
      predi_lasso_PM2_24fois[h,i+k] = predi_lasso_PM2_24fois[h,i+k] + diff[k]
    }else{
      predi_lasso_PM2_24fois[h,24-i] = predi_lasso_PM2_24fois[h,24-i] + diff[k]
    }
  }
}


#### Regression pour PM10
# Moyenne des heures
meanPM10_h = rep(6,length = 24)
for(k in 25:48){
  a = 0
  for(i in 0:23){
    h = which(Xtrain[,2] == i)
    if (i+k<=48){
      a = a + sum(log(Ytrain[h,i+k]))
    }else{
      a = a + sum(log(Ytrain[h,48-i]))
    }
  }
  meanPM10_h[k-24] = a/length(Xtrain)
}

PM10 = rowMeans(log(Ytrain[,25:48]))
meanPM10 = mean(PM10)
diff_PM10 = meanPM10_h - meanPM10

# Lasso
lasso_PM10 = cv.glmnet(as.matrix(Xtrain[,-1]),PM10, family = "gaussian", alpha = 1)
c_PM10<-coef(lasso_PM10,s=exp(-2.8),exact=TRUE)
ind_PM10<-which(c_PM10!=0)
length(ind_PM10)

# Regression Lasso
Xtrain_lasso_PM10 = Xtrain[,ind_PM10]
Xtest_lasso_PM10 = Xtest[,ind_PM10]
reg_lasso_PM10 = lm(PM10~.,data = Xtrain_lasso_PM10[,-1])
predi_lasso_PM10 = predict(reg_lasso_PM10, Xtest_lasso_PM10)

# Dupliaction de predi 24fois pour avoir une matrice 
predi_lasso_PM10_24fois = replicate(24,predi_lasso_PM10)

# mse lasso avec le diff
for(k in 1:24){
  for(i in 0:23){
    h = which(Xtest[,2] == i)
    if (i+k<=24){
      predi_lasso_PM10_24fois[h,i+k] = predi_lasso_PM10_24fois[h,i+k] + diff_PM10[k]
    }else{
      predi_lasso_PM10_24fois[h,24-i] = predi_lasso_PM10_24fois[h,24-i] + diff_PM10[k]
    }
  }
}


#### Regression pour O3
meanO3_h = rep(6,length = 24)
for(k in 49:72){
  a = 0
  for(i in 0:23){
    h = which(Xtrain[,2] == i)
    if (i+k<=72){
      a = a + sum(log(Ytrain[h,i+k]))
    }else{
      a = a + sum(log(Ytrain[h,72-i]))
    }
  }
  meanO3_h[k-48] = a/length(Xtrain)
}


O3 = rowMeans(Ytrain[,49:72])
meanO3 = mean(O3)
diff_O3 = meanO3_h - meanO3

lasso_O3 = cv.glmnet(as.matrix(Xtrain[,-1]),O3, family = "gaussian", alpha = 1)
plot(lasso_O3)
c_O3<-coef(lasso_O3,s=exp(0.6),exact=TRUE)
ind_O3<-which(c_O3!=0)
length(ind_O3)

Xtrain_lasso_O3 = Xtrain[,ind_O3]
Xtest_lasso_O3 = Xtest[,ind_O3]
reg_lasso_O3 = lm(O3~.,data = Xtrain_lasso_O3[,-1])
predi_lasso_O3 = predict(reg_lasso_O3, Xtest_lasso_O3)

predi_lasso_O3_24fois = replicate(24,predi_lasso_O3)

# mse lasso avec le diff
for(k in 1:24){
  for(i in 0:23){
    h = which(Xtest[,2] == i)
    if (i+k<=24){
      predi_lasso_O3_24fois[h,i+k] = predi_lasso_O3_24fois[h,i+k] + diff_O3[k]
    }else{
      predi_lasso_O3_24fois[h,24-i] = predi_lasso_O3_24fois[h,24-i] + diff_O3[k]
    }
  }
}


##### Regression pour NO2
meanNO2_h = rep(6,length = 24)
for(k in 1:24){
  a = 0
  for(i in 0:23){
    h = which(Xtrain[,2] == i)
    if (i+k<=24){
      a = a + sum(log(Ytrain[h,72+i+k]))
    }else{
      a = a + sum(log(Ytrain[h,96-i]))
    }
  }
  meanNO2_h[k] = a/length(Xtrain)
}

NO2 = rowMeans(log(Ytrain[,73:96]))
meanNO2 = mean(NO2)
diff_NO2 = meanPM10_h - meanPM10

lasso_NO2 = cv.glmnet(as.matrix(Xtrain[,-1]),NO2, family = "gaussian", alpha = 1)
plot(lasso_NO2)
c_NO2<-coef(lasso_NO2,s=exp(-2.8),exact=TRUE)
ind_NO2<-which(c_NO2!=0)
length(ind_NO2)

Xtrain_lasso_NO2 = Xtrain[,ind_NO2]
Xtest_lasso_NO2 = Xtest[,ind_NO2]
reg_lasso_NO2 = lm(NO2~.,data = Xtrain_lasso_NO2[,-1])
predi_lasso_NO2 = predict(reg_lasso_NO2, Xtest_lasso_NO2)

predi_lasso_NO2_24fois = replicate(24,predi_lasso_NO2)

# mse avec le diff
for(k in 1:24){
  for(i in 0:23){
    h = which(Xtest[,2] == i)
    if (i+k<=24){
      predi_lasso_NO2_24fois[h,i+k] = predi_lasso_NO2_24fois[h,i+k] + diff_NO2[k]
    }else{
      predi_lasso_NO2_24fois[h,24-i] = predi_lasso_NO2_24fois[h,24-i] + diff_NO2[k]
    }
  }
}


### export
dim(predi_lasso_PM2_24fois)
dim(predi_lasso_PM10_24fois)
dim(predi_lasso_O3_24fois)
dim(predi_lasso_NO2_24fois)

predi_lasso_Ytest = cbind(exp(predi_lasso_PM2_24fois),exp(predi_lasso_PM10_24fois),predi_lasso_O3_24fois,exp(predi_lasso_NO2_24fois))
Y = rbind(names(Ytrain), predi_lasso_Ytest)
write.csv2(Y ,file =  "Ypredi.csv")



