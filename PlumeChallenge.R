library(Metrics)
library(ggplot2)
library(stringr)
library(glmnet)
library(leaps)
library(MASS)

# Import des données
Xtrain = read.table("Xtrain.csv",sep=",",header=TRUE)
attach(Xtrain)
head(Xtrain[,1:5])
n_train = nrow(Xtrain)

Ytrain = read.table("Ytrain.csv",sep=";",header=TRUE)
dim(Ytrain)

Xtest = read.table("Xtest.csv",sep=",",header=TRUE)
head(Xtest[,1:5])
n_test = nrow(Xtest)

# Transformation des dates et représentation
## as.POSIXct
date_train <- sapply(strsplit(as.character(Xtrain[,1]), " "), "[",1)
head(date_train)
date_train = as.POSIXct(date_train,format='%Y-%m-%d')
date_test =  sapply(strsplit(as.character(Xtest[,1]), " "), "[", 1)
date_test = as.POSIXct(date_test,format='%Y-%m-%d')#(Xtest[,1])
head(date_test)
#CEST : central european summer time
#CET : central european time

## Répartition des dates entre train et test
plot(date_train, rep(1,n_train))
points(date_test, rep(1,n_test),col="red")

# Représentation des Y sur les 56 premiers jours
## Il y a 4032 observations prises toutes les heures.Cela fait 4032/24=168 jours
## On répresente les Y des 5 premiers jours

#PM2_5 : 
par(mfrow=c(2,2))
plot(1:24,Ytrain[1,1:24],type="l",xlab="heures", ylab="PM2_5")
for (k in 2:5){
  lines(c(1:24),Ytrain[((24*k)+1),(1:24)],type="l",col=k)
}

#PM10
plot(1:24,Ytrain[1,25:48],type="l",xlab="heures", ylab="PM10")
for (k in 2:5){
  lines(c(1:24),Ytrain[((24*k)+1),(25:48)],type="l",col=k)
}

#O3
plot(1:24,Ytrain[1,49:72],type="l",xlab="heures", ylab="O3")
for (k in 2:5){
  lines(c(1:24),Ytrain[((24*k)+1),(49:72)],type="l",col=k)
}

#NO2
plot(1:24,Ytrain[1,73:96],type="l",xlab="heures",ylab="NO2")
for (k in 2:5){
  lines(c(1:24),Ytrain[((24*k)+1),(73:96)],type="l",col=k)
}


# On va regarder la répartition des taux de PM2_5_1 : C'est à dire le taux de PM2_5 1h après la mesure de l'observation en question.
## Histogramme
head(Xtrain)
library(ggplot2)#aller voir de la documentation sur ggplot2 pour comprendre la construction des graphes
heure =as.factor(Xtrain[,2])
jour =as.factor(Xtrain[,3])
df = data.frame(jour = jour,  heure = heure, y =as.numeric(Ytrain[,1]))#,25,49,73)])
head(df)
m <- ggplot(df, aes(x=y))+geom_histogram(aes(y = ..density..))+geom_density()
plot(m)

## L'histogramme n'étant pas symétrique, il faut transformer Y avec un log :
m <- ggplot(df, aes(x=log(y)))+geom_histogram()
m <- ggplot(df, aes(x=log(y)))+geom_histogram(aes(y = ..density..))+geom_density()
plot(m)
log(mean(as.numeric(Ytrain[,1])))

## Y a t il un effet jour ?
ggplot(df, aes(x = jour, y = log(y),fill=jour)) +
  geom_boxplot() 
#Le taux median semble diminuer un peu le weekend. Il bat son plein entre mercredi et vendredi

## Y a t il un effet heure ?
p=ggplot(df, aes(x = heure, y = log(y), fill=heure)) +
  geom_boxplot() 
plot(p)
#le taux médian augmente légèrement avec l'heure tandis que la variance elle diminue avec l'heure.
#Notons que cette variation du taux en fonction de l'heure va nous servir par la suite. 
#Nous allons calculer le taux moyen sur une journée puis le taux heure par heure puis calculer la différence pour connaitre 
#l'écart à la moyenne de chaque heure.

# On regarde maintenant la 1ere colonne de PM10
## Histogramme
df = data.frame(jour = jour,heure = heure, y =as.numeric(Ytrain[,25]))#,25,49,73)])
m <- ggplot(df, aes(x=y))+geom_histogram(aes(y = ..density..))+geom_density()
plot(m)

## Il faut transformer Y avec log
m <- ggplot(df, aes(x=log(y)))+geom_histogram(aes(y = ..density..))+geom_density()
plot(m)

## Y a t il un effet jour ?
ggplot(df, aes(x = jour, y = log(y), fill=jour)) +
  geom_boxplot() 
#diminution le weekend puis augmente petit à petit durant la semaine.

## Y a t il un effet heure ?
ggplot(df, aes(x = heure, y = log(y), fill=heure)) +
  geom_boxplot() 
#Faible avant 7h puis augmente,diminue à partir de midi et réaugmente à partir de 19h. A nouveau la variance est plus faible le soir 


# On regarde maintenant la 1ere colonne de O3
## Histogramme
df = data.frame(jour=jour,heure = heure, y =as.numeric(Ytrain[,49]))#,25,49,73)])
m <- ggplot(df, aes(x=y))+geom_histogram(aes(y = ..density..))+geom_density()
plot(m)

## Y a t il un effet jour ?
ggplot(df, aes(x = jour, y = y, fill=jour)) +
  geom_boxplot() 
#pas d'effet jour

## Y a t il un effet heure ?
ggplot(df, aes(x = heure, y = y, fill=heure)) +
  geom_boxplot() 
#diminue à partir de 6h, augmente vers 8h et diminue à nouveau vers 16h. La variance est plus élevée quand on est dans 
#un maximum ou minimum


# On regarde maintenant la 1ere colonne de NO3
## Histogramme
df = data.frame(heure = heure, y =as.numeric(Ytrain[,73]))#,25,49,73)])
m <- ggplot(df, aes(x=y))+geom_histogram(aes(y = ..density..))+geom_density()
plot(m)

## Il faut transformer Y avec log
m <- ggplot(df, aes(x=log(y)))+geom_histogram()
plot(m)

## Y a t il un effet jour ?
ggplot(df, aes(x = jour, y = log(y), fill=jour)) +
  geom_boxplot() 
#commes PM2_5 et PM10 : augmente dans la semaine et diminue le weekend

## Y a t il un effet heure ?
ggplot(df, aes(x = heure, y = log(y), fill=heure)) +
  geom_boxplot() 
#oui il y a un effet heure : ça augmente vers 5-6h, diminue vers 9h-10h et réaugmente vers 15h


#Globalement, mis à part l'O3, on voit une opposiation semaine/weekend ainsi qu'une augmentation 
#des taux le matin et le soir certainement au moment de prendre la voiture pour aller au travail.

## Nous allons créer nos propres echantillons apprentissage/test :
### Il faut comprendre comment le (vrai) test a été construit !
plot(c(600:700),date_train[600:700])
plot(c(600:699),diff(date_train[600:700]))
#On voit que la date passe du 25 février au 6 mars dans l'echantillon d'apprentissage 
#(saut sur le graphe de gauche et point très haut sur le graphe de droite : les petits sauts étant le passage d'un jour à un autre)
which(diff(date_train)==950400)#On cherche les grand saut pour voir où les données d'apprentissage ont été récupérer pour faire l'echantillon test
diff(which(diff(date_train)==950400))#ligne inutile je crois
date_train[665:672]#on voit le premier saut de date
date_test[1:10]#cela correspond au premières dates de l'echantillon test : on remarque que le jour 26 a été supprimé.
#Xtrain commence le 28janvier à 3h et a sa première coupure le 25 fevrier à 1h
#La première partie de Xtest se fait le 27 fevrier à 2h et se termine le 6 mars à 1h
#Xtrain reprend le 8 mars à 2h 

#La prise de données pour Xtest se fait toutes les 672 observations (soit tous les 28 jours: 672/24)
#on supprime les 0 de Ytrain qui peuvent nous géner avec l'application du log.
for(i in 1:4031){
  for(j in 1:96){
    if (Ytrain[i,j]==0){
      Ytrain[i,j]=1
    }
  }
}
#Il faudra par la suite les retransformer en récupérant les indices où Ytrain=0 

#on a pris des données pour Xtest juste après les données qu'ils ont eux-même pris pour le test. 
#On a, par ailleurs, pris le même pourcentage des données qu'eux pour former l'echantillon test.
Xtrain_test=Xtrain[c(672:822,1344:1494,2016:2166,2688:2838,3360:3510),]
Ytrain_test=Ytrain[c(672:822,1344:1494,2016:2166,2688:2838,3360:3510),]
Xtrain_train=Xtrain[c(1:671,823:1343,1495:2015,2167:2687,2839:3359,3511:4031),]
Ytrain_train=Ytrain[c(1:671,823:1343,1495:2015,2167:2687,2839:3359,3511:4031),]

#On commence par s'intéresser à PM2_5 uniquement : c'est à dire aux 24 premières colonnes de Ytrain_train.
#Premier calcul de mse sur PM2_5 moyen sur 1 jour
# On va calculer la moyenne du taux de PM2_5 sur les 24h après l'observation :  
library(ggplot2)
heure =as.factor(Xtrain_train[,3])
jour =as.factor(Xtrain_train[,4])
PM2_5 = rowMeans(log(Ytrain_train[,1:24]))
df = data.frame(heure = heure, y =PM2_5)#,25,49,73)])
#on est passé du multivarié à l'univarié : la moyenne des PM2_5 (ou plutot de leurs log) sur une journée est notre nouvel Y (provisoire). 

#On l'a nommé PM2_5 : c'est un vecteur de taille 3276 et on va mtn le régresser sur notre Xtrain_train
fit_full_PM2_5=lm(PM2_5~., data=Xtrain_train[,-1])#on enlève la date(-1), surtout qu'on a déjà des variables jour mois heure.
summary(fit_full_PM2_5)

#a partir de Xtrain_test on fait une prédiction du taux moyen de PM2_5 dans la journée qui suit les mesures. 
#On obtient un vecteur. 
#En le répliquant 24 fois (et en faisant de même pour PM10,O3 et NO2), on obtient une première prédiction de Ytrain_test
#on peut alors calculer notre mean square error (résultat à minimiser pour le challenge)
pred_full_PM2_5=predict(fit_full_PM2_5,Xtrain_test)
library(Metrics)
mse_full_PM2_5 = mse(rowMeans(log(Ytrain_test[,1:24])), pred_full_PM2_5)#mse=21.7
#Notons que ceci est le mse pour le log. Le mse actuel est donc exp(21.7) : c'est énorme!
#De plus ce mse est à multiplier par 24 car on a ici qu'une colonne. Puis à multiplier par 4
#(en supposant qu'on trouve la meme chose pour PM10, NO2 et O3)

#brouillon pour les autres variables  : PM10 O3 et NO2 (à regarder plus tard si besoin est)
#On tente une régression de O3 sur toutes les variables de Xtrain à part la date
O3=rowMeans(log(Ytrain_train[,49:72]))
#représentation de la distribution de O3
df = data.frame(heure = heure, y =O3)#,25,49,73)])
m <- ggplot(df, aes(x=log(y)))+geom_histogram()
plot(m)
#regression sur O3
fit_full_O3=lm(O3~., data=Xtrain_train[,-1])
summary(fit_full_O3)
pred_full_O3=predict(fit_full_O3,Xtrain_test)
library(Metrics)
mse(Ytrain_test,pred_full_O3)


#On fait la meme chose avec PM10 :
PM10 = rowMeans(Ytrain_train[,25:48])
df = data.frame(heure = heure, y =PM10)#,25,49,73)])
m <- ggplot(df, aes(x=log(y)))+geom_histogram()
plot(m)
fit_full_PM10=lm(PM10~., data=Xtrain_train[,-1])
summary(fit_full_PM10)
pred_full_PM10=predict(fit_full_PM10,Xtrain_test)
mse(Ytrain_test,pred_full_PM10)


#On fait la meme chose avec NO2 :
NO2 = rowMeans(Ytrain_train[,73:96])
df = data.frame(heure = heure, y =PM10)#,25,49,73)])
m <- ggplot(df, aes(x=log(y)))+geom_histogram()
plot(m)
fit_full_NO2=lm(NO2~., data=Xtrain_train[,-1])
summary(fit_full_NO2)
pred_full_NO2=predict(fit_full_NO2,Xtrain_test)
mse(Ytrain_test,pred_full_NO2)


#Nous allons maintenant ajuster heure par heure notre prediction.
#Grace à la representation précédente nous avons vu que le taux de PM2_5 variait en fonction des heures.
#Nous allons calculer la moyenne de plusieurs vecteurs : 
#- celle du vecteur PM2_5=rowmeans(Ytrain_train[,1:24]) qu'on a utilisé précédemment 
moyenne_glo=mean(PM2_5)
#et celles des vecteurs du taux de PM2_5 heure par heure. Ces vecteurs font tous une taille de 3276 car, 
#pour chaque observation(donc chaque ligne) de Y_train_train, une des 24 colonnes de PM2_5 correspond à l'heure 1, ou l'heure 2, etc...
#(Attention : En revanche Ytrain_train[,1] ne représente pas toujours l'heure 1 : cela dépend de l'heure à laquelle à été mesurée 
#l'observation)


################################################## Regression pour PM2_5 ################################################## 

# Moyenne des heures
meanPM2_h = rep(0,length = 24)
for(k in 1:24){
  PM2=0
  for(i in 0:23){
    h=which(Xtrain_train[,2]==i)
    if (i<=k-1){
      PM2 = PM2 + sum(log(Ytrain_train[h,k-i]))
    }else{
      PM2 = PM2 + sum(log(Ytrain_train[h,24-(i-k)]))
    }
  }
  meanPM2_h[k]=PM2/length(Ytrain_train[,1])
}


PM2 = rowMeans(log(Ytrain_train[,1:24]))
PM2_test = rowMeans(log(Ytrain_test[,1:24]))
meanPM2 = mean(PM2)
diff_PM2 = meanPM2_h - meanPM2

# Lasso
lasso_PM2 = cv.glmnet(as.matrix(Xtrain_train[,-1]),PM2, family = "gaussian", alpha = 0.95)
plot(lasso_PM2)
c_PM2<-coef(lasso_PM2,s=exp(-2.7),exact=TRUE)
ind_PM2<-which(c_PM2!=0)
length(ind_PM2)

# Regression Ridge
Xtrain_lasso_PM2 = Xtrain_train[,ind_PM2]
Xtest_lasso_PM2 = Xtrain_test[,ind_PM2]

reg_PM2 = cv.glmnet(as.matrix(Xtrain_lasso_PM2[,-1]),PM2,alpha=0)
plot(reg_PM2)
predi_PM2 = predict(reg_PM2, as.matrix(Xtest_lasso_PM2[,-1]), s="lambda.min")
predi_PM2 = as.vector(predi_PM2)

# Dupliaction de predi 24fois pour avoir une matrice 
predi_PM2_24fois = replicate(24,predi_PM2)

# On ajoute diff a notre prediction
for(k in 1:24){
  for(i in 0:23){
    h = which(Xtrain_test[,2] == i)
    if (i<=k-1){
      predi_PM2_24fois[h,k-i] = predi_PM2_24fois[h,k-i] + diff_PM2[k]
    }else{
      predi_PM2_24fois[h,24-(i-k)] = predi_PM2_24fois[h,24-(i-k)] + diff_PM2[k]
    }
  }
}
mse_PM2 = mse((Ytrain_test[,1:24]), exp(predi_PM2_24fois)) # 64.17438, 64.16771
mse_PM2


################################################## Regression pour PM10 ################################################## 
# Moyenne des heures
meanPM10_h = rep(0,length = 24)
for(k in 1:24){
  PM10=0
  for(i in 0:23){
    h=which(Xtrain_train[,2]==i)
    if (i<=k-1){
      PM10 = PM10 + sum(log(Ytrain_train[h,24+k-i]))
    }else{
      PM10 = PM10 + sum(log(Ytrain_train[h,48-(i-k)]))
    }
  }
  meanPM10_h[k]=PM10/length(Ytrain_train[,1])
}

PM10 = rowMeans(log(Ytrain_train[,25:48]))
PM10_test = rowMeans(log(Ytrain_test[,25:48]))
meanPM10 = mean(PM10)
diff_PM10 = meanPM10_h - meanPM10

# Lasso
lasso_PM10 = cv.glmnet(as.matrix(Xtrain_train[,-1]),PM10, family = "gaussian", alpha = 0.95)
plot(lasso_PM10)
c_PM10<-coef(lasso_PM10,s=exp(-2.9),exact=TRUE)
ind_PM10<-which(c_PM10!=0)
length(ind_PM10)

# Regression Ridge
Xtrain_lasso_PM10 = Xtrain_train[,ind_PM10]
Xtest_lasso_PM10 = Xtrain_test[,ind_PM10]

reg_PM10 = cv.glmnet(as.matrix(Xtrain_lasso_PM10[,-1]),PM10,alpha=0)
predi_PM10 = predict(reg_PM10, as.matrix(Xtest_lasso_PM10[,-1]), s="lambda.min")
predi_PM10 = as.vector(predi_PM10)

# Dupliaction de predi 24fois pour avoir une matrice 
predi_PM10_24fois = replicate(24,predi_PM10)

# On ajoute diff a notre prediction
for(k in 1:24){
  for(i in 0:23){
    h = which(Xtrain_test[,2] == i)
    if (i<=k-1){
      predi_PM10_24fois[h,k-i] = predi_PM10_24fois[h,k-i] + diff_PM10[k]
    }else{
      predi_PM10_24fois[h,24-(i-k)] = predi_PM10_24fois[h,24-(i-k)] + diff_PM10[k]
    }
  }
}

mse_PM10 = mse((Ytrain_test[,25:48]), exp(predi_PM10_24fois)) # 76.86605,  76.86553
mse_PM10


##################################################  Regression pour O3 ################################################## 
# Moyenne des heures pour O3
meanO3_h = rep(0,length = 24)
for(k in 1:24){
  a=0
  for(i in 0:23){
    h=which(Xtrain_train[,2]==i)
    if (i<=k-1){
      a = a + sum(Ytrain_train[h,48+k-i])
    }else{
      a = a + sum(Ytrain_train[h,72-(i-k)])
    }
  }
  meanO3_h[k]=a/length(Ytrain_train[,1])
}

O3 = rowMeans(Ytrain_train[,49:72])
O3_test = rowMeans(Ytrain_test[,49:72])
meanO3 = mean(O3)
diff_O3 = meanO3_h - meanO3

# Lasso
lasso_O3 = cv.glmnet(as.matrix(Xtrain_train[,-1]),O3, family = "gaussian", alpha = 0.95)
plot(lasso_O3)
c_O3<-coef(lasso_O3,s=exp(-2.2),exact=TRUE)
ind_O3<-which(c_O3!=0)
length(ind_O3)

# Regression Ridge
Xtrain_lasso_O3 = Xtrain_train[,ind_O3]
Xtest_lasso_O3 = Xtrain_test[,ind_O3]

reg_O3 = cv.glmnet(as.matrix(Xtrain_lasso_O3[,-1]),O3,alpha=0)
predi_O3 = predict(reg_O3, as.matrix(Xtest_lasso_O3[,-1]), s="lambda.min")
predi_O3 = as.vector(predi_O3)

# Dupliaction de predi 24fois pour avoir une matrice
predi_O3_24fois = replicate(24,predi_O3)

# On ajoute diff a notre prediction
for(k in 1:24){
  for(i in 0:23){
    h = which(Xtrain_test[,2] == i)
    if (i<=k-1){
      predi_O3_24fois[h,k-i] = predi_O3_24fois[h,k-i] + diff_O3[k]
    }else{
      predi_O3_24fois[h,24-(i-k)] = predi_O3_24fois[h,24-(i-k)] + diff_O3[k]
    }
  }
}

mse_O3 = mse((Ytrain_test[,49:72]), predi_O3_24fois) # 438.4617
mse_O3


##################################################  Regression pour NO2 ################################################## 
# Moyenne des heures pour NO2
meanNO2_h = rep(0,length = 24)
for(k in 1:24){
  NO2=0
  for(i in 0:23){
    h=which(Xtrain_train[,2]==i)
    if (i<=k-1){
      NO2 = NO2 + sum(log(Ytrain_train[h,72+k-i]))
    }else{
      NO2 = NO2 + sum(log(Ytrain_train[h,96-(i-k)]))
    }
  }
  meanNO2_h[k]=NO2/length(Ytrain_train[,1])
}

NO2 = rowMeans(log(Ytrain_train[,73:96]))
NO2_test = rowMeans(log(Ytrain_test[,73:96]))
meanNO2 = mean(NO2)
diff_NO2 = meanNO2_h - meanNO2

# Lasso
lasso_NO2 = cv.glmnet(as.matrix(Xtrain_train[,-1]),NO2, family = "gaussian", alpha = 0.95)
plot(lasso_NO2)
c_NO2<-coef(lasso_NO2,s=exp(-5.9),exact=TRUE)
ind_NO2<-which(c_NO2!=0)
length(ind_NO2)

# Regression Ridge
Xtrain_lasso_NO2 = Xtrain_train[,ind_NO2]
Xtest_lasso_NO2 = Xtrain_test[,ind_NO2]

reg_NO2 = cv.glmnet(as.matrix(Xtrain_lasso_NO2[,-1]),NO2,alpha=0)
predi_NO2 = predict(reg_NO2, as.matrix(Xtest_lasso_NO2[,-1]), s="lambda.min")
predi_NO2 = as.vector(predi_NO2)

# Dupliaction de predi 24fois pour avoir une matrice
predi_NO2_24fois = replicate(24,predi_NO2)

# On ajoute diff a notre prediction
for(k in 1:24){
  for(i in 0:23){
    h = which(Xtrain_test[,2] == i)
    if (i<=k-1){
      predi_NO2_24fois[h,k-i] = predi_NO2_24fois[h,k-i] + diff_NO2[k]
    }else{
      predi_NO2_24fois[h,24-(i-k)] = predi_NO2_24fois[h,24-(i-k)] + diff_NO2[k]
    }
  }
}

mse_NO2 = mse((Ytrain_test[,73:96]), exp(predi_NO2_24fois)) # 237.9348 
mse_NO2



##################################################  MSE TOTAL ################################################## 


predi_lasso_Ytest = cbind(exp(predi_PM2_24fois),exp(predi_PM10_24fois),predi_O3_24fois,exp(predi_NO2_24fois))
mse(Ytrain_test,predi_lasso_Ytest) # 204.3597


