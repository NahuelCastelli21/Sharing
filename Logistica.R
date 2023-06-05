library("Hmisc")
library("lmtest")
library("MASS")
library("RcmdrMisc") #Para Stepwise
install.packages("DescTools")
library("DescTools") #Para los pseudoR2
library("generalhoslem") #Para el Hosmer-Lemeshow
library("caret") #Tabla de clasificacion
library("pROC") #Curva ROC
library("ROCR")


l<-read.csv("C:/Users/marti/Desktop/diabetes2.csv",sep = ",")

l<-read.csv("D:/nahuel/ayme/diabetes2.csv",sep = ",")

#Cargamos la libreria
library("corrplot")
oldpar<-par(no.readonly = TRUE)

#ANTES DE HACER NADA, VEMOS LAS VARIABLES CON "NA" Y SI SON POCOS, ELIMINAMOS LOS REGISTROS
#VEMOS LOS NA
colSums(is.na(l))

#ELIMINAMOS LOS REGISTROS CON "NA"
l<-na.omit(l)

#Realizamos un descriptivo de los datos
summary(l)

l$DiabetesPedigreeFunction <- NULL

attach(l)

logist<-glm(Outcome~Pregnancies+Glucose+BloodPressure+SkinThickness+Insulin+BMI+Age,data = l,family = binomial(link = "logit"))
summary(logist)$coefficients


d<-round(summary(logist)$coefficients,7)
e<-data.frame(d,Wald=(d[,1]/d[,2])^2,PValue=pchisq((d[,1]/d[,2])^2,1,lower.tail=FALSE))
round(e,5)


cbind(coef(logist),confint.default(logist))

#Vemos los odds-ratio y sus IC al 95%

exp(cbind(coef(logist),confint.default(logist)))

#Vemos ahora los -2LL al introducir cada variable. Los GL son por las categorias que tiene la variable.
anova(logist)

#Pedimos las medidas de bondad de ajuste,especificamos McFadden, COx y Nalgerkerke.
PseudoR2(logist,c("McFadden","CoxSnell","Nagelkerke"))


#La primera regresion introdujo todas las variables sin tener en cuenta la significatividad
p<-stepwise(logist, direction='forward', criterion='AIC')
p$anova

#Nuevo modelo con criterio de seleccion pasos adelante
logist1<-glm(Outcome~Glucose+BMI,data = l,family = binomial(link = "logit"))

summary(logist1)

#examinamos nuevos pseudos R2 del nuevo modelo

round(PseudoR2(logist1,c("McFadden","CoxSnell","Nagelkerke")),4)

#El LRtest es complicado para comparar modelo ya que arroja los LL y faltaria multiplicarlo por -2 y la chi si es la diferencia.
#Probamos para c/ variable seleccionada

lrtest(logist1,"Glucose")

#Para hacer esta prueba con "anova", debemos correr ambos modelo a comparar
logist2<-glm(Outcome~BMI,data = l,family = binomial(link = "logit"))

#Pedimos el anova para comparar, esta funcion es mas completa que "lrtest" ya que calcula el PV de la Chi. Debemos poner primero el modelo restringuido, luego el completo

anova(logist2,logist1, test = "LRT")

#Pedimos el estadistico Hosmer-Lemeshow
f1<-logitgof(l$Outcome,fitted(logist1),g=4);f1




#Generamos valores observados y esperados
g1<-cbind(f1$observed,f1$expected)
h1<-as.data.frame(g1)
colnames(h1)<-c("Obs=0","Obs=1","Pred=0","Pred=1");h1

#Generamos las predicciones
prediccion2<-predict(logist1, type="response")
corte<-0.5
i<-ifelse(prediccion2>corte,1,0)

#Pasamos a factor ambas variables, la prediccion y la observacion
i<-factor(i,levels=c(0:1),labels = c("No","Si"))
l$Outcome<-factor(l$Outcome,levels=c(0:1),labels = c("No","Si"))

#Tabla de clasificacion
clasif<-confusionMatrix(i,l$Outcome,positive = "Si");clasif
#Siendo en la tabla "Si" el target o presencia
#                A  B
#                C  D
#Sensibilidad = D / B+D
#Especificidad = A/A+C
#Pos Pred Value = D/C+D
#Neg Pred Value = A/A+B
#Prevalencia = B+D/N = 79/150
#Detection rate D/n =aciertos/n = 70/150
#Detection Prevalence = C+D/n = 95/150
#Balance Accuracy = (Sensibilidad+Especificidad)/2=(0.886+.648)/2

#tabla como porcentaje, la proporcion de aciertos esta dada por la diagonal principal (Sensibilidad=1, Especificidad=0)
clasif
round(prop.table(clasif$table,margin=2),3)
#Pedimos los marginales fila y Proporcion de Positivos y de Negativos
round(prop.table(clasif$table,margin=1),3)

#Generamos las tablas para diversos puntos de corte, en este 10 entre 0.1 y 1
for (i in seq(0.1,1,length=10)) {
  r<-ifelse(prediccion2>i,1,0)
  r<-factor(r,levels=c(0:1),labels = c("No","Si"))
  s<-confusionMatrix(r,l$Outcome,positive = "Si")$table
  print(s)
}



#Curva Roc
curva2<-roc(l$Outcome,prediccion2);curva2

#Buscamos optimo, criterio de Youden
opt<-coords(curva2, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                  "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                  "1-sensitivity", "1-accuracy", "1-npv", "1-ppv",
                                  "precision", "recall"))
#Grafico de Curva Roc
{ 
plot(1-curva2$specificities,curva2$sensitivities,type="l",xlab="1-Especificidad",ylab="Sensibilidad",lwd=2,col="blue",main="Curva ROC")
segments(0,0,1,1,lwd=2,col="red")
points(1-opt[[2]],opt[[3]],col="red",cex=1.5)

#Coordenada del corte de Youden, el primer valor es la Especificidad (ojo)
legend(1-opt[[2]],.15+opt[[3]],paste("(",round(opt[[2]],4),",",round(opt[[3]],4),")"),col="red",bty="n",cex=1.5)
}

#Vemos los puntos de corte, la Sensibilidad y 1-Especificidad
tabla2<-round(data.frame(Corte=curva2$thresholds,Sensibilidad=curva2$sensitivities,`1-Especificidad`=1-curva2$specificities),4);tabla2

#Calculamos area bajo la curva 
(areaRoc<-auc(l$Outcome,prediccion2))

#Calculamos los IC del area bajo la curva
ci(areaRoc)


#Grafico de probabilidades predichas para grupo
scales::alpha('grey',.6)
hist(prediccion2[l$Outcome=="No"], xlim=c(0,1), ylim=c(0,100), breaks = 20, freq=T, col='skyblue2',border=F, xlab="Probabilidad de participar", main="Grafico de Probabilidades",xaxp=c(0,1,20),cex.axis=.8)
hist(prediccion2[l$Outcome=="Si"], xlim=c(0,1), ylim=c(0,100), breaks = 20, freq=T, add=T,col=scales::alpha('grey50',.6),border=F)
legend(x="topleft", fill=c('skyblue2',scales::alpha('grey50',.6)), legend=c("No Dbts", "Dbts"),bty = 'n',border = NA)


{
  preddiab <- function(Glucose, BMI)
  secgluc <- seq(min(l$Glucose), max(l$Glucose), length.out = 30)
secbmi <- seq(min(l$BMI), max(l$BMI), length.out = 30)
pred.gluc.bmi <- outer(secgluc, secbmi)

persp(secgluc, secbmi, pred.gluc.bmi, phi=30, theta=30, 
      border = "darkblue", col="cyan", xlab="glucosa", ylab="BMI", zlab="P(diabetes)")
}
