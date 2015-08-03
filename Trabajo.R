#### Trabajo Final
#Ana Julia Escobar
#1
library(readxl)
install.packages('DT')
pob1<-read_excel("poblacion1.xlsx", sheet=1, na=" ")
pob2<-read_excel("poblacion2.xlsx", sheet=1, na=" ")
View(pob1)
str(pob1)
View(pob2)
str(pob2)
nrow(pob1) 
ncol(pob1) 
nrow(pob2) 
ncol(pob2) 
#2
poblacion <- merge(pob1,pob2,by="identificador",suffixes=c("",""))
View(poblacion)


cla <- numeric(ncol(poblacion))
for (i in 1:ncol(poblacion)){
  cla[i] <- class(poblacion[,i])
}
cla

cajas <- function(i){
  boxplot(poblacion[,i])
}
for (i in 1:ncol(poblacion)){
  if (is.numeric(poblacion[,i])==T){
    cajas(i)
  }
}
for (i in 1:ncol(poblacion)){
  if (is.numeric(poblacion[,i])==F){
    barplot(table(poblacion[,i]))
  }
}


#####

tipo <- 1:dim(poblacion)[2]
maximo <- 1:dim(poblacion)[2]
minimo <- 1:dim(poblacion)[2]
med <- 1:dim(poblacion)[2]
des <- 1:dim(poblacion)[2]
uno_cuartil <- 1:dim(poblacion)[2]


for(i in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,i])==TRUE){
    tipo[i] <- class(poblacion[,i])
    maximo[i] <- max(poblacion[,i])
    minimo[i] <- min(poblacion[,i])
    med[i] <- mean(poblacion[,i])
    des[i] <- sd(poblacion[,i])
    uno_cuartil[i] <- quantile(poblacion[,i],probs=seq(0,1,0.25),na.rm = FALSE)[2]
  }else {
    
    tipo[i] <- class(poblacion[,i])
    maximo[i] <- NA
    minimo[i] <- NA
    med[i] <- NA
    des[i] <- NA
    uno_cuartil[i] <- NA 
    table(poblacion[,i])/dim(poblacion)[1]
  }
}

respuesta1<- data.frame(names(poblacion),tipo,maximo,minimo,med,des,uno_cuartil)
View(respuesta1)

fr_reg  <- table(poblacion[,9])/dim(poblacion)[1]
fr_sr.bas.compl <- table(poblacion[,10])/dim(poblacion)[1]

fr_reg
fr_sr.bas.compl

core <- 3:dim(poblacion)[2]
core[1] <- NA


for(i in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,i])==TRUE){
    core[i] <- cor(poblacion[,2], poblacion[,i])
  } else {
    core[i] <- NA
  }
}

#Analizando la correclación en cada una de las variables tenemos.

respuesta2 <- data.frame(names(poblacion),core) 
respuesta2
View(respuesta2)

#2.5. Luego tranformamos las columnas region y ser...  a variables factor.
#Así podremos usar el test t (student)
serv.bas.compl_fac <- factor(poblacion[,"serv.bas.compl"],levels=c("SI","NO"),labels=c("YES","NO"))
reg_fac <- factor(poblacion[,"region"],levels=c("A","B"),labels=c("1","0"))
pob_fac <- data.frame(poblacion[,1:7],reg_fac,serv.bas.compl_fac)


# Realizando la prueba de hipotesis tenemos. 
prueba1 <- t.test(poblacion ~ serv.bas.compl_fac , data=pob_fac, conf.level=0.9)
prueba1


#Con los resultados anteriores podemos aceptar que la diferencia sea cero.

#2.6.
region1 <- lm(poblacion~var.pobl.mayor+menores.18+tasa.crimen, data=pob_fac)
summary(region1)

qf(0.90,df1=3,df2=36)

#2.7.- Interpretar el R2

#Con los datos obtenidos de la region se obtiene que R cuadrado es.

R2 <- summary(region1)[["r.squared"]]
#R2= 0.1533239

#Lo que es lo mismo decir que la regresion explica el: 
por <- 100*summary(region1)[["r.squared"]] 
#porcentaje=15.33%
#de la variabilidad.

#2.8.-

anova1 <- aov(reg1)
summary(anova1)

#2.9.-  Realice un análisis detallado de los residuos.

res<-1:40  
for(j in 1:40){
  res[j]<-summary(region1)[["residuals"]][i]
}

#pobtotal vs res
plot(poblacion[,"poblacion"],res)

#histograma 
hist(res)

qqnorm(res)
qqline(res,col="pink")

