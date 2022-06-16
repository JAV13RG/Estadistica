library(dplyr) # manipulacion 
library(Rmisc) # intevalos de confianza
library(nortest) # test de normalidad
library(moments) # curtosis, etc....

cardio_train2

glimpse(cardio_train2)
summary(cardio_train2)

cardio1 <- cardio_train2 %>% filter(weight <=100, ap_hi <=180,ap_lo >=0,ap_lo<120,gender ==1 )
cardio2 <- cardio_train2 %>% filter(weight <=100, ap_hi <=180,ap_lo >=0,ap_lo<120,gender ==2 )
dim(cardio1)
dim(cardio2)

boxplot(cardio_train2$weight)

plot(density(cardio_train2$weight))


# cuando el n es suficientemente grande, podemos asumir normalidad
# es decir, los datos vienen de una distribucion normal

mean(cardio1$weight)
mean(cardio2$weight)


# tomamos una muestra de los datos
# hombres
nrow(cardio1) # numero de filas

set.seed(08)
cardio1.s <- sample(cardio1$weight, size = 1000,replace = F)
cardio2.s <- sample(cardio2$weight, size = 1000,replace = F)

mean(cardio1$weight)
mean(cardio1.s)

t.test(cardio1.s)
t.test(cardio2.s)

#diferencia de medias es igual a 0 (mu1 - mu2 = 0)
t.test(x=cardio1.s , y=cardio2.s)

#diferencia sera menor que -5
t.test(x=cardio1.s , y=cardio2.s,
       alternative = "greater",
       mu = -5) #(H0 = mu1 - mu2 < -5 / H1 = mu1 - mu2 > -5)

#promedio=mean
mean(cardio1$weight) - mean(cardio2$weight)

t.test(x = cardio1.s,
       alternative = "greater",
       conf.level = 0.90,
       mu = 50)











