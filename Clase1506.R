
library(readxl)
library(nortest)
library(stests)
library(ggplot2)
library(dplyr)

bases <- read_csv("C:/Users/Javier/Desktop/Estadistica Aplicada/Placement_Data_Full_Class.csv")
View(bases)
dim(bases)
head(bases)
str(bases)

# PRUEBAS DE HIPOTESIS (H0 vs H1)

# Los argumentos a definir dentro de t.test para hacer laprueba son:
#   
# x: vector numérico con los datos.
# alternative: tipo de hipótesis alternativa posibilidades 
# "two.sided" cuando ≠ , 
# "less" para el caso <   
# "greater" para  >
# mu: valor de referencia  o mu_0.
# conf.level: nivel de confianza para reportar el intervalo de confianza asociado (opcional).

# en prop.test
# p: valor de referencia de la prueba, p_0.
# correct: valor lógico para indicar si se usa la corrección de Yates. usamos FALSE

# en var.test 
# null.value: valor de referencia de la prueba. sigma2_0
# install.packages('devtools')
# devtools::install_github('fhernanb/stests', force=TRUE)
# devtools::install_github('fhernanb/usefultools', force=TRUE)

# variables:

# ssc_p           : Secondary Education percentage- 10th Grade
# ssc_b           : Board of Education- Central/ Others
# hsc_p           : Higher Secondary Education percentage- 12th Grade
# hsc_b           : Board of Education- Central/ Others
# hsc_s           : Specialization in Higher Secondary Education
# degree_p        : Degree Percentage
# degree_t        : Under Graduation(Degree type)- Field of degree education
# workex          : Work Experience
# etest_p         : Employability test percentage ( conducted by college)
# specialisation  : Post Graduation(MBA)- Specialization
# mba_p           : MBA percentage
# status          : Status of placement- Placed/Not placed
# salary          : Salary offered by corporate to candidates


plot(density(bases$mba_p, na.rm = T)) 

#test para probar la normalidad (test de lillie):
lillie.test(bases$mba_p)

base_mba <- split(bases$mba_p,bases$degree_t) #hacer una lista por categoria

lapply(base_mba, lillie.test) #aplicar el test a cada lista de datos


boxplot(bases$mba_p ~ bases$degree_t)


bases %>% 
  ggplot(aes(degree_p, color=degree_t))+
  geom_density() + theme_minimal()

dim(bases)
lillie.test(bases$mba_p)

#cuando usemos bases, tomaremos la muestra para comparar:
  #una muestra aleatoria de la bd "bases", de tamaño 100, sin reemplazo:
  set.seed(2022)
  muestra <- sample(bases$mba_p, size = 100, replace = F) 
  muestra
  
  mean(bases$mba_p)
  var(bases$mba_p)
  
# para la media ----
  #Hipotesis: saber si la media es mayor que 65 (0.95)
  #H0: mu > 65  H1: mu < 65
  t.test(x = muestra,
         alternative = "less",
         mu = 65,
         conf.level = 0.95) #p-value = aprox. 0 (rechazo H0)

  #H0: mu < 65  H1: mu > 65
  t.test(x = muestra,
         alternative = "greater",
         mu = 65,
         conf.level = 0.95) #p-value = 1 (acepto H0)
  
# para la proporcion ----
  #x/n
  prop.test(x = 75,
            n = 1500,
            conf.level = 0.9,
            p = 0.07,
            alternative = "less")
  
  #como p-values < 0.05 rechazo H0
  prop.test(x = 75,
            n = 1500,
            conf.level = 0.95,
            p = 0.02,
            alternative = "two.sided")
  
# para la varianza ----
  library(stests)
  
  var.test(muestra,
           alternative = "greater",
           null.value = 30)
  
  
  var.test(muestra,
           alternative = "two.sided",
           null.value = 34)
  
# para el cociente de varianzas ----
  #se trabaja con base_mba
  var.test(x = base_mba$`Sci&Tech`, y = base_mba$Others,
           alternative = "two.sided", null.value = 1)
  #(las varianzas son iguales)
  
  sd(base_mba$`Comm&Mgmt`)
  sd(base_mba$Others)
  
# para la diferencia de medias ----
  
  #H1: mu1 - mu2 = 0  H2: mu1 - mu2 =! 0
  t.test(base_mba$`Sci&Tech`, base_mba$Others, 
         alternative = "two.sided", mu = 0, var.equal = TRUE,
         conf.level = 0.95) #p-value = 0.29 (Acepto H1)
  
  #H1: mu1 - mu2 > 7  H2: mu1 - mu2 <= 7
  t.test(base_mba$`Sci&Tech`, base_mba$Others, 
         alternative = "two.sided", mu = 7, var.equal = TRUE,
         conf.level = 0.95) #p-value = 0.01 (Rechazo H1)
  
# con varianzas iguales ----
# con varianzas distintas ----
  
# para la diferencia de proporciones ----
  prop.test(x = c(75,80), n = c(1500,2000), conf.level = 0.90,
            alternative = "less", p = c(0.07,0.06))
  
# para la diferencia de medias pareadas ----


