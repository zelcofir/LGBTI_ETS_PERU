## Abriendo paquetes: Utiliza el comando library()
library(readxl)
library(tidyverse)
library(dplyr)

# Importando bases de datos

datos <- read_xlsx("datos/Base1_importacion.xlsx")


# Importando bases de datos
## Importando base de datos SPSS
library(haven)
datos <- read_sav("datos/602-Modulo1287.sav")

library(tidyverse)
library(Hmisc)



## Guardo solo las variables de interes 
datos1 <- datos %>% 
          select(p109, p105_2,
                 p112, edad, p124,
                 p136, p105_1, p113,
                 p101, p110, p134,
                 p104, p205_8) %>% 
  glimpse()

# sexo : p112 ----

##1 - Replace on all columns

datos1$sexo <- datos1$p112

# opcion 2.0
sum(is.na(datos1$sexo))
datos1["sexo"][datos1["sexo"] == 9] <- NA
class(datos1$sexo)

# etiquetado
datos1$sexo <- as.factor(datos1$sexo)

datos1 <- datos1 %>% 
  mutate(sexo = recode (sexo, 
                        "1" = 1,
                        "2" = 0))
         
datos1$sexo <- factor(datos1$sexo,
                        levels = c("0", "1"),
                        labels = c("Femenino",
                                   "Masculino"))
label(datos1$sexo) <- "Sexo biológico"


table(datos1$sexo)


# edad : edad | NO NORMAL ----
datos$edad
summary(datos1$edad)
hist(datos1$edad)
library(ggpubr)
ggqqplot(datos1$edad)
boxplot(datos1$edad)
label(datos1$edad) <- "Edad"


# Discapacidad : p124----
datos1$disc <- datos1$p124

# convertir 9 en NA
sum(is.na(datos1_respaldo$p124))
sum(is.na(datos1$disc))

datos1["disc"][datos1["disc"] == 9] <- NA
table(datos1$disc)


datos1$disc <- as.factor(datos1$disc)

datos1 <- datos1 %>% 
  mutate(disc = recode (disc, 
                             "1" = 1, # si recibio
                             "2" = 0)) %>% # no recibió
  glimpse()

table(datos1$disc)


datos1$disc <- factor(datos1$disc,
                     levels = c("0", "1"),
                     labels = c("No",
                                "Si"))

label(datos1$disc) <- "Discapacidad"

table(datos1$disc)



# Raza : p136----
datos1$raza <- datos1$p136

## se encontró 
datos1["raza"][datos1["raza"] == 9] <- NA
table(datos1$raza)
## a factor
datos1$raza <- as.factor(datos1$raza)
## procedo con el recodeo
datos1 <- datos1 %>% 
  mutate(raza = recode (raza, 
                           "1" = 2,
                           "2" = 2,
                           "3" = 2,
                           "4" = 2,
                           "5" = 3,
                           "6" = 4,
                           "7" = 1, #mestizo como base
                           "8" = 5)) %>% 
  glimpse()

# verifico la conversión
table(datos1$raza)

#etiqueto
datos1$raza <- factor(datos1$raza,
                    levels = c("1", "2", "3",
                               "4", "5"),
                    labels = c("Mestizo",
                               "Pueblo indígena",
                               "Negro",
                               "Blanco",
                               "Otro" ))

label(datos1$raza) <- "Raza"
table(datos1$raza)



# Antecedente de enfermedad crónica : p105_1----
datos1$cronic <- datos1$p105_1

str(datos1$p105_1)
datos1$cronic <- factor(datos1$cronic,
                     levels = c("0", "1"),
                     labels = c("No",
                                "Si"))
label(datos1$cronic) <- "Enfermedad crónica"
table(datos1$cronic)


# Orientación sexual : p113----
datos1$orsex <- datos1$p113
datos1["orsex"][datos1["orsex"] == 9] <- NA

# recodeo
## convierto a factores para el recodeo
datos1$orsex <- as.factor(datos1$orsex)
## procedo con el recodeo
datos1 <- datos1 %>% 
  mutate(orsex = recode (orsex, 
                           "1" = 1,
                           "2" = 2,
                           "3" = 3,
                           "4" = 4,
                           "5" = 5,
                           "6" = 5,
                           "7" = 5)) %>% 
  glimpse()

# verifico la conversión
table(datos1$orsex)
datos1$orsex <- factor(datos1$orsex,
                       levels = c("1", "2", "3",
                                  "4", "5"),
                       labels = c("Heterosexual",
                                  "Gay",
                                  "Lesbiana",
                                  "Bisexual",
                                  "Pansexual/asexual/otro"))

label(datos1$orsex) <- "Orientación sexual"
table(datos1$orsex)

# Nivel de estudios alcanzado : p101----
datos1$edu <- datos1$p101

table(datos1$edu)

datos1["edu"][datos1["edu"] == 99] <- NA

table(datos1$edu)
datos1$edu <- as.factor(datos1$edu)

datos1 <- datos1 %>% 
  mutate(edu = recode (edu, 
                           "1" = 1,
                           "2" = 1,
                           "3" = 1,
                           "4" = 1,
                           "5" = 1,
                           "6" = 2,
                           "7" = 2,
                           "8" = 2,
                            "9" = 2,
                            "10" = 3,
                            "11" = 3)) %>% 
  glimpse()

# verifico la conversión
table(datos1$edu)
datos1$edu <- factor(datos1$edu,
                     levels = c("1", "2","3"),
                     labels = c("Primaria","Secundaria",
                                "Universitario o superior"))

label(datos1$edu) <- "Grado de instrucción"

table(datos1$edu)


# Metodo para prevenir infecciones p110----
datos1$previnfec <- datos1$p110
# por algun motivo no se lograba pasar a NA, por eso
# se pasó a factor, luego a numerico y por eso ahora
# se reemplaza NA en el 3
datos1$previnfec <- as.factor(datos1$previnfec) 
datos1$previnfec <- as.numeric(datos1$previnfec)

datos1["previnfec"][datos1["previnfec"] == 3] <- NA
table(datos1$previnfec)
# recodeo para ordenar si y no
datos1 <- datos1 %>% 
  mutate(previnfec = recode (previnfec, 
                       "1" = 1, # si recibio
                       "2" = 0)) %>% # no recibió
  glimpse()

table(datos1$previnfec)

datos1$previnfec <- factor(datos1$previnfec,
                     levels = c("0", "1"),
                     labels = c("No",
                                "Si, al menos uno"))

label(datos1$previnfec) <- "Usa método para prevenir infecciones"

table(datos1$previnfec)

# Metodo de barrera condon : p110_1----

# Metodo de barrera barrera latex : p110_2----
# Metodo de barrera_otro: p110_3----
# Metodo de barrera_ninguno : p110_4----
# ya no se realizarian del p110_1 al 4

# Trabajo sexual, al menos una vez : p134----
datos1$sexualjob <- datos1$p134
table(datos1$sexualjob)

datos1["sexualjob"][datos1["sexualjob"] == 9] <- NA
table(datos1$sexualjob)

# convertir a factor
datos1$sexualjob <- as.factor(datos1$sexualjob) 

# recodeo para ordenar si y no
datos1 <- datos1 %>% 
  mutate(sexualjob = recode (sexualjob, 
                             "1" = 1, # si al menos una vez
                             "2" = 0)) %>% # no 
  glimpse()
table(datos1$sexualjob)
# 
datos1$sexualjob <- factor(datos1$sexualjob,
                      levels = c("0", "1"),
                      labels = c("No",
                                 "Si, al menos una vez"))

label(datos1$sexualjob) <- "Trabajador sexual"

table(datos1$sexualjob)

# Seguro de salud : p104----
datos1$seguro <- datos1$p104
table(datos1$seguro)

datos1["seguro"][datos1["seguro"] == 9] <- NA
table(datos1$seguro)
# convertir a factor
datos1$seguro <- as.factor(datos1$seguro) 

# recodeo para ordenar si y no
datos1 <- datos1 %>% 
  mutate(seguro = recode (seguro, 
                             "1" = 1, # si al menos uno
                             "2" = 0)) %>% # no 
  glimpse()

table(datos1$seguro)

datos1$seguro <- factor(datos1$seguro,
                      levels = c("0", "1"),
                      labels = c("No",
                                 "Si, al menos uno"))

label(datos1$seguro) <- "Seguro de salud"

table(datos1$seguro)
# Abuso de sustancias : p205_8---- eliminado por NAs----
datos1$sustancias <- datos1$p205_8
table(datos1$sustancias)

datos1$sustancias <- factor(datos1$sustancias,
                      levels = c("0", "1"),
                      labels = c("No",
                                 "Si"))

label(datos1$sustancias) <- "Abuso de sustancias"

table(datos1$sustancias)

# Probando variables del estudio----
names(datos)

# Busqueda variables de interes
## 1: Ud. ha recibido información sobre la
## prevención de las infecciones de ITS/VIH-SIDA, 
## a sus necesidades como persona LGBTI (p109)
### recodeo

datos1$knowITS <- datos$p109
table(datos1$knowITS)
datos1["knowITS"][datos1["knowITS"] == 9] <- NA
table(datos1$knowITS)

## convierto a factores para el recodeo
datos1$knowITS <- as.factor(datos1$knowITS)

## procedo con el recodeo
datos1 <- datos1 %>% 
  mutate(knowITS = recode (knowITS, 
                            "1" = 1,# si recibió
                            "2" = 1,
                            "3" = 1,
                            "4" = 1,
                            "5" = 1,
                            "6" = 1,
                            "7" = 0)) %>% # no recibió
  glimpse()


# verifico la conversión
table(datos1$knowITS)


# lo convierto a numeros de nuevo
datos1$knowITS <- as.numeric(datos1$knowITS)

# aplico labels a las categorias
datos1$knowITS <- factor(datos1$knowITS,
                               levels = c("0", "1"),
                               labels = c("No recibió",
                                          "Si recibió"))

table(datos1$knowITS)

label(datos1$knowITS) <- "Recibio informacion sobre ITS/VIH"
table(datos1$knowITS)

#---
# guardamos una copia para estratificar el tipo de capacitación
datos1$capacitacion <- datos$p109
table(datos1$capacitacion)
head(datos1$capacitacion)

library(tidyverse)

datos1$capacitacion <- as.factor(datos1$capacitacion)

datos1 <- datos1 %>% 
  mutate(capacitacion = recode (capacitacion, 
                           "1" = 1,
                           "2" = 2,
                           "3" = 3,
                           "4" = 3,
                           "5" = 3,
                           "6" = 3,
                           "7" = 0)) 

datos1$capacitacion <- factor(datos1$capacitacion,
                         levels = c("0","1", "2","3"),
                         labels = c("No recibió","De una organización del Estado",
                                    "De una organización de la Sociedad Civil (Colectivo, ONG, etc.)",
                                    "Otro"))

table(datos1$capacitacion)

label(datos1$capacitacion) <- "Entidad que brindó información"


## 2: En los últimos 12 meses, ud. Tuvo algún problema 
## de salud como: Enfermedades infecto contagiosas? (p105_2)
datos1$infectado <- datos$p105_2

datos1$infectado <- factor(datos1$infectado,
                        levels = c("0", "1"),
                        labels = c("No",
                                   "Si"))

label(datos1$infectado) <- "Enfermedad infectocontagiosa"

table(datos1$infectado)

# selecciono solo variables de mi interes
datos2 <- datos1 %>% 
  select(sexo, edad, raza,
         edu, orsex, disc,
         cronic, previnfec, sexualjob,
         seguro, capacitacion, knowITS,
         infectado) %>% 
  glimpse() # no estoy considerando sustancias por no ser importante
            # en el modelo de regresion

names(datos2)
###
# Filtro de datos
library(tidyverse)
datos3 <- datos2

# evalúo la cantidad de missing
sum(is.na(datos3$knowITS))
sum(is.na(datos3$infectado))
sum(is.na(datos3$sexo))
sum(is.na(datos3$edad))
sum(is.na(datos3$raza)) # mantener
sum(is.na(datos3$edu)) 
sum(is.na(datos3$disc)) # mantener
sum(is.na(datos3$cronic))
sum(is.na(datos3$previnfec))
sum(is.na(datos3$sexualjob)) # mantener
sum(is.na(datos3$seguro))
sum(is.na(datos3$sustancias)) # 4618 | quitar de modelo

## PRIMER FILTRO ----
## Elimino missings de  (112) y or. sex. (113), id genero
dfiltro1 <- datos3

sum(is.na(dfiltro1$sexo)) # verifico los missings 357
sum(is.na(dfiltro1$orsex)) # verifico los missings 359

# aplico el filtro
dfiltro1 <- datos3 %>% 
            filter(!is.na(orsex),
                   !is.na(sexo))

# Compruebo filtro
sum(is.na(dfiltro1$sexo)) #filtro exitoso
sum(is.na(dfiltro1$orsex)) #filtro exitoso

# cantidad de datos filtrados: 359 *100/12026 = 2,99% = 3%

## SEGUNDO FILTRO ----
## Elimino missings de variables de interés 
# N = 11667
###  105_2: infectado y 109: conocimiento
sum(is.na(dfiltro1$infectado)) #NAs = 6
sum(is.na(dfiltro1$knowITS)) #NAs = 6

dfiltro2 <- dfiltro1 %>% 
            filter(!is.na(knowITS),
                   !is.na(infectado))

sum(is.na(dfiltro2$knowITS)) #filtro exitoso
sum(is.na(dfiltro2$infectado)) #filtro exitoso

# cantidad de datos filtrados: 6 *100/12026 = 0,05%


# TERCER FILTRO ----
## datos de las demas variables faltantes con missings
# N = 11661
library(tidyr)
sum(is.na(dfiltro2)) # se cuentan más NAs debido a que considera 
                      # por cada variable
dfiltro3 <- dfiltro2 %>% drop_na()
sum(is.na(dfiltro3)) #filtro exitoso


# cantidad de datos filtrados: 938 *100/12026 = 7.8%


# Estratificacion de variables dep e indep x orientacion sexual ----
# Infectado
library(compareGroups)
dfinal <- dfiltro3
createTable(compareGroups(data=dfinal,
                          method = c(3, # categorica
                                     2, 
                                     3, 
                                     3,
                                     3,
                                     3,
                                     3,
                                     3,
                                     3,
                                     3,
                                     3,
                                     3,
                                     3),
                                     formula = infectado~.,#
                                     byrow = T))
table(dfinal$capacitacion)
# Informacion
createTable(compareGroups(data=dfinal, 
                          formula = knowITS~.,#
                          byrow = T))


##################################################
############## TABLAS VERSION FINAL  ###################
##################################################

# TABLA 1 ----
library(compareGroups)
dfinal <- dfiltro3

createTable(compareGroups(data=dfinal))
tablafinal1 <- createTable(compareGroups(data=dfinal,
                                         method = c(3, # categorica
                                                    2, 
                                                    3, 
                                                    3,
                                                    3,
                                                    3,
                                                    3,
                                                    3,
                                                    3,
                                                    3,
                                                    3,
                                                    3,
                                                    3),byrow = T),
                           digits = 2)
tablafinal1

# EXPORTANDO TABLA
export2xls(tablafinal1, "tablas/tabla1_FINAL.xlsx")

# TABLA 2 ----
tablafinal2 <- createTable(compareGroups(data=dfinal,
                                        method = c(3, # categorica
                                                   2, 
                                                   3, 
                                                   3,
                                                   3,
                                                   3,
                                                   3,
                                                   3,
                                                   3,
                                                   3,
                                                   3,
                                                   3,
                                                   3),
                                        formula = infectado~.,#
                                        byrow = T),
                           digits = 2)

tablafinal2
export2xls(tablafinal2, "tablas/tabla2_FINAL2.0.xlsx")

#####################################################
#### Regresion logística repaso previo a poisson ####
#####################################################

library(sjPlot)

# genero otra base exclusiva para la regresion de poisson
#dfinalpoisson_respaldo <- dfinalpoisson | respaldo de regresion sin quien brindo la info de prevencion
dfinalpoisson <- dfinal
dfinal <- dfinal
# creo ID
dfinalpoisson <- tibble::rowid_to_column(dfinalpoisson, "ID")

# Trato de convertir a numerica los factores previamente creados
# porque no me permitia crear la regresion con los valores no y si
# verificar corriendo el codigo con la base de datos = datos3
# esta si tiene la categorización no y si
library(tidyverse)
table(dfinalpoisson$infectado)
dfinalpoisson <- dfinalpoisson %>% 
  mutate(infectado = recode (infectado, 
                             "No" = 0,
                             "Si" = 1)) %>% 
  glimpse()

table(dfinalpoisson$infectado)

dfinalpoisson <- as.factor(dfinalpoisson$infectado)

# modelo de 4 categorias
table(dfinalpoisson$capacitacion)
dfinalpoisson$capacitacion <- dfinal$capacitacion
table(dfinal$capacitacion)
dfinalpoisson <- dfinalpoisson %>% 
  mutate(capacitacion = recode (capacitacion, 
                             "No recibió" = 0,
                             "De una organización del Estado" = 1,
                             "De una organización de la Sociedad Civil (Colectivo, ONG, etc.)" =2,
                             "Otro" = 3)) %>% 
  glimpse()
table(dfinalpoisson$capacitacion)


## CON EL MODELO geeglm----
install.packages("geepack")
library(geepack)

# modelo bi y multivariable, aqui si debería correr
tail(dfinalpoisson)

geeglm1 <- geeglm(formula = infectado ~ knowITS,
                  data    = dfinalpoisson,
                  family  = poisson(link = "log"),
                  id      = c(1:10723),
                  corstr  = "exchangeable")
summary(geeglm1)
exp(coef(geeglm1))

geeglm2 <- geeglm(formula = infectado ~ knowITS + 
                    sexo + edad + raza + edu + orsex +
                    disc + cronic + previnfec + sexualjob +
                    seguro,
                  data    = dfinalpoisson,
                  family  = poisson(link = "log"),
                  id      = c(1:10723),
                  corstr  = "exchangeable")
summary(geeglm2)
# modelo no funciona con sjplot por ser funcion geeglm
sjPlot::tab_model(geeglm2)
?exp(coef())
# se puede resumir,,, no funciona ;-;
ci(geeglm1)

summary(exp(coef(geeglm1)))
exp(coef(glm.log.poisson))
# ultimo intento = no funciona!
cbind("PR" = exp(coef(geeglm2), exp(confint(geeglm2))))

## Tabla de contingencia
library(tableone)
createTable(compareGroups(data=dfinalpoisson, 
                          aes(data=dfinalpoisson,
                          formula=infectado~knowITS), 
                          riskratio = T, 
                          byrow = T), 
            show.ratio = T)


###################################################
###--------------- POISSON CON MODEL()----
#####################################################

## Creando función para obtener CI 95% con varianza robusta
install.packages("sandwich")
library(sandwich)

# CONSULTAR SI ME QUEDO con este o sigo con geeglm
RP.Poisson <- function(modelo) 
{
  library(sandwich)
  m1 <- modelo
  cov.m1 <- vcovHC(m1, type="HC0")
  std.err <- sqrt(diag(cov.m1))
  r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
                 LL = coef(m1) - 1.96 * std.err,
                 UL = coef(m1) + 1.96 * std.err,
                 "RP" = exp(coef(m1)),
                 "LCL" = exp(coef(m1) - 1.96 * std.err),
                 "UCL" = exp(coef(m1) + 1.96 * std.err))
  return(r.est)
  rm(m1,cov.m1,std.err,r.est)
}

# Simple
## Creando glm poisson sin varianza robusta
model1 <- glm(data=dfinalpoisson, 
             formula=infectado~knowITS, 
             family = poisson())
summary(model)
library(sjPlot)
tab_model(model1)

## Calculando IC 95% con varianza robusta metodo sandwich
RP.Poisson(model1)

# Multivariable
## Creando glm poisson sin varianza robusta
model2 <- glm(data=dfinalpoisson, 
             formula=infectado ~ knowITS + 
               sexo + edad + raza + edu + orsex +
               disc + cronic + previnfec + sexualjob +
               seguro,
             family = poisson())
tab_model(model1, model2)

## Calculando IC 95% con varianza robusta metodo sandwich
RP.Poisson(model1)
RP.Poisson(model2)


## Modelo con 4 categorias de capacitacion infectocontagiosa
model3 <- glm(data=dfinalpoisson, 
              formula=infectado ~ capacitacion,
              family = poisson())

model4 <- glm(data=dfinalpoisson, 
              formula=infectado ~ capacitacion + 
                sexo + edad + raza + edu + orsex +
                disc + cronic + previnfec + sexualjob +
                seguro,
              family = poisson())
tab_model(model3, model4)

RP.Poisson(model3)

RP.Poisson(model4)





## Fit by glm() then test using robust SE estimator
glm.log.poisson <- glm(infectado ~ knowITS + 
                         sexo + edad + raza + edu + orsex +
                         disc + cronic + previnfec + sexualjob +
                         seguro, data = dfinalpoisson,
                       family = poisson(link = "log"))
## Load sandwich package for robust estimator
library(sandwich)
## Load lmtest package for coeftest
install.packages("lmtest")
library(lmtest)
## Poisson model with SE estimated via robust variance estimator
coeftest(glm.log.poisson, vcov = sandwich)
exp(coef(glm.log.poisson))


## CON EL MODELO glm----
# con modelo de regresion poisson con glm
mrp1 <-  glm(data = dfinalpoisson, 
             formula = infectado ~ knowITS,
             family = poisson(link = "log"))


summary(mrp1)
sjPlot::tab_model(mrp1)
cbind("PR" = exp(coef(mrp1)), exp(confint(mrp1)))

coef(mrp1)
confint(mrp1)


mrp2 <-  glm(data = dfinalpoisson, 
             formula = infectado ~ knowITS +
               sexo + edad + raza + edu + orsex +
               disc + cronic + previnfec + sexualjob +
               seguro + sustancias,
             family = poisson(link = "log"))

summary(mrp2)

sjPlot::tab_model(mrp1, mrp2) # regresion exitosa!!

# grafico bonito
install.packages("forestmodel")
library(forestmodel)
forest_model(mrp1)
forest_model(mrp1,
             format_options = forest_model_format_options(colour = "grey",
                                                          color = NULL,
                                                          shape = 15,
                                                          text_size = 5,
                                                          point_size = 5,
                                                          banded = T))

forest_model(mrp2)

?forest_model
# se compararon las regresiones bivariadas de geeglm y glm
# son bastante similares
summary(geeglm2)
summary(mrp2)


############################################
###---------// analisis por orientacion sexual------------
##########################################

table(dfinal$orsex)
summary(dfinal$orsex)
library(compareGroups)
createTable(compareGroups(data=dfinal, 
                          formula = infectado~.,#
                          byrow = T))

# Gay----
# filtro
df_gay <- filter(dfinalpoisson, 
                 orsex %in% c("Gay"))

table(df_gay$orsex)

# aplico regresion
mrp1_gay <-  glm(data = df_gay, 
                 formula = infectado ~ knowITS,
                 family = poisson(link = "log"))


summary(mrp1_gay)

mrp2_gay <-  glm(data = df_gay, 
                 formula = infectado ~ knowITS +
                   sexo + edad + raza + edu +
                   disc + cronic + previnfec + sexualjob +
                   seguro,
                 family = poisson(link = "log"))

summary(mrp2_gay)
sjPlot::tab_model(mrp1_gay, mrp2_gay) # regresion exitosa!!
library(compareGroups)
createTable(compareGroups(data=df_gay, 
                          formula = infectado~.,#
                          byrow = T))
# poisson robusto
RP.Poisson(mrp1_gay)

RP.Poisson(mrp2_gay)



# Lesbian----
# filtro
table(df_lesbian$orsex)
df_lesbian <- filter(dfinalpoisson, 
                     orsex %in% c("Lesbiana"))
table(df_lesbian$orsex)

# aplico regresion
mrp1_lesbian <-  glm(data = df_lesbian, 
                     formula = infectado ~ knowITS,
                     family = poisson(link = "log"))


summary(mrp1_lesbian)

mrp2_lesbian <-  glm(data = df_lesbian, 
                     formula = infectado ~ knowITS +
                       sexo + edad + raza + edu +
                       disc + cronic + previnfec + sexualjob +
                       seguro,
                     family = poisson(link = "log"))

summary(mrp2_lesbian)
sjPlot::tab_model(mrp1_lesbian, mrp2_lesbian) # regresion exitosa!!
createTable(compareGroups(data=df_lesbian, 
                          formula = infectado~.,#
                          byrow = T))
# poisson robusto
RP.Poisson(mrp1_lesbian)

RP.Poisson(mrp2_lesbian)




# Bisexual----
# filtro
df_bisexual <- filter(dfinalpoisson, 
                      orsex %in% c("Bisexual"))
table(df_bisexual$orsex)

# aplico regresion
mrp1_bisexual <-  glm(data = df_bisexual, 
                      formula = infectado ~ knowITS,
                      family = poisson(link = "log"))


summary(mrp1_bisexual)

mrp2_bisexual <-  glm(data = df_bisexual, 
                      formula = infectado ~ knowITS +
                        sexo + edad + raza + edu +
                        disc + cronic + previnfec + sexualjob +
                        seguro + sustancias,
                      family = poisson(link = "log"))

summary(mrp2_bisexual)
sjPlot::tab_model(mrp1_bisexual, mrp2_bisexual) # regresion exitosa!!
createTable(compareGroups(data=df_bisexual, 
                          formula = infectado~.,#
                          byrow = T))
# poisson robusto
RP.Poisson(mrp1_bisexual)

RP.Poisson(mrp2_bisexual)



# Pansexual/Asexual/Other----
df_pan <- filter(dfinalpoisson, 
                 orsex %in% c("Pansexual/asexual/otro"))
table(df_pan$orsex)

# aplico regresion
mrp1_pan <-  glm(data = df_pan, 
                 formula = infectado ~ knowITS,
                 family = poisson(link = "log"))


summary(mrp1_pan)

mrp2_pan <-  glm(data = df_pan, 
                 formula = infectado ~ knowITS +
                   sexo + edad + raza + edu +
                   disc + cronic + previnfec + sexualjob +
                   seguro,
                 family = poisson(link = "log"))

summary(mrp2_pan)
sjPlot::tab_model(mrp1_pan, mrp2_pan) # regresion exitosa!!
createTable(compareGroups(data=df_pan, 
                          formula = infectado~.,#
                          byrow = T))


# poisson robusto
RP.Poisson(mrp1_pan)

RP.Poisson(mrp2_pan)


## analisis por interación--------
interac1 <-  glm(data = dfinalpoisson, 
                 formula = infectado ~ knowITS * orsex,
                 family = poisson(link = "log"))


interac2 <-  glm(data = dfinalpoisson, 
                 formula = infectado ~ knowITS * orsex +
                   sexo + edad + raza + edu +
                   disc + cronic + previnfec + sexualjob +
                   seguro,
                 family = poisson(link = "log"))

# Regresion sin varianza robusta
sjPlot::tab_model(mrp1_pan, mrp2_pan) # regresion exitosa!!
exp(coef(interac2)["knowITSSi recibió"]) * exp(coef(interac2)["orsexGay"]) *
  exp(coef(interac2)["knowITSSi recibió:orsexGay"])

# varianza robusta
RP.Poisson(interac1)
RP.Poisson(interac2)




## estimado para gay
1.79 * 3.59 * 0.76 # PR = 4.88

## estimado para lesbiana
1.79 * 1.99 * 0.31 # PR = 1.10

## estimado para bisexual
1.79 * 2.34 * 0.66 # PR = 2.76

## estimado para Pansexual/Asexual/Other
1.79 * 2.58 * 0.98 # PR = 4.52











############### OTROS INTENTOS #####################----
# otros----
datos3 <- filter(datos2, 
                  knowITS %in% c("No recibió", "Si recibió"), 
                 infectado %in% c("No", "Si"),
                 sexo %in% c("Masculino", "Femenino"))
sum(is.na(datos2))
sum(is.na(dfiltro3))
sum(is.na(datos3$sexo))
table(datos2$knowITS)
table(datos2$infectado)
table(datos2$sexo)

df <- datos3 %>% drop_na()
sum(is.na(df$sexo))
sum(is.na(datos3$sexo))


##################################################
############## CREANDO TABLAS v1 con missings  ###
##################################################

# TABLA 1 ----
library(compareGroups)
createTable(compareGroups(data=datos3))
tabla1 <- createTable(compareGroups(data=datos3,
                          method = c(3, # categorica
                                     2, 
                                     3, 
                                     3,
                                     3,
                                     3,
                                     3,
                                     3,
                                     3,
                                     3,
                                     3,
                                     3,
                                     3),byrow = T))
tabla1

# EXPORTANDO TABLA
export2xls(tabla1, "tablas/tabla1 3.0.xlsx")

# TABLA 2 ----
tabla2 <- createTable(compareGroups(data=datos3, 
                                    formula = infectado~.,#
                                    byrow = T))

tabla2
export2xls(tabla2, "tablas/tabla2 3.0.xlsx")


#####################################################
#### Regresion logística repaso previo a poisson ####
#####################################################

library(sjPlot)

datos2$knowITS<-as.factor(datos2$knowITS)
datos2$infectado<-as.factor(datos2$infectado)
table(datos3$knowITS)
table(datos3$infectado)
table(datos3$infectado, datos3$knowITS)


datos3 <- as_tibble(datos2)
glm(data = datos3, 
    formula = infectado ~ knowITS,
    family = poisson(link = "log"))

# regresion logistica 1
m1 <-  glm(data = datos3, 
          formula = infectado ~ knowITS,
          family = binomial())
summary(m1)
table(datos3$knowITS)

sjPlot::tab_model(m1)

# regresion logistica 1
names(datos3)
m2 <-  glm(data = datos3, 
           formula = infectado ~ knowITS +
           sexo + edad + raza + edu + orsex +
           disc + cronic + previnfec + sexualjob +
           seguro + sustancias,
           family = binomial())
summary(m2)

sjPlot::tab_model(m1, m2)
#######################################
######## Intento 2.0 POISSON ##########
#######################################
# genero otra base exclusiva para la regresion de poisson
datospoisson <-datos3

# creo ID
datospoisson <- tibble::rowid_to_column(datospoisson, "ID")

# Tratao de convertir a numerica los factores previamente creados
# porque no me permitia crear la regresion con los valores no y si
# verificar corriendo el codigo con la base de datos = datos3
# esta si tiene la categorización no y si
datospoisson <- datospoisson %>% 
  mutate(infectado = recode (infectado, 
                             "No" = 0,
                             "Si" = 1)) %>% 
  glimpse()

mp0 <-  glm(data = datos3, # este es el ejemplo, no correría 
            formula = infectado ~ knowITS,
            family = poisson(link = "log"))

## CON EL MODELO geeglm----
install.packages("geepack")
library(geepack)

# modelo multivariable, aqui no corre por NAs en covariables
geeglm.log.poisson <- geeglm(formula = infectado ~ knowITS + 
                              sexo + edad + raza + edu + orsex +
                             disc + cronic + previnfec + sexualjob +
                             seguro + sustancias,
                             data    = datos3,
                             family  = poisson(link = "log"),
                             id      = c(1:11993),
                             corstr  = "exchangeable")
summary(geeglm.log.poisson)

# modelo bivariable, aqui si corre porque se controlaron las var
# ahora solo hay celdas completas
geeglm.log.poisson <- geeglm(formula = infectado ~ knowITS,
                             data    = datospoisson,
                             family  = poisson(link = "log"),
                             id      = c(1:11993),
                             corstr  = "exchangeable")
summary(geeglm.log.poisson)
sjPlot::tab_model(geeglm.log.poisson) # tab model no funciona

sjPlot::tab_model(geeglm.log.poisson) # tab model no funciona


## CON EL MODEL glm----
mp1 <-  glm(data = datospoisson, 
           formula = infectado ~ knowITS,
           family = poisson(link = "log"))

?glm
summary(mp1)
sjPlot::tab_model(mp1)

mp2 <-  glm(data = datospoisson, 
           formula = infectado ~ knowITS +
             sexo + edad + raza + edu + orsex +
             disc + cronic + previnfec + sexualjob +
             seguro + sustancias,
           family = poisson(link = "log"))

summary(mp2)

sjPlot::tab_model(mp1, mp2) # regresion exitosa!!


# se compararon las regresiones bivariadas de geeglm y glm
# son bastante similares
summary(geeglm.log.poisson)
summary(mp1)
