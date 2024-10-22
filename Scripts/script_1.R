#---------------------------------CASE STUDY ----------------------------------

#Cargar paquetes --------------------------------------------------------------
library(haven)
library(dplyr)
library(readr)

#Cargar datos -----------------------------------------------------------------
envigmu <- read_dta("Data/201911_EnvigmuBDD_casadas_unidas.dta")

#Limpieza de datos ------------------------------------------------------------

#Data envigmu
#Para el presente caso de estudio se creará una nueva variable que indique: 
#1 si la mujer ha sufrido algún tipo de violencia (física, psicológica, sexual,
# económica/patrimonial) y 0 en caso contrario, para esto se utiliza la SECCIÓN 
#7A: VIDA EN PAREJA RELACIÓN ACTUAL - TIPOS DE VIOLENCIA

data_study <- envigmu %>% select(id_per,starts_with("f2_s7a_14"), f2_s3a_1,f2_s7b_19_7) %>% 
  mutate(violencia=case_when(
      f2_s7a_14_1 %in% c(1, 2, 3) | 
      f2_s7a_14_2 %in% c(1, 2, 3) | 
      f2_s7a_14_3 %in% c(1, 2, 3) | 
      f2_s7a_14_4 %in% c(1, 2, 3) | 
      f2_s7a_14_5 %in% c(1, 2, 3) | 
      f2_s7a_14_6 %in% c(1, 2, 3) | 
      f2_s7a_14_7 %in% c(1, 2, 3) | 
      f2_s7a_14_8 %in% c(1, 2, 3) | 
      f2_s7a_14_9 %in% c(1, 2, 3) | 
      f2_s7a_14_10 %in% c(1, 2, 3) | 
      f2_s7a_14_11 %in% c(1, 2, 3) | 
      f2_s7a_14_12 %in% c(1, 2, 3) | 
      f2_s7a_14_13 %in% c(1, 2, 3) | 
      f2_s7a_14_14 %in% c(1, 2, 3) | 
      f2_s7a_14_15 %in% c(1, 2, 3) | 
      f2_s7a_14_16 %in% c(1, 2, 3) | 
      f2_s7a_14_17 %in% c(1, 2, 3) | 
      f2_s7a_14_18 %in% c(1, 2, 3) | 
      f2_s7a_14_19 %in% c(1, 2, 3) | 
      f2_s7a_14_20 %in% c(1, 2, 3) | 
      f2_s7a_14_21 %in% c(1, 2, 3) | 
      f2_s7a_14_22 %in% c(1, 2, 3) | 
      f2_s7a_14_23 %in% c(1, 2, 3)  ~ 1,
    TRUE ~ 0
  ),
  trabajo = case_when(
    f2_s3a_1 == 1 ~ 1,   
    f2_s3a_1 == 2 ~ 0,   
    TRUE ~ NA_real_      
  ),
  celos= case_when(
    f2_s7b_19_7==1~1,
    f2_s7b_19_7==2~0)
  )



# Prueba de proporciones 1---------------------------------------------------

# H0: No hay diferencia entre la proporcion de las mujeres casadas que han sufrido 
# violencia que trabajan vs las que no trabajan 
# H1: Si existe diferencia entre la proporcion de las mujeres casadas que han 
# sufrido violencia que trabajan vs las que no trabajan 
# Se calcula el número de mujeres que han sufrido violencia en cada grupo
si_trabajo <- sum(data_study$trabajo == 1)  # Total de mujeres que trabajan
no_trabajo <- sum(data_study$trabajo == 0)  # Total de mujeres que no trabajan

n_violencia_trabajo <- sum(data_study$trabajo == 1 & data_study$violencia == 1)  # Mujeres que trabajan y han sufrido violencia
n_violencia_no_trabajo <- sum(data_study$trabajo == 0 & data_study$violencia == 1)  # Mujeres que no trabajan y han sufrido violencia

# Realizar la prueba de proporciones
resultado_proporciones <- prop.test(c(n_violencia_trabajo, n_violencia_no_trabajo), 
                                    c(si_trabajo, no_trabajo), 
                                    alternative = "two.sided")

# Resultado de la prueba de proporciones
print(resultado_proporciones)

# De los resultados de la hipótesis se obtuvo que si hay una diferencia 
# significativa entre las mujeres casadas que han sufrido violencia que sí
# trabajan vs las que no trabajan

# Prueba de proporciones 2------------------------------------------------------

# Según una investigación previa, las mujeres que trabajan tienen mayor probabilidad de 
# experimentar violencia por parte de su pareja, lo vamos a comprobar con la siguiente prueba.

# H0: No hay diferencia entre la proporcion de las mujeres casadas que han sufrido
# violencia que trabajan vs las que no trabajan 
# H1: La proporción de mujeres casadas que han sufrido violencia y que trabajan 
# es mayor a la proporción de mujeres casadas que han sufrido violencia y no trabajan.

prop.test(c(n_violencia_trabajo, n_violencia_no_trabajo), 
          c(si_trabajo, no_trabajo), 
          alternative = "greater")

#Los resultados indican que es posible rechazar H0 en favor de H1. 

# Prueba de proporciones 3------------------------------------------------------

# H0: No hay diferencia en la proporción de mujeres que creen que el maltrato fue d
# debido a celos frente a las que no lo creen.

# H1:Hay una diferencia en la proporción de mujeres que creen que el maltrato fue 
# debido a celos frente a las que no lo creen

# Mujeres que han sufrido violencia
mujeres_violencia <- data_study %>% filter(violencia==1)
  
celos <- table(mujeres_violencia$celos)["1"]  # Mujeres que creen que fue por celos
no_celos <- table(mujeres_violencia$celos)["0"]  #Mujeres que no creen que fue por celos
n_total_violencia <- nrow(mujeres_violencia) # Número total de mujeres que han sufrido violencia

# Realizar la prueba de proporciones
prop.test(c(celos, no_celos), 
          c(n_total_violencia, n_total_violencia), 
           alternative = "two.sided")

# Dado que el resultado es significativo, se propone la siguiente prueba de proporciones:

# prueba de proporciones 4------------------------------------------------------
# H0: No hay diferencia en la proporción de mujeres que creen que el maltrato fue d
# debido a celos frente a las que no lo creen.
# H1: La proporción de mujeres que cren que el maltrado fue por causa de celos
# es mayor en comparación con la que no creen que celos haya sido la causa
prop.test(c(celos, no_celos), 
          c(n_total_violencia, n_total_violencia), 
          alternative = "greater")

#No hay evidencia suficente para rechazar H0..

