#---------------------------------CASE STUDY ----------------------------------

#Cargar paquetes --------------------------------------------------------------
library(haven)
library(dplyr)

#Cargar datos -----------------------------------------------------------------
envigmu <- read_dta("Data/201911_EnvigmuBDD_casadas_unidas.dta")

#Limpieza de datos ------------------------------------------------------------

#Para el presente caso de estudio se creará una nueva variable que indique: 
#1 si la mujer ha sufrido algún tipo de violencia (física, psicológica, sexual,
# económica/patrimonial) y 0 en caso contrario, para esto se utiliza la SECCIÓN 
#7A: VIDA EN PAREJA RELACIÓN ACTUAL - TIPOS DE VIOLENCIA

data_study <- envigmu %>% select(id_per:persona,starts_with("f2_s7a_14"), f2_s3a_1) %>% 
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
  )
  )

  # Prueba de proporciones ---------------------------------------------------

# H0: No hay diferencia entre la proporcion de las mujeres que trabajan y no trabajan y han sufrido violencia
# H1: Si existe diferencia entre la proporcion de las mujeres que trabajan y no trabajan y han sufrido violencia

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


