#Proyecto Final--R
##Integrantes: Johanna Vinueza, Erika Neira, Ninibeth Bancho
##Grupo 4 - Parte 4

#Paquetes y librerias----

paquetes<-c("openxlsx", "magrittr", "tidyverse", "readr", "dplyr", "readxl")
lapply(paquetes, library, character.only=TRUE)

#Exportar datos----

balance_2014_df<-read.xlsx("Data/balances_2014.xlsx")%>% view("balance_2014")

str(balance_2014_df)

#Tareas especificas

#1-----------------------------------------------------------------------------------------
#Se filtra y se toma valores positivos para luego proceder a calcular los indicadores de liquidez y solvencia y evitar divisiones entre cero
balance_2014_filter<-balance_2014_df %>% mutate(v539=ifelse(v539>0, v539, NA), v599=ifelse(v599>0, v599, NA), v698=ifelse(v698>0, v698, NA), v498=ifelse(v498>0, v498, NA)) %>% 
filter(!is.na(v539) & !is.na(v599) & !is.na(v698) & !is.na(v498)) %>%  view("balance_2014_filter")


#Se crea la base de datos con las variables solicitadas 
empresas_df1<-balance_2014_filter %>% transmute(Empresas= nombre_cia, Status= situacion, Tipo_de_empresa=tipo,
                    País= pais, Provincia=provincia, Canton=canton, Ciudad= ciudad, 
                    Actividad_economica= ciiu4_nivel1, Subactividad=ciiu4_nivel6,
                    Liquidez_Corriente= v345/v539, Endeudamiento_activo= v499/v599  ,
                    Endeudamiento_patrimonial= v499/v698, Endeudamiento_activo_fijo= v698/v498 , Apalancamiento= v599/v698) %>% view("empresas_df1")

str(empresas_df1)

# Convertir a tibble
empresas<-tibble::as_tibble(empresas_df1)
glimpse(empresas)

#2--------------------------------------------------------------------------------------

tabla1_conteo_act.econo<-empresas %>% group_by(Actividad_economica) %>% count() 
tabla1_actividad_econ<-data.frame(tabla1_conteo_act.econo)%>% view("tabla_activ_economica")

tabla2_conteo_act.econo<-empresas %>% group_by(Actividad_economica, Canton) %>% count() %>% view("actividad_economica_por_canton")
tabla2_conteo_act.econo<-data.frame(tabla2_conteo_act.econo)%>% view("tabla_activ_economica")

#Convertir col a fila- tabla
pivot_tablafinal_df<-tabla2_conteo_act.econo %>%
  pivot_wider(names_from= Actividad_economica, values_from = n ) %>% view()

#Pendientes:
# Sacar la suma por actividad economica, i,e el total y hacer el cambio a la descripcion de codigo ciiu
# filtro por pequenas, grandes y medianas empresas del endeudamiento del activo 





