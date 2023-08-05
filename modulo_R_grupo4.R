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

empresas_df1<-balance_2014_df %>% transmute(Empresas= nombre_cia, Status= situacion, Tipo_de_empresa=tipo,
                    País= pais, Provincia=provincia, Canton=canton, Ciudad= ciudad, 
                    Actividad_economica= ciiu4_nivel1, Subactividad=ciiu4_nivel6) %>% view("empresas_df1")

str(empresas_df1)







