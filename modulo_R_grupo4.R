#Proyecto Final--R
##Integrantes: Johanna Vinueza, Erika Neira, Ninibeth Bancho
##Grupo 4 - Parte 4

#Paquetes y librerias----

paquetes<-c("openxlsx", "magrittr", "tidyverse", "readr", "dplyr", "readxl")
lapply(paquetes, library, character.only=TRUE)

#Exportar datos----

balance_2014_df<-read.xlsx("Data/balances_2014.xlsx")%>% view("balance_2014")
str(balance_2014_df)

#Transformamos a tibble

ciiu_table<-read.xlsx("Data/ciiu.xlsx")%>% view("ciuu")
ciiu_df2<-tibble::as_tibble(ciiu_table)
glimpse(ciiu_df2)

#Tareas especificas

#1-----------------------------------------------------------------------------------------
#Se filtra y se toma valores positivos para luego proceder a calcular los indicadores de liquidez y solvencia y evitar divisiones entre cero
balance_2014_filter<-balance_2014_df %>% mutate(v539=ifelse(v539>0, v539, NA), v599=ifelse(v599>0, v599, NA), v698=ifelse(v698>0, v698, NA), v498=ifelse(v498>0, v498, NA)) %>% 
filter(!is.na(v539) & !is.na(v599) & !is.na(v698) & !is.na(v498)) %>%  view("balance_2014_filter")


#Se crea la base de datos con las variables solicitadas 
empresas_df1<-balance_2014_filter %>% transmute(Empresas= nombre_cia, Status= situacion, Tipo_de_empresa=tipo,
                    Pais= pais, Provincia=provincia, Canton=canton, Ciudad= ciudad, 
                    Actividad_economica= ciiu4_nivel1, Subactividad=ciiu4_nivel6,
                    Liquidez_Corriente= v345/v539, Endeudamiento_activo= v599/v499  ,
                    Endeudamiento_patrimonial= v599/v698, Endeudamiento_activo_fijo= v698/v498 , Apalancamiento= v499/v698) %>% view("empresas_df1")

str(empresas_df1)



#Cambio de codigos subactividad y actividad_economica a sus respectivas descripciones

empresas_subac<-empresas_df1 %>% inner_join(ciiu_df2, by = c("Subactividad"="CODIGO"))%>% mutate(Subactividad=DESCRIPCION) %>% select(-c(DESCRIPCION, NIVEL)) %>% view("change_Subac")

empresas_final<-empresas_subac %>% inner_join(ciiu_df2, by = c("Actividad_economica"="CODIGO")) %>% mutate(Actividad_economica=DESCRIPCION) %>% select(-c(DESCRIPCION, NIVEL)) %>% view("final_table")


# Convertir a tibble
empresas<-tibble::as_tibble(empresas_final) %>% view("empresas")
glimpse(empresas)

tabla_resumen_pichincha <- tabla_resumen %>%
  filter(Provincia == "Pichincha")


#2--------------------------------------------------------------------------------------

#Tabla total de empresas por actividad economica
tabla1_conteo_act.econo<-empresas %>% group_by(Actividad_economica) %>% count() 
tabla1_actividad_econ<-data.frame(tabla1_conteo_act.econo)%>% view("tabla_activ_economica")

#Tabla total de actividad economica por canton
tabla2_conteo_act.econo<-empresas %>% group_by(Actividad_economica, Canton) %>% count() %>% view("actividad_economica_por_canton")
tabla2_conteo_act.econo<-data.frame(tabla2_conteo_act.econo)%>% view("tabla_activ_economica")


#con el numero total de empresas listadas todas en una misma columna
tabla2a_conteo_act.econo <- tabla2_conteo_act.econo %>%
  group_by(Actividad_economica) %>%
  summarise(No_compañias = n())


total_compañias <- tabla2a_conteo_act.econo %>%
  summarise(No_compañias = sum(No_compañias)) %>%
  mutate(Actividad_economica = "Total")

tabla2a_conteo_act.econo <- bind_rows(tabla2a_conteo_act.econo, total_compañias)



#Una sola tabla
#Convertir de col a fila
pivot_tablafinal_df<-tabla2_conteo_act.econo %>%
  pivot_wider(names_from= Actividad_economica, values_from = n ) %>% view("table_1")

#Reemplazando los NA por zero para obtener el total 
pivot_tablafinal_df <- pivot_tablafinal_df %>%
  mutate(across(everything(), ~replace_na(., 0))) %>% view("sin NA")


glimpse(pivot_tablafinal_df)

#Adding a row which contain the sum by column

table_sum_bycolumn<-pivot_tablafinal_df %>% select(-Canton) %>% summarise(across(everything(), sum)) %>% view("sum")


#Tabla final que resume el número total de empresas por actividad económica y por actividad económica por cantón

table_summarize<- pivot_tablafinal_df %>% bind_rows(table_sum_bycolumn) %>% view("Tabla_resumen")
total_count<- "Total"   ##esta ultima parte sale NA EN LA FILA enVez de TOTAL--



#Table como un tibble
table_Resumen_final<-table_summarize %>% mutate(Canton=ifelse(is.na(Canton),total_count , Canton))  %>% view("Tabla_resumen1")
glimpse(table_Resumen_final)

#3-------------------------------------------------------------------------------------------------------------------------------
#Gráfico de barras para indicador de liquidez


# First, calculate the mean Liquidez_Corriente for each province
mean_liquidez_by_province <- aggregate(Liquidez_Corriente ~ Provincia, data = empresas, FUN = mean)

# Sort the provinces based on mean Liquidez_Corriente in descending order
top_provinces <- head(mean_liquidez_by_province[order(-mean_liquidez_by_province$Liquidez_Corriente), ], 5)

# Filter the data to keep only the rows corresponding to the top provinces
empresas_top_provinces <- subset(empresas, Provincia %in% top_provinces$Provincia)



ggplot(empresas_top_provinces, aes(x=Provincia, y=Liquidez_Corriente)) + 
  geom_bar(stat = "identity", position = "dodge") + labs(title="Comparativo de Indicador de Liquidez Corriente", x="Status", y="Liquidez" )+
  facet_wrap(~Status)

ggplot(empresas_top_provinces, aes(x = Provincia, y = Liquidez_Corriente, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparativo de Indicador de Liquidez Corriente por Status y Provincia",
       x = "Provincia", y = "Liquidez") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Activo" = "blue", "Inactivo" = "red"))




#Opcion 1

empresas_unique_provincias <- distinct(empresas_top_provinces, Provincia, Status, .keep_all = TRUE)

ggplot(empresas_unique_provincias, aes(x=Liquidez_Corriente, y=Provincia)) + 
  geom_bar(stat = "identity", position = "dodge") + labs(title="Comparativo de Indicador de Liquidez Corriente", x="Pron", y="Liquidez" )+
  facet_wrap(~Status)

ggplot(empresas_unique_provincias, aes(x = Liquidez_Corriente, y = Provincia)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparativo de Indicador de Liquidez Corriente",
       x = "Pron", y = "Liquidez") +
  facet_grid(Status ~ ., scales = "free_y", space = "free_y") +
  theme(axis.text.y = element_text(hjust = 0)) +
  coord_flip()


#Opcion 2
empresas_unique_provincias <- distinct(empresas, Provincia, Status, .keep_all = TRUE)

ggplot(empresas_unique_provincias, aes(x=Status, y=Liquidez_Corriente)) + 
  geom_bar(stat = "identity", position = "dodge") + labs(title="Comparativo de Indicador de Liquidez Corriente", x="Pron", y="Liquidez" )+
  facet_wrap(~Provincia)


##GRAFICA POR LIQUIDEZ CORRIENTE SEGUN STATUS Y PROVINCIA EXCLUYENDO MANABI POR DISTORCIONAR LA GRAFICA

ggplot(table_Resumen_final1, aes(x = Provincia, y = Liquidez_Corriente, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Índice de Liquidez Corriente por Status y Provincia",
       x = "Provincia", y = "Liquidez Corriente") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Status")


##GRAFICA POR ENDEUDAMIENTO DE ACTIVO SEGUN STATUS Y PROVINCIA EXCLUYENDO MANABI POR DISTORCIONAR LA GRAFICA

ggplot(table_Resumen_final1 , aes(x = Provincia, y = Endeudamiento_activo, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Índice de Endeudamiento del activo por Status y Provincia",
       x = "Provincia", y = "Endeudamiento del activo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Status")

#4--------------------------------------------------------------------------------------------------

#tratando de ingresar el tamaño de la compañia
empresas2<-balance_2014_filter %>% transmute(Empresas= nombre_cia, Status= situacion, Tipo_de_empresa=tipo,
                                                País= pais, Provincia=provincia, Canton=canton, Ciudad= ciudad, 
                                                Actividad_economica= ciiu4_nivel1, Subactividad=ciiu4_nivel6, tipo_cia =tamanio,
                                                N_Direc = trab_direc, N_adm =trab_admin, Liquidez_Corriente= v345/v539, Endeudamiento_activo= v599/v499  ,
                                                Endeudamiento_patrimonial= v599/v698, Endeudamiento_activo_fijo= v698/v498 , Apalancamiento= v499/v698) %>% view("empresas_con_tamaño")

# análisis endeudamiento del activo entre pequeñas 
comparacion_endeudamiento <- empresas2 %>%
  mutate(
    Categoria_Empresa = ifelse(tipo_cia %in% c("MICRO", "PEQUEÑA"), "MICRO + PEQUEÑA", "GRANDE")
  ) %>%
  group_by(tipo_cia) %>%
  summarise(Promedio_Endeudamiento_Activo = mean(Endeudamiento_activo))



#realizando comparativa de liquidez

comparativa_liquidez <- empresas2 %>% 
  group_by(Tipo_de_empresa) %>% 
  mutate(
    Cumple_Condiciones = case_when(
      is.na(N_Direc) | is.na(N_adm) ~ NA_character_,
      N_Direc > 60 & N_adm >= 100 & N_adm <= 800 ~ "Cumple",
      TRUE ~ "No Cumple"
    )   
  ) %>% view ("liquidez_final")


resultados_por_tipo <- comparativa_liquidez %>%
  group_by(Tipo_de_empresa, Cumple_Condiciones) %>%
  summarize(
    Promedio_Endeudamiento_Activo = mean(Endeudamiento_activo, na.rm = TRUE),
    Promedio_Liquidez_Corriente = mean(Liquidez_Corriente, na.rm = TRUE)
  )

