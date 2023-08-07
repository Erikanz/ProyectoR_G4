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
head(empresas)
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
#Gráficos correspondientes al indicador Liquidez y Solvencia por Status y Provincia


#Grafica de Liquidez Corriente por Status y Provincia

summarized_data_0 <- empresas %>%
  group_by(Provincia, Status) %>% summarize(Mean_Liquidez_Corriente = mean(Liquidez_Corriente, na.rm = TRUE)) 

fig0<-ggplot(summarized_data_0, aes(x = Provincia  , y = Mean_Liquidez_Corriente , fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Promedio de Índice de Liquidez Corriente por Provincia y Status",
       x = "Provincia", y = "Promedio por Liquidez Corriente") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Status") + facet_grid(~ Provincia, scales = "free_x", space = "free_x")

#Grafica de Solvencia por Status y Provincia: Endeudamiento Activo

summarized_data_1 <- empresas %>%
  group_by(Provincia, Status) %>% summarize(Mean_Endeudamiento_activo = mean(Endeudamiento_activo, na.rm = TRUE)) %>% view("t2")

fig1<-ggplot(summarized_data_1, aes(x = Provincia, y = Mean_Endeudamiento_activo, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Promedio de Índice de Endeudamiento del activo por Provincia y Status",
       x = "Provincia", y = "Promedio de Endeudamiento de activo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Status") + facet_grid(~ Provincia, scales = "free_x", space = "free_x")

#Grafica de Solvencia por Status y Provincia: Endeudamiento Patrimonial

summarized_data_2 <- empresas %>%
  group_by(Provincia, Status) %>% summarize(Mean_Endeudamiento_patrimonial = mean(Endeudamiento_patrimonial, na.rm = TRUE)) %>% view("t3")

ggplot(summarized_data_2, aes(x = Provincia, y = Mean_Endeudamiento_patrimonial, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Promedio de Índice de Endeudamiento patrimonial por Provincia y Status",
       x = "Provincia", y = "Promedio de Endeudamiento patromonial") +

  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Status") + facet_grid(~ Provincia, scales = "free_x", space = "free_x")


#Grafica de Solvencia por Status y Provincia:Endeudamiento_Activo_Fijo
summarized_data_3 <- empresas %>%
  group_by(Provincia, Status) %>% summarize(Mean_Endeudamiento_activo_fijo = mean(Endeudamiento_activo_fijo, na.rm = TRUE)) %>% view("t4")

ggplot(summarized_data_3, aes(x = Provincia, y = Mean_Endeudamiento_activo_fijo, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Promedio de Índice de Endeudamiento activo fijo por Provincia y Status",
       x = "Provincia", y = "Promedio de Endeudamiento activo fijo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Status") + facet_grid(~ Provincia, scales = "free_x", space = "free_x")

#Grafica de Solvencia por Status y Provincia:Apalancamiento

summarized_data_4 <- empresas %>%
  group_by(Provincia, Status) %>% summarize(Mean_Apalancamiento = mean(Apalancamiento, na.rm = TRUE)) %>% view("t5")

ggplot(summarized_data_4, aes(x = Provincia, y = Mean_Apalancamiento, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Promedio de Índice de Apalancamiento por Provincia y Status",
       x = "Provincia", y = "Promedio de Apalancamiento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_discrete(name = "Status") + facet_grid(~ Provincia, scales = "free_x", space = "free_x")


#Gráficos correspondientes al indicador Liquidez y Solvencia por Tipo de Empresa


#Grafico de Liquidez Corriente por tipo de empresa

summarized_data_5 <- empresas %>%
  group_by(Tipo_de_empresa) %>% summarize(Mean_Liquidez_Corriente = mean(Liquidez_Corriente, na.rm = TRUE)) %>% view("Tte1")

ggplot(summarized_data_5, aes(x = Tipo_de_empresa  , y = Mean_Liquidez_Corriente)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Promedio de Índice de Liquidez Corriente por Tipo de Empresa",
       x = "Tipo de Empresa", y = "Liquidez Corriente") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Grafica de Solvencia por Tipo de Empresa: Endeudamiento Activo

summarized_data_6 <- empresas %>%
  group_by(Tipo_de_empresa) %>% summarize(Mean_Endeudamiento_Activo = mean(Endeudamiento_activo, na.rm = TRUE)) %>% view("Tte2")

ggplot(summarized_data_6, aes(x = Tipo_de_empresa  , y = Mean_Endeudamiento_Activo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Promedio de Índice de Endeudamiento activo por Tipo de Empresa",
       x = "Tipo de Empresa", y = "Endeudamiento Activo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Grafica de Solvencia por Tipo de Empresa: Endeudamiento Patrimonial

summarized_data_7 <- empresas %>%
  group_by(Tipo_de_empresa) %>% summarize(Mean_Endeudamiento_Patrimonial = mean(Endeudamiento_patrimonial, na.rm = TRUE)) %>% view("Tte3")

ggplot(summarized_data_7, aes(x = Tipo_de_empresa  , y = Mean_Endeudamiento_Patrimonial)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Promedio de Índice de Endeudamiento patrimonial por Tipo de Empresa",
       x = "Tipo de Empresa", y = "Endeudamiento Patrimonial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Grafica de Solvencia por Tipo de Empresa: Endeudamiento_Activo_Fijo

summarized_data_8 <- empresas %>%
  group_by(Tipo_de_empresa) %>% summarize(Mean_Endeudamiento_activo_fijo = mean(Endeudamiento_activo_fijo, na.rm = TRUE)) %>% view("Tte4")

ggplot(summarized_data_8, aes(x = Tipo_de_empresa  , y = Mean_Endeudamiento_activo_fijo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Promedio de Índice de Endeudamiento activo fijo por Tipo de Empresa",
       x = "Tipo de Empresa", y = "Endeudamiento Activo fijo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Grafica de Solvencia por Tipo de Empresa: Apalancamiento


summarized_data_9 <- empresas %>%
  group_by(Tipo_de_empresa) %>% summarize(Mean_Apalancamiento = mean(Apalancamiento, na.rm = TRUE)) %>% view("Tte5")

ggplot(summarized_data_9, aes(x = Tipo_de_empresa  , y = Mean_Apalancamiento)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Promedio de Índice de Apalancamiento por Tipo de Empresa",
       x = "Tipo de Empresa", y = "Apalancamiento") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#4--------------------------------------------------------------------------------------------------


#Para responder preguntas
#tratando de ingresar el tamaño de la compañia
empresas2<-balance_2014_filter %>% transmute(Empresas= nombre_cia, Status= situacion, Tipo_de_empresa=tipo,
                                                País= pais, Provincia=provincia, Canton=canton, Ciudad= ciudad, 
                                                Actividad_economica= ciiu4_nivel1, Subactividad=ciiu4_nivel6, tipo_cia =tamanio,
                                                N_Direc = trab_direc, N_adm =trab_admin, Liquidez_Corriente= v345/v539, Endeudamiento_activo= v599/v499  ,
<<<<<<< HEAD
                                                Endeudamiento_patrimonial= v599/v698, Endeudamiento_activo_fijo= v698/v498 , Apalancamiento= v499/v698) %>% view("empresas_con_tamaño")

# análisis endeudamiento del activo entre pequeñas 
comparacion_endeudamiento <- empresas2 %>%
=======
                                                Endeudamiento_patrimonial= v599/v698, Endeudamiento_activo_fijo= v698/v498 , Apalancamiento= v499/v698) %>% view("empresas_con_tamano")

# análisis endeudamiento del activo entre pequeñas 
comparacion_endeudamiento <- table_Resumen_fin %>%
>>>>>>> 1a96f0704bd8658df91e99bb2c2e92c4a3dc0749
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

