#Proyecto Final--R
##Integrantes: Johanna Vinueza, Erika Neira
##Grupo 4 - Parte corregida

#Paquetes y librerias----
library(conflicted)
library(openxlsx)
library(magrittr)
library(tibble)
library(ggplot2)
library(tinytex)
library(dplyr)
library(knitr)
library(tidyr)
library(gridExtra)

#Exportar datos----

balance_2014_df<-read.xlsx("Data/balances_2014.xlsx")%>% view("balance_2014")
ciiu_table<-read.xlsx("Data/ciiu.xlsx")%>% view("ciuu")
str(balance_2014_df)
balance_2014_df1<-tibble(balance_2014_df)
glimpse(balance_2014_df)

empresas_df1<-balance_2014_df1 %>% transmute(Empresas= nombre_cia, Status= situacion,
                                                Tipo_de_empresa=tipo,País= pais, Provincia=provincia, Canton=canton, Ciudad= ciudad, 
                                                Actividad_economica= ciiu4_nivel1, Subactividad=ciiu4_nivel6,Tipo_cia =tamanio,
                                                N_Direc = trab_direc, N_adm =trab_admin,
                                                Liquidez_Corriente= v345/v539, Endeudamiento_activo= v599/v499,
                                                Endeudamiento_patrimonial= v599/v698, Endeudamiento_activo_fijo= v698/v498,
                                                Apalancamiento= v499/v698) %>% view("empresas_df1")

empresas_fin<-empresas_df1 %>% 
  left_join(ciiu_df2, by = c("Actividad_economica"="CODIGO")) %>%
  mutate(Actividad_economica=DESCRIPCION) %>% select(-c(DESCRIPCION, NIVEL)) %>% 
  view("final_table")


empresas_subact<-empresas_fin %>% 
  left_join(ciiu_df2, by = c("Subactividad"="CODIGO"))%>% 
  mutate(Subactividad=DESCRIPCION) %>% select(-c(DESCRIPCION, NIVEL)) %>% 
  view("change_Subac")

balance_2014_df2<- empresas_subact %>% 
  filter_all(all_vars(!is.numeric(.) | (. > 0 & is.finite(.))))


empresas_segun_canton<- balance_2014_df2%>% group_by(Actividad_economica,Canton) %>% 
  summarise(total_empresas=n(), .groups = "drop") %>%
  arrange(desc(total_empresas)) %>%
  head(10)

sumas_activos_pasivos <- balance_2014_df %>%
  group_by(situacion, provincia) %>%
  summarize(
    Suma_Activo_Corriente = sum(v345, na.rm = TRUE),
    Suma_Pasivo_Corriente = sum(v539, na.rm = TRUE)
  ) 

# Filtrar solo las empresas activas y calcular la liquidez
sumas_activos_pasivos <- sumas_activos_pasivos %>%
  mutate(liquidez_corriente = Suma_Activo_Corriente / Suma_Pasivo_Corriente)

sumas_activos_pasivos_activos <- sumas_activos_pasivos %>%
  dplyr::filter(situacion == "ACTIVA") %>%
  group_by(provincia)%>%
  mutate(mean(liquidez_corriente = Suma_Activo_Corriente / Suma_Pasivo_Corriente))

balance_2014_df2_activas <- balance_2014_df2%>%
  dplyr::filter(Status == "ACTIVA")

# Crear el gráfico de líneas
grafico <- ggplot(balance_2014_df2_activas, aes(x = Provincia, y = Liquidez_Corriente)) +
  geom_line() +
  labs(x = "", y = "Liquidez Corriente", title = "Liquidez Corriente para Empresas Activas por Provincia") +
  facet_wrap(~ Status) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar etiquetas del eje x
        legend.position = "none")  # Eliminar la leyenda de color

balance_2014_df2_liq <- balance_2014_df2%>%
  dplyr::filter(Status == "DISOLUC. LIQUIDAC. OFICIO INSC. EN RM")

# Crear el gráfico de líneas
grafico2 <- ggplot(balance_2014_df2_liq, aes(x = Provincia, y = Liquidez_Corriente)) +
  geom_line() +
  labs(x = "", y = "Liquidez Corriente", title = "Liquidez Corriente para Empresas en Liquidacion  por oficio por Provincia") +
  facet_wrap(~ Status) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar etiquetas del eje x
        legend.position = "none")  # Eliminar la leyenda de color
### ultima 


##Contruyendo grafica_1
status_to_filter <- c("DISOLUC. LIQUIDAC. OFICIO INSC. EN RM", "ACTIVA")
balance_2014_df2_filtered <- balance_2014_df2 %>%
  dplyr::filter(Status %in% status_to_filter) %>%
  group_by(Status) %>%
  dplyr::filter(!all(is.na(Liquidez_Corriente)))

# Crear los gráficos de líneas para cada estado por provincia
grafico1 <- ggplot(balance_2014_df2_filtered, aes(x = Provincia, y = Liquidez_Corriente, color = Provincia)) +
  geom_line() +
  labs(x = "", y = "Liquidez Corriente", title = "LIQUIDEZ CORRIENTE DE EMPRESAS SEGUN STATUS Y PROVINCIA") +
  facet_wrap(~ Status, ncol = 1, scales = "free_x") +  # Ajustar los ejes x para cada gráfico
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none",
        plot.title = element_text(hjust = 0.5))
# Mostrar el gráfico1
print(grafico1)

##Contruyendo graficaS POR STATUS
status_to_filter <- c("DISOLUC. LIQUIDAC. OFICIO INSC. EN RM", "ACTIVA")
balance_2014_df2_filtered <- balance_2014_df2 %>%
  dplyr::filter(Status %in% status_to_filter) %>%
  group_by(Status) %>%
  dplyr::filter(!all(is.na(Endeudamiento_activo)))



# Crear los gráficos de líneas para cada estado por provincia
grafico3 <- ggplot(balance_2014_df2_filtered, aes(x = Provincia, y = Endeudamiento_patrimonial, color = Provincia)) +
  geom_line() +
  labs(x = "", y = "Endeudamiento Patrimonio", title = "ENDEUDAMIENTO PATRIMONIAL DE EMPRESAS SEGUN STATUS Y PROVINCIA") +
  facet_wrap(~ Status, ncol = 1, scales = "free_x") +  # Ajustar los ejes x para cada gráfico
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none",
        plot.title = element_text(hjust = 0.5))
# Mostrar el gráfico2
print(grafico3)

status_to_filter <- c("DISOLUC. LIQUIDAC. OFICIO INSC. EN RM", "ACTIVA", "DISOLUC. Y LIQUIDAC. ANTIC. NO INSC. EN RM")
variables_to_plot <- c("Endeudamiento_patrimonial", "Liquidez_Corriente", "Endeudamiento_activo_fijo",
                       "Apalancamiento", "Endeudamiento_activo")

status_to_filter <- c("DISOLUC. LIQUIDAC. OFICIO INSC. EN RM", "ACTIVA", "DISOLUC. Y LIQUIDAC. ANTIC. NO INSC. EN RM")


# Crear y mostrar gráficos para cada variable
variables_to_plot <- c("Endeudamiento_patrimonial", "Liquidez_Corriente", "Endeudamiento_activo_fijo",
                       "Apalancamiento", "Endeudamiento_activo")

for (variable in variables_to_plot) {
  create_and_show_graph(variable)
}

##En este código, hemos calculado el promedio de endeudamiento patrimonial por provincia y lo hemos utilizado para crear el gráfico. Las etiquetas se muestran solo para los 3 valores más grandes en cada estado, y el formato de las etiquetas se ajusta para mostrar el promedio con dos decimales.









print(grafico2)


# Crear el gráfico con facet_wrap
grafico1 <- ggplot(datos_provincias, aes(x = situacion, y = liquidez_corriente, fill = situacion)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Situación y Liquidez Corriente por Provincia",
       x = "Situación",
       y = "Liquidez Corriente") +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~ provincia, ncol = 2) 
print(grafico1)





## para comparar endeudamiento---
comparacion_endeudamiento2 <- balance_2014_df2 %>%
  mutate(
    Categoria_Empresa = case_when(
      Tipo_cia %in% c("MICRO", "PEQUEÑA") ~ "MICRO + PEQUEÑA",
      TRUE ~ "GRANDE"
    )
  ) %>%
  group_by(Categoria_Empresa) %>%
  summarise(Promedio_Endeudamiento_Activo = mean(Endeudamiento_activo))



  
#Transformamos a tibble
ciiu_df2<-tibble::as_tibble(ciiu_table)
glimpse(ciiu_df2)

# Filtrar los datos por los estados "ACTIVA" y "LIQUIDACION"
filtered_data <- balance_2014_df2 %>%
dplyr::filter(Status %in% c("ACTIVA", "LIQUIDACION"))

# Calcular el promedio de Endeudamiento_activo por estado, provincia y situación
average_endeudamiento_activo <- filtered_data %>%
  group_by(Ciudad,Provincia)
  summarise(avg_Endeudamiento_activo = mean(pmax(0, pmin(Endeudamiento_activo, 12)), na.rm = TRUE))

# Crear gráficos lineales por provincia y estado usando ggplot2
graphs_list <- lapply(unique(average_endeudamiento_activo$Provincia), function(provincia) {
  graph <- ggplot(average_endeudamiento_activo %>% filter(Provincia == provincia),
                  aes(x = Status, y = avg_Endeudamiento_activo, color = Status)) +
    geom_line() +
    labs(x = "", y = "Promedio de Endeudamiento Activo",
         title = paste("Promedio de Endeudamiento Activo para", provincia)) +
    theme_minimal() +
    theme(legend.position = "top")
  return(graph)
})

