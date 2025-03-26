#Base de datos para proyecto final

#librerias
#necesario para correr el codigo tener la dirección del csv "viviendas" de la ENIGH en setwd()
library(tidyverse)
library(tidymodels)
library(duckdb)
library(themis)


setwd('E:/Ciencias de datos/R scripts/ENIGH/enigh2022_ns_hogares_csv')


#creando un duckdb para generar la base 
#######################################
#establecer conexion a duckdb
conexion = dbConnect(
  drv = duckdb::duckdb(),
  dbdir = 'ENIGH.duckdb'
)

#nos devuelve una base vacía
dbListTables(conn = conexion)

#lista de archivos en carpeta (concetradohogar & hogares)
# Lista de archivos en la carpeta
archivos <- list.files(pattern = '.csv')


# Leer y crear tablas para cada archivo CSV
for (archivo in archivos) {
  nombre_tabla <- archivo %>%
    str_remove('.csv') %>%
    tolower()
  # Crear una tabla con los csv extraídos
  if (archivo == "viviendas.csv") {
    # Sobrescribir el tipo de 'mat_pisos' a 'VARCHAR' para viviendas.csv
    query <- sprintf(
      fmt = "CREATE TABLE %s AS SELECT * FROM read_csv_auto('%s', types={'mat_pisos': 'VARCHAR'});",
      nombre_tabla,
      archivo
    )
  } else {
    # Para otros archivos, usa read_csv_auto sin modificar los tipos
    query <- sprintf(
      fmt = "CREATE TABLE %s AS SELECT * FROM read_csv_auto('%s');",
      nombre_tabla,
      archivo
    )
  }
  
  # Ejecutar el query 
  print(query)
  dbExecute(conn = conexion, statement = query)
}

dbDisconnect(conexion)
#######################################


#CREAR VARIABLES DE REZAGO Y NECESIDADES DE AMPLIACIÓN
#######################################
#establecer conexion a duckdb
conexion = dbConnect(
  drv = duckdb::duckdb(),
  dbdir = 'ENIGH.duckdb'
)
#extraer las variables necesarias de la tabla viviendas
query = "
SELECT
  folioviv,
  factor,
  mat_pared,
  mat_techos,
  mat_pisos,
  tot_resid,
  num_cuarto,
  excusado,
  tenencia,
FROM viviendas;
  "

variables_rezago = dbGetQuery(conexion, query)
dbDisconnect(conexion)


variables_rezago = variables_rezago %>% 
  mutate(
    pared_det = case_when(
      mat_pared %in% c(1, 2, 4, 5) ~ 1,
      mat_pared %in% c(3, 6, 7, 8) ~ 0,
      TRUE ~ NA_real_  # Si no cumple con las condiciones anteriores
    ),
    techo_det = case_when(
      mat_techos %in% c('01', '02', '06') ~ 1,
      mat_techos %in% c('03', '04', '05', '07', '08', '09', '10') ~ 0,
      TRUE ~ NA_real_
    ),
    pared_reg = case_when(
      mat_pared %in% c(3, 6) ~ 1,
      mat_pared %in% c(1, 2, 4, 5, 7, 8) ~ 0,
      TRUE ~ NA_real_
    ),
    techo_reg = case_when(
      mat_techos %in% c('03', '04', '07', '09') ~ 1,
      mat_techos %in% c('01', '02', '05', '06', '08', '10') ~ 0,
      TRUE ~ NA_real_
    ),
    pisos_reg = case_when(
      mat_pisos == 1 ~ 1,
      mat_pisos > 1 ~ 0,
      TRUE ~ NA_real_
    ),
    res_cuarto = tot_resid/num_cuarto
  ) %>% 
  mutate(
    mat_det = case_when(
      pared_det == 1 | techo_det == 1 ~ 1,  # Si alguna de las dos es 1
      pared_det == 0 & techo_det == 0 ~ 0,  # Si ambas son 0
      TRUE ~ NA_real_
    ),
    mat_reg = case_when(
      pared_reg == 1 | techo_reg == 1 | pisos_reg == 1 ~ 1,
      pared_reg == 0 & techo_reg == 0 & pisos_reg == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    hacin = case_when(
      res_cuarto > 2.5 ~ 1,
      res_cuarto <= 2.5 ~ 0,
      TRUE ~ NA_real_
    ),
    excu = if_else(
      excusado == 2, 1, 0
    ),
    prec_esp = case_when(
      hacin == 1 | excu == 1 ~ 1,
      hacin == 0 & excu == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    rezago = case_when(
      mat_det == 1 | mat_reg == 1 | prec_esp == 1 ~ 1,
      mat_det == 0 & mat_reg == 0 & prec_esp == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    estado_codigo = substr(folioviv, 1, 2),
    entidad = case_when(
      estado_codigo == '01' ~ 'Aguascalientes',
      estado_codigo == '02' ~ 'Baja California',
      estado_codigo == '03' ~ 'Baja California Sur',
      estado_codigo == '04' ~ 'Campeche',
      estado_codigo == '05' ~ 'Coahuila',
      estado_codigo == '06' ~ 'Colima',
      estado_codigo == '07' ~ 'Chiapas',
      estado_codigo == '08' ~ 'Chihuahua',
      estado_codigo == '09' ~ 'Ciudad de México',
      estado_codigo == '10' ~ 'Durango',
      estado_codigo == '11' ~ 'Guanajuato',
      estado_codigo == '12' ~ 'Guerrero',
      estado_codigo == '13' ~ 'Hidalgo',
      estado_codigo == '14' ~ 'Jalisco',
      estado_codigo == '15' ~ 'Estado de México',
      estado_codigo == '16' ~ 'Michoacán',
      estado_codigo == '17' ~ 'Morelos',
      estado_codigo == '18' ~ 'Nayarit',
      estado_codigo == '19' ~ 'Nuevo León',
      estado_codigo == '20' ~ 'Oaxaca',
      estado_codigo == '21' ~ 'Puebla',
      estado_codigo == '22' ~ 'Querétaro',
      estado_codigo == '23' ~ 'Quintana Roo',
      estado_codigo == '24' ~ 'San Luis Potosí',
      estado_codigo == '25' ~ 'Sinaloa',
      estado_codigo == '26' ~ 'Sonora',
      estado_codigo == '27' ~ 'Tabasco',
      estado_codigo == '28' ~ 'Tamaulipas',
      estado_codigo == '29' ~ 'Tlaxcala',
      estado_codigo == '30' ~ 'Veracruz',
      estado_codigo == '31' ~ 'Yucatán',
      estado_codigo == '32' ~ 'Zacatecas',
      TRUE ~ NA_character_  # Para códigos no listados
    ),
    sustitucion = if_else(pared_det == 1 | pared_reg == 1,1,0),
    mejora = if_else(sustitucion == 0 & (techo_det == 1 | techo_reg == 1 | pisos_reg == 1),1,0),
    ampliacion = if_else(sustitucion == 0 & mejora == 0 & prec_esp == 1,1,0),
    vivienda_no_propia = ifelse(tenencia == 1 & tenencia == 2 & tenencia == 6, 1, 0),
    vivienda_pagando = ifelse(tenencia == 3,1,0),
    vivienda_propia = ifelse(tenencia == 4,1,0)
  )

rezagos_entidad <- variables_rezago %>%
  group_by(entidad) %>%
  summarise(
    total_viviendas = sum(factor), # Total de viviendas expandidas
    sin_rezago = sum(factor[rezago == 0]), # Viviendas sin rezago (rezago == 0)
    con_rezago = sum(factor[rezago == 1]), # Viviendas con rezago (rezago == 1)
    porcentaje_rezago = (con_rezago / total_viviendas) * 100 # Porcentaje de viviendas en rezago
  ) %>%
  ungroup()


necesidades_entidad <- variables_rezago %>%
  group_by(entidad) %>%
  summarise(
    sustitucion = sum(factor[sustitucion == 1]), # Total de viviendas expandidas con necesidad de sustitución
    mejora = sum(factor[mejora == 1]), #Total de viviendas expandidas con necesidad de mejora
    ampliacion = sum(factor[ampliacion == 1]) # Total de viviendas expandidas con necesidad de ampliación
  ) %>%
  ungroup()

#######################################


#Vivienda adquirida por tipo de adquisición y estratos
#######################################
#establecer conexion a duckdb
conexion = dbConnect(
  drv = duckdb::duckdb(),
  dbdir = 'ENIGH.duckdb'
)
#extraer las variables necesarias de la tabla viviendas
query = "
SELECT
  folioviv,
  factor,
  tipo_adqui,
  est_socio
FROM viviendas;"
tipo_adquisicion = dbGetQuery(conexion, query)


dbDisconnect(conexion)


tipo_adquisicion = tipo_adquisicion %>%
  filter(!grepl(" ", tipo_adqui) & grepl("\\d", tipo_adqui)) %>%
  mutate(
    estado_codigo = substr(folioviv, 1, 2),
    entidad = case_when(
      estado_codigo == '01' ~ 'Aguascalientes',
      estado_codigo == '02' ~ 'Baja California',
      estado_codigo == '03' ~ 'Baja California Sur',
      estado_codigo == '04' ~ 'Campeche',
      estado_codigo == '05' ~ 'Coahuila',
      estado_codigo == '06' ~ 'Colima',
      estado_codigo == '07' ~ 'Chiapas',
      estado_codigo == '08' ~ 'Chihuahua',
      estado_codigo == '09' ~ 'Ciudad de México',
      estado_codigo == '10' ~ 'Durango',
      estado_codigo == '11' ~ 'Guanajuato',
      estado_codigo == '12' ~ 'Guerrero',
      estado_codigo == '13' ~ 'Hidalgo',
      estado_codigo == '14' ~ 'Jalisco',
      estado_codigo == '15' ~ 'Estado de México',
      estado_codigo == '16' ~ 'Michoacán',
      estado_codigo == '17' ~ 'Morelos',
      estado_codigo == '18' ~ 'Nayarit',
      estado_codigo == '19' ~ 'Nuevo León',
      estado_codigo == '20' ~ 'Oaxaca',
      estado_codigo == '21' ~ 'Puebla',
      estado_codigo == '22' ~ 'Querétaro',
      estado_codigo == '23' ~ 'Quintana Roo',
      estado_codigo == '24' ~ 'San Luis Potosí',
      estado_codigo == '25' ~ 'Sinaloa',
      estado_codigo == '26' ~ 'Sonora',
      estado_codigo == '27' ~ 'Tabasco',
      estado_codigo == '28' ~ 'Tamaulipas',
      estado_codigo == '29' ~ 'Tlaxcala',
      estado_codigo == '30' ~ 'Veracruz',
      estado_codigo == '31' ~ 'Yucatán',
      estado_codigo == '32' ~ 'Zacatecas',
      TRUE ~ NA_character_  # Para códigos no listados
    ),
    compro_hecha = if_else(
      tipo_adqui == 1, 1, 0
    ),
    mando_construir = if_else(
      tipo_adqui == 2, 1, 0
    ),
    auto_construida = if_else(
      tipo_adqui == 3, 1, 0
    ),
    obtenida_otro = if_else(
      tipo_adqui == 4, 1, 0 
    ),
    est_bajo = if_else(
      est_socio == 1,1,0
    ),
    est_mediobajo = if_else(
      est_socio == 2,1,0
    ),
    est_medioalto = if_else(
      est_socio == 3,1,0
    ),
    est_alto = if_else(
      est_socio == 4,1,0
    )
  )

  
adquisiciones_entidad <- tipo_adquisicion %>%
  group_by(entidad) %>%
  summarise(
    total_viviendas = sum(factor), # Total de viviendas expandidas
    compro_hecha = sum(factor[compro_hecha == 1]), # Viviendas compradas
    mando_construir = sum(factor[mando_construir == 1]), # Viviendas construidas
    auto_construida = sum(factor[auto_construida == 1]), # viviendas autoconstruidas
    otro = sum(factor[obtenida_otro == 1])
    ) %>%
  ungroup()

#adquisicion de vivienda nacional
adquisiciones_nacional = tipo_adquisicion %>%
  summarise(
    total_viviendas = sum(factor), # Total de viviendas expandidas
    compro_hecha = sum(factor[compro_hecha == 1]), # Viviendas compradas
    mando_construir = sum(factor[mando_construir == 1]), # Viviendas construidas
    auto_construida = sum(factor[auto_construida == 1]), # viviendas autoconstruidas
    otro = sum(factor[obtenida_otro == 1])
  ) %>%
  ungroup()



# Agrupar datos por estrato económico y tipo de adquisición
tipo_adquisicion_resumen <- tipo_adquisicion %>%
  group_by(estrato = case_when(
    est_bajo == 1 ~ "Bajo",
    est_mediobajo == 1 ~ "Medio Bajo",
    est_medioalto == 1 ~ "Medio Alto",
    est_alto == 1 ~ "Alto"
  )) %>%
  summarise(
    total_viviendas_estrato = sum(factor), # Total de viviendas por estrato
    compro_hecha = sum(factor[compro_hecha == 1]),
    mando_construir = sum(factor[mando_construir == 1]),
    auto_construida = sum(factor[auto_construida == 1]),
    obtenida_otro = sum(factor[obtenida_otro == 1])
  ) %>%
  pivot_longer(cols = compro_hecha:obtenida_otro, names_to = "tipo_adquisicion", values_to = "viviendas_tipo") %>%
  mutate(porcentaje = (viviendas_tipo / total_viviendas_estrato) * 100)  # Calcular porcentaje de cada tipo


# Asignar etiquetas más claras a los tipos de adquisición y ordenar los estratos
tipo_adquisicion_resumen <- tipo_adquisicion_resumen %>%
  mutate(
    tipo_adquisicion = factor(recode(tipo_adquisicion,
                                     compro_hecha = "Compra Hecha",
                                     mando_construir = "Mandó Construir",
                                     auto_construida = "Autoconstruida",
                                     obtenida_otro = "Otra forma"),
                              levels = c("Autoconstruida", "Mandó Construir", "Compra Hecha", "Otra forma")),  # Orden específico
    estrato = factor(estrato, levels = c("Bajo", "Medio Bajo", "Medio Alto", "Alto"))  # Orden de los estratos
  )

# Crear el gráfico de barras con porcentajes y mostrar los porcentajes dentro de las barras
grafico = ggplot(tipo_adquisicion_resumen, aes(x = estrato, y = porcentaje, fill = tipo_adquisicion)) +
  geom_bar(stat = "identity", position = "fill") +  # 'position = "fill"' para apilar al 100%
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)), 
            position = position_fill(vjust = 0.5), size = 7, color = "white") +  # Agregar porcentajes
  labs(title = "Tipos de Adquisición de Vivienda por Estrato Económico",
       x = "Estrato Económico", 
       y = "Porcentaje (%)",
       fill = "Tipo de Adquisición",
       caption = "Elaboración propia con datos de la ENIGH 2022") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Mostrar ejes en porcentaje
  scale_fill_manual(
    values = c("Autoconstruida" = "#E74C3C",     # Rojo para "Autoconstruida"
               "Mandó Construir" = "#95A5A6",    # Gris claro para "Mandó Construir"
               "Compra Hecha" = "#2C3E50",       # Gris oscuro para "Compra Hecha"
               "Otra forma" = "#34495E")         # Azul grisáceo para "Otra forma"
  )+
  theme_minimal() + 
  theme(
    text = element_text(size = 15),         
    axis.title = element_text(size = 19), 
    axis.text = element_text(size = 13), 
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 13), 
    plot.title = element_text(size = 23, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 1)
  )

# Guardar el gráfico con fondo transparente
ggsave("grafico_transparente.png", plot = grafico, bg = "transparent", width = 10, height = 7, dpi = 300)





#######################################

#tipo de financiamiento utilizado para la vivienda
#######################################
#extraer las variables
#establecer conexion a duckdb
conexion = dbConnect(
  drv = duckdb::duckdb(),
  dbdir = 'ENIGH.duckdb'
)
#extraer las variables necesarias de la tabla viviendas
query = "
SELECT
  folioviv,
  factor,
  tipo_finan
FROM viviendas;
  "

tipo_financiamiento = dbGetQuery(conexion, query)
dbDisconnect(conexion)


tipo_financiamiento = tipo_financiamiento %>%
  mutate(
    estado_codigo = substr(folioviv, 1, 2),
    entidad = case_when(
      estado_codigo == '01' ~ 'Aguascalientes',
      estado_codigo == '02' ~ 'Baja California',
      estado_codigo == '03' ~ 'Baja California Sur',
      estado_codigo == '04' ~ 'Campeche',
      estado_codigo == '05' ~ 'Coahuila',
      estado_codigo == '06' ~ 'Colima',
      estado_codigo == '07' ~ 'Chiapas',
      estado_codigo == '08' ~ 'Chihuahua',
      estado_codigo == '09' ~ 'Ciudad de México',
      estado_codigo == '10' ~ 'Durango',
      estado_codigo == '11' ~ 'Guanajuato',
      estado_codigo == '12' ~ 'Guerrero',
      estado_codigo == '13' ~ 'Hidalgo',
      estado_codigo == '14' ~ 'Jalisco',
      estado_codigo == '15' ~ 'Estado de México',
      estado_codigo == '16' ~ 'Michoacán',
      estado_codigo == '17' ~ 'Morelos',
      estado_codigo == '18' ~ 'Nayarit',
      estado_codigo == '19' ~ 'Nuevo León',
      estado_codigo == '20' ~ 'Oaxaca',
      estado_codigo == '21' ~ 'Puebla',
      estado_codigo == '22' ~ 'Querétaro',
      estado_codigo == '23' ~ 'Quintana Roo',
      estado_codigo == '24' ~ 'San Luis Potosí',
      estado_codigo == '25' ~ 'Sinaloa',
      estado_codigo == '26' ~ 'Sonora',
      estado_codigo == '27' ~ 'Tabasco',
      estado_codigo == '28' ~ 'Tamaulipas',
      estado_codigo == '29' ~ 'Tlaxcala',
      estado_codigo == '30' ~ 'Veracruz',
      estado_codigo == '31' ~ 'Yucatán',
      estado_codigo == '32' ~ 'Zacatecas',
      TRUE ~ NA_character_  # Para códigos no listados
    ),
    infonavit = if_else( #para creditos INFONAVIT, FOVISSSTE & FONHAPO
      tipo_finan == 1, 1, 0
    ),
    banco = if_else( #financiado por bancos, SOFOL o caja de ahorro
      tipo_finan == 2,1,0
    ),
    otra_institucion = if_else(
      tipo_finan == 3,1,0
    ),
    amigo_familiar = if_else(
      tipo_finan == 4,1,0 
    ),
    propios_recursos = if_else(
      tipo_finan == 5,1,0
    )
  )


financiamiento_entidad <- tipo_financiamiento %>%
  #group_by(entidad) %>%
  summarise(
    total_viviendas = sum(factor), # Total de viviendas expandidas
    infonavit = sum(factor[infonavit == 1]),
    banco = sum(factor[banco == 1]), 
    otra_institucion = sum(factor[otra_institucion == 1]), 
    amigo_familiar = sum(factor[amigo_familiar == 1]),
    propios_recursos = sum(factor[propios_recursos == 1])
    ) %>%
  ungroup()

# Reestructurar el dataframe y calcular los porcentajes
financiamiento_entidad_long <- financiamiento_entidad %>%
  pivot_longer(cols = c(infonavit, banco, otra_institucion, amigo_familiar, propios_recursos), 
               names_to = "tipo", 
               values_to = "total") %>%
  mutate(porcentaje = (total / total_viviendas) * 100) %>%
  arrange(desc(porcentaje))

# Crear el histograma con los porcentajes
ggplot(financiamiento_entidad_long, aes(x = reorder(tipo, porcentaje), y = porcentaje)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)), vjust = -0.5, size = 3.5) +
  coord_flip() +  # Para un gráfico horizontal
  labs(x = "Tipo de Financiamiento", y = "Porcentaje de Viviendas", 
       title = "Participación por Tipo de Financiamiento (en porcentaje)") +
  theme_minimal()


#######################################


#financiameinto utilizado para la vivienda adquirirda y autoproducida
#######################################
#extraer las variables
#establecer conexion a duckdb
conexion = dbConnect(
  drv = duckdb::duckdb(),
  dbdir = 'ENIGH.duckdb'
)
#extraer las variables necesarias de la tabla viviendas
query = "
SELECT
  folioviv,
  factor,
  tipo_finan,
  tipo_adqui
FROM viviendas;
  "

tipo_finan_adqui = dbGetQuery(conexion, query)
dbDisconnect(conexion)

tipo_finan_adqui = tipo_finan_adqui %>%
  mutate(
  viv_adquirida = if_else(
    tipo_adqui == 1,1,0
  ),
  viv_autoproducida = if_else(
    tipo_adqui %in% c(2,3), 1, 0
  ),
  adquirida_otro = if_else(
    tipo_adqui == 4, 1, 0
  ),
  infonavit = if_else( #para creditos INFONAVIT, FOVISSSTE & FONHAPO
    tipo_finan == 1, 1, 0
  ),
  banco = if_else( #financiado por bancos, SOFOL o caja de ahorro
    tipo_finan == 2,1,0
  ),
  otra_institucion = if_else(
    tipo_finan == 3,1,0
  ),
  amigo_familiar = if_else(
    tipo_finan == 4,1,0 
  ),
  propios_recursos = if_else(
    tipo_finan == 5,1,0
  )
)

# Para viviendas adquiridas
adquirida_financiamiento <- tipo_finan_adqui %>%
  filter(viv_adquirida == 1) %>%
  summarise(
    total_viviendas = sum(factor),
    infonavit = sum(factor[infonavit == 1]),
    banco = sum(factor[banco == 1]),
    otra_institucion = sum(factor[otra_institucion == 1]),
    amigo_familiar = sum(factor[amigo_familiar == 1]),
    propios_recursos = sum(factor[propios_recursos == 1])
  ) %>%
  pivot_longer(cols = infonavit:propios_recursos, names_to = "tipo", values_to = "total") %>%
  mutate(porcentaje = (total / total_viviendas) * 100,
         tipo_vivienda = "Adquirida")

# Para viviendas autoproducidas
autoproducida_financiamiento <- tipo_finan_adqui %>%
  filter(viv_autoproducida == 1) %>%
  summarise(
    total_viviendas = sum(factor),
    infonavit = sum(factor[infonavit == 1]),
    banco = sum(factor[banco == 1]),
    otra_institucion = sum(factor[otra_institucion == 1]),
    amigo_familiar = sum(factor[amigo_familiar == 1]),
    propios_recursos = sum(factor[propios_recursos == 1])
  ) %>%
  pivot_longer(cols = infonavit:propios_recursos, names_to = "tipo", values_to = "total") %>%
  mutate(porcentaje = (total / total_viviendas) * 100,
         tipo_vivienda = "Autoproducida")


# Unir ambos dataframes
financiamiento_combined <- bind_rows(adquirida_financiamiento, autoproducida_financiamiento)

# Renombrar los tipos de financiamiento
financiamiento_combined %>%
  mutate(tipo = recode(tipo,
                       infonavit = "Financiamiento Social",
                       banco = "Instituciones de crédito",
                       otra_institucion = "Otra Institución",
                       amigo_familiar = "Préstamo Informal",
                       propios_recursos = "Recursos Propios"))%>%
  # Crear el gráfico con dos colores diferentes y aumentar el tamaño del texto
  ggplot(aes(x = reorder(tipo, porcentaje), y = porcentaje, fill = tipo_vivienda)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 7) +  # Aumentar el tamaño del texto de las etiquetas
  scale_fill_manual(values = c("Adquirida" = "#FF000080", "Autoproducida" = "#FF0000C0")) +  # Colores rojo 50% y rojo 75%
  coord_flip() +
  labs(x = "Tipo de Financiamiento", y = "Porcentaje de Viviendas", 
       title = "Tipos de Financiamiento para Viviendas Adquiridas y Autoproducidas",
       fill = "Tipo de Vivienda",
       caption = "Elaboración propia con datos de la ENIGH 2022"  # Añadir pie de imagen
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 15),         
    axis.title = element_text(size = 19), 
    axis.text = element_text(size = 13), 
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 13), 
    plot.title = element_text(size = 23, hjust = 0.5),
    plot.caption = element_text(size = 11, hjust = 1)
  )

#######################################


#ingresos de hogares para comparacion múltiple por quintiles
#######################################
#establecer conexion a duckdb
conexion = dbConnect(
  drv = duckdb::duckdb(),
  dbdir = 'ENIGH.duckdb'
)

dbListTables(conexion)

query = "
SELECT 
  ing_cor,
  foliohog,
  folioviv,
  factor
FROM concentradohogar;"

ingresos = dbGetQuery(conexion, query)
dbDisconnect(conexion)

ingresos <- ingresos %>%
  group_by(folioviv,foliohog) %>%
  mutate(ing_cor_hogar = sum(ing_cor),
         ing_mensual_hogar = ing_cor_hogar/3)
#aqui ya tenemos ingresos mensuales por hogar
#llave primaria, folioviv, foliohog, num_ren
ingresos_mensuales_hogar <- ingresos %>%
  group_by(folioviv, foliohog) %>%
  summarise(
    ing_mensual_hogar = sum(ing_cor) / 3,  # Sumar los ingresos trimestrales por hogar y dividir entre 3
    factor = first(factor)  # Mantener la variable 'factor' a nivel de concentradohogar
  ) %>%
  ungroup()

#que condiciones tienen las viviendas de los hogares por quintiles?
#condiciones en funcion del ingreso

#Aqui es necesario correr el codigo por tipo adquisicion de viviendas ANTES!!!!!!!
ingresos_mensuales_hogar = ingresos_mensuales_hogar %>%
  mutate(quintil = factor(ntile(ing_mensual_hogar, 5), 
                          labels = c("Quintil I", "Quintil II", "Quintil III", "Quintil IV", "Quintil V"))) %>%
  left_join(tipo_adquisicion, by = c("folioviv","factor")) %>%
  filter(!is.na(tipo_adqui))


# Agrupar directamente por la columna quintil ya etiquetada
tipo_adquisicion_resumen <- ingresos_mensuales_hogar %>%
  group_by(quintil) %>%
  summarise(
    total_viviendas_quintil = sum(factor, na.rm = TRUE),  # Total de viviendas por quintil
    compro_hecha = sum(factor[compro_hecha == 1], na.rm = TRUE),
    mando_construir = sum(factor[mando_construir == 1], na.rm = TRUE),
    auto_construida = sum(factor[auto_construida == 1], na.rm = TRUE),
    obtenida_otro = sum(factor[obtenida_otro == 1], na.rm = TRUE)
  ) %>%
  pivot_longer(cols = compro_hecha:obtenida_otro, names_to = "tipo_adquisicion", values_to = "viviendas_tipo") %>%
  mutate(porcentaje = (viviendas_tipo / total_viviendas_quintil) * 100)  # Calcular porcentaje de cada tipo

# Asignar etiquetas claras y asegurarse de que los niveles estén en el orden correcto
tipo_adquisicion_resumen <- tipo_adquisicion_resumen %>%
  mutate(
    tipo_adquisicion = factor(recode(tipo_adquisicion,
                                     compro_hecha = "Compra Hecha",
                                     mando_construir = "Mandó Construir",
                                     auto_construida = "Autoconstruida",
                                     obtenida_otro = "Otra forma"),
                              levels = c("Autoconstruida", "Mandó Construir", "Compra Hecha", "Otra forma"))
  )

# Crear el gráfico de barras con porcentajes y mostrar los porcentajes dentro de las barras
grafico_1 <- ggplot(tipo_adquisicion_resumen, aes(x = quintil, y = porcentaje, fill = tipo_adquisicion)) +
  geom_bar(stat = "identity", position = "fill") +  # 'position = "fill"' para apilar al 100%
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)), 
            position = position_fill(vjust = 0.5), size = 7, color = "white") +  # Agregar porcentajes
  labs(
    title = "Adquisición de Vivienda por Quintil Socioeconómico de hogares",
    x = "", 
    y = "Porcentaje (%)",
    fill = "Tipo de Adquisición",
    caption = "Elaboración propia con datos de la ENIGH 2022"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Mostrar ejes en porcentaje
  scale_fill_manual(
    values = c("Autoconstruida" = "#e74c3c",     # Rojo para "Autoconstruida"
               "Mandó Construir" = "#c0392b",    # Gris claro para "Mandó Construir"
               "Compra Hecha" = "#95a5a6",       # Gris oscuro para "Compra Hecha"
               "Otra forma" = "#34495E")         # Azul grisáceo para "Otra forma"
  ) +
  theme_minimal() + 
  theme(
    plot.margin = margin(20, 20, 20, 40),  # Aumentar margen izquierdo
    text = element_text(size = 15),         
    axis.title = element_text(size = 19), 
    axis.text = element_text(size = 13), 
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 13), 
    plot.title = element_text(size = 23, hjust = 0.5),  # Mantener centrado el título
    plot.caption = element_text(size = 11, hjust = 1)
  )

# Guardar el gráfico con fondo transparente
ggsave("grafico1_transparente.png", plot = grafico_1, bg = "transparent", width = 10, height = 7, dpi = 300)


#imputar a estos hogares las características de vivienda
#hacinamiento, rezago habitacional, necesidades de remplazo, ampliacion y mejoramiento

#después, usar la base de poblacion y/o ingresos y pegarla para armar grupos de posibles demandantes
#de créditos hipotecarios INFONAVIT abajo


#######################################


#Tipo de crédito INFONAVIT de acuerdo a características de vivienda 
#(demanda potencial de créditos para los hogares hogares)
#######################################
#características de la poblacion dentro de los hogares y viviendas
#SE DEBE de correr primero variables de rezago y ampliacion para pegar por viviendas de los individuos

#establecer conexion a duckdb
conexion = dbConnect(
  drv = duckdb::duckdb(),
  dbdir = 'ENIGH.duckdb'
)

query = "
SELECT 
  folioviv,
  foliohog,
  numren,
  factor,
  sexo,
  edad,
  disc_camin,
  disc_ver,
  disc_brazo,
  disc_oir,
  disc_acti,
  pareja_hog,
  atemed,
  inscr_1,
  entidad
FROM poblacion;"
poblacion = dbGetQuery(conexion, query)

query = "SELECT
numren,
ing_tri
FROM ingresos;"
ingresos = dbGetQuery(conexion,query)

dbDisconnect(conexion)

#quedandonos con la poblacion formal (posibles demandantes)
poblacion_formal <- poblacion %>%
  left_join(ingresos %>% distinct(numren, .keep_all = TRUE), by = "numren") %>%
  mutate(atemed = ifelse(atemed == 2, 0, atemed)) %>%
  filter(atemed == 1 & inscr_1 == 1 & edad >= 16 & edad < 73) %>%
  mutate(ing_mens = ing_tri / 3) %>%
  select(-ing_tri) %>%
  left_join(variables_rezago %>% distinct(folioviv, .keep_all = TRUE), by = "folioviv") %>%
  mutate(factor = coalesce(factor.x, factor.y), # Fusionar factor.x y factor.y
         quintil = ntile(ing_mens, 5)  # Crear la nueva variable de quintil basada en ing_mens
  ) %>%  
  select(-factor.x, -factor.y) %>%  # Eliminar las columnas originales
  group_by(numren)

#aligerando la memoria tirando datafrmaes ya integrados
rm(ingresos,poblacion,variables_rezago)


sum(poblacion_formal$factor)
#POBLACION FORMAL (formal mayores de 18 año y menores a 75 en 2024) = 22,991,477 PERSONAS
#SUPONIENDO que sus condiciones de vivienda y personales numren se mantuvieron constantes desde
#constantes desde 2022 hasta 2024


#falta un nuevo mutate para generar 1 y 0 en variables nuevas de tipos de crédito
#creando variable de tiene roomies (para cofinavit)
poblacion_formal = poblacion_formal %>%
  group_by(folioviv) %>%  # Agrupar por vivienda
  mutate(tiene_roomie = ifelse(n() > 1, 1, 0)) %>%  # Si más de una persona vive en la misma vivienda
  ungroup() %>%
  mutate(pareja_hog = ifelse(pareja_hog == " ", 0, pareja_hog))
         "discapacidad = ifelse(disc_camin == 1 |
                                 disc_ver == 1 |
                                 disc_brazo == 1 |
                                 disc_oir == 1 |
                                 disc_acti == 1,1,0))%>%
  select(-disc_camin,-disc_ver,disc_brazo,disc_oir,disc_acti) 
)" #antes de asignar discapacidad falta ver el grado de la discapacidad

poblacion_formal_creditos = poblacion_formal %>%
  mutate(
    #selección de candidatos 
    #CREDITOS PARA ADQUIRIR O AUTOPRODUCIR VIVIENDA (incluyen los que necesitan sustitución)
    credito_infonavit = ifelse(vivienda_no_propia == 1 | 
                                 vivienda_pagando == 1 | #para pago de creditos
                                 sustitucion == 1,1,0 
                                ),
    join_credito_infonavit = ifelse(credito_infonavit == 1 & tiene_roomie ==1,1,0),
    apoyo_infonavit = ifelse(vivienda_no_propia == 1 | 
                               sustitucion == 1,1,0),
    cofinavit = ifelse(vivienda_no_propia == 1 | 
                         vivienda_pagando == 1 |
                         sustitucion== 1 &
                         pareja_hog == 1,1,0),
    cofinavit_ing_adicional = ifelse(vivienda_no_propia == 1 |
                                       sustitucion == 1 &
                                       pareja_hog == 1 &
                                       ing_mens <= 12872.06,1,0),
    infonavit_total = ifelse(vivienda_no_propia == 1 |
                               sustitucion == 1 & 
                               ing_mens >= 12872.06,1,0),
    
    #CREDITOS DE AMPLIACION (afecta estructura de la vivienda existente) 
    construyo_infonavit = ifelse(vivienda_propia == 1 &
                                   ampliacion == 1,1,0), 
    
    #MEJORAMIENTO
    #construyo infonavit aplica 
    mejoravit_renueva = ifelse(vivienda_propia == 1 & 
                                 mejora == 1,1,0),
    mejoravit_repara = ifelse(vivienda_propia == 1 &
                                mejora == 1,1,0),
    equipa_tu_casa = ifelse(credito_infonavit == 1 | #aumenta el monto de otro crédito 
                              apoyo_infonavit == 1 |
                              cofinavit == 1 |
                              cofinavit_ing_adicional == 1 |
                              infonavit_total == 1 |
                              construyo_infonavit == 1,1,0)
    
    #PAGAR DEUDA
    #aplica credito_infonavit de arriba
    
  )

#gráfico de posibles demandantes por tipo de crédito
library(ggtext)

grafico_1 <- poblacion_formal_creditos %>%
  gather(key = "tipo_credito", value = "aplica", credito_infonavit, join_credito_infonavit, apoyo_infonavit, cofinavit, 
         cofinavit_ing_adicional, infonavit_total, construyo_infonavit, mejoravit_renueva, mejoravit_repara, equipa_tu_casa) %>%
  filter(aplica == 1) %>%
  group_by(tipo_credito) %>%
  summarise(
    personas_total = sum(factor),
    personas_jovenes = sum(factor[edad <= 29])  # Definir jóvenes como <= 29 años
  ) %>%
  mutate(
    porcentaje_escala_total = (personas_jovenes / max(personas_total)) * 100,  # Escala ajustada al máximo total
    tipo_credito = recode(tipo_credito,
                          credito_infonavit = "Crédito INFONAVIT",
                          join_credito_infonavit = "Crédito INFONAVIT conjunto",
                          apoyo_infonavit = "Apoyo INFONAVIT",
                          cofinavit = "Cofinavit",
                          cofinavit_ing_adicional = "Cofinavit ingresos adicionales",
                          infonavit_total = "INFONAVIT total",
                          construyo_infonavit = "construYO",
                          mejoravit_renueva = "mejoravit RENUEVA",
                          mejoravit_repara = "mejoravit REPARA",
                          equipa_tu_casa = "Equipa tu Casa")
  ) %>%
  ggplot(aes(y = fct_reorder(tipo_credito, personas_total), x = porcentaje_escala_total)) +
  # Barra para el porcentaje de jóvenes escalado sobre el total
  geom_bar(stat = "identity", fill = "#E74C3C", color = NA, width = 0.8) +
  # Línea para el total de personas
  geom_segment(aes(x = 0, xend = personas_total / max(personas_total) * 100, y = tipo_credito, yend = tipo_credito),
               linewidth = 0.6, color = "#BDC3C7", linetype = "dashed") +
  # Etiquetas para el porcentaje de jóvenes
  geom_text(
    aes(label = paste0(round((personas_jovenes / personas_total) * 100, 1), "%")),
    hjust = -0.2, size = 3.5, color = "#2C3E50"
  ) +
  # Etiquetas para el total de personas ajustadas a los márgenes
  geom_text(
    aes(x = personas_total / max(personas_total) * 100 + 5, label = scales::comma(personas_total)),
    hjust = 0, size = 3.5, color = "#95A5A6"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Porcentaje de jóvenes y demanda potencial\npor tipo de crédito",  # Salto de línea con \n
    caption = "Fuente: Elaboración propia con datos de la ENIGH 2022."
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 10)) +  # Ajustar escala para evitar cortes
  theme_minimal(base_size = 14) +
  theme(
    plot.margin = margin(30, 30, 30, 30),  # Márgenes amplios para evitar cortes
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "#2C3E50", size = 12),
    axis.text.y = element_text(color = "#2C3E50", size = 12),
    axis.title.x = element_text(size = 12, color = "#2C3E50"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Ajustar el tamaño y alineación del título
    legend.position = "none",  # Eliminar la leyenda
    plot.caption = element_text(hjust = 1, color = "#7F8C8D", size = 10)
  )

# Guardar el gráfico con fondo transparente
ggsave("grafico1_transparente.png", plot = grafico_1, bg = "transparent", width = 10, height = 8, dpi = 300)



#######################################


#Mapa de clusters de densidad de demanda de vivienda
#######################################
#establecer conexion a duckdb
conexion = dbConnect(
  drv = duckdb::duckdb(),
  dbdir = 'ENIGH.duckdb'
)

#extraer las variables necesarias de la tabla viviendas
query = "
SELECT
  folioviv,
  factor,
  mat_pared,
  mat_techos,
  mat_pisos,
  tot_resid,
  num_cuarto,
  excusado,
  tenencia,
FROM viviendas;
  "
variables_rezago = dbGetQuery(conexion, query)

query = "
SELECT 
  folioviv,
  foliohog,
  numren,
  factor,
  sexo,
  edad,
  disc_camin,
  disc_ver,
  disc_brazo,
  disc_oir,
  disc_acti,
  pareja_hog,
  atemed,
  inscr_1,
FROM poblacion;"
poblacion = dbGetQuery(conexion, query)

query = "SELECT
numren,
foliohog,
folioviv,
ing_tri
FROM ingresos;"
ingresos = dbGetQuery(conexion,query)

dbDisconnect(conexion)



variables_rezago = variables_rezago %>% 
  mutate(
    pared_det = case_when(
      mat_pared %in% c(1, 2, 4, 5) ~ 1,
      mat_pared %in% c(3, 6, 7, 8) ~ 0,
      TRUE ~ NA_real_  # Si no cumple con las condiciones anteriores
    ),
    techo_det = case_when(
      mat_techos %in% c('01', '02', '06') ~ 1,
      mat_techos %in% c('03', '04', '05', '07', '08', '09', '10') ~ 0,
      TRUE ~ NA_real_
    ),
    pared_reg = case_when(
      mat_pared %in% c(3, 6) ~ 1,
      mat_pared %in% c(1, 2, 4, 5, 7, 8) ~ 0,
      TRUE ~ NA_real_
    ),
    techo_reg = case_when(
      mat_techos %in% c('03', '04', '07', '09') ~ 1,
      mat_techos %in% c('01', '02', '05', '06', '08', '10') ~ 0,
      TRUE ~ NA_real_
    ),
    pisos_reg = case_when(
      mat_pisos == 1 ~ 1,
      mat_pisos > 1 ~ 0,
      TRUE ~ NA_real_
    ),
    res_cuarto = tot_resid/num_cuarto
  ) %>% 
  mutate(
    mat_det = case_when(
      pared_det == 1 | techo_det == 1 ~ 1,  # Si alguna de las dos es 1
      pared_det == 0 & techo_det == 0 ~ 0,  # Si ambas son 0
      TRUE ~ NA_real_
    ),
    mat_reg = case_when(
      pared_reg == 1 | techo_reg == 1 | pisos_reg == 1 ~ 1,
      pared_reg == 0 & techo_reg == 0 & pisos_reg == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    hacin = case_when(
      res_cuarto > 2.5 ~ 1,
      res_cuarto <= 2.5 ~ 0,
      TRUE ~ NA_real_
    ),
    excu = if_else(
      excusado == 2, 1, 0
    ),
    prec_esp = case_when(
      hacin == 1 | excu == 1 ~ 1,
      hacin == 0 & excu == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    rezago = case_when(
      mat_det == 1 | mat_reg == 1 | prec_esp == 1 ~ 1,
      mat_det == 0 & mat_reg == 0 & prec_esp == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    sustitucion = if_else(pared_det == 1 | pared_reg == 1,1,0),
    mejora = if_else(sustitucion == 0 & (techo_det == 1 | techo_reg == 1 | pisos_reg == 1),1,0),
    ampliacion = if_else(sustitucion == 0 & mejora == 0 & prec_esp == 1,1,0),
    vivienda_no_propia = ifelse(tenencia == 1 & tenencia == 2 & tenencia == 6, 1, 0),
    vivienda_pagando = ifelse(tenencia == 3,1,0),
    vivienda_propia = ifelse(tenencia == 4,1,0),
    estado_codigo = substr(folioviv, 1, 2),
    entidad = case_when(
      estado_codigo == '01' ~ 'Aguascalientes',
      estado_codigo == '02' ~ 'Baja California',
      estado_codigo == '03' ~ 'Baja California Sur',
      estado_codigo == '04' ~ 'Campeche',
      estado_codigo == '05' ~ 'Coahuila',
      estado_codigo == '06' ~ 'Colima',
      estado_codigo == '07' ~ 'Chiapas',
      estado_codigo == '08' ~ 'Chihuahua',
      estado_codigo == '09' ~ 'Ciudad de México',
      estado_codigo == '10' ~ 'Durango',
      estado_codigo == '11' ~ 'Guanajuato',
      estado_codigo == '12' ~ 'Guerrero',
      estado_codigo == '13' ~ 'Hidalgo',
      estado_codigo == '14' ~ 'Jalisco',
      estado_codigo == '15' ~ 'Estado de México',
      estado_codigo == '16' ~ 'Michoacán',
      estado_codigo == '17' ~ 'Morelos',
      estado_codigo == '18' ~ 'Nayarit',
      estado_codigo == '19' ~ 'Nuevo León',
      estado_codigo == '20' ~ 'Oaxaca',
      estado_codigo == '21' ~ 'Puebla',
      estado_codigo == '22' ~ 'Querétaro',
      estado_codigo == '23' ~ 'Quintana Roo',
      estado_codigo == '24' ~ 'San Luis Potosí',
      estado_codigo == '25' ~ 'Sinaloa',
      estado_codigo == '26' ~ 'Sonora',
      estado_codigo == '27' ~ 'Tabasco',
      estado_codigo == '28' ~ 'Tamaulipas',
      estado_codigo == '29' ~ 'Tlaxcala',
      estado_codigo == '30' ~ 'Veracruz',
      estado_codigo == '31' ~ 'Yucatán',
      estado_codigo == '32' ~ 'Zacatecas',
      TRUE ~ NA_character_  # Para códigos no listados
    )
  )
  

poblacion_demanda = poblacion %>% 
  left_join(variables_rezago, by = 'folioviv') %>% 
  left_join(ingresos, by = c('numren', 'foliohog', 'folioviv'))%>%
  mutate(factor = coalesce(factor.x, factor.y)) %>%  # Combina factor.x y factor.y en una sola columna llamada 'factor'
  #select(-factor.x, -factor.y, entidad.x, entidad.y) %>% 
  mutate(demanda = ifelse(vivienda_no_propia == 1 |
                            sustitucion == 1,1,0)) %>% 
  filter(demanda == 1) %>% 
  group_by(entidad) %>% 
  summarise(total_personas = sum(factor, na.rm = TRUE))





#generando un mapa con las personas demandantes de vivienda
# Cargar las librerías
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)

# Descargar el shapefile de México a nivel de entidades
mexico <- ne_states(country = "Mexico", returnclass = "sf")


# Asegúrate que los nombres de las entidades coincidan
# Unir el shapefile con los datos de población (ajusta el nombre de la columna si es necesario)
mapa_datos <- mexico %>%
  # Cambiar "Distrito Federal" a "Ciudad de México" en el shapefile de México 
  mutate(name = ifelse(name == "Distrito Federal", "Ciudad de México", name),
         name = ifelse(name == "México","Estado de México",name)) %>% 
  left_join(poblacion_demanda, by = c("name" = "entidad"))

# Crear el mapa de calor
ggplot(data = mapa_datos) +
  geom_sf(aes(fill = total_personas), color = "white") +  # Usar 'total_personas' para el color
  scale_fill_viridis_c(option = "plasma", na.value = "grey") +  # Escala de colores con NA en gris
  theme_minimal() +
  labs(title = "Mapa de Calor por Entidades en México",
       fill = "Total de personas") +  # Etiqueta de la leyenda
  theme(
    legend.position = "right",  # Barra de colores a la derecha
    legend.title = element_text(angle = 0),  # Título horizontal
    legend.text = element_text(angle = 0),   # Números horizontales
    legend.title.align = 0.5  # Centrar el título en la barra
  ) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 20))  # Ajuste de proporciones de la barra




# Ordenar los datos de poblacion_demanda de mayor a menor
poblacion_demanda <- poblacion_demanda %>%
  arrange(desc(total_personas))

# Crear el gráfico de barras para 
ggplot(poblacion_demanda, aes(x = reorder(entidad, -total_personas), y = total_personas)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Cantidad de personas por entidad",
       x = "Entidad",
       y = "Total de personas") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# dividir por clusters
# Definir el número de clusters (3 en este caso: alta, media, baja)
set.seed(123)  # Fijar semilla para reproducibilidad

# Asignar clusters en poblacion_demanda primero
clusters <- kmeans(poblacion_demanda$total_personas, centers = 3)

# Agregar la clasificación de clusters al dataframe
poblacion_demanda <- poblacion_demanda %>%
  mutate(demanda_cluster = factor(clusters$cluster, 
                                  labels = c("Baja Demanda", "Media Demanda", "Alta Demanda")))

# Cambiar "Distrito Federal" a "Ciudad de México" en el shapefile de México
mexico <- mexico %>%
  mutate(name = ifelse(name == "Distrito Federal", "Ciudad de México", name),
         name = ifelse(name == "México", "Estado de México", name))

# Unir el shapefile con los datos de población
mapa_datos <- mexico %>%
  left_join(poblacion_demanda, by = c("name" = "entidad"))

# Filtrar filas sin cluster asignado
mapa_datos <- mapa_datos %>%
  filter(!is.na(demanda_cluster))


# Crear el mapa coloreado por clusters de demanda
ggplot(data = mapa_datos) +
  geom_sf(aes(fill = demanda_cluster), color = "white") +  # Colorear por clusters
  scale_fill_manual(
    values = c("Baja Demanda" = "#FFEDA0",   # Amarillo vibrante
               "Media Demanda" = "#FEB24C",  # Naranja vibrante
               "Alta Demanda" = "#F03B20"),  # Rojo vibrante
    drop = FALSE  # Evitar mostrar NA en la leyenda si no hay
  ) +
  theme_minimal() +
  labs(
    title = "Clusteres de demanda potencial de vivienda",  # Nuevo título
    fill = "Cluster",  # Etiqueta de la leyenda
    caption = "Elaboración propia con datos de la ENIGH"  # Pie de página
  ) +
  theme(
    plot.title = element_text(size = 25,hjust = 0.5),  # Aumentar el tamaño del título
    legend.title = element_text(size = 18),  # Aumentar el tamaño del título de la leyenda
    legend.text = element_text(size = 16),  # Aumentar el tamaño de las etiquetas de la leyenda
    axis.text = element_text(size = 16),  # Aumentar el tamaño del texto de los ejes
    plot.caption = element_text(size = 16,hjust = 1),  # Pie de página con el mismo tamaño que las etiquetas
    legend.position = "right"  # Mantener la leyenda a la derecha
  )



# Ver los nombres de las entidades en el shapefile
unique(sort(mexico$name))

# Ver los nombres de las entidades en el dataframe de población
unique(sort(poblacion_demanda$entidad))

#######################################


#problema de clasificación (independencia no independencia, renta o compra)
#######################################

#establecer conexion a duckdb
conexion = dbConnect(
  drv = duckdb::duckdb(),
  dbdir = 'ENIGH.duckdb'
)

#extraer las variables necesarias
query = "
SELECT
  folioviv,
  tenencia,
  num_dueno1,
  num_dueno2,
  hog_dueno1,
  hog_dueno2
FROM viviendas;
  "
viviendas = dbGetQuery(conexion, query) %>%
  mutate(hog_dueno1 = as.numeric(hog_dueno1),hog_dueno2 = as.numeric(hog_dueno2))

query = "
SELECT 
  folioviv,
  foliohog,
  numren,
  factor AS factor_poblacion,
  sexo,
  edad,
  pareja_hog,
  atemed,
  inscr_1,
  hablaind,
  nivelaprob,
  parentesco
FROM poblacion;"
poblacion = dbGetQuery(conexion, query) %>%
  left_join(
    y = select(viviendas, folioviv, tenencia),
    by = 'folioviv'
  ) %>%
  left_join(
    y = select(viviendas, folioviv, num_dueno1, hog_dueno1) %>%
      mutate(dueno = ifelse(hog_dueno1 == ' ',0,1)),
    by = c('folioviv','foliohog'='hog_dueno1','numren'='num_dueno1')
  ) %>%
  left_join(
    y = select(viviendas, folioviv, num_dueno2, hog_dueno2) %>%
      mutate(dueno2 = ifelse(hog_dueno2 == ' ',0,1)),
    by = c('folioviv','foliohog'='hog_dueno2','numren'='num_dueno2')
  ) %>%
  filter(tenencia %in% c(1,3), edad > 17, edad < 36) %>%
  mutate(
    dueno = ifelse(is.na(dueno), dueno2, dueno), 
    dueno = coalesce(dueno, 0), #Adquiere vivienda cuando dueno = 1, renta = 0
    nivelaprob = as.factor(nivelaprob),  #nivel de escolaridad
    independencia = ifelse(parentesco %in% c("101","201","202","205","501"),1,0)
  ) 


query = "SELECT
numren,
foliohog,
folioviv,
ing_tri
FROM ingresos;"
ingresos = dbGetQuery(conexion,query)


query = "SELECT 
folioviv,
foliohog,
vivienda,
tot_integ
FROM concentradohogar
;"
concentradohogar = dbGetQuery(conexion,query)



# Calcular gasto per capita por hogar (concentrado hogar)
# Costo de capital = gasto percapita / ingreso personal [para iningreso>0]



dbDisconnect(conexion)



variables_demanda = viviendas %>% 
  mutate(
    estado_codigo = substr(folioviv, 1, 2),
    entidad = case_when(
      estado_codigo == '01' ~ 'Aguascalientes',
      estado_codigo == '02' ~ 'Baja California',
      estado_codigo == '03' ~ 'Baja California Sur',
      estado_codigo == '04' ~ 'Campeche',
      estado_codigo == '05' ~ 'Coahuila',
      estado_codigo == '06' ~ 'Colima',
      estado_codigo == '07' ~ 'Chiapas',
      estado_codigo == '08' ~ 'Chihuahua',
      estado_codigo == '09' ~ 'Ciudad de México',
      estado_codigo == '10' ~ 'Durango',
      estado_codigo == '11' ~ 'Guanajuato',
      estado_codigo == '12' ~ 'Guerrero',
      estado_codigo == '13' ~ 'Hidalgo',
      estado_codigo == '14' ~ 'Jalisco',
      estado_codigo == '15' ~ 'Estado de México',
      estado_codigo == '16' ~ 'Michoacán',
      estado_codigo == '17' ~ 'Morelos',
      estado_codigo == '18' ~ 'Nayarit',
      estado_codigo == '19' ~ 'Nuevo León',
      estado_codigo == '20' ~ 'Oaxaca',
      estado_codigo == '21' ~ 'Puebla',
      estado_codigo == '22' ~ 'Querétaro',
      estado_codigo == '23' ~ 'Quintana Roo',
      estado_codigo == '24' ~ 'San Luis Potosí',
      estado_codigo == '25' ~ 'Sinaloa',
      estado_codigo == '26' ~ 'Sonora',
      estado_codigo == '27' ~ 'Tabasco',
      estado_codigo == '28' ~ 'Tamaulipas',
      estado_codigo == '29' ~ 'Tlaxcala',
      estado_codigo == '30' ~ 'Veracruz',
      estado_codigo == '31' ~ 'Yucatán',
      estado_codigo == '32' ~ 'Zacatecas',
      TRUE ~ NA_character_  # Para códigos no listados
    )
  )


# Paso 1: Calcular el ingreso total trimestral y el porcentaje de ingreso de cada persona en el hogar
ingresos <- ingresos %>%
  group_by(folioviv, foliohog, numren) %>%
  summarize(ing_tri = sum(ing_tri, na.rm = TRUE)) #%>%      # Aseguramos que cada persona tenga solo un ingreso trimestral total
#ungroup() %>%
#group_by(folioviv, foliohog) %>%
#mutate(
#  ing_hogar_tri = sum(ing_tri, na.rm = TRUE),            # Ingreso total del hogar
#  pctg_ing = ing_tri / ing_hogar_tri                     # Porcentaje de ingreso de cada persona en el hogar
#) %>%
#ungroup()


# Paso 3: Unir las tablas y calcular variables a nivel individual
poblacion_demanda <- poblacion %>%
  left_join(variables_demanda, by = "folioviv") %>%
  left_join(ingresos, by = c("folioviv", "foliohog", "numren")) %>%
  left_join(concentradohogar, by = c("folioviv", "foliohog")) %>%
  
  mutate(
    ingreso_mensual = ing_tri / 3,  # Ingreso mensual individual
    hablaindigena = ifelse(hablaind == 1, 1, 0),
    hablaindigena = as.factor(hablaindigena),
    vive_en_pareja = ifelse(pareja_hog == 1, 1, 0),
    atencion_medica = ifelse(atemed == 1, 1, 0),
    prestaciones = ifelse(inscr_1 == 1, 1, 0),
    sexo = ifelse(sexo == 1, sexo, 0),  # 1 == hombres
    sexo = as.factor(sexo), 
    vivienda = as.numeric(vivienda),
    #FALTA CREAR AQUI LAS VARIABLES CORRECTAS DE DEMANDA DE VIVIEDNA
    gasto = (vivienda / tot_integ) / 3
    
  ) %>%
  select(-pareja_hog, -atemed, -inscr_1) %>% 
  filter(edad>= 18, edad <= 40, ingreso_mensual > 0) %>% 
  mutate(
    costo_uso_capital = gasto / ingreso_mensual)




#aligerar la memoria
rm(conexion,concentradohogar,ingresos,poblacion,variables_demanda,viviendas)

#garbage collector
gc()

#DF para analisis se independiza o no
final <- poblacion_demanda %>%
  sample_frac(size = 1, weight = factor_poblacion) %>%   # 100% de la muestra ponderada
  select(factor_poblacion,sexo, edad, nivelaprob,dueno, independencia, 
         ingreso_mensual, hablaindigena, costo_uso_capital
  )


#DF para analisis compra o renta
final_2 <- poblacion_demanda %>%
  filter(independencia == 1) %>% #filtro para cuando deciden la independencia
  sample_frac(size = 1, weight = factor_poblacion) %>%   # 100% de la muestra ponderada
  select(-folioviv, -foliohog, -numren, -tenencia.x,tenencia.y)


#######################################


#Modelos LOGIT Y RANDFOREST (modificar los insumos como sea conveniente)
#######################################

#correr antes el problema de clasificacion y escoger un df para usar: final, final_2

##MODELO LOGIT

# Definir lista de variables explicativas
variables_explicativas <- c("ingreso_mensual", "costo_uso_capital", "nivelaprob", "edad", "sexo","hablaindigena")

# Asegurar que demanda_vivienda es un factor
modelo <- final_2 %>% #Cambiar variable de acuerdo al modelo 1 o 2 (independencia o dueno )
  mutate(demanda_vivienda = factor(dueno, levels = c(0, 1), labels = c("No", "Si")))

# Separar los datos en entrenamiento y prueba
data_split <- modelo %>%
  initial_split(strata = demanda_vivienda) # Estratificar por la variable objetivo
train <- training(data_split) # Datos de entrenamiento
test <- testing(data_split)   # Datos de prueba

# Seleccionar solo las columnas relevantes en los datos
train <- train %>%
  select(all_of(c("demanda_vivienda", variables_explicativas)))

test <- test %>%
  select(all_of(c("demanda_vivienda", variables_explicativas)))

# Crear pliegues de validación cruzada
folds <- vfold_cv(train, v = 5)

# Crear una receta
receta <- recipe(demanda_vivienda ~ ., data = train) %>%
  update_role(all_of(variables_explicativas), new_role = "predictor") %>%
  update_role(demanda_vivienda, new_role = "outcome") %>%
  step_mutate(sexo = as.factor(sexo)) %>%        # Convertir sexo en factor
  step_dummy(all_nominal_predictors()) %>%       # Crear variables dummies
  step_impute_median(all_predictors()) %>%       # Imputar valores faltantes con la mediana
  step_smote(demanda_vivienda) %>%               # Aplicar SMOTE después de manejar NAs
  step_log(ingreso_mensual, offset = 0.001)                    # Remover predictores con varianza cero

# Definir el modelo logístico con glmnet
reg_logistica <- logistic_reg() %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  set_args(penalty = tune(), mixture = tune()) # Hiperparámetros a tunear

# Crear el flujo de trabajo
modelo_logistico_glmnet <- workflow() %>%
  add_recipe(receta) %>%
  add_model(reg_logistica)

# Ajuste de hiperparámetros mediante validación cruzada
parametros <- modelo_logistico_glmnet %>%
  extract_parameter_set_dials()

calibracion <- modelo_logistico_glmnet %>%
  tune_grid(
    resamples = folds,
    param_info = parametros,
    grid = 300,                                # 100 combinaciones de hiperparámetros
    metrics = metric_set(roc_auc, accuracy),   # Métricas de desempeño
    control = control_grid(verbose = TRUE)    # Barra de progreso
  )



# Obtener las métricas de calibración
metricas_calibracion <- calibracion %>%
  collect_metrics()

# Seleccionar el mejor modelo basado en ROC
mejor_modelo <- calibracion %>%
  select_best(metric = "roc_auc")

# Ajuste final con los datos completos
ajuste_final <- modelo_logistico_glmnet %>%
  finalize_workflow(mejor_modelo) %>%
  last_fit(data_split)

# Evaluar el modelo final
resultados_finales <- ajuste_final %>%
  collect_metrics()

# Mostrar las métricas
print(resultados_finales)

# Matriz de confusión
ajuste_final %>%
  collect_predictions() %>%
  conf_mat(truth = demanda_vivienda, estimate = .pred_class)

# Interpretación del modelo: predicciones y probabilidades
resultados <- ajuste_final %>%
  extract_workflow() %>%  # Extraer el flujo de trabajo final ajustado
  augment(new_data = test)  # Especificar los datos sobre los cuales quieres las predicciones



# Visualización de probabilidades
#CODIGO PARA ANALISIS DE RESULTADOS
grafico_4 = resultados %>% 
  mutate(
    # Crear nueva variable agrupada para nivelaprob
    educacion = case_when(
      nivelaprob == 0 ~ "Ninguna",                          # Ninguna
      nivelaprob %in% c(1, 2, 3) ~ "Básica",                # Básica
      nivelaprob %in% c(4, 5, 6) ~ "Media Superior",        # Media Superior
      nivelaprob %in% c(7, 8, 9) ~ "Superior"               # Superior
    ),
    lengua = ifelse(
      hablaindigena == 0,
      yes = 'No habla lengua indígena', 
      no = 'Habla lengua indígena'
    ),
    sexo = ifelse(
      sexo == 0,
      yes = 'Mujer', 
      no = 'Hombre'
    )
  ) %>%
  ggplot(aes(
    x = ingreso_mensual,
    y = .pred_Si)) + 
  # y = log(.pred_Si/.pred_No))) +  ### para verlo en forma lineal
  geom_point(
    aes(fill = edad),
    shape = 21,
    col = 'white',
    alpha = .8,
    size = 2.5
  ) + 
  geom_hline(yintercept = 0.5, linetype = 'dashed', col = 'gray50'
  ) +
  stat_smooth(
    method = 'glm', method.args = list(family = binomial),
    formula = y ~ x, se = FALSE,
    aes(col = sexo)  # Usar la nueva agrupación de educación
  ) +
  scale_x_continuous(
    trans = log10_trans(),
    labels = scales::dollar_format(),
    breaks = c(100, 1000, 10000, 100000, 1000000)
  ) + 
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  scale_fill_gradient2(
    low = '#e7e7df' ,mid = '#e38981', high = '#d13030'
  ) +
  scale_colour_manual(
    values = c('#2c3e50','#ff233d')
  ) +
  facet_wrap( ~ educacion) +  # Facet ahora usa la agrupación de educación
  labs(
    x = 'Ingreso mensual (log)',
    col = 'Educación',  # Actualizar la etiqueta de la leyenda
    title = 'Determinantes de la probabilidad de demanda de vivienda',
    caption = 'Fuente = Elaboración propia con datos de ENIGH2022',
  ) + 
  theme_bw(base_size = 8) + 
  theme(axis.title.y = element_blank())

# Guardar el gráfico con fondo transparente
ggsave("grafico4_transparente.png", plot = grafico_4, bg = "transparent", width = 10, height = 7, dpi = 300)






#modelo RANDOMFOREST para predicción de clases


# 1. Definir lista de variables explicativas
variables_explicativas <- c("ingreso_mensual", "costo_uso_capital", "nivelaprob", "edad", "sexo","hablaindigena")

# 2. Asegurar que demanda_vivienda es un factor
modelo <- final %>%
  mutate(demanda_vivienda = factor(independencia, levels = c(0, 1), labels = c("No", "Si")))

# 3. Separar los datos en entrenamiento y prueba
data_split <- modelo %>%
  initial_split(strata = demanda_vivienda)  # Estratificar por la variable objetivo
train <- training(data_split)
test <- testing(data_split)

# 4. Seleccionar solo las columnas relevantes
train <- train %>%
  select(all_of(c("demanda_vivienda", variables_explicativas)))

test <- test %>%
  select(all_of(c("demanda_vivienda", variables_explicativas)))

# 5. Crear pliegues de validación cruzada
folds <- vfold_cv(train, v = 5)

# 6. Crear una receta con manejo de NAs y SMOTE
receta <- recipe(demanda_vivienda ~ ., data = train) %>%
  update_role(all_of(variables_explicativas), new_role = "predictor") %>%
  update_role(demanda_vivienda, new_role = "outcome") %>%
  step_mutate(sexo = as.factor(sexo)) %>%        # Convertir sexo en factor
  step_dummy(all_nominal_predictors()) %>%       # Crear variables dummies
  step_impute_median(all_predictors()) %>%       # Imputar valores faltantes con la mediana
  step_smote(demanda_vivienda) %>%               # Aplicar SMOTE después de manejar NAs
  step_log(ingreso_mensual)                      # Remover predictores con varianza cero

# 7. Definir el modelo Random Forest
rf_model <- rand_forest(
  mtry = tune(),  # Número de predictores a considerar en cada división
  min_n = tune(),  # Número mínimo de datos por nodo
  trees = tune()      # Número de árboles
) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# 8. Crear flujo de trabajo
modelo_rf <- workflow() %>%
  add_recipe(receta) %>%
  add_model(rf_model)

# 9. Ajuste de hiperparámetros mediante validación cruzada
parametros_rf <- modelo_rf %>%
  extract_parameter_set_dials()

calibracion_rf <- modelo_rf %>%
  tune_grid(
    resamples = folds,
    param_info = parametros_rf,
    grid = 100,                                # Número de combinaciones de hiperparámetros
    metrics = metric_set(roc_auc, accuracy),  # Métricas de desempeño
    control = control_grid(verbose = TRUE)    # Barra de progreso
  )

# 10. Obtener las métricas de calibración
metricas_calibracion_rf <- calibracion_rf %>%
  collect_metrics()

# 11. Seleccionar el mejor modelo basado en ROC
mejor_modelo_rf <- calibracion_rf %>%
  select_best(metric = "roc_auc")

# 12. Ajuste final con los datos completos
ajuste_final_rf <- modelo_rf %>%
  finalize_workflow(mejor_modelo_rf) %>%
  last_fit(data_split)

# 13. Evaluar el modelo final
resultados_finales_rf <- ajuste_final_rf %>%
  collect_metrics()

# Mostrar las métricas
print(resultados_finales_rf)

# 14. Matriz de confusión
matriz_confusion_rf <- ajuste_final_rf %>%
  collect_predictions() %>%
  conf_mat(truth = demanda_vivienda, estimate = .pred_class)

print(matriz_confusion_rf)

# 15. Interpretación del modelo: predicciones y probabilidades
resultados_rf <- ajuste_final_rf %>%
  extract_workflow() %>%
  augment(new_data = test)

# Mostrar algunas predicciones
print(head(resultados_rf))


#CODIGO PARA ANALISIS DE RESULTADOS
grafico_6 = resultados_rf %>% 
  mutate(
    # Crear nueva variable agrupada para nivelaprob
    educacion = case_when(
      nivelaprob == 0 ~ "Ninguna",                          # Ninguna
      nivelaprob %in% c(1, 2, 3) ~ "Básica",                # Básica
      nivelaprob %in% c(4, 5, 6) ~ "Media Superior",        # Media Superior
      nivelaprob %in% c(7, 8, 9) ~ "Superior"               # Superior
    ),
    lengua = ifelse(
      hablaindigena == 0,
      yes = 'No habla lengua indígena', 
      no = 'Habla lengua indígena'
    ),
    sexo = ifelse(
      sexo == 0,
      yes = 'Mujer', 
      no = 'Hombre'
    )
  ) %>%
  ggplot(aes(
    x = ingreso_mensual,
    y = .pred_Si)) + 
  # y = log(.pred_Si/.pred_No))) +  ### para verlo en forma lineal
  geom_point(
    aes(fill = edad),
    shape = 21,
    col = 'white',
    alpha = .8,
    size = 2.5
  ) + 
  geom_hline(yintercept = 0.5, linetype = 'dashed', col = 'gray50'
  ) +
  stat_smooth(
    method = 'glm', method.args = list(family = binomial),
    formula = y ~ x, se = FALSE,
    aes(col = sexo)  # Usar la nueva agrupación de educación
  ) +
  scale_x_continuous(
    trans = log10_trans(),
    labels = scales::dollar_format(),
    breaks = c(100, 1000, 10000, 100000, 1000000)
  ) + 
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  scale_fill_gradient2(
    low = '#e7e7df' ,mid = '#e38981', high = '#d13030'
  ) +
  scale_colour_manual(
    values = c('#2c3e50','#ff233d')
  ) +
  facet_wrap( ~ educacion) +  # Facet ahora usa la agrupación de educación
  labs(
    x = 'Ingreso mensual (log)',
    col = 'Educación',  # Actualizar la etiqueta de la leyenda
    title = 'Determinantes de la probabilidad de demanda de vivienda',
    caption = 'Fuente = Elaboración propia con datos de ENIGH2022\nAutor: Andrés Gayón',
  ) + 
  theme_bw(base_size = 8) + 
  theme(axis.title.y = element_blank())

# Guardar el gráfico con fondo transparente
ggsave("grafico6_transparente.png", plot = grafico_6, bg = "transparent", width = 10, height = 7, dpi = 300)





#Guardando en csv la información (RF)
# Crear directorio para guardar resultados si no existe
if (!dir.exists("resultados_rf")) {
  dir.create("resultados_rf")
}

# Guardar métricas de calibración
metricas_calibracion_rf %>%
  write.csv("resultados_rf/metricas_calibracion_rf.csv", row.names = FALSE)

# Guardar el mejor modelo basado en ROC
mejor_modelo_rf %>%
  write.csv("resultados_rf/mejor_modelo_rf.csv", row.names = FALSE)

# Guardar las métricas finales del modelo ajustado
resultados_finales_rf %>%
  write.csv("resultados_rf/resultados_finales_rf.csv", row.names = FALSE)

# Convertir matriz de confusión a un formato "tidy"
matriz_confusion_rf_tidy <- matriz_confusion_rf %>%
  tidy()

# Guardar la matriz de confusión como archivo CSV
write.csv(
  matriz_confusion_rf_tidy,
  "resultados_rf/matriz_confusion_rf.csv",
  row.names = FALSE
)

# Guardar las predicciones y probabilidades del modelo
resultados_rf %>%
  write.csv("resultados_rf/resultados_rf.csv", row.names = FALSE)






#Guardando en csv la información (LOGIT)
# Crear directorio para guardar resultados si no existe
if (!dir.exists("resultados_logit")) {
  dir.create("resultados_logit")
}

# Guardar métricas de calibración
metricas_calibracion_rf %>%
  write.csv("resultados_logit/metricas_calibracion.csv", row.names = FALSE)

# Guardar el mejor modelo basado en ROC
mejor_modelo_logit %>%
  write.csv("resultados_logit/mejor_modelo.csv", row.names = FALSE)

# Guardar las métricas finales del modelo ajustado
resultados_finales_logit %>%
  write.csv("resultados_logit/resultados_finales.csv", row.names = FALSE)

# Convertir matriz de confusión a un formato "tidy"
matriz_confusion_logit_tidy <- matriz_confusion_rf %>%
  tidy()

# Guardar la matriz de confusión como archivo CSV
write.csv(
  matriz_confusion_logit_tidy,
  "resultados_logit/matriz_confusion.csv",
  row.names = FALSE
)

# Guardar las predicciones y probabilidades del modelo
resultados %>%
  write.csv("resultados_logit/resultados.csv", row.names = FALSE)


#######################################




