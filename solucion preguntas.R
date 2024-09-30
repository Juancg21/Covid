
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(corrplot)
library(kableExtra)
require(pander)
library(wordcloud)
library(knitr)

install.packages("plotly")
library(plotly)

install.packages("ggiraph")
library(ggiraph)

install.packages("highcharter")
library(highcharter)

install.packages("leaflet")
library(leaflet)

install.packages("DT")
library(DT)

install.packages("echarts4r")
library(echarts4r)


install.packages("waffle")

install.packages("extrafont")

install.packages("scales")
library(scales)

install.packages("rmarkdown")
library(rmarkdown)

datatable(pais_distribution)


#Exploración básica de datos###########

skim(Dset_cleam) ####util util util para ver dtframe en general #########

# Resumen detallado del data frame
describe(Dset_cleam)

colnames(limpio_mio)



any(is.na(limpio_mio))

sum(is.na(limpio_mio))

colSums(is.na(limpio_mio))

str(Dset_cleam)



#Preguntas

#1. ID: ¿Cuál es el propósito del campo de identificación y cómo se generan estos IDs?

#El campo ID en un data frame suele representar un identificador único para cada 
#fila o registro. Su propósito principal es asegurar que cada observación (persona, evento, objeto, etc.) 
#tenga un valor único, permitiendo que cada fila se distinga de las demás.

#  2. Edad: ¿Cuál es la distribución de edades de las personas en el dataset y hay algún rango de edad específico que esté más representado?

# Cargar las librerías necesarias
library(ggplot2)

# Crear un histograma de las edades
ggplot(Dset_cleam, aes(x = Edad)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Edades", x = "Edad", y = "Frecuencia") +
  theme_minimal()
#___________________


# Crear un histograma con curva de densidad
ggplot(Dset_cleam, aes(x = Edad)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "blue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +  # Agregar curva de densidad
  labs(title = "Distribución de Edades", x = "Edad", y = "Densidad") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold", color = "blue"),
    axis.title.y = element_text(face = "bold", color = "blue")
  )


#  3. Género: ¿Cuál es la distribución de géneros en el dataset y cómo se clasifican los diferentes géneros?

# Verificar el tipo de la variable Género
str(Dset_cleam$Género)

# Obtener los niveles únicos de la variable Género
unique(Dset_cleam$Género)


# Contar las frecuencias de cada género
genero_distribución <- table(Dset_cleam$Género)
genero_distribución

#-----------------------------------------

# Preparar los datos: contar la frecuencia de cada género
genero_distribución <- Dset_cleam %>%
  group_by(Género) %>%
  summarise(Frecuencia = n())

# Crear gráfico de donut
ggplot(genero_distribución, aes(x = "", y = Frecuencia, fill = Género)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Frecuencia / sum(Frecuencia) * 100), "%")),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Distribución de Géneros", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "right")

#-----------------------------------------

# Crear un gráfico de barras con etiquetas de frecuencia
ggplot(genero_distribución, aes(x = Género, y = Frecuencia, fill = Género)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = Frecuencia), vjust = -0.3, size = 5) +
  labs(title = "Distribución de Géneros", x = "Género", y = "Frecuencia") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none")

#-----------------------------------------


#  4. País: ¿Cuántos países diferentes están representados en el dataset y cuál es el país con más casos registrados?
# Instala el paquete leaflet (solo la primera vez)
install.packages("leaflet")

# Carga el paquete
library(leaflet)

# Crear un dataframe actualizado con los datos proporcionados
pais_distribucionn <- data.frame(
  País = c("Francia", "Brasil", "Argentina", "España", "Alemania", 
           "México", "Canadá", "Italia", "Reino Unido", "USA"),
  Casos = c(17, 15, 13, 13, 12, 12, 11, 11, 11, 1),
  Latitud = c(46.6034, -14.2350, -38.4161, 40.4637, 51.1657, 
              23.6345, 56.1304, 41.8719, 55.3781, 37.0902),
  Longitud = c(1.8883, -51.9253, -63.6167, -3.7038, 10.4515, 
               -102.5528, -106.3468, 12.5674, -3.4360, -95.7129)
)

# Imprimir el dataframe para verificar
print(pais_distribucionn)

# Crear el mapa interactivo
leaflet(data = pais_distribucionn) %>%
  addTiles() %>%
  addMarkers(lng = ~Longitud, lat = ~Latitud, 
             popup = ~paste(País, ": ", Casos, " casos"))

####________________



# Crear un dataframe con las URLs de las banderas
pais_distributionM <- data.frame(
  País = c("Francia", "Brasil", "Argentina", "España", "Alemania", 
           "México", "Canadá", "Italia", "Reino Unido", "USA"),
  Casos = c(17, 15, 13, 13, 12, 12, 11, 11, 11, 1),
  Latitud = c(46.6034, -14.2350, -38.4161, 40.4637, 51.1657, 
              23.6345, 56.1304, 41.8719, 55.3781, 37.0902),
  Longitud = c(1.8883, -51.9253, -63.6167, -3.7038, 10.4515, 
               -102.5528, -106.3468, 12.5674, -3.4360, -95.7129),
  Banderas = c(
    "https://flagcdn.com/w320/fr.png", # Francia
    "https://flagcdn.com/w320/br.png", # Brasil
    "https://flagcdn.com/w320/ar.png", # Argentina
    "https://flagcdn.com/w320/es.png", # España
    "https://flagcdn.com/w320/de.png", # Alemania
    "https://flagcdn.com/w320/mx.png", # México
    "https://flagcdn.com/w320/ca.png", # Canadá
    "https://flagcdn.com/w320/it.png", # Italia
    "https://flagcdn.com/w320/gb.png", # Reino Unido
    "https://flagcdn.com/w320/us.png"  # USA
  )
)

# Crear el mapa interactivo con las banderas de los países
leaflet(data = pais_distributionM) %>%
  addTiles() %>%
  addMarkers(
    lng = ~Longitud, lat = ~Latitud, 
    icon = ~icons(iconUrl = Banderas, 
                  iconWidth = 30, iconHeight = 20),  # Tamaño de la bandera
    popup = ~paste("<b>", País, "</b>", "<br/>", 
                   Casos, " casos")  # Texto emergente
  )
###############################################################################

library(leaflet)

#GRAFICA DE MAPA MEJORADO CON SOMBREADO DE FRANCIA

# Crear el dataframe con las banderas y datos
pais_distributionM <- data.frame(
  País = c("Francia", "Brasil", "Argentina", "España", "Alemania", 
           "México", "Canadá", "Italia", "Reino Unido", "USA"),
  Casos = c(17, 15, 13, 13, 12, 12, 11, 11, 11, 1),
  Latitud = c(46.6034, -14.2350, -38.4161, 40.4637, 51.1657, 
              23.6345, 56.1304, 41.8719, 55.3781, 37.0902),
  Longitud = c(1.8883, -51.9253, -63.6167, -3.7038, 10.4515, 
               -102.5528, -106.3468, 12.5674, -3.4360, -95.7129),
  Banderas = c(
    "https://flagcdn.com/w320/fr.png", # Francia
    "https://flagcdn.com/w320/br.png", # Brasil
    "https://flagcdn.com/w320/ar.png", # Argentina
    "https://flagcdn.com/w320/es.png", # España
    "https://flagcdn.com/w320/de.png", # Alemania
    "https://flagcdn.com/w320/mx.png", # México
    "https://flagcdn.com/w320/ca.png", # Canadá
    "https://flagcdn.com/w320/it.png", # Italia
    "https://flagcdn.com/w320/gb.png", # Reino Unido
    "https://flagcdn.com/w320/us.png"  # USA
  )
)

# Crear el mapa interactivo
leaflet(data = pais_distributionM) %>%
  addTiles() %>%
  addMarkers(
    lng = ~Longitud, lat = ~Latitud, 
    icon = ~icons(iconUrl = Banderas, iconWidth = 30, iconHeight = 20),  # Tamaño de la bandera
    popup = ~paste(País, ": ", Casos, " casos")  # Texto emergente
  ) %>%
  # Añadir un marcador especial para Francia
  addMarkers(
    lng = ~Longitud[País == "Francia"], lat = ~Latitud[País == "Francia"],
    icon = ~icons(iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c3/Flag_of_France.svg/1024px-Flag_of_France.svg.png", 
                  iconWidth = 30, iconHeight = 20),  # Ícono grande de la bandera
    popup = ~paste0("<b style='font-size: 20px; color: red;'>", País, "</b>", "<br/>", 
                    Casos, " casos"),  # Popup más destacado
    label = "¡Francia!",  # Etiqueta para el marcador
    labelOptions = labelOptions(style = list("font-weight" = "bold", "font-size" = "16px", "color" = "red")) # Etiqueta personalizada
  ) %>%
  # Añadir un círculo pulsante para Francia
  addCircles(lng = ~Longitud[País == "Francia"], lat = ~Latitud[País == "Francia"], 
             radius = 50000, color = "red", fillOpacity = 0.1, stroke = FALSE) %>%
  # Animación en el círculo pulsante
  addCircleMarkers(
    lng = ~Longitud[País == "Francia"], lat = ~Latitud[País == "Francia"], 
    radius = 30, color = "red", fill = TRUE, fillOpacity = 0.5, 
    stroke = FALSE, popup = "!Francia tiene la mayor cantidad de casos! 17",
    options = markerOptions(riseOnHover = TRUE, riseOffset = 100)
  )


###############################################################################
#tabla bonita

# Crear un dataframe actualizado con las columnas deseadas
pais_distribu <- pais_distribucionn %>%
  select(País, Casos)  # Selecciona solo las columnas País y Casos

# Ver el nuevo dataframe
print(pais_distribu)

datatable(pais_distribu)
# HASTA AQUI ESTA BIEN, LO DEMAS ES AVARICIA

#--------------------------------------

# Contar los casos por país
pais_distribución <- Dset_cleam %>%
  group_by(País) %>%
  summarise(Casos = n()) %>%
  arrange(desc(Casos))
print(pais_distribución)


# Crear un gráfico de barras para la distribución de casos por país
ggplot(pais_distribu, aes(x = reorder(País, -Casos), y = Casos)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "blue") +
  labs(title = "Distribución de Casos por País",
       x = "País",
       y = "Número de Casos") +
  theme_minimal() +
  coord_flip()  # Girar las barras para mejor visualización

###_______





#  5. Estado: ¿Cuál es la proporción de estados (positivo, recuperado, etc.) en el dataset y cómo se determina el estado de una persona?

# Verificar el tipo de la variable Estado
str(Dset_cleam$Estado)

# Obtener los niveles únicos de la variable Estado
unique(Dset_cleam$Estado)


# Calcular la proporción de cada estado
estado_distribution <- Dset_cleam %>%
  group_by(Estado) %>%
  summarise(Casos = n()) %>%
  mutate(Proporción = Casos / sum(Casos) * 100) %>%
  arrange(desc(Proporción))

# Mostrar la distribución por estado
print(estado_distribution)
# Crear un gráfico de barras para la proporción de estados
ggplot(estado_distribution, aes(x = reorder(Estado, -Proporción), y = Proporción)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "darkgreen") +
  labs(title = "Proporción de Estado de Salud",
       x = "Estado",
       y = "Proporción (%)") +
  theme_minimal() +
  geom_text(aes(label = round(Proporción, 1)), 
            position = position_stack(vjust = 0.5), 
            color = "black") +  # Añadir etiquetas con las proporciones
  coord_flip()  # Girar las barras para mejor visualización

###_-----------------------------------------

# Crear un gráfico de tipo donut
fig <- plot_ly(data = estado_distribution, 
               labels = ~Estado, 
               values = ~Proporción, 
               type = 'pie', 
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'), 
               hole = 0.4) %>%
  layout(title = 'Proporción de Estado de Salud',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Mostrar el gráfico de donut
fig



#  6. Síntomas: ¿Cuáles son los síntomas más comunes reportados en el dataset y hay algún patrón entre ellos?

# Cargar librerías necesarias
library(dplyr)
library(stringr)

# Separar los síntomas en una lista usando comas como delimitador
Dset_cleam_separado <- Dset_cleam %>%
  mutate(Síntomas = str_split(Síntomas, ","))  # Separar por comas

# Convertir la lista en un formato más manejable (una fila por síntoma)
Dset_sintomas_long <- Dset_cleam_separado %>%
  unnest(Síntomas) %>%          # Expandir la lista de síntomas
  mutate(Síntomas = str_trim(Síntomas))  # Eliminar espacios en blanco


#Paso 3: Calcular la frecuencia de cada síntoma
#Con los síntomas ya separados y listos para analizar, podemos contar cuántas veces aparece cada síntoma en el dataset.


# Contar la frecuencia de cada síntoma
ffrecuencia_sintomas <- Dset_sintomas_long %>%
  group_by(Síntomas) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

# Mostrar las frecuencias de los síntomas más comunes
print(ffrecuencia_sintomas)

####---------------------------------



# Crear un gráfico de barras para la frecuencia de síntomas
ggplot(ffrecuencia_sintomas, aes(x = reorder(Síntomas, -Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "darkblue") +
  labs(title = "Síntomas Más Comunes Reportados",
       x = "Síntomas",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x
  geom_text(aes(label = Frecuencia), vjust = -0.5, color = "darkblue")  # Añadir etiquetas con frecuencias

#-------------------


#  7. Fecha de diagnóstico: ¿Cómo se distribuyen las fechas de diagnóstico a lo largo del tiempo y hay algún patrón estacional o temporal en los diagnósticos?

names(Dset_cleam)
class("Fecha de diagnóstico")

# Convertir la columna "Fecha de diagnóstico" a formato de fecha
Dset_cleam$`Fecha de diagnóstico` <- as.Date(Dset_cleam$`Fecha de diagnóstico`, format = "%d/%m/%Y")  # Cambia el formato según tus datos

class(Dset_cleam$`Fecha de diagnóstico`)

# Contar la frecuencia de diagnósticos por fecha
frecuencia_Fecha_diagnosticos <- Dset_cleam %>%
  group_by(`Fecha de diagnóstico`) %>%
  summarise(Casos = n())

# Crear un gráfico de líneas para visualizar la distribución
ggplot(frecuencia_Fecha_diagnosticos, aes(x = `Fecha de diagnóstico`, y = Casos)) +
  geom_line(color = "blue") +
  labs(title = "Distribución de Diagnósticos a lo Largo del Tiempo",
       x = "Fecha de Diagnóstico",
       y = "Número de Diagnósticos") +
  theme_minimal()


#----

  ggplot(frecuencia_Fecha_diagnosticos, aes(x = `Fecha de diagnóstico`, y = Casos)) +
  geom_area(fill = "lightblue", alpha = 0.6) +
  labs(title = "Gráfico de Área de Diagnósticos por Fecha",
       x = "Fecha de Diagnóstico",
       y = "Frecuencia de Diagnósticos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#_______



#  8. Temperatura: ¿Cuál es la distribución de las temperaturas registradas y hay alguna temperatura que sea anormalmente alta o baja?

# Verificar el tipo de la variable Temperatura
class(Dset_cleam$Temperatura)

# Resumen estadístico de la variable Temperatura
summary(Dset_cleam$Temperatura)


#_----------------------
ggplot(Dset_cleam, aes(y = Temperatura)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Boxplot de Temperaturas Registradas",
       y = "Temperatura (°C)") +
  theme_minimal()

#_------------------
ggplot(Dset_cleam, aes(x = seq_along(Temperatura), y = Temperatura)) +
  geom_point(color = "blue") +
  labs(title = "Gráfico de Puntos de Temperaturas Registradas",
       x = "Índice",
       y = "Temperatura (°C)") +
  theme_minimal()



#  9. Presión arterial: ¿Cómo varía la presión arterial registrada y hay alguna tendencia o patrón entre los valores?
class(Dset_cleam$`Presión arterial`)

str(Dset_cleam)

# Ver las primeras filas de la columna
head(Dset_cleam$`Presión arterial`)


# Asegúrate de que no hay espacios en blanco en la columna original
Dset_cleam$`Presión arterial` <- str_trim(Dset_cleam$`Presión arterial`)

# Separar la columna "Presión arterial" en dos columnas: Sistólica y Diastólica
Dset_cleam <- Dset_cleam %>%
  separate(`Presión arterial`, into = c("Sistólica", "Diastólica"), sep = "/", convert = TRUE)

# Comprobar que las nuevas columnas se han creado y están en formato numérico
head(Dset_cleam[c("Sistólica", "Diastólica")])
class(Dset_cleam$Sistólica)  # Verifica que la clase sea "numeric"
class(Dset_cleam$Diastólica)  # Verifica que la clase sea "numeric"

#----
library(ggplot2)

# Crear un gráfico de dispersión
ggplot(Dset_cleam, aes(x = Sistólica, y = Diastólica)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Gráfico de Dispersión: Presión Arterial",
       x = "Presión Sistólica (mmHg)",
       y = "Presión Diastólica (mmHg)") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "red", se = FALSE)  # Línea de tendencia



#  10. Nivel de oxígeno: ¿Cuál es el rango típico de niveles de oxígeno y cómo se relaciona esto con el estado de salud de los pacientes?
# Verificar el tipo de dato del nivel de oxígeno
class(Dset_cleam$`Nivel de oxígeno`)

# Resumen estadístico
summary(Dset_cleam$`Nivel de oxígeno`)

library(ggplot2)

# Crear un boxplot para ver la distribución del nivel de oxígeno por estado de salud
ggplot(Dset_cleam, aes(x = Estado, y = `Nivel de oxígeno`, fill = Estado)) +
  geom_boxplot() +
  labs(title = "Distribución del Nivel de Oxígeno por Estado de Salud",
       x = "Estado de Salud",
       y = "Nivel de Oxígeno") +
  theme_minimal() +
  theme(legend.position = "none")

######

# Calcular la media del nivel de oxígeno por estado de salud
nivel_oxigeno_por_estado <- Dset_cleam %>%
  group_by(Estado) %>%
  summarise(Media_nivel_oxigeno = mean(`Nivel de oxígeno`, na.rm = TRUE),
            Mediana_nivel_oxigeno = median(`Nivel de oxígeno`, na.rm = TRUE))

# Mostrar resultados
print(nivel_oxigeno_por_estado)





#  11. Tratamiento: ¿Qué tipos de tratamientos son más comunes y cómo varían según el estado de los pacientes?

# Contar la frecuencia de cada tratamiento
tratamiento_frecuencia <- Dset_cleam %>%
  group_by(Tratamiento) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

# Mostrar las frecuencias
print(tratamiento_frecuencia)


# Contar la frecuencia de tratamientos según el estado de los pacientes
tratamiento_estado <- Dset_cleam %>%
  group_by(Estado, Tratamiento) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

# Mostrar la relación entre tratamientos y estado
print(tratamiento_estado)

# Crear un gráfico de barras apiladas para los tratamientos según el estado
##
ggplot(tratamiento_estado, aes(x = Estado, y = Frecuencia, fill = Tratamiento)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proporción de Tratamientos según el Estado de los Pacientes",
       x = "Estado",
       y = "Proporción",
       fill = "Tratamiento") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)  # Mostrar el eje y en porcentaje

### hasta aqui creo que esta bien.###########




#  12. Vacunado: ¿Qué proporción de la población en el dataset está vacunada y cómo se relaciona esto con el estado de salud y el curso de la enfermedad?

# Calcular la proporción de personas vacunadas
proporcion_vacunado <- Dset_cleam %>%
  group_by(Vacunado) %>%
  summarise(Frecuencia = n()) %>%
  mutate(Proporción = Frecuencia / sum(Frecuencia))

# Mostrar la proporción


library(ggplot2)
library(scales)  # Para formatear etiquetas

# Gráfico de tarta con mejoras visuales
ggplot(proporcion_vacunado, aes(x = "", y = Proporción, fill = Vacunado)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Borde blanco para cada segmento
  coord_polar("y", start = 0) +  # Comenzar en la parte superior
  labs(title = "Proporción de la Población Vacunada",
       subtitle = "Distribución de vacunados y no vacunados en la muestra",
       ,
       x = NULL,  # Eliminar etiqueta del eje X
       y = NULL) +  # Eliminar etiqueta del eje Y
  theme_void() +  # Eliminar elementos de fondo
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right"  # Posicionar la leyenda a la derecha
  ) +
  geom_text(aes(label = percent(Proporción)),  # Agregar etiquetas de porcentaje
            position = position_stack(vjust = 0.5),  # Centrar etiquetas en los segmentos
            color = "white", size = 5, fontface = "bold")  # Color y tamaño de las etiquetas


# Calcular la frecuencia de vacunación por estado
vacunado_estado <- Dset_cleam %>%
  group_by(Estado, Vacunado) %>%
  summarise(Frecuencia = n()) %>%
  mutate(Proporción = Frecuencia / sum(Frecuencia))

# Mostrar los resultados
print(vacunado_estado)
ggplot(vacunado_estado, aes(x = Estado, y = Proporción, fill = Vacunado)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Relación entre Vacunación y Estado de Salud",
       x = "Estado de Salud",
       y = "Proporción",
       fill = "Vacunado") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)  # Mostrar el eje y en porcentaje


# Frecuencia de vacunación por resultado de la prueba
vacunado_resultado <- Dset_cleam %>%
  group_by(Resultado = `Resultado de la prueba`, Vacunado) %>%
  summarise(Frecuencia = n()) %>%
  mutate(Proporción = Frecuencia / sum(Frecuencia))

# Gráfico de barras apiladas para el resultado de la prueba
ggplot(vacunado_resultado, aes(x = Resultado, y = Proporción, fill = Vacunado)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Relación entre Vacunación y Resultado de la Prueba",
       x = "Resultado de la Prueba",
       y = "Proporción",
       fill = "Vacunado") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)

#  13. Fecha de vacunación: ¿Cómo se distribuyen las fechas de vacunación y hay alguna relación entre la fecha de vacunación y el estado posterior del paciente?
#  14. Vacuna: ¿Qué vacunas son más comunes en el dataset y hay alguna diferencia en la efectividad entre ellas?
#  15. Contacto con casos positivos: ¿Cuál es la proporción de personas que han estado en contacto con casos positivos y cómo afecta esto a su propio estado?
#  16. Viajes recientes: ¿Cuántas personas han viajado recientemente y cómo se relaciona esto con la propagación de la enfermedad?
# 17. Resultado de la prueba: ¿Cuál es la proporción de resultados positivos y negativos en las pruebas y cómo se relaciona esto con otros factores?
#  18. Fecha de recuperación: ¿Cómo se distribuyen las fechas de recuperación y hay algún patrón en la duración de la enfermedad?
#  19. Hospitalizado: ¿Qué proporción de personas en el dataset han sido hospitalizadas y cómo afecta esto a la gravedad y duración de la enfermedad?
#  20. Comorbilidades: ¿Cuáles son las comorbilidades más comunes entre los pacientes y cómo se relacionan estas condiciones preexistentes con el curso de la enfermedad y el resultado final?
  

# Instalar y cargar la librería 'knitr' si no lo tienes
if (!requireNamespace("knitr", quietly = TRUE)) {
  install.packages("knitr")
}
library(knitr)

