
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(corrplot)
library(kableExtra)
require(pander)
library(wordcloud)

library(plotly)

library(ggiraph)

library(highcharter)

library(leaflet)

library(DT)

library(echarts4r)

library(waffle)
library(extrafont)
library(scales)
library(rmarkdown)


getwd()

# Cargar los datos desde el archivo Excel
Dset_cleam <- read_xlsx("Dset cleam.xlsx",sheet = "Datos")


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
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Edades", x = "Edad", y = "Frecuencia") +
  theme_minimal()


#  3. Género: ¿Cuál es la distribución de géneros en el dataset y cómo se clasifican los diferentes géneros?

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


#  4. País: ¿Cuántos países diferentes están representados en el dataset y cuál es el país con más casos registrados?
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


#  5. Estado: ¿Cuál es la proporción de estados (positivo, recuperado, etc.) en el dataset y cómo se determina el estado de una persona?

# Calcular la proporción de cada estado
estado_distribution <- Dset_cleam %>%
  group_by(Estado) %>%
  summarise(Casos = n()) %>%
  mutate(Proporción = Casos / sum(Casos) * 100) %>%
  arrange(desc(Proporción))

# Mostrar la distribución por estado
print(estado_distribution)

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




#Al  6. Síntomas: ¿Cuáles son los síntomas más comunes reportados en el dataset y hay algún patrón entre ellos?

# 1. Separar los síntomas en filas y mantener el estado
sintomas_separados <- Dset_cleam %>%
  select(ID, Estado, Síntomas) %>% 
  separate_rows(Síntomas, sep = ", ") 

print(sintomas_separados)
# 2. Contar la frecuencia de cada síntoma
sintomas_frecuencia <- sintomas_separados %>%
  group_by(Síntomas) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(sintomas_frecuencia)



# Crear un gráfico de barras para la frecuencia de síntomas
ggplot(sintomas_frecuencia, aes(x = reorder(Síntomas, -Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "darkblue") +
  labs(title = "Síntomas Más Comunes Reportados",
       x = "Síntomas",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x
  geom_text(aes(label = Frecuencia), vjust = -0.5, color = "darkblue")  # Añadir etiquetas con frecuencias

# Crear una tabla de contingencia entre síntomas y el estado

sintomas_estado <- table(sintomas_separados$Síntomas, sintomas_separados$Estado)

print(sintomas_estado)

# 5. Visualizar la relación de síntomas con el estado 
sintomas_estado_df <- as.data.frame(sintomas_estado) %>%
  filter(Freq > 0)

ggplot(sintomas_estado_df, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relación entre Síntomas y Estado",
       x = "Síntomas",
       y = "Frecuencia") +
  theme_minimal() +
  coord_flip() 


# 1. Separar los síntomas en filas y mantener el estado
sintomas_separados <- Dset_cleam %>%
  select(ID, Estado, Síntomas) %>% 
  separate_rows(Síntomas, sep = ", ") 

print(sintomas_separados)
# 2. Contar la frecuencia de cada síntoma
sintomas_frecuencia <- sintomas_separados %>%
  group_by(Síntomas) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(sintomas_frecuencia)


##
ggplot(sintomas_frecuencia, aes(x = reorder(Síntomas, Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "darkblue") +
  labs(title = "Síntomas Más Comunes Reportados",
       x = "Síntomas",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +  # Ajustar las etiquetas del eje y
  geom_text(aes(label = Frecuencia), hjust = -0.2, color = "darkblue") +  # Ajustar posición de etiquetas
  coord_flip()  # Cambiar el gráfico a horizontal



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

#  8. Temperatura: ¿Cuál es la distribución de las temperaturas registradas y hay alguna temperatura que sea anormalmente alta o baja?
# Resumen estadístico de la variable Temperatura
summary(Dset_cleam$Temperatura)

# graficos de Dispersion
ggplot(Dset_cleam, aes(x = seq_along(Temperatura), y = Temperatura)) +
  geom_point(color = "blue") +
  labs(title = "Gráfico de Puntos de Temperaturas Registradas",
       x = "Personas",
       y = "Temperatura (°C)") +
  theme_minimal()


# Histograma para visualizar la distribución de las temperaturas
ggplot(Dset_cleam, aes(x = Temperatura)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Distribución de las temperaturas", x = "Temperatura (°C)", y = "Frecuencia") +
  theme_minimal()


#  9. Presión arterial: ¿Cómo varía la presión arterial registrada y hay alguna tendencia o patrón entre los valores?

# Separar presión arterial en dos columnas: Sistólica y Diastólica
Dset_cleam <- Dset_cleam %>%
  separate( `Presión arterial`, into = c("Sistólica", "Diastólica"), sep = "/", convert = TRUE)

# Verificar los cambios

head(Dset_cleam)

# Gráficos de densidad para presión arterial sistólica y diastólica
ggplot(Dset_cleam, aes(x = Sistólica)) +
  geom_density(fill = "lightblue") +
  labs(title = "Distribución de la presión arterial sistólica", x = "Presión sistólica (mmHg)", y = "Densidad") +
  theme_minimal()

ggplot(Dset_cleam, aes(x = Diastólica)) +
  geom_density(fill = "lightgreen") +
  labs(title = "Distribución de la presión arterial diastólica", x = "Presión diastólica (mmHg)", y = "Densidad") +
  theme_minimal()

# Gráfico de dispersión para analizar la relación entre edad y presión arterial sistólica
ggplot(Dset_cleam, aes(x = Edad, y = Sistólica)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre edad y presión arterial sistólica", x = "Edad", y = "Presión sistólica (mmHg)") +
  theme_minimal()

#  10. Nivel de oxígeno: ¿Cuál es el rango típico de niveles de oxígeno y cómo se relaciona esto con el estado de salud de los pacientes?

# Resumen estadístico
summary(Dset_cleam$`Nivel de oxígeno`)




# Crear un boxplot para ver la distribución del nivel de oxígeno por estado de salud
ggplot(Dset_cleam, aes(x = Estado, y = `Nivel de oxígeno`, fill = Estado)) +
  geom_boxplot() +
  labs(title = "Distribución del Nivel de Oxígeno por Estado de Salud",
       x = "Estado de Salud",
       y = "Nivel de Oxígeno") +
  theme_minimal() +
  theme(legend.position = "none")

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


# Gráfico de barras agrupadas para mostrar tratamientos según el estado de los pacientes
ggplot(tratamiento_estado, aes(x = Estado, y = Frecuencia, fill = Tratamiento)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tratamientos según el estado de los pacientes", x = "Estado", y = "Frecuencia") +
  theme_minimal()


#  12. Vacunado: ¿Qué proporción de la población en el dataset está vacunada y cómo se relaciona esto con el estado de salud y el curso de la enfermedad?

# Calcular la proporción de personas vacunadas
proporcion_vacunado <- Dset_cleam %>%
  group_by(Vacunado) %>%
  summarise(Frecuencia = n()) %>%
  mutate(Proporción = Frecuencia / sum(Frecuencia))

# Mostrar la proporción
print(proporcion_vacunado)


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


# Relación entre el estado de salud y la vacunación
vacunacion_estado <- Dset_cleam %>%
  group_by(Vacunado, Estado) %>%
  summarise(Frecuencia = n(), .groups = "drop") %>%
  arrange(desc(Frecuencia))

print(vacunacion_estado)
# Gráfico de barras agrupadas para mostrar el estado de salud según vacunación
ggplot(vacunacion_estado, aes(x = Estado, y = Frecuencia, fill = Vacunado)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relación entre estado de salud y vacunación", x = "Estado de salud", y = "Frecuencia") +
  theme_minimal()
labels = scales::percent_format(scale = 1) # Formato de porcentaje

# Relación entre vacunación y hospitalización
vacunacion_hospitalizado <- Dset_cleam %>%
  group_by(Vacunado, Hospitalizado) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

print(vacunacion_hospitalizado)


#
# Gráfico de barras agrupadas para mostrar la relación entre vacunación y hospitalización
ggplot(vacunacion_hospitalizado, aes(x = Hospitalizado, y = Frecuencia, fill = Vacunado)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Relación entre Vacunación y Hospitalización",
       x = "Hospitalizado",
       y = "Frecuencia") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"),  # Colores suaves
                    labels = c("Vacunado", "No Vacunado")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_blank(),  # Sin título en la leyenda
    legend.position = "top"
  )



#  13. Fecha de vacunación: ¿Cómo se distribuyen las fechas de vacunación y hay alguna relación entre la fecha de vacunación y el estado posterior del paciente?

# Filtrar datos sin fecha de vacunación
fecha_vacunacion <- Dset_cleam %>%
  filter(!is.na(`Fecha de vacunación`))


# Gráfico de densidad para la distribución de fechas de vacunación
ggplot(fecha_vacunacion, aes(x = `Fecha de vacunación`)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Distribución de Fechas de Vacunación",
       x = "Fecha de Vacunación",
       y = "Densidad") +
  theme_minimal()


# Agrupar por fecha de vacunación y resultado de la prueba
vacunacion_resultado <- Dset_cleam %>%
  group_by(`Fecha de vacunación`, `Resultado de la prueba`) %>%
  summarise(Frecuencia = n(), .groups = "drop") %>%
  filter(!is.na(`Fecha de vacunación`) & !is.na(`Resultado de la prueba`))  # Filtrar NA

# Gráfico de líneas para la relación entre fecha de vacunación y resultado de la prueba
ggplot(vacunacion_resultado, aes(x = `Fecha de vacunación`, y = Frecuencia, color = `Resultado de la prueba`)) +
  geom_line() +
  geom_point() +
  labs(title = "Relación entre Fecha de Vacunación y Resultado de la Prueba",
       x = "Fecha de Vacunación",
       y = "Frecuencia") +
  theme_minimal()



#  14. Vacuna: ¿Qué vacunas son más comunes en el dataset y hay alguna diferencia en la efectividad entre ellas?

# 1. Contar las vacunas más comunes
vacunas_comunes <- Dset_cleam %>%
  group_by(Vacuna) %>%
  summarise(Conteo = n()) %>%
  arrange(desc(Conteo))

print(vacunas_comunes)
# 2. Relación entre Vacuna y Estado (efectividad aproximada)
vacuna_efectividad <- Dset_cleam %>%
  filter(!is.na(Vacuna)) %>%
  group_by(Vacuna, Estado) %>%
  summarise(Conteo = n()) %>%
  arrange(Vacuna, Estado)

print(vacuna_efectividad)

# Visualizar la relación entre Vacuna y Estado
ggplot(vacuna_efectividad, aes(x = Vacuna, y = Conteo, fill = Estado)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Efectividad de las Vacunas por Estado de Salud", x = "Vacuna", y = "Número de Pacientes") +
  theme_minimal()


#  15. Contacto con casos positivos: ¿Cuál es la proporción de personas que han estado en contacto con casos positivos y cómo afecta esto a su propio estado?

# 1. Proporción de personas que han estado en contacto con casos positivos
contacto_proporcion <- Dset_cleam %>%
  group_by(`Contacto con casos positivos`) %>%
  summarise(Conteo = n()) %>%
  mutate(Proporcion = Conteo / sum(Conteo) * 100)

print(contacto_proporcion)

# 2. Relación entre contacto con casos positivos y estado de salud
contacto_estado <- Dset_cleam %>%
  group_by(`Contacto con casos positivos`, Estado) %>%
  summarise(Conteo = n()) %>%
  arrange(desc(`Contacto con casos positivos`))

print(contacto_estado)

# Visualizar la relación entre contacto y estado de salud
ggplot(contacto_estado, aes(x = `Contacto con casos positivos`, y = Conteo, fill = Estado)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Efecto del Contacto con Casos Positivos en el Estado de Salud", 
       x = "Contacto con Casos Positivos", y = "Número de Pacientes") +
  theme_minimal()


#  16. Viajes recientes: ¿Cuántas personas han viajado recientemente y cómo se relaciona esto con la propagación de la enfermedad?

# 1. Proporción de personas que han viajado recientemente
viajes_proporcion <- Dset_cleam %>%
  group_by(`Viajes recientes`) %>%
  summarise(Conteo = n()) %>%
  mutate(Proporcion = Conteo / sum(Conteo) * 100)

print(viajes_proporcion)
# Crear la gráfica de torta interactiva con Plotly
fig <- plot_ly(viajes_proporcion, 
               labels = ~`Viajes recientes`, 
               values = ~Conteo, 
               type = 'pie',  # Definir tipo de gráfico como 'pie'
               textinfo = 'label+percent',  # Mostrar etiquetas y porcentaje
               hoverinfo = 'text',  # Información que aparece al pasar el cursor
               text = ~paste("Viajes recientes: ", `Viajes recientes`, 
                             "<br>Número de personas: ", Conteo, 
                             "<br>Porcentaje: ", round(Proporcion, 1), "%"),
               marker = list(line = list(color = '#FFFFFF', width = 2))) %>%
  
  layout(title = 'Proporción de Personas con Viajes Recientes',
         showlegend = TRUE)  # Mostrar leyenda

# Mostrar la gráfica
fig


# 2. Relación entre viajes recientes y estado de salud
viajes_estado <- Dset_cleam %>%
  group_by(`Viajes recientes`, Estado) %>%
  summarise(Conteo = n()) %>%
  arrange(desc(`Viajes recientes`))

print(viajes_estado)

# Grafico de barras para la proporción de personas con viajes recientes
ggplot(viajes_proporcion, aes(x = `Viajes recientes`, y = Proporcion, fill = `Viajes recientes`)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Proporción de Personas con Viajes Recientes", 
       x = "Viajes Recientes", 
       y = "Proporción (%)") +
  theme_minimal()


# Visualizar la relación entre viajes y estado de salud
ggplot(viajes_estado, aes(x = `Viajes recientes`, y = Conteo, fill = Estado)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Efecto de los Viajes Recientes en el Estado de Salud", 
       x = "Viajes Recientes", y = "Número de Pacientes") +
  theme_minimal()



#  17. Resultado de la prueba: ¿Cuál es la proporción de resultados positivos y negativos en las pruebas y cómo se relaciona esto con otros factores?
# 1. Proporción de resultados de la prueba
resultado_proporcion <- Dset_cleam %>%
  group_by(`Resultado de la prueba`) %>%
  summarise(Conteo = n()) %>%
  mutate(Proporcion = Conteo / sum(Conteo) * 100)

# Mostrar la proporción
print(resultado_proporcion)

# Relación entre resultado de la prueba y estado de salud
resultado_estado <- Dset_cleam %>%
  group_by(`Resultado de la prueba`, Estado) %>%
  summarise(Conteo = n()) %>%
  arrange(desc(`Resultado de la prueba`))

# Mostrar la tabla
print(resultado_estado)

# Relación entre resultado de la prueba y vacunación
resultado_vacunacion <- Dset_cleam %>%
  group_by(`Resultado de la prueba`, Vacunado) %>%
  summarise(Conteo = n()) %>%
  arrange(desc(`Resultado de la prueba`))

# Mostrar la tabla
print(resultado_vacunacion)

# Relación entre resultado de la prueba y contacto con casos positivos
resultado_contacto <- Dset_cleam %>%
  group_by(`Resultado de la prueba`, `Contacto con casos positivos`) %>%
  summarise(Conteo = n()) %>%
  arrange(desc(`Resultado de la prueba`))

# Visualización de la relación entre resultado de la prueba y contacto con casos positivos
ggplot(resultado_contacto, aes(x = `Resultado de la prueba`, y = Conteo, fill = `Contacto con casos positivos`)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Relación entre Resultado de la Prueba y Contacto con Casos Positivos", 
       x = "Resultado de la Prueba", y = "Número de Pacientes") +
  theme_minimal()

#  18. Fecha de recuperación: ¿Cómo se distribuyen las fechas de recuperación y hay algún patrón en la duración de la enfermedad?


# Calcular la duración de la enfermedad (en días)
Dset_cleam <- Dset_cleam %>%
  mutate(Duracion_enfermedad = as.numeric(difftime(`Fecha de recuperación`, `Fecha de diagnóstico`, units = "days")))

# Mostrar las primeras filas con la duración
head(Dset_cleam %>% select(`Fecha de diagnóstico`, `Fecha de recuperación`, Duracion_enfermedad))

# Histograma de la duración de la enfermedad
ggplot(Dset_cleam, aes(x = Duracion_enfermedad)) +
  geom_histogram(binwidth = 3, fill = "coral", color = "black") +
  labs(title = "Distribución de la Duración de la Enfermedad", 
       x = "Duración de la Enfermedad (días)", y = "Número de Pacientes") +
  theme_minimal()

# Relación entre la edad y la duración de la enfermedad
ggplot(Dset_cleam, aes(x = Edad, y = Duracion_enfermedad)) +
  geom_point(color = "purple", size = 3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre la Edad y la Duración de la Enfermedad", 
       x = "Edad", y = "Duración de la Enfermedad (días)") +
  theme_minimal()


#  19. Hospitalizado: ¿Qué proporción de personas en el dataset han sido hospitalizadas y cómo afecta esto a la gravedad y duración de la enfermedad?
# Proporción de personas hospitalizadas
hospitalizados_proporcion <- Dset_cleam %>%
  group_by(Hospitalizado) %>%
  summarise(Conteo = n()) %>%
  mutate(Proporcion = Conteo / sum(Conteo) * 100)

# Mostrar la proporción de personas hospitalizadas
print(hospitalizados_proporcion)
# Relación entre hospitalización y estado de salud
hospitalizacion_estado <- Dset_cleam %>%
  group_by(Hospitalizado, Estado) %>%
  summarise(Conteo = n()) %>%
  arrange(desc(Hospitalizado))

# Mostrar la relación entre hospitalización y estado de salud
print(hospitalizacion_estado)

# Visualización de la relación entre hospitalización y estado de salud
ggplot(hospitalizacion_estado, aes(x = Hospitalizado, y = Conteo, fill = Estado)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Relación entre la Hospitalización y el Estado de Salud", 
       x = "Hospitalizado", y = "Número de Pacientes") +
  theme_minimal()

# Relación entre hospitalización y duración de la enfermedad
ggplot(Dset_cleam, aes(x = Hospitalizado, y = Duracion_enfermedad, fill = Hospitalizado)) +
  geom_boxplot(color = "black") +
  labs(title = "Duración de la Enfermedad en Pacientes Hospitalizados vs No Hospitalizados", 
       x = "Hospitalizado", y = "Duración de la Enfermedad (días)") +
  theme_minimal()


#  20. Comorbilidades: ¿Cuáles son las comorbilidades más comunes entre los pacientes y cómo se relacionan estas condiciones preexistentes con el curso de la enfermedad y el resultado final?

# Contar las comorbilidades más comunes
comorbilidades_comunes <- Dset_cleam %>%
  group_by(Comorbilidades) %>%
  summarise(Conteo = n()) %>%
  arrange(desc(Conteo))

# Mostrar las comorbilidades más comunes
print(comorbilidades_comunes)

# Relación entre comorbilidades y el estado de salud
comorbilidades_estado <- Dset_cleam %>%
  group_by(Comorbilidades, Estado) %>%
  summarise(Conteo = n()) %>%
  arrange(desc(Comorbilidades))

# Mostrar la relación entre comorbilidades y estado de salud
print(comorbilidades_estado)

# Visualizar la relación entre comorbilidades y el estado de salud
ggplot(comorbilidades_estado, aes(x = Comorbilidades, y = Conteo, fill = Estado)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Relación entre Comorbilidades y Estado de Salud", 
       x = "Comorbilidades", y = "Número de Pacientes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




 