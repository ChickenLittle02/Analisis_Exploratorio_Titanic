# Cargar la base de datos Titanic
titanic <- read.csv("Titanic.csv")

# Mostrar un resumen de la base de datos para asegurarte de que se cargó correctamente
summary(titanic)

library(ggplot2)

# Filtrar datos para obtener solo niños varones
ninos_varones <- subset(titanic, Sex == "Male" & Age == "Child")

# Crear un gráfico de barras de niños varones sobrevivientes por clase
grafico <- ggplot(ninos_varones, aes(x = factor(Class), y = Freq, fill = factor(Survived))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Cantidad de Niños Varones Sobrevivientes por Clase",
    x = "Clase",
    y = "Cantidad de Niños Varones",
    fill = "Sobreviviente"
  ) +
  theme_minimal()

# Guardar el gráfico como una imagen PNG
ggsave("ninos_varones_sobrevivientes_por_clase.png", plot = grafico, width = 8, height = 6)


# Crear un data frame con la información para el gráfico
data_grafico <- aggregate(Freq ~ Class + Survived, data = ninos_varones, sum)

# Guardar el data frame como un archivo CSV
write.csv(data_grafico, "ninos_varones_sobrevivientes_por_clase.csv", row.names = FALSE)





# Filtrar datos para obtener solo hombres adultos
hombres_adultos <- subset(titanic, Sex == "Male" & Age == "Adult")

# Crear un gráfico de barras de hombres adultos sobrevivientes por clase
grafico_hombres_adultos <- ggplot(hombres_adultos, aes(x = factor(Class), y = Freq, fill = factor(Survived))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Cantidad de Hombres Adultos Sobrevivientes por Clase",
    x = "Clase",
    y = "Cantidad de Hombres Adultos",
    fill = "Sobreviviente"
  ) +
  theme_minimal()

# Mostrar el gráfico
print(grafico_hombres_adultos)

# Guardar el gráfico como una imagen PNG
ggsave("hombres_adultos_sobrevivientes_por_clase.png", plot = grafico_hombres_adultos, width = 8, height = 6)


# Crear un data frame con la información para el gráfico
data_grafico <- aggregate(Freq ~ Class + Survived, data = hombres_adultos, sum)

# Guardar el data frame como un archivo CSV
write.csv(data_grafico, "hombres_adultos_sobrevivientes_por_clase.csv", row.names = FALSE)








# Filtrar datos para obtener solo mujeres adultas
mujeres_adultas <- subset(titanic, Sex == "Female" & Age == "Adult")

# Crear un gráfico de barras de mujeres adultas sobrevivientes por clase
grafico_mujeres_adultas <- ggplot(mujeres_adultas, aes(x = Class, y = Freq, fill = factor(Survived))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Cantidad de Mujeres Adultas Sobrevivientes por Clase",
    x = "Clase",
    y = "Cantidad de Mujeres Adultas",
    fill = "Sobreviviente"
  ) +
  theme_minimal()

# Mostrar el gráfico
print(grafico_mujeres_adultas)

# Guardar el gráfico como una imagen PNG
ggsave("mujeres_adultas_sobrevivientes_por_clase.png", plot = grafico_mujeres_adultas, width = 8, height = 6)

# Crear un data frame con la información para el gráfico
data_grafico <- aggregate(Freq ~ Class + Survived, data = mujeres_adultas, sum)

# Guardar el data frame como un archivo CSV
write.csv(data_grafico, "mujeres_adultas_sobrevivientes_por_clase.csv", row.names = FALSE)








# Filtrar datos para obtener solo niñas
ninas <- subset(titanic, Sex == "Female" & Age == "Child")

# Crear un gráfico de barras de niñas sobrevivientes por clase
grafico_ninas <- ggplot(ninas, aes(x = Class, y = Freq, fill = factor(Survived))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Cantidad de Niñas Sobrevivientes por Clase",
    x = "Clase",
    y = "Cantidad de Niñas",
    fill = "Sobreviviente"
  ) +
  theme_minimal()

# Mostrar el gráfico
print(grafico_ninas)

# Guardar el gráfico como una imagen PNG
ggsave("ninas_sobrevivientes_por_clase.png", plot = grafico_ninas, width = 8, height = 6)


# Crear un data frame con la información para el gráfico
data_grafico <- aggregate(Freq ~ Class + Survived, data = ninas, sum)

# Guardar el data frame como un archivo CSV
write.csv(data_grafico, "ninas_sobrevivientes_por_clase.csv", row.names = FALSE)