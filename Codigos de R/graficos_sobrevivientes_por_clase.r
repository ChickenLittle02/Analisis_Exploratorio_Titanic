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

# 1. Convertir la variable 'Age' a numérica
# Asumimos que 'Child' = 10 años y 'Adult' = 30 años como ejemplo
titanic_data$Age <- ifelse(titanic_data$Age == "Child", 10, 
                           ifelse(titanic_data$Age == "Adult", 30, NA))

# Convertir la variable 'Age' a numérica
titanic_data$Age <- as.numeric(titanic_data$Age)

## Chi -Cuadrado 
# Summarize the data to get total frequencies for each class and survival status
summary_data <- titanic_data %>%
  group_by(Class, Survived) %>%
  summarise(Total = sum(Freq), .groups = 'drop')

# Create a histogram of the frequencies
ggplot(summary_data, aes(x = Class, y = Total, fill = Survived)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Survival by Class on the Titanic", x = "Class", y = "Total Frequency") +
  scale_fill_manual(values = c("red", "green"), name = "Survived", labels = c("No", "Yes")) +
  theme_minimal()

#Correlacion 
# Convertir variables categóricas a numéricas
titanic_data$Class_num <- as.numeric(factor(titanic_data$Class, levels = c("1st", "2nd", "3rd", "Crew")))
titanic_data$Sex_num <- as.numeric(factor(titanic_data$Sex, levels = c("Male", "Female")))
titanic_data$Survived_num <- as.numeric(factor(titanic_data$Survived, levels = c("No", "Yes")))

# Convertir la columna Age a numérica
titanic_data$Age_num <- as.numeric(factor(titanic_data$Age, levels = c("Child", "Adult"), labels = c(0, 1)))

# Calcular la matriz de correlación
cor_matrix <- cor(titanic_data[, c("Class_num", "Sex_num", "Age_num", "Survived_num", "Freq")], use = "complete.obs")

# Convertir la matriz de correlación a formato largo para ggplot
cor_melted <- melt(cor_matrix)

# Crear el heatmap
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       name="Correlación") +
  labs(title = "Matriz de Correlación del Conjunto de Datos del Titanic", 
       x = "Variables", 
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
