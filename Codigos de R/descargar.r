# Cargar la base de datos Titanic
data("Titanic")

# Convertir el dataset Titanic a un data frame
titanic_df <- as.data.frame(Titanic)

# Guardar la base de datos Titanic en un archivo CSV
write.csv(titanic_df, "Titanic.csv", row.names = TRUE)  # row.names = FALSE para no incluir nombres de las filas, ya que no es necesario


print("El archivo Titanic.csv se ha guardado exitosamente.")