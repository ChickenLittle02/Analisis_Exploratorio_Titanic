#!/bin/bash

# Especificar el nombre del archivo de salida
output_file="file_list.txt"

# Listar todos los archivos en el directorio actual y guardarlos en el archivo de texto
ls > "$output_file"

# Confirmar que el archivo se ha creado
echo "La lista de archivos se ha guardado en $output_file"
