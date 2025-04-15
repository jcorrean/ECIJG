# Pre-Processing
library(readtext)
library(pdftools)
library(dplyr)
library(stringr)
#Investigadores <- readtext("Investigadores/")
#Investigadores$doc_id <- gsub("[^0-9-]", "", Investigadores$doc_id)

directorio <- "Investigadores/"

# Obtener la lista de archivos PDF en el directorio
archivos_pdf <- list.files(directorio, pattern = "\\.pdf$", full.names = TRUE)

# Función para extraer información de la primera página del PDF
extraer_info_investigador <- function(archivo_pdf) {
  texto <- pdftools::pdf_text(archivo_pdf)[1]
  texto_lineas <- strsplit(texto, "\n")[[1]]
  
  # 1. Limpiar todas las líneas de espacios en blanco al inicio y al final
  texto_lineas <- trimws(texto_lineas)
  
  # 2. Eliminar líneas vacías o casi vacías
  texto_lineas <- texto_lineas[nchar(texto_lineas) > 2]
  
  # 3. Unir las líneas limpias en un solo string para facilitar la búsqueda con regex
  texto_completo <- paste(texto_lineas, collapse = " ")
  
  # 4. Expresión regular para encontrar "Nombre en citaciones" seguido del nombre
  # y detenerse antes del siguiente campo (usando una palabra clave como "Nacionalidad" como delimitador)
  match <- str_extract(texto_completo, "(Nombre en citaciones)\\s*([^\\n\\r]*)\\s*(Nacionalidad|Sexo|Formación Académica|Experiencia profesional|Áreas de actuación|Idiomas|Líneas de investigación)")
  
  if (!is.na(match)) {
    # Extraer el texto después de "Nombre en citaciones"
    nombre_citaciones <- sub("Nombre en citaciones\\s*", "", match)
    # Eliminar el texto a partir del delimitador
    nombre_citaciones <- sub("\\s*(Nacionalidad|Sexo|Formación Académica|Experiencia profesional|Áreas de actuación|Idiomas|Líneas de investigación).*", "", nombre_citaciones)
    nombre_citaciones <- trimws(nombre_citaciones) # Limpiar el resultado final
    return(nombre_citaciones)
  } else {
    return(NA)
  }
}

# Leer los PDFs con readtext
Investigadores <- readtext(archivos_pdf)

# Limpiar los doc_id
Investigadores$doc_id <- gsub("[^0-9-]", "", Investigadores$doc_id)

# Aplicar la función para extraer el nombre del investigador a cada archivo
nombres_investigadores <- sapply(archivos_pdf, extraer_info_investigador)

# Crear un data frame con los nombres y los doc_id
nombres_df <- data.frame(doc_id = gsub("[^0-9-]", "", basename(archivos_pdf)), Investigador = nombres_investigadores)

# Fusionar los nombres con el data frame original `Investigadores` usando `doc_id` como clave
Investigadores <- left_join(Investigadores, nombres_df, by = "doc_id")

# Ahora el data frame `Investigadores` tiene una columna adicional llamada "Investigador"
print(Investigadores)


