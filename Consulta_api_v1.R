# Importar librerias necesarias

library(httr)
library(jsonlite)
library(dplyr)
library(dotenv)

# Cargar la clave API desde un archivo .env
# Nota: Este script asume que posees un archivo .env con las credenciales de tu API. Esto es por motivos de
# seguridad de tu clave privada. Sin embargo, puedes modificar el codigo facilmente si quieres introducir directamente
# la clave en el script.

dotenv::load_dot_env()

api_key <- Sys.getenv("API_KEY_DATOS_FINANCIEROS")  #Sustituir con tu API Key


# Establecer headers para el request

headers <- add_headers(
  'Ocp-Apim-Subscription-Key' = api_key,
  'User-Agent' = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36"
)

# Establecer la URL base (o raiz) a partir de la cual se construyen los endpoints y las diferentes consultas

base_url <- "https://apis.sb.gob.do/estadisticas/v2/"

# Definimos la funcion para consultar los datos

obtener_datos_financieros_rd <- function(end_point, params) {
  
  ############################### DOC STRING #####################################
  # Obtiene datos financieros desde una API REST y devuelve un DataFrame de pandas.
  # 
  # Esta función realiza múltiples solicitudes a un endpoint de la API de 
  # datos públicos de la Superintendencia de Bancos de la República Dominicana 
  # para recuperar datos financieros, paginando a través de los resultados. 
  # Los datos se recopilan en un DataFrame de pandas que se devuelve al finalizar.
  # 
  # Parámetros
  # ----------
  #   end_point : str
  # El endpoint de la API que se utilizará para la solicitud de datos.
  # 
  # params : str
  # Filtros a ser aplicados en la petición al endpoint
  # 
  # Returns
  # -------
  #   data.frame
  # Un DataFrame que contiene los datos financieros obtenidos 
  # del API.
  # 
  # 
  # Ejemplo de uso
  # ------
  # params <- list(
  # periodoInicial = "2022-01",  # Fecha Inicial
  # periodoFinal = "2022-02",
  # tipoEntidad = "BM",
  # paginas = 1,
  # registros = 150  
  # )
  #   obtener_datos_financieros_rd('indicadores/morosidad-estresada',params)
  # 
  # Notes
  # -----
  #   - La función gestiona la paginación automáticamente, asegurándose de que 
  # se recuperen todos los registros disponibles en el rango de fechas especificado.
  # - Se asume que la respuesta de la API incluye un encabezado `x-pagination` 
  # con metadatos que indican si hay más páginas de datos disponibles.
  
  ##################################################################################
  
  next_page <- TRUE
  registros_list <- list()
  
  while (next_page) {
    # Concatenar la base de URL con el endpoint especificado
    url <- paste0(base_url, end_point)
    
    # Añadir los parámetros de consulta
    url <- modify_url(url, query = params)
    
    tryCatch({
      # Hacer la solicitud
      response <- GET(url, headers)
      
      # Verificar si la solicitud fue exitosa
      stop_for_status(response)
      
      # Parsear la respuesta JSON
      datos <- fromJSON(content(response, "text"), flatten = TRUE)
      
      # Agregar los datos a una lista de DataFrames
      registros_list <- append(registros_list, list(as.data.frame(datos)))
      
      # Obtener metadatos de la respuesta
      metadatos <- fromJSON(response$headers[['x-pagination']])
      next_page <- metadatos$HasNext
      total_pages <- metadatos$TotalPages
      total_record <- metadatos$TotalRecords
      
      # Imprimir la página actual y el total de páginas
      print(paste('Página:', params['paginas'], '/', total_pages))
      
      # Incrementar 1 para la siguiente página
      params['paginas'] <- as.numeric(params['paginas']) + 1
      
    }, error = function(e) {
      print(paste("Ha ocurrido un error:", e))
      stop(e$message) 
      # next_page <- FALSE
    })
  }
  
  # Combinar los registros en un solo DataFrame
  registros_finales <- bind_rows(registros_list)
  
  return(registros_finales)
}



# Definir los parámetros
params <- list(
  periodoInicial = "2022-01",  # Fecha Inicial
  periodoFinal = "2022-02",
  tipoEntidad = "BM",
  paginas = 1,
  registros = 100  
)

# Llamar a la función
resultados <- obtener_datos_financieros_rd("indicadores/financieros", params)

# Guardar datos en .csv
write.csv(resultados, "consulta_api.csv", row.names = FALSE)

print(resultados)


