# Master: Análisis de la eficiencia de la conservación LIFE

Se recomienda descargar todo el contenido del repositorio y guardarlo como un projecto de R.

Para este proyecto se ha empleado información sobre el estado de conservación y cambios genuinos de especies catalogadas en la Lista Roja de la UICN y proyectos confinanciados por el programa LIFE. Las evaluaciones se consultaron a la API de la Lista Roja (para ello es necesario solicitar un token, https://apiv3.iucnredlist.org/api/v3/docs), los cambios genuinos se descargaron de la página web de la Lista Roja (https://www.iucnredlist.org/resources/summary-statistics) y la información referente a los proyectos LIFE (disponible en su base de datos pública, https://webgate.ec.europa.eu/life/publicWebsite/search) se consiguió empleando la herramienta de *web scraping* Octoparse (la rutina creada puede consultarse en formato otd).

### Datos brutos
Datos brutos utilizados en el proyecto. Encontramos información en diferentes formatos de la Lista Roja de la UICN y de la base de datos del programa LIFE.

### Datos procesados
Bases de datos creadas a lo largo del proyecto.

### Codigo
Seis scripts de R escritos a lo largo del proyecto. 
  - rredlist_functions_modif.R: contiene funciones del paquete rredlist y modificadas.
  - RedList.R: código para extraer información de la Lista Roja de la UICN.
  - LIFEProjects.R: código para importar y preparar la información de la base de datos LIFE (extraida con octoparse).
  - CambiosGenuinos.R: código para importar información sobre cambios genuinos en el estado de conservación de especies de la Lista Roja.
  - LIFE_RedList.R: código para combinar la información de proyectos LIFE y la Lista Roja de la UICN.
  - AnalisisDatos.R: código para el tratamiento final, análisis y representación de los datos disponibles de LIFE y la Lista Roja.
            
### Workspaces
Espacios de trabajos de R guardados con el objetivo de facilitar la accesibilidad a datasets, ante posibles dificultades.

### Plots
Conjunto de figuras exportadas a lo largo del proyecto.
