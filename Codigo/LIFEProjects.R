##
## IMPORTAR Y PREPARAR INFORMACION DE LA BASE DE DATOS LIFE (extraida con octoparse)
## Fecha: 03/08/2022
## Autor: Maite Telletxea Martinez
## 
## Inputs:
##          - LIFE0.xlsx (Base de datos de proyectos LIFE trabajada primero en excel)
## Outputs: 
##          - LIFE.csv (Dataset más completo y definitivo)
##          - LIFE_byspecies.csv (Base de datos de inversion para cada especies)
##          - LIFE_byspeciesproject.csv (Base de datos de inversion por proyecto y especie)
##

#### Packages ####
library(readxl)
library(tidyverse)
library(lubridate)

############ PRIMERA PARTE ############

##### Importar excel con informacion de todos los proyectos LIFE #####
LIFE0 <- read_excel("Datos_procesados/LIFE0.xlsx")

##### Corregir columnas de Project Description #####
for (nregistros in 1:nrow(LIFE0)) {
  
  # Comprobamos errores en la columna de Results (si no es NA)
  if (!is.na(LIFE0[nregistros,]$Results)) {
    
    if (str_detect(LIFE0[nregistros,]$Results, pattern = "BACKGROUND")) {
      
      if (is.na(LIFE0[nregistros,]$Background)) {
        # Aparece el titulo de Background en Results y no se han extraído datos en Background
        # Indica que en Results se ha extraido el Background, por lo que pasamos la información a Background y la eliminamos de Results
        LIFE0[nregistros,]$Background <- LIFE0[nregistros,]$Results
        LIFE0[nregistros,]$Background <- str_remove(LIFE0[nregistros,]$Background, pattern = "(.*)BACKGROUND(\\s*)")
        LIFE0[nregistros,]$Results <- NA

      } else {
        # Aparece el titulo de Background en Results pero se han extraido datos en la columna de Background
        # Indica que en Results se han extraido Background y Objectives, por lo que pasamos la informacion de Objectives a su columna y limpiamos Results
        LIFE0[nregistros,]$Objectives <- LIFE0[nregistros,]$Results
        LIFE0[nregistros,]$Objectives <- str_remove(LIFE0[nregistros,]$Objectives, pattern = "(.*)OBJECTIVES(:?)(\\s*)")
        LIFE0[nregistros,]$Results <- NA }
      
    }  else if (str_detect(LIFE0[nregistros,]$Results, pattern = "OBJECTIVES")) {
      # Aparece solo el titulo de Objectives en Results
      # Indica que en Results se han extraido los Objectives, por lo que pasamos la información a Objectives y la eliminamos de Results
      LIFE0[nregistros,]$Objectives <- LIFE0[nregistros,]$Results
      LIFE0[nregistros,]$Objectives <- str_remove(LIFE0[nregistros,]$Objectives, pattern = "(.*)OBJECTIVES(\\s*)")
      LIFE0[nregistros,]$Results <- NA }
  }
  
  # Comprobamos errores en la columna de Background  (si no es NA)
  if (!is.na(LIFE0[nregistros,]$Background)) {
    # Si hay títulos no deseados, los eliminamos
    if (str_detect(LIFE0[nregistros,]$Background, pattern = "¿(\\s*)BACKGROUND(\\s*)")) {
      LIFE0[nregistros,]$Background <- str_remove_all(LIFE0[nregistros,]$Background, pattern = "¿(\\s*)BACKGROUND(\\s*)|(\\s*)¿ PROJECT") }
  }
  
  # Comprobamos errores en la columna de Objectives  (si no es NA)
  if (!is.na(LIFE0[nregistros,]$Objectives)) {
    #Si aparece el título de Background en Objectives, extraemos la información del Background y corregimos Objectives
    if (str_detect(LIFE0[nregistros,]$Objectives, pattern = "Background")) {
      LIFE0[nregistros,]$Background <- str_extract(LIFE0[nregistros,]$Objectives, pattern = "(?<=Background:)(.*)(?=Objectives)")
      LIFE0[nregistros,]$Objectives <- str_remove(LIFE0[nregistros,]$Objectives, pattern = "Background(.*)Objectives(:?)(\\s*)") }
  }
}

##### Recortar espacios y saltos de línea innecesarios #####
LIFE0[2:5] <- lapply(LIFE0[2:5], function(x) str_replace_all(x, pattern = "\\s{2,}", replacement = " "))
LIFE0[12:16] <- lapply(LIFE0[12:16], function(x) str_replace_all(x, pattern = "\r\n\r\n", replacement = "\n"))

##### Crear dataset de proyectos LIFE previo al definitivo #####
LIFE1 <- LIFE0 %>%
  # Separar especies
  mutate(SpeciesSplit = str_split(Species, pattern = "\n"))

# Eliminar el ultimo elemento de Species y posibles nombres científicos duplicados
LIFE1$SpeciesSplit <- lapply(LIFE1$SpeciesSplit, FUN = function(x) unique(head(x,-1)))

# Contar numero de especies a las que va dirigido cada proyecto
LIFE1$number_sp <- NA
for (nregistros in 1:nrow(LIFE1)) {
  LIFE1$number_sp[nregistros] <- length(LIFE1$SpeciesSplit[[nregistros]]) }

LIFE2 <- LIFE1 %>%
  # Numero de especies a las que va dirigido el proyecto
  mutate(number_sp = ifelse(str_detect(Species, pattern = "None or non applicable"), 0, number_sp)) %>%
  # Ponderar el presupuesto, si el proyecto esta dirigido a la conservacion de n especies se considerara que se ha destinado la n-esima parte del presupuesto a cada especie
  mutate(TotalBudgetWeighted = ifelse(is.infinite(round(TotalBudget/number_sp)), TotalBudget, round(TotalBudget/number_sp)),
         EUContributionWeighted = ifelse(is.infinite(round(EUContribution/number_sp)), EUContribution, round(EUContribution/number_sp))) %>%
  # Diferenciar entre proyecto dirigido a multiples especies y una unica especie (puede ser interesante, efectividad de multispecies plans vs. single-species plans)
  mutate(SingleSpeciesProject = ifelse(number_sp == 1, TRUE, FALSE))


##### Crear el dataset de proyectos LIFE definitivo #####
LIFE <- LIFE2 %>%
  # Desahacerse de las columnas number_sp y SpeciesSplit
  mutate(Species = SpeciesSplit) %>% 
  select(-c(number_sp,SpeciesSplit)) %>%
  # unnest convierte la columna SpeciesSplit (columna lista, tras str_split). Hace que cada elemento de la lista tenga su propia fila
  unnest(Species, keep_empty = TRUE)

# Antes de guardar, maximizar coincidencias
LIFE <- max_names_coincidences(LIFE)

##### Guardar el dataset de la informacion sobre los proyectos LIFE definitivo y completo #####
write.csv(LIFE,"./Datos_procesados/LIFE.csv", row.names = FALSE)

############ SEGUNDA PARTE ############

##### Crear 2 datasets complementarios para el analisis #####

## Dataset de la inversion dirigida a cada especie
LIFE_byspecies <- LIFE %>%
  # Agrupar por especies
  group_by(Species) %>%
  # Sumar presupuestos dirigidos a una misma especie
  summarise(ConservationStart = min(year(StartDate)),
            NumProjects = n(),
            SumTotalBudgetWeighted = sum(TotalBudgetWeighted),
            SumEUContributionWeighted = sum(EUContributionWeighted)) %>%
  # Ordenar de mayor a menor el presupuesto total
  arrange(desc(SumTotalBudgetWeighted)) %>%
  # Filtrar especies
  filter(Species != "None or non applicable") %>% 
  # Convertir a dataframe
  as.data.frame()

# Guardamos el dataset
write.csv(LIFE_byspecies,"./Datos_procesados/LIFE_byspecies.csv", row.names = FALSE)

## Dataset de inversion realizada por cada proyecto y especie
LIFE_byspeciesproject <- LIFE %>% 
  # Filtrar especies
  filter(Species != "None or non applicable") %>%
  # Crear columnas para el año de inicio y fin de cada proyecto
  mutate(StartYear = year(StartDate), EndYear = year(EndDate)) %>%
  # Seleccionar columnas
  select(Reference, StartYear, EndYear, TotalBudgetWeighted, EUContributionWeighted, ProjectLocation, Species, SingleSpeciesProyect) %>%
  # Ordenar de mayor a menor el presupuesto total
  arrange(desc(TotalBudgetWeighted)) %>% 
  # Convertir a dataframe
  as.data.frame()

# Guardamos el dataset
write.csv(LIFE_byspeciesproject,"./Datos_procesados/LIFE_byspeciesproject.csv", row.names = FALSE)

#### Guardar datasets de interes como workspace ####
save(LIFE, LIFE_byspecies, LIFE_byspeciesproject, file = "./Workspaces/LIFE.RData")


############ Funciones empleadas en el script ############

#### Depurar nombres cientificos de especies de LIFE ####
max_names_coincidences <- function(x){
  for (ind in 1:length(x$Species)) {
    
    if (!is.na(x$Species[ind])) {
      
      # Asegurar que solo aparece el nombre cientifico de la especie
      x$Species[ind] <- str_remove(x$Species[ind], pattern = "^[A-Z][A-Z]+\\s")
      x$Species[ind] <- str_remove(x$Species[ind], pattern = "\\s+all others")
      
      if ((x$Species[ind] == "Vipera ursinii rakosiensis") == TRUE) {
        # En caso de que el registro corresponda a la especie Vipera ursinii rakosiensis, esta aparece con el termino ssp. en la Lista Roja de la IUCN
        x$Species[ind] <- "Vipera ursinii ssp. rakosiensis"
        
      } else if ((x$Species[ind] == "Jacobaea vulgaris ssp. gotlandica") == TRUE) {
        # En caso de que el registro corresponda a Jacobaea vulgaris ssp. gotlandica, se soluciona el problema de sinonimia entre LIFE y la Lista Roja de la IUCN
        x$Species[ind] <- "Senecio jacobaea ssp. gotlandicus"
        
      } else if (str_detect(x$Species[ind], pattern = "ssp.") & !str_detect(x$Species[ind], pattern = "Dianthus|Herniaria")) {
        # En la mayoria de los casos, se sustituira ssp. por subsp., ya que asi se encontraria en la Lista Roja de la IUCN
        x$Species[ind] <- str_replace(x$Species[ind], pattern = "ssp.", "subsp.")
      }
    }
  }
  return(x)
}
