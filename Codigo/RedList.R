##
## EXTRAER INFORMACION DE LA LISTA ROJA DE LA UICN
## Fecha: 03/08/2022
## Autor: Maite Telletxea Martinez
## 
## Outputs: 
##          - species_europe.csv (Base de datos de consultas de todos los organismos evaluados a nivel europeo)
##          - species_mediterranean.csv (Base de datos de consultas de todos los organismos evaluados a nivel mediterraneo)
##          - RedListIUCN.csv (Base de datos del historico todas las evaluaciones a nivel europeo y mediterraneo)
##          - synonyms.csv (Base de datos de los sinonimos existentes)
##          - RedListIUCNsyn.csv (Base de datos que sera cruzada con la base de datos de projectos LIFE)
##

#### Packages ####
library(rredlist)
library(Rcpp)
library(tidyverse)
library(stringr)

### Cargar un workspace con unas funciones modificadas del paquete rredlist, que nos permite realizar tambien consultas a nivel regional ###
load("./Workspaces/species_region_functions.RData")

#### Token API ####
# mt_key <- ### tu token ###

############ PRIMERA PARTE ############

#### Primera consulta a la Lista Roja de la UICN ####

# Obtener informacion de las especies que se encuentran evaluadas en Europa y en la región mediterranea
species_europe <- do.call(rbind, lapply(rl_sp_region(region = "europe", key = mt_key, all = TRUE), "[[", "result")) # Extraer $result de cada elemento de la lista y unir filas
species_mediterranean <- do.call(rbind, lapply(rl_sp_region(region = "mediterranean", key = mt_key, all = TRUE), "[[", "result"))

# Guardar datasets de especies europeas y mediterraneas
write.csv(species_europe, "./Datos_brutos/RedList/species_europe.csv", row.names = FALSE)
write.csv(species_mediterranean, "./Datos_brutos/RedList/species_mediterranean.csv", row.names = FALSE)

#### Obtener los nombres o ids de las especies para realizar las consultas ####
length(unique(species_europe$scientific_name))
length(unique(species_europe$taxonid))
# Como hay tantos nombres cientificos y ids como registros a nivel europeo, utilizaremos el nombre cientifico para realizar las consultas
sciname_eu <- species_europe$scientific_name # para evaluaciones y sinonimos

length(unique(species_mediterranean$scientific_name))
length(unique(species_mediterranean$taxonid))
# Como hay nombres cientificos repetidos, en este caso utilizaremos los ids para realizar las consultas de las evaluaciones
sciname_med <- species_mediterranean$scientific_name # para sinonimos
taxonid_med <- species_mediterranean$taxonid # para evaluaciones

############ SEGUNDA PARTE ############

#### Consultas del historico de evaluaciones a la Lista Roja de la UICN (bucle for) (region = europe) ####
lista_df_eu <- list() # Crear lista donde se guardaran las consultas
indice_eu <- 1
for (name in sciname_eu) {
  
  # Guardamos los registros de una especie en su correspondiente indice
  lista_df_eu[[indice_eu]] <- rl_history(name = name, key = mt_key, region = "europe")

  # Corregir lista_df_eu (historial de especies con scope of assessment multiple)
  # Si el alcance de la evaluacion no solo es "Europe", sino tambien "Global" y/o "Mediterranean", la consulta anterior sera fallida
  if (length(lista_df_eu[[indice_eu]]$result) == 0) {
    # Repetir consulta sin especificar el parametro region
    lista_df_eu[[indice_eu]] <- rl_history(name = name, key = mt_key) }
  
  # Incluir campo de region_identifier a los elementos que no tengan
  if (length(lista_df_eu[[indice_eu]]) < 3) {
    lista_df_eu[[indice_eu]]$region_identifier <- "europe&" }
  
  indice_eu <- indice_eu + 1
  
}

#### Consultas del historico de evaluaciones a la Lista Roja de la UICN (bucle for) (region = mediterranean) ####
lista_df_med <- list() # Crear lista donde se guardaran las consultas
indice_med <- 1
for (id in taxonid_med) {
  
  # Guardamos los registros de una especie en su correspondiente indice
  lista_df_med[[indice_med]] <- rl_history(id = id, key = mt_key, region = "mediterranean")
  
  # Corregir lista_df_med
  # Si el alcance de la evaluacion no solo es "Mediterranean", sino tambien "Global" y/o "Europe", la consulta anterior sera fallida
  if (length(lista_df_med[[indice_med]]$result) == 0) {
    # Repetir consulta sin especificar el parametro region
    lista_df_med[[indice_med]] <- rl_history(id = id, key = mt_key) }
  
  # Incluir campo de region_identifier a los elementos que no tengan
  if (length(lista_df_med[[indice_med]]) < 3) {
    lista_df_med[[indice_med]]$region_identifier <- "mediterranean&" }
  
  indice_med <- indice_med + 1
  
}

# Corregir nombres de la lista (por si existen problemas de encoding o aparece el id)
lista_df_eu <- correct_names(sciname_eu, lista_df_eu)
lista_df_med <- correct_names(sciname_med, lista_df_med)

#### Convertir lista de consultas a dataframe del historico de evaluaciones ####

# Evaluaciones a escala europea
sp_df_eu <- do.call(bind_rows, lapply(lista_df_eu, list_to_df))

# Evaluaciones a escala mediterranea
sp_df_med <- do.call(bind_rows, lapply(lista_df_med, list_to_df))

#### Ordenar años del historico de evaluaciones ####
sp_eu_ord <- ordenar_evaluaciones(sp_df_eu)
sp_med_ord <- ordenar_evaluaciones(sp_df_med)

#### Crear datasets de las evaluaciones europeas y mediterraneas ####
RedListIUCN_eu <- evaluaciones(species_europe, sp_eu_ord)
RedListIUCN_med <- evaluaciones(species_mediterranean, sp_med_ord)

#### Converger registros de evaluaciones europeas y mediterraneas en un unico dataset ####
RedListIUCN <- bind_rows(RedListIUCN_eu, RedListIUCN_med)

#### Convertir columnas de categorias de la UICN en columnas ordinales (factor ord) ####
RedListIUCN[4:ncol(RedListIUCN)] <- lapply(RedListIUCN[4:ncol(RedListIUCN)], RL_factor)

#### Guardar dataset del historico de todas las evaluaciones ####
write.csv(RedListIUCN,"./Datos_procesados/RedListIUCN.csv", row.names = FALSE)

############ TERCERA PARTE ############

#### Consulta de sinonimos de las especies ####
scinames <- unique(c(sciname_eu, sciname_med))
lista_df_syn <- list()
indice_syn <- 1
for (name in scinames) {
  # Consultar sinonimos para arreglar problemas de sinonimia
  lista_df_syn[[indice_syn]] <- rl_synonyms(name = name, key = mt_key)
  indice_syn <- indice_syn + 1
}

#### Crear dataset con sinonimos, para no perder informacion ####
sp_syn <- do.call(bind_rows, lapply(lista_df_syn, list_to_df_syn))

# Eliminar espacios innecesarios para maximizar coincidencias, y duplicaciones
sp_syn$synonym <- str_remove(sp_syn$synonym, pattern = "^\\s")
sp_syn$synonym <- str_remove(sp_syn$synonym, pattern = "\\s$")
synonyms <- sp_syn[!duplicated(sp_syn), ]

# Evitar que un nombre aceptado aparezca mas de una vez en la columna de sinonimos (ya que la columna sinonimos se utilizara para el cruce de datasets)
for (i in 1:nrow(synonyms)) {
  if (length(which(str_like(string = synonyms$accepted_name, pattern = synonyms$synonym[i]) == TRUE)) > 0) {
    if (synonyms$accepted_name[i] != synonyms$synonym[i]) {
      synonyms$synonym[i] <- NA
    }
  }
}

# Descartar registros con NAs
synonyms <- synonyms[complete.cases(synonyms), ]

#### Guardar dataset de sinonimos ####
write.csv(synonyms,"./Datos_procesados/synonyms.csv", row.names = FALSE)

############ CUARTA PARTE ############

#### Preparar dataset complementario que cruzaremos con la base de datos de LIFE ####
RedListIUCNsyn <- left_join(RedListIUCN, synonyms, by = c("scientific_name" = "accepted_name", "taxonid" = "accepted_id")) %>% 
  # Si la especie no tiene ningún sinonimo registrado, en su lugar aparecera el scientific_name
  mutate(synonym = ifelse(is.na(synonym), scientific_name, synonym))

# Convertir las columnas de las categorias a factor (ordered)
RedListIUCNsyn[4:(ncol(RedListIUCNsyn)-1)] <- lapply(RedListIUCNsyn[4:(ncol(RedListIUCNsyn)-1)], RL_factor)

# Guardar dataset complementario
write.csv(RedListIUCNsyn,"./Datos_procesados/RedListIUCNsyn.csv", row.names = FALSE)

# El dataset cruzado con la informacion de LIFE sera RedListIUCNsyn, una modificacion de RedListIUCN con los sinonimos de especies, por si no se encuentran registradas en LIFE con el "accepted name")

#### Guardar datasets como workspace ####
save(species_europe, species_mediterranean, RedListIUCN, RedListIUCN_eu, RedListIUCN_med, RedListIUCNsyn, file = "./Workspaces/redlist.RData")
saveRDS(synonyms, file = "./Workspaces/synonyms.rds")


############ Funciones empleadas en el script ############

#### Corregir nombres ####
correct_names <- function(nombres, lista) {
  for (n in 1:length(nombres)) {
    lista[[n]]$name <- nombres[n]
  }
  return(lista)
}

#### Lista a dataframe para evaluaciones ####
list_to_df <- function(lista) {
  
  if (is.null(lista$result)) {
    # Si el resultado es nulo, para evitar errores, se extraera la categoria actual del dataframe de especies europeas o mediterraneas, segun corresponda
    lista$result <- data.frame("scientific_name" = lista$name,
                               "year" = "current_category",
                               "region_identifier" = lista$region_identifier,
                               "code" = ifelse(str_detect(lista$region_identifier, pattern = "europe"),
                                               species_europe$category[which(species_europe$scientific_name == lista$name)],
                                               species_mediterranean$category[which(species_mediterranean$scientific_name == lista$name)]))
    result <- lista$result
    result %>%
      pivot_wider(names_from = "year",
                  values_from = "code")
    
  } else {
    # Si el resultado no es nulo
    result <- lista$result
    
    if (length(result) != 0) {
      # El resultado tiene longitud distinta a cero (hay registros)
      result$scientific_name <- lista$name
      result$region_identifier <- lista$region_identifier
      
      if (length(unique(result$assess_year)) == nrow(result)) {
        # Si no hay años de evaluacion repetidos, se registrara "assess_year"
        result %>%
          select(scientific_name, assess_year, region_identifier, code) %>%
          pivot_wider(names_from = "assess_year",
                      values_from = "code")
        
      } else if (length(unique(result$year)) == nrow(result)) {
        # Si hay años de evaluacion repetidos, pero no se repiten años de actualizacion en la Lista Roja, se registrara "year" para evitar errores
        result %>%
          select(scientific_name, year, region_identifier, code) %>%
          pivot_wider(names_from = "year",
                      values_from = "code")
        
      } else {
        # Si ambos años estan repetidos, se eliminara un año de evaluacion para evitar errores
        result <- result[-which(duplicated(result$assess_year)),]
        result %>%
          select(scientific_name, assess_year, region_identifier, code) %>%
          pivot_wider(names_from = "assess_year",
                      values_from = "code")
      }
      
    } else {
      # El resultado tiene longitud de cero, para evitar errores, se extraera la categoria actual del dataframe de especies europeas o mediterraneas, segun corresponda
      result <- data.frame("scientific_name" = lista$name,
                           "year" = "current_category",
                           "region_identifier" = lista$region_identifier,
                           "code" = ifelse(str_detect(lista$region_identifier, pattern = "europe"),
                                           species_europe$category[which(species_europe$scientific_name == lista$name)],
                                           species_mediterranean$category[which(species_mediterranean$scientific_name == lista$name)]))
      result %>%
        pivot_wider(names_from = "year",
                    values_from = "code")
    }
  }
}

#### Lista a dataframe para sinonimos ####
list_to_df_syn <- function(lista) {
  
  if (is.null(lista$count)) {
    # Si no tiene sinonimos, eliminar elemento de la lista
    lista <- NULL
    
  } else {

    if (lista$count == 0) {
      # Si no tiene sinonimos, eliminar elemento de la lista
      lista <- NULL
      
    } else {
      # Si tiene sinonimos, seleccionar campos de interes
      result <- as.data.frame(lista$result) %>% select(accepted_id, accepted_name, synonym)
      # Crear registro del propio nombre cientifico
      accepted <- data.frame("accepted_id" = lista$result$accepted_id[which(lista$result$accepted_name == lista$name)][1], "accepted_name" = lista$name, "synonym" = lista$name)
      bind_rows(result, accepted)
    }
  }
}

#### Ordenar años de los registros ####
ordenar_evaluaciones <- function(dataframe) {
  char_cols <- dataframe %>% select(scientific_name, region_identifier)
  sp_df_years <- dataframe %>% select(-c(scientific_name, region_identifier))
  sp_df_years <- sp_df_years[, order(colnames(sp_df_years))]
  sp_df_order <- bind_cols(char_cols, sp_df_years)
  return(sp_df_order)
}

#### Base de datos Evaluaciones ####
evaluaciones <- function(species_df, sp_df_ord) {
  taxonid <- species_df %>% select(taxonid)
  current_category <- species_df %>% select(current_category = category)
  sp_df_ord2 <- bind_cols(taxonid, sp_df_ord %>% select(-current_category))
  RedListIUCN <- bind_cols(sp_df_ord2, current_category)
}

#### Convertir columnas de categorías de la UICN en columnas ordinales (factor ord) ####
RL_factor <- function(column) {
  # Unificar categorías equivalentes
  column[column == "E"] <- "EN"
  column[column == "V"] <- "VU"
  column[column == "K"] <- "DD"
  column[column == "T"] <- "NT"
  column[column == "Ex"] <- "EX"
  column[column == "Ex?"] <- "EX"
  column[column == "LR/lc"] <- "LC"
  column[column == "LR/nt"] <- "NT"
  column[column == "CR(PE)"] <- "CR"
  column[column == "CR (PE)"] <- "CR"
  column[column == "CR(PEW)"] <- "CR"
  factor(column, order = TRUE, levels = c("NA", "DD", "LC", "NT", "LR/cd", "VU", "EN", "CR", "RE", "EW", "EX"))
}
