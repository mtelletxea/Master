##
## IMPORTAR INFORMACION SOBRE CAMBIOS EN EL ESTADO DE CONSERVACION DE ESPECIES
## Fecha: 03/08/2022
## Autor: Maite Telletxea Martinez
## 
## Inputs:
##          - 16 archivos CSV con los cambios de categoria registrados entre un año y el anterior
##
## Outputs:
##          - genuine_changes.csv (Base de datos de cambios genuinos registrados en la Lista Roja de la IUCN)
##          - genuine_changes_syn.csv (Base de datos de cambios genuinos de especies y sus sinonimos taxonomicos registrados en la Lista Roja)
##

#### Packages ####
library(readr)
library(tidyverse)
library(lubridate)

#### Cargar workspace de sinonimos ####
synonyms <- readRDS("G:/Mi unidad/TFM/Workspaces/sinonimoss.rds")

############ PRIMERA PARTE ############

#### Importar archivos CSV ####
path <- "./Datos_brutos/RedList/RedListCambios/Table_7/Tabula/"
archivos <- list.files(path) # Obtener los nombres de los archivos
lista_df_tablas <- list() # Crear tabla para guardar los archivos importados
indice_tablas <- 1
for (mi_fichero in archivos) {
  # Importar archivo
  df_tabla <- read_csv(paste0(path, mi_fichero), locale = locale(encoding = "UTF-8"))
  # Unificar el nombre de los campos
  colnames(df_tabla)[1] <- "Species"
  colnames(df_tabla)[2] <- "CommonName"
  colnames(df_tabla)[3] <- "PrvCategory"
  colnames(df_tabla)[4] <- "PstCategory"
  colnames(df_tabla)[5] <- "ReasonForChange"
  colnames(df_tabla)[6] <- "RedListVersion"
  # Convertir la última columna a character
  df_tabla[[6]] <- as.character(df_tabla[[6]])
  # Añadir elemento a la lista
  lista_df_tablas[[indice_tablas]] <- df_tabla
  indice_tablas <- indice_tablas + 1
}

#### Crear un dataframe con todos los registros importados sobre cambios (genuinos y no genuinos) ####
df_changes <- bind_rows(lista_df_tablas) %>% 
  # Eliminar CommonName
  select(-CommonName) %>% 
  # Filtrar filas que no interesan
  filter(!is.na(ReasonForChange))

#### Unificar años (RedListVersion) ####
df_changes$RedListVersion <- str_extract(df_changes$RedListVersion, pattern = "^[0-9]{4}")

#### Filtrar cambios genuinos ####
genuine_changes <- df_changes %>% 
  filter(ReasonForChange == "G")

#### Guardar el dataset de los cambios genuinos de la Lista Roja de la IUCN ####
write.csv(genuine_changes,"./Datos_procesados/genuine_changes.csv", row.names = FALSE)

############ SEGUNDA PARTE ############

#### Preparar dataset que sera cruzado con la informacion obtenida de evaluaciones y proyectos ####
genuine_changes_syn <- left_join(genuine_changes, synonyms, by = c("Species" = "accepted_name")) %>% 
  # Si la especie no tiene ningun sinonimo registrado, en su lugar aparecera el scientific_name
  mutate(synonym = ifelse(is.na(synonym), Species, synonym)) %>% 
  # Eliminar accepted_id
  select(-accepted_id)

# Eliminar registros duplicados
genuine_changes_syn <- genuine_changes_syn[!duplicated(genuine_changes_syn), ]

# Guardar dataset complementario
write.csv(genuine_changes_syn,"./Datos_procesados/genuine_changes_syn.csv", row.names = FALSE)

#### Guardar datasets como workspace ####
save(df_changes, genuine_changes, genuine_changes_syn, file = "./Workspaces/cambios.RData")
