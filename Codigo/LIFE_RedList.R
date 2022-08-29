## COMBINAR INFORMACION DE PROYECTOS LIFE Y LA LISTA ROJA DE LA IUCN
## Fecha: 10/08/2022
## Autor: Maite Telletxea Martinez
## 
## Input datasets:
##          - LIFE_byspecies (Base de datos de la inversion LIFE para cada especie)
##          - LIFE_byspeciesproject (Base de datos de la inversion LIFE por proyecto y especie)
##          - RedListIUCNsyn (Base de datos del historico de las evaluaciones para cada especie o su sinonimo taxonomico)
##
## Output datasets:
##          - LIFE_IUCN_all (Base de datos de inversion por especies asociada a su histórico de evaluaciones)
##          - LIFEproject_IUCN_all (Base de datos de inversion por projecto y especies asociada a su histórico de evaluaciones)
##          - RedListIUCN2 (RedListIUCN modificado)
##          - RedListIUCNsyn (modificado)
##

#### Packages ####
library(tidyverse)
library(rredlist)

#### Token API ####
# mt_key <- ### tu token ###

#### Cargar workspaces ####
load("G:/Mi unidad/TFM/Workspaces/redlist.RData")
load("G:/Mi unidad/TFM/Workspaces/LIFE.RData")


############ PRIMERA PARTE (I) ############

#### Combinar LIFE_byspecies & RedListIUCNsyn, inner join ####

# Incluir solo los datos de especies comunes a ambas bases de datos
LIFE_IUCNpre <- inner_join(LIFE_byspecies, RedListIUCNsyn, by = c("Species"="synonym"))

# Buscar especies registradas con 2 nombres distintos en LIFE
species_rep <- unique((LIFE_IUCNpre %>%
                         # Agrupar por nombre, id y region
                         group_by(scientific_name, taxonid, region_identifier) %>%
                         # Recuento
                         count() %>%
                         # Filtrar que especies estan registradas con mas de un nombre distinto en LIFE
                         filter(n >= 2))$scientific_name) # hay 14 especies

# Combinamos informacion de las 14 especies con distintos nombres cientificos en LIFE
LIFE_IUCN <- LIFE_IUCNpre %>%
  # Agrupar por nombre cientifico, id y region
  group_by(scientific_name, taxonid, region_identifier) %>%
  # Recalcular columnas, si procede
  mutate(ConservationStart = ifelse(scientific_name %in% species_rep, yes = min(ConservationStart), no = ConservationStart),
         NumProjects = ifelse(scientific_name %in% species_rep, yes = sum(NumProjects), no = NumProjects),
         SumTotalBudgetWeighted = ifelse(scientific_name %in% species_rep, yes = sum(SumTotalBudgetWeighted), no = SumTotalBudgetWeighted),
         SumEUContributionWeighted = ifelse(scientific_name %in% species_rep, yes = sum(SumEUContributionWeighted), no = SumEUContributionWeighted),
         Species = ifelse(scientific_name %in% species_rep, yes = scientific_name, no = Species)) %>%
  # Quedarse con un unico registro para cada especie
  slice(1)

############ PRIMERA PARTE (II) ############

#### Encontrar especies en LIFE de las que no tenemos historial de evaluaciones ####
species_LIFE <- unique(LIFE_byspecies$Species) # Vector de nombres cientificos de especies en LIFE
species_RLL <- unique(LIFE_IUCNpre$Species) # Vector de nombres cientificos de especies en LIFE y Lista Roja

speciesLIFE_notRL <- species_LIFE[!(species_LIFE %in% species_RLL)] # Vector de especies que estan en LIFE pero no tenemos evaluacion

#### Consulta a la Lista Roja de la UICN ####

# Consultar especies objeto de programas LIFE que no se encuentran evaluadas en Europa y en la region mediterranea
species_df <- do.call(rbind, lapply(rl_sp(key = mt_key, all = TRUE), "[[", "result")) # Extraer $result de cada elemento de la lista y unir filas

# Seleccionar especies de interes
global_notRL <- species_df %>% 
  # Filtrar especies sin evaluaciones 
  filter(scientific_name %in% speciesLIFE_notRL) %>% 
  # Crear columna para discriminar poblaciones no europeas
  mutate(lifenotEurope = ifelse(is.na(population), FALSE, str_detect(population, pattern = "Hawaiian|Pacific|Atlantic|Indian"))) %>% 
  # Filtrar poblaciones europeas
  filter(lifenotEurope == FALSE) %>%
  # Eliminar columna
  select(-lifenotEurope)

length(speciesLIFE_notRL) - length(unique(global_notRL$scientific_name)) # No hemos recuperado evaluaciones de 82 especies

# Como hay nombres cientificos repetidos debido a poblaciones, utilizaremos los ids para realizar las consultas de las evaluaciones
sciname_LIFE_notRL <- global_notRL$scientific_name
taxonid_LIFE_notRL <- global_notRL$taxonid

##### Consultas del historico de evaluaciones a la Lista Roja de la IUCN (bucle for) (region = global) #####
lista_rem <- list() # Crear lista donde se guardaran las consultas
indice_rem <- 1
for (id in taxonid_LIFE_notRL) {
  # Guardamos los registros de una especie en su correspondiente indice
  lista_rem[[indice_rem]] <- rl_history(key = mt_key, id = id, region = "global")
  indice_rem <- indice_rem + 1
}

# Cambiar id por el nombre de las especies de la lista
lista_rem <- correct_names(sciname_LIFE_notRL, lista_rem)

#### Convertir lista de consultas a dataframe del historico de evaluaciones ####
sp_rem <- do.call(bind_rows, lapply(lista_rem, list_to_df))

#### Ordenar años del historico de evaluaciones ####
sp_rem_ord <- ordenar_evaluaciones(sp_rem)

#### Crear datasets de las evaluaciones globales ####
RedListIUCN_gl <- evaluaciones_other(global_notRL, sp_rem_ord)

# Preparar para combinar con las evaluaciones a escala europea y mediterranea (crear columna de sinonimos)
RedListIUCN_gl$synonym <- RedListIUCN_gl$scientific_name

#### Convertir columnas de categorias de la IUCN en columnas ordinales (factor ord) ####
RedListIUCN_gl[4:(ncol(RedListIUCN_gl)-1)] <- lapply(RedListIUCN_gl[4:(ncol(RedListIUCN_gl)-1)], RL_factor)

#### Guardar datasets como workspace ####
save(global_notRL, RedListIUCN_gl, file = "./Workspaces/RedListIUCNglobal.RData")

#### Añadir estas evaluaciones a RedListIUCN y RedLisIUCNsyn, para completar los datasets ####
RedListIUCN2 <- bind_rows(RedListIUCN, RedListIUCN_gl)
RedListIUCNsyn <- bind_rows(RedListIUCNsyn, RedListIUCN_gl)

# Guardar dataset
write.csv(RedListIUCN2,"./Datos_procesados/RedListIUCN2.csv", row.names = FALSE)

############ PRIMERA PARTE (III) ############

#### Combinar LIFE_byspecies & RedListIUCN_gl, inner join ####

# Incluir solo los datos de especies comunes a ambas bases de datos
LIFE_IUCN_gl <- inner_join(LIFE_byspecies, RedListIUCN_gl, by = c("Species"="synonym"))

#### Finalmente, añadir estas evaluaciones al dataset LIFE_IUCN ####
LIFE_IUCN <- bind_rows(LIFE_IUCN, LIFE_IUCN_gl)

############ SEGUNDA PARTE ############

#### Combinar LIFE_byspecies & RedListIUCNsyn, right join ####

# Incluir todas las especies de la Lista Roja de la IUCN
LIFE_IUCN_large <- right_join(LIFE_byspecies, RedListIUCNsyn, by = c("Species"="synonym")) %>% 
  # Crear columna para discriminar especies con y sin conservacion
  mutate(ConservationProjects = ifelse(is.na(SumTotalBudgetWeighted), FALSE, TRUE))

#### Limpiar LIFE_IUCN_large, eliminar registros de sinonimos y mantener contrafactual ####

# Id de especies con conservacion
taxonproject <- unique(LIFE_IUCN$taxonid)
# Especies y sinonimos taxonomicos sin conservacion
NAprojects <- LIFE_IUCN_large %>% filter(ConservationProjects == FALSE)
# NAs que pertenecen a sinonimos (porque su taxonid pertenece a una especies con conservacion)
NAsynonyms <- NAprojects[which(NAprojects$taxonid %in% taxonproject),]
# NAs que pertenecen a especies sin conservacion: contrafactual
contrafactual <- NAprojects[which(!NAprojects$taxonid %in% taxonproject),] %>% 
  # Agrupar por nombre, id y region
  group_by(scientific_name, taxonid, region_identifier) %>% 
  # Quedarse unicamente con un registro para cada especie
  slice_tail()

#### Crear dataset que recoja las especies con y sin conservacion ####
LIFE_IUCN_all <- bind_rows(LIFE_IUCN_large %>% filter(ConservationProjects == TRUE), contrafactual)

# Combinamos informacion de las 14 especies con distintos nombres cientificos en LIFE
LIFE_IUCN_all <- LIFE_IUCN_all %>% 
  # Agrupar por nombre cientifico, id y region
  group_by(scientific_name,taxonid,region_identifier) %>%
  # Recalcular columnas, si procede
  mutate(NumProjects = ifelse(scientific_name %in% species_rep, yes = sum(NumProjects), no = NumProjects),
         SumTotalBudgetWeighted = ifelse(scientific_name %in% species_rep, yes = sum(SumTotalBudgetWeighted), no = SumTotalBudgetWeighted),
         SumEUContributionWeighted = ifelse(scientific_name %in% species_rep, yes = sum(SumEUContributionWeighted), no = SumEUContributionWeighted),
         Species = ifelse(scientific_name %in% species_rep, yes = scientific_name, no = Species)) %>%
  # Quedarse con un unico registro para cada especie
  slice(1)

# Especies cuyo scientific_name figura tanto con conservacion como sin conservacion, por lo que se sustituye por el nombre que figura en Species
species_both <- ((LIFE_IUCN_all %>% group_by(scientific_name, ConservationProjects) %>% count(sort = TRUE)) %>% group_by(scientific_name) %>% count() %>% filter(n >= 2))$scientific_name

LIFE_IUCN_all <- LIFE_IUCN_all %>%
  mutate(scientific_name = ifelse(scientific_name %in% species_both, yes = Species, no = scientific_name))

# Guardar dataset
write.csv(LIFE_IUCN_all,"./Datos_procesados/LIFE_IUCN_all.csv", row.names = FALSE)

# Ver summary
summary(LIFE_IUCN_all)

############ TERCERA PARTE ############

#### Combinar LIFE_byspeciesproject & RedListIUCNsyn, inner join ####

# Incluir solo los datos de especies comunes a ambas bases de datos
LIFEproject_IUCN <- inner_join(LIFE_byspeciesproject, RedListIUCNsyn, by = c("Species"="synonym"))

LIFEproject_IUCN %>%
  # Agrupar por proyecto
  group_by(Reference) %>%
  # Recuento
  count() %>% 
  # Ordenar
  arrange(desc(n)) # 1137 proyectos dirigidos a especies concretas registradas en la Lista Roja

############ CUARTA PARTE ############

#### Combinar LIFE_byspeciesproject & RedListIUCNsyn, inner join ####

# Incluir todos los registros de la base de datos RedListIUCN (incluidas tambien especies sin proyectos dirigidos a su conservacion)
LIFEproject_IUCN_large <- right_join(LIFE_byspeciesproject, RedListIUCNsyn, by = c("Species"="synonym")) %>%
  # Ordenar por taxonid
  arrange(taxonid) %>% 
  # Crear columna para discriminar con y sin conservacion
  mutate(ConservationProject = ifelse(is.na(Reference), FALSE, TRUE))

#### Limpiar LIFE_IUCN_large, eliminar registros de sinonimos y mantener contrafactual ####

# Especies y sinonimos taxonomicos sin conservacion
NAprojects <- LIFEproject_IUCN_large %>% filter(ConservationProject == FALSE)
# NAs que pertenecen a sinonimos (porque su taxonid pertenece a una especies con conservacion)
NAsynonyms <- NAprojects[which(NAprojects$taxonid %in% taxonproject),]
# NAs que pertenecen a especies sin conservacion: contrafactual
contrafactual <- NAprojects[which(!NAprojects$taxonid %in% taxonproject),] %>% 
  # Agrupar por nombre, id y region
  group_by(scientific_name, taxonid, region_identifier) %>% 
  # Quedarse unicamente con un registro para cada especie
  slice_tail()

#### Crear dataset que recoja las especies con y sin conservacion ####
LIFEproject_IUCN_all <- bind_rows(LIFEproject_IUCN_large %>% filter(ConservationProject == TRUE), contrafactual)

##### Modificar LIFEproject_IUCN_all #####

# Rellenar estado de conservación anterior (PrevStatus) y posterior (PostStatus)
LIFEproject_IUCN_all <- PrevPostStatus(LIFEproject_IUCN_all)

# Guardar dataset
write.csv(LIFEproject_IUCN_all,"./Datos_procesados/LIFEproject_IUCN_all.csv", row.names = FALSE)

# Ver summary
summary(LIFEproject_IUCN_all)


#### Guardar datasets de interes como workspace ####
save(LIFE_IUCN, LIFE_IUCN_all, LIFEproject_IUCN, LIFEproject_IUCN_all, RedListIUCN2, RedListIUCNsyn, file = "./Workspaces/LifeRedlist.RData")


############ Funciones empleadas en el script ############

#### Corregir nombres de la lista (por si existen problemas de encoding) ####
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

#### Ordenar años de los registros ####
ordenar_evaluaciones <- function(dataframe) {
  char_cols <- dataframe %>% select(scientific_name, region_identifier)
  sp_df_years <- dataframe %>% select(-c(scientific_name, region_identifier))
  sp_df_years <- sp_df_years[, order(colnames(sp_df_years))]
  sp_df_order <- bind_cols(char_cols, sp_df_years)
  return(sp_df_order)
}

#### Base de datos Evaluaciones ####
evaluaciones_other <- function(species_df, sp_df_ord) {
  taxonid <- species_df %>% select(taxonid)
  current_category <- species_df %>% select(current_category = category)
  sp_df_ord2 <- bind_cols(taxonid, sp_df_ord)
  RedListIUCN <- bind_cols(sp_df_ord2, current_category)
}

#### Convertir columnas de categorías de la IUCN en columnas ordinales (factor ord) ####
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

#### Rellenar estado de conservacion anterior (PrevStatus) y posterior (PostStatus) ####
PrevPostStatus <- function(data) {
  
  # Añadir columnas en blanco para estado de conservacion antes y despues de los proyectos
  data$PrevStatus <- "NA"
  data$PostStatus <- "NA"
  
  # Convertir las nuevas columnas de las categorias a factor (ordered)
  data[c("PrevStatus","PostStatus")] <- lapply(data[c("PrevStatus","PostStatus")], RL_factor)
  
  # Vector numerico de los años (colnames) de las evaluciones, para poder operar con ellos
  col_years <- as.numeric(colnames(data)[12:(which(colnames(data) == "current_category")-1)])
  
  for (fila in 1:nrow(data)) {
    
    if (ConservationProject == TRUE) {
      
      # Comienzo y fin del proyecto
      st_year <- data[fila,]$StartYear
      end_year <- data[fila,]$EndYear
      
      # Encontrar el año mas cercano (y anterior) de evaluacion de la especie al comienzo del proyecto
      st_dif <- abs(col_years-st_year)
      st_year <- col_years[which(st_dif == min(st_dif))]
      if (length(st_year) != 1) {
        st_year <- st_year[1] }
      
      # Encontrar el año mas cercano (y posterior) de evaluacion de la especie al fin del proyecto
      end_dif <- abs(col_years-(end_year+10))
      end_year <- col_years[which(end_dif == min(end_dif))]
      if (length(end_year) != 1) {
        end_year <- end_year[2] }
      
      # Guardar el indice de las columnas de los años de esas evaluaciones
      if (st_year > 1900) {
        idcol_prev <- which(colnames(data) == st_year)
      } else {idcol_prev <- which(colnames(data) == 1900) }
      if (end_year < 2023) {
        idcol_post <- which(colnames(data) == end_year)
      } else {idcol_post <- ncol(data)-1 }
      
      # Guardar estados de conservacion anterior y posterior
      prev_status <- data[fila,][[idcol_prev]]
      post_status <- data[fila,][[idcol_post]]
      
      # Guardar prev_status y post_status
      while (is.na(prev_status) & idcol_prev>12) {
        # si cuando se comprueba el estado previo la especie no ha sido evaluada ese año, se buscara en el anterior
        idcol_prev <- idcol_prev-1
        prev_status <- data[fila,][[idcol_prev]]
      }
      while (is.na(post_status) & idcol_post<(which(colnames(data) == "current_category"))) {
        # si cuando se comprueba el estado posterior la especie no ha sido evaluada ese año, se buscara en el siguiente
        idcol_post <- idcol_post+1
        post_status <- data[fila,][[idcol_post]]
      }
      
      # Rellenar columnas PrevStatus y PostStatus
      if (!is.na(prev_status)) {
        # Si no es NA, rellenar PrevStatus con el estado de conservacion previo al comienzo del proyecto
        data[fila,]$PrevStatus <- prev_status
      } else { data[fila,]$PrevStatus <- NA } # Si es NA, completar con NA
      if (!is.na(post_status)) {
        # Si no es NA, rellenar PostStatus con el estado de conservacion posterior al fin del proyecto
        data[fila,]$PostStatus <- post_status
      } else { data[fila,]$PostStatus <- NA } # Si es NA, completar con NA
      
    } else {
      
      # Encontrar el primer año de evaluacion de la especie
      idcol_prim <- which(colnames(data) == col_years[1])
      
      # Guardar primer estado de conservación asignado
      prim_status <- data[fila,][[idcol_prim]]
      
      while (is.na(prim_status) & idcol_prim<(which(colnames(data) == "current_category"))) {
        # si cuando se comprueba el estado la especie no ha sido evaluada ese año, se buscara en el siguiente
        idcol_prim <- idcol_prim+1
        prim_status <- data[fila,][[idcol_prim]]
      }
      
      # Rellenar columnas PrevStatus y PostStatus
      if (!is.na(prim_status)) {
        # Si no es NA, rellenar PrevStatus con el estado de conservación previo al comienzo del proyecto
        data[fila,]$PrevStatus <- prim_status
      } else { data[fila,]$PrevStatus <- NA } # Si es NA, completar con NA
      
      data[fila,]$PostStatus <- data[fila,]$current_category
    }
  }
  
  return(data)
}
