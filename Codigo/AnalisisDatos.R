##
## CONSIDERAR INFORMACION SOBRE CAMBIOS GENUINOS EN EL ESTADO DE CONSERVACION DE ESPECIES
## ANALISIS Y REPRESENTACION DE DATOS
## Fecha: 24/08/2022
## Autor: Maite Telletxea Martinez
## 
## Inputs:
##          - genuine_changes_syn.csv (Base de datos de cambios genuinos registrados de especies o sinonimos taxonomicos en la Lista Roja de la IUCN)
##
## Outputs:
##          - LIFE_IUCN_all_genuine.csv (Base de datos de registros de especies en la Lista Roja y LIFE que experimentan un cambio genuino)
##          - LIFEproject_IUCN_all_genuine.csv (Base de datos de registros de proyectos que incluyen especies que experimentan un cambio genuino)
##

#### Packages ####
library(tidyverse)
library(lubridate)
library(cowplot)
library(imager)
library(magick)
library(gridExtra)
library(grid)
library(car)
options(scipen = 999)

load("G:/Mi unidad/TFM/Workspaces/LifeRedlist.RData")
load("G:/Mi unidad/TFM/Workspaces/cambios.RData")
load("G:/Mi unidad/TFM/Workspaces/redlist.RData")
load("G:/Mi unidad/TFM/Workspaces/life.RData")

############ PRIMERA PARTE ############

#### Cruzar LIFE_IUCN_all con cambios genuinos ####

# Incluir solo los datos de especies comunes a ambas bases de datos
LIFE_IUCN_all_genuine <- inner_join(LIFE_IUCN_all, genuine_changes_syn %>% select(-Species), by = c("Species"="synonym"))

#### Añadir columna para discriminar la escala del cambio ####
LIFE_IUCN_all_genuine$ChangeScale <- NA

for (row in 1:nrow(LIFE_IUCN_all_genuine)) {
  # Guardar año de la version de la Lista Roja
  year_version <- LIFE_IUCN_all_genuine$RedListVersion[row]
  # Guardar columna que corresponde a ese año
  col_year <- which(colnames(LIFE_IUCN_all_genuine) == year_version)
  
  # Si el estado de conservacion registrado en el historico no es NA
  if (!is.na(LIFE_IUCN_all_genuine[row, col_year])) {
    # Y coindice con el cambio registrado en la base de datos de cambios genuinos
    if (LIFE_IUCN_all_genuine[row, col_year] == LIFE_IUCN_all_genuine$PstCategory[row]) {
      # La escala de cambio es la correcta
      LIFE_IUCN_all_genuine$ChangeScale[row] <- "correct" }
    
    # Si en ese año es NA, pero si hay registrada una categoria el año anterior
  } else if (!is.na(LIFE_IUCN_all_genuine[row, col_year-1])) {
    # Y coindice con el cambio registrado en la base de datos de cambios genuinos
    if (LIFE_IUCN_all_genuine[row, col_year-1] == LIFE_IUCN_all_genuine$PstCategory[row]) {
      # La escala de cambio es la correcta
      LIFE_IUCN_all_genuine$ChangeScale[row] <- "correct" }
    
    # En cualquier otro caso, la escala es incorrecta
  } else { LIFE_IUCN_all_genuine$ChangeScale[row] <- "incorrect" }
}

#### Convertir columnas de categorias de la IUCN en columnas ordinales (factor ord) ####
LIFE_IUCN_all_genuine[c("PrvCategory","PstCategory")] <- lapply(LIFE_IUCN_all_genuine[c("PrvCategory","PstCategory")], RL_factor)

#### Filtrar registros cuya escala de cambio es correcta y que no consideren cambios anteriores al inicio de la conservación de una especie ####
LIFE_IUCN_all_genuine <- LIFE_IUCN_all_genuine %>% filter(ChangeScale == "correct") %>% 
  # Calcular tiempo que transcurre entre el comienzo de la conservacion y el cambio de categoria
  mutate(TimeforChange = ifelse(!is.na(ConservationStart), as.numeric(RedListVersion) - ConservationStart, NA)) %>% 
  # Eliminar registros de cambios anteriores a la conservacion
  filter(TimeforChange > 0 | is.na(TimeforChange)) %>% 
  select(-c(ReasonForChange, ChangeScale))

mean(LIFE_IUCN_all_genuine$TimeforChange, na.rm = TRUE)
# Media de años que cuesta ver un cambio en el estado de conservacion de una especie segun los registros que poseemos: 16 años

#### Completar variable de presupuesto para todas las especies ####

# Aquellas que no sean objeto de conservacion del programa LIFE, tendran un presupuesto igual a cero
LIFE_IUCN_all_genuine$SumTotalBudgetWeighted[is.na(LIFE_IUCN_all_genuine$SumTotalBudgetWeighted) == TRUE] <- 0
LIFE_IUCN_all_genuine$SumEUContributionWeighted[is.na(LIFE_IUCN_all_genuine$SumEUContributionWeighted) == TRUE] <- 0
LIFE_IUCN_all_genuine$NumProjects[is.na(LIFE_IUCN_all_genuine$NumProjects) == TRUE] <- 0

#### Calcular el cambio en el estado de conservacion de las especies ###
LIFE_IUCN_all_genuine$ChangeValue <- as.integer(LIFE_IUCN_all_genuine$PrvCategory) - as.integer(LIFE_IUCN_all_genuine$PstCategory)
# Si el cambio es a mejor, tendra un valor positivo: ha bajado de categoria
# Si el cambio es a peor, tendra un valor negativo: ha subido de categoria

#### Guardar dataset teniendo en cuenta solo cambios genuinos (I) ####
write.csv(LIFE_IUCN_all_genuine,"./Datos_procesados/LIFE_IUCN_all_genuine.csv", row.names = FALSE)

############ SEGUNDA PARTE ############

#### Cruzar LIFEproject_IUCN_all con cambios genuinos ####
LIFEproject_IUCN_all_genuine_pre <- inner_join(LIFEproject_IUCN_all, genuine_changes_syn %>% select(-Species), by = c("Species" = "synonym"))

#### Añadir columna para discriminar la escala del cambio ####
LIFEproject_IUCN_all_genuine_pre$ChangeScale <- NA

for (row in 1:nrow(LIFEproject_IUCN_all_genuine_pre)) {
  year_version <- LIFEproject_IUCN_all_genuine_pre$RedListVersion[row]
  col_year <- which(colnames(LIFEproject_IUCN_all_genuine_pre) == year_version)
  if (!is.na(LIFEproject_IUCN_all_genuine_pre[row, col_year])) {
    if (LIFEproject_IUCN_all_genuine_pre[row, col_year] == LIFEproject_IUCN_all_genuine_pre$PstCategory[row]) {
      LIFEproject_IUCN_all_genuine_pre$ChangeScale[row] <- "correct" }
  } else if (!is.na(LIFEproject_IUCN_all_genuine_pre[row, col_year-1])) {
    if (LIFEproject_IUCN_all_genuine_pre[row, col_year-1] == LIFEproject_IUCN_all_genuine_pre$PstCategory[row]) {
      LIFEproject_IUCN_all_genuine_pre$ChangeScale[row] <- "correct" }
  } else { LIFEproject_IUCN_all_genuine_pre$ChangeScale[row] <- "incorrect" }
}

#### Convertir columnas de categorias de la IUCN en columnas ordinales (factor ord) ####
LIFEproject_IUCN_all_genuine_pre[c("PrvCategory","PstCategory")] <- lapply(LIFEproject_IUCN_all_genuine_pre[c("PrvCategory","PstCategory")], RL_factor)

#### Añadir columna para seleccionar aquellas recategorizaciones registradas tras el comienzo de cada proyecto ####
LIFEproject_IUCN_all_genuine <- LIFEproject_IUCN_all_genuine_pre %>%
  mutate(ChangeAfterConservation = ifelse(PostStatus == PstCategory & PrevStatus == PrvCategory, TRUE,
                                          ifelse(current_category == PstCategory & PrevStatus == PrvCategory, TRUE, FALSE)))

#### Filtrar registros cuya escala de cambio es correcta y que consideren cambios posteriores a la finalización de cada proyecto ####
LIFEproject_IUCN_all_genuine <- LIFEproject_IUCN_all_genuine %>% 
  filter(ChangeScale == "correct", ChangeAfterConservation == TRUE) %>% 
  select(-c(ReasonForChange, ChangeScale, ChangeAfterConservation))

#### Calcular el cambio en el estado de conservacion de las especies ###
LIFEproject_IUCN_all_genuine$ChangeValue <- as.integer(LIFEproject_IUCN_all_genuine$PrvCategory) - as.integer(LIFEproject_IUCN_all_genuine$PstCategory)

#### Guardar dataset teniendo en cuenta solo cambios genuinos (II) ####
write.csv(LIFEproject_IUCN_all_genuine,"./Datos_procesados/LIFEproject_IUCN_all_genuine.csv", row.names = FALSE)

#### Guardar datasets como workspace ####
save(LIFE_IUCN_all_genuine, LIFEproject_IUCN_all_genuine, file = "./Workspaces/LifeRedlistgenuines.RData")

############ TERCERA PARTE ############

#### Analisis de datos de la Lista Roja de la IUCN ####

## Especies incluidas en cada categoria a escala europea
RedListIUCN_category_count_eu <- RedListIUCN[str_detect(RedListIUCN$region_identifier, pattern = "europe") == TRUE, ] %>% 
  # No incluir categoria NA
  filter(current_category != "NA") %>%
  # Agrupar por categoria
  group_by(current_category) %>%
  # Contar especies en cada grupo
  summarise(number_species = n()) %>% 
  # Calcular porcentaje
  mutate(percentage = number_species/sum(number_species)*100)

## Especies incluidas en las tres categorias de amenaza a escala europea
threat_eu <- RedListIUCN[str_detect(RedListIUCN$region_identifier, pattern = "europe") == TRUE, ] %>%
  # No incluir categoria NA
  filter(current_category != "NA") %>%
  # Clasificar especies en amenazada o no amenazada
  mutate(threat = ifelse(str_detect(current_category, pattern = "VU|EN|CR"), "amenazada", "no amenazada")) %>%
  # Agrupar por riesgo de amenaza
  group_by(threat) %>% 
  # Contar especies en cada grupo
  summarise(number_species = n()) %>% 
  # Calcular proporcion
  mutate(percentage = number_species/sum(number_species)*100)

## Representar grafico de especies incluidas y no incluidas en las categorias de amenaza a escala europea
threat_eu %>%
  ggplot(aes(x = threat, y = percentage, fill = threat)) +
  # Grafico de barras
  geom_col() +
  # Definir color
  scale_fill_grey() +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = NULL, y = "Porcentaje (%)") +
  # Modificar leyenda y texto de los ejes
  theme(legend.position = "none",
        axis.text = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 20, colour = "black")) +
  # Incluir etiquetas
  geom_text(aes(label = paste(number_species,"\n","(",sprintf("%0.2f", round(percentage, digits = 2)),"%)")), colour = "black", vjust = -1, size = 5) +
  # Ajustar eje y
  scale_y_continuous(limits = c(0, 100))

# Guardar y cargar grafico anterior
gr_threat_eu <- load.image("./Plots/threat_eu.png")

## Representar grafico de especies incluidas en cada categoria de la Lista Roja a escala europea
gr_category_eu <- RedListIUCN_category_count_eu %>%
  ggplot(aes(x = current_category, y = percentage, fill = current_category)) +
  # Grafico de barras
  geom_col() +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = "Categoría", y = "Porcentaje (%)") +
  # Modificar leyenda y texto de los ejes
  theme(legend.position = "none",
        axis.text = element_text(size = 30, colour = "black"),
        axis.title = element_text(size = 30, colour = "black")) +
  # Incluir etiquetas
  geom_text(aes(label = paste(number_species,"\n","(",sprintf("%0.2f", round(percentage, digits = 2)),"%)")), colour = "black", vjust = -0.5, size = 5) +
  # Ajustar eje y
  scale_y_continuous(limits = c(0, 55),
                     breaks = c(10, 20, 30, 40, 50))

### Representar ambos graficos sobre la distribucion de categorias actual a escala europea
grafico_eu <- ggdraw(gr_category_eu) + 
  draw_image(gr_threat_eu, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.5, height = 0.5)

## Especies incluidas en cada categoria a escala mediterranea
RedListIUCN_category_count_med <- RedListIUCN[str_detect(RedListIUCN$region_identifier, pattern = "mediterranean") == TRUE, ] %>%
  # No incluir categoria NA
  filter(current_category != "NA") %>%
  # Agrupar por categoria
  group_by(current_category) %>%
  # Contar especies en cada grupo
  summarise(number_species = n()) %>% 
  # Calcular porcentaje
  mutate(percentage = number_species/sum(number_species)*100)

# Especies incluidas en las tres categorias de amenaza a escala mediterranea
threat_med <- RedListIUCN[str_detect(RedListIUCN$region_identifier, pattern = "mediterranean") == TRUE, ] %>% 
  # No incluir categoria NA
  filter(current_category != "NA") %>%
  # Clasificar especies en amenazada o no amenazada
  mutate(threat = ifelse(str_detect(current_category, pattern = "VU|EN|CR"), "amenazada", "no amenazada")) %>%
  # Agrupar por riesgo de amenaza
  group_by(threat) %>% 
  # Contar especies en cada grupo
  summarise(number_species = n()) %>% 
  # Calcular proporcion
  mutate(percentage = number_species/sum(number_species)*100)

## Representar grafico de especies incluidas y no incluidas en las categorias de amenaza a escala mediterranea
threat_med %>%
  ggplot(aes(x = threat, y = percentage, fill = threat)) +
  # Grafico de barras
  geom_col() +
  # Deinir color
  scale_fill_grey() +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = NULL, y = "Porcentaje (%)") +
  # Modificar leyenda y texto de los ejes
  theme(legend.position = "none",
        axis.text = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 20, colour = "black")) +
  # Incluir etiquetas
  geom_text(aes(label = paste(number_species,"\n","(",sprintf("%0.2f", round(percentage, digits = 2)),"%)")), colour = "black", vjust = -1, size = 5) +
  # Ajustar eje y
  scale_y_continuous(limits = c(0, 100))

# Guardar y cargar grafico anterior
gr_threat_med <- load.image("./Plots/threat_med.png")

## Representar grafico de especies incluidas en cada categoria de la Lista Roja a escala mediterranea
gr_category_med <- RedListIUCN_category_count_med %>%
  # Ignorar categoria NA en la representacion
  filter(current_category != "NA") %>% 
  ggplot(aes(x = current_category, y = percentage, fill = current_category)) +
  # Grafico de barras
  geom_col() +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = "Categoría", y = "Porcentaje (%)") +
  # Modificar leyenda y texto de los ejes
  theme(legend.position = "none",
        axis.text = element_text(size = 30, colour = "black"),
        axis.title = element_text(size = 30, colour = "black")) +
  # Incluir etiquetas
  geom_text(aes(label = paste(number_species,"\n","(",sprintf("%0.2f", round(percentage, digits = 2)),"%)")), colour = "black", vjust = -0.5, size = 5) +
  # Ajustar eje y
  scale_y_continuous(limits = c(0, 55),
                     breaks = c(10, 20, 30, 40, 50))

### Representar ambos graficos sobre la distribucion de categorias actual a escala mediterranea
grafico_med <- ggdraw(gr_category_med) + 
  draw_image(gr_threat_med, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.5, height = 0.5)

### Combinar en un unico plot los graficos de la distribucion de categorias a escala europea y mediterranea
plot_grid(grafico_eu, grafico_med, labels = c('AUTO'), label_size = 50, rel_widths = c(1, 1))

#### Analisis de los datos de LIFE ####

## Representar grafica de las 20 especies en las que mas presupuesto en conservacion ha invertido LIFE
gr_sp_budget <- LIFE_byspecies %>%
  # Seleccionar las primeras 20 especies
  head(n=20) %>%
  # Dividir para representar en millones de euros
  mutate(SumTotalBudgetWeighted = SumTotalBudgetWeighted/1000000) %>% 
  ggplot(aes(x = reorder(Species, SumTotalBudgetWeighted, sum), y = SumTotalBudgetWeighted)) +
  # Grafico de barras
  geom_bar(stat = "identity", fill = "#440154FF") +
  # Intercambiar ejes x e y
  coord_flip() +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = "Especie", y = "Presupuesto total (millones de euros)") +
  # Modificar texto de los ejes
  theme(axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_text(size = 20, colour = "black", face = "italic"),
        axis.title = element_text(size = 20, colour = "black")) +
  # Incluir etiquetas
  geom_text(aes(label = round(SumTotalBudgetWeighted, digits = 2)), hjust = 1.5, colour = "white", size = 5)

## Representar grafica de las 20 especies incluidas en el mayor numero de proyectos conservacion financiados por LIFE
gr_sp_npro <- LIFE_byspecies %>%
  # Ordenar ahora por numero de proyectos
  arrange(desc(NumProjects)) %>% 
  # Seleccionar las primeras 20 especies
  head(n=20) %>%
  ggplot(aes(x = reorder(Species, NumProjects, sum), y = NumProjects)) +
  # Grafico de barras
  geom_bar(stat = "identity", fill = "#440154FF") +
  # Intercambiar ejes x e y
  coord_flip() +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = "Especie", y = "Número de proyectos") +
  # Modificar texto de los ejes
  theme(axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_text(size = 20, colour = "black", face = "italic"),
        axis.title = element_text(size = 20, colour = "black")) +
  # Incluir etiquetas
  geom_text(aes(label = NumProjects), hjust = 1.5, colour = "white", size = 5)

### Combinar en un unico plot las especies con mayor inversion economica y logistica
plot_grid(gr_sp_budget, gr_sp_npro, labels = c('AUTO'), label_size = 50, rel_widths = c(1, 1))

#### Analisis de los datos de LIFE_IUCN_all ####

## Representar distribucion de evaluaciones más recientes en cada categoría de la Lista Roja según la especie este o no en LIFE 
gr_ev_category <- LIFE_IUCN_all %>%
  # Ignorar categoria NA para la representacion
  filter(current_category != "NA") %>% 
  # Agrupar por especie presente o no en LIFE y categoria
  group_by(ConservationProjects, current_category) %>%
  # Contar especies en cada grupo
  summarise(number_sp = n()) %>% 
  # Calcular porcentaje
  mutate(percentage = number_sp/sum(number_sp)*100) %>% 
  ggplot(aes(x = current_category, y = percentage, fill = ConservationProjects)) +
  # Grafico de barras
  geom_col(position = position_dodge(preserve = "single")) +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes y leyenda
  labs(x = "Categoría", y = "Porcentaje (%)", fill = "LIFE") +
  # Modificar leyenda y texto de la leyenda y los ejes
  theme(legend.position = c(0.55, 0.65),
        legend.title = element_text(size = 30, colour = "black"),
        legend.text = element_text(size = 25, colour = "black"),
        axis.text = element_text(size = 30, colour = "black"),
        axis.title = element_text(size = 30, colour = "black")) +
  # Ajustar el relleno de los elementos
  scale_fill_viridis_d(begin = 0.6, end = 0, labels = c("NO", "SI")) +
  # Ajustar eje y
  scale_y_continuous(limits=c(0,55), breaks = c(0, 10, 20, 30, 40, 50))

# Cargar imagen de grafico
im_ev_threat <- load.image("./Plots/ev_threat.png")

### Combinar en un unico plot los graficos sobre la distribucion de las evaluaciones del dataset LIFE_IUCN_all
ggdraw(gr_ev_category) + 
  draw_image(im_ev_threat, x = 1.15, y = 1, hjust = 1, vjust = 1, width = 0.65, height = 0.65)

# Numero de evaluaciones más recientes en categorias o no de amenaza de especies con conservacion LIFE
LIFE_IUCN_all %>% 
  # Selecionar especies incluidas en proyectos de conservacion LIFE
  filter(ConservationProjects == TRUE) %>% 
  # Ignorar categoria NA para la representacion
  filter(current_category != "NA") %>%
  # Clasificar especies en amenazada o no amenazada
  mutate(threat = ifelse(str_detect(current_category, pattern = "VU|EN|CR"), "amenazada", "no amenazada")) %>%
  # Agrupar por riesgo de amenaza
  group_by(threat) %>% 
  # Contar evaluaciones en cada grupo
  summarise(number_species = n())

# Numero de evaluaciones más recientes en categorias o no de amenaza de especies sin conservacion LIFE
LIFE_IUCN_all %>% 
  # Selecionar especies incluidas en proyectos de conservacion LIFE
  filter(ConservationProjects == FALSE) %>% 
  # Ignorar categoria NA para la representacion
  filter(current_category != "NA") %>%
  # Clasificar especies en amenazada o no amenazada
  mutate(threat = ifelse(str_detect(current_category, pattern = "VU|EN|CR"), "amenazada", "no amenazada")) %>%
  # Agrupar por riesgo de amenaza
  group_by(threat) %>% 
  # Contar evaluaciones en cada grupo
  summarise(number_species = n())

#### Analisis de los datos de LIFE_IUCN_all_genuine ####

# Categorias anteriores
prv_categ_long <- LIFE_IUCN_all_genuine %>%
  # Seleccionar campos
  select(scientific_name, taxonid, region_identifier, ConservationProjects, PrvCategory) %>%
  # Agrupar por especie presente o no en LIFE y categoria anterior
  group_by(ConservationProjects, PrvCategory) %>%
  # Pivot longer para añadir columna
  pivot_longer(names_to = "EvaluationMoment", values_to = "Category", cols = 5)

# Categorias posteriores
pst_categ_long <- LIFE_IUCN_all_genuine %>%
  # Seleccionar campos
  select(scientific_name, taxonid, region_identifier, ConservationProjects, PstCategory) %>%
  # Agrupar por especie presente o no en LIFE y categoria anterior
  group_by(ConservationProjects, PstCategory) %>%
  # Pivot longer para añadir columna
  pivot_longer(names_to = "EvaluationMoment", values_to = "Category", cols = 5)

# Preparar dataframe para test y representar
category_count <- bind_rows(prv_categ_long, pst_categ_long)

## Chi-cuadrado
df_chitest_LIFE <- category_count %>%
  filter(ConservationProjects == TRUE) %>%
  group_by(EvaluationMoment, Category) %>%
  count() %>%
  pivot_wider(names_from = Category, values_from = n) %>%
  as.data.frame()
df_chitest_LIFE[is.na(df_chitest_LIFE)] <- 0
rownames(df_chitest_LIFE) <- df_chitest_LIFE$EvaluationMoment
df_chitest_LIFE[,1] <- NULL

df_chitest_noLIFE <- category_count %>%
  filter(ConservationProjects == FALSE) %>%
  group_by(EvaluationMoment, Category) %>%
  count() %>%
  pivot_wider(names_from = Category, values_from = n) %>%
  as.data.frame()
df_chitest_noLIFE[is.na(df_chitest_noLIFE)] <- 0
rownames(df_chitest_noLIFE) <- df_chitest_noLIFE$EvaluationMoment
df_chitest_noLIFE[,1] <- NULL

chisq.test(df_chitest_LIFE)
chisq.test(df_chitest_noLIFE)

## Representar la distribucion de categorias antes y despues del cambio genuino en evaluaciones de especies sin conservacion LIFE
grafico_noLIFE <- category_count %>%
  # Selecionar especies no incluidas en proyectos de conservacion LIFE
  filter(ConservationProjects == FALSE) %>%
  # Agrupar por momento de evaluacion y categoria
  group_by(EvaluationMoment, Category) %>%
  # Contar numero de evaluaciones en cada grupo
  summarise(number_evaluaciones = n()) %>%
  ggplot(aes(x = Category, y = number_evaluaciones, fill = EvaluationMoment)) +
  # Grafico de barras
  geom_col(position = position_dodge(preserve = "single")) +
  # Definir tema
  theme_minimal() +
  # Modificar nombres del titulo, los ejes y la leyenda
  labs(title = "Especies sin conservación", x = "Categoría", y = "Número de evaluaciones", fill = "Evaluación") +
  # Modificar texto de la leyenda, los ejes y el titulo
  theme(legend.title = element_text(size = 30, colour = "black"),
        legend.text = element_text(size = 25, colour = "black"),
        axis.text = element_text(size = 30, colour = "black"),
        axis.title.x = element_text(size = 30, colour = "black"),
        axis.title.y = element_text(size = 30, colour = "black"),
        plot.title = element_text(size = 30, hjust = 0.5)) +
  # Ajustar el relleno de los elementos
  scale_fill_viridis_d(begin = 0.1, end = 0.9, labels = c("Anterior", "Posterior")) +
  # Ajustar eje y
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25))

## Representar la distribucion de categorias antes y despues del cambio genuino en evaluaciones de especies con conservacion LIFE
grafico_LIFE <- category_count %>%
  # Selecionar especies incluidas en proyectos de conservacion LIFE
  filter(ConservationProjects == TRUE) %>%
  # Agrupar por momento de evaluacion y categoria
  group_by(EvaluationMoment, Category) %>%
  # Contar numero de evaluaciones en cada grupo
  summarise(number_evaluaciones = n()) %>%
  ggplot(aes(x = Category, y = number_evaluaciones, fill = EvaluationMoment)) +
  # Grafico de barras
  geom_col(position = position_dodge(preserve = "single")) +
  # Definir tema
  theme_minimal() +
  # Modificar nombres del titulo y los ejes
  labs(title = "Especies con conservación", x = "Categoría", y = NULL) +
  # Modificar leyenda y texto de los ejes y el titulo
  theme(legend.position = "none",
        axis.text = element_text(size = 30, colour = "black"),
        axis.title = element_text(size = 30, colour = "black"),
        plot.title = element_text(size = 30, hjust = 0.5)) +
  # Ajustar el relleno de los elementos
  scale_fill_viridis_d(begin = 0.1, end = 0.9, labels = c("Anterior", "Posterior")) +
  # Ajustar eje y
  scale_y_continuous(limits = c(0,12),
                     breaks = c(0, 5, 10))

### Combinar en un unico plot los graficos sobre la distribucion anterior y posterior de las categorias
plot_grid(grafico_noLIFE, grafico_LIFE, labels = c("A", "B"), label_size = 60, rel_widths = c(1.2, 1))

## Representar Valor del cambio en las evaluaciones vs. Presupuesto invertido en especies
rects <- data.frame(start = c(-4, 0), end = c(0, 4), col = c("Deterioro", "Mejora"))

# Grafico desde 0 a 20 millones de euros
budget_change1 <- LIFE_IUCN_all_genuine %>%
  # Agrupar por presupuesto y valor del cambio
  group_by(ConservationProjects, SumTotalBudgetWeighted, ChangeValue) %>%
  # Contar numero de evaluaciones
  summarise(number_assessment = n()) %>%
  # Dividir para representar en millones de euros
  mutate(SumTotalBudgetWeighted = SumTotalBudgetWeighted/1000000) %>% 
  ggplot() +
  # Color de fondo del grafico segun mejora o deterioro
  geom_rect(data = rects, aes(xmin = 0, xmax = 20, ymin = start, ymax = end, fill = col), alpha = 0.2) +
  # Línea horizontal en y = 0
  geom_hline(yintercept = 0, size = 2) +
  # Grafico de dispersion
  geom_point(aes(x = SumTotalBudgetWeighted , y = ChangeValue, color = ConservationProjects, size = number_assessment), alpha = 0.6, shape = 16) +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = NULL, y = "Valor del cambio") +
  # Modificar leyenda y texto de los ejes
  theme(legend.position = "none",
        axis.text = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 25, colour = "black")) +
  # Modificar elementos de la leyenda
  guides(color = guide_legend(override.aes = list(size = 7))) +
  # Modificar relleno de los elementos
  scale_color_viridis_d(name = "LIFE", labels = c("NO", "SÍ"), begin = 0.6, end = 0) +
  # Modificar tamaño de los elementos
  scale_size_continuous(name = "Nº de registros", range = c(4, 20), breaks = c(2, 4, 8, 16)) +
  # Ajustar eje x
  scale_y_continuous(limits = c(-4, 4)) +
  # Ajustar eje x
  scale_x_continuous(limits = c(0, 20),
                      breaks = c(0, 5, 10, 15, 20))
    

# Grafico desde 99 a 105.5
budget_change2 <- LIFE_IUCN_all_genuine %>%
  # Agrupar por conservacion LIFE o no, presupuesto y valor del cambio
  group_by(ConservationProjects, SumTotalBudgetWeighted, ChangeValue) %>%
  # Contar numero de evaluaciones
  summarise(number_assessment = n()) %>%
  # Dividir para representar en millones de euros
  mutate(SumTotalBudgetWeighted = SumTotalBudgetWeighted/1000000) %>% 
  ggplot() +
  # Color de fondo del grafico segun mejora o deterioro
  geom_rect(data = rects, aes(xmin = 99, xmax = 105.5, ymin = start, ymax = end, fill = col), alpha = 0.2) +
  scale_fill_discrete(guide = "none") +
  # Línea horizontal en y = 0
  geom_hline(yintercept = 0, size = 2) +
  # Grafico de dispersion
  geom_point(aes(x = SumTotalBudgetWeighted , y = ChangeValue, color = ConservationProjects, size = number_assessment), alpha = 0.6) +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = NULL, y = NULL) +
  # Modificar leyenda y texto de la leyenda y los ejes
  theme(legend.title = element_text(size = 20, colour = "black"),
        legend.text = element_text(size = 20, colour = "black"),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_text(size = 20, colour = "white"),
        axis.title = element_text(size = 25, colour = "white")) +
  # Modificar elementos de la leyenda
  guides(color = guide_legend(override.aes = list(size = 7))) +
  # Modificar relleno de los elementos
  scale_color_viridis_d(name = "LIFE", labels = c("NO", "SÍ"), begin = 0.6, end = 0) +
  # Modificar tamaño de los elementos
  scale_size_continuous(name = "Nº de registros", range = c(4, 20), breaks = c(2, 4, 8, 16)) +
  # Ajustar eje x
  scale_x_continuous(limits = c(99, 105.5),
                     breaks = c(100, 105))

# Combinar las dos partes del grafico del valor del cambio vs. presupuesto
grid.arrange(budget_change1, budget_change2, nrow = 1, widths = c(8, 6), bottom = textGrob("Presupuesto total (millones de euros)", gp = gpar(fontsize = 25, col="black")))

## Representar Valor del cambio en las evaluaciones vs. Numero de proyectos dirigidos a especies
LIFE_IUCN_all_genuine %>%
  # Agrupar por conservacion LIFE o no, presupuesto
  group_by(ConservationProjects, NumProjects, ChangeValue) %>%
  # Contar numero de evaluaciones
  summarise(number_assessment = n()) %>%
  ggplot() +
  # Color de fondo del grafico segun mejora o deterioro
  geom_rect(data = rects, aes(xmin = 0, xmax = 31, ymin = start, ymax = end, fill = col), alpha = 0.2) +
  scale_fill_discrete(guide = "none") +
  # Línea horizontal en y = 0
  geom_hline(yintercept = 0, size = 2) +
  # Grafico de dispersion
  geom_point(aes(x = NumProjects , y = ChangeValue, color = ConservationProjects, size = number_assessment), alpha = 0.6) +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de ejes
  labs(x = "Número de proyectos", y = "Valor del cambio") +
  # Modificar leyenda y texto de la leyenda y los ejes
  theme(legend.title = element_text(size = 20, colour = "black"),
        legend.text = element_text(size = 20, colour = "black"),
        axis.text = element_text(size = 25, colour = "black"),
        axis.title = element_text(size = 25, colour = "black")) +
  # Modificar elementos de la leyenda
  guides(color = guide_legend(override.aes = list(size = 7))) +
  # Modificar relleno de los elementos
  scale_color_viridis_d(name = "LIFE", labels = c("NO", "SÍ"), begin = 0.6, end = 0) +
  # Modificar tamaño de los elementos
  scale_size_continuous(name = "Nº de registros", range = c(4, 20), breaks = c(2, 4, 8, 16))

# Supuesto de normalidad para ANOVA
by(LIFE_IUCN_all_genuine$ChangeValue, LIFE_IUCN_all_genuine$ConservationProjects, shapiro.test) # no se cumple supuesto de normalidad
# Supuesto de homocedasticidad
leveneTest(y = LIFE_IUCN_all_genuine$ChangeValue, group = as.factor(LIFE_IUCN_all_genuine$ConservationProjects), center = "median") # no podemos rechazar la hipotesis nula, varianzas iguales

# Alternativa, test de Kruskal-Wallis
conservation_test <- kruskal.test(formula = ChangeValue~ConservationProjects, data = LIFE_IUCN_all_genuine)
conservation_test

## Representar Tiempo transcurrido entre el comienzo de la conservacion y el cambio de categoria vs. Presupuesto invertido en especies
# Grafico desde 0 a 20 millones de euros
budget_time1 <- LIFE_IUCN_all_genuine %>%
  # Selecionar especies incluidas en proyectos de conservacion LIFE
  filter(ConservationProjects == TRUE) %>%
  # Agrupar por presupuesto, valor del cambio y tiempo transcurrido hasta el cambio
  group_by(SumTotalBudgetWeighted, ChangeValue, TimeforChange) %>%
  # Contar numero de evaluaciones
  summarise(number_assessment = n()) %>%
  # Dividir para representar en millones de euros y añadir columna para clasificar el cambio
  mutate(SumTotalBudgetWeighted = SumTotalBudgetWeighted/1000000,
         Change = ifelse(ChangeValue > 0, "Mejora", "Deterioro")) %>%
  ggplot(aes(x = SumTotalBudgetWeighted , y = TimeforChange, color = Change, size = number_assessment)) +
  # Grafico de dispersion
  geom_point(alpha = 0.6) +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = NULL, y = "Tiempo (años)") +
  # Modificar leyenda y texto de los ejes
  theme(legend.position = "none",
        axis.text = element_text(size = 20, colour = "black"),
        axis.title.x = element_text(size = 25, colour = "black"),
        axis.title.y = element_text(size = 25, colour = "black")) +
  # Modificar elementos de la leyenda
  guides(color = guide_legend(override.aes = list(size = 6))) +
  # Modificar nombre en leyenda del borde de los elementos
  scale_color_discrete(name = "Tipo de cambio" ) +
  # Modificar tamaño de los elementos
  scale_size_continuous(name = "Nº de evaluaciones", range = c(6, 10), breaks = c(1, 2)) +
  # Ajustar eje x
  scale_x_continuous(limits = c(0, 20),
                     breaks = c(0, 5, 10, 15, 20))

# Grafico desde 99 a 105.5 millones de euros
budget_time2 <- LIFE_IUCN_all_genuine %>%
  # Selecionar especies incluidas en proyectos de conservacion LIFE
  filter(ConservationProjects == TRUE) %>%
  # Agrupar por presupuesto, valor del cambio y tiempo transcurrido hasta el cambio
  group_by(SumTotalBudgetWeighted, ChangeValue, TimeforChange) %>%
  # Contar numero de evaluaciones
  summarise(number_assessment = n()) %>%
  # Añadir columna para clasificar el cambio
  mutate(SumTotalBudgetWeighted = SumTotalBudgetWeighted/1000000,
         Change = ifelse(ChangeValue > 0, "Mejora", "Deterioro")) %>%
  ggplot(aes(x = SumTotalBudgetWeighted , y = TimeforChange, color = Change, size = number_assessment)) +
  # Grafico de dispersion
  geom_point(alpha = 0.6) +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = NULL, y = NULL) +
  # Modificar leyenda y texto de los ejes
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.text.y = element_text(size = 20, colour = "white"),
        axis.title = element_text(size = 25, colour = "white")) +
  # Modificar elementos de la leyenda
  guides(color = guide_legend(override.aes = list(size = 6))) +
  # Modificar nombre en leyenda del relleno de los elementos
  scale_color_discrete(name = "Tipo de cambio" ) +
  # Modificar tamaño de los elementos
  scale_size_continuous(name = "Nº de evaluaciones", range = c(6, 10), breaks = c(1, 2)) +
  # Ajustar eje x
  scale_x_continuous(limits = c(99, 105.5),
                     breaks = c(100, 105))

# Combinar las dos partes del grafico del tiempo en producirse el cambio vs. presupuesto
budget_time <- grid.arrange(budget_time1, budget_time2, nrow = 1, widths = c(8, 3), bottom = textGrob("Presupuesto total (millones de euros)", gp = gpar(fontsize = 25, col="black")))

projects_time <- LIFE_IUCN_all_genuine %>%
  # Selecionar especies incluidas en proyectos de conservacion LIFE
  filter(ConservationProjects == TRUE) %>%
  # Agrupar por numero de proyectos, valor del cambio y tiempo transcurrido hasta el cambio
  group_by(NumProjects, ChangeValue, TimeforChange) %>%
  # Contar numero de evaluaciones
  summarise(number_assessment = n()) %>%
  # Añadir columna para clasificar el cambio
  mutate(Change = ifelse(ChangeValue > 0, "Mejora", "Deterioro")) %>%
  ggplot(aes(x = NumProjects , y = TimeforChange, color = Change, size = number_assessment)) +
  # Grafico de dispersion
  geom_point(alpha = 0.6) +
  # Definir tema
  theme_minimal() +
  # Modificar nombres de los ejes
  labs(x = "Número de proyectos", y = "NA") +
  # Modificar texto de la leyenda y los ejes
  theme(legend.title = element_text(size = 20, colour = "black"),
        legend.text = element_text(size = 20, colour = "black"),
        axis.text = element_text(size = 20, colour = "black"),
        axis.title.x = element_text(size = 25, colour = "black"),
        axis.title.y = element_text(size = 15, colour = "white")) +
  # Modificar elementos de la leyenda
  guides(color = guide_legend(override.aes = list(size = 6))) +
  # Modificar nombre en leyenda del borde de los elementos
  scale_color_discrete(name = "Tipo de cambio") +
  # Modificar tamaño de los elementos
  scale_size_continuous(name = "Nº de registros", range = c(6, 10), breaks = c(1, 2))

### Combinar en un unico plot los graficos sobre el tiempo transcurrido hasta el cambio de categoria vs. inversion
plot_grid(budget_time, projects_time, labels = c('AUTO'), label_size = 50, label_x = c(0.875, 0.65), rel_widths = c(1, 1.15))

#### Analisis de los datos de LIFEproject_IUCN_all_genuine ####

projects_test_df <- LIFEproject_IUCN_all_genuine %>%
  # Modificar columna SingleSpeciesProyect
  mutate(SingleSpeciesProyect = ifelse(is.na(SingleSpeciesProyect), "No project",
                                       ifelse(SingleSpeciesProyect == TRUE, "Single-species project", "Multi-species project"))) %>%
  select(Reference, SingleSpeciesProyect, PrvCategory, PstCategory, ChangeValue)

# Supuesto de normalidad para ANOVA
by(projects_test_df$ChangeValue, projects_test_df$SingleSpeciesProyect, shapiro.test) # no se cumple supuesto de normalidad
# Supuesto de homocedasticidad
leveneTest(y = projects_test_df$ChangeValue, group = as.factor(projects_test_df$SingleSpeciesProyect), center = "median") # no podemos rechazar la hipotesis nula, varianzas iguales

# Alternativa, test de Kruskal-Wallis
projects_test <- kruskal.test(formula = ChangeValue~SingleSpeciesProyect, data = projects_test_df) 
projects_test # p-valor < 0.05 significativo, hay diferencias entre grupos

# Comparacion Post-Hoc
pairwise.wilcox.test(x = projects_test_df$ChangeValue, g = projects_test_df$SingleSpeciesProyect, p.adjust.method = "holm" )

############ Funciones empleadas en el script ############

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
