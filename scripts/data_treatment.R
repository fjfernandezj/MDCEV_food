#############################################################
#               Preparación Base de Datos Apollo            #
#                   Fuente: Base de datos EPF               #
#############################################################

# library
library("tidyverse")
library("rlang")
library("haven")
library("labelled")
library("survey")
library("srvyr")
library("calidad")
library("scales")
library("Hmisc")
library("readxl")


#############################################################
#               Lectura de Datos                            #
# Bases de datos gastos, personas, cantidades y ccif        #
#############################################################
### Gastos
base_gastos <- haven::read_dta(file = "data/raw/epf/base-gastos-ix-epf-stata.dta") |> 
  # Filtrar para que queden solo las divisiones 01 (Alimentos y bebidas no alcohólicas) y 11 (Servicios de Restaurantes y Alojamiento)
  filter(d %in% c("01", "11")) %>%
  # Excluir categorías que contengan las palabras específicas relacionadas a alojamiento
  filter(
    !str_detect(glosa_ccif, regex("HOTEL|HOTELES|ALOJAMIENTO|MOTELES|RESORTS|CAMPING|CAMPINGS|CABAÑAS|ALBERGUES|HOSTALES", ignore_case = TRUE)))

### Cantidades
base_cantidades <- haven::read_dta(file = "data/raw/epf/base-cantidades-ix-epf-stata.dta") |> 
  filter(d %in% c("01", "11")) |>  # Se filtran para que queden solo las divisiones 1 (Alimentos y bebidas no alcohólicas) y 11 (Servicios de Restaurantes y Alojamiento)
  # Excluir categorías que contengan las palabras específicas relacionadas a alojamiento
  filter(
    !str_detect(glosa_ccif, regex("HOTEL|HOTELES|ALOJAMIENTO|MOTELES|RESORTS|CAMPING|CAMPINGS|CABAÑAS|ALBERGUES|HOSTALES", ignore_case = TRUE)))

### Personas
base_personas <- haven::read_dta(file = "data/raw/epf/base-personas-ix-epf-stata.dta")

### Datos CCIF
base_ccif <- haven::read_dta(file = "data/raw/epf/ccif-ix-epf-stata.dta")

### Nueva categorización
# De manera externa se realiza nueva categorización de alimentos para aproximarse a categorización de Willet et al. (2019)
nvaCateg <- read_xlsx("data/raw/mappings/mapping_consumo_final.xlsx") |>
  rename(ccif = "CÓDIGO CCIF", # Se renombran variables para que coincida con bases de datos de EPF
         glosa_ccif = "GLOSA")

names(nvaCateg)
#############################################################
#                Integración de Datos                       #
#              Gastos - Nueva Categoría                     #
#############################################################

### Integrar nueva categorización a bases de datos Gastos
base_gastos <- base_gastos |>  
  # Unir con la nueva categorización
  left_join(nvaCateg, by = c("ccif", "glosa_ccif")) |> 
  mutate(NvaCtgria2 = replace_na(NvaCtgria2, "Others")) # Reemplazar NA por "Others" en las columnas de la nueva categorización

## Verificación 
head(base_gastos)
base_gastos$NvaCtgria2 <- as.factor(base_gastos$NvaCtgria2)
summary(base_gastos)


#############################################################
#                Integración de Datos                       #
#              Cantidades - Nueva Categoría                 #
#############################################################

## Para base de datos de Cantidades
base_cantidades <- base_cantidades |> 
  left_join(nvaCateg, by = c("ccif", "glosa_ccif")) |> 
  mutate(NvaCtgria2 = replace_na(NvaCtgria2, "Others")) # Reemplazar NA por "Others" en las columnas de la nueva categorización

## Verificación 
head(base_cantidades)
base_cantidades$NvaCtgria2 <- as.factor(base_cantidades$NvaCtgria2)
summary(base_cantidades)


#############################################################
#                Preparacion de Datos                       #
#                   Base personas                           #
#############################################################
## Características por hogar --> Variables explicativas por hogar para base Apollo
caracteristicas_por_hogar <- base_personas |> 
  select(macrozona, folio, cse, npersonas, edad,mh09, edue, petg,
         ing_total_hogar_hd, ing_total_hog_hd_pc, gastot_hd_pc, gastot_hd) |> 
  group_by(macrozona, folio) |> 
  summarise(npersonas = mean(npersonas, na.rm = TRUE),
            num_ninos_NoEdad_trabajar = sum(petg == 2), # menores de 15 años
            children_0_6 = sum(edad >= 0 & edad <= 6),
            children_7_15 = sum(edad >= 7 & edad <= 15),
            children_15_older = sum(edad > 15 & edad <= 18),
            ing_total_hogar = mean(ing_total_hogar_hd, na.rm = TRUE),
            ing_pc = mean(ing_total_hog_hd_pc, na.rm = TRUE),
            clasf_se = mean(cse),
            average_age = round(mean(edad), 0),
            indigena_hogar = ifelse(any(mh09 == 1), 1, 0),
            educ_promedio = mean(edue),
            ratio_trabajo = sum(petg == 1)/npersonas,
            gasto_tot_hogar = mean(gastot_hd),
            gastot_pc = mean(gastot_hd_pc)) |> 
  mutate(quintil = ntile(ing_total_hogar, 5))


#############################################################
#                Tratamiento Base_Gastos                    #
#                   Para BD Apollo                          #
#############################################################
gastos_apollo <- base_gastos |> 
  group_by(folio, NvaCtgria2) |> 
  summarise(gasto_por_categoria = sum(gasto, na.rm = TRUE)) |> 
  spread(NvaCtgria2, gasto_por_categoria) |> 
  # Renombrar columnas (añadir "g_" a la segunda columna en adelante)
  rename_with(~ paste0("g_", .), -folio) |> 
  mutate_at(vars(starts_with("g")), ~replace_na(., 0))

# Aplicar el supuesto del 25% para WholeGrains en la base de gastos
# y ajustar por la diferencia de precio
# gastos_apollo <- gastos_apollo %>%
#  mutate(
    # Calcular el gasto equivalente al 25% de la cantidad de RefinedGrains
#    gasto_equivalente_wholegrains = 0.25 * g_RefinedGrains,
    
    # Ajustar el gasto de WholeGrains considerando que son 25% más caros
#    g_WholeGrains = gasto_equivalente_wholegrains * 1.25,
    
    # Restar el gasto equivalente (sin ajuste de precio) de RefinedGrains
#    g_RefinedGrains = g_RefinedGrains - gasto_equivalente_wholegrains,
    
    # Eliminar la columna temporal
#    .keep = "unused"
#  )

###Es importante notar que el supuesto en diferencia de precio se obtiene de Harriman (2012) Shrinking the Price Gap for Whole Grains. (https://www.cerealsgrains.org/publications/plexus/cfwplexus/library/books/Documents/WholeGrainsSummit2012/CPLEX-2013-1001-17B.pdf) Este enfoque asegura que:

# El 25% de la cantidad de granos se reclasifica como WholeGrains.
# El gasto en WholeGrains refleja su mayor precio (25% más caro).
# El gasto total aumenta ligeramente debido al mayor costo de los WholeGrains. diferencia.

#############################################################
#                Tratamiento Base_Cantidades                #
#                   Para BD Apollo                          #
#############################################################
factor_conversion <- 1 # Asumiendo factor de conversión 1 ML = 1GR (REVISAR!!!!)

cantidades_apollo <- base_cantidades |> 
  filter(!unidad_medida %in% c("COMPRA", "UNIDAD")) |> 
  mutate(cantidad = if_else(unidad_medida == "ML", cantidad * factor_conversion, cantidad),
         unidad_medida = if_else(unidad_medida == "ML", "GR", unidad_medida)) |> 
  group_by(folio, NvaCtgria2) |> 
  summarise(cantidad_por_categoria = sum(cantidad, na.rm = TRUE)) |> 
  spread(NvaCtgria2, cantidad_por_categoria) |> 
  # Renombrar columnas (añadir "c_" a la segunda columna en adelante)
  rename_with(~ paste0("c_", .), -folio) |> 
  mutate_at(vars(starts_with("c")), ~replace_na(., 0))


## Supuestos para aproximarnos al consumo de WholegRains
###################################################################################################################################################################
# Primera aproximación
# Se Considera WholeGrains a Maiz, Avena y Quinoa. Luego se considera que el 25% categorizado como refined Grain es en efecto consumido como WholeGrain
# Supuesto basado en Fisberg et al (2022) y Sawicki et al (2021) 
## Total and whole grain intake in Latin America: findings from the multicenter cross-sectional Latin American Study of Health and Nutrition (ELANS)
## Whole- and Refined-Grain Consumption and Longitudinal Changes in Cardiometabolic Risk Factors in the Framingham Offspring Cohort
###################################################################################################################################################################
#cantidades_apollo <- cantidades_apollo %>%
#  mutate(
#    # Calcular el 25% de c_RefinedGrains
#    porcentaje_25_refined = 0.25 * c_RefinedGrains,
    
    # Restar el 25% de c_RefinedGrains
#    c_RefinedGrains = c_RefinedGrains - porcentaje_25_refined,
    
    # Sumar el 25% a c_WholeGrains
#    c_WholeGrains = c_WholeGrains + porcentaje_25_refined
#  ) |> 
# Eliminar la columna temporal porcentaje_5_refined
#  select(-porcentaje_25_refined)


  ###################################################################################################################################################################
  # Segunda aproximación
  # 
  ###################################################################################################################################################################


## IMPORTANTE: La BD de cantidades de EPF no identifica si productos como ARROZ o Pan son integrales (Whole grains) o refinados o "blancos" (Refined grains)
## Ante esto asumimos que un 25% del consumo informado de este tipo de alimento es de tipo Whole Grain (integral)
## 
#cantidades_apollo <- cantidades_apollo %>%
#  mutate(
    # Calcular el 25% de c_RefinedGrains
 #   porcentaje_5_refined = 0.25 * c_RefinedGrains,
    
    # Restar el 25% de c_RefinedGrains
#    c_RefinedGrains = c_RefinedGrains - c_WholeGrains
    
    # Sumar el 5% a c_WholeGrains
#    c_WholeGrains = c_WholeGrains + porcentaje_5_refined
#  )
  # Eliminar la columna temporal porcentaje_5_refined
#  select(-porcentaje_5_refined)



#############################################################
#                  Unión Base de Datos                      #
#                   Para BD Apollo                          #
#############################################################
## Union base de datos gastos - caracteristicas por hogar
base_datos_gastos_apollo <- inner_join(gastos_apollo, caracteristicas_por_hogar, by = "folio") 

## Union base de base_datos_gastos_apollo - cantidades apollo
base_datos_apollo <- inner_join(base_datos_gastos_apollo, cantidades_apollo, by = "folio") 


#############################################################
#              Creación Nuevas Variables                    #
#                   Para BD Apollo                          #
#############################################################

# Calcular gramos por día por persona para cada categoría de alimentos
base_datos_apollo <- base_datos_apollo |> 
  mutate(across(starts_with("c_"), ~ (. / npersonas)/30, .names = "c_daily_{sub('c_', '', col)}"))


# Verifica el resultado
select(base_datos_apollo, folio, starts_with("c_daily_")) %>% head()

names(base_datos_apollo)

###----

# Cálculo de ingesta calórica por grupo de alimento
# Se Define vector con las calorías por gramo (promedio) para cada grupo de alimentos
calories_per_gram <- c(
  c_daily_AddedFats = 9.0,
  c_daily_AddedSugars = 4.0,
  c_daily_Beef_and_lamb = 2.5,
  c_daily_Chicken_and_other_poultry = 2.3,
  c_daily_DairyFoods = 0.7,
  c_daily_Eggs = 1.4,
  c_daily_Fish = 2.0,
  c_daily_Fruits = 0.5,
  c_daily_Legumes = 3.5,
  c_daily_meatOther = 2.3,
  c_daily_Nuts = 5.5,
  c_daily_Pork = 2.4,
  c_daily_RefinedGrains = 3.6,
  c_daily_SoyFoods = 3.0,
  c_daily_StarchyVegetables = 0.9,
  c_daily_Vegetables = 0.3,
#  c_daily_WholeGrains = 3.3,
  c_daily_Others = 2.77
)

# Calcular el consumo calórico diario (kcal/día) para cada grupo de alimentos
base_datos_apollo <- base_datos_apollo %>%
  mutate(across(starts_with("c_daily_"), ~ . * calories_per_gram[cur_column()], .names = "kcal_{sub('c_', '', col)}"))

# Verifica el resultado                           {sub('c_', '', col)}"))
select(base_datos_apollo, folio, starts_with("kcal_")) %>% head()

# Sumar el consumo calórico total diario por persona
base_datos_apollo <- base_datos_apollo %>%
  rowwise() %>%
  mutate(kcal_total = sum(c_across(starts_with("kcal_")), na.rm = TRUE)) %>%
  ungroup()

# Crear nuevas variables de gasto diario y cantidad diaria por hogar (no por persona), excluyendo c_daily_*
base_datos_apollo <- base_datos_apollo %>%
  mutate(across(starts_with("g_"), ~ . / 15, .names = "gasto_diario_hgr_{sub('g_', '', col)}")) %>%
  mutate(across(starts_with("c_") & !starts_with("c_daily_"), ~ . / 30, .names = "cantidad_diaria_hgr_{sub('c_', '', col)}"))

# Asegurarnos de que todas las columnas están correctamente creadas antes de la siguiente mutación
print(names(base_datos_apollo))

################################################################################# Aca seguir mañana
# Calcular el precio por gramo por día por hogar para cada grupo de alimentos
base_datos_apollo <- base_datos_apollo |> 
  mutate(across(starts_with("gasto_diario_hgr_"), 
                ~ ifelse(get(paste0("cantidad_diaria_hgr", sub("gasto_diario_hgr", "", cur_column()))) > 0,
                         . / get(paste0("cantidad_diaria_hgr", sub("gasto_diario_hgr", "", cur_column()))),
                         NA),
                .names = "precio_por_gr_{sub('gasto_diario_hgr_', '', col)}")) |> 
  # Filtrar valores extremos (opcional)
  # filter(precio_por_gr_Chicken_and_other_poultry < 10) |> 
  # Calcular precio promedio por categoría
  mutate(across(
    starts_with("precio_por_gr_"),
    ~median(., na.rm = TRUE),
    .names = "precio_mediana_{sub('precio_por_gr_', '', col)}"
  ))

# Recalcular nueva cantidad diaria en base a precio medio 
base_datos_apollo <- base_datos_apollo |> 
  mutate(across(starts_with("gasto_diario_hgr_"),
               ~. / get(paste0("precio_mediana_", sub("gasto_diario_hgr_", "", cur_column()))),
                .names = "new_q_daily_hgr_{sub('precio_mediana_', '', col)}"))

base_datos_apollo |> 
  select(gasto_diario_hgr_Chicken_and_other_poultry, c_daily_Chicken_and_other_poultry, cantidad_diaria_hgr_Chicken_and_other_poultry, 
         precio_por_gr_Chicken_and_other_poultry, precio_mediana_Chicken_and_other_poultry, new_q_daily_hgr_gasto_diario_hgr_Chicken_and_other_poultry)




#Guardar archivo csv para Apollo
write.csv(base_datos_apollo, "data/processed/base_datos_epf_apollo.csv")


############################################################# 
#               Estadisticas descriptivas                   #
#                                                           #
#############################################################

names(base_datos_apollo)

## Figure 1 -  Gastos en fuentes de proteína por macrozona y Nivel socio económico
base_datos_apollo |> 
  group_by(macrozona, clasf_se) |> 
  summarise(Beef = mean(g_Beef_and_lamb),
            Poultry = mean(g_Chicken_and_other_poultry),
            Eggs = mean(g_Eggs),
            Fish = mean(g_Fish),
            Legumes = mean(g_Legumes),
            Nuts = mean(g_Nuts),
            Pork = mean(g_Pork),
            Soy = mean(g_SoyFoods)) |> 
  gather(Grupo, "Gasto", Beef:Soy) |> 
  ggplot(aes(Gasto, fct_reorder(Grupo, Gasto)))+
  geom_col(aes(fill = as.factor(clasf_se)), position = "dodge")+
  facet_wrap(~macrozona, labeller = labeller(macrozona = 
                                               c("1" = "North",
                                                 "2" = "Santiago",
                                                 "3" = "Center",
                                                 "4" = "South")))+
  xlab("Average Expenditure in Protein Sources ($CLP)")+
  ylab("Protein Sources")+
  scale_fill_discrete(name = "Socio-economic Class.",
                      labels = c("Low", "Medium", "High"))+
  theme(text=element_text(size=21))


## Figure 2 -  Cantidad consumida de fuentes de proteína por macrozona y Nivel socio económico
base_datos_apollo |> 
  group_by(macrozona, clasf_se) |> 
  summarise(Beef = mean(c_Beef_and_lamb),
            Poultry = mean(c_Chicken_and_other_poultry),
            Eggs = mean(c_Eggs),
            Fish = mean(c_Fish),
            Legumes = mean(c_Legumes),
            Nuts = mean(c_Nuts),
            Pork = mean(c_Pork),
            Soy = mean(c_SoyFoods)) |> 
  gather(Grupo, "Cantidad", Beef:Soy) |> 
  ggplot(aes(Cantidad, fct_reorder(Grupo, Cantidad)))+
  geom_col(aes(fill = as.factor(clasf_se)), position = "dodge")+
  facet_wrap(~macrozona, labeller = labeller(macrozona = 
                                               c("1" = "North",
                                                 "2" = "Santiago",
                                                 "3" = "Center",
                                                 "4" = "South")))+
  xlab("Average Quantity consumed in Protein Sources (GR)")+
  ylab("Protein Sources")+
  scale_fill_discrete(name = "Socio-economic Class.",
                      labels = c("Low", "Medium", "High"))+
  theme(text=element_text(size=21))

# Tabla resumen para comprar con Willet et al., (2019)
tabla_resumen <- base_datos_apollo %>%
  summarise(
#    WholeGrains_g = mean(c_daily_WholeGrains, na.rm = TRUE),
#    WholeGrains_kcal = mean(kcal_daily_WholeGrains, na.rm = TRUE),
    
    RefinedGrains_g = mean(c_daily_RefinedGrains, na.rm = TRUE),
    RefinedGrains_kcal = mean(kcal_daily_RefinedGrains, na.rm = TRUE),
    
    StarchyVegetables_g = mean(c_daily_StarchyVegetables, na.rm = TRUE),
    StarchyVegetables_kcal = mean(kcal_daily_StarchyVegetables, na.rm = TRUE),
    
    Vegetables_g = mean(c_daily_Vegetables, na.rm = TRUE),
    Vegetables_kcal = mean(kcal_daily_Vegetables, na.rm = TRUE),
    
    Fruits_g = mean(c_daily_Fruits, na.rm = TRUE),
    Fruits_kcal = mean(kcal_daily_Fruits, na.rm = TRUE),
    
    DairyFoods_g = mean(c_daily_DairyFoods, na.rm = TRUE),
    DairyFoods_kcal = mean(kcal_daily_DairyFoods, na.rm = TRUE),
    
    Beef_and_lamb_g = mean(c_daily_Beef_and_lamb, na.rm = TRUE),
    Beef_and_lamb_kcal = mean(kcal_daily_Beef_and_lamb, na.rm = TRUE),
    
    Pork_g = mean(c_daily_Pork, na.rm = TRUE),
    Pork_kcal = mean(kcal_daily_Pork, na.rm = TRUE),
    
    Chicken_and_other_poultry_g = mean(c_daily_Chicken_and_other_poultry, na.rm = TRUE),
    Chicken_and_other_poultry_kcal = mean(kcal_daily_Chicken_and_other_poultry, na.rm = TRUE),
    
    Eggs_g = mean(c_daily_Eggs, na.rm = TRUE),
    Eggs_kcal = mean(kcal_daily_Eggs, na.rm = TRUE),
    
    Fish_g = mean(c_daily_Fish, na.rm = TRUE),
    Fish_kcal = mean(kcal_daily_Fish, na.rm = TRUE),
    
    Legumes_g = mean(c_daily_Legumes, na.rm = TRUE),
    Legumes_kcal = mean(kcal_daily_Legumes, na.rm = TRUE),
    
    SoyFoods_g = mean(c_daily_SoyFoods, na.rm = TRUE),
    SoyFoods_kcal = mean(kcal_daily_SoyFoods, na.rm = TRUE),
    
    Nuts_g = mean(c_daily_Nuts, na.rm = TRUE),
    Nuts_kcal = mean(kcal_daily_Nuts, na.rm = TRUE),
    
    AddedFats_g = mean(c_daily_AddedFats, na.rm = TRUE),
    AddedFats_kcal = mean(kcal_daily_AddedFats, na.rm = TRUE),
    
    AddedSugars_g = mean(c_daily_AddedSugars, na.rm = TRUE),
    AddedSugars_kcal = mean(kcal_daily_AddedSugars, na.rm = TRUE)
  ) |> 
  mutate(porcentaje_25_refined = 0.25 * RefinedGrains_g,
         RefinedGrains_g = RefinedGrains_g - porcentaje_25_refined,
         WholeGrains_g = porcentaje_25_refined,
         WholeGrains_kcal = WholeGrains_g * 3.3) |> 
  select(-porcentaje_25_refined) |> 
  relocate(WholeGrains_g) |> 
  relocate(WholeGrains_kcal, .after = WholeGrains_g)

# Convertir a una tabla más legible
tabla_resumen_long <- tibble::tibble(
  Category = c("Whole grains", "Refined Graines", "Starchy vegetables", "Vegetables", "Fruits", "Dairy foods",
               "Beef and lamb", "Pork", "Chicken and other poultry", "Eggs", "Fish", 
               "Legumes", "Soy foods", "Nuts", "Added fats", "Added sugars"),
  `Macronutrient intake (g/day)` = unlist(tabla_resumen[seq(1, ncol(tabla_resumen), 2)]),
  `Caloric intake (kcal/day)` = unlist(tabla_resumen[seq(2, ncol(tabla_resumen), 2)])
)

#Figura 3
# Datos de la tabla de referencia (Healthy reference diet) Willet et al., 2019 --> Table 1
## IMPORTANTE!!: ASUMIMOS QUE 10% DEL CONSUMO SUGERIDO POR WILLET ET AL (2019) PARA WHOLE GRAINS PUEDE SER CONSUMIDO COMO REFINED GRAINS
tabla_referencia <- tibble::tibble(
  Category = c("Whole grains", "Refined Graines", "Starchy vegetables", "Vegetables", "Fruits", "Dairy foods",
               "Beef and lamb", "Pork", "Chicken and other poultry", "Eggs", "Fish", 
               "Legumes", "Soy foods", "Nuts", "Added fats", "Added sugars"),
  `Macronutrient intake (g/day)` = c(208.8, 23.2, 50, 300, 200, 250, 7, 7, 29, 13, 28, 50, 25, 25, 54, 31),
  `Caloric intake (kcal/day)` = c(729.9, 81.1, 39, 78, 126, 153, 15, 15, 62, 19, 40, 172, 112, 149, 450, 120) # 78 es la suma de caloric intake informado en la tabla para vegetables
)

# Aseguramos que ambas tablas tienen las mismas categorías
tabla_referencia$Category <- factor(tabla_referencia$Category, levels = tabla_resumen_long$Category)
tabla_resumen_long$Category <- factor(tabla_resumen_long$Category, levels = tabla_referencia$Category)

# Combinar ambas tablas en un solo dataframe largo para ggplot2
comparacion <- dplyr::bind_rows(
  tabla_resumen_long %>% mutate(Source = "Calculated"),
  tabla_referencia %>% mutate(Source = "Reference")
)

# Comparación de Macronutrient intake (g/day)
ggplot(comparacion, aes(x = Category, y = `Macronutrient intake (g/day)`, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de Ingesta de Macronutrientes (g/día)",
       x = "Categoría de Alimento", y = "Ingesta (g/día)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Comparación de Caloric intake (kcal/day)
ggplot(comparacion, aes(x = Category, y = `Caloric intake (kcal/day)`, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de Ingesta Calórica (kcal/día)",
       x = "Categoría de Alimento", y = "Ingesta Calórica (kcal/día)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        legend.text=element_text(size=16),
        legend.title=element_text(size=18))



str(base_datos_apollo)
