# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(tidyverse)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MDCEV_with_outside_good_Proyecto_Alimentacion",
  modelDescr      = "Modelo MDCEV para EPF alpha-gamma profile with outside good and socio-demographics",
  indivID         = "folio", 
  outputDirectory = "outputs/output_fondecyt_0510",
  mixing          = FALSE 
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #


data <- read.csv("data/processed/base_datos_epf_apollo.csv")
database = data

# Escalar datos
database <- database %>%
  mutate(#cd_0111_1 = new_q_daily_hgr_gasto_diario_hgr_WholeGrains, # Probar con cantidad diaria consumida (ojalá en gramos)
         cd_0111_2 = new_q_daily_hgr_gasto_diario_hgr_RefinedGrains,
         cd_0112   = new_q_daily_hgr_gasto_diario_hgr_StarchyVegetables,
         cd_0113   = new_q_daily_hgr_gasto_diario_hgr_Vegetables,
         cd_0114   = new_q_daily_hgr_gasto_diario_hgr_Fruits,
         cd_0115   = new_q_daily_hgr_gasto_diario_hgr_DairyFoods,
         cd_0116_1 = new_q_daily_hgr_gasto_diario_hgr_Beef_and_lamb,
         cd_0116_2 = new_q_daily_hgr_gasto_diario_hgr_Pork,
         cd_0116_3 = new_q_daily_hgr_gasto_diario_hgr_Chicken_and_other_poultry,
         cd_0116_4 = new_q_daily_hgr_gasto_diario_hgr_Eggs,
         cd_0116_5 = new_q_daily_hgr_gasto_diario_hgr_Fish,
         cd_0116_6 = new_q_daily_hgr_gasto_diario_hgr_Legumes,
         cd_0116_7 = new_q_daily_hgr_gasto_diario_hgr_Nuts,
         cd_0116_8 = new_q_daily_hgr_gasto_diario_hgr_SoyFoods,
         cd_0116_9 = new_q_daily_hgr_gasto_diario_hgr_meatOther,
         cd_0117   = new_q_daily_hgr_gasto_diario_hgr_AddedFats,
         cd_0118   = new_q_daily_hgr_gasto_diario_hgr_AddedSugars, # Se agregan refrescos
         cd_0119   = new_q_daily_hgr_gasto_diario_hgr_Others,
         #gd_0111_1 = gasto_diario_hgr_WholeGrains, # Probar con cantidad diaria consumida
         gd_0111_2 = gasto_diario_hgr_RefinedGrains/1000,
         gd_0112   = gasto_diario_hgr_StarchyVegetables/1000,
         gd_0113   = gasto_diario_hgr_Vegetables/1000,
         gd_0114   = gasto_diario_hgr_Fruits/1000,
         gd_0115   = gasto_diario_hgr_DairyFoods/1000,
         gd_0116_1 = gasto_diario_hgr_Beef_and_lamb/1000,
         gd_0116_2 = gasto_diario_hgr_Pork/1000,
         gd_0116_3 = gasto_diario_hgr_Chicken_and_other_poultry/1000,
         gd_0116_4 = gasto_diario_hgr_Eggs/1000,
         gd_0116_5 = gasto_diario_hgr_Fish/1000,
         gd_0116_6 = gasto_diario_hgr_Legumes/1000,
         gd_0116_7 = gasto_diario_hgr_Nuts/1000,
         gd_0116_8 = gasto_diario_hgr_SoyFoods/1000,
         gd_0116_9 = gasto_diario_hgr_meatOther/1000,
         gd_0117   = gasto_diario_hgr_AddedFats/1000,
         gd_0118   = gasto_diario_hgr_AddedSugars/1000, # Se agregan refrescos
         gd_0119   = gasto_diario_hgr_Others/1000,
#         ingreso   = prom_ing_total_hogar/1000000,
         #kcal_0111_1 = kcal_daily_WholeGrains/1000,
         kcal_0111_2 = kcal_daily_RefinedGrains/1000,
         kcal_0112 = kcal_daily_StarchyVegetables/1000,
         kcal_0113 = kcal_daily_Vegetables/1000,
         kcal_0114 = kcal_daily_Fruits/1000,
         kcal_0115 = kcal_daily_DairyFoods/1000,
         kcal_0116_1 = kcal_daily_Beef_and_lamb/1000,
         kcal_0116_2 = kcal_daily_Pork/1000,
         kcal_0116_3 = kcal_daily_Chicken_and_other_poultry/10000,
         kcal_0116_4 = kcal_daily_Eggs/1000,
         kcal_0116_5 = kcal_daily_Fish/1000,
         kcal_0116_6 = kcal_daily_Legumes/1000,
         kcal_0116_7 = kcal_daily_Nuts/1000,
         kcal_0116_8 = kcal_daily_SoyFoods/1000,
         kcal_0116_9 = kcal_daily_meatOther/1000,
         kcal_0117 = kcal_daily_AddedFats/1000,
         kcal_0118 = kcal_daily_AddedSugars/1000,
         kcal_0119 = kcal_daily_Others/1000) %>% 
  mutate(gd_total = rowSums(select(.,  gd_0111_2, gd_0112, gd_0113, gd_0114, gd_0115,
                                   gd_0116_1, gd_0116_2, gd_0116_3, gd_0116_4, gd_0116_5, gd_0116_6, gd_0116_7, gd_0116_8, gd_0116_9,
                                   gd_0117, gd_0118))) 
  

# Usar precio promedio () por categorías; recalcular el gasto y que converse budget con expenditure 



# Determinar outside good (cantidad consumida en outside good)
database$t_outside = database$gd_0119 # outside good: quantity consumed in other foods

database <- database |> 
  filter(t_outside > 0) |> 
  rename(ingreso = prom_ing_total_hogar,
         npersonas = prom_npersonas) |>
  mutate(budget = gd_total + gd_0119,
         ingreso = log(ingreso + 1)) |> # transformación logarítmica Variable ingreso 
  filter(macrozona == 4)  


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation Configuracion 5
apollo_beta = c(alpha_base                       = 0,
#                gamma_WholeGrains                = 1,
                gamma_RefinedGrains              = 1,
                gamma_StarchyVegetables          = 1,
                gamma_Vegetables                 = 1,
                gamma_Fruits                     = 1,
                gamma_DairyFoods                 = 1,
                gamma_Beef_and_lamb              = 1,
                gamma_Pork                       = 1,
                gamma_Chicken_and_other_poultry  = 1,
                gamma_Eggs                       = 1,
                gamma_Fish                       = 1,
                gamma_Legumes                    = 1,
                gamma_Nuts                       = 1,
                gamma_SoyFoods                   = 1,
                gamma_meatOther                  = 1,
                gamma_AddedFats                  = 1,
                gamma_AddedSugars                = 1,
#                gamma_Others                     = 1,
##Wholegrains
 #               delta_WholeGrains                = 0,
#               delta_WholeGrains_npersonas      = 0,
#               delta_WholeGrains_quintil        = 0,
#                delta_WholeGrains_age            = 0,
#               delta_WholeGrains_edu            = 0,
#                delta_WholeGrains_precio         = 0,
#               delta_WholeGrains_kcal           = 0,
##RefinedGrains
                  delta_RefinedGrains              = 0,
                  delta_RefinedGrains_npersonas    = 0,
                  delta_RefinedGrains_ingreso    = 0,
#                 delta_RefinedGrains_quintil      = 0,
                 delta_RefinedGrains_age          = 0,
                 delta_RefinedGrains_edu          = 0,
#                 delta_RefinedGrains_precio       = 0,
#                 delta_RefinedGrains_kcal         = 0,
##StarchyVegetables
                 delta_StarchyVegetables            = 0,
                 delta_StarchyVegetables_npersonas  = 0,
                 delta_StarchyVegetables_ingreso  = 0,
#                delta_StarchyVegetables_quintil    = 0,
                delta_StarchyVegetables_age        = 0,
                delta_StarchyVegetables_edu        = 0,
#                delta_StarchyVegetables_precio     = 0,
#                delta_StarchyVegetables_kcal       = 0,
##Vegetables
                 delta_Vegetables                   = 0,
                 delta_Vegetables_npersonas         = 0,
                 delta_Vegetables_ingreso         = 0,
#                delta_Vegetables_quintil           = 0,
                delta_Vegetables_age               = 0,
                delta_Vegetables_edu               = 0,
#                delta_Vegetables_precio            = 0,
#                delta_Vegetables_kcal              = 0,
## Fruits
                 delta_Fruits                       = 0,
                 delta_Fruits_npersonas             = 0,
                 delta_Fruits_ingreso             = 0,
#                 delta_Fruits_quintil               = 0,
                 delta_Fruits_age                   = 0,
                 delta_Fruits_edu                   = 0,
#                 delta_Fruits_precio                = 0,
#                delta_Fruits_kcal                  = 0,
##DairyFoods
                delta_DairyFoods                    = 0,
                delta_DairyFoods_npersonas          = 0,
                delta_DairyFoods_ingreso          = 0,
#               delta_DairyFoods_quintil            = 0,
                delta_DairyFoods_age                = 0,
                delta_DairyFoods_edu                = 0,
#                delta_DairyFoods_precio             = 0,
#               delta_DairyFoods_kcal               = 0,
##Beef_and_Lamb
                delta_Beef_and_lamb                 = 0,
                delta_Beef_and_lamb_npersonas       = 0,
                delta_Beef_and_lamb_ingreso         = 0,
#               delta_Beef_and_lamb_quintil         = 0,
                delta_Beef_and_lamb_age             = 0,
                delta_Beef_and_lamb_edu             = 0,
#                delta_Beef_and_lamb_precio          = 0,
#                delta_Beef_and_lamb_kcal           = 0,
##Pork
                delta_Pork                          = 0,
                delta_Pork_npersonas                = 0,
                delta_Pork_ingreso                  = 0,
#               delta_Pork_quintil                  = 0,
                delta_Pork_age                      = 0,
                delta_Pork_edu                      = 0,
#                delta_Pork_precio                   = 0,
#                delta_Pork_kcal                    = 0,

##Chicken_and_poultry
                delta_Chicken_and_other_poultry             = 0,
                delta_Chicken_and_other_poultry_npersonas   = 0,
                delta_Chicken_and_other_poultry_ingreso     = 0,
#                delta_Chicken_and_other_poultry_quintil     = 0,
                delta_Chicken_and_other_poultry_age         = 0,
                delta_Chicken_and_other_poultry_edu         = 0,
#                delta_Chicken_and_other_poultry_precio      = 0,
#                delta_Chicken_and_other_poultry_kcal       = 0,

##Eggs
                delta_Eggs                      = 0,
                delta_Eggs_npersonas            = 0,
                delta_Eggs_ingreso              = 0,
#               delta_Eggs_quintil              = 0,
                delta_Eggs_age                  = 0,
                delta_Eggs_edu                  = 0,
#                delta_Eggs_precio               = 0,
#                delta_Eggs_kcal                = 0,

##Fish
                delta_Fish                      = 0,
                delta_Fish_npersonas            = 0,
                delta_Fish_ingreso            = 0,
#                delta_Fish_quintil              = 0,
                delta_Fish_age                  = 0,
                delta_Fish_edu                  = 0,
#                delta_Fish_precio               = 0,
#                delta_Fish_kcal                = 0,

##Legumes
                delta_Legumes                    = 0,
                delta_Legumes_npersonas          = 0,
                delta_Legumes_ingreso            = 0,
#               delta_Legumes_quintil            = 0,
                delta_Legumes_age                = 0,
                delta_Legumes_edu                = 0,
#                delta_Legumes_precio             = 0,
#                delta_Legumes_kcal              = 0,
##Nuts
                delta_Nuts                       = 0,
                delta_Nuts_npersonas             = 0,
                delta_Nuts_ingreso               = 0,
#               delta_Nuts_quintil               = 0,
                delta_Nuts_age                   = 0,
                delta_Nuts_edu                   = 0,
#                delta_Nuts_precio                = 0,
#                #delta_Nuts_kcal                 = 0,
##SoyFoods
                delta_SoyFoods                   = 0,
                delta_SoyFoods_npersonas         = 0,
                delta_SoyFoods_ingreso           = 0,
#                delta_SoyFoods_quintil           = 0,
                delta_SoyFoods_age               = 0,
                delta_SoyFoods_edu               = 0,
#                delta_SoyFoods_precio            = 0,
#                delta_SoyFoods_kcal             = 0,
#meatOther
                delta_meatOther                  = 0,
                delta_meatOther_npersonas        = 0,
                delta_meatOther_ingreso          = 0,
#                delta_meatOther_quintil          = 0,
                delta_meatOther_age              = 0,
                delta_meatOther_edu              = 0,
#                delta_meatOther_precio           = 0,
#                delta_meatOther_kcal            = 0,
##AddedFats
                delta_AddedFats                  = 0,
                delta_AddedFats_npersonas        = 0,
                delta_AddedFats_ingreso          = 0,
#                delta_AddedFats_quintil          = 0,
                delta_AddedFats_age              = 0,
                delta_AddedFats_edu              = 0,
#                delta_AddedFats_precio           = 0,
#                delta_AddedFats_kcal            = 0,
##AddedSugars
                delta_AddedSugars                = 0,
                delta_AddedSugars_npersonas      = 0,
                delta_AddedSugars_ingreso      = 0,
#                delta_AddedSugars_quintil        = 0,
                delta_AddedSugars_age            = 0,
                delta_AddedSugars_edu            = 0,
#                delta_AddedSugars_precio         = 0,
#                delta_AddedSugars_kcal          = 0,
#                delta_Others                    = 0,
                sig                              = 1)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("alpha_base")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
### Define individual alternatives
  alternatives = c("outside",
# WholeGrains",
                   "RefinedGrains",
                   "StarchyVegetables",
                   "Vegetables",
                   "Fruits",
                   "DairyFoods",
                   "Beef_and_lamb",
                   "Pork",
                   "Chicken_and_other_poultry",
                   "Eggs",
                   "Fish",
                   "Legumes",
                   "Nuts",
                   "SoyFoods",
                   "meatOther",
                   "AddedFats",
                   "AddedSugars")
#                   "Others"
  
  ### Define availabilities
  avail = list(outside = 1,
 #              WholeGrains = 1,
               RefinedGrains = 1,
               StarchyVegetables = 1,
               Vegetables = 1,
               Fruits = 1,
               DairyFoods = 1,
               Beef_and_lamb = 1,
               Pork = 1,
               Chicken_and_other_poultry = 1,
               Eggs = 1,
               Fish = 1,
               Legumes = 1,
               Nuts = 1,
               SoyFoods = 1,
               meatOther = 1,
               AddedFats = 1,
               AddedSugars = 1)
#               Others = 1)
  
  ### Define continuous consumption for individual alternatives
  continuousChoice = list(outside    = t_outside,
#                          WholeGrains = cd_0111_1,
                          RefinedGrains = cd_0111_2,
                          StarchyVegetables = cd_0112,
                          Vegetables = cd_0113,
                          Fruits = cd_0114,
                          DairyFoods = cd_0115,
                          Beef_and_lamb = cd_0116_1,
                          Pork = cd_0116_2,
                          Chicken_and_other_poultry = cd_0116_3,
                          Eggs = cd_0116_4,
                          Fish = cd_0116_5,
                          Legumes = cd_0116_6,
                          Nuts = cd_0116_7,
                          SoyFoods = cd_0116_8,
                          meatOther = cd_0116_9,
                          AddedFats = cd_0117,
                          AddedSugars = cd_0118)
#                          Others = qd_0119)
                          
   ### Define utilities for individual alternatives
  V = list()
  
  V[["outside"]]  = 0
  
 # V[["WholeGrains"]] = delta_WholeGrains +  delta_WholeGrains_npersonas * npersonas #+ delta_WholeGrains_quintil * quintil + delta_WholeGrains_age * average_age + delta_WholeGrains_edu * educ_promedio + delta_WholeGrains_precio * precio_por_gr_g_WholeGrains #+ delta_WholeGrains_kcal * kcal_0111_1
    
  V[["RefinedGrains"]] = delta_RefinedGrains +  delta_RefinedGrains_npersonas * npersonas + delta_RefinedGrains_age * average_age + delta_RefinedGrains_edu * educ_promedio+ delta_RefinedGrains_ingreso * ingreso #+ delta_RefinedGrains_kcal * kcal_0111_2
  
  V[["StarchyVegetables"]] = delta_StarchyVegetables +  delta_StarchyVegetables_npersonas * npersonas + delta_StarchyVegetables_age * average_age + delta_StarchyVegetables_edu * educ_promedio + delta_StarchyVegetables_ingreso * ingreso #+ delta_StarchyVegetables_kcal * kcal_0112
  
  V[["Vegetables"]] = delta_Vegetables + delta_Vegetables_npersonas * npersonas + delta_Vegetables_age * average_age +  delta_Vegetables_edu * educ_promedio + delta_Vegetables_ingreso * ingreso #+ delta_Vegetables_kcal * kcal_0113
  
  V[["Fruits"]] = delta_Fruits + delta_Fruits_npersonas * npersonas + delta_Fruits_age * average_age + delta_Fruits_edu * educ_promedio + delta_Fruits_ingreso * ingreso #+ delta_Fruits_kcal * kcal_0114
  
  V[["DairyFoods"]] = delta_DairyFoods +  delta_DairyFoods_npersonas * npersonas + delta_DairyFoods_age * average_age + delta_DairyFoods_edu * educ_promedio+ delta_DairyFoods_ingreso * ingreso #+ delta_DairyFoods_kcal * kcal_0115
  
  V[["Beef_and_lamb"]] = delta_Beef_and_lamb + delta_Beef_and_lamb_npersonas * npersonas + delta_Beef_and_lamb_age * average_age + delta_Beef_and_lamb_edu * educ_promedio + delta_Beef_and_lamb_ingreso * ingreso #+ delta_Beef_and_lamb_kcal * kcal_0116_1
  
  V[["Pork"]] = delta_Pork  + delta_Pork_npersonas * npersonas + delta_Pork_age * average_age + delta_Pork_edu * educ_promedio + delta_Pork_ingreso * ingreso #+ delta_Pork_kcal * kcal_0116_2
  
  V[["Chicken_and_other_poultry"]] = delta_Chicken_and_other_poultry + delta_Chicken_and_other_poultry_npersonas * npersonas + delta_Chicken_and_other_poultry_age * average_age + delta_Chicken_and_other_poultry_edu * educ_promedio + delta_Chicken_and_other_poultry_ingreso * ingreso  #+ delta_Chicken_and_other_poultry_kcal * kcal_0116_3
  
  V[["Eggs"]] = delta_Eggs +  delta_Eggs_npersonas * npersonas + delta_Eggs_age * average_age + delta_Eggs_edu * educ_promedio+ delta_Eggs_ingreso * ingreso  #+ delta_Eggs_kcal * kcal_0116_4
  
  V[["Fish"]] = delta_Fish +  delta_Fish_npersonas * npersonas + delta_Fish_age * average_age + delta_Fish_edu * educ_promedio + delta_Fish_ingreso * ingreso #+ delta_Fish_kcal * kcal_0116_5
  
  V[["Legumes"]] = delta_Legumes +  delta_Legumes_npersonas * npersonas + delta_Legumes_age * average_age + delta_Legumes_edu * educ_promedio + delta_Legumes_ingreso * ingreso #+ delta_Legumes_kcal * kcal_0116_6
  
  V[["Nuts"]] = delta_Nuts + delta_Nuts_npersonas * npersonas + delta_Nuts_age * average_age + delta_Nuts_edu * educ_promedio + delta_Nuts_ingreso * ingreso #+ delta_Nuts_kcal * kcal_0116_7
  
  V[["SoyFoods"]] = delta_SoyFoods  + delta_SoyFoods_npersonas * npersonas + delta_SoyFoods_age * average_age +  delta_SoyFoods_edu * educ_promedio + delta_SoyFoods_ingreso * ingreso #  delta_SoyFoods_kcal * kcal_0116_8
  
  V[["meatOther"]] = delta_meatOther + delta_meatOther_npersonas * npersonas + delta_meatOther_age * average_age + delta_meatOther_edu * educ_promedio + delta_meatOther_ingreso * ingreso #+ delta_meatOther_kcal * kcal_0116_9
  
  V[["AddedFats"]] = delta_AddedFats + delta_AddedFats_npersonas * npersonas + delta_AddedFats_age * average_age + delta_AddedFats_edu * educ_promedio + delta_AddedFats_ingreso * ingreso #+ delta_AddedFats_kcal * kcal_0117
  
  V[["AddedSugars"]] = delta_AddedSugars + delta_AddedSugars_npersonas * npersonas + delta_AddedSugars_age * average_age + delta_AddedSugars_edu * educ_promedio + delta_AddedSugars_ingreso * ingreso #+ delta_AddedSugars_kcal * kcal_0118
  
#  V[["Others"]] = delta_Others 
  
  ### Define alpha parameters (No se hicieron cambios en las formulas)
  alpha = list(outside     = alpha_base ,
 #              WholeGrains      = alpha_base, 
               RefinedGrains      = alpha_base, 
               StarchyVegetables     = alpha_base, 
               Vegetables     = alpha_base, 
               Fruits      = alpha_base,
               DairyFoods      = alpha_base,
               Beef_and_lamb  = alpha_base, 
               Pork      = alpha_base, 
               Chicken_and_other_poultry = alpha_base,
               Eggs  = alpha_base, 
               Fish  = alpha_base, 
               Legumes  = alpha_base,
               Nuts  = alpha_base, 
               SoyFoods  = alpha_base, 
               meatOther  = alpha_base, 
               AddedFats  = alpha_base, 
               AddedSugars  = alpha_base)
#               Others  = alpha_base)

  
  ### Define gamma parameters
  gamma = list(#WholeGrains      = gamma_WholeGrains,
               RefinedGrains      = gamma_RefinedGrains,
               StarchyVegetables     = gamma_StarchyVegetables,
               Vegetables     = gamma_Vegetables,
               Fruits      = gamma_Fruits,
               DairyFoods      = gamma_DairyFoods,
               Beef_and_lamb  = gamma_Beef_and_lamb,
               Pork      = gamma_Pork,
               Chicken_and_other_poultry  = gamma_Chicken_and_other_poultry,
               Eggs  = gamma_Eggs,
               Fish  = gamma_Fish,
               Legumes  = gamma_Legumes,
               Nuts  = gamma_Nuts,
               SoyFoods  = gamma_SoyFoods,
               meatOther  = gamma_meatOther,
               AddedFats  = gamma_AddedFats,
               AddedSugars  = gamma_AddedSugars)
#               Others  = gamma_Others)
 
  
  ### Define costs for individual alternatives
  cost = list(outside = 1, 
 #             WholeGrains = precio_mediana_WholeGrains, # precio por unidad consumida
              RefinedGrains = precio_mediana_RefinedGrains/1000,
              StarchyVegetables = precio_mediana_StarchyVegetables/1000,
              Vegetables = precio_mediana_Vegetables/1000,
              Fruits = precio_mediana_Fruits/1000,
              DairyFoods = precio_mediana_DairyFoods/1000,
              Beef_and_lamb = precio_mediana_Beef_and_lamb/1000,
              Pork = precio_mediana_Pork/1000,
              Chicken_and_other_poultry = precio_mediana_Chicken_and_other_poultry/1000,
              Eggs = precio_mediana_Eggs/1000,
              Fish = precio_mediana_Fish/1000,
              Legumes = precio_mediana_Legumes/1000,
              Nuts = precio_mediana_Nuts/1000,
              SoyFoods = precio_mediana_SoyFoods/1000,
              meatOther = precio_mediana_meatOther/1000,
              AddedFats = precio_mediana_AddedFats/1000,
              AddedSugars = precio_mediana_AddedSugars/1000)
#              Others = 1)
  
  ### Define settings for MDCEV model (En budget se uso el promedio de ingreso total por hogar)
  mdcev_settings <- list(alternatives      = alternatives,
                         avail             = avail,
                         continuousChoice  = continuousChoice,
                         utilities         = V,
                         alpha             = alpha,
                         gamma             = gamma, 
                         sigma             = sig, 
                         cost              = cost,
                         budget            = budget)
  
  ### Compute probabilities using MDCEV model
  P[["model"]] = apollo_mdcev(mdcev_settings, functionality)
  
  ### Se eliminó línea que tenía la función de juntar todos los valores de los datos de un mismo usuario (mismo usuario, distintos días)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model_fondecyt = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
#                                 estimate_settings = list(estimationRoutine = "bfgs"))

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model_fondecyt, modelOutput_settings = list(printPVal = TRUE))

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model_fondecyt,  saveOutput_settings = list(printPVal = TRUE))

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
apollo_sink()

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS                                          ----
# ----------------------------------------------------------------- #

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model_fondecyt, apollo_probabilities, apollo_inputs)

#colMeans(predictions_base)

######imagine aumenta los gastos en carne de vacuno por alza de impuestos---------------------------------------------------##


# nueva variable ni?os=0
database$precio_mediana_Beef_and_lamb = 2*database$precio_mediana_Beef_and_lamb
apollo_inputs = apollo_validateInputs()


### Rerun predictions with the new data, and save into a separate matrix
predictions_incBeef = apollo_prediction(model_fondecyt, apollo_probabilities, apollo_inputs)

#colMeans(predictions_incBeef)

# retornar a la data original
database$precio_mediana_Beef_and_lamb = 1/2*database$precio_mediana_Beef_and_lamb


# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

apollo_sink()