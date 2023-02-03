##Bottom Trawl  - Oceana
library(tidyverse)

# Clear workspace
rm(list = ls())

##Nutrient supply
ave_trawl_biomass_nutrients <- read_csv("outputs/ave_trawl_biomass_nutrients.csv")

countries_ISO <- read.csv("data/countries_ISO.csv", encoding = "UTF-8") %>% 
  distinct(missing_countries, .keep_all = T)

pop = world_bank_pop %>% 
  filter(indicator == "SP.POP.TOTL") %>% 
  select(country, `2017`) %>% 
  rename(pop = `2017`,
         iso3c = country)

nut_supply = ave_trawl_biomass_nutrients %>% 
  filter(sector == "Industrial",
         year == 2017,
         end_use == "Direct human consumption") %>%
  left_join(countries_ISO, by=c("fishing_entity" = "missing_countries")) %>% 
  group_by(iso3c, nutrient) %>% 
  summarise(nut_supply = sum(nut_supply)) %>% 
  left_join(pop) %>% 
  mutate(nut_supply = (nut_supply/pop)/365,
         production_sector = "bottom_trawl",
         nutrient = recode(nutrient, "Vitamin A, RAE" = "Vitamin A")) %>% 
  ungroup() %>% 
  select(iso3c, production_sector, nutrient, nut_supply)
  
##Integrate with GND

####Read GND############
BaseNutrients <- read_csv("data/NutrientsBaseRev3.csv") %>% mutate(scenario = "base") %>% 
  rename("output" = "OUTPUT,0",
         "iso3c" = "...2",
         "food_abrev" = "...3",
         "nutrient" = "...4") %>% 
  #separate(X10, c("X11", "nutrient", "total"), "\\..",remove=T) %>%
  separate(elements, c("nutrient_long", "units", "X12"), "\\[", remove=F) %>% 
  mutate(units = gsub("]", "", units),
         nutrient = recode(nutrient,
                           "CA" = "Calcium",
                           "FE" = "Iron",
                           "MFAT" = "Monounsaturated fatty acids",
                           "O3" = "Omega-3 fatty acids",
                           "PFAT" = "Polyunsaturated fatty acids",
                           "SFAT" = "Saturated fatty acids",
                           "TFAT" = "Fat",
                           "VitA1" = "Vitamin A",
                           "VitB" = "Vitamin B12",
                           "ZN" = "Zinc",
                           "PROT" = "Protein",
                           "DES" = "Calories")) %>% 
  dplyr::select(-X12)


##Change to long format
NutrientsScen_long = reshape2::melt(BaseNutrients, id.vars=c("output", "countries", "products", "elements",     
                                                             "nutrient_long", "units", "iso3c",        
                                                             "food_abrev", "nutrient", "scenario")) %>% 
  rename("year" = "variable") %>% 
  mutate(year = gsub("A", "", year)) %>% 
  filter(nutrient %in% c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12", "Omega-3 fatty acids", "Calcium"))

countries_with_bug <- read_csv("~/Fisheries Nutrition Modeling/countries_with_bug.csv")
countries_bug = as.vector(countries_with_bug$iso)

NutrientsScen_long = NutrientsScen_long %>% 
  filter(!iso3c %in% countries_bug)

# Build EU27 key
eu27_key <- read.csv("data/COSIMO_AGLINK_2020_country_key.csv", as.is=T) %>% 
  filter(group_code=="EUN") %>% 
  select(iso3, country, iso3_use, country_use) %>% 
  filter(country!="Czechoslovakia")

# Non-EU data
data_non_eu <- NutrientsScen_long %>% 
  filter(!iso3c %in% c("EUN", "WLD"))

# EU data
data_eun <- NutrientsScen_long %>% 
  filter(iso3c %in% "EUN")

# Duplicate EUN data for each E27 country
data_eun_use <- purrr::map_df(1:nrow(eu27_key), function(x) {
  
  iso_do <- eu27_key$iso3_use[x]
  country_do <- eu27_key$country_use[x]
  
  cdata <- data_eun %>%
    mutate(iso3c=iso_do,
           countries=country_do)
  
})

NutrientsScen_long <- bind_rows(data_non_eu, data_eun_use) %>% 
  arrange(countries,nutrient)

NutrientsScen_long_other = NutrientsScen_long %>% 
  filter(!products=="Total food") %>% 
  rename("nutrient_supply" = "value") %>% 
  mutate(production_sector = case_when(products == "Fish" ~ "Fish",
                                       products == "Beef and Veal" ~ "ASF",
                                       products == "Eggs" ~ "ASF",
                                       products == "Pork" ~ "ASF",
                                       products == "Poultry" ~ "ASF",
                                       products == "Sheep" ~ "ASF",
                                       products == "Butter" ~ "ASF",
                                       products == "Fruits and vegetables" ~ "Other",
                                       products == "Milk" ~ "ASF",
                                       products == "Other Coarse Grains" ~ "Other",
                                       products == "Other Oilseeds" ~ "Other",
                                       products == "#N/A" ~ "Other",
                                       products == "Pulses" ~ "Other",
                                       products == "Sugar" ~ "Other",
                                       products == "Rice" ~ "Other",
                                       products == "Vegetable oils" ~ "Other",
                                       products == "Maize" ~ "Other",
                                       products == "Wheat" ~ "Other",
                                       products == "Roots tubers" ~ "Other")) %>%
  filter(year == 2017) %>% 
  group_by(iso3c, production_sector, nutrient) %>% 
  summarise(nut_supply = sum(nutrient_supply, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(nutrient = recode(nutrient, "Omega-3 fatty acids" = "DHA+EPA"))

nutrient_supply_final_v2 = rbind(NutrientsScen_long_other, nut_supply) %>% 
  spread(production_sector, nut_supply) %>%
  drop_na(ASF) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total = ASF + Other + Fish,
         ASF = ASF + Fish,
         perc_fish = 100*bottom_trawl/Fish,
         perc_ASF = 100*bottom_trawl/ASF,
         perc_tot = 100*bottom_trawl/total)

#Export data
write.csv(nutrient_supply_final_v2, "outputs/nut_supply_GND.csv", row.names = F)


###############################Coastal population########################

# Clear workspace
rm(list = ls())

##Nutrient supply
ave_trawl_biomass_nutrients <- read_csv("outputs/ave_trawl_biomass_nutrients.csv")

countries_ISO <- read.csv("data/countries_ISO.csv", encoding = "UTF-8") %>% 
  distinct(missing_countries, .keep_all = T)

pop <- read_csv("~/SAU-Fisheries-Nutrition/data/coastal_population_all_V2.csv") %>% 
  select(iso3c, km_20) %>% 
  rename(pop = km_20)

nut_supply = ave_trawl_biomass_nutrients %>% 
  filter(sector == "Industrial",
         year == 2017,
         end_use == "Direct human consumption") %>%
  left_join(countries_ISO, by=c("fishing_entity" = "missing_countries")) %>% 
  group_by(iso3c, nutrient) %>% 
  summarise(nut_supply = sum(nut_supply)) %>% 
  left_join(pop) %>% 
  mutate(nut_supply = (nut_supply/pop)/365,
         production_sector = "bottom_trawl",
         nutrient = recode(nutrient, "Vitamin A, RAE" = "Vitamin A")) %>% 
  ungroup() %>% 
  select(iso3c, production_sector, nutrient, nut_supply)

##Integrate with GND

####Read GND############
BaseNutrients <- read_csv("~/Fisheries Nutrition Modeling/data/NutrientsBaseRev3.csv") %>% mutate(scenario = "base") %>% 
  rename("output" = "OUTPUT,0",
         "iso3c" = "...2",
         "food_abrev" = "...3",
         "nutrient" = "...4") %>% 
  #separate(X10, c("X11", "nutrient", "total"), "\\..",remove=T) %>%
  separate(elements, c("nutrient_long", "units", "X12"), "\\[", remove=F) %>% 
  mutate(units = gsub("]", "", units),
         nutrient = recode(nutrient,
                           "CA" = "Calcium",
                           "FE" = "Iron",
                           "MFAT" = "Monounsaturated fatty acids",
                           "O3" = "Omega-3 fatty acids",
                           "PFAT" = "Polyunsaturated fatty acids",
                           "SFAT" = "Saturated fatty acids",
                           "TFAT" = "Fat",
                           "VitA1" = "Vitamin A",
                           "VitB" = "Vitamin B12",
                           "ZN" = "Zinc",
                           "PROT" = "Protein",
                           "DES" = "Calories")) %>% 
  dplyr::select(-X12)


##Change to long format
NutrientsScen_long = reshape2::melt(BaseNutrients, id.vars=c("output", "countries", "products", "elements",     
                                                             "nutrient_long", "units", "iso3c",        
                                                             "food_abrev", "nutrient", "scenario")) %>% 
  rename("year" = "variable") %>% 
  mutate(year = gsub("A", "", year)) %>% 
  filter(nutrient %in% c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12", "Omega-3 fatty acids", "Calcium"))

countries_with_bug <- read_csv("~/MPA_Nutrition/data/countries_with_bug.csv")
countries_bug = as.vector(countries_with_bug$iso)

NutrientsScen_long = NutrientsScen_long %>% 
  filter(!iso3c %in% countries_bug)

# Build EU27 key
eu27_key <- read.csv("~/MPA_Nutrition/data/COSIMO_AGLINK_2020_country_key.csv", as.is=T) %>% 
  filter(group_code=="EUN") %>% 
  select(iso3, country, iso3_use, country_use) %>% 
  filter(country!="Czechoslovakia")

# Non-EU data
data_non_eu <- NutrientsScen_long %>% 
  filter(!iso3c %in% c("EUN", "WLD"))

# EU data
data_eun <- NutrientsScen_long %>% 
  filter(iso3c %in% "EUN")

# Duplicate EUN data for each E27 country
data_eun_use <- purrr::map_df(1:nrow(eu27_key), function(x) {
  
  iso_do <- eu27_key$iso3_use[x]
  country_do <- eu27_key$country_use[x]
  
  cdata <- data_eun %>%
    mutate(iso3c=iso_do,
           countries=country_do)
  
})

NutrientsScen_long <- bind_rows(data_non_eu, data_eun_use) %>% 
  arrange(countries,nutrient)

NutrientsScen_long_other = NutrientsScen_long %>% 
  filter(!products=="Total food") %>% 
  rename("nutrient_supply" = "value") %>% 
  mutate(production_sector = case_when(products == "Fish" ~ "Fish",
                                       products == "Beef and Veal" ~ "ASF",
                                       products == "Eggs" ~ "ASF",
                                       products == "Pork" ~ "ASF",
                                       products == "Poultry" ~ "ASF",
                                       products == "Sheep" ~ "ASF",
                                       products == "Butter" ~ "ASF",
                                       products == "Fruits and vegetables" ~ "Other",
                                       products == "Milk" ~ "ASF",
                                       products == "Other Coarse Grains" ~ "Other",
                                       products == "Other Oilseeds" ~ "Other",
                                       products == "#N/A" ~ "Other",
                                       products == "Pulses" ~ "Other",
                                       products == "Sugar" ~ "Other",
                                       products == "Rice" ~ "Other",
                                       products == "Vegetable oils" ~ "Other",
                                       products == "Maize" ~ "Other",
                                       products == "Wheat" ~ "Other",
                                       products == "Roots tubers" ~ "Other")) %>%
  filter(year == 2017) %>% 
  group_by(iso3c, production_sector, nutrient) %>% 
  summarise(nut_supply = sum(nutrient_supply, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(nutrient = recode(nutrient, "Omega-3 fatty acids" = "DHA+EPA"))

nutrient_supply_final_v2 = rbind(NutrientsScen_long_other, nut_supply) %>% 
  spread(production_sector, nut_supply) %>%
  drop_na(ASF) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total = ASF + Other + Fish,
         ASF = ASF + Fish,
         perc_fish = 100*bottom_trawl/Fish,
         perc_ASF = 100*bottom_trawl/ASF,
         perc_tot = 100*bottom_trawl/total)

#Export data
write.csv(nutrient_supply_final_v2, "outputs/nut_supply_GND_coastal.csv", row.names = F)






# Produce a file of the average (across years) biomass (column "tonnes") 
#by country (col A), 
#fishing entity (col B), 
#species (col C), 
#reporting (col J), 
#and end use (col L) only when gear (col K) is bottom trawl. 

dat2 = data %>% 
 filter(gear == "bottom trawl",
        sector == "Industrial") %>% 
  group_by(year, eez, fishing_entity, sector, reporting_status, end_use, scientific_name) %>% 
  summarise(tonnes = mean(tonnes, na.rm = T))

write.csv(dat2, "outputs/ave_trawl_biomass.csv", row.names = F)

# Produce a file of the total biomass per species
dat3 = data %>% 
  filter(gear == "bottom trawl",
         sector == "Industrial") %>% 
  group_by(scientific_name) %>% 
  summarise(tonnes = sum(tonnes, na.rm = T)) %>% 
  arrange(desc(tonnes))


write.csv(dat3, "outputs/total_trawl_biomass.csv", row.names = F)

# Produce a file of the total biomass by species and country
dat4 = data %>% 
  filter(gear == "bottom trawl",
         sector == "Industrial") %>% 
  group_by(scientific_name, fishing_entity) %>% 
  summarise(tonnes = sum(tonnes, na.rm = T)) %>% 
  arrange(desc(tonnes))


write.csv(dat4, "outputs/total_trawl_biomass_by_country.csv", row.names = F)

# Produce a file of the total biomass by species, country and year
dat5 = data %>% 
  filter(gear == "bottom trawl",
         sector == "Industrial") %>% 
  group_by(scientific_name, fishing_entity, gear, sector, year) %>% 
  summarise(tonnes = sum(tonnes, na.rm = T)) %>% 
  arrange(desc(tonnes))


write.csv(dat5, "outputs/total_trawl_biomass_by_country_year.csv", row.names = F)

# Produce a file of the total biomass by species, country and year for Philippines
dat6 = data %>% 
  filter(gear == "bottom trawl",
         sector == "Industrial",
         fishing_entity == "Philippines") %>% 
  group_by(scientific_name, fishing_entity, gear, sector, year) %>% 
  summarise(tonnes = sum(tonnes, na.rm = T)) %>% 
  arrange(desc(tonnes))


write.csv(dat6, "outputs/total_trawl_biomass_by_country_year_Philippines.csv", row.names = F)


#2) Check to make sure that col K = bottom trawl ONLY when column H = industrial. If not, facet by column H in #1 too. 
unique(dat2$sector)
# "Industrial"   "Artisanal"    "Subsistence"  "Recreational"

#3) Match the species name in Column C to AFCD
SAU_unique_spp <- read_csv("data/SAU_unique_spp.csv") %>% 
  separate(scientific_name, c("genus", "spp"), " ", remove=FALSE) %>% 
  select(-scientific_name, -spp) %>% 
  distinct(genus, .keep_all = T)

source("species_nutrients.R")

spp_vec = unique(dat2$scientific_name)

x = species_nutrients(sci_name = spp_vec,
                      prep = c("raw",
                               "frozen",
                               "small sample"),
                      part = c("muscle tissue", 
                               "edible", 
                               "raw", 
                               "small sample"),
                      nut = c("Iron, total", 
                              "Zinc", 
                              "Calcium", 
                              "Total fatty acids, polyunsaturated",
                              "Protein, total; calculated from total nitrogen",
                              "Vitamin A; sum of retinol/carotenoids",
                              "Vitamin B12"))

x = x %>% 
  select(species, nutrient, nutrient_units, value, ssd, count, se, lower_ci, upper_ci, taxa_match) %>% 
  mutate(species = stringr::str_to_sentence(species))

spp = dat2 %>%
  left_join(x, by = c("scientific_name" = "species"))

#missing_nut = unique(dat3$scientific_name[is.na(dat3$value)])

##Fill missing value by ISCAAP group
##Calculate confidence intervals
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

spp_complete = spp %>% 
  filter(!is.na(value)) %>% 
  separate(scientific_name, c("genus", "spp"), " ", remove=FALSE) %>% 
  left_join(SAU_unique_spp)

spp_ISCAAP = spp_complete %>% 
  group_by(functional_group, nutrient, nutrient_units) %>% 
  summarize(smean = mean(value),
            ssd = sd(value),
            count = n()) %>% 
  mutate(se = ssd / sqrt(count),
         lower_ci = lower_ci(smean, se, count),
         upper_ci = upper_ci(smean, se, count)) %>% 
  ungroup() %>% 
  rename(value = smean) %>% 
  drop_na(functional_group)

missing_nut = spp %>%  
  separate(scientific_name, c("genus", "spp"), " ", remove=FALSE) %>% 
  left_join(SAU_unique_spp) %>% 
  filter(is.na(value)) %>% 
  select(-value, -ssd, -count, -se, -lower_ci, -upper_ci, -taxa_match) %>% 
  left_join(spp_ISCAAP) %>% 
  mutate(taxa_match = "Functional group")

final_dta = rbind(spp_complete, missing_nut) %>% 
  mutate(nut_supply = value*tonnes*10*1000) %>% 
  mutate(nutrient = recode(nutrient,                                     
                           "Iron, total" = "Iron",                                   
                           "Protein, total; calculated from total nitrogen" = "Protein",
                           "Total fatty acids, polyunsaturated" = "DHA+EPA",            
                           "Vitamin A; sum of retinol/carotenoids" = "Vitamin A, RAE"))

write.csv(final_dta, "outputs/ave_trawl_biomass_nutrients.csv", row.names = F)
#4) In addition to the biomass estimate (one column), add our usual suspects of nutrients (additional columns)

#5) By country, calculate the difference between nutrient supply by "end use", 
#comparing DHC and FMFO. Provide a file that ranks countries by highest ratio
#of "DHC" nutrient supply : "FMFO" nutrient supply. Do this for each nutrient.
nut_supply_fmfo = final_dta %>% 
  filter(sector == "Industrial") %>% 
  group_by(year, fishing_entity, end_use, nutrient) %>% 
  summarise(nut_supply = sum(nut_supply, na.rm = T)) %>% 
  spread(end_use, nut_supply) %>% 
  mutate(`Fishmeal and fish oil` = if_else(is.na(`Fishmeal and fish oil`), 0, `Fishmeal and fish oil`),
         fmfo_dhc_ratio = `Direct human consumption`/`Fishmeal and fish oil`,
         dhc_fmfo_ratio = `Fishmeal and fish oil`/`Direct human consumption`) %>% 
  arrange(desc(fmfo_dhc_ratio))

write.csv(nut_supply_fmfo, "outputs/nut_supply_fmfo.csv", row.names = F)

#6) 








#5) By country, calculate the differences between nutrient supply by "end use", so comparing DHC, Discards, and Other. Provide a file that ranks countries by highest ratio of "Discard" nutrient supply : "DHC" nutrient supply. Do this for each nutrient.
#Do you want this by sector?
nut_supply_discards = final_dta %>% 
  filter(sector == "Industrial") %>% 
  group_by(year, fishing_entity, end_use, nutrient) %>% 
  summarise(nut_supply = sum(nut_supply)) %>% 
  spread(end_use, nut_supply) %>% 
  mutate(discard_ratio = Discards/`Direct human consumption`) %>% 
  arrange(desc(discard_ratio))

write.csv(nut_supply_discards, "outputs/nut_supply_discards.csv", row.names = F)

#6) By country, calculate the differences between nutrient supply by reported vs unreported. Provide a file that ranks countries by highest ratio of unreported to reported from a nutrient standpoint. Do this for each nutrient.
nut_supply_reporting = final_dta %>% 
  filter(sector == "Industrial") %>% 
  group_by(year, fishing_entity, reporting_status, nutrient) %>% 
  summarise(nut_supply = sum(nut_supply)) %>% 
  spread(reporting_status, nut_supply) %>% 
  mutate(reporting_ratio = Unreported/Reported) %>% 
  arrange(desc(reporting_ratio))

write.csv(nut_supply_reporting, "outputs/nut_supply_reporting.csv", row.names = F)

#7) Across countries, determine the most important species captured in bottom trawls from a nutrient perspective. 
nut_supply_spp = final_dta %>% 
  filter(sector == "Industrial") %>% 
  group_by(scientific_name, nutrient) %>% 
  summarise(nut_supply = sum(nut_supply)) %>% 
  spread(nutrient, nut_supply) %>% 
  arrange(desc(Calcium))

write.csv(nut_supply_spp, "outputs/nut_supply_spp.csv", row.names = F)

