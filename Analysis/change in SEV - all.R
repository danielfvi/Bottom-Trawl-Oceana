###Change in SEV

library(tidyverse)
library(countrycode)
library(ggrepel)

# Clear workspace
rm(list = ls())

##Load data
# data <- read.csv("data/catch_data_2014-2018.csv", encoding = "UTF-8") %>% 
#   mutate(fishing_entity = recode(fishing_entity, "Côte d'Ivoire" = "Ivory Coast"))
# 
# data = data %>% 
#   filter(year == 2017)
# 
# write.csv(data, "data/catch_data_2017.csv", row.names = F)

data <- read.csv("data/catch_data_2017.csv")

##Species nutrient value 
trawl_nutrients <- read_csv("outputs/ave_trawl_biomass_nutrients.csv") %>% 
  select(scientific_name, nutrient, value) %>% 
  unique()

#ISO codes
countries_ISO <- read.csv("data/countries_ISO.csv", encoding = "UTF-8")

#Country population
pop = world_bank_pop %>% 
  filter(indicator == "SP.POP.TOTL") %>% 
  select(country, `2017`) %>% 
  rename(pop = `2017`,
         iso3c = country)

##First, we need to calcuate the nutrient biomass that is taken from foreign fleets in each EEZ

dat2 = data %>% 
  filter(gear == "bottom trawl",
         sector == "Industrial") %>% 
  separate(eez, c("eez_country", "eez_region"), "([_;,()%/])", remove=FALSE) %>%
  left_join(countries_ISO, by=c("fishing_entity" = "missing_countries")) %>% 
  rename(fishing_entity_ISO = iso3c) %>% 
  mutate(eez_country = str_trim(eez_country)) %>% 
  mutate(eez_iso = countrycode(eez_country,'country.name', 'iso3c')) 

dat3 = dat2 %>% 
  left_join(trawl_nutrients) %>% ##Merge with nutrient content data
  mutate(nut_supply = value*tonnes*10*1000) %>% ##Nutrient units are in g,mg,ug/100g
  group_by(eez_country, eez_iso, fishing_entity, fishing_entity_ISO, nutrient) %>% 
  summarise(nut_supply = sum(nut_supply, na.rm = T)) %>% ##Sum nutrient supply from all species
  mutate(is_same = if_else(eez_iso == fishing_entity_ISO, "your_country", "other_country"), ##Create variable to differentiate foreign fishing
         continent = countrycode(eez_country,'country.name', 'continent'))

dat4 = dat3 %>% 
  group_by(eez_country, eez_iso, continent, is_same, nutrient) %>% 
  summarise(nut_supply = sum(nut_supply, na.rm = T)) %>% 
  spread(is_same, nut_supply) %>% 
  drop_na(eez_iso) %>% 
  left_join(pop, by=c("eez_iso" = "iso3c")) %>% #Merge with population size
  mutate(your_country = if_else(is.na(your_country), 0, your_country),
         other_country = if_else(is.na(other_country), 0, other_country),
         total = your_country + other_country,
         per_capita = other_country/pop/365)##Per capita nutrient supply from foreign fishing

write.csv(dat4, "outputs/foreign_nut_supply.csv", row.names = F)

#Next we need to calculate the nutrient biomass that is caught by each fishing entity in other EEZs 
dat5 = dat3 %>% 
  group_by(fishing_entity_ISO, is_same, nutrient) %>% 
  summarise(nut_supply = sum(nut_supply, na.rm = T)) %>% 
  spread(is_same, nut_supply) %>% 
  drop_na(fishing_entity_ISO) %>% 
  left_join(pop, by=c("fishing_entity_ISO" = "iso3c")) %>% 
  mutate(your_country_entity = if_else(is.na(your_country), 0, your_country),
         other_country_entity = if_else(is.na(other_country), 0, other_country),
         per_capita_entity = other_country_entity/pop/365) %>% ##Nutrient supply from fishing in other eezs
  ungroup() %>% 
  rename(iso3c = fishing_entity_ISO) %>% 
  select(-"<NA>", -your_country, -other_country, -pop)

##Now, we need to calculate the net nutrient supply in case all catch from bottom trawl is caught and consumed within each EEZ.
#THerefore, we need to first add what is caught by foreign fleets and substract what is caught in other countries.
#For example, if country A catch 10t in their EEZ, catch 3t in other EEZs and other countries catch 5t in their EEZ the net nutrient supply will be 10 + 5 -3 = 12t

dat6 = dat4 %>% 
  rename(iso3c = eez_iso) %>% 
  left_join(dat5) %>% 
  drop_na(iso3c) %>%
  rename(your_country_eez = your_country,
         other_country_eez = other_country,
         per_capita_eez = per_capita) %>% 
  ungroup() %>% 
  mutate(per_capita_entity = if_else(is.na(per_capita_entity), 0, per_capita_entity),
         per_capita_eez = if_else(is.na(per_capita_eez), 0, per_capita_eez),
         per_capita = per_capita_eez - per_capita_entity) %>% #Remove what was cauught in other eezs from national supply
  select(iso3c, nutrient, per_capita)

write.csv(dat6, "outputs/foreign_nut_supply_all.csv", row.names = F)

#################Change in SEV (Summary exposure value)
#Taken from Golden et al 2021
# Clear workspace
rm(list = ls())

##Load data
per_capita_supply <- read_csv("outputs/foreign_nut_supply_all.csv") %>%
  #rename(iso3c = eez_iso) %>% 
  select(iso3c, nutrient, per_capita) %>% 
  mutate(nutrient = recode(nutrient, 
                           "DHA+EPA" = "Omega-3 fatty acids",
                           "Vitamin A, RAE" = "Vitamin A"))

countries_with_bug <- read_csv("data/countries_with_bug.csv")
countries_with_bug = as.vector(countries_with_bug$iso)
#Import datasets
BaseNutrients <- read_csv("data/NutrientsBaseRev3.csv") %>% mutate(scenario = "base")

##Nutrient output
BaseNutrients = BaseNutrients %>% 
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
  filter(nutrient %in% c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin B12", "Omega-3 fatty acids", "Calcium"),
         !iso3c %in% countries_with_bug)

Nutrients_other = NutrientsScen_long %>% 
  filter(!products=="Fish",
         !products=="Total food",
         year==2017) %>% 
  group_by(iso3c, nutrient) %>% 
  summarise(supply = sum(value, na.rm = T)) %>% 
  ungroup()

Nutrients_fish_high = NutrientsScen_long %>% 
  filter(products=="Fish",
         year=="2017",
         scenario == "base") %>% 
  left_join(per_capita_supply) %>%
  rename(value_base = value) %>% 
  mutate(value = if_else(is.na(per_capita), value_base, value_base + per_capita),
         scenario = "high") %>%
  left_join(Nutrients_other) %>% 
  mutate(total_supply = value + supply) %>%
  select(iso3c, nutrient, units, scenario, total_supply)

iso_reef = unique(Nutrients_fish_high$iso3c)

##Subset model output - all years
Nutrients_fish_base = NutrientsScen_long %>% 
  filter(products=="Fish",
         year=="2017",
         scenario == "base") %>% 
  left_join(per_capita_supply) %>%
  left_join(Nutrients_other) %>% 
  mutate(total_supply = value + supply) %>% 
  select(iso3c, nutrient, units, scenario, total_supply)

Nutrients_fish_base_high = rbind(Nutrients_fish_high, Nutrients_fish_base) 

#write.csv(Nutrients_fish_base_high, "Outputs/NutrientSupply_MPA_bayes.csv", row.names = F)

########Step 2

# Read data
#data1_orig <- read.csv("data/NutrientSupply_MPA_bayes.csv", as.is=T)
data1_orig <- Nutrients_fish_base_high

# Build EU27 key
eu27_key <- read.csv("data/COSIMO_AGLINK_2020_country_key.csv", as.is=T) %>% 
  filter(group_code=="EUN") %>% 
  select(iso3, country, iso3_use, country_use) %>% 
  filter(country!="Czechoslovakia")

# Format data
################################################################################

# Format data
data <- data1_orig %>% 
  # Rename columns
  rename(intake=total_supply) %>% 
  # Add country column
  mutate(country=countrycode(iso3c, "iso3c", "country.name")) %>% 
  # Fill missing countries
  mutate(country=ifelse(is.na(country), iso3c, country),
         country=recode(country, 
                        "ANT"="Netherlands Antilles", 
                        "BLX"="Belgium-Luxembourg",
                        "CZ2"="Czechoslovakia",
                        "ET2"="Ethiopia PDR",
                        "EUN"="EU 27",
                        "SRM"="Serbia and Montenegro",
                        "USR"="USSR",
                        "YUG"="Yugoslav SFR")) %>% 
  # Format scenario
  mutate(scenario=recode(scenario, "base"="Base", "high"="High")) %>% 
  # Arrange
  select(iso3c, country, nutrient, units, scenario, intake, everything()) %>% 
  arrange(country, nutrient, scenario)

# Inspect data
str(data)
#freeR::complete(data)
table(data$nutrient)


# Explode EU data
################################################################################

# Non-EU data
data_non_eu <- data %>% 
  filter(!iso3c %in% c("EUN", "WLD"))

# EU data
data_eun <- data %>% 
  filter(iso3c %in% "EUN")

# Duplicate EUN data for each E27 country
data_eun_use <- purrr::map_df(1:nrow(eu27_key), function(x) {
  
  iso_do <- eu27_key$iso3_use[x]
  country_do <- eu27_key$country_use[x]
  
  cdata <- data_eun %>% 
    mutate(iso3=iso_do,
           country=country_do)
  
})

# Merge EU-expanded and non-EU data
data_out <- bind_rows(data_non_eu, data_eun_use) %>% 
  arrange(country,nutrient, scenario) %>% 
  # Final formatting
  rename(nutrient_units=units) %>% 
  mutate(nutrient=recode(nutrient, 
                         "Vitamin A"="Vitamin A, RAE",
                         "Vitamin B12"="Vitamin B-12"),
         scenario=recode(scenario, "High"="High road")) %>% 
  mutate(iso3 = if_else(is.na(iso3), iso3c, iso3)) %>% 
  select(-iso3c)

#write.csv(data_out, "Outputs/NutrientSupply_MPA_EUN_bayes.csv", row.names = F)


#############Step 3
# Read COSIMO output
#cosimo_orig <- read.csv("Outputs/NutrientSupply_MPA_EUN_bayes.csv", as.is=T)
cosimo_orig <- data_out

# Read GENUS dataset
genus_orig <- readRDS("data/genus_nutrient_supplies_w_fort_by_age_sex_2011.Rds")

# Read COSIMO-GENUS I3O matching key
cosimo_genus_iso_key <- readxl::read_excel("data/TableS4_cosimo_genus_countries.xlsx", skip=1) %>% 
  setNames(c("iso3_cosimo", "country_cosimo", "iso3_genus", "country_genus")) %>% 
  select(iso3_cosimo, iso3_genus)

# Read SPADE-derived scalars (for omega-3s and Vitamin B-12)
spade_scalars <- readRDS(file.path("data/intake_distribution_age_sex_scalars.Rds")) %>% 
  rename(iso3=country_iso3) %>% 
  mutate(sex=recode(sex, "men"="Males", "women"="Females"),
         age_group=as.character(age_group), 
         nutrient=recode(nutrient, "Vitamin A"="Vitamin A, RAE"))


# Format GENUS for merge 
################################################################################

# Expand GENUS to separate children into male/female
genus_c_f <- genus_orig %>% 
  filter(sex=="Children") %>% 
  mutate(sex="Females")
genus_c_m <- genus_orig %>% 
  filter(sex=="Children") %>% 
  mutate(sex="Males")
genus_adults <- genus_orig %>% 
  filter(sex!="Children")
genus_exp <- bind_rows(genus_c_f, genus_c_m, genus_adults) %>% 
  arrange(country_use, nutrient, sex, age_range)


# Build key for fraction of nutrients
################################################################################

# COSIMO ISO3s
cosimo_isos <- sort(unique(cosimo_orig$iso3))
cosimo_nutrs <- sort(unique(cosimo_orig$nutrient))

# GENUS nutrients
sort(unique(genus_exp$nutrient))
genus_nutr_use <- c("Calcium", # Calcium
                    "Calories", # Energy
                    "Iron", # Iron
                    "Monounsaturated fatty acids", # Monounsaturated fatty acids
                    # Omega-3 fatty acids not available in GENUS
                    "Polyunsaturated fatty acids", # Polyunsaturated fatty acids
                    "Protein", # Protein
                    "Saturated fatty acids", # Saturated fatty acids
                    "Fat", # Total lipids
                    "Vitamin A", # Vitamin A, RAE
                    "Vitamin B-12", # for Vitamin B6? B^is not available in GENUS
                    "Zinc") # Zinc

# Build scalar key
intake_scalars <- genus_exp %>% 
  # Compute scalar distribution of intake within country-sex-age groups
  group_by(iso3_use, country_use, nutrient, units_long, units_short) %>% 
  mutate(scalar=value_med/mean(value_med)) %>% 
  ungroup() %>% 
  # Reduce to useful data
  filter(!is.na(scalar)) %>% 
  # Simplify key
  select(iso3_use, nutrient, sex, age_range, scalar) %>% 
  # Rename for COSIMO output consistency
  rename(iso3=iso3_use, age_group=age_range)

# Expands scalar key to include 80+ components
################################################################################

# Not 80+
intake_scalars_no80 <- intake_scalars %>% 
  filter(age_group!="80+")

# 80+
intake_scalars80 <- intake_scalars %>% 
  filter(age_group=="80+")

# Expand 80+ data
plus_groups <- c("80-84", "85-89", "90-94", "95-99")
intake_scalars80_exp <- purrr::map_df(plus_groups, function(x){
  edata <-intake_scalars80 %>% mutate(age_group=x)
})

# Merge pre-80 and expanded 80+
intake_scalars_exp <- bind_rows(intake_scalars_no80, intake_scalars80_exp) %>% 
  arrange(iso3, nutrient, sex, age_group, scalar)


# Build key for fraction of nutrients
################################################################################

# Build country-sex-age key
cntry_sex_age_key <- expand.grid(iso3=sort(unique(cosimo_orig$iso3)),
                                 sex=c("Males", "Females"),
                                 age_group=sort(unique(spade_scalars$age_group))) %>% 
  arrange(iso3, sex, age_group)

# Build key
data <- cosimo_orig %>% 
  # Add GENUS nutrient name for scalar matching
  mutate(nutrient_genus=recode(nutrient, 
                               "Energy"="Calories",
                               "Total lipids"="Fat",
                               "Vitamin A, RAE"="Vitamin A")) %>% 
  # Add GENUS ISO3s for scalar matching
  left_join(cosimo_genus_iso_key, by=c("iso3"="iso3_cosimo")) %>% 
  mutate(iso3_genus=ifelse(is.na(iso3_genus), iso3, iso3_genus)) %>% 
  # Add sex/age group
  left_join(cntry_sex_age_key, by="iso3") %>% 
  # Add GENUS-derived scalars
  left_join(intake_scalars_exp, by=c("iso3_genus"="iso3", "nutrient_genus"="nutrient", "sex", "age_group")) %>% 
  rename(scalar_genus=scalar) %>% 
  # Add SPADE-derived scalars
  left_join(spade_scalars %>% select(iso3, nutrient, sex, age_group, scalar)) %>%
  rename(scalar_spade=scalar) %>% 
  # Select scalar
  mutate(scalar=ifelse(nutrient %in% c("Omega-3 fatty acids", "Vitamin B-12") | is.na(scalar_genus), scalar_spade, scalar_genus)) %>% 
  # Calculate sex-age means
  rename(mean_cntry=intake) %>% 
  mutate(mean_group=mean_cntry * scalar) %>% 
  # Arrange output
  select(scenario, iso3, country,
         nutrient, nutrient_units, sex, age_group,
         nutrient_genus, iso3_genus, scalar_genus, scalar_spade, scalar, 
         mean_cntry, mean_group, everything())

# Inspect data
# The only rows missing values are 2018-2030 values for countries that don't exist anymore
freeR::complete(data)

# Inspect missing
check <- data %>% 
  filter(is.na(scalar))
sort(unique(check$nutrient))
sort(unique(check$country))



# Add the distribution fits
################################################################################

# Read fits
dists <- readRDS("data/intake_distributions_for_all_cosimo_countries.Rds") %>% 
  mutate(nutrient=recode(nutrient, "Vitamin A"="Vitamin A, RAE")) %>% 
  mutate(age_group=as.character(age_group))

# Add distribution fits
data1 <- data %>% 
  # Recode sex for merge
  # mutate(sex=recode(sex, "Females"="women", "Males"="men")) %>% 
  # Add distribution fits
  left_join(dists, by=c("iso3"="country_iso3", "nutrient", "sex", "age_group")) %>% 
  # Add means and differences
  mutate(g_mean=g_shape/g_rate,
         g_mean_diff=mean_group-g_mean) %>% 
  mutate(ln_mean=exp(ln_meanlog + ln_sdlog^2/2),
         ln_mean_diff=mean_group-ln_mean)

# Inspect
#freeR::complete(data1)

# Check
# The only rows missing values shoudl be those for uninteresting nutrients
check2 <- data1 %>% 
  filter(is.na(best_dist))
sort(unique(check2$nutrient))

# Export
################################################################################

# # Export
# saveRDS(data1, "Outputs/COSIMO_2010_2030_country_nutrient_age_sex_means_and_distributions_with_disagg.Rds")

# Read other data (in repository)
omega_N_raw_2019 <- openxlsx::read.xlsx('data/omega_RR_2019.xlsx')
red_meat_raw_2019 <- openxlsx::read.xlsx('data/meat_RR_2019.xlsx')
EAR_requirements <- openxlsx::read.xlsx('data/EAR_requirements_GBDgroups.xlsx')

# Read distributions (micronutrients)
dists <- data1

# Read distributions (red meat)
# dists_meat <- readRDS(file=file.path("data/cosimo/processed/COSIMO_2010_2030_country_red_meat_age_sex_means_and_distributions.Rds"))

# Read HDI/SDI key
sdi_hdi_key <- readRDS("data/COSIMO_country_key_with_SDI_HDI_info.rds") %>% 
  select(iso3, sdi, sdi_group, hdi)

# Source helpher functions
source("~/Bottom-trawl/code/RR_functions.R")


# Notes
################################################################################

# AGE GROUP CODES
# 5 1-4 years 
# 6 5-9 years 
# 7 10-14 years
# 8 15-19 years
# 9 20-24 years
# 10 25-29 years
# 11 30-34 years
# 12 35-39 years
# 13 40-44 years 
# 14 45-49 years 
# 15 50-54 years 
# 16 55-59 years 
# 17 60-64 years 
# 18 65-69 years 
# 19 70-74 years 
# 20 75-79 years
# 30 80-84 years
# 31 85-89 years
# 32 90-94 years
# 33 95-99 years

# CAUSE CODES
# 409 noncommunicable diseases
# 295 Communicable, maternal, neonatal, and nutritional diseases
# 294 all cause

# CODES
# 121 low seafood
# 97 zinc
# 96 vitamin A
# 95 iron
# 117 processed meat
# 116 red meat

# CAUSE
# 429 breast cancer 
# 441 Colon and rectum cancer 
# 493 Ischemic heart disease 
# 495 Ischemic stroke
# 496 Intracerebral hemorrhage 
# 497 Subarachnoid hemorrhage 
# 976 Diabetes mellitus type 2  

# Build data
################################################################################

# Meat causes
cause_meat <- c(429,441,493,495,496,497,976) 
cause_meat_no_ischemic <- c(429,441,495,496,497,976)



# Format intake distributions
##########################################################################################

# Merge data
dists2017 <- dists %>% 
  # Reduce to distributions with a mean
  filter(!is.na(mean_group)) %>% 
  # Simplify 
  select(country, iso3, nutrient, sex, age_group, scenario, mean_group, best_dist, 
         g_shape, g_rate, g_mean, g_mean_diff,
         ln_meanlog, ln_sdlog, ln_mean, ln_mean_diff) %>% 
  # Add age id and sex id
  mutate(sex_id=recode(sex, 
                       "Males"=1,
                       "Females"=2) %>% as.numeric(),
         age_id=recode(age_group,
                       "0-4"="5",
                       "5-9"="6",
                       "10-14"="7",
                       "15-19"="8",
                       "20-24"="9",
                       "25-29"="10",
                       "30-34"="11",
                       "35-39"="12",
                       "40-44"="13",
                       "45-49"="14",
                       "50-54"="15",
                       "55-59"="16",
                       "60-64"="17",
                       "65-69"="18",
                       "70-74"="19",
                       "75-79"="20",
                       "80-84"="30",
                       "85-89"="31",
                       "90-94"="32",
                       "95-99"="33") %>% as.numeric()) %>% 
  select(-c(sex, age_group))

# Calculate changes in summary exposure values (SEVs) -- micronutrients
##########################################################################################

# Nutrients to calculate SEVS for
nutr_sevs <- c("Zinc", "Iron", "Calcium", "Vitamin A, RAE", "Vitamin B-12")

# Build data required for micronutrient SEV calculations
data_sev_mn <- dists2017 %>% 
  # Reduce to nutrients of interest
  filter(nutrient %in% nutr_sevs) %>% 
  # Add SDI group
  left_join(sdi_hdi_key, by=c("iso3")) %>% 
  # Reduce to age groups with required data
  filter(age_id>=5)

# Loop through micronutrients to calculate SEVs for
#x <- 141
# sevs_micronutrients <- purrr::map_df(1:nrow(data_sev_mn), function(x){
for(x in 1:nrow(data_sev_mn)){
  
  # Parameters
  print(x)
  scenario_do <- data_sev_mn$scenario[x]
  iso3_do <- data_sev_mn$iso3[x]
  nutr_do <- data_sev_mn$nutrient[x]
  sdi_group_do <- data_sev_mn$sdi_group[x]
  age_id_do <- data_sev_mn$age_id[x]
  sex_id_do <- data_sev_mn$sex_id[x]
  best_dist <- data_sev_mn$best_dist[x]
  
  # If gamma distribution....
  if(best_dist=="gamma"){
    shape <- data_sev_mn$g_shape[x]
    rate <- data_sev_mn$g_rate[x]
    x_shift <- data_sev_mn$g_mean_diff[x]
    intake_function <- function(x){y <- dgamma(x-x_shift, shape=shape, rate=rate)}
    val_hi <- qgamma(p=0.9999, shape = shape, rate=rate) + x_shift
    val_lo <- qgamma(p=0.0001, shape = shape, rate=rate) + x_shift
  }
  
  # If lognormal distribution...
  if(best_dist=="log-normal"){
    mu <- data_sev_mn$ln_meanlog[x]
    sigma <- data_sev_mn$ln_sdlog[x]
    x_shift <- data_sev_mn$ln_mean_diff[x]
    intake_function <- function(x){y <- dlnorm(x-x_shift, meanlog=mu, sdlog=sigma)}
    val_hi <- qlnorm(p=0.9999, meanlog=mu, sdlog=sigma) + x_shift
    val_lo <- qlnorm(p=0.0001, meanlog=mu, sdlog=sigma) + x_shift
  }
  
  # Plot dists 
  if(F){
    x <- seq(-200, 2000, by=1)
    y <- intake_function(x=x)
    plot(x, y, type="l")
    abline(v=c(val_lo, val_hi), lty=1)
  }
  
  # Calculate SEV
  sev <- try(micronutrient_SEV(Intake=intake_function, 
                               age=age_id_do, 
                               sex=sex_id_do,
                               nutrient=nutr_do, 
                               country_SDIgroup=sdi_group_do, 
                               EAR_requirements,
                               val_lo=val_lo,
                               val_hi=val_hi))
  
  # Record based on try()
  if(inherits(sev, "try-error")){
    sev_out <- NA
  }else{
    sev_out <- sev
  }
  
  # Record
  data_sev_mn$sev[x] <- sev_out
  
}

# Format SEVs
sev_mn_final <- data_sev_mn %>% 
  select(scenario, nutrient, country, iso3, sex_id, age_id, sev) %>% 
  spread(key="scenario", value="sev") %>% 
  rename(sev_high="High road", sev_base="Base") %>% 
  mutate(sev_delta=sev_base - sev_high)

# Export
write.csv(sev_mn_final, "outputs/sevs_base_high_foreign_catch_all.csv", row.names=F)

# Calculate changes in summary exposure values (SEVs) -- omega-3 fatty acids
##########################################################################################

# Build data required for micronutrient SEV calculations
data_sev_omega <- dists2017 %>% 
  # Reduce to nutrients of interest
  filter(nutrient %in% "Omega-3 fatty acids") %>% 
  # Reduce to age groups with required data
  filter(age_id>=10)

# Loop through micronutrients to calculate SEVs for
#x <- 1
for(x in 1:nrow(data_sev_omega)){
  
  # Parameters
  print(x)
  scenario_do <- data_sev_omega$scenario[x]
  iso3_do <- data_sev_omega$iso3[x]
  nutr_do <- data_sev_omega$nutrient[x]
  age_id_do <- data_sev_omega$age_id[x]
  sex_id_do <- data_sev_omega$sex_id[x]
  best_dist <- data_sev_omega$best_dist[x]
  
  # If gamma distribution....
  if(best_dist=="gamma"){
    shape <- data_sev_omega$g_shape[x]
    rate <- data_sev_omega$g_rate[x]
    x_shift <- data_sev_omega$g_mean_diff[x]
    intake_function <- function(x){y <- dgamma(x-x_shift, shape=shape, rate=rate)}
    val_hi <- qgamma(p=0.9999, shape = shape, rate=rate) + x_shift
    val_lo <- qgamma(p=0.0001, shape = shape, rate=rate) + x_shift
  }
  
  # If lognormal distribution...
  if(best_dist=="log-normal"){
    mu <- data_sev_omega$ln_meanlog[x]
    sigma <- data_sev_omega$ln_sdlog[x]
    x_shift <- data_sev_omega$ln_mean_diff[x]
    intake_function <- function(x){y <- dlnorm(x-x_shift, meanlog=mu, sdlog=sigma)}
    val_hi <- qlnorm(p=0.9999, meanlog=mu, sdlog=sigma) + x_shift
    val_lo <- qlnorm(p=0.0001, meanlog=mu, sdlog=sigma) + x_shift
  }
  
  # Plot dists 
  if(F){
    x <- seq(-1,5,by=0.01)
    y <- intake_function(x=x)
    plot(x, y, type="l")
    abline(v=c(val_lo, val_hi), lty=1)
  }
  
  # Calculate SEV
  sev <- try(omega_n3_SEV(Intake=intake_function,
                          age=age_id_do, 
                          val_hi=val_hi,
                          val_lo=val_lo,
                          omega_N_raw_2019,
                          omega_n3_RR))
  
  # Record based on try()
  if(inherits(sev, "try-error")){
    sev_out <- NA
  }else{
    sev_out <- sev
  }
  
  # Record
  data_sev_omega$sev[x] <- sev_out
  
}

# Format SEVs
sev_omega_final <- data_sev_omega %>% 
  mutate(nutrient="Omega-3 fatty acids") %>% 
  select(scenario, nutrient, country, iso3, sex_id, age_id, sev) %>% 
  spread(key="scenario", value="sev") %>% 
  rename(sev_high="High road", sev_base="Base") %>% 
  mutate(sev_delta=sev_base - sev_high)

# Export
write.csv(sev_omega_final, "outputs/sevs_base_high_foreign_catch_o3_all.csv", row.names=F)

sev_final = rbind(read_csv("Outputs/sevs_base_high_foreign_catch_all.csv"), 
                  sev_omega_final)


write.csv(sev_final, "outputs/sevs_base_high_foreign_catch_final_all.csv", row.names=F)

####################Calculate number of people###################### 
# Read data
sevs <- read_csv("Outputs/sevs_base_high_foreign_catch_final_all.csv") %>% # sevs_meat
  # Format sex
  mutate(sex=ifelse(sex_id==1, "Males", "Females")) %>% 
  # Format age group
  mutate(age_group=as.character(age_id),
         age_group=recode_factor(age_group,
                                 "5"="0-4",
                                 "6"="5-9",
                                 "7"="10-14",
                                 "8"="15-19",
                                 "9"="20-24",
                                 "10"="25-29",
                                 "11"="30-34",
                                 "12"="35-39",
                                 "13"="40-44",
                                 "14"="45-49",
                                 "15"="50-54",
                                 "16"="55-59",
                                 "17"="60-64",
                                 "18"="65-69",
                                 "19"="70-74",
                                 "20"="75-79",
                                 "30"="80-84",
                                 "31"="85-89",
                                 "32"="90-94",
                                 "33"="95-99"))  %>% 
  # Arrange
  select(nutrient, country, iso3, sex_id, sex, age_id, age_group, everything())

# Read population projection
pop_orig <- readRDS("data/UN_WPP_2019_pop_proj_by_cntry_agesex.Rds")

# Build data
################################################################################

# Format population data
pop <- pop_orig %>% 
  filter(year==2020) %>% 
  select(iso3_use, sex, age_range, pop_size_50perc) %>% 
  rename(iso3=iso3_use, age_group=age_range, npeople=pop_size_50perc) %>% 
  mutate(sex=recode(sex, "male"="Males", "female"="Females"))

# Build data
data <- sevs %>% 
  # Add population size
  left_join(pop) %>% 
  # Arrange
  select(country:age_group, npeople, nutrient, everything()) %>% 
  # Calculate
  mutate(ndeficient_base=npeople*sev_base/100,
         ndeficient_high=npeople*sev_high/100,
         ndeficient_diff=ndeficient_high-ndeficient_base,
         ndeficient_diff = if_else(ndeficient_diff>0, 0, ndeficient_diff),
         ndeficient_diff = abs(ndeficient_diff)) 

##Export data
write.csv(data, "Outputs/impacted_pop_SEV_foreign_catch_all.csv", row.names = F)
