library(tidyverse)
species_nutrients = function(sci_name, prep, part, nut){
  
  ##Calculate confidence intervals
  lower_ci <- function(mean, se, n, conf_level = 0.95){
    lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
  }
  upper_ci <- function(mean, se, n, conf_level = 0.95){
    upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
  }
  
  ##Taxa_table
  taxa_table = readRDS("data/taxa_table.Rds")
  
  ##Load AFCD
  AFCD = readRDS("data/AFCD_data_taxa.Rds") %>% 
    filter(nutrient %in% nut,
           food_prep %in% prep,
           food_part %in% part)
  
  nutrient_key = AFCD %>% 
    distinct(nutrient, .keep_all = T) %>% 
    select(nutrient, nutrient_units)
  
  afcd_species = AFCD %>% 
    group_by(sciname, nutrient) %>% 
    summarize(smean = mean(value),
              ssd = sd(value),
              count = n()) %>% 
    mutate(se = ssd / sqrt(count),
           lower_ci = lower_ci(smean, se, count),
           upper_ci = upper_ci(smean, se, count)) %>% 
    ungroup() %>% 
    drop_na(sciname) %>% 
    mutate(sciname = tolower(sciname)) %>% 
    rename(value = smean)
  
    #calculate mean values for genus
    afcd_genus = AFCD %>%
    group_by(genus, nutrient) %>% 
    summarize(smean = mean(value),
              ssd = sd(value),
              count = n()) %>% 
    mutate(se = ssd / sqrt(count),
           lower_ci = lower_ci(smean, se, count),
           upper_ci = upper_ci(smean, se, count)) %>% 
    ungroup() %>% 
    drop_na(genus) %>% 
    mutate(genus = tolower(genus)) %>% 
    rename(value = smean)
  
  #calculate mean values for family
  afcd_family = AFCD %>%
    group_by(family, nutrient) %>% 
    summarize(smean = mean(value),
              ssd = sd(value),
              count = n()) %>% 
    mutate(se = ssd / sqrt(count),
           lower_ci = lower_ci(smean, se, count),
           upper_ci = upper_ci(smean, se, count)) %>% 
    ungroup() %>% 
    drop_na(family) %>% 
    mutate(family = tolower(family)) %>% 
    rename(value = smean)
  
  #calculate mean values for order
  afcd_order = AFCD %>%
    group_by(order, nutrient) %>% 
    summarize(smean = mean(value),
              ssd = sd(value),
              count = n()) %>% 
    mutate(se = ssd / sqrt(count),
           lower_ci = lower_ci(smean, se, count),
           upper_ci = upper_ci(smean, se, count)) %>% 
    ungroup() %>% 
    drop_na(order) %>% 
    mutate(order = tolower(order)) %>% 
    rename(value = smean)
  
  #calculate mean values for class
  afcd_class = AFCD %>%
    group_by(class, nutrient) %>% 
    summarize(smean = mean(value),
              ssd = sd(value),
              count = n()) %>% 
    mutate(se = ssd / sqrt(count),
           lower_ci = lower_ci(smean, se, count),
           upper_ci = upper_ci(smean, se, count)) %>% 
    ungroup() %>% 
    drop_na(class) %>% 
    mutate(class = tolower(class)) %>% 
    rename(value = smean)
  
  
  all_spp = data.frame(species = rep(sci_name, length(nut)),
                       nutrient = rep(nut, length(sci_name)) %>% sort()) %>%
    mutate(species = tolower(species)) %>% 
    separate(species, c("genus", "spp"), " ", remove=FALSE) %>% 
    left_join(taxa_table, by = c("genus"))
  
  #Fill taxa by family
  missing_family = all_spp %>% 
    filter(is.na(class)) %>% 
    select(-order, -class) %>% 
    left_join(taxa_table %>% select(-genus) %>% distinct(family, .keep_all=T), by = c("genus" = "family"))
  
  all_spp = all_spp %>% 
    filter(!is.na(class)) %>% 
    rbind(missing_family)
  
  #Fill taxa by order
  missing_order = all_spp %>% 
    filter(is.na(class)) %>% 
    select(-class) %>% 
    left_join(taxa_table %>% select(-genus, -family) %>% distinct(order, .keep_all=T), by = c("genus" = "order"))
  
  all_spp = all_spp %>% 
    filter(!is.na(class)) %>% 
    rbind(missing_order) %>% 
    mutate(genus = recode(genus, 
                          "osteichthyes" = "actinopterygii",
                          "elasmobranchii" = "chondrichthyes"))
  
  ##########################Fill nutritional data by species scientific name 
  #join databases
  spp_nutrient = left_join(all_spp, afcd_species, by=c("species" = "sciname", "nutrient"))
  
  #find NAs
  missing = spp_nutrient %>% 
    filter(is.na(value)) %>% 
    select(-value, -ssd, -count, -se, -lower_ci, -upper_ci)
  
  spp_nutrient = spp_nutrient %>% 
    filter(!is.na(value)) %>% 
    mutate(taxa_match = "species")
  
  ###############Fill missing with genus #######################
  #Join datasets 
  missing = left_join(missing, afcd_genus, by=c("genus", "nutrient"))
  missing_genus = missing %>% 
    filter(!is.na(value)) %>% 
    mutate(taxa_match = "genus")
  
  ##include filled values
  spp_nutrient = rbind(spp_nutrient, missing_genus)
  
  ##Seperate remaining missing values
  missing = missing %>% 
    filter(is.na(value)) %>% 
    dplyr::select(-value, -ssd, -count, -se, -lower_ci, -upper_ci)
  
  
  ###############Fill missing with family #######################
  
  #Join datasets
  missing = left_join(missing, afcd_family, by=c("family", "nutrient"))
  missing_family = missing %>% 
    filter(!is.na(value)) %>% 
    mutate(taxa_match = "family")
  
  ##include filled values
  spp_nutrient = rbind(spp_nutrient, missing_family)
  
  ##Seperate remaining missing values
  missing = missing %>% 
    filter(is.na(value)) %>% 
    dplyr::select(-value, -ssd, -count, -se, -lower_ci, -upper_ci)
  
  ##Join for family = genus
  #Join datasets
  missing = left_join(missing, afcd_family, by=c("genus" = "family", "nutrient"))
  missing_family2 = missing %>% 
    filter(!is.na(value)) %>% 
    mutate(taxa_match = "family")
  
  ##include filled values
  spp_nutrient = rbind(spp_nutrient, missing_family2)
  
  ##Seperate remaining missing values
  missing = missing %>% 
    filter(is.na(value)) %>% 
    dplyr::select(-value, -ssd, -count, -se, -lower_ci, -upper_ci)
  
  ###############Fill missing with order #######################
  #Join datasets
  missing = left_join(missing, afcd_order, by=c("order", "nutrient"))
  missing_order = missing %>% 
    filter(!is.na(value)) %>% 
    mutate(taxa_match = "order")

  ##include filled values
  spp_nutrient = rbind(spp_nutrient, missing_order)
  
  ##Seperate remaining missing values
  missing = missing %>% 
    filter(is.na(value)) %>% 
    dplyr::select(-value, -ssd, -count, -se, -lower_ci, -upper_ci)
  
  ##genus = order
  #Join datasets
  missing = left_join(missing, afcd_order, by=c("genus" = "order", "nutrient"))
  missing_order2 = missing %>% 
    filter(!is.na(value)) %>% 
    mutate(taxa_match = "order")
  
  ##include filled values
  spp_nutrient = rbind(spp_nutrient, missing_order2)
  
  ##Seperate remaining missing values
  missing = missing %>% 
    filter(is.na(value)) %>% 
    dplyr::select(-value, -ssd, -count, -se, -lower_ci, -upper_ci)
  
  ###############Fill missing with class #######################
  
  #Join datasets
  missing = left_join(missing, afcd_class, by=c("class", "nutrient"))
  missing_class = missing %>% 
    filter(!is.na(value)) %>% 
    mutate(taxa_match = "class")
  
    ##include filled values
  spp_nutrient = rbind(spp_nutrient, missing_class)
  
  ##Seperate remaining missing values
  missing = missing %>% 
    filter(is.na(value)) %>% 
    dplyr::select(-value, -ssd, -count, -se, -lower_ci, -upper_ci)
  
  ##Genus = class
  #Join datasets
  missing = left_join(missing, afcd_class, by=c("genus" = "class", "nutrient"))
  missing_class2 = missing %>% 
    filter(!is.na(value)) %>% 
    mutate(taxa_match = "class")
  
  ##Seperate remaining missing values
  missing = missing %>% 
    filter(is.na(value)) %>% 
    mutate(taxa_match = NA)
  
  ##include filled values
  spp_nutrient = rbind(spp_nutrient, missing_class2, missing) %>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>% 
    left_join(nutrient_key, by = c("nutrient")) %>% 
    select(species, genus, spp, family, order, class, nutrient, nutrient_units, value, ssd, count, se, lower_ci, upper_ci, taxa_match)
  
 return(spp_nutrient)
  
}
  
# x = species_nutrients(sci_name = c("Homarus americanus", "Pangasius pangasius"),
#                       prep = "raw",
#                       part = "muscle tissue",
#                       nut = c("Iron, total", "Zinc"))
#   
#   
# sci_name = c("Homarus americanus", "Pangasius pangasius")
# prep = "raw"
# part = "muscle tissue"
# nut = c("Iron, total", "Zinc")
  
