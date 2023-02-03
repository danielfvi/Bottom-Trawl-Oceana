##Bottom Trawl  - Oceana
library(tidyverse)
library(countrycode)
library(ggrepel)

# Clear workspace
rm(list = ls())

data <- read.csv("data/catch_data_2014-2018.csv", encoding = "UTF-8") %>% 
  mutate(fishing_entity = recode(fishing_entity, "Côte d'Ivoire" = "Ivory Coast"))
countries_ISO <- read.csv("data/countries_ISO.csv", encoding = "UTF-8")

sevs = read_csv("~/MPA_Nutrition/data/2017_perc_pop_deficient.csv") %>% 
  mutate(nutrient = dplyr::recode(nutrient, 
                                  "Vitamin B-12" = "Vitamin B12",
                                  "Vitamin A, RAE" = "Vitamin A")) %>% 
  group_by(iso3) %>% 
  summarise(intake = mean(perc_deficient))

dat = data %>%
  filter(gear == "bottom trawl",
         sector == "Industrial") %>% 
  filter(eez == "Guinea-Bissau") %>% 
  group_by(common_name, scientific_name) %>% 
  summarise(tonnes = sum(tonnes))

dat2 = data %>% 
  filter(gear == "bottom trawl",
         sector == "Industrial") %>% 
  separate(eez, c("eez_country", "eez_region"), "([_;,()%/])", remove=FALSE) %>%
  left_join(countries_ISO, by=c("fishing_entity" = "missing_countries")) %>% 
  rename(fishing_entity_ISO = iso3c) %>% 
  mutate(eez_country = str_trim(eez_country)) %>% 
  mutate(eez_iso = countrycode(eez_country,'country.name', 'iso3c')) %>% 
  group_by(eez_country, eez_iso, fishing_entity, fishing_entity_ISO) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  mutate(is_same = if_else(eez_iso == fishing_entity_ISO, "your_country", "other_country"),
         continent = countrycode(eez_country,'country.name', 'continent'))

dat3 = dat2 %>% 
  group_by(eez_country, eez_iso, continent, is_same) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  spread(is_same, tonnes) %>% 
  mutate(your_country = if_else(is.na(your_country), 0, your_country),
         other_country = if_else(is.na(other_country), 0, other_country),
         total = your_country + other_country,
         perc_foreign = 100*other_country/total,
         total_log = log(total),
         total_log = if_else(total_log<0, 1, total_log)) %>% 
  left_join(sevs, by=c("eez_iso" = "iso3"))

write.csv(dat3, "outputs/foreign_catch.csv", row.names = F)

dat4 = dat2 %>% 
  group_by(fishing_entity_ISO, is_same) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  spread(is_same, tonnes) %>% 
  rename(iso3c = fishing_entity_ISO,
         your_country_entity = your_country,
         other_country_entity = other_country) %>% 
  select(-"<NA>")

dat5 = dat3 %>% 
  rename(iso3c = eez_iso) %>% 
  left_join(dat4) %>% 
  drop_na(iso3c) %>%
  rename(your_country_eez = your_country,
         other_country_eez = other_country) %>% 
  ungroup() %>% 
  select(iso3c, your_country_eez, other_country_eez, your_country_entity, other_country_entity)

write.csv(dat5, "outputs/foreign_catch_all.csv", row.names = F)

##Plot
foreign_catch <- read_csv("outputs/foreign_catch.csv")

p = ggplot(data = foreign_catch, aes(x = total_log, y = perc_foreign, fill = intake, label = eez_iso)) +
  geom_point(shape=21, alpha = 0.6, size = 5) +
  geom_text_repel(min.segment.length = 0.5, seed = 42, box.padding = 1, size = 5) +
  scale_fill_gradientn(name="Inadequate \nintake (%)", colors=RColorBrewer::brewer.pal(9, "Reds"), na.value = "grey") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 1, barheight = 5, order = 2)) +
  scale_x_continuous(breaks=c(2.3, 4.6, 6.9, 9.2, 11.5, 13.8), 
                     labels=c("10", "100", "1,000", "10,000", "100,000", "1,000,000")) +
  labs(x="Total bottom trawl catch (tonnes)", 
       y = "Capture by foreign fleet (%)") +
  theme_bw() +
  theme(axis.title = element_text(size = 23),
        axis.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 17),
        legend.title.align = 0,
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

p
  
ggsave(p, filename = "Figures/Nutrient flows by foreign catch.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(p, filename = "Figures/Nutrient flows by foreign catch.jpeg", 
       height = 8, 
       width = 12)

##Stats
stats_fc = foreign_catch %>% 
  mutate(is_vulnerable = if_else(intake>25, 1, 0),
         is_foreign = if_else(perc_foreign>90, 3, 0),
         is_all = is_vulnerable + is_foreign)

x = stats_fc %>% 
  filter(is_vulnerable == 1)

y = stats_fc %>% 
  filter(is_all == 4)
