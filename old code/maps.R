##Maps

library(tidyverse) 
library(gdata)
library(ggpubr)
library(cowplot)
library(ggnewscale)
library(ggrepel)
library(sf)
library(scatterpie)

# Clear workspace
rm(list = ls())


#Load world map
world <- rnaturalearth::ne_countries("small", returnclass = "sf")

# Extract French Guiana
fguiana <-world %>% 
  sf::st_cast(to="POLYGON") %>% 
  filter(gu_a3=="FRA") %>% 
  mutate(id=1:n()) %>% 
  select(id) %>% 
  filter(id==1)

#World centroids
world_lg <- rnaturalearth::ne_countries(scale="large", returnclass = "sf") %>% 
  mutate(area_sqkm=sf::st_area(.)/(1000*1000)) %>%
  mutate(area_sqkm=as.numeric(area_sqkm)) %>% 
  sf::st_centroid() %>% 
  select(continent, subunit, su_a3, area_sqkm) %>% 
  rename(country=subunit, iso3=su_a3) 

# Small nation centroids
world_tiny <- rnaturalearth::ne_countries(type="tiny_countries", returnclass = "sf") %>% 
  select(continent, subunit, su_a3) %>% 
  rename(country=subunit, iso3=su_a3) %>% 
  mutate(area_sqkm=10)

# Merge centroids
world_centers <- bind_rows(world_lg, world_tiny)


# Base theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=5),
                    strip.text=element_blank(),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))

##1)Discards
countries_ISO <- read.csv("data/countries_ISO.csv", encoding = "UTF-8") %>% 
  distinct(missing_countries, .keep_all = T)

nut_supply_discards <- read.csv("outputs/nut_supply_discards.csv", encoding = "UTF-8") %>% 
  left_join(countries_ISO, by=c("fishing_entity" = "missing_countries")) %>% 
  filter(year == 2018,
         !nutrient == "Protein") %>% 
  mutate(discard_ratio = if_else(discard_ratio>2.5, 2.5, discard_ratio))

ggplot(data = nut_supply_discards) +
  geom_boxplot(aes(x = nutrient, y = discard_ratio))
# Open Access reef area
discard_map = function(nut){
  # Format data
  mpa_dta_sf <- world %>% 
    left_join(nut_supply_discards %>% filter(nutrient == nut), by=c("gu_a3"="iso3c"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(nut_supply_discards %>% filter(nutrient == nut), by=c("iso3"="iso3c")) %>% 
    # Reduce to ones with data
    filter(!is.na(discard_ratio)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  plot1 <- ggplot(mpa_dta_sf) +
    geom_sf(mapping=aes(fill=discard_ratio), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt %>% filter(nutrient == nut), mapping=aes(fill=discard_ratio), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Discard ratio",
                         breaks=seq(0, 2.5, 0.5), labels=c("0", "0.5", "1.0", "1.5", "2.0", ">= 2.5"),
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.6, barheight = 2)) +
    labs(title = nut)+
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=c(0.08,0.37),
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 6))
  return(plot1)
}

# Build maps
g1 <- discard_map(nut = "DHA+EPA")
g2 <- discard_map(nut = "Vitamin B12")
g3 <- discard_map(nut = "Iron")
g4 <- discard_map(nut = "Zinc")
g5 <- discard_map(nut = "Calcium")
g6 <- discard_map(nut = "Vitamin A, RAE")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6, ncol=2)
#g

ggsave(g, filename="Figures/discard_ratio.pdf", 
       width=6.5, height=6.2, units="in", dpi=600, device=cairo_pdf)

##2)Reporting
nut_supply_reporting <- read.csv("outputs/nut_supply_reporting.csv", encoding = "UTF-8") %>% 
  left_join(countries_ISO, by=c("fishing_entity" = "missing_countries")) %>% 
  filter(year == 2018,
         !nutrient == "Protein") %>% 
  mutate(reporting_ratio = if_else(reporting_ratio>10, 10, reporting_ratio))

ggplot(data = nut_supply_reporting) +
  geom_boxplot(aes(x = nutrient, y = reporting_ratio)) +
  ylim(0, 50)

# Open Access reef area
reporting_map = function(nut){
  # Format data
  mpa_dta_sf <- world %>% 
    left_join(nut_supply_reporting %>% filter(nutrient == nut), by=c("gu_a3"="iso3c"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(nut_supply_reporting %>% filter(nutrient == nut), by=c("iso3"="iso3c")) %>% 
    # Reduce to ones with data
    filter(!is.na(reporting_ratio)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  plot1 <- ggplot(mpa_dta_sf) +
    geom_sf(mapping=aes(fill=reporting_ratio), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt %>% filter(nutrient == nut), mapping=aes(fill=reporting_ratio), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="reporting\nratio", 
                         breaks=seq(0, 10, 2), labels=c("0", "2", "4", "6", "8", ">= 10"),
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.6, barheight = 2)) +
    labs(title = nut)+
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=c(0.08,0.37),
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 6))
  return(plot1)
}

# Build maps
g1 <- reporting_map(nut = "DHA+EPA")
g2 <- reporting_map(nut = "Vitamin B12")
g3 <- reporting_map(nut = "Iron")
g4 <- reporting_map(nut = "Zinc")
g5 <- reporting_map(nut = "Calcium")
g6 <- reporting_map(nut = "Vitamin A, RAE")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6, ncol=2)
#g

ggsave(g, filename="Figures/reporting_ratio.pdf", 
       width=6.5, height=6.2, units="in", dpi=600, device=cairo_pdf)
