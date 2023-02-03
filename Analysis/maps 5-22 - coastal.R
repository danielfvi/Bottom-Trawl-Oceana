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


#############################Nutrient supply - ASF
nut_supply_GND <- read_csv("outputs/nut_supply_GND_coastal.csv")

library(countrycode)
nut_supply_iso = nut_supply_GND %>%
  mutate(country = countrycode(iso3c, 'iso3c', 'country.name')) %>% 
  filter(!nutrient == "Protein")

ggplot(data = nut_supply_GND) +
  geom_boxplot(aes(y = perc_ASF)) +
  facet_wrap(~nutrient, scales = "free")

nut_supply_GND$perc_ASF[nut_supply_GND$nutrient=="Calcium" & nut_supply_GND$perc_ASF>5] = 5
nut_supply_GND$perc_ASF[nut_supply_GND$nutrient=="DHA+EPA" & nut_supply_GND$perc_ASF>50] = 50
nut_supply_GND$perc_ASF[nut_supply_GND$nutrient=="Vitamin A" & nut_supply_GND$perc_ASF>10] = 10
nut_supply_GND$perc_ASF[nut_supply_GND$nutrient=="Zinc" & nut_supply_GND$perc_ASF>5] = 5
nut_supply_GND$perc_ASF[nut_supply_GND$nutrient=="Vitamin B12" & nut_supply_GND$perc_ASF>10] = 10
nut_supply_GND$perc_ASF[nut_supply_GND$nutrient=="Iron" & nut_supply_GND$perc_ASF>10] = 10

# Set breaks and labels
breaks_list <- list("DHA+EPA"=c(0, 25, 50),
                    "Vitamin B12"=c(0, 5, 10),
                    "Iron"=c(0, 5, 10),
                    "Zinc"=c(0, 2.5, 5),
                    "Calcium"=c(0, 2.5, 5),
                    "Vitamin A"=c(0, 5, 10))

labels_list <- list("DHA+EPA"=c("0", "25", ">50"),
                    "Vitamin B12"=c("0", "5", ">10"),
                    "Iron"=c("0", "5", ">10"),
                    "Zinc"=c("0", "2.5", ">5"),
                    "Calcium"=c("0", "2.5", ">5"),
                    "Vitamin A"=c("0", "5", ">10"))

# percent ASF
ASF_map = function(nut){
  # Format data
  mpa_dta_sf <- world %>% 
    left_join(nut_supply_GND %>% filter(nutrient == nut), by=c("gu_a3"="iso3c"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(nut_supply_GND %>% filter(nutrient == nut), by=c("iso3"="iso3c")) %>% 
    # Reduce to ones with data
    filter(!is.na(perc_ASF)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  if(nut %in% c("DHA+EPA", "Iron", "Calcium")){
    legend.pos = "left"
  }else{
    legend.pos = "right"
  }
  
  # Get breaks and labels
  breaks <- breaks_list[[nut]]
  labels <- labels_list[[nut]]
  
  plot1 <- ggplot(mpa_dta_sf) +
    geom_sf(mapping=aes(fill=perc_ASF), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt %>% filter(nutrient == nut), mapping=aes(fill=perc_ASF), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Nutrient\nsupply (%)",
                         breaks=breaks, labels=labels,
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.6, barheight = 2)) +
    labs(title = nut)+
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=legend.pos,
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 9),
          plot.title = element_text(size = 13))
  return(plot1)
}

# Build maps
g1 <- ASF_map(nut = "DHA+EPA")
g2 <- ASF_map(nut = "Vitamin B12")
g3 <- ASF_map(nut = "Iron")
g4 <- ASF_map(nut = "Zinc")
g5 <- ASF_map(nut = "Calcium")
g6 <- ASF_map(nut = "Vitamin A")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6, ncol=2)
#g

ggsave(g, filename = "Figures/Nutrient supply relative to ASF - coastal.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Nutrient supply relative to ASF - coastal.jpeg", 
       height = 6.2, 
       width = 10)

#############################Nutrient supply relative to all other foods
ggplot(data = nut_supply_GND) +
  geom_boxplot(aes(y = perc_tot)) +
  facet_wrap(~nutrient, scales = "free")

nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="Calcium" & nut_supply_GND$perc_tot>3] = 3
nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="DHA+EPA" & nut_supply_GND$perc_tot>3] = 3
nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="Vitamin A" & nut_supply_GND$perc_tot>2] = 2
nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="Zinc" & nut_supply_GND$perc_tot>2] = 2
nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="Vitamin B12" & nut_supply_GND$perc_tot>3] = 3
nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="Iron" & nut_supply_GND$perc_tot>2] = 2

# Set breaks and labels
breaks_list_all <- list("DHA+EPA"=c(0, 1.5, 2.8),
                    "Vitamin B12"=c(0, 1.5, 2.8),
                    "Iron"=c(0, 1, 2),
                    "Zinc"=c(0, 1, 2),
                    "Calcium"=c(0, 1.5, 2.8),
                    "Vitamin A"=c(0, 1, 2))

labels_list_all <- list("DHA+EPA"=c("0", "1.5", ">3"),
                    "Vitamin B12"=c("0", "1.5", ">3"),
                    "Iron"=c("0", "1", ">2"),
                    "Zinc"=c("0", "1", ">2"),
                    "Calcium"=c("0", "1.5", ">3"),
                    "Vitamin A"=c("0", "1", ">2"))

# percent ASF
all_map = function(nut){
  # Format data
  mpa_dta_sf <- world %>% 
    left_join(nut_supply_GND %>% filter(nutrient == nut), by=c("gu_a3"="iso3c"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(nut_supply_GND %>% filter(nutrient == nut), by=c("iso3"="iso3c")) %>% 
    # Reduce to ones with data
    filter(!is.na(perc_tot)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  if(nut %in% c("DHA+EPA", "Iron", "Calcium")){
    legend.pos = "left"
  }else{
    legend.pos = "right"
  }
  
  # Get breaks and labels
  breaks <- breaks_list_all[[nut]]
  labels <- labels_list_all[[nut]]
  
  plot1 <- ggplot(mpa_dta_sf) +
    geom_sf(mapping=aes(fill=perc_tot), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt %>% filter(nutrient == nut), mapping=aes(fill=perc_tot), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Nutrient\nsupply (%)",
                         breaks=breaks, labels=labels,
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.6, barheight = 2)) +
    labs(title = nut)+
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=legend.pos,
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 9),
          plot.title = element_text(size = 13))
  return(plot1)
}

# Build maps
g1 <- ASF_map(nut = "DHA+EPA")
g2 <- all_map(nut = "Vitamin B12")
g3 <- all_map(nut = "Iron")
g4 <- all_map(nut = "Zinc")
g5 <- all_map(nut = "Calcium")
g6 <- all_map(nut = "Vitamin A")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6, ncol=2)
#g

ggsave(g, filename = "Figures/Nutrient supply relative to all other foods - coastal.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Nutrient supply relative to all other foods - coastal.jpeg", 
       height = 6.2, 
       width = 10)

#############################Nutrient supply - Fish
ggplot(data = nut_supply_GND) +
  geom_boxplot(aes(y = perc_fish)) +
  facet_wrap(~nutrient, scales = "free")

nut_supply_GND$perc_fish[nut_supply_GND$nutrient=="Calcium" & nut_supply_GND$perc_fish>30] = 30
nut_supply_GND$perc_fish[nut_supply_GND$nutrient=="DHA+EPA" & nut_supply_GND$perc_fish>30] = 30
nut_supply_GND$perc_fish[nut_supply_GND$nutrient=="Vitamin A" & nut_supply_GND$perc_fish>30] = 30
nut_supply_GND$perc_fish[nut_supply_GND$nutrient=="Zinc" & nut_supply_GND$perc_fish>30] = 30
nut_supply_GND$perc_fish[nut_supply_GND$nutrient=="Vitamin B12" & nut_supply_GND$perc_fish>30] = 30
nut_supply_GND$perc_fish[nut_supply_GND$nutrient=="Iron" & nut_supply_GND$perc_fish>30] = 30

# Set breaks and labels
breaks_list_fish <- list("DHA+EPA"=c(0, 15, 29.9),
                    "Vitamin B12"=c(0, 15, 29.9),
                    "Iron"=c(0, 15, 29.9),
                    "Zinc"=c(0, 15, 29.9),
                    "Calcium"=c(0, 15, 29.9),
                    "Vitamin A"=c(0, 15, 29.9))

labels_list_fish <- list("DHA+EPA"=c("0", "15", ">30"),
                    "Vitamin B12"=c("0", "15", ">30"),
                    "Iron"=c("0", "15", ">30"),
                    "Zinc"=c("0", "15", ">30"),
                    "Calcium"=c("0", "15", ">30"),
                    "Vitamin A"=c("0", "15", ">30"))

# percent ASF
fish_map = function(nut){
  # Format data
  mpa_dta_sf <- world %>% 
    left_join(nut_supply_GND %>% filter(nutrient == nut), by=c("gu_a3"="iso3c"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(nut_supply_GND %>% filter(nutrient == nut), by=c("iso3"="iso3c")) %>% 
    # Reduce to ones with data
    filter(!is.na(perc_fish)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  if(nut %in% c("DHA+EPA", "Iron", "Calcium")){
    legend.pos = "left"
  }else{
    legend.pos = "right"
  }
  
  # Get breaks and labels
  breaks <- breaks_list_fish[[nut]]
  labels <- labels_list_fish[[nut]]
  
  plot1 <- ggplot(mpa_dta_sf) +
    geom_sf(mapping=aes(fill=perc_fish), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt %>% filter(nutrient == nut), mapping=aes(fill=perc_fish), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Nutrient\nsupply (%)",
                         breaks=breaks, labels=labels,
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.6, barheight = 2)) +
    labs(title = nut)+
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=legend.pos,
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 9),
          plot.title = element_text(size = 13))
  return(plot1)
}

# Build maps
g1 <- fish_map(nut = "DHA+EPA")
g2 <- fish_map(nut = "Vitamin B12")
g3 <- fish_map(nut = "Iron")
g4 <- fish_map(nut = "Zinc")
g5 <- fish_map(nut = "Calcium")
g6 <- fish_map(nut = "Vitamin A")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6, ncol=2)
#g

ggsave(g, filename = "Figures/Nutrient supply relative to fish - coastal.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Nutrient supply relative to fish - coastal.jpeg", 
       height = 6.2, 
       width = 10)


#############################Change in SEVs

sevs_base_high_foreign_catch_final <- read_csv("outputs/sevs_base_high_foreign_catch_final_all.csv")  %>% 
  group_by(iso3, country, nutrient) %>%
  summarise(sev_base = mean(sev_base),
            sev_high = mean(sev_high)) %>%
  mutate(sev_delta=sev_high - sev_base,
         nutrient = recode(nutrient, 
                           "Omega-3 fatty acids" = "DHA+EPA",
                           "Vitamin A, RAE" = "Vitamin A",
                           "Vitamin B-12" = "Vitamin B12"))

ggplot(data = sevs_base_high_foreign_catch_final) +
  geom_boxplot(aes(y = sev_delta)) +
  facet_wrap(~nutrient, scales = "free")

nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="Calcium" & nut_supply_GND$perc_tot>0.03] = 0.03
nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="DHA+EPA" & nut_supply_GND$perc_tot>0.03] = 0.03
nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="Vitamin A" & nut_supply_GND$perc_tot>0.02] = 0.02
nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="Zinc" & nut_supply_GND$perc_tot>0.02] = 0.02
nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="Vitamin B12" & nut_supply_GND$perc_tot>0.03] = 0.03
nut_supply_GND$perc_tot[nut_supply_GND$nutrient=="Iron" & nut_supply_GND$perc_tot>0.02] = 0.02


# percent ASF
SEV_map = function(nut){
  # Format data
  mpa_dta_sf <- world %>% 
    left_join(sevs_base_high_foreign_catch_final %>% filter(nutrient == nut), by=c("gu_a3"="iso3"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(sevs_base_high_foreign_catch_final %>% filter(nutrient == nut), by=c("iso3"="iso3")) %>% 
    # Reduce to ones with data
    filter(!is.na(sev_delta)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  if(nut %in% c("DHA+EPA", "Iron", "Calcium")){
    legend.pos = "left"
  }else{
    legend.pos = "right"
  }
  

  plot1 <- ggplot(mpa_dta_sf) +
    geom_sf(mapping=aes(fill=sev_delta), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt %>% filter(nutrient == nut), mapping=aes(fill=sev_delta), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Change in\ninadequate\nintake (%)",
                         #breaks=breaks, labels=labels,
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.6, barheight = 2)) +
    labs(title = nut)+
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=legend.pos,
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 9),
          plot.title = element_text(size = 13))
  return(plot1)
}

# Build maps
g1 <- SEV_map(nut = "DHA+EPA")
g2 <- SEV_map(nut = "Vitamin B12")
g3 <- SEV_map(nut = "Iron")
g4 <- SEV_map(nut = "Zinc")
g5 <- SEV_map(nut = "Calcium")
g6 <- SEV_map(nut = "Vitamin A")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6, ncol=2)
#g

ggsave(g, filename = "Figures/Delta SEV.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Delta SEV.jpeg", 
       height = 6.2, 
       width = 10)

#Table
x = sevs_base_high_foreign_catch_final %>%
  ungroup() %>% 
  select(country, nutrient, sev_delta) %>% 
  spread(nutrient, sev_delta)

write.csv(x, "outputs/sev_table.csv", row.names = F)

######################Impacted pop
impacted_pop_SEV_foreign_catch <- read_csv("outputs/impacted_pop_SEV_foreign_catch_all.csv")  %>% 
  group_by(iso3, country, nutrient) %>% 
  summarise(ndeficient_diff = sum(ndeficient_diff, na.rm=T)) %>% 
  mutate(nutrient = recode(nutrient, 
                           "Omega-3 fatty acids" = "DHA+EPA",
                           "Vitamin A, RAE" = "Vitamin A",
                           "Vitamin B-12" = "Vitamin B12"),
         ndeficient_diff_log = log(ndeficient_diff),
         ndeficient_diff_log = if_else(ndeficient_diff_log>0, ndeficient_diff_log, 0))

sum(impacted_pop_SEV_foreign_catch$ndeficient_diff)/1000000


ggplot(data = impacted_pop_SEV_foreign_catch) +
  geom_boxplot(aes(y = ndeficient_diff_log)) +
  facet_wrap(~nutrient, scales = "free")


# percent ASF
pop_map = function(nut){
  # Format data
  mpa_dta_sf <- world %>% 
    left_join(impacted_pop_SEV_foreign_catch %>% filter(nutrient == nut), by=c("gu_a3"="iso3"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(impacted_pop_SEV_foreign_catch %>% filter(nutrient == nut), by=c("iso3"="iso3")) %>% 
    # Reduce to ones with data
    filter(!is.na(ndeficient_diff_log)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  if(nut %in% c("DHA+EPA", "Iron", "Calcium")){
    legend.pos = "left"
  }else{
    legend.pos = "right"
  }
  
  
  plot1 <- ggplot(mpa_dta_sf) +
    geom_sf(mapping=aes(fill=ndeficient_diff_log), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt %>% filter(nutrient == nut), mapping=aes(fill=ndeficient_diff_log), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Change in\ninadequate\nintake (%)",
                         breaks=c(2.3, 4.6, 6.9, 9.2, 11.5), 
                         labels=c("10", "100", "1,000", "10,000", "100,000"),
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.6, barheight = 2)) +
    labs(title = nut)+
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=legend.pos,
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 9),
          plot.title = element_text(size = 13))
  return(plot1)
}

# Build maps
g1 <- pop_map(nut = "DHA+EPA")
g2 <- pop_map(nut = "Vitamin B12")
g3 <- pop_map(nut = "Iron")
g4 <- pop_map(nut = "Zinc")
g5 <- pop_map(nut = "Calcium")
g6 <- pop_map(nut = "Vitamin A")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6, ncol=2)
#g

ggsave(g, filename = "Figures/Delta SEV.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/Delta SEV.jpeg", 
       height = 6.2, 
       width = 10)
