
################################################################################

################################################################################
######################### AMR Vivli data challenge #############################
################################################################################

################################################################################

#libraries required
library(dplyr)
library(tidyr)
library(readxl) #to read excel files
library(tidyverse) # str_replace_all()
library(here)
library(cowplot)
library(RColorBrewer)

pal = c("royalblue4", "royalblue1", "firebrick4", "firebrick2")
################################################################################
########################### Load AMR datasets ##################################
################################################################################

#load prepared data from all sources of data
df_AMR <- read.csv(here("data","final_AMR_dataset.csv"))

time_lab = gsub(":", "_", gsub(" ", "_", Sys.time()))
dir.create(here::here("plots", time_lab))

#names datasets
name_data = unique(df_AMR$Data)

################################################################################
########################## Countries exploration ###############################
################################################################################

#countries in each dataset
countries_GLASS = unique(df_AMR[df_AMR$Data == 'GLASS', c('Country')])
countries_ATLAS = unique(df_AMR[df_AMR$Data == 'ATLAS', c('Country')])
countries_SIDERO = unique(df_AMR[df_AMR$Data == 'SIDERO', c('Country')])
countries_GEARS = unique(df_AMR[df_AMR$Data == 'GEARS', c('Country')])
countries_KEYSTONE = unique(df_AMR[df_AMR$Data == 'KEYSTONE', c('Country')])

#intersection of countries between datasets
countries_ATLAS_GLASS <- intersect(countries_ATLAS, countries_GLASS)
countries_SIDERO_GLASS <- intersect(countries_GLASS, countries_SIDERO)
countries_GEARS_GLASS <- intersect(countries_GEARS, countries_GLASS)
countries_KEYSTONE_GLASS <- intersect(countries_KEYSTONE, countries_GLASS)

################################################################################
########################## Proportion calculation ##############################
################################################################################

#proportion of resistance
df_AMR$p <- df_AMR$Resistant/df_AMR$Total*100

################################################################################
####################### Plots like Catalan et al. ##############################
################################################################################

##### GLASS vs datasets individually #####

x = df_AMR[df_AMR$Data %in% c('GLASS'),]

for(data in name_data){
  
  y = df_AMR[df_AMR$Data %in% data,]
  
  x_y <- merge(x, y, by = c('Country', 'Year', 'Pathogen', 'Antibiotic')) %>%
    filter(!is.nan(p.y))
  
  if(nrow(x_y)==0){
    cat("Warning: no overlap between", data, "and GLASS, skipping...\n")
    next
  }
  
  x_y_text = data.frame()
  
  for(pathogen in unique(x_y$Pathogen)){
    x_y_p = x_y %>%
      filter(Pathogen == pathogen)
    
    x_y_text = rbind(x_y_text,
                     data.frame(Pathogen = pathogen,
                                Comparisons = nrow(x_y_p),
                                Data_points = sum(x_y_p$Total.y),
                                Prop_within = round(sum(abs(x_y_p$p.y-x_y_p$p.x)<=10)/nrow(x_y_p), 2),
                                Mean_diff = round(mean(x_y_p$p.y-x_y_p$p.x), 2)))
  }
  
  ggplot(data = x_y) +
    geom_point(aes(x = p.x, y = p.y, colour = Antibiotic), size = 3) +
    facet_grid(cols = vars(Pathogen)) +
    scale_color_manual(values = pal,
                       breaks = sort(unique(df_AMR$Antibiotic))) +
    geom_text(data = x_y_text,
              aes(x = 15, y = 80, label = paste0("Comparisons:\n",
                                                 Comparisons))) +
    geom_text(data = x_y_text,
              aes(x = 15, y = 92, label = paste0("Data points:\n",
                                                 Data_points))) +
    geom_text(data = x_y_text,
              aes(x = 85, y = 17, label = paste0("Prop within +/-10%:\n",
                                                 Prop_within))) +
    geom_text(data = x_y_text,
              aes(x = 85, y = 5, label = paste0("Mean % difference:\n",
                                                Mean_diff))) +
    geom_abline() +
    geom_abline(slope = 1, intercept = 10, linetype = "dashed") +
    geom_abline(slope = 1, intercept = -10, linetype = "dashed") +
    scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
    scale_x_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
    labs(x = "% resistance GLASS", y = paste0("% resistance ", data), colour = "") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          strip.text = element_text(size = 12),
          legend.position = "bottom",
          legend.text = element_text(size = 12))
  
  ggsave(here("plots", time_lab, paste0("agreement_GLASS_vs_", data, ".png")))
  
}

#####

##### GLASS vs datasets combined #####

y = df_AMR %>%
  filter(Data != "GLASS") %>%
  group_by(Country, Year, Pathogen, Antibiotic) %>%
  summarise(Total = sum(Total),
            Resistant = sum(Resistant)) %>%
  mutate(Data = "All",
         p = Resistant/Total*100)


x_y <- merge(x, y, by = c('Country', 'Year', 'Pathogen', 'Antibiotic')) %>%
  filter(!is.nan(p.y))

x_y_text = data.frame()

for(pathogen in unique(x_y$Pathogen)){
  x_y_p = x_y %>%
    filter(Pathogen == pathogen)
  
  x_y_text = rbind(x_y_text,
                   data.frame(Pathogen = pathogen,
                              Comparisons = nrow(x_y_p),
                              Data_points = sum(x_y_p$Total.y),
                              Prop_within = round(sum(abs(x_y_p$p.y-x_y_p$p.x)<=10)/nrow(x_y_p), 2),
                              Mean_diff = round(mean(x_y_p$p.y-x_y_p$p.x), 2)))
}

ggplot(data = x_y) +
  geom_point(aes(x = p.x, y = p.y, colour = Antibiotic), size = 3) +
  facet_grid(cols = vars(Pathogen)) +
  scale_color_manual(values = pal,
                     breaks = sort(unique(df_AMR$Antibiotic))) +
  geom_text(data = x_y_text,
            aes(x = 15, y = 80, label = paste0("Comparisons:\n",
                                               Comparisons))) +
  geom_text(data = x_y_text,
            aes(x = 15, y = 92, label = paste0("Data points:\n",
                                               Data_points))) +
  geom_text(data = x_y_text,
            aes(x = 85, y = 17, label = paste0("Prop within +/-10%:\n",
                                               Prop_within))) +
  geom_text(data = x_y_text,
            aes(x = 85, y = 5, label = paste0("Mean % difference:\n",
                                              Mean_diff))) +
  geom_abline() +
  geom_abline(slope = 1, intercept = 10, linetype = "dashed") +
  geom_abline(slope = 1, intercept = -10, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(x = "% resistance GLASS", y = "% resistance ALL", colour = "") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

ggsave(here("plots", time_lab, paste0("agreement_GLASS_vs_ALL.png")))

cat("Plots saved in the folder:", here::here("plots", time_lab), "\n")

#####