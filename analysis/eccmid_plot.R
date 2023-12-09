
################################################################################

################################################################################
######################### AMR Vivli data challenge #############################
################################################################################

################################################################################

#libraries required
library(dplyr)
library(tidyr)
library(readxl)
library(tidyverse)
library(here)
library(cowplot)
library(RColorBrewer)
library(AMR)

pal = c("royalblue4", "royalblue1", "firebrick4", "firebrick2")
################################################################################
########################### Load AMR datasets ##################################
################################################################################

#load prepared data from all sources of data
df_AMR <- read.csv(here("data","final_AMR_dataset.csv"))

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

##### GLASS vs datasets combined #####

x = df_AMR[df_AMR$Data %in% c('GLASS'),] %>%
  filter(Pathogen %in% c("Escherichia coli", "Klebsiella pneumoniae"))


y = df_AMR %>%
  filter(Data != "GLASS") %>%
  group_by(Country, Region, Year, Pathogen, Antibiotic) %>%
  summarise(Total = sum(Total),
            Resistant = sum(Resistant)) %>%
  mutate(Data = "All",
         p = Resistant/Total*100) %>%
  ungroup %>%
  filter(Pathogen %in% c("Escherichia coli", "Klebsiella pneumoniae"))



x_y <- merge(x, y, by = c('Country', 'Region', 'Year', 'Pathogen', 'Antibiotic')) %>%
  filter(!is.nan(p.y)) %>%
  mutate(Total = Total.y+Total.x,
         Diff = p.y-p.x)

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

col_pal = c(RColorBrewer::brewer.pal(8,"Dark2"), "royalblue3", "firebrick3", "black")

pa = ggplot(x_y) +
  geom_point(aes(x = p.x, y = p.y, colour = Antibiotic), size = 2) +
  facet_wrap(~Pathogen) +
  scale_color_discrete(type = col_pal) +
  geom_label(data = x_y_text,
             aes(x = 15, y = 75, label = paste0("Comparisons:\n",
                                                Comparisons)), alpha = 0.5, label.size = NA) +
  geom_label(data = x_y_text,
             aes(x = 15, y = 95, label = paste0("Data points:\n",
                                                Data_points)), alpha = 0.5, label.size = NA) +
  geom_label(data = x_y_text,
             aes(x = 80, y = 22, label = paste0("Prop within +/-10%:\n",
                                                Prop_within)), alpha = 0.5, label.size = NA) +
  geom_label(data = x_y_text,
             aes(x = 80, y = 5, label = paste0("Mean % difference:\n",
                                               Mean_diff)), alpha = 0.5, label.size = NA) +
  geom_abline() +
  geom_abline(slope = 1, intercept = 10, linetype = "dashed") +
  geom_abline(slope = 1, intercept = -10, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  labs(x = "% resistance GLASS", y = "% resistance combined industry datasets", colour = "") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

pb = ggplot(x_y) +
  geom_boxplot(aes(x=Antibiotic, y=p.y, colour = Antibiotic)) +
  facet_wrap(~Pathogen) +
  scale_x_discrete(breaks = sort(unique(x_y$Antibiotic)),
                   labels = as.ab(sort(unique(x_y$Antibiotic)))) +
  scale_color_discrete(type=col_pal) +
  theme_bw() +
  guides(colour = "none") +
  labs(y = "% resistance combined industry dataset")+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12))


plot_grid(pa, pb, ncol = 1, labels = c("a)", "b)"), hjust = 0, rel_heights = c(1,0.85))
ggsave(here::here("plots", "eccmid_plot.png"), height = 9, width = 8)

#####