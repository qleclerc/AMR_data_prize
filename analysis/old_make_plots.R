
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

#names datasets
name_data = unique(df_AMR$Data)

################################################################################
########################## Countries exploration ###############################
################################################################################

#countries in each dataset
countries_GLASS = unique(df_AMR[df_AMR$Data == 'GLASS', c('Country')])
countries_GLASS
countries_ATLAS = unique(df_AMR[df_AMR$Data == 'ATLAS', c('Country')])
countries_SIDERO = unique(df_AMR[df_AMR$Data == 'SIDERO', c('Country')])
countries_GEARS = unique(df_AMR[df_AMR$Data == 'GEARS', c('Country')])
countries_KEYSTONE = unique(df_AMR[df_AMR$Data == 'KEYSTONE', c('Country')])

#intersection of countries between datasets
countries_ATLAS_GLASS <- intersect(countries_ATLAS, countries_GLASS)
countries_ATLAS_GLASS
countries_SIDERO_GLASS <- intersect(countries_GLASS, countries_SIDERO)
countries_SIDERO_GLASS
countries_GEARS_GLASS <- intersect(countries_GEARS, countries_GLASS)
countries_GEARS_GLASS
countries_KEYSTONE_GLASS <- intersect(countries_KEYSTONE, countries_GLASS)
countries_KEYSTONE_GLASS

################################################################################
########################## Proportion calculation ##############################
################################################################################

#proportion of resistance
df_AMR$p <- df_AMR$Resistant/df_AMR$Total*100

#binomial 95% confidence interval calculation
#TO DO

################################################################################
####################### Plots like Catalan et al. ##############################
################################################################################


##### Global plot #####

#choose data of interest
df_of_interest = df_AMR[df_AMR$Antibiotic == 'Ceftazidime' &
                          df_AMR$Pathogen == 'Escherichia coli' &
                          df_AMR$Data %in% c('GLASS', 'ATLAS') &
                          df_AMR$Country %in% countries_ATLAS_GLASS,]

#plot by year and country
ggplot(data = df_of_interest) +
  geom_point(aes(x = as.numeric(Year), y = (Resistant/Total), color = Data), size = 3) +
  scale_x_continuous(breaks = c(2018, 2019)) +
  facet_wrap(~Country, ncol = 8) +
  xlab('Year') +
  ylab('Proportion of resistance') +
  theme_bw()

ggsave(here("plots", "ATLAS_vs_GLASS.png"))

#####

##### GLASS vs datasets individually #####

# Currently, looking by class completely over or underestimates compared to GLASS
df_AMR_2 = df_AMR %>%
  filter(!(Antibiotic %in% c("Cephalosporins", "Carbapenems")))
x = df_AMR_2[df_AMR_2$Data %in% c('GLASS'),]

for(data in name_data){
  
  y = df_AMR_2[df_AMR_2$Data %in% data,]
  
  x_y <- merge(x, y, by = c('Country', 'Year', 'Pathogen', 'Antibiotic')) %>%
    filter(!is.nan(p.y))
  
  x_y_e_col <- x_y %>% filter(Pathogen == "Escherichia coli")
  x_y_k_pne <- x_y %>% filter(Pathogen == "Klebsiella pneumoniae")
  
  pa = ggplot(data = x_y_e_col) +
    geom_point(aes(x = p.x, y = p.y, colour = Antibiotic), size = 3) +
    facet_grid(cols = vars(Pathogen)) +
    scale_color_manual(values = pal,
                       breaks = sort(unique(df_AMR$Antibiotic))) +
    geom_text(aes(x = 27, y = 75, label = paste0("Comparisons:\n",
                                                 nrow(x_y_e_col)))) +
    geom_text(aes(x = 27, y = 90, label = paste0(data, " data points:\n",
                                                 sum(Total.y)))) +
    geom_text(aes(x = 80, y = 20, label = paste0("Prop within +/-10%:\n",
                                                 round(sum(abs(p.y-p.x)<=10)/nrow(x_y_e_col), 2)))) +
    geom_text(aes(x = 80, y = 5, label = paste0("Mean % difference:\n",
                                                round(mean(p.y-p.x), 2)))) +
    geom_abline() +
    geom_abline(slope = 1, intercept = 10, linetype = "dashed") +
    geom_abline(slope = 1, intercept = -10, linetype = "dashed") +
    scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
    scale_x_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
    xlab("% resistance GLASS") +
    ylab(paste0("% resistance ", data)) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    guides(colour = "none")
  
  pb = ggplot(data = x_y_k_pne) +
    geom_point(aes(x = p.x, y = p.y, colour = Antibiotic), size = 3) +
    facet_grid(cols = vars(Pathogen)) +
    scale_color_manual(values = pal,
                       breaks = sort(unique(df_AMR$Antibiotic))) +
    geom_text(aes(x = 25, y = 75, label = paste0("Comparisons:\n",
                                                 nrow(x_y_k_pne)))) +
    geom_text(aes(x = 25, y = 90, label = paste0(data, " data points:\n",
                                                 sum(Total.y)))) +
    geom_text(aes(x = 80, y = 20, label = paste0("Prop within +/-10%:\n",
                                                 round(sum(abs(p.y-p.x)<=10)/nrow(x_y_k_pne), 2)))) +
    geom_text(aes(x = 80, y = 5, label = paste0("Mean % difference:\n",
                                                round(mean(p.y-p.x), 2)))) +
    geom_abline() +
    geom_abline(slope = 1, intercept = 10, linetype = "dashed") +
    geom_abline(slope = 1, intercept = -10, linetype = "dashed") +
    scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
    scale_x_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
    xlab("% resistance GLASS") +
    ylab(paste0("% resistance ", data)) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = element_text(size = 12)) +
    guides(colour = "none")
  
  plot_grid(pa, pb, nrow = 1, rel_widths = c(1,0.90))
  
  ggsave(here("plots", paste0("agreement_GLASS_vs_", data, ".png")), width = 7, height = 4)
}

#####

##### GLASS vs datasets combined #####

y = df_AMR_2 %>%
  filter(Data != "GLASS") %>%
  group_by(Country, Year, Pathogen, Antibiotic) %>%
  summarise(Total = sum(Total),
            Resistant = sum(Resistant)) %>%
  mutate(Data = "All",
         p = Resistant/Total*100)


x_y <- merge(x, y, by = c('Country', 'Year', 'Pathogen', 'Antibiotic')) %>%
  filter(!is.nan(p.y))
x_y_e_col <- x_y %>% filter(Pathogen == "Escherichia coli")
x_y_k_pne <- x_y %>% filter(Pathogen == "Klebsiella pneumoniae")

pa = ggplot(data = x_y_e_col) +
  geom_point(aes(x = p.x, y = p.y, colour = Antibiotic), size = 3) +
  facet_grid(cols = vars(Pathogen)) +
  scale_color_manual(values = pal,
                     breaks = sort(unique(df_AMR$Antibiotic))) +
  geom_text(aes(x = 15, y = 80, label = paste0("Comparisons:\n",
                                                 nrow(x_y_e_col)))) +
  geom_text(aes(x = 15, y = 92, label = paste0("Data points:\n",
                                                 sum(Total.y)))) +
  geom_text(aes(x = 85, y = 17, label = paste0("Prop within +/-10%:\n",
                                                  round(sum(abs(p.y-p.x)<=10)/nrow(x_y_e_col), 2)))) +
  geom_text(aes(x = 85, y = 5, label = paste0("Mean % difference:\n",
                                                  round(mean(p.y-p.x), 2)))) +
  geom_abline() +
  geom_abline(slope = 1, intercept = 10, linetype = "dashed") +
  geom_abline(slope = 1, intercept = -10, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  xlab("% resistance GLASS") +
  ylab("% resistance ALL") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  guides(colour = "none")

pb = ggplot(data = x_y_k_pne) +
  geom_point(aes(x = p.x, y = p.y, colour = Antibiotic), size = 3) +
  facet_grid(cols = vars(Pathogen)) +
  scale_color_manual(values = pal,
                     breaks = sort(unique(df_AMR$Antibiotic))) +
  geom_text(aes(x = 15, y = 80, label = paste0("Comparisons:\n",
                                                 nrow(x_y_k_pne)))) +
  geom_text(aes(x = 15, y = 92, label = paste0("Data points:\n",
                                                 sum(Total.y)))) +
  geom_text(aes(x = 85, y = 17, label = paste0("Prop within +/-10%:\n",
                                                  round(sum(abs(p.y-p.x)<=10)/nrow(x_y_k_pne), 2)))) +
  geom_text(aes(x = 85, y = 5, label = paste0("Mean % difference:\n",
                                                  round(mean(p.y-p.x), 2)))) +
  geom_abline() +
  geom_abline(slope = 1, intercept = 10, linetype = "dashed") +
  geom_abline(slope = 1, intercept = -10, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,20), limits = c(0,100)) +
  xlab("% resistance GLASS") +
  ylab("% resistance ALL") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12)) +
  guides(colour = "none")

p_legend = get_legend(ggplot(df_AMR) +
                        geom_point(aes(Total, Resistant, colour = Antibiotic), size = 3) +
                        scale_color_manual(values = pal, breaks = sort(unique(df_AMR$Antibiotic))) +
                        theme_bw() +
                        theme(legend.position = "bottom",
                              legend.text = element_text(size = 12),
                              legend.title = element_blank()))

plot_grid(plot_grid(pa, pb, nrow = 1),
          p_legend, ncol = 1, rel_heights = c(1, 0.05))

ggsave(here("plots", paste0("agreement_GLASS_vs_ALL.png")), width = 12, height = 5)


#####