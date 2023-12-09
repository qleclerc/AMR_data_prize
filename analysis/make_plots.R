
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
  
  x_y <- merge(x, y, by = c('Country', 'Region', 'Year', 'Pathogen', 'Antibiotic')) %>%
    filter(!is.nan(p.y)) %>%
    mutate(Total = Total.y+Total.x,
           Diff = p.y-p.x)
  
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
  
  ggplot(x_y) +
    geom_point(aes(x = p.x, y = p.y, colour = Antibiotic), size = 3) +
    facet_wrap(~Pathogen) +
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
  
  ggplot(x_y) +
    geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_brewer(palette = "Dark2") +
    labs(x = paste0("Isolates from ", unique(x_y$Data.y)," used in comparison"),
         y = "Difference in resistance (percentage points)",
         colour = "WHO Region:") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12))
  
  ggsave(here("plots", time_lab, paste0("agreement_by_size_GLASS_vs_", data, ".png")),
         width = 7, height = 5)
  
}

#####

##### GLASS vs datasets combined #####

y = df_AMR %>%
  filter(Data != "GLASS") %>%
  group_by(Country, Region, Year, Pathogen, Antibiotic) %>%
  summarise(Total = sum(Total),
            Resistant = sum(Resistant)) %>%
  mutate(Data = "All",
         p = Resistant/Total*100)


x_y <- merge(x, y, by = c('Country', 'Region', 'Year', 'Pathogen', 'Antibiotic')) %>%
  filter(!is.nan(p.y)) %>%
  mutate(Total = Total.y+Total.x,
         Diff = p.y-p.x)

p0 = ggplot(x_y) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = paste0("Isolates from ", unique(x_y$Data.y)," used in comparison"),
       y = "Difference in resistance (percentage points)",
       colour = "WHO Region:") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))


af_data = x_y %>%
  filter(Region == "African Region") %>%
  mutate(Diff = abs(Diff))
cor_af = cor.test(af_data$Diff, af_data$Total.y)

p1 = ggplot(af_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_af$estimate, 2),
                                "\np val: ", round(cor_af$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[1]) +
  labs(x = paste0("Isolates from ", unique(x_y$Data.y)," used in comparison"),
       y = "Absolute diff. in resistance",
       colour = "WHO Region:") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  guides(colour = "none")

em_data = x_y %>%
  filter(Region == "Eastern Mediterranean Region") %>%
  mutate(Diff = abs(Diff))
cor_em=cor.test(em_data$Diff, em_data$Total.y)

p2 = ggplot(em_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_em$estimate, 2),
                                "\np val: ", round(cor_em$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[2]) +
  labs(x = paste0("Isolates from ", unique(x_y$Data.y)," used in comparison"),
       y = "Absolute diff. in resistance",
       colour = "WHO Region:") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  guides(colour = "none")

eu_data = x_y %>%
  filter(Region == "European Region") %>%
  mutate(Diff = abs(Diff))
cor_eu=cor.test(eu_data$Diff, eu_data$Total.y)

p3 = ggplot(eu_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_eu$estimate, 2),
                                "\np val: ", round(cor_eu$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[3]) +
  labs(x = paste0("Isolates from ", unique(x_y$Data.y)," used in comparison"),
       y = "Absolute diff. in resistance",
       colour = "WHO Region:") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  guides(colour = "none")

am_data = x_y %>%
  filter(Region == "Region of the Americas") %>%
  mutate(Diff = abs(Diff))
cor_am=cor.test(am_data$Diff, am_data$Total.y)

p4 = ggplot(am_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_am$estimate, 2),
                                "\np val: ", round(cor_am$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[4]) +
  labs(x = paste0("Isolates from ", unique(x_y$Data.y)," used in comparison"),
       y = "Absolute diff. in resistance",
       colour = "WHO Region:") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  guides(colour = "none")

se_data = x_y %>%
  filter(Region == "South-East Asia Region") %>%
  mutate(Diff = abs(Diff))
cor_se=cor.test(se_data$Diff, se_data$Total.y)

p5 = ggplot(se_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_se$estimate, 2),
                                "\np val: ", round(cor_se$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[5]) +
  labs(x = paste0("Isolates from ", unique(x_y$Data.y)," used in comparison"),
       y = "Absolute diff. in resistance",
       colour = "WHO Region:") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  guides(colour = "none")

wp_data = x_y %>%
  filter(Region == "Western Pacific Region") %>%
  mutate(Diff = abs(Diff))
cor_wp=cor.test(wp_data$Diff, wp_data$Total.y)

p6 = ggplot(wp_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_wp$estimate, 2),
                                "\np val: ", round(cor_wp$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[6]) +
  labs(x = paste0("Isolates from ", unique(x_y$Data.y)," used in comparison"),
       y = "Absolute diff. in resistance",
       colour = "WHO Region:") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  guides(colour = "none")

plot_grid(p0,
          plot_grid(p1 + theme(axis.title.x = element_blank()),
                    p2 + theme(axis.title.y = element_blank(),
                               axis.title.x = element_blank()),
                    p3 + theme(axis.title.y = element_blank(),
                               axis.title.x = element_blank()),
                    p4,
                    p5 + theme(axis.title.y = element_blank()),
                    p6 + theme(axis.title.y = element_blank()),
                    ncol=3), ncol=1, labels = c("a)", "b)"), hjust = 0, vjust = c(1,0))

ggsave(here("plots", time_lab, paste0("agreement_by_size_GLASS_vs_ALL.png")),
       width = 9, height = 10)


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
  geom_point(aes(x = p.x, y = p.y, colour = Antibiotic), size = 2) +
  facet_wrap(~Pathogen) +
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
  labs(x = "% resistance GLASS", y = "% resistance ALL", colour = "") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

ggsave(here("plots", time_lab, paste0("agreement_GLASS_vs_ALL.png")),
       width = 10, height = 8)

cat("Plots saved in the folder:", here::here("plots", time_lab), "\n")

#####