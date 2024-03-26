
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

################################################################################
########################### Load AMR datasets ##################################
################################################################################

#load prepared data from all sources of data
df_AMR <- read.csv(here("data","final_AMR_dataset.csv"))

################################################################################
########################## Proportion calculation ##############################
################################################################################

#proportion of resistance
df_AMR$p <- df_AMR$Resistant/df_AMR$Total

################################################################################
####################### Plots like Catalan et al. ##############################
################################################################################

##### GLASS vs datasets combined #####

# x = df_AMR[df_AMR$Data %in% c('GLASS'),] %>%
#   filter(Pathogen %in% c("Escherichia coli", "Klebsiella pneumoniae"))
x = df_AMR[df_AMR$Data %in% c('GLASS'),]

plot_list = list()
i=0

for(dataset in unique(df_AMR$Data)){
  
  i=i+1
  y = df_AMR %>%
    filter(Data == dataset) %>%
    group_by(Country, Region, Year, Pathogen, Antibiotic) %>%
    summarise(Total = sum(Total),
              Resistant = sum(Resistant)) %>%
    mutate(p = Resistant/Total) %>%
    ungroup
  # filter(Pathogen %in% c("Escherichia coli", "Klebsiella pneumoniae"))
  
  
  
  x_y <- merge(x, y, by = c('Country', 'Region', 'Year', 'Pathogen', 'Antibiotic')) %>%
    filter(!is.nan(p.y)) %>%
    mutate(Total = Total.y+Total.x,
           Diff = p.y-p.x) %>%
    mutate(Antibiotic = factor(Antibiotic, levels = c("Amikacin", "Gentamicin",
                                                      "Doripenem", "Imipenem", "Meropenem",
                                                      "Cefepime", "Ceftazidime", "Ceftriaxone",
                                                      "Ciprofloxacin", "Levofloxacin",
                                                      "Ampicillin", "Benzylpenicillin", "Oxacillin",
                                                      "Colistin",
                                                      "Minocycline", "Tigecycline",
                                                      "Trimethoprim/sulfamethoxazole")))
  
  x_y_text = data.frame()
  
  for(pathogen in unique(x_y$Pathogen)){
    x_y_p = x_y %>%
      filter(Pathogen == pathogen)
    
    x_y_text = rbind(x_y_text,
                     data.frame(Pathogen = pathogen,
                                Comparisons = nrow(x_y_p),
                                Data_points = sum(x_y_p$Total.y),
                                Prop_within = round(sum(abs(x_y_p$p.y-x_y_p$p.x)<=0.1)/nrow(x_y_p), 2),
                                Mean_diff = round(mean(x_y_p$p.y-x_y_p$p.x), 3)))
  }
  
  col_pal = c(Amikacin="red1", Gentamicin="firebrick3",
              Doripenem="mediumorchid1", Imipenem="mediumpurple2", Meropenem="purple3",
              Cefepime="orange1", Ceftazidime="darkorange1", Ceftriaxone="darkorange3",
              Ciprofloxacin="darkolivegreen3", Levofloxacin="darkolivegreen",
              Ampicillin="deepskyblue", Benzylpenicillin="dodgerblue", Oxacillin="royalblue",
              Colistin="gold1",
              Minocycline="black", Tigecycline="grey70",
              `Trimethoprim/sulfamethoxazole`="slategrey")
  
  col_pal = col_pal[unique(x_y$Antibiotic)]
  

  pp=ggplot(x_y) +
    geom_point(aes(x = p.x, y = p.y, colour = Antibiotic), size = 2) +
    facet_wrap(~Pathogen) +
    scale_color_discrete(type = col_pal) +
    geom_label(data = x_y_text,
               aes(x = 0.15, y = 0.75, label = paste0("Comparisons:\n",
                                                      Comparisons)), alpha = 0, label.size = NA, fontface="bold") +
    geom_label(data = x_y_text,
               aes(x = 0.15, y = 0.95, label = paste0("Data points:\n",
                                                      Data_points)), alpha = 0, label.size = NA, fontface="bold") +
    geom_label(data = x_y_text,
               aes(x = 0.77, y = 0.22, label = paste0("Fraction within +/-0.1:\n",
                                                      Prop_within)), alpha = 0, label.size = NA, fontface="bold") +
    geom_label(data = x_y_text,
               aes(x = 0.77, y = 0.05, label = paste0("Mean difference:\n",
                                                      Mean_diff)), alpha = 0, label.size = NA, fontface="bold") +
    geom_abline() +
    geom_abline(slope = 1, intercept = 0.10, linetype = "dashed") +
    geom_abline(slope = 1, intercept = -0.10, linetype = "dashed") +
    scale_y_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
    scale_x_continuous(breaks = seq(0,1,0.2), limits = c(0,1)) +
    labs(x = "Resistance proportion GLASS", y = paste0("Resistance proportion ", dataset), colour = "") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          strip.text = element_text(size = 12, face="italic"),
          legend.text = element_text(size = 12))
  plot_list[[i]] = pp
  
}


#####

plot_grid(plot_grid(plot_list[[2]]+theme(legend.position = "none"),
                    plot_list[[4]]+theme(legend.position = "none"), 
                    plot_list[[5]]+theme(legend.position = "none"), 
                    plot_list[[3]]+theme(legend.position = "none"),
                    labels = c("a)", "b)", "c)", "d)"),
                    rel_heights = c(1,0.55),
                    rel_widths = c(1,0.65)),
          get_legend(plot_list[[2]]+theme(legend.position="bottom")), rel_heights = c(1,0.11),ncol=1)

ggsave(here::here("plots","suppfig1.png"), width = 17, height = 10)

