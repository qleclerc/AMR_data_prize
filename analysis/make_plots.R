
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
  df_AMR$p <- df_AMR$Resistant/df_AMR$Total
  
  #binomial 95% confidence interval calculation
  #TO DO
  
################################################################################
####################### Plots like Catalan et al. ##############################
################################################################################

##### Plots themes #####

  theme_opts <- list(theme(
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12)
    ))
  
#####
  
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
    theme_bw() +
    theme_opts

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
    
    pa = ggplot(data = x_y) +
        geom_point(aes(x = p.x, y = p.y), size = 3, color = '#7fcdbb') +
        geom_text(aes(x = 0.25, y = 0.8, label = paste0("Number of points:\n",
                                                        nrow(x_y)))) +
        geom_abline() +
        ylim(0,1) +
        xlim(0,1) +
        xlab("% resistance GLASS") +
        ylab(paste0("% resistance ", data)) +
        theme_bw() +
        theme_opts
   
    pb = ggplot(data = x_y) +
      geom_histogram(aes(p.y-p.x), fill = '#7fcdbb', binwidth = 0.01) +
      geom_text(aes(x = max(p.y-p.x)/2, y = nrow(x_y)/5, label = paste0("Prop within +/- 0.1:\n",
                                                                         round(sum(abs(p.y-p.x)<=0.1)/nrow(x_y), 4)))) +
      geom_text(aes(x = max(p.y-p.x)/2, y = nrow(x_y)/10, label = paste0("Mean difference:\n",
                                                                         round(mean(p.y-p.x), 4)))) +
      geom_vline(xintercept = 0) +
      geom_vline(xintercept = 0.1, linetype = "dashed") +
      geom_vline(xintercept = -0.1, linetype = "dashed") +
      xlab("Absolute difference") +
      ylab("Count") +
      theme_bw() +
      theme_opts
    
    plot_grid(pa, pb, nrow = 1)
    
    ggsave(here("plots", paste0("agreement_GLASS_vs_", data, ".png")))
  }
  
#####
  
  #TO DO: GLASS TO GLASS problem because multiples sources of infections !!

##### GLASS vs datasets combined #####

  y = df_AMR_2 %>%
    filter(Data != "GLASS") %>%
    group_by(Country, Year, Pathogen, Antibiotic) %>%
    summarise(Total = sum(Total),
              Resistant = sum(Resistant)) %>%
    mutate(Data = "All",
           p = Resistant/Total)
  
  
  x_y <- merge(x, y, by = c('Country', 'Year', 'Pathogen', 'Antibiotic')) %>%
    filter(!is.nan(p.y))
  
  pa = ggplot(data = x_y) +
    geom_point(aes(x = p.x, y = p.y), size = 3, color = '#7fcdbb') +
    geom_text(aes(x = 0.25, y = 0.8, label = paste0("Number of points:\n",
                                                    nrow(x_y)))) +
    geom_abline() +
    ylim(0,1) +
    xlim(0,1) +
    xlab("% resistance GLASS") +
    ylab("% resistance ALL") +
    theme_bw() +
    theme_opts
  
  pb = ggplot(data = x_y) +
    geom_histogram(aes(p.y-p.x), fill = '#7fcdbb', binwidth = 0.01) +
    geom_text(aes(x = max(p.y-p.x)/2, y = nrow(x_y)/5, label = paste0("Prop within +/- 0.1:\n",
                                                                      round(sum(abs(p.y-p.x)<=0.1)/nrow(x_y), 4)))) +
    geom_text(aes(x = max(p.y-p.x)/2, y = nrow(x_y)/10, label = paste0("Mean difference:\n",
                                                            round(mean(p.y-p.x), 4)))) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = 0.1, linetype = "dashed") +
    geom_vline(xintercept = -0.1, linetype = "dashed") +
    xlab("Absolute difference") +
    ylab("Count") +
    theme_bw() +
    theme_opts
  
  plot_grid(pa, pb, nrow = 1)
  
  ggsave(here("plots", paste0("agreement_GLASS_vs_ALL.png")))
  

#####



