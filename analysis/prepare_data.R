
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

################################################################################
########################### Load AMR datasets ##################################
################################################################################

#industry
df_ATLAS <- read.csv(here("data", "raw", "2023_06_15 atlas_antibiotics.csv"))
df_SIDERO <- read_excel(here("data", "raw", "Updated_Shionogi Five year SIDERO-WT Surveillance data(without strain number)_Vivli_220409.xlsx"))
df_DREAM <- read_excel(here("data", "raw", "BEDAQUILINE DREAM DATASET FOR VIVLI - 06-06-2022.xlsx"))
df_GEARS <- read_excel(here("data", "raw", "Venatorx surveillance data for Vivli 27Feb2023.xlsx"))
df_SOARE <- read.csv(here("data", "raw", "gsk_201818_published.csv"))
df_KEYSTONE <- read_excel(here("data", "raw", "Omadacycline_2014_to_2022_Surveillance_data.xlsx"))

#public
df_GLASS <- read.csv("https://raw.githubusercontent.com/qleclerc/GLASS2022/master/compiled_WHO_GLASS_2022.csv") #Quentin Git

################################################################################
############### Choose variable of interests across datasets ###################
################################################################################

  #Countries
  
  #Years
    years_of_interest = c(2018, 2019)
  
  #Bacterial species 
    #E. coli & K. pneumoniae --> not in DREAM and SOARE
    bacteria_of_interest = c("E. coli", "Escherichia coli", "K. pneumoniae", "Klebsiella pneumoniae")
  
  #Antibiotics tested
    #TO DO: !! WE HAVE TO CHOOSE WHICH MOLECULES !!
    #TO DO: !! WE HAVE TO CHOOSE ALGORITHM TO CHOOSE RESISTANCE IN ONE CLASS !!
    #ESBL (3GC) and carbapenems (CBP)
    molecules_ESBL = c('Ceftazidime', 'Ceftriaxone')
    molecules_CBP = c('Imipenem', 'Meropenem')
  
  #Resistance status to antibiotics
    #number of R isolates
    #number of total isolates
    #TO DO: for MIC data, which threshold to use = CLSI or EUCAST?
    #TO DO: choose NonResistant as Susceptible and Intermediate?
    #TO DO: if resistant to one, resistant to the class ? (3GC or carbapenems)
    #TO DO: 3GC as proxy for ESBL?


################################################################################
############### Get same variables for all AMR datasets ########################
################################################################################

#### ATLAS ####
  colnames(df_ATLAS)
  df_ATLAS_2 <- df_ATLAS[,c("Country", "Year", "Species", paste0(molecules_ESBL, "_I"), paste0(molecules_CBP, "_I"))]
  #get bacteria
  unique(df_ATLAS_2$Species)
  df_ATLAS_2 <- df_ATLAS_2 %>% filter(Species %in% bacteria_of_interest)
  #get years
  df_ATLAS_2 <- df_ATLAS_2 %>% filter(Year %in% years_of_interest)
  
  #INFOS: MIC and resistance status
  #no data for ceftriaxone for 2018-2019
#####
  
#### GEARS ####
  colnames(df_GEARS)
  df_GEARS_2 <- df_GEARS[,c("Country", "Year", "Organism", "CAZ_MIC", "IPM_MIC", "MEM_MIC")]
  #get bacteria
  unique(df_GEARS_2$Organism)
  df_GEARS_2 <- df_GEARS_2 %>% filter(Organism %in% bacteria_of_interest)
  #get years
  df_GEARS_2 <- df_GEARS_2 %>% filter(Year %in% years_of_interest)
  
  #INFOS: MIC data
  #TO DO: convert to resistance status
  #TO DO: what are the molecules??
  #CAZ: ceftazidime
  #no data for ceftriaxone
  #IPM = imipenem? --> no data it seems for the two bugs
  #MEM = meropenem?
#####
  
#### KEYSTONE ####
  colnames(df_KEYSTONE)
  df_KEYSTONE_2 <- df_KEYSTONE[,c("Country", "Study Year", "Organism", molecules_ESBL, molecules_CBP[1])]
  #get bacteria
  unique(df_KEYSTONE_2$Organism)
  df_KEYSTONE_2 <- df_KEYSTONE_2 %>% filter(Organism %in% bacteria_of_interest)
  #get years
  df_KEYSTONE_2 <- df_KEYSTONE_2 %>% filter(`Study Year` %in% years_of_interest)
  
  #INFOS: MIC data
  #no data for meropenem
  #TO DO: convert to resistance status
  #FALSE or TRUE ???
#####

#### SIDERO ####
  colnames(df_SIDERO)
  df_SIDERO_2 <- df_SIDERO[,c("Country", "Year Collected", "Organism Name", "Ceftazidime/ Avibactam", "Imipenem/ Relebactam", molecules_CBP[2])]
  #get bacteria
  unique(df_SIDERO_2$`Organism Name`)
  df_SIDERO_2 <- df_SIDERO_2 %>% filter(`Organism Name` %in% bacteria_of_interest)
  #get years
  df_SIDERO_2 <- df_SIDERO_2 %>% filter(`Year Collected` %in% years_of_interest)
  
  #Infos: MIC data
  #TO DO: convert to resistance status
  #TO DO: what are the molecules?
  #no data for ceftriaxone
  #combinations for ceftazidime and imipenem !!
#####

#### GLASS ####
  colnames(df_GLASS)
  df_GLASS_2 <- df_GLASS[,c("CountryTerritoryArea", "Year", "PathogenName", "AbTargets", "InterpretableAST", "Resistant")]
  #get molecules
  unique(df_GLASS_2$AbTargets)
  df_GLASS_2 <- df_GLASS_2 %>% filter(AbTargets %in% c(molecules_ESBL, molecules_CBP))
  #get bacteria
  unique(df_GLASS_2$PathogenName)
  df_GLASS_2 <- df_GLASS_2 %>% filter(PathogenName %in% bacteria_of_interest)
  #get years
  df_GLASS_2 <- df_GLASS_2 %>% filter(Year %in% years_of_interest)
  
  #INFOS: resistance status
#####
  
################################################################################
############## Compute Resistance status when only MIC available ###############
################################################################################

## TO DO for:
  ## GEARS
  ## KEYSTONE
  ## SIDERO
  ## ATLAS (for NonResistant status)
  
## Get thresholds - here EUCAST in mg/L (E. coli and K. pneumoniae the same)
  t_ceftazidime = 4
  t_ceftriaxone = 2
  t_imipenem = 4
  t_meropenem = 8
  
## Get S, I or R status based on thresholds (name of column like ATLAS)
  
## GEARS
  colnames(df_GEARS_2)
  summary(df_GEARS_2)
  
  df_GEARS_2$Ceftazidime_I <- ifelse((is.na(df_GEARS_2$CAZ_MIC) | df_GEARS_2$CAZ_MIC == "-"),
                                   NA,
                                   ifelse(df_GEARS_2$CAZ_MIC > t_ceftazidime,
                                          'Resistant',
                                          'NonResistant')
                                    )
  
  df_GEARS_2$Imipenem_I <- ifelse((is.na(df_GEARS_2$IPM_MIC) | df_GEARS_2$IPM_MIC == "-"),
                                  NA,
                                  ifelse(df_GEARS_2$IPM_MIC > t_imipenem,
                                         "Resistant",
                                         "NonResistant")
                                  )
  
  df_GEARS_2$Meropenem_I <- ifelse((is.na(df_GEARS_2$MEM_MIC) | df_GEARS_2$MEM_MIC == "-"),
                                     NA,
                                     ifelse(df_GEARS_2$MEM_MIC > t_meropenem,
                                                'Resistant',
                                                'NonResistant')
                                     )
  #get rid of MIC data
  df_GEARS_2 <- df_GEARS_2 %>%
    select(!c("CAZ_MIC", "IPM_MIC", "MEM_MIC"))

## KEYSTONE  
  colnames(df_KEYSTONE_2)
  summary(df_KEYSTONE_2)
  # summary(as.factor(df_KEYSTONE_2$Imipenem))
  
  df_KEYSTONE_2$Ceftazidime_I <- ifelse(is.na(df_KEYSTONE_2$Ceftazidime),
                                          NA,
                                          ifelse(df_KEYSTONE_2$Ceftazidime == 'TRUE',
                                                 "Resistant",
                                                 "NonResistant")
                                          ) # TRUE OR FALSE ???????
  
  df_KEYSTONE_2$Ceftriaxone_I <- ifelse(is.na(df_KEYSTONE_2$Ceftriaxone),
                                          NA, 
                                          ifelse(df_KEYSTONE_2$Ceftriaxone == 'TRUE',
                                                 "Resistant",
                                                 "NonResistant")
                                          ) # TRUE OR FALSE ???????

  df_KEYSTONE_2$Imipenem_I <- ifelse(is.na(df_KEYSTONE_2$Imipenem),
                                       NA, ifelse(df_KEYSTONE_2$Imipenem == '≤0.12',
                                                  'NonResistant', ifelse(df_KEYSTONE_2$Imipenem == '>8',
                                                                         'Resistant', ifelse(as.numeric(df_KEYSTONE_2$Imipenem) > t_imipenem,
                                                                                             'Resistant',
                                                                                             'NonResistant')
                                                  )
                                            )
                                      )
  #get rid of MIC data
  df_KEYSTONE_2 <- df_KEYSTONE_2 %>%
    select(!c("Ceftazidime", "Ceftriaxone", "Imipenem"))

## SIDERO
  colnames(df_SIDERO_2)
  summary(df_SIDERO_2)
  # summary(as.factor(df_SIDERO_2$`Imipenem/ Relebactam`))
  
  df_SIDERO_2$Ceftazidime_I <- ifelse(is.na(df_SIDERO_2$`Ceftazidime/ Avibactam`),
                                      NA, ifelse(df_SIDERO_2$`Ceftazidime/ Avibactam` > t_ceftazidime,
                                                 "Resistant",
                                                 "NonResistant")
                                      )
  
  df_SIDERO_2$Imipenem_I <- ifelse(is.na(df_SIDERO_2$`Imipenem/ Relebactam`) |  df_SIDERO_2$`Imipenem/ Relebactam` == 'NULL',
                                   NA, ifelse(as.numeric(df_SIDERO_2$`Imipenem/ Relebactam`) > t_imipenem,
                                             "Resistant",
                                             "NonResistant")
  )
  
  df_SIDERO_2$Meropenem_I <- ifelse(is.na(df_SIDERO_2$Meropenem),
                                       NA, ifelse(df_SIDERO_2$Meropenem > t_meropenem,
                                                  "Resistant",
                                                  "NonResistant")
                                    )
  #get rid of MIC data
  df_SIDERO_2 <- df_SIDERO_2 %>%
    select(!c("Ceftazidime/ Avibactam", "Imipenem/ Relebactam", "Meropenem"))
  
## ATLAS
  colnames(df_ATLAS_2)
  summary(df_ATLAS_2)
  
  df_ATLAS_2$Ceftazidime_I <- ifelse(df_ATLAS_2$Ceftazidime_I == 'Intermediate' | df_ATLAS_2$Ceftazidime_I == 'Susceptible',
                                     'NonResistant',
                                     df_ATLAS_2$Ceftazidime_I)
  
  df_ATLAS_2$Ceftriaxone_I <- ifelse(df_ATLAS_2$Ceftriaxone_I == 'Intermediate' | df_ATLAS_2$Ceftriaxone_I == 'Susceptible',
                                     'NonResistant',
                                     df_ATLAS_2$Ceftriaxone_I)

  df_ATLAS_2$Imipenem_I <- ifelse(df_ATLAS_2$Imipenem_I == 'Intermediate' | df_ATLAS_2$Imipenem_I == 'Susceptible',
                                  'NonResistant',
                                  df_ATLAS_2$Imipenem_I)
  
  df_ATLAS_2$Meropenem_I <- ifelse(df_ATLAS_2$Meropenem_I == 'Intermediate' | df_ATLAS_2$Meropenem_I == 'Susceptible',
                                   'NonResistant',
                                   df_ATLAS_2$Meropenem_I)

  
################################################################################
####### Compute total and R isolates by country/year/bacteria/antibiotic #######
################################################################################
  
## Format like in GLASS
  final_column_names = c("Country", "Year", "Pathogen", "Antibiotic", "Total", "Resistant")
  
#### ATLAS ####
  
  ## Ceftazidime
    
    #count isolates by country, year, species
    df_ATLAS_2_Ceftazidime <- df_ATLAS_2 %>%
      group_by(Country, Year, Species, .drop = FALSE) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Ceftazidime_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Ceftazidime_I) %>%
      filter(Ceftazidime_I == 'Resistant' | Ceftazidime_I == 'NonResistant')
    #spread for antibiotic
    df_ATLAS_2_Ceftazidime <- spread(df_ATLAS_2_Ceftazidime, key = Ceftazidime_I, value = n)
    #antibiotic
    df_ATLAS_2_Ceftazidime$Antibiotic <- 'Ceftazidime'
    #total isolates
    df_ATLAS_2_Ceftazidime$Total <- rowSums(df_ATLAS_2_Ceftazidime[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_ATLAS_2_Ceftazidime <- df_ATLAS_2_Ceftazidime[,c("Country", "Year", "Species", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_ATLAS_2_Ceftazidime) <- final_column_names
    
  ## Imipenem
    
    #count isolates by country, year, species
    df_ATLAS_2_Imipenem <- df_ATLAS_2 %>%
      group_by(Country, Year, Species, .drop = FALSE) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Imipenem_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Imipenem_I) %>%
      filter(Imipenem_I == 'Resistant' | Imipenem_I == 'NonResistant')
    #spread for antibiotic
    df_ATLAS_2_Imipenem <- spread(df_ATLAS_2_Imipenem, key = Imipenem_I, value = n)
    #antibiotic
    df_ATLAS_2_Imipenem$Antibiotic <- 'Imipenem'
    #total isolates
    df_ATLAS_2_Imipenem$Total <- rowSums(df_ATLAS_2_Imipenem[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_ATLAS_2_Imipenem <- df_ATLAS_2_Imipenem[,c("Country", "Year", "Species", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_ATLAS_2_Imipenem) <- final_column_names
    
  ## Meropenem
    
    #count isolates by country, year, species
    df_ATLAS_2_Meropenem <- df_ATLAS_2 %>%
      group_by(Country, Year, Species, .drop = FALSE ) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Meropenem_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Meropenem_I) %>%
      filter(Meropenem_I == 'Resistant' | Meropenem_I == 'NonResistant')
    #spread for antibiotic
    df_ATLAS_2_Meropenem <- spread(df_ATLAS_2_Meropenem, key = Meropenem_I, value = n)
    #antibiotic
    df_ATLAS_2_Meropenem$Antibiotic <- 'Meropenem'
    #total isolates
    df_ATLAS_2_Meropenem$Total <- rowSums(df_ATLAS_2_Meropenem[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_ATLAS_2_Meropenem <- df_ATLAS_2_Meropenem[,c("Country", "Year", "Species", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_ATLAS_2_Meropenem) <- final_column_names
    
  ## Merge molecules
    df_ATLAS_2_final <- rbind(df_ATLAS_2_Ceftazidime, df_ATLAS_2_Imipenem, df_ATLAS_2_Meropenem)
    
  ## Make NA into 0s
    df_ATLAS_2_final$Resistant <- ifelse(is.na(df_ATLAS_2_final$Resistant),
                                         0,
                                         df_ATLAS_2_final$Resistant) #OK ????
    
  ## Add dataset name
    df_ATLAS_2_final$Data <- 'ATLAS'

#####

##### GEARS ####
    
  ## Ceftazidime
    
    #count isolates by country, year, species
    df_GEARS_2_Ceftazidime <- df_GEARS_2 %>%
      group_by(Country, Year, Organism, .drop = FALSE) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Ceftazidime_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Ceftazidime_I) %>%
      filter(Ceftazidime_I == 'Resistant' | Ceftazidime_I == 'NonResistant')
    #spread for antibiotic
    df_GEARS_2_Ceftazidime <- spread(df_GEARS_2_Ceftazidime, key = Ceftazidime_I, value = n)
    #antibiotic
    df_GEARS_2_Ceftazidime$Antibiotic <- 'Ceftazidime'
    #total isolates
    df_GEARS_2_Ceftazidime$Total <- rowSums(df_GEARS_2_Ceftazidime[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_GEARS_2_Ceftazidime <- df_GEARS_2_Ceftazidime[,c("Country", "Year", "Organism", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_GEARS_2_Ceftazidime) <- final_column_names
    
  ## Meropenem
    
    #count isolates by country, year, species
    df_GEARS_2_Meropenem <- df_GEARS_2 %>%
      group_by(Country, Year, Organism, .drop = FALSE ) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Meropenem_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Meropenem_I) %>%
      filter(Meropenem_I == 'Resistant' | Meropenem_I == 'NonResistant')
    #spread for antibiotic
    df_GEARS_2_Meropenem <- spread(df_GEARS_2_Meropenem, key = Meropenem_I, value = n)
    #antibiotic
    df_GEARS_2_Meropenem$Antibiotic <- 'Meropenem'
    #total isolates
    df_GEARS_2_Meropenem$Total <- rowSums(df_GEARS_2_Meropenem[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_GEARS_2_Meropenem <- df_GEARS_2_Meropenem[,c("Country", "Year", "Organism", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_GEARS_2_Meropenem) <- final_column_names
    
    ## Merge molecules
    df_GEARS_2_final <- rbind(df_GEARS_2_Ceftazidime, df_GEARS_2_Meropenem)
    
    ## Make NA into 0s
    df_GEARS_2_final$Resistant <- ifelse(is.na(df_GEARS_2_final$Resistant),
                                         0,
                                         df_GEARS_2_final$Resistant) #OK ????
    
    ## Add dataset name
    df_GEARS_2_final$Data <- 'GEARS'
    
#####
    
#### KEYSTONE ####
    
  ## Ceftazidime
    
    #count isolates by country, year, species
    df_KEYSTONE_2_Ceftazidime <- df_KEYSTONE_2 %>%
      group_by(Country, `Study Year`, Organism, .drop = FALSE) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Ceftazidime_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Ceftazidime_I) %>%
      filter(Ceftazidime_I == 'Resistant' | Ceftazidime_I == 'NonResistant')
    #spread for antibiotic
    df_KEYSTONE_2_Ceftazidime <- spread(df_KEYSTONE_2_Ceftazidime, key = Ceftazidime_I, value = n)
    #antibiotic
    df_KEYSTONE_2_Ceftazidime$Antibiotic <- 'Ceftazidime'
    #total isolates
    df_KEYSTONE_2_Ceftazidime$Total <- rowSums(df_KEYSTONE_2_Ceftazidime[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_KEYSTONE_2_Ceftazidime <- df_KEYSTONE_2_Ceftazidime[,c("Country", "Study Year", "Organism", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_KEYSTONE_2_Ceftazidime) <- final_column_names
    
  ## Ceftriaxone
    
    #count isolates by country, year, species
    df_KEYSTONE_2_Ceftriaxone <- df_KEYSTONE_2 %>%
      group_by(Country, `Study Year`, Organism, .drop = FALSE) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Ceftriaxone_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Ceftriaxone_I) %>%
      filter(Ceftriaxone_I == 'Resistant' | Ceftriaxone_I == 'NonResistant')
    #spread for antibiotic
    df_KEYSTONE_2_Ceftriaxone <- spread(df_KEYSTONE_2_Ceftriaxone, key = Ceftriaxone_I, value = n)
    #antibiotic
    df_KEYSTONE_2_Ceftriaxone$Antibiotic <- 'Ceftriaxone'
    #total isolates
    df_KEYSTONE_2_Ceftriaxone$Total <- rowSums(df_KEYSTONE_2_Ceftriaxone[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_KEYSTONE_2_Ceftriaxone <- df_KEYSTONE_2_Ceftriaxone[,c("Country", "Study Year", "Organism", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_KEYSTONE_2_Ceftriaxone) <- final_column_names
    
  ## Imipenem
    
    #count isolates by country, year, species
    df_KEYSTONE_2_Imipenem <- df_KEYSTONE_2 %>%
      group_by(Country, `Study Year`, Organism, .drop = FALSE) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Imipenem_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Imipenem_I) %>%
      filter(Imipenem_I == 'Resistant' | Imipenem_I == 'NonResistant')
    #spread for antibiotic
    df_KEYSTONE_2_Imipenem <- spread(df_KEYSTONE_2_Imipenem, key = Imipenem_I, value = n)
    #antibiotic
    df_KEYSTONE_2_Imipenem$Antibiotic <- 'Imipenem'
    #total isolates
    df_KEYSTONE_2_Imipenem$Total <- rowSums(df_KEYSTONE_2_Imipenem[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_KEYSTONE_2_Imipenem <- df_KEYSTONE_2_Imipenem[,c("Country", "Study Year", "Organism", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_KEYSTONE_2_Imipenem) <- final_column_names
    
    ## Merge molecules
    df_KEYSTONE_2_final <- rbind(df_KEYSTONE_2_Ceftazidime, df_KEYSTONE_2_Ceftriaxone, df_KEYSTONE_2_Imipenem)
    
    ## Make NA into 0s
    df_KEYSTONE_2_final$Resistant <- ifelse(is.na(df_KEYSTONE_2_final$Resistant),
                                           0,
                                           df_KEYSTONE_2_final$Resistant) #OK ????
    
    ## Add dataset name
    df_KEYSTONE_2_final$Data <- 'KEYSTONE'
    
#####
 
#### SIDERO ####
    
  ## Ceftazidime
    
    #count isolates by country, year, species
    df_SIDERO_2_Ceftazidime <- df_SIDERO_2 %>%
      group_by(Country, `Year Collected`, `Organism Name`, .drop = FALSE) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Ceftazidime_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Ceftazidime_I) %>%
      filter(Ceftazidime_I == 'Resistant' | Ceftazidime_I == 'NonResistant')
    #spread for antibiotic
    df_SIDERO_2_Ceftazidime <- spread(df_SIDERO_2_Ceftazidime, key = Ceftazidime_I, value = n)
    #antibiotic
    df_SIDERO_2_Ceftazidime$Antibiotic <- 'Ceftazidime'
    #total isolates
    df_SIDERO_2_Ceftazidime$Total <- rowSums(df_SIDERO_2_Ceftazidime[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_SIDERO_2_Ceftazidime <- df_SIDERO_2_Ceftazidime[,c("Country", "Year Collected", "Organism Name", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_SIDERO_2_Ceftazidime) <- final_column_names
    
  ## Imipenem
    
    #count isolates by country, year, species
    df_SIDERO_2_Imipenem <- df_SIDERO_2 %>%
      group_by(Country, `Year Collected`, `Organism Name`, .drop = FALSE) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Imipenem_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Imipenem_I) %>%
      filter(Imipenem_I == 'Resistant' | Imipenem_I == 'NonResistant')
    #spread for antibiotic
    df_SIDERO_2_Imipenem <- spread(df_SIDERO_2_Imipenem, key = Imipenem_I, value = n)
    #antibiotic
    df_SIDERO_2_Imipenem$Antibiotic <- 'Imipenem'
    #total isolates
    df_SIDERO_2_Imipenem$Total <- rowSums(df_SIDERO_2_Imipenem[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_SIDERO_2_Imipenem <- df_SIDERO_2_Imipenem[,c("Country", "Year Collected", "Organism Name", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_SIDERO_2_Imipenem) <- final_column_names
    
  ## Meropenem
    
    #count isolates by country, year, species
    df_SIDERO_2_Meropenem <- df_SIDERO_2 %>%
      group_by(Country, `Year Collected`, `Organism Name`, .drop = FALSE) %>% #.drop = FALSE to include 0 counts in the table
      filter(!is.na(Meropenem_I)) %>% #get rid of isolates not tested against the molecule
      dplyr::count(Meropenem_I) %>%
      filter(Meropenem_I == 'Resistant' | Meropenem_I == 'NonResistant')
    #spread for antibiotic
    df_SIDERO_2_Meropenem <- spread(df_SIDERO_2_Meropenem, key = Meropenem_I, value = n)
    #antibiotic
    df_SIDERO_2_Meropenem$Antibiotic <- 'Meropenem'
    #total isolates
    df_SIDERO_2_Meropenem$Total <- rowSums(df_SIDERO_2_Meropenem[, c("NonResistant", "Resistant")], na.rm = T)
    #get in order
    df_SIDERO_2_Meropenem <- df_SIDERO_2_Meropenem[,c("Country", "Year Collected", "Organism Name", "Antibiotic", "Total", "Resistant")]
    #name
    colnames(df_SIDERO_2_Meropenem) <- final_column_names
    
    ## Merge molecules
    df_SIDERO_2_final <- rbind(df_SIDERO_2_Ceftazidime, df_SIDERO_2_Imipenem, df_SIDERO_2_Meropenem)
    
    ## Make NA into 0s
    df_SIDERO_2_final$Resistant <- ifelse(is.na(df_SIDERO_2_final$Resistant),
                                          0,
                                          df_SIDERO_2_final$Resistant) #OK ????
    
    ## Add dataset name
    df_SIDERO_2_final$Data <- 'SIDERO'
    
#####  
     
#### GLASS ####
    
  df_GLASS_2_final <- df_GLASS_2
  #name
  colnames(df_GLASS_2_final) <- final_column_names
  
  ## Add dataset name
  df_GLASS_2_final$Data <- 'GLASS'
  
#####

################################################################################
############################# Join AMR datasets ################################
################################################################################

## Join all datasets
  
  df_AMR <- rbind(df_GLASS_2_final,
                  df_ATLAS_2_final,
                  df_GEARS_2_final,
                  df_KEYSTONE_2_final,
                  df_SIDERO_2_final)
  
## Change countries names so they correspond between datasets
  unique(df_AMR$Country)
  length(unique(df_AMR$Country))
  
  df_AMR$Country <- str_replace_all(df_AMR$Country,
                                    c("United States" = "USA",
                                      "United Kingdom of Great Britain and Northern Ireland" = "UK",
                                      "United Kingdom" = "UK",
                                      "Korea, South" = "South Korea",
                                      "Republic of Korea" = "South Korea",
                                      "Russian Federation" = "Russia",
                                      "Lao People's Democratic Republic" = "Laos",
                                      "Slovak Republic" = "Slovakia"))
  
  length(unique(df_AMR$Country))
  unique(df_AMR$Country)
  
## Save it into csv 
  
  write.csv(x = df_AMR,
            file = here("data", "final_AMR_dataset.csv"),
            row.names = F)








