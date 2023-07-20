
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
library(AMR)
library(reshape2)

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
    bacteria_of_interest = as.mo(c("E. coli", "K. pneumoniae"))
  
  #Antibiotics tested
    #TO DO: !! WE HAVE TO CHOOSE WHICH MOLECULES !!
    #TO DO: !! WE HAVE TO CHOOSE ALGORITHM TO CHOOSE RESISTANCE IN ONE CLASS !!
    #ESBL (3GC) and carbapenems (CBP)
    molecules_ESBL = as.ab(c('Ceftazidime', 'Ceftriaxone'))
    molecules_CBP = as.ab(c('Imipenem', 'Meropenem'))
  
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
  df_ATLAS_2 = df_ATLAS[,-c(104:126)]
  df_ATLAS_2 <- df_ATLAS_2[,c(grep(pattern = "_I", colnames(df_ATLAS_2), invert=T))]
  df_ATLAS_2 <- df_ATLAS_2[,c(5,12,3,14:ncol(df_ATLAS_2))]
  colnames(df_ATLAS_2)[-c(1:3)] = as.ab(colnames(df_ATLAS_2)[-c(1:3)])
  df_ATLAS_2$Species = as.mo(df_ATLAS_2$Species)
  
  #get antibiotics
  df_ATLAS_2 <- df_ATLAS_2[,c(colnames(df_ATLAS_2)[1:3], molecules_CBP, molecules_ESBL)]
  #get bacteria
  unique(df_ATLAS_2$Species)
  df_ATLAS_2 <- df_ATLAS_2 %>% filter(Species %in% bacteria_of_interest)
  #get years
  df_ATLAS_2 <- df_ATLAS_2 %>% filter(Year %in% years_of_interest)
  
  #format resistances
  df_ATLAS_2 = df_ATLAS_2 %>%
    mutate_at(all_of(c(molecules_CBP,molecules_ESBL)), as.mic)
  df_ATLAS_2 = df_ATLAS_2 %>%
    mutate_if(is.mic, as.sir, mo = .$Species)
  
  #INFOS: MIC and resistance status
  #no data for ceftriaxone for 2018-2019
#####
  
#### GEARS ####
  colnames(df_GEARS)
  df_GEARS_2 <- df_GEARS[,c(6,2,3,11:ncol(df_GEARS))]
  colnames(df_GEARS_2)[colnames(df_GEARS_2)=="C_MIC"] = "CZT_MIC"
  colnames(df_GEARS_2)[-c(1:3)] = as.ab(colnames(df_GEARS_2)[-c(1:3)])
  df_GEARS_2$Organism = as.mo(df_GEARS_2$Organism)
  
  #get antibiotics
  df_GEARS_2 <- df_GEARS_2[,c(1:3, which(colnames(df_GEARS_2) %in% c(molecules_CBP, molecules_ESBL)))]
  #get bacteria
  unique(df_GEARS_2$Organism)
  df_GEARS_2 <- df_GEARS_2 %>% filter(Organism %in% bacteria_of_interest)
  #get years
  df_GEARS_2 <- df_GEARS_2 %>% filter(Year %in% years_of_interest)
  
  #format resistances
  df_GEARS_2 = df_GEARS_2 %>%
    mutate_at(vars(-Country, -Year, -Organism), as.numeric) %>%
    mutate_at(vars(-Country, -Year, -Organism), round, digits = 3) %>%
    mutate_at(vars(-Country, -Year, -Organism), as.mic)
  df_GEARS_2 = df_GEARS_2 %>%
    mutate_if(is.mic, as.sir, mo = .$Organism)
  
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
  df_KEYSTONE_2 <- df_KEYSTONE[,c(34, 2, 3, c(4:32))]
  colnames(df_KEYSTONE_2)[-c(1:3)] = as.ab(colnames(df_KEYSTONE_2)[-c(1:3)])
  df_KEYSTONE_2$Organism = as.mo(df_KEYSTONE_2$Organism)
  
  #get antibiotics
  df_KEYSTONE_2 <- df_KEYSTONE_2[,c(1:3, which(colnames(df_KEYSTONE_2) %in% c(molecules_CBP, molecules_ESBL)))]
  #get bacteria
  unique(df_KEYSTONE_2$Organism)
  df_KEYSTONE_2 <- df_KEYSTONE_2 %>% filter(Organism %in% bacteria_of_interest)
  #get years
  df_KEYSTONE_2 <- df_KEYSTONE_2 %>% filter(`Study Year` %in% years_of_interest)
  
  #format resistances
  df_KEYSTONE_2 = df_KEYSTONE_2 %>%
    mutate_if(is.logical, as.numeric)
  
  df_KEYSTONE_2 = df_KEYSTONE_2 %>%
    mutate_at(colnames(df_KEYSTONE_2)[
      (sapply(colnames(df_KEYSTONE_2), nchar) == 3) &
        (sapply(sapply(df_KEYSTONE_2, class),"[[",1) == "character")], as.mic)
  
  df_KEYSTONE_2 = df_KEYSTONE_2 %>%
    mutate_at(colnames(df_KEYSTONE_2)[
      (sapply(colnames(df_KEYSTONE_2), nchar) == 3) &
        (sapply(sapply(df_KEYSTONE_2, class),"[[",1) != "mic")], as.character)
  
  df_KEYSTONE_2 = df_KEYSTONE_2 %>%
    mutate_at(colnames(df_KEYSTONE_2)[
      (sapply(colnames(df_KEYSTONE_2), nchar) == 3) &
        (sapply(sapply(df_KEYSTONE_2, class),"[[",1) == "character")], ~replace(., . == "0", "S"))
  
  df_KEYSTONE_2 = df_KEYSTONE_2 %>%
    mutate_at(colnames(df_KEYSTONE_2)[
      (sapply(colnames(df_KEYSTONE_2), nchar) == 3) &
        (sapply(sapply(df_KEYSTONE_2, class),"[[",1) == "character")], ~replace(., . == "1", "R"))
  
  df_KEYSTONE_2 = df_KEYSTONE_2 %>%
      mutate_at(colnames(df_KEYSTONE_2)[
        (sapply(colnames(df_KEYSTONE_2), nchar) == 3) &
          (sapply(sapply(df_KEYSTONE_2, class),"[[",1) != "mic")], as.sir)
      
  df_KEYSTONE_2 = df_KEYSTONE_2 %>%
    mutate_if(is.mic, as.sir, mo = .$Organism)
  
  #INFOS: MIC data
  #no data for meropenem
  #TO DO: convert to resistance status
  #FALSE or TRUE ???
#####

#### SIDERO ####
  colnames(df_SIDERO)
  df_SIDERO_2 <- df_SIDERO[,c(3,5,1,7:20)]
  df_SIDERO_2 = df_SIDERO_2 %>% select(-"Meropenem/ Vaborbactam at 8")
  colnames(df_SIDERO_2) = sapply(colnames(df_SIDERO_2), function(x) unlist(strsplit(x, "/"))[[1]])
  colnames(df_SIDERO_2)[-c(1:3)] = as.ab(colnames(df_SIDERO_2)[-c(1:3)])
  df_SIDERO_2$`Organism Name` = as.mo(df_SIDERO_2$`Organism Name`)
  
  #get antibiotics
  df_SIDERO_2 <- df_SIDERO_2[,c(1:3, which(colnames(df_SIDERO_2) %in% c(molecules_CBP, molecules_ESBL)))]
  #get bacteria
  unique(df_SIDERO_2$`Organism Name`)
  df_SIDERO_2 <- df_SIDERO_2 %>% filter(`Organism Name` %in% bacteria_of_interest)
  #get years
  df_SIDERO_2 <- df_SIDERO_2 %>% filter(`Year Collected` %in% years_of_interest)
  
  #format resistances
  df_SIDERO_2 = df_SIDERO_2 %>%
    mutate_at(vars(-Country, -`Year Collected`, -`Organism Name`), as.numeric) %>%
    mutate_at(vars(-Country, -`Year Collected`, -`Organism Name`), round, digits = 3) %>%
    mutate_at(vars(-Country, -`Year Collected`, -`Organism Name`), as.mic)
  
  df_SIDERO_2 = df_SIDERO_2 %>%
    mutate_if(is.mic, as.sir, mo = .$`Organism Name`)
  
  #Infos: MIC data
  #TO DO: convert to resistance status
  #TO DO: what are the molecules?
  #no data for ceftriaxone
  #combinations for ceftazidime and imipenem !!
#####

#### GLASS ####
  colnames(df_GLASS)
  #df_GLASS = df_GLASS %>% filter(Specimen == "BLOOD")
  df_GLASS_2 <- df_GLASS[,c("CountryTerritoryArea", "Year", "PathogenName", "AbTargets", "InterpretableAST", "Resistant")]
  
  #get antibiotics
  unique(df_GLASS_2$AbTargets)
  df_GLASS_2$AbTargets = as.ab(df_GLASS_2$AbTargets)
  df_GLASS_2 <- df_GLASS_2 %>% filter(AbTargets %in% c(molecules_ESBL, molecules_CBP))
  #get bacteria
  unique(df_GLASS_2$PathogenName)
  df_GLASS_2$PathogenName = as.mo(df_GLASS_2$PathogenName)
  df_GLASS_2 <- df_GLASS_2 %>% filter(PathogenName %in% bacteria_of_interest)
  #get years
  df_GLASS_2 <- df_GLASS_2 %>% filter(Year %in% years_of_interest)
  
  df_GLASS_2 = df_GLASS_2 %>%
    group_by(CountryTerritoryArea, Year, PathogenName, AbTargets) %>%
    summarise(InterpretableAST = sum(InterpretableAST),
              Resistant = sum(Resistant)) %>%
    ungroup()
  
  #INFOS: resistance status

################################################################################
####### Compute total and R isolates by country/year/bacteria/antibiotic #######
################################################################################
  
## Format like in GLASS
  final_column_names = c("Country", "Year", "Pathogen", "Antibiotic", "Total", "Resistant")
  
#### ATLAS ####
  
  df_ATLAS_3 = df_ATLAS_2 %>%
    mutate(Cephalosporins = "S") %>%
    mutate(Cephalosporins = replace(Cephalosporins, IPM == "R" | MEM == "R", "R")) %>%
    mutate(Cephalosporins = replace(Cephalosporins, is.na(IPM)&is.na(MEM), NA)) %>%
    mutate(Carbapenems = "S") %>%
    mutate(Carbapenems = replace(Carbapenems, CAZ == "R" | CRO == "R", "R")) %>%
    mutate(Carbapenems = replace(Carbapenems, is.na(CAZ)&is.na(CRO), NA)) %>%
    melt(id.vars = c("Country", "Year", "Species")) %>%
    mutate(value = as.sir(value)) %>%
    filter(!is.na(value)) %>%
    group_by(Country, Year, Species, variable, .drop = FALSE) %>%
    summarise(Total = n(), Resistant = count_resistant(value)) %>%
    ungroup %>%
    rename(Pathogen = Species,
           Antibiotic = variable) %>%
    select(all_of(final_column_names))
  
  ## Add dataset name
  df_ATLAS_3$Data <- 'ATLAS'

#####

##### GEARS ####
    
  df_GEARS_3 = df_GEARS_2 %>%
    mutate(Cephalosporins = "S") %>%
    mutate(Cephalosporins = replace(Cephalosporins, IPM == "R" | MEM == "R", "R")) %>%
    mutate(Cephalosporins = replace(Cephalosporins, is.na(IPM)&is.na(MEM), NA)) %>%
    mutate(Carbapenems = "S") %>%
    mutate(Carbapenems = replace(Carbapenems, CAZ == "R", "R")) %>%
    mutate(Carbapenems = replace(Carbapenems, is.na(CAZ), NA)) %>%
    melt(id.vars = c("Country", "Year", "Organism")) %>%
    mutate(value = as.sir(value)) %>%
    filter(!is.na(value)) %>%
    group_by(Country, Year, Organism, variable, .drop = FALSE) %>%
    summarise(Total = n(), Resistant = count_resistant(value)) %>%
    ungroup %>%
    rename(Pathogen = Organism,
           Antibiotic = variable) %>%
    select(all_of(final_column_names))
  
    ## Add dataset name
  df_GEARS_3$Data <- 'GEARS'
    
#####
    
#### KEYSTONE ####
    
  df_KEYSTONE_3 = df_KEYSTONE_2 %>%
    mutate(Cephalosporins = "S") %>%
    mutate(Cephalosporins = replace(Cephalosporins, IPM == "R", "R")) %>%
    mutate(Cephalosporins = replace(Cephalosporins, is.na(IPM), NA)) %>%
    mutate(Carbapenems = "S") %>%
    mutate(Carbapenems = replace(Carbapenems, CAZ == "R" | CRO == "R", "R")) %>%
    mutate(Carbapenems = replace(Carbapenems, is.na(CAZ)&is.na(CRO), NA)) %>%
    melt(id.vars = c("Country", "Study Year", "Organism")) %>%
    mutate(value = as.sir(value)) %>%
    filter(!is.na(value)) %>%
    group_by(Country, `Study Year`, Organism, variable, .drop = FALSE) %>%
    summarise(Total = n(), Resistant = count_resistant(value)) %>%
    ungroup %>%
    rename(Pathogen = Organism,
           Antibiotic = variable,
           Year = `Study Year`) %>%
    select(all_of(final_column_names))
  
    ## Add dataset name
  df_KEYSTONE_3$Data <- 'KEYSTONE'
    
#####
 
#### SIDERO ####
    
  df_SIDERO_3 = df_SIDERO_2 %>%
    mutate(Cephalosporins = "S") %>%
    mutate(Cephalosporins = replace(Cephalosporins, IPM == "R" | MEM == "R", "R")) %>%
    mutate(Cephalosporins = replace(Cephalosporins, is.na(IPM)&is.na(MEM), NA)) %>%
    mutate(Carbapenems = "S") %>%
    mutate(Carbapenems = replace(Carbapenems, CAZ == "R", "R")) %>%
    mutate(Carbapenems = replace(Carbapenems, is.na(CAZ), NA)) %>%
    melt(id.vars = c("Country", "Year Collected", "Organism Name")) %>%
    mutate(value = as.sir(value)) %>%
    filter(!is.na(value)) %>%
    group_by(Country, `Year Collected`, `Organism Name`, variable, .drop = FALSE) %>%
    summarise(Total = n(), Resistant = count_resistant(value)) %>%
    ungroup %>%
    rename(Pathogen = `Organism Name`,
           Antibiotic = variable,
           Year = `Year Collected`) %>%
    select(all_of(final_column_names))
  
    ## Add dataset name
  df_SIDERO_3$Data <- 'SIDERO'
    
#####  
     
#### GLASS ####
  
  #for GLASS we take the average of the number of resistant and total isolates tested over the two molecules
  
  ##Cephalosporins
  
  molecule11 <- df_GLASS_2 %>% filter(AbTargets == as.ab('Ceftriaxone'))
  molecule12 <- df_GLASS_2 %>% filter(AbTargets == as.ab('Ceftazidime'))
  df_GLASS_2_ceph <- merge(molecule11, molecule12, by = c("CountryTerritoryArea", "Year", "PathogenName"), all = T)
  
  #new columns
  df_GLASS_2_ceph$AbTargets <- "Cephalosporins"
  df_GLASS_2_ceph$InterpretableAST <- rowMeans(df_GLASS_2_ceph[,c("InterpretableAST.x", "InterpretableAST.y")], na.rm = T)
  df_GLASS_2_ceph$Resistant <- rowMeans(df_GLASS_2_ceph[,c("Resistant.x", "Resistant.y")], na.rm = T)
  
  #remove old molecules
  df_GLASS_2_ceph <- df_GLASS_2_ceph %>% select(!(c("AbTargets.x", "InterpretableAST.x", "Resistant.x", "AbTargets.y", "InterpretableAST.y","Resistant.y")))
  
  ##Carbapenems
  
  molecule21 <- df_GLASS_2 %>% filter(AbTargets == as.ab('Imipenem'))
  molecule22 <- df_GLASS_2 %>% filter(AbTargets == as.ab('Meropenem'))
  df_GLASS_2_carb <- merge(molecule21, molecule22, by = c("CountryTerritoryArea", "Year", "PathogenName"), all = T)
  
  #new columns
  df_GLASS_2_carb$AbTargets <- "Carbapenems"
  df_GLASS_2_carb$InterpretableAST <- rowMeans(df_GLASS_2_carb[,c("InterpretableAST.x", "InterpretableAST.y")], na.rm = T)
  df_GLASS_2_carb$Resistant <- rowMeans(df_GLASS_2_carb[,c("Resistant.x", "Resistant.y")], na.rm = T)
  
  #remove old molecules
  df_GLASS_2_carb <- df_GLASS_2_carb %>% select(!(c("AbTargets.x", "InterpretableAST.x", "Resistant.x", "AbTargets.y", "InterpretableAST.y","Resistant.y")))
  
  ## Merge two classes
  df_GLASS_3 <- rbind(df_GLASS_2_ceph, df_GLASS_2_carb, df_GLASS_2)
  
  #name
  colnames(df_GLASS_3) <- final_column_names
  
  ## Add dataset name
  df_GLASS_3$Data <- 'GLASS'
  
#####

################################################################################
############################# Join AMR datasets ################################
################################################################################

## Join all datasets
  
  df_AMR <- rbind(df_GLASS_3,
                  df_ATLAS_3,
                  df_GEARS_3,
                  df_KEYSTONE_3,
                  df_SIDERO_3)
  
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
  
  ## Restore nicer bacteria and antibiotic names
  df_AMR$Antibiotic[nchar(df_AMR$Antibiotic) == 3] = ab_name(as.ab(df_AMR$Antibiotic[nchar(df_AMR$Antibiotic) == 3]))
  df_AMR$Pathogen = mo_name(as.mo(df_AMR$Pathogen))
  
## Save it into csv 
  
  write.csv(x = df_AMR,
            file = here("data", "final_AMR_dataset.csv"),
            row.names = F)








