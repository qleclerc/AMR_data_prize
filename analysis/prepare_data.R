
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
df_SOAR <- read.csv(here("data", "raw", "gsk_201818_published.csv"))
df_KEYSTONE <- read_excel(here("data", "raw", "Omadacycline_2014_to_2022_Surveillance_data.xlsx"))

#public
df_GLASS <- read.csv("https://raw.githubusercontent.com/qleclerc/GLASS2022/master/compiled_WHO_GLASS_2022.csv") #Quentin Git

################################################################################
############### Choose variable of interests across datasets ###################
################################################################################

#Countries

#Years
years_of_interest = c(2018:2019)

#Bacterial species 
#E. coli & K. pneumoniae --> not in DREAM and SOARE
bacteria_of_interest = as.mo(c("E. coli", "K. pneumoniae", "A. baumannii"))

#Antibiotics tested
#ESBL (3GC) and carbapenems (CBP)
antibiotics_of_interest = as.ab(c("Ceftazidime", "Ceftriaxone",
                                  "Imipenem", "Meropenem"))

#Resistance status to antibiotics
#number of R isolates
#number of total isolates
#TO DO: 3GC as proxy for ESBL?


################################################################################
############### Get same variables for all AMR datasets ########################
################################################################################

#### ATLAS ####
df_ATLAS_2 = df_ATLAS[,-c(104:126)]
df_ATLAS_2 <- df_ATLAS_2[,c(grep(pattern = "_I", colnames(df_ATLAS_2), invert=T))]
df_ATLAS_2 <- df_ATLAS_2[,c(5,12,3,14:ncol(df_ATLAS_2))]
colnames(df_ATLAS_2)[-c(1:3)] = as.ab(colnames(df_ATLAS_2)[-c(1:3)])
df_ATLAS_2$Species = as.mo(df_ATLAS_2$Species)

#get antibiotics
df_ATLAS_2 <- df_ATLAS_2[,c(colnames(df_ATLAS_2)[1:3], antibiotics_of_interest)]
#get bacteria
df_ATLAS_2 <- df_ATLAS_2 %>% filter(Species %in% bacteria_of_interest)
#get years
df_ATLAS_2 <- df_ATLAS_2 %>% filter(Year %in% years_of_interest)

if(nrow(df_ATLAS_2) > 0){
  #format resistances
  df_ATLAS_2 = df_ATLAS_2 %>%
    mutate_at(all_of(c(antibiotics_of_interest)), as.mic)
  df_ATLAS_2 = df_ATLAS_2 %>%
    mutate_if(is.mic, as.sir, mo = .$Species)
}
#INFOS: MIC and resistance status
#no data for ceftriaxone for 2018-2019
#####

#### GEARS ####
df_GEARS_2 <- df_GEARS[,c(6,2,3,11:ncol(df_GEARS))]
colnames(df_GEARS_2)[colnames(df_GEARS_2)=="C_MIC"] = "CZT_MIC"
colnames(df_GEARS_2)[-c(1:3)] = as.ab(colnames(df_GEARS_2)[-c(1:3)])
df_GEARS_2$Organism = as.mo(df_GEARS_2$Organism)

#get antibiotics
df_GEARS_2 <- df_GEARS_2[,c(1:3, which(colnames(df_GEARS_2) %in% antibiotics_of_interest))]
#get bacteria
df_GEARS_2 <- df_GEARS_2 %>% filter(Organism %in% bacteria_of_interest)
#get years
df_GEARS_2 <- df_GEARS_2 %>% filter(Year %in% years_of_interest)

if(nrow(df_GEARS_2) > 0){
  #format resistances
  df_GEARS_2 = df_GEARS_2 %>%
    mutate_at(vars(-Country, -Year, -Organism), as.numeric) %>%
    mutate_at(vars(-Country, -Year, -Organism), round, digits = 3) %>%
    mutate_at(vars(-Country, -Year, -Organism), as.mic)
  df_GEARS_2 = df_GEARS_2 %>%
    mutate_if(is.mic, as.sir, mo = .$Organism)
}

#INFOS: MIC data
#CAZ: ceftazidime
#no data for ceftriaxone
#IPM = imipenem? --> no data it seems for the two bugs
#MEM = meropenem?
#####

#### KEYSTONE ####
df_KEYSTONE_2 <- df_KEYSTONE[,c(34, 2, 3, c(4:32))]
colnames(df_KEYSTONE_2)[-c(1:3)] = as.ab(colnames(df_KEYSTONE_2)[-c(1:3)])
df_KEYSTONE_2$Organism = as.mo(df_KEYSTONE_2$Organism)

#get antibiotics
df_KEYSTONE_2 <- df_KEYSTONE_2[,c(1:3, which(colnames(df_KEYSTONE_2) %in% antibiotics_of_interest))]
#get bacteria
df_KEYSTONE_2 <- df_KEYSTONE_2 %>% filter(Organism %in% bacteria_of_interest)
#get years
df_KEYSTONE_2 <- df_KEYSTONE_2 %>% filter(`Study Year` %in% years_of_interest)

if(nrow(df_KEYSTONE_2) > 0){
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
}
#INFOS: MIC data
#no data for meropenem
#####

#### SIDERO ####
df_SIDERO_2 <- df_SIDERO[,c(3,5,1,7:20)]
df_SIDERO_2 = df_SIDERO_2 %>% select(-"Meropenem/ Vaborbactam at 8")
colnames(df_SIDERO_2) = sapply(colnames(df_SIDERO_2), function(x) unlist(strsplit(x, "/"))[[1]])
colnames(df_SIDERO_2)[-c(1:3)] = as.ab(colnames(df_SIDERO_2)[-c(1:3)])
df_SIDERO_2$`Organism Name` = as.mo(df_SIDERO_2$`Organism Name`)

#get antibiotics
df_SIDERO_2 <- df_SIDERO_2[,c(1:3, which(colnames(df_SIDERO_2) %in% antibiotics_of_interest))]
#get bacteria
df_SIDERO_2 <- df_SIDERO_2 %>% filter(`Organism Name` %in% bacteria_of_interest)
#get years
df_SIDERO_2 <- df_SIDERO_2 %>% filter(`Year Collected` %in% years_of_interest)

if(nrow(df_SIDERO_2) > 0){
  #format resistances
  df_SIDERO_2 = df_SIDERO_2 %>%
    mutate_at(vars(-Country, -`Year Collected`, -`Organism Name`), as.numeric) %>%
    mutate_at(vars(-Country, -`Year Collected`, -`Organism Name`), round, digits = 3) %>%
    mutate_at(vars(-Country, -`Year Collected`, -`Organism Name`), as.mic)
  
  df_SIDERO_2 = df_SIDERO_2 %>%
    mutate_if(is.mic, as.sir, mo = .$`Organism Name`)
}
#Infos: MIC data
#no data for ceftriaxone
#combinations for ceftazidime and imipenem !!
#####


#### DREAM ####
df_DREAM_2 <- df_DREAM[,c(3,1,5,7:18)]
colnames(df_DREAM_2)[colnames(df_DREAM_2) == "EMB"] = "ETH"
colnames(df_DREAM_2)[colnames(df_DREAM_2) == "RMP"] = "RIF"
colnames(df_DREAM_2)[-c(1:3)] = as.ab(colnames(df_DREAM_2)[-c(1:3)])
df_DREAM_2$Organism = as.mo(df_DREAM_2$Organism)
df_DREAM_2$BDQ[df_DREAM_2$BDQ=="1.4999999999999999E-2"] = "0.015"

#get antibiotics
df_DREAM_2 <- df_DREAM_2[,c(1:3, which(colnames(df_DREAM_2) %in% antibiotics_of_interest))]
#get bacteria
df_DREAM_2 <- df_DREAM_2 %>% filter(Organism %in% bacteria_of_interest)
#get years
df_DREAM_2 <- df_DREAM_2 %>% filter(`Year Collected` %in% years_of_interest)

if(nrow(df_DREAM_2) > 0){
  #format resistances
  df_DREAM_2 = df_DREAM_2 %>%
    mutate_at(vars(-Country, -`Year Collected`, -Organism), as.mic)
  
  df_DREAM_2 = df_DREAM_2 %>%
    mutate_if(is.mic, as.sir, mo = .$Organism)
}
#Infos: MIC data
#no MIC breakpoints for several antibiotics!
#####

#### SOAR ####
df_SOAR_2 <- df_SOAR[,c(5,9,6,11:23)]
colnames(df_SOAR_2)[-c(1:3)] = as.ab(colnames(df_SOAR_2)[-c(1:3)])
df_SOAR_2$ORGANISMNAME = as.mo(df_SOAR_2$ORGANISMNAME)

#get antibiotics
df_SOAR_2 <- df_SOAR_2[,c(1:3, which(colnames(df_SOAR_2) %in% antibiotics_of_interest))]
#get bacteria
df_SOAR_2 <- df_SOAR_2 %>% filter(ORGANISMNAME %in% bacteria_of_interest)
#get years
df_SOAR_2 <- df_SOAR_2 %>% filter(YEARCOLLECTED %in% years_of_interest)

if(nrow(df_SOAR_2) > 0){
  #format resistances
  df_SOAR_2 = df_SOAR_2 %>%
    mutate_at(vars(-COUNTRY, -YEARCOLLECTED, -ORGANISMNAME), as.mic)
  
  df_SOAR_2 = df_SOAR_2 %>%
    mutate_if(is.mic, as.sir, mo = .$ORGANISMNAME)
}
#Infos: MIC data
#####


#### GLASS ####
df_GLASS = df_GLASS %>%
  mutate(PathogenName = replace(PathogenName, PathogenName=="Salmonella spp.", "Salmonella enterica"),
         PathogenName = replace(PathogenName, PathogenName=="Acinetobacter spp.", "Acinetobacter baumannii"),
         PathogenName = replace(PathogenName, PathogenName=="Shigella spp.", "Shigella sonnei"))
#df_GLASS = df_GLASS %>% filter(Specimen == "BLOOD")
df_GLASS_2 <- df_GLASS[,c("CountryTerritoryArea", "Year", "PathogenName", "AbTargets", "InterpretableAST", "Resistant")]

#get antibiotics
df_GLASS_2$AbTargets = as.ab(df_GLASS_2$AbTargets)
df_GLASS_2 <- df_GLASS_2 %>% filter(AbTargets %in% antibiotics_of_interest)
#get bacteria
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

if(nrow(df_ATLAS_2) > 0 & !(all(is.na(df_ATLAS_2)[,-c(1:3)]))){
  df_ATLAS_3 = df_ATLAS_2 %>%
    # mutate(Cephalosporins = "S") %>%
    # mutate(Cephalosporins = replace(Cephalosporins, IPM == "R" | MEM == "R", "R")) %>%
    # mutate(Cephalosporins = replace(Cephalosporins, is.na(IPM)&is.na(MEM), NA)) %>%
    # mutate(Carbapenems = "S") %>%
    # mutate(Carbapenems = replace(Carbapenems, CAZ == "R" | CRO == "R", "R")) %>%
    # mutate(Carbapenems = replace(Carbapenems, is.na(CAZ)&is.na(CRO), NA)) %>%
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
} else df_ATLAS_3 = data.frame()

#####

##### GEARS ####

if(nrow(df_GEARS_2) > 0 & !(all(is.na(df_GEARS_2)[,-c(1:3)]))){
  df_GEARS_3 = df_GEARS_2 %>%
    # mutate(Cephalosporins = "S") %>%
    # mutate(Cephalosporins = replace(Cephalosporins, IPM == "R" | MEM == "R", "R")) %>%
    # mutate(Cephalosporins = replace(Cephalosporins, is.na(IPM)&is.na(MEM), NA)) %>%
    # mutate(Carbapenems = "S") %>%
    # mutate(Carbapenems = replace(Carbapenems, CAZ == "R", "R")) %>%
    # mutate(Carbapenems = replace(Carbapenems, is.na(CAZ), NA)) %>%
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
} else df_GEARS_3 = data.frame()

#####

#### KEYSTONE ####

if(nrow(df_KEYSTONE_2) > 0 & !(all(is.na(df_KEYSTONE_2)[,-c(1:3)]))){ 
  df_KEYSTONE_3 = df_KEYSTONE_2 %>%
    # mutate(Cephalosporins = "S") %>%
    # mutate(Cephalosporins = replace(Cephalosporins, IPM == "R", "R")) %>%
    # mutate(Cephalosporins = replace(Cephalosporins, is.na(IPM), NA)) %>%
    # mutate(Carbapenems = "S") %>%
    # mutate(Carbapenems = replace(Carbapenems, CAZ == "R" | CRO == "R", "R")) %>%
    # mutate(Carbapenems = replace(Carbapenems, is.na(CAZ)&is.na(CRO), NA)) %>%
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
} else df_KEYSTONE_3 = data.frame()

#####

#### SIDERO ####

if(nrow(df_SIDERO_2) > 0 & !(all(is.na(df_SIDERO_2)[,-c(1:3)]))){
  df_SIDERO_3 = df_SIDERO_2 %>%
    # mutate(Cephalosporins = "S") %>%
    # mutate(Cephalosporins = replace(Cephalosporins, IPM == "R" | MEM == "R", "R")) %>%
    # mutate(Cephalosporins = replace(Cephalosporins, is.na(IPM)&is.na(MEM), NA)) %>%
    # mutate(Carbapenems = "S") %>%
    # mutate(Carbapenems = replace(Carbapenems, CAZ == "R", "R")) %>%
    # mutate(Carbapenems = replace(Carbapenems, is.na(CAZ), NA)) %>%
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
} else df_SIDERO_3 = data.frame()

#####  

#### DREAM ####

if(nrow(df_DREAM_2) > 0 & !(all(is.na(df_DREAM_2)[,-c(1:3)]))){
  df_DREAM_3 = df_DREAM_2 %>%
    # mutate(Cephalosporins = "S") %>%
    # mutate(Cephalosporins = replace(Cephalosporins, IPM == "R" | MEM == "R", "R")) %>%
    # mutate(Cephalosporins = replace(Cephalosporins, is.na(IPM)&is.na(MEM), NA)) %>%
    # mutate(Carbapenems = "S") %>%
    # mutate(Carbapenems = replace(Carbapenems, CAZ == "R", "R")) %>%
    # mutate(Carbapenems = replace(Carbapenems, is.na(CAZ), NA)) %>%
    melt(id.vars = c("Country", "Year Collected", "Organism")) %>%
    mutate(value = as.sir(value)) %>%
    filter(!is.na(value)) %>%
    group_by(Country, `Year Collected`, Organism, variable, .drop = FALSE) %>%
    summarise(Total = n(), Resistant = count_resistant(value)) %>%
    ungroup %>%
    rename(Pathogen = Organism,
           Antibiotic = variable,
           Year = `Year Collected`) %>%
    select(all_of(final_column_names))
  
  ## Add dataset name
  df_DREAM_3$Data <- 'DREAM'
} else df_DREAM_3 = data.frame()

#####  

#### SOAR ####

if(nrow(df_SOAR_2) > 0 & !(all(is.na(df_SOAR_2)[,-c(1:3)]))){
  df_SOAR_3 = df_SOAR_2 %>%
    # mutate(Cephalosporins = "S") %>%
    # mutate(Cephalosporins = replace(Cephalosporins, IPM == "R" | MEM == "R", "R")) %>%
    # mutate(Cephalosporins = replace(Cephalosporins, is.na(IPM)&is.na(MEM), NA)) %>%
    # mutate(Carbapenems = "S") %>%
    # mutate(Carbapenems = replace(Carbapenems, CAZ == "R", "R")) %>%
    # mutate(Carbapenems = replace(Carbapenems, is.na(CAZ), NA)) %>%
    melt(id.vars = c("COUNTRY", "YEARCOLLECTED", "ORGANISMNAME")) %>%
    mutate(value = as.sir(value)) %>%
    filter(!is.na(value)) %>%
    group_by(COUNTRY, YEARCOLLECTED, ORGANISMNAME, variable, .drop = FALSE) %>%
    summarise(Total = n(), Resistant = count_resistant(value)) %>%
    ungroup %>%
    rename(Pathogen = ORGANISMNAME,
           Antibiotic = variable,
           Year = YEARCOLLECTED,
           Country = COUNTRY) %>%
    select(all_of(final_column_names))
  
  ## Add dataset name
  df_SOAR_3$Data <- 'DREAM'
} else df_SOAR_3 = data.frame()

#####  

#### GLASS ####

#for GLASS we take the average of the number of resistant and total isolates tested over the two molecules

# ##Cephalosporins
# 
# molecule11 <- df_GLASS_2 %>% filter(AbTargets == as.ab('Ceftriaxone'))
# molecule12 <- df_GLASS_2 %>% filter(AbTargets == as.ab('Ceftazidime'))
# df_GLASS_2_ceph <- merge(molecule11, molecule12, by = c("CountryTerritoryArea", "Year", "PathogenName"), all = T)
# 
# #new columns
# df_GLASS_2_ceph$AbTargets <- "Cephalosporins"
# df_GLASS_2_ceph$InterpretableAST <- rowMeans(df_GLASS_2_ceph[,c("InterpretableAST.x", "InterpretableAST.y")], na.rm = T)
# df_GLASS_2_ceph$Resistant <- rowMeans(df_GLASS_2_ceph[,c("Resistant.x", "Resistant.y")], na.rm = T)
# 
# #remove old molecules
# df_GLASS_2_ceph <- df_GLASS_2_ceph %>% select(!(c("AbTargets.x", "InterpretableAST.x", "Resistant.x", "AbTargets.y", "InterpretableAST.y","Resistant.y")))
# 
# ##Carbapenems
# 
# molecule21 <- df_GLASS_2 %>% filter(AbTargets == as.ab('Imipenem'))
# molecule22 <- df_GLASS_2 %>% filter(AbTargets == as.ab('Meropenem'))
# df_GLASS_2_carb <- merge(molecule21, molecule22, by = c("CountryTerritoryArea", "Year", "PathogenName"), all = T)
# 
# #new columns
# df_GLASS_2_carb$AbTargets <- "Carbapenems"
# df_GLASS_2_carb$InterpretableAST <- rowMeans(df_GLASS_2_carb[,c("InterpretableAST.x", "InterpretableAST.y")], na.rm = T)
# df_GLASS_2_carb$Resistant <- rowMeans(df_GLASS_2_carb[,c("Resistant.x", "Resistant.y")], na.rm = T)
# 
# #remove old molecules
# df_GLASS_2_carb <- df_GLASS_2_carb %>% select(!(c("AbTargets.x", "InterpretableAST.x", "Resistant.x", "AbTargets.y", "InterpretableAST.y","Resistant.y")))
# 
# ## Merge two classes
# df_GLASS_3 <- rbind(df_GLASS_2_ceph, df_GLASS_2_carb, df_GLASS_2)


if(nrow(df_GLASS_2) > 0){
  df_GLASS_3 = df_GLASS_2
  #name
  colnames(df_GLASS_3) <- final_column_names
  
  ## Add dataset name
  df_GLASS_3$Data <- 'GLASS'
} else df_GLASS_3 = data.frame()

#####

################################################################################
############################# Join AMR datasets ################################
################################################################################

## Join all datasets

df_AMR <- rbind(df_GLASS_3,
                df_ATLAS_3,
                df_GEARS_3,
                df_KEYSTONE_3,
                df_SIDERO_3,
                df_DREAM_3,
                df_SOAR_3)

if(nrow(df_AMR) == 0){
  cat("The requested year-drug-bug combination does not exist in any dataset!\n")
} else{
  
  for(dataset in c("df_GLASS_3",
                   "df_ATLAS_3",
                   "df_GEARS_3",
                   "df_KEYSTONE_3",
                   "df_SIDERO_3",
                   "df_DREAM_3",
                   "df_SOAR_3")){
    if(nrow(get(dataset)) == 0) cat("The requested year-drug-bug combination does not exist in",
                                    strsplit(dataset, "_")[[1]][2], "\n")
  }
  
  ## Change countries names so they correspond between datasets
  df_AMR$Country <- str_replace_all(df_AMR$Country,
                                    c("United States" = "USA",
                                      "United Kingdom of Great Britain and Northern Ireland" = "UK",
                                      "United Kingdom" = "UK",
                                      "Korea, South" = "South Korea",
                                      "Republic of Korea" = "South Korea",
                                      "Russian Federation" = "Russia",
                                      "Lao People's Democratic Republic" = "Laos",
                                      "Slovak Republic" = "Slovakia"))
  
  ## Restore nicer bacteria and antibiotic names
  df_AMR$Antibiotic = as.character(df_AMR$Antibiotic)
  df_AMR$Antibiotic[nchar(df_AMR$Antibiotic) == 3] = ab_name(as.ab(df_AMR$Antibiotic[nchar(df_AMR$Antibiotic) == 3]))
  df_AMR$Pathogen = mo_name(as.mo(df_AMR$Pathogen))
  
  ## Summary
  cat("Combined datasets cover", length(unique(df_AMR$Country)), "countries\n",
      "with a total of", sum(df_AMR$Total), "data points\n",
      "(one data point is one test for one year-country-bug-drug-dataset combination)\n")
  cat("Countries per datasets:")
  print(df_AMR %>% group_by(Data) %>% summarise(n_countries = length(unique(Country))))
  cat("Data points per datasets:")
  print(df_AMR %>% group_by(Data) %>% summarise(n_datapoints = sum(Total)))
  
  ## Save it into csv 
  write.csv(x = df_AMR,
            file = here("data", "final_AMR_dataset.csv"),
            row.names = F)
  
}









