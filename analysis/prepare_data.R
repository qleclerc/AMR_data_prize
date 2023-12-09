
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
library(AMR)
library(reshape2)

################################################################################
########################### Load AMR datasets ##################################
################################################################################

if(!dir.exists(here("data", "raw"))) stop("Please place the industry surveillance datasets in a new subfolder of the \"data\" folder, named \"raw\"")

#industry
df_ATLAS <- read.csv(here("data", "raw", "2023_06_15 atlas_antibiotics.csv"))
df_SIDERO <- read_excel(here("data", "raw", "Updated_Shionogi Five year SIDERO-WT Surveillance data(without strain number)_Vivli_220409.xlsx"))
df_DREAM <- read_excel(here("data", "raw", "BEDAQUILINE DREAM DATASET FOR VIVLI - 06-06-2022.xlsx"))
df_GEARS <- read_excel(here("data", "raw", "Venatorx surveillance data for Vivli 27Feb2023.xlsx"))
df_SOAR <- read.csv(here("data", "raw", "gsk_201818_published.csv"))
df_KEYSTONE <- read_excel(here("data", "raw", "Omadacycline_2014_to_2022_Surveillance_data.xlsx"))

#public
#I'm doing some GLASS reformatting here already, easier than doing it later
df_GLASS = read.csv(here::here("data", "glass_combined.csv"))
df_GLASS = df_GLASS %>%
  mutate(PathogenName = replace(PathogenName, grepl("Salmonella", PathogenName), "Salmonella enterica"),
         PathogenName = replace(PathogenName, grepl("Acinetobacter", PathogenName), "Acinetobacter baumannii"),
         PathogenName = replace(PathogenName, grepl("Shigella", PathogenName), "Shigella sonnei"))
df_GLASS = df_GLASS %>%
  mutate(AbTargets = replace(AbTargets, AbTargets %in% c("Meticillin", "Cefoxitin"), "Oxacillin")) %>%
  group_by(Iso3, CountryTerritoryArea, WHORegionName, Year, Specimen, PathogenName, AbTargets) %>%
  mutate(TotalSpecimenIsolates = sum(TotalSpecimenIsolates),
         InterpretableAST = sum(InterpretableAST),
         Resistant = sum(Resistant)) %>%
  mutate(PercentResistant = Resistant/InterpretableAST*100) %>%
  ungroup


################################################################################
############### Choose variable of interests across datasets ###################
################################################################################

#Countries

#Years
# years_of_interest = c(2018:2019)
years_of_interest = c(2017:2020)

#Bacterial species 
#E. coli & K. pneumoniae --> not in DREAM and SOARE
# bacteria_of_interest = as.mo(c("E. coli", "K pneumoniae"))
# bacteria_of_interest = as.mo(c("A. baumannii"))
bacteria_of_interest = as.mo(unique(df_GLASS$PathogenName))

#Antibiotics tested
#ESBL (3GC) and carbapenems (CBP)
# antibiotics_of_interest = as.ab(c("Ceftazidime", "Ceftriaxone", "Imipenem", "Meropenem"))
# antibiotics_of_interest = as.ab(c("Colistin", "Gentamicin", "Imipenem", "Meropenem"))
antibiotics_of_interest = as.ab(unique(df_GLASS$AbTargets))

#Sample source
sample_of_interest = c()

# HERE is my attempt to start harmonising sample sources
# As you can quickly see, it's very chaotic and requires manual tweaking
# I've tried to comment what I tried to do, but might be difficult to read sorry!

# basically, I'm trying to generate an overall "sample_mapping" dataset which I
# can then use to remap sample source across all datasets to a homogeneous format
# eg if in dataset 1 it's "BLOOD" and in dataset 2 it's "BloOD", and I want the
# mapping to be "Blood", the "sample_mapping" dataset should have one line saying
# "BLOOD" should be "Blood", and one line saying "BloOD" should be "Blood"

# SIDERO is the most complete, so I'm starting from that to create the sample_mapping
# in "sample_mapping", the first col is the target format we want, second col
# is the short label that can be found in datasets, third col is the long label
# why both short and long labels you ask? Because it's chaotic lol
# temp is just an intermediary dataset for manual tweaking
temp = sort(unique(df_SIDERO$`Body Location`))
temp[temp=="Bodily Fluids"] = "Bodily Fluids: Bodily Fluids"
temp[temp=="Unknown"] = "Unknown: Unknown"
temp[temp=="Respiratory"] = "Respiratory: Respiratory"
temp[temp=="CardioVascular (CVS)"] = "CVS: CardioVascular (CVS)"
temp[temp=="GastroIntestinal (GI)"] = "GI: GastroIntestinal (GI)"

sample_mapping = data.frame(target = unname(sapply(temp, FUN = function(x) unlist(strsplit(x, ":"))[[1]])),
                            short = unname(sapply(temp, FUN = function(x) unlist(strsplit(x, ": "))[[2]])),
                            full = temp)
sample_mapping$full[sample_mapping$full == "Unknown: Unknown"] = "Unknown"

sample_mapping$target[sample_mapping$target == "RTI"] = "Respiratory"
sample_mapping$target[sample_mapping$target == "SSI"] = "INT"
sample_mapping$target[sample_mapping$target == "UTI"] = "GU"
sample_mapping$target[sample_mapping$target == "PD"] = "Bodily Fluids"

# adding in mapping for GEARS not already included
temp = sort(unique(df_GEARS$BodySite)[!(unique(df_GEARS$BodySite) %in% unique(sample_mapping$full))])

sample_gears = data.frame(target = unname(sapply(temp, FUN = function(x) unlist(strsplit(x, ":"))[[1]])),
                          short = unname(sapply(temp, FUN = function(x) unlist(strsplit(x, ": "))[[2]])),
                          full = temp)

sample_mapping = rbind(sample_mapping, sample_gears)

# adding in mapping for SOAR not already included
temp = sort(unique(df_SOAR$BODYLOCATION)[!(unique(df_SOAR$BODYLOCATION) %in% unique(sample_mapping$full))])

sample_soar = data.frame(target = unname(sapply(temp, FUN = function(x) unlist(strsplit(x, ":"))[[1]])),
                         short = unname(sapply(temp, FUN = function(x) unlist(strsplit(x, ": "))[[2]])),
                         full = temp)

sample_mapping = rbind(sample_mapping, sample_soar)

# for KEYSTONE... it's problematic
# basically the rule is to first use infection type, but if it says "other", then
# you have to look at specimen type
# so for KEYSTONE it's actually easier to have a separate mapping dataframe, not merge into sample_mapping
# but I haven't finished that yet
sample_keystone = data.frame(target = c("Respiratory", "CVS", "INT", "GU", "IAI", "Respiratory"),
                             short = unique(df_KEYSTONE$`Infection Type`)[-7],
                             full = "")
sample_keystone = rbind(sample_keystone,
                        data.frame(target = c("Bodily Fluids",
                                              "Bodily Fluids",
                                              "INT",
                                              "CVS",
                                              "CVS",
                                              "HEENT",
                                              "HEENT",
                                              "GU",
                                              "CVS",
                                              "Bodily Fluids",
                                              "Unknown",
                                              "Bodily Fluids",
                                              "Bodily Fluids",
                                              "GI",
                                              "Respiratory",
                                              "Respiratory",
                                              "INT",
                                              "Respiratory",
                                              "GU",
                                              "Bodily Fluids"),
                                   short = "",
                                   full = sort(unique(df_KEYSTONE$`Specimen Type`[df_KEYSTONE$`Infection Type` == "other sites"]))))

# aaaaand I haven't had the time to do the remapping for other datasets
# then, the final step missing is of course to actually apply the mapping to change the datasets



################################################################################
############### Get same variables for all AMR datasets ########################
################################################################################

#### ATLAS ####
df_ATLAS_2 = df_ATLAS[,-c(104:126)]
df_ATLAS_2 <- df_ATLAS_2[,c(grep(pattern = "_I", colnames(df_ATLAS_2), invert=T))]
# MRSA harmonisation - MRSA status indicated by "MRSA" label in Phenotype, not necessarily oxa MIC
# so, set all MRSA phenotypes to have oxa MIC of 8
df_ATLAS_2$Oxacillin[df_ATLAS_2$Phenotype == "MSSA"] = 0.5
df_ATLAS_2$Oxacillin[df_ATLAS_2$Phenotype == "MRSA"] = 8
# ESBL harmonisation - ESBL status indicated by "ESBL" label in Phenotype, not necessarily MIC
# so, set all ESBL phenotypes to have ampicillin MIC of 32
df_ATLAS_2$Ampicillin[df_ATLAS_2$Phenotype == "ESBL"] = 32
df_ATLAS_2 <- df_ATLAS_2[,c(5,12,3,14:ncol(df_ATLAS_2))]
colnames(df_ATLAS_2)[-c(1:3)] = as.ab(colnames(df_ATLAS_2)[-c(1:3)])
df_ATLAS_2$Species = as.mo(df_ATLAS_2$Species)

#get antibiotics
if(!(all(is.na(antibiotics_of_interest)))) df_ATLAS_2 <- df_ATLAS_2[,c(1:3, which(colnames(df_ATLAS_2) %in% antibiotics_of_interest))]
#get bacteria
if(!(all(is.na(bacteria_of_interest)))) df_ATLAS_2 <- df_ATLAS_2 %>% filter(Species %in% bacteria_of_interest)
#get years
if(length(years_of_interest) != 0) df_ATLAS_2 <- df_ATLAS_2 %>% filter(Year %in% years_of_interest)
#get sample source
if(length(sample_of_interest) != 0) df_ATLAS_2 <- df_ATLAS_2 %>% filter(Source %in% sample_of_interest)

if(nrow(df_ATLAS_2) > 0){
  #format resistances
  df_ATLAS_2 = df_ATLAS_2 %>%
    mutate_at(vars(-Country, -Year, -Species), as.mic)
  df_ATLAS_2 = df_ATLAS_2[,which(apply(df_ATLAS_2, 2, function(x) !(all(is.na(x)))))]
  
  df_ATLAS_2b = df_ATLAS_2 %>%
    mutate_if(is.mic, as.sir, mo = .$Species, guideline = "CLSI")
  
  # NEW: I'm now applying CLSI breakpoints to all datasets in a first instance
  # however, sometimes there are no CLSI breakpoints, so for those I try again with
  # EUCAST breakpoints. This process is the same for all datasets.
  if(any(apply(df_ATLAS_2b, 2, function(x) all(is.na(x))))){
    cat("\nRetrying some columns with EUCAST guidelines...\n")
    to_retry = which(apply(df_ATLAS_2b, 2, function(x) all(is.na(x))))
    df_ATLAS_2b[,to_retry] = df_ATLAS_2 %>%
      select(all_of(c(3,to_retry))) %>%
      mutate_if(is.mic, as.sir, mo = .$Species, guideline = "EUCAST") %>%
      select(-1)
  }
  df_ATLAS_2 = df_ATLAS_2b
  rm(df_ATLAS_2b)
}

#####

#### GEARS ####
df_GEARS_2 <- df_GEARS[,c(6,2,3,11:ncol(df_GEARS))]
colnames(df_GEARS_2)[colnames(df_GEARS_2)=="C_MIC"] = "CZT_MIC"
colnames(df_GEARS_2)[-c(1:3)] = as.ab(colnames(df_GEARS_2)[-c(1:3)])
df_GEARS_2$Organism = as.mo(df_GEARS_2$Organism)
# no MRSA here so no harmonisation needed

#get antibiotics
if(!(all(is.na(antibiotics_of_interest)))) df_GEARS_2 <- df_GEARS_2[,c(1:3, which(colnames(df_GEARS_2) %in% antibiotics_of_interest))]
#get bacteria
if(!(all(is.na(bacteria_of_interest)))) df_GEARS_2 <- df_GEARS_2 %>% filter(Organism %in% bacteria_of_interest)
#get years
if(length(years_of_interest) != 0) df_GEARS_2 <- df_GEARS_2 %>% filter(Year %in% years_of_interest)
#get sample source
if(length(sample_of_interest) != 0) df_GEARS_2 <- df_GEARS_2 %>% filter(BodySite %in% sample_of_interest)


if(nrow(df_GEARS_2) > 0){
  #format resistances
  df_GEARS_2 = df_GEARS_2 %>%
    mutate_at(vars(-Country, -Year, -Organism), as.numeric) %>%
    mutate_at(vars(-Country, -Year, -Organism), round, digits = 3) %>%
    mutate_at(vars(-Country, -Year, -Organism), as.mic)
  df_GEARS_2 = df_GEARS_2[,which(apply(df_GEARS_2, 2, function(x) !(all(is.na(x)))))]
  
  df_GEARS_2b = df_GEARS_2 %>%
    mutate_if(is.mic, as.sir, mo = .$Organism, guideline = "CLSI")
  
  if(any(apply(df_GEARS_2b, 2, function(x) all(is.na(x))))){
    cat("\nRetrying some columns with EUCAST guidelines...\n")
    to_retry = which(apply(df_GEARS_2b, 2, function(x) all(is.na(x))))
    df_GEARS_2b[,to_retry] = df_GEARS_2 %>%
      select(all_of(c(3,to_retry))) %>%
      mutate_if(is.mic, as.sir, mo = .$Organism, guideline = "EUCAST") %>%
      select(-1)
  }
  df_GEARS_2 = df_GEARS_2b
  rm(df_GEARS_2b)
  
}

#####

#### KEYSTONE ####
df_KEYSTONE_2 <- df_KEYSTONE[,c(34, 2, 3, c(4:32))]
colnames(df_KEYSTONE_2)[-c(1:3)] = as.ab(colnames(df_KEYSTONE_2)[-c(1:3)])
# MRSA harmonisation - here, only oxacillin is used so okay
df_KEYSTONE_2$Organism[grepl("Acinetobacter baumannii", df_KEYSTONE_2$Organism)] = "Acinetobacter baumannii"
df_KEYSTONE_2$Organism = as.mo(df_KEYSTONE_2$Organism)

#get antibiotics
if(!(all(is.na(antibiotics_of_interest)))) df_KEYSTONE_2 <- df_KEYSTONE_2[,c(1:3, which(colnames(df_KEYSTONE_2) %in% antibiotics_of_interest))]
#get bacteria
if(!(all(is.na(bacteria_of_interest)))) df_KEYSTONE_2 <- df_KEYSTONE_2 %>% filter(Organism %in% bacteria_of_interest)
#get years
if(length(years_of_interest) != 0) df_KEYSTONE_2 <- df_KEYSTONE_2 %>% filter(`Study Year` %in% years_of_interest)
#get sample source
if(length(sample_of_interest) != 0) df_KEYSTONE_2 <- df_KEYSTONE_2 %>% filter(`Specimen Type` %in% sample_of_interest)

if(nrow(df_KEYSTONE_2) > 0){
  #format resistances
  # WARNING: several columns use TRUE/FALSE for resistance. Assuming TRUE means
  # resistant, but this leads to some inconsistent results (eg very high S aureus
  # vancomycin resistance). Overall, there are even indications that the interpretation
  # might differ by column, since in any case many vancomycin-res S aureus are MSSA,
  # which seems highly unlikely)
  # I tried to compare to published KEYSTONE estimates, but I should have 100% sus
  # for S aureus vanc in 2019, and that is not the case
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
  
  df_KEYSTONE_2 = df_KEYSTONE_2[,which(apply(df_KEYSTONE_2, 2, function(x) !(all(is.na(x)))))]
  
  df_KEYSTONE_2b = df_KEYSTONE_2 %>%
    mutate_if(is.mic, as.sir, mo = .$Organism, guideline = "CLSI")
  
  if(any(apply(df_KEYSTONE_2b, 2, function(x) all(is.na(x))))){
    cat("\nRetrying some columns with EUCAST guidelines...\n")
    to_retry = which(apply(df_KEYSTONE_2b, 2, function(x) all(is.na(x))))
    df_KEYSTONE_2b[,to_retry] = df_KEYSTONE_2 %>%
      select(all_of(c(3,to_retry))) %>%
      mutate_if(is.mic, as.sir, mo = .$Organism, guideline = "EUCAST") %>%
      select(-1)
  }
  df_KEYSTONE_2 = df_KEYSTONE_2b
  rm(df_KEYSTONE_2b)
  
}
#####

#### SIDERO ####
df_SIDERO_2 <- df_SIDERO[,c(3,5,1,7:20)]
df_SIDERO_2 = df_SIDERO_2 %>% select(-"Meropenem/ Vaborbactam at 8")
colnames(df_SIDERO_2) = sapply(colnames(df_SIDERO_2), function(x) unlist(strsplit(x, "/"))[[1]])
colnames(df_SIDERO_2)[-c(1:3)] = as.ab(colnames(df_SIDERO_2)[-c(1:3)])
df_SIDERO_2$`Organism Name` = as.mo(df_SIDERO_2$`Organism Name`)
# no MRSA here so no harmonisation needed

#get antibiotics
if(!(all(is.na(antibiotics_of_interest)))) df_SIDERO_2 <- df_SIDERO_2[,c(1:3, which(colnames(df_SIDERO_2) %in% antibiotics_of_interest))]
#get bacteria
if(!(all(is.na(bacteria_of_interest)))) df_SIDERO_2 <- df_SIDERO_2 %>% filter(`Organism Name` %in% bacteria_of_interest)
#get years
if(length(years_of_interest) != 0) df_SIDERO_2 <- df_SIDERO_2 %>% filter(`Year Collected` %in% years_of_interest)
#get sample source
if(length(sample_of_interest) != 0) df_SIDERO_2 <- df_SIDERO_2 %>% filter(`Body Location` %in% sample_of_interest)

if(nrow(df_SIDERO_2) > 0){
  #format resistances
  df_SIDERO_2 = df_SIDERO_2 %>%
    mutate_at(vars(-Country, -`Year Collected`, -`Organism Name`), as.numeric) %>%
    mutate_at(vars(-Country, -`Year Collected`, -`Organism Name`), round, digits = 3) %>%
    mutate_at(vars(-Country, -`Year Collected`, -`Organism Name`), as.mic)
  df_SIDERO_2 = df_SIDERO_2[,which(apply(df_SIDERO_2, 2, function(x) !(all(is.na(x)))))]
  
  df_SIDERO_2b = df_SIDERO_2 %>%
    mutate_if(is.mic, as.sir, mo = .$`Organism Name`, guideline = "CLSI")
  
  if(any(apply(df_SIDERO_2b, 2, function(x) all(is.na(x))))){
    cat("\nRetrying some columns with EUCAST guidelines...\n")
    to_retry = which(apply(df_SIDERO_2b, 2, function(x) all(is.na(x))))
    df_SIDERO_2b[,to_retry] = df_SIDERO_2 %>%
      select(all_of(c(3,to_retry))) %>%
      mutate_if(is.mic, as.sir, mo = .$`Organism Name`, guideline = "EUCAST") %>%
      select(-1)
  }
  df_SIDERO_2 = df_SIDERO_2b
  rm(df_SIDERO_2b)
  
}
#####


#### DREAM ####
df_DREAM_2 <- df_DREAM[,c(3,1,5,7:18)]
colnames(df_DREAM_2)[colnames(df_DREAM_2) == "EMB"] = "ETH"
colnames(df_DREAM_2)[colnames(df_DREAM_2) == "RMP"] = "RIF"
colnames(df_DREAM_2)[-c(1:3)] = as.ab(colnames(df_DREAM_2)[-c(1:3)])
df_DREAM_2$Organism = as.mo(df_DREAM_2$Organism)
df_DREAM_2$BDQ[df_DREAM_2$BDQ=="1.4999999999999999E-2"] = "0.015"
# no MRSA here so no harmonisation needed

#get antibiotics
if(!(all(is.na(antibiotics_of_interest)))) df_DREAM_2 <- df_DREAM_2[,c(1:3, which(colnames(df_DREAM_2) %in% antibiotics_of_interest))]
#get bacteria
if(!(all(is.na(bacteria_of_interest)))) df_DREAM_2 <- df_DREAM_2 %>% filter(Organism %in% bacteria_of_interest)
#get years
if(length(years_of_interest) != 0) df_DREAM_2 <- df_DREAM_2 %>% filter(`Year Collected` %in% years_of_interest)
#get sample source
if(length(sample_of_interest) != 0) df_DREAM_2 <- df_DREAM_2 %>% filter(Specimen %in% sample_of_interest)

if(nrow(df_DREAM_2) > 0){
  #format resistances
  df_DREAM_2 = df_DREAM_2 %>%
    mutate_at(vars(-Country, -`Year Collected`, -Organism), as.mic)
  df_DREAM_2 = df_DREAM_2[,which(apply(df_DREAM_2, 2, function(x) !(all(is.na(x)))))]
  
  df_DREAM_2b = df_DREAM_2 %>%
    mutate_if(is.mic, as.sir, mo = .$Organism, guideline = "CLSI")
  
  if(any(apply(df_DREAM_2b, 2, function(x) all(is.na(x))))){
    cat("\nRetrying some columns with EUCAST guidelines...\n")
    to_retry = which(apply(df_DREAM_2b, 2, function(x) all(is.na(x))))
    df_DREAM_2b[,to_retry] = df_DREAM_2 %>%
      select(all_of(c(3,to_retry))) %>%
      mutate_if(is.mic, as.sir, mo = .$Organism, guideline = "EUCAST") %>%
      select(-1)
  }
  df_DREAM_2 = df_DREAM_2b
  rm(df_DREAM_2b)
  
}
#####

#### SOAR ####
df_SOAR_2 <- df_SOAR[,c(5,9,6,11:23)]
colnames(df_SOAR_2)[-c(1:3)] = as.ab(colnames(df_SOAR_2)[-c(1:3)])
df_SOAR_2$ORGANISMNAME = as.mo(df_SOAR_2$ORGANISMNAME)
# no MRSA here so no harmonisation needed

#get antibiotics
if(!(all(is.na(antibiotics_of_interest)))) df_SOAR_2 <- df_SOAR_2[,c(1:3, which(colnames(df_SOAR_2) %in% antibiotics_of_interest))]
#get bacteria
if(!(all(is.na(bacteria_of_interest)))) df_SOAR_2 <- df_SOAR_2 %>% filter(ORGANISMNAME %in% bacteria_of_interest)
#get years
if(length(years_of_interest) != 0) df_SOAR_2 <- df_SOAR_2 %>% filter(YEARCOLLECTED %in% years_of_interest)
#get sample source
if(length(sample_of_interest) != 0) df_SOAR_2 <- df_SOAR_2 %>% filter(BODYLOCATION %in% sample_of_interest)

if(nrow(df_SOAR_2) > 0){
  #format resistances
  df_SOAR_2 = df_SOAR_2 %>%
    mutate_at(vars(-COUNTRY, -YEARCOLLECTED, -ORGANISMNAME), as.mic)
  df_SOAR_2 = df_SOAR_2[,which(apply(df_SOAR_2, 2, function(x) !(all(is.na(x)))))]
  
  df_SOAR_2b = df_SOAR_2 %>%
    mutate_if(is.mic, as.sir, mo = .$ORGANISMNAME, guideline = "CLSI")
  
  if(any(apply(df_SOAR_2b, 2, function(x) all(is.na(x))))){
    cat("\nRetrying some columns with EUCAST guidelines...\n")
    to_retry = which(apply(df_SOAR_2b, 2, function(x) all(is.na(x))))
    df_SOAR_2b[,to_retry] = df_SOAR_2 %>%
      select(all_of(c(3,to_retry))) %>%
      mutate_if(is.mic, as.sir, mo = .$ORGANISMNAME, guideline = "EUCAST") %>%
      select(-1)
  }
  df_SOAR_2 = df_SOAR_2b
  rm(df_SOAR_2b)
  
}
#####


#### GLASS ####
df_GLASS_2 <- df_GLASS[,c("CountryTerritoryArea", "Year", "PathogenName", "AbTargets", "InterpretableAST", "Resistant")]

#get antibiotics
df_GLASS_2$AbTargets = as.ab(df_GLASS_2$AbTargets)
if(!(all(is.na(antibiotics_of_interest)))) df_GLASS_2 <- df_GLASS_2 %>% filter(AbTargets %in% antibiotics_of_interest)
#get bacteria
df_GLASS_2$PathogenName = as.mo(df_GLASS_2$PathogenName)
if(!(all(is.na(bacteria_of_interest)))) df_GLASS_2 <- df_GLASS_2 %>% filter(PathogenName %in% bacteria_of_interest)
#get years
if(length(years_of_interest) != 0) df_GLASS_2 <- df_GLASS_2 %>% filter(Year %in% years_of_interest)
#get sample source
if(length(sample_of_interest) != 0) df_GLASS_2 <- df_GLASS_2 %>% filter(Specimen %in% sample_of_interest)

df_GLASS_2 = df_GLASS_2 %>%
  group_by(CountryTerritoryArea, Year, PathogenName, AbTargets) %>%
  summarise(InterpretableAST = sum(InterpretableAST),
            Resistant = sum(Resistant)) %>%
  ungroup()


################################################################################
####### Compute total and R isolates by country/year/bacteria/antibiotic #######
################################################################################

## Format like in GLASS
final_column_names = c("Country", "Year", "Pathogen", "Antibiotic", "Total", "Resistant")

#### ATLAS ####

if(nrow(df_ATLAS_2) > 0 & !(all(is.na(df_ATLAS_2)[,-c(1:3)]))){
  df_ATLAS_3 = df_ATLAS_2 %>%
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
  df_SOAR_3$Data <- 'SOAR'
} else df_SOAR_3 = data.frame()

#####  

#### GLASS ####

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
                                      "US$" = "USA",
                                      "United Kingdom of Great Britain and Northern Ireland" = "UK",
                                      "United Kingdom" = "UK",
                                      "Korea, South" = "South Korea",
                                      "Republic of Korea" = "South Korea",
                                      "Russian Federation" = "Russia",
                                      "Lao People's Democratic Republic" = "Laos",
                                      "Slovak Republic" = "Slovakia",
                                      "INDIA" = "India",
                                      "PHILIPPINES" = "Philippines",
                                      "Iran (Islamic Republic of)" = "Iran",
                                      "Czechia" = "Czech Republic"))
  
  ## Add WHO Region ##
  df_GLASS$CountryTerritoryArea =  str_replace_all(df_GLASS$CountryTerritoryArea,
                                                   c("United States" = "USA",
                                                     "US$" = "USA",
                                                     "United Kingdom of Great Britain and Northern Ireland" = "UK",
                                                     "United Kingdom" = "UK",
                                                     "Korea, South" = "South Korea",
                                                     "Republic of Korea" = "South Korea",
                                                     "Russian Federation" = "Russia",
                                                     "Lao People's Democratic Republic" = "Laos",
                                                     "Slovak Republic" = "Slovakia",
                                                     "INDIA" = "India",
                                                     "PHILIPPINES" = "Philippines",
                                                     "Iran (Islamic Republic of)" = "Iran",
                                                     "Czechia" = "Czech Republic"))
  
  df_AMR = df_AMR %>%
    left_join(df_GLASS %>%
                select(CountryTerritoryArea, WHORegionName) %>%
                distinct() %>%
                setNames(c("Country", "Region")))
  
  
  ## Restore nicer bacteria and antibiotic names
  df_AMR$Antibiotic = as.character(df_AMR$Antibiotic)
  df_AMR$Antibiotic[nchar(df_AMR$Antibiotic) == 3] = ab_name(as.ab(df_AMR$Antibiotic[nchar(df_AMR$Antibiotic) == 3]))
  df_AMR$Pathogen = mo_name(as.mo(df_AMR$Pathogen))
  
  ## Summary
  cat("Combined datasets cover", length(unique(df_AMR$Country)), "countries\n",
      "with a total of", sum(df_AMR$Total), "data points\n",
      "(one data point is one test for one year-country-bacteria-antibiotic-dataset combination)\n")
  cat("Countries per datasets:")
  print(df_AMR %>% group_by(Data) %>% summarise(n_countries = length(unique(Country))))
  cat("Data points per datasets:")
  print(df_AMR %>% group_by(Data) %>% summarise(n_datapoints = sum(Total)))
  
  # Save it into csv
  write.csv(x = df_AMR,
            file = here("data", "final_AMR_dataset.csv"),
            row.names = F)
  
}









