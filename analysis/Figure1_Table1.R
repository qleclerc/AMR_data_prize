
#libraries required
library(dplyr)
library(tidyr)
library(readxl) 
library(tidyverse)
library(ggthemes)
library(mapproj)
library(here)
library(AMR)
library(reshape2)

########################### Load AMR datasets ##################################
################################################################################

#industry
atlas_data <- read.csv(here("data", "raw", "2023_06_15 atlas_antibiotics.csv"))
sidero_data <- read_excel(here("data", "raw", "Updated_Shionogi Five year SIDERO-WT Surveillance data(without strain number)_Vivli_220409.xlsx"))
dream_data <- read_excel(here("data", "raw", "BEDAQUILINE DREAM DATASET FOR VIVLI - 06-06-2022.xlsx"))
gears_data <- read_excel(here("data", "raw", "Venatorx surveillance data for Vivli 27Feb2023.xlsx"))
soar_data <- read.csv(here("data", "raw", "gsk_201818_published.csv"))
keystone_data <- read_excel(here("data", "raw", "Omadacycline_2014_to_2022_Surveillance_data.xlsx"))

#public
glass_data = read.csv(here::here("data", "glass_combined.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GLASS (public) - coverage map ~~~~~~~~~~~~~~~~~
glass_countries = glass_data %>%
  select(CountryTerritoryArea) %>%
  distinct() %>%
  mutate(dataset = "GLASS") %>%
  rename(V1 = CountryTerritoryArea)

# consistent names
glass_countries1 = glass_countries %>%
  mutate(V1 = replace(V1, V1=="United States of America", "USA")) %>%
  mutate(V1 = replace(V1, V1=="United Kingdom of Great Britain and Northern Ireland", "UK")) %>%
  mutate(V1 = replace(V1, V1=="Korea (Republic of)", "South Korea")) %>%
  mutate(V1 = replace(V1, V1=="Russian Federation", "Russia")) %>%
  mutate(V1 = replace(V1, V1=="Taiwan Province of China", "Taiwan")) %>%
  mutate(V1 = replace(V1, V1=="Viet Nam", "Vietnam")) %>%
  mutate(V1 = replace(V1, V1=="Venezuela (Bolivarian Republic of)", "Venezuela")) %>%
  mutate(V1 = replace(V1, V1=="Iran (Islamic Republic of)", "Iran")) %>%
  mutate(V1 = replace(V1, V1=="Czechia", "Czech Republic")) %>%
  mutate(V1 = replace(V1, V1=="Republic of Korea", "South Korea")) %>%
  mutate(V1 = replace(V1, V1=="Lao People's Democratic Republic", "Laos")) %>%
  group_by(V1) %>%
  summarise(n = n()) %>%
  rename(region = V1)


world_map = map_data("world") %>% 
  filter(! long > 180)

glass.countries = world_map %>% 
  distinct(region) %>% 
  rowid_to_column() %>%
  left_join(glass_countries1, by = "region") %>%  # add countries covered by dataset of interest (GLASS)
  mutate(n = replace(n, is.na(n), 0))          # replace NA with 0

#################### Map the coverage for GLASS:
glass.countries %>% 
  ggplot(aes(fill = as.factor(n), map_id = region)) +
  geom_map(map = world_map, color = "white", size = 0.2) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  theme_map() +
  labs(fill = "") + 
  scale_fill_manual(values=c("grey", "deepskyblue2"),
                    labels=c("Unavailable", "Available")) +
  labs(fill = "Data:") + 
  theme(legend.text=element_text(size=10),
        legend.title=element_text(size=12))

ggsave(here("plots", "Map1_GLASS.png"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####### 5 industry datasets: ATLAS, SIDERO, GEARS, KEYSTONE, DREAM, and SOAR

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ATLAS
atlas_countries = atlas_data %>%
  select(Country) %>%
  distinct() %>%
  mutate(dataset = "ATLAS") %>%
  rename(V1 = Country)

atlas_countries1 = atlas_countries %>%
  mutate(V1 = replace(V1, V1=="United States", "USA")) %>%
  mutate(V1 = replace(V1, V1=="United Kingdom", "UK")) %>%
  mutate(V1 = replace(V1, V1=="Korea (Republic of)", "South Korea")) %>%
  mutate(V1 = replace(V1, V1=="Russian Federation", "Russia")) %>%
  mutate(V1 = replace(V1, V1=="Taiwan Province of China", "Taiwan")) %>%
  mutate(V1 = replace(V1, V1=="Viet Nam", "Vietnam")) %>%
  mutate(V1 = replace(V1, V1=="Venezuela (Bolivarian Republic of)", "Venezuela")) %>%
  mutate(V1 = replace(V1, V1=="Korea, South", "South Korea")) %>%
  filter(dataset == "ATLAS") %>%
  group_by(V1) %>%
  summarise(n = n()) %>%
  rename(region = V1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SIDERO:
sidero_countries = sidero_data %>%
  select(Country) %>%
  distinct() %>%
  mutate(dataset = "SIDERO") %>%
  rename(V1 = Country)

sidero_countries1 = sidero_countries %>%
  mutate(V1 = replace(V1, V1=="Korea, South", "South Korea")) %>%
  mutate(V1 = replace(V1, V1=="United States", "USA")) %>%
  mutate(V1 = replace(V1, V1=="United Kingdom", "UK")) %>%
  filter(dataset == "SIDERO") %>%
  group_by(V1) %>%
  summarise(n = n()) %>%
  rename(region = V1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ GEARS:
gears_countries = gears_data %>%
  select(Country) %>%
  distinct() %>%
  mutate(dataset = "GEARS") %>%
  rename(V1 = Country)

gears_countries1 = gears_countries %>%
  mutate(V1 = replace(V1, V1=="United States", "USA")) %>%
  mutate(V1 = replace(V1, V1=="United Kingdom", "UK")) %>%
  filter(dataset == "GEARS") %>%
  group_by(V1) %>%
  summarise(n = n()) %>%
  rename(region = V1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ KEYSTONE: ~~~~~~~~~~~~~~~~~~
keystone_countries = keystone_data %>%
  select(Country) %>%
  distinct() %>%
  mutate(dataset = "KEYSTONE") %>%
  rename(V1 = Country)

keystone_countries1 = keystone_countries %>%
  mutate(V1 = replace(V1, V1=="United States", "USA")) %>%
  mutate(V1 = replace(V1, V1=="United Kingdom", "UK")) %>%
  filter(dataset == "KEYSTONE") %>%
  group_by(V1) %>%
  summarise(n = n()) %>%
  rename(region = V1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ DREAM:
dream_countries = dream_data %>%
  select(Country) %>%
  distinct() %>%
  mutate(dataset = "DREAM") %>%
  rename(V1 = Country)

dream_countries1 = dream_countries %>%
  mutate(V1 = replace(V1, V1=="INDIA", "India")) %>%
  mutate(V1 = replace(V1, V1=="US", "USA")) %>%
  mutate(V1 = replace(V1, V1=="PHILIPPINES", "Phillippines")) %>%
  #filter(dataset == "DREAM") %>%
  group_by(V1) %>%
  summarise(n = n()) %>%
  rename(region = V1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SOAR:
soar_countries = soar_data %>%
  select(COUNTRY) %>%
  distinct() %>%
  mutate(dataset = "SOAR") %>%
  rename(V1 = COUNTRY)

soar_countries1 = soar_countries %>%
  mutate(V1 = replace(V1, V1=="INDIA", "India")) %>%
  mutate(V1 = replace(V1, V1=="US", "USA")) %>%
  mutate(V1 = replace(V1, V1=="PHILIPPINES", "Phillippines")) %>%
  group_by(V1) %>%
  summarise(n = n()) %>%
  rename(region = V1)

#####################################################################
#####################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Map with all 7 datasets
#####################################################################
#####################################################################

glass_countries1      # 46 countries
atlas_countries1      # 83 countries
sidero_countries1     # 51 countries
gears_countries1      # 59 countries
keystone_countries1   # 27 countries
dream_countries1      # 11 countries
soar_countries1       # 9 countries

sets_7 = data.frame()
sets_7 <- rbind(atlas_countries1, sidero_countries1, gears_countries1, keystone_countries1, 
                glass_countries1,
                dream_countries1, soar_countries1) %>%
  group_by(region) %>%
  summarise(n = n())   # 100 countries

####### MAP global coverage for all 7 datasets combined:
world_map = map_data("world") %>% 
  filter(! long > 180)

df7_countries = world_map %>% 
  distinct(region) %>% 
  rowid_to_column() %>%
  left_join(sets_7, by = "region") %>%  # add countries covered by dataset of interest
  mutate(n = replace(n, is.na(n), 0))          # replace NA with 0

# Map the coverage for all 7 datasets:
df7_countries %>% 
  ggplot(aes(fill = as.factor(n), map_id = region)) +
  geom_map(map = world_map, color = "white", size = 0.2) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_map("moll") +
  theme_map() +
  labs(fill = "Data:", size=12) + 
  scale_fill_manual(values=c("grey", "burlywood1", "gold",  "orange", 
                             "darkorange2", "darkorange3","darkorange4","darkgoldenrod"), 
                    labels=c("Unavailable", "1", "2", "3", "4","5","6","7")) +
  labs(title = "") + 
  theme(legend.text=element_text(size=10), 
        legend.title=element_text(size=12))

ggsave(here("plots", "Map2_ALL.png"))



#####################################################################
#####################################################################
################# Bacterial pathogens (diff species) in each dataset
#####################################################################
#####################################################################

atlas_species = atlas_data %>%
  select(Species) %>%
  distinct() %>%
  mutate(dataset = "ATLAS") %>%
  rename(V1 = Species)

sidero_species = sidero_data %>%
  select(`Organism Name`) %>%
  distinct() %>%
  mutate(dataset = "SIDERO") %>%
  rename(V1 = `Organism Name`)

gears_species = gears_data %>%
  select(Organism) %>%
  distinct() %>%
  mutate(dataset = "GEARS") %>%
  rename(V1 = Organism)


keystone_species = keystone_data %>%
  select(Organism) %>%
  distinct() %>%
  mutate(dataset = "KEYSTONE") %>%
  rename(V1 = Organism)

glass_species = glass_data %>%
  select(PathogenName) %>%
  distinct() %>%
  mutate(dataset = "GLASS") %>%
  rename(V1 = PathogenName)

dream_species = dream_data %>%
  select(Organism) %>%
  distinct() %>%
  mutate(dataset = "DREAM") %>%
  rename(V1 = Organism)

soar_species = soar_data %>%
  select(ORGANISMNAME) %>%
  distinct() %>%
  mutate(dataset = "SOAR") %>%
  rename(V1 = ORGANISMNAME)

all_species <- rbind(atlas_species, sidero_species, gears_species, 
                     keystone_species, glass_species,     
                     dream_species, soar_species) %>%
  mutate(V1 = replace(V1, V1=="M. Tuberculosis", "M. tuberculosis")) %>%
  mutate(V1 = replace(V1, V1=="Acinetobacter baumannii-calcoaceticus species complex", "Acinetobacter baumannii complex")) %>%
  mutate(V1 = replace(V1, V1=="Acinetobacter pitii", "Acinetobacter pittii")) %>%
  mutate(V1 = replace(V1, V1=="Acinetobacter sp.", "Acinetobacter spp")) %>%
  mutate(V1 = replace(V1, V1=="Citrobacter sp", "Citrobacter spp")) %>%
  mutate(V1 = replace(V1, V1=="Enterobacter cloacae species complex", "Enterobacter cloacae complex")) %>%
  mutate(V1 = replace(V1, V1=="Enterobacter sp", "Enterobacter spp")) %>%
  mutate(V1 = replace(V1, V1=="Escherichia hermanii", "Escherichia hermannii")) %>%
  mutate(V1 = replace(V1, V1=="Proteus sp", "Proteus spp")) %>%
  mutate(V1 = replace(V1, V1=="Unspeciated Acinetobacter", "Acinetobacter, non-speciated")) %>%
  mutate(V1 = replace(V1, V1=="Providencia sp", "Providencia spp")) %>%
  mutate(V1 = replace(V1, V1=="Salmonella spp.", "Salmonella spp")) %>%
  mutate(V1 = replace(V1, V1=="Serratia sp", "Serratia spp")) %>%
  mutate(V1 = replace(V1, V1=="Staphylococcus aureus, MSSA", "Staphylococcus aureus"))

Nb_unique_species = data.frame()
Nb_unique_species <- unique(all_species$V1) # nb of unique species in all 7 datasets
str(Nb_unique_species)  #416 



##############################################################################################################
##############################################################################################################
############## Nb of isolates and different antibiotics for all datasets ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##############################################################################################################
##############################################################################################################

################# GLASS
glass_atb = glass_data %>%
  select(AbTargets) %>%
  distinct() %>%
  mutate(dataset = "GLASS") %>%
  rename(V1 = AbTargets)

glass_atb # nb of antibiotic agents 24

################# Number of isolates in GLASS data 9775067
glass_data_iso = glass_data %>% 
  select(CountryTerritoryArea, Year, Specimen, TotalSpecimenIsolates) %>%
  distinct()

sum(glass_data_iso$TotalSpecimenIsolates) # Nb of isolates


################# Number of isolates in ATLAS data 858233
atlas_data_iso = atlas_data %>% 
  select(Isolate.Id) %>%
  distinct()
atlas_data_iso

################# Number of isolates in KEYSTONE data 83209
keystone_data_iso = keystone_data %>%
  select(`Collection \r\nNumber`) %>%
  distinct()
str(keystone_data)

################# Number of isolates in SIDERO-WT data 47615
################# Number of isolates in GEARS data 24782
################# Number of isolates in DREAM data 5928
################# Number of isolates in SOAR data 2413



####################### ANTIBIOTICS ##################################


################################## GLASS 24 atbs
glass_atb # glass atb dataset 24 different antibiotics

#################################  GEARS 13 antibiotics
gears_data
gears_atb <- gather(gears_data,"antibiotic","value", 11:23)
gears_atb2 <- gears_atb %>%
  select(antibiotic) %>%
  distinct() %>%
  mutate(dataset = "GEARS") %>%
  rename(V1 = antibiotic)
gears_atb2
#################################  SIDERO-WT 14 antibiotics
sidero_data
sidero_atb <- gather(sidero_data,"antibiotic","value", 7:20)
sidero_atb2 <- sidero_atb %>%
  select(antibiotic) %>%
  distinct() %>%
  mutate(dataset = "SIDERO") %>%
  rename(V1 = antibiotic)
sidero_atb2
################################# KEYSTONE 29 antibiotics
keystone_data
keystone_atb <- gather(keystone_data,"antibiotic","value", 4:32)  
keystone_atb2 <- keystone_atb %>%
  select(antibiotic) %>%
  distinct() %>%
  mutate(dataset = "KEYSTONE") %>%
  rename(V1 = antibiotic)
keystone_atb2   ###### 29 antibiotics
###################################################################

################################# DREAM 12 antibiotics
dream_data
dream_atb <- gather(dream_data,"antibiotic","value", 7:18)
dream_atb2 <- dream_atb %>%
  select(antibiotic) %>%
  distinct() %>%
  mutate(dataset = "DREAM") %>%
  rename(V1 = antibiotic)
dream_atb2
##################################################################

################################## SOAR 13 antibiotics
soar_data
soar_atb <- gather(soar_data,"antibiotic","value", 11:23)
soar_atb2 <- soar_atb %>%
  select(antibiotic) %>%
  distinct() %>%
  mutate(dataset = "SOAR") %>%
  rename(V1 = antibiotic)
soar_atb2
##################################################################

################################# ATLAS 45 atbs

#### ATLAS ####
df_ATLAS_2 = atlas_data[,-c(104:126)] # remove last columns
df_ATLAS_2 <- df_ATLAS_2[,c(grep(pattern = "_I", colnames(df_ATLAS_2), invert=T))]

str(df_ATLAS_2)
atlas_atb <- gather(df_ATLAS_2,"antibiotic","value", 14:58)
atlas_atb2 <- atlas_atb %>%
  select(antibiotic) %>%
  distinct() %>%
  mutate(dataset = "ATLAS")
# atlas antibiotics
atlas_atb2 <- atlas_atb2 %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Amoxycillin.clavulanate", "Amoxycillin clavulanate")) %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Piperacillin.tazobactam", "Piperacillin tazobactam")) %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Ampicillin.sulbactam", "Ampicillin sulbactam")) %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Aztreonam.avibactam", "Aztreonam avibactam")) %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Ceftaroline.avibactam", "Ceftaroline avibactam")) %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Ceftazidime.avibactam", "Ceftazidime avibactam")) %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Quinupristin.dalfopristin", "Quinupristin dalfopristin")) %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Trimethoprim.sulfa", "Trimethoprim sulfa")) %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Ceftolozane.tazobactam", "Ceftolozane tazobactam")) %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Cefoperazone.sulbactam", "Cefoperazone sulbactam")) %>%
  mutate(antibiotic = replace(antibiotic, antibiotic=="Meropenem.vaborbactam", "Meropenem vaborbactam")) %>%
  rename(V1 = antibiotic)

atlas_atb2


##################################### ANTIBIOTICS:
all_atbs <- rbind(glass_atb, gears_atb2, sidero_atb2, 
                  keystone_atb2, dream_atb2, soar_atb2, atlas_atb2) 
all_atbs

all_atbs_renamed <- all_atbs %>%
  mutate(V1 = replace(V1, V1=="AMI", "Amikacin")) %>%
  mutate(V1 = replace(V1, V1=="AMOXICILLIN", "Amoxycillin")) %>%
  mutate(V1 = replace(V1, V1=="AMOXICILLIN_CLAVULANATE", "Amoxycillin clavulanate")) %>%
  mutate(V1 = replace(V1, V1=="Amoxicillin-\r\nclavulanic acid", "Amoxycillin clavulanate")) %>%
  mutate(V1 = replace(V1, V1=="AMPICILLIN", "Ampicillin")) %>%
  mutate(V1 = replace(V1, V1=="Ampicillin/ Sulbactam", "Ampicillin sulbactam")) %>%
  mutate(V1 = replace(V1, V1=="AZITHROMYCIN", "Azithromycin")) %>%
  mutate(V1 = replace(V1, V1=="Aztreonam/ Avibactam", "Aztreonam avibactam")) %>%
  mutate(V1 = replace(V1, V1=="Ceftazidime/ Avibactam", "Ceftazidime avibactam")) %>%
  mutate(V1 = replace(V1, V1=="Ceftazidime/ Avibactam", "Ceftazidime avibactam")) %>%
  mutate(V1 = replace(V1, V1=="Ceftolozane/ Tazobactam", "Ceftolozane tazobactam")) %>%
  mutate(V1 = replace(V1, V1=="CEFTRIAXONE", "Ceftriaxone")) %>%
  mutate(V1 = replace(V1, V1=="CFZ", "Clofazimine")) %>%
  mutate(V1 = replace(V1, V1=="CLARITHROMYCIN", "Clarithromycin")) %>%
  mutate(V1 = replace(V1, V1=="ERYTHROMYCIN", "Erythromycin")) %>%
  mutate(V1 = replace(V1, V1=="LEVOFLOXACIN", "Levofloxacin")) %>%
  mutate(V1 = replace(V1, V1=="Meropenem/ Vaborbactam at 8", "Meropenem vaborbactam")) %>%
  mutate(V1 = replace(V1, V1=="Methicillin resistance", "Methicillin")) %>%
  mutate(V1 = replace(V1, V1=="MOXIFLOXACIN", "Moxifloxacin")) %>%
  mutate(V1 = replace(V1, V1=="MXF", "Moxifloxacin")) %>%
  mutate(V1 = replace(V1, V1=="PENICILLIN", "Penicillin")) %>%
  mutate(V1 = replace(V1, V1=="Piperacillin-\r\ntazobactam", "Piperacillin tazobactam")) %>%
  mutate(V1 = replace(V1, V1=="TRIMETHOPRIM_SULFA", "Trimethoprim sulfa")) %>%
  mutate(V1 = replace(V1, V1=="Trimethoprim-sulfamethoxazole", "Trimethoprim sulfa")) %>%
  mutate(V1 = replace(V1, V1=="Trimethoprim/ Sulfamethoxazole", "Trimethoprim sulfa")) %>%
  select(V1) %>%
  distinct()

all_atbs_renamed

all_atbs_coded <- rbind(all_atbs_renamed$V1,
                        all_atbs_renamed$V1 %>% set_ab_names(property = "atc"))
all_atbs_coded # 81 different antibiotic treatments 
