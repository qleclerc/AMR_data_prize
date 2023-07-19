################################################################################
####### Algorithm to compute Cephalosporin or Carbapenem resistance  ###########
################################################################################

#--> if resistant to one molecule of the class, isolate is considered resistant to the class

#### ATLAS ####

df_ATLAS_2$Cephalosporins <- ifelse(is.na(df_ATLAS_2$Ceftazidime_I) & is.na(df_ATLAS_2$Ceftriaxone_I),
                                    NA,
                                    ifelse(df_ATLAS_2$Ceftazidime_I %in% 'Resistant' | df_ATLAS_2$Ceftriaxone_I %in% 'Resistant',
                                           "Resistant",
                                           "NonResistant"))

df_ATLAS_2$Carbapenems <- ifelse(is.na(df_ATLAS_2$Imipenem_I) & is.na(df_ATLAS_2$Meropenem_I),
                                 NA,
                                 ifelse(df_ATLAS_2$Imipenem_I %in% 'Resistant' | df_ATLAS_2$Meropenem_I %in% 'Resistant',
                                        "Resistant",
                                        "NonResistant"))
#####

##### KEYSTONE ####

df_KEYSTONE_2$Cephalosporins <- ifelse(is.na(df_KEYSTONE_2$Ceftazidime_I) & is.na(df_KEYSTONE_2$Ceftriaxone_I),
                                       NA,
                                       ifelse(df_KEYSTONE_2$Ceftazidime_I %in% 'Resistant' | df_KEYSTONE_2$Ceftriaxone_I %in% 'Resistant',
                                              "Resistant",
                                              "NonResistant"))

df_KEYSTONE_2$Carbapenems <- ifelse(is.na(df_KEYSTONE_2$Imipenem_I),
                                    NA,
                                    ifelse(df_KEYSTONE_2$Imipenem_I %in% 'Resistant',
                                           "Resistant",
                                           "NonResistant"))

#####

#### SIDERO ####

df_SIDERO_2$Cephalosporins <- ifelse(is.na(df_SIDERO_2$Ceftazidime_I),
                                     NA,
                                     ifelse(df_SIDERO_2$Ceftazidime_I %in% 'Resistant',
                                            "Resistant",
                                            "NonResistant"))

df_SIDERO_2$Carbapenems <- ifelse(is.na(df_SIDERO_2$Imipenem_I) & is.na(df_SIDERO_2$Meropenem_I),
                                  NA,
                                  ifelse(df_SIDERO_2$Imipenem_I %in% 'Resistant' | df_SIDERO_2$Meropenem_I %in% 'Resistant',
                                         "Resistant",
                                         "NonResistant"))
#####  

#### GEARS ####

df_GEARS_2$Cephalosporins <- ifelse(is.na(df_GEARS_2$Ceftazidime_I),
                                    NA,
                                    ifelse(df_GEARS_2$Ceftazidime_I %in% 'Resistant',
                                           "Resistant",
                                           "NonResistant"))

df_GEARS_2$Carbapenems <- ifelse(is.na(df_GEARS_2$Imipenem_I) & is.na(df_GEARS_2$Meropenem_I),
                                 NA,
                                 ifelse(df_GEARS_2$Imipenem_I %in% 'Resistant' | df_GEARS_2$Meropenem_I %in% 'Resistant',
                                        "Resistant",
                                        "NonResistant"))

#####

##### GLASS ####

#for GLASS we take the average of the number of resistant and total isolates tested over the two molecules

##Cephalosporins

molecule11 <- df_GLASS_2 %>% filter(AbTargets %in% c('Ceftriaxone'))
molecule12 <- df_GLASS_2 %>% filter(AbTargets %in% c('Ceftazidime'))
df_GLASS_2_ceph <- merge(molecule11, molecule12, by = c("CountryTerritoryArea", "Year", "PathogenName"), all = T)

#new columns
df_GLASS_2_ceph$AbTargets <- "Cephalosporins"
df_GLASS_2_ceph$InterpretableAST <- rowMeans(df_GLASS_2_ceph[,c("InterpretableAST.x", "InterpretableAST.y")], na.rm = T)
df_GLASS_2_ceph$Resistant <- rowMeans(df_GLASS_2_ceph[,c("Resistant.x", "Resistant.y")], na.rm = T)

#remove old molecules
df_GLASS_2_ceph <- df_GLASS_2_ceph %>% select(!(c("AbTargets.x", "InterpretableAST.x", "Resistant.x", "AbTargets.y", "InterpretableAST.y","Resistant.y")))

##Carbapenems

molecule21 <- df_GLASS_2 %>% filter(AbTargets %in% c('Imipenem'))
molecule22 <- df_GLASS_2 %>% filter(AbTargets %in% c('Meropenem'))
df_GLASS_2_carb <- merge(molecule21, molecule22, by = c("CountryTerritoryArea", "Year", "PathogenName"), all = T)

#new columns
df_GLASS_2_carb$AbTargets <- "Carbapenems"
df_GLASS_2_carb$InterpretableAST <- rowMeans(df_GLASS_2_carb[,c("InterpretableAST.x", "InterpretableAST.y")], na.rm = T)
df_GLASS_2_carb$Resistant <- rowMeans(df_GLASS_2_carb[,c("Resistant.x", "Resistant.y")], na.rm = T)

#remove old molecules
df_GLASS_2_carb <- df_GLASS_2_carb %>% select(!(c("AbTargets.x", "InterpretableAST.x", "Resistant.x", "AbTargets.y", "InterpretableAST.y","Resistant.y")))

## Merge two classes

df_GLASS_2 <- rbind(df_GLASS_2_ceph, df_GLASS_2_carb)

#####

#TO DO: !! change names of molecules to names of classes in code below !!