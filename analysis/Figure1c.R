
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

#industry
df_ATLAS <- read.csv(here("data", "raw", "2023_06_15 atlas_antibiotics.csv"))
df_SIDERO <- read_excel(here("data", "raw", "Updated_Shionogi Five year SIDERO-WT Surveillance data(without strain number)_Vivli_220409.xlsx"))
df_DREAM <- read_excel(here("data", "raw", "BEDAQUILINE DREAM DATASET FOR VIVLI - 06-06-2022.xlsx"))
df_GEARS <- read_excel(here("data", "raw", "Venatorx surveillance data for Vivli 27Feb2023.xlsx"))
df_SOAR <- read.csv(here("data", "raw", "gsk_201818_published.csv"))
df_KEYSTONE <- read_excel(here("data", "raw", "Omadacycline_2014_to_2022_Surveillance_data.xlsx"))

#public
df_GLASS = read.csv(here::here("data", "glass_combined.csv"))

df_ATLAS = df_ATLAS %>%
  count(Year) %>%
  mutate(dataset = "ATLAS")
colnames(df_ATLAS)[1] = "Year"

df_SIDERO = df_SIDERO %>%
  count(`Year Collected`) %>%
  mutate(dataset = "SIDERO")
colnames(df_SIDERO)[1] = "Year"

df_DREAM = df_DREAM %>%
  count(`Year Collected`) %>%
  mutate(dataset = "DREAM")
colnames(df_DREAM)[1] = "Year"

df_GEARS = df_GEARS %>%
  count(Year) %>%
  mutate(dataset = "GEARS")
colnames(df_GEARS)[1] = "Year"

df_SOAR = df_SOAR %>%
  count(YEARCOLLECTED) %>%
  mutate(dataset = "SOAR")
colnames(df_SOAR)[1] = "Year"

df_KEYSTONE = df_KEYSTONE %>%
  count(`Study Year`) %>%
  mutate(dataset = "KEYSTONE")
colnames(df_KEYSTONE)[1] = "Year"

df_GLASS = df_GLASS %>%
  select(CountryTerritoryArea, Year, Specimen, TotalSpecimenIsolates) %>%
  distinct %>%
  group_by(Year) %>%
  summarise(n = sum(TotalSpecimenIsolates)) %>%
  mutate(dataset = "GLASS")
colnames(df_GLASS)[1] = "Year"

rbind(df_ATLAS, df_DREAM, df_GEARS, df_GLASS, df_KEYSTONE, df_SIDERO, df_SOAR) %>%
  ggplot() +
  geom_line(aes(Year, log10(n), colour = dataset), linewidth = 1) +
  scale_x_continuous(breaks = seq(2004, 2022, 2)) +
  scale_y_continuous(breaks = c(1:7)) +
  theme_bw() +
  labs(x = "Year", y = "Number of isolates (log10)", colour = "") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12))

ggsave(here::here("plots", "fig1c.png"))
