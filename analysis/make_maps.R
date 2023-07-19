
library(ggplot2)
library(dplyr)
library(RColorBrewer)

col_pal = brewer.pal(7, "RdBu")[-4]
datasets = list.files(here::here("data"))

all_abx = data.frame()
all_bac = data.frame()
all_countries = data.frame()
all_source = data.frame()

for(dataset in datasets){
  
  if(grepl(".csv", dataset)) next
  all_abx = rbind(all_abx, cbind(read.table(here::here("data", dataset, "abx.txt"), sep="\n"), dataset, V2="antibiotic"))
  all_bac = rbind(all_bac, cbind(read.table(here::here("data", dataset, "bac.txt"), sep="\n"), dataset, V2="bacteria"))
  all_countries = rbind(all_countries, cbind(read.table(here::here("data", dataset, "countries.txt"), sep="\n"), dataset, V2="country"))
  all_source = rbind(all_source, cbind(read.table(here::here("data", dataset, "source.txt"), sep="\n"), dataset, V2="source"))
  
}

all_data = rbind(all_abx, all_bac, all_countries, all_source) %>%
  rename(value = V1, variable = V2) %>%
  select(variable, dataset, value)

write.csv(all_data, "all_surveillance_data.csv", row.names=F)

glass_data = read.csv(here::here("data", "compiled_WHO_GLASS_2022.csv"))
all_countries = rbind(all_countries,
                      glass_data %>%
                        select(CountryTerritoryArea) %>%
                        distinct() %>%
                        mutate(dataset = "GLASS") %>%
                        rename(V1 = CountryTerritoryArea))

cov_countries = all_countries %>%
  mutate(V1 = replace(V1, V1=="United States of America", "USA")) %>%
  mutate(V1 = replace(V1, V1=="Korea (Republic of)", "South Korea")) %>%
  mutate(V1 = replace(V1, V1=="Russian Federation", "Russia")) %>%
  mutate(V1 = replace(V1, V1=="Taiwan Province of China", "Taiwan")) %>%
  mutate(V1 = replace(V1, V1=="United Kingdom of Great Britain and Northern Ireland", "UK")) %>%
  mutate(V1 = replace(V1, V1=="Viet Nam", "Vietnam")) %>%
  mutate(V1 = replace(V1, V1=="Venezuela (Bolivarian Republic of)", "Venezuela")) %>%
  mutate(V1 = replace(V1, V1=="Iran (Islamic Republic of)", "Iran")) %>%
  mutate(V1 = replace(V1, V1=="Czechia", "Czech Republic")) %>%
  mutate(V1 = replace(V1, V1=="Republic of Korea", "South Korea")) %>%
  mutate(V1 = replace(V1, V1=="Lao People's Democratic Republic", "Laos")) %>%
  group_by(V1) %>%
  summarise(n = n()) %>%
  rename(region = V1)

cov_map = map_data("world") %>%
  left_join(cov_countries, by = "region") %>%
  mutate(n = replace(n, is.na(n), 0))


ggplot(cov_map) +
  geom_polygon(aes(long,lat,group=group,fill=as.factor(n)), colour = "black") +
  theme_void() +
  scale_fill_manual(values=c("grey", col_pal)) +
  labs(fill = "Datasets:")
ggsave("all_map.png")

round(prop.table(table(cov_map %>% select(region, n) %>% distinct %>% select(n))), 2)

atlas_countries = all_countries %>%
  mutate(V1 = replace(V1, V1=="United States of America", "USA")) %>%
  mutate(V1 = replace(V1, V1=="Korea (Republic of)", "South Korea")) %>%
  mutate(V1 = replace(V1, V1=="Russian Federation", "Russia")) %>%
  mutate(V1 = replace(V1, V1=="Taiwan Province of China", "Taiwan")) %>%
  mutate(V1 = replace(V1, V1=="United Kingdom of Great Britain and Northern Ireland", "UK")) %>%
  mutate(V1 = replace(V1, V1=="Viet Nam", "Vietnam")) %>%
  mutate(V1 = replace(V1, V1=="Venezuela (Bolivarian Republic of)", "Venezuela")) %>%
  filter(dataset == "ATLAS") %>%
  group_by(V1) %>%
  summarise(n = n()) %>%
  rename(region = V1)

atlas_map = map_data("world") %>%
  left_join(atlas_countries, by = "region") %>%
  mutate(n = replace(n, is.na(n), 0))

ggplot(atlas_map) +
  geom_polygon(aes(long,lat,group=group,fill=as.factor(n)), colour = "black") +
  theme_void() +
  scale_fill_manual(values=c("grey", col_pal[6])) +
  labs(fill="")
ggsave("atlas_map.png")

all_countries %>%
  group_by(dataset) %>%
  summarise(n=n())
length(unique(all_countries$V1))
