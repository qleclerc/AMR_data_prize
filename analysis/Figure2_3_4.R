
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
library(ggh4x)

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


y = df_AMR %>%
  filter(Data != "GLASS") %>%
  group_by(Country, Region, Year, Pathogen, Antibiotic) %>%
  summarise(Total = sum(Total),
            Resistant = sum(Resistant)) %>%
  mutate(Data = "All",
         p = Resistant/Total) %>%
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

col_pal = c("red1", "firebrick3",
            "mediumorchid1", "mediumpurple2", "purple3",
            "orange1", "darkorange1", "darkorange3",
            "darkolivegreen3", "darkolivegreen",
            "deepskyblue", "dodgerblue", "royalblue",
            "gold1",
            "black", "grey70",
            "slategrey")


## FIGURE 2 ####

ggplot(x_y) +
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
  labs(x = "Resistance proportion GLASS", y = "Resistance proportion combined industry datasets", colour = "") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size = 12, face = "italic"),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

ggsave(here::here("plots", "fig2.png"), height = 8, width = 10)


## FIGURE 3 ####

x_y %>%
  group_by(Year, Pathogen, Antibiotic) %>%
  summarise(Total.y = sum(Total.y),
            Resistant.y = sum(Resistant.y)) %>%
  ungroup %>%
  mutate(p.y = Resistant.y/Total.y) %>%
  mutate(err = 1.96*sqrt(p.y*(1-p.y)/Total.y)) %>%
  mutate(Class = as.character(Antibiotic)) %>%
  mutate(Class = replace(Class, Class %in% c("Amikacin", "Gentamicin"), "Amino."),
         Class = replace(Class, Class %in% c("Doripenem", "Imipenem", "Meropenem"), "Carb."),
         Class = replace(Class, Class %in% c("Cefepime", "Ceftazidime", "Ceftriaxone"), "Ceph."),
         Class = replace(Class, Class %in% c("Ciprofloxacin", "Levofloxacin"), "Fluo."),
         Class = replace(Class, Class %in% c("Ampicillin", "Benzylpenicillin", "Oxacillin"), "Beta."),
         Class = replace(Class, Class %in% c("Minocycline", "Tigecycline"), "Tetra."),
         Class = replace(Class, Class %in% c("Colistin"), "Col."),
         Class = replace(Class, Class %in% c("Trimethoprim/sulfamethoxazole"), "Trim.")) %>%
  mutate(Class = factor(Class, levels = c("Amino.", "Carb.", "Ceph.",
                                          "Fluo.", "Beta.", "Col.",
                                          "Tetra.", "Trim."))) %>%
  ggplot() +
  geom_line(aes(x=Year, y=p.y, colour = Antibiotic, group=Antibiotic), linewidth=0.8) +
  geom_errorbar(aes(x=Year, ymin=p.y-err, ymax=p.y+err, colour = Antibiotic, group=Antibiotic), width=0.5, linewidth=0.8) +
  facet_grid(cols=vars(Pathogen), rows=vars(Class)) +
  scale_color_discrete(type=col_pal) +
  theme_bw() +
  labs(y = "Resistance proportion in combined industry dataset", colour="")+
  theme(axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.title = element_text(size = 11),
        strip.text.x = element_text(face = "italic"),
        legend.position = "bottom",
        legend.text = element_text(size = 11))

ggsave(here::here("plots", "fig3.png"), height = 8, width = 9.5)

## FIGURE 4 ####

p0 = ggplot(x_y) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Industry isolates in comparison",
       y = "Difference in resistance proportions",
       colour = "WHO Region:") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))


af_data = x_y %>%
  filter(Region == "African Region") %>%
  mutate(Diff = abs(Diff))
cor_af = cor.test(af_data$Diff, af_data$Total.y, method = "spearman")

p1 = ggplot(af_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_af$estimate, 2),
                                "\np val: ", round(cor_af$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[1]) +
  labs(x = "Industry isolates in comparison",
       y = "Absolute diff. in resistance",
       colour = "WHO Region:",
       title = "African Region") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size=12, face = "bold")) +
  guides(colour = "none")

em_data = x_y %>%
  filter(Region == "Eastern Mediterranean Region") %>%
  mutate(Diff = abs(Diff))
cor_em=cor.test(em_data$Diff, em_data$Total.y, method = "spearman")

p2 = ggplot(em_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_em$estimate, 2),
                                "\np val: ", round(cor_em$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[2]) +
  labs(x = "Industry isolates in comparison",
       y = "Absolute diff. in resistance",
       colour = "WHO Region:",
       title = "Eastern Mediterranean Region") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size=12, face = "bold")) +
  guides(colour = "none")

eu_data = x_y %>%
  filter(Region == "European Region") %>%
  mutate(Diff = abs(Diff))
cor_eu=cor.test(eu_data$Diff, eu_data$Total.y, method = "spearman")

p3 = ggplot(eu_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_eu$estimate, 2),
                                "\np val: ", round(cor_eu$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[3]) +
  labs(x = "Industry isolates in comparison",
       y = "Absolute diff. in resistance",
       colour = "WHO Region:",
       title = "European Region") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size=12, face = "bold")) +
  guides(colour = "none")

am_data = x_y %>%
  filter(Region == "Region of the Americas") %>%
  mutate(Diff = abs(Diff))
cor_am=cor.test(am_data$Diff, am_data$Total.y, method = "spearman")

p4 = ggplot(am_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_am$estimate, 2),
                                "\np val: ", round(cor_am$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[4]) +
  labs(x = "Industry isolates in comparison",
       y = "Absolute diff. in resistance",
       colour = "WHO Region:",
       title = "Region of the Americas") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size=12, face = "bold")) +
  guides(colour = "none")

se_data = x_y %>%
  filter(Region == "South-East Asia Region") %>%
  mutate(Diff = abs(Diff))
cor_se=cor.test(se_data$Diff, se_data$Total.y, method = "spearman")

p5 = ggplot(se_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_se$estimate, 2),
                                "\np val: ", round(cor_se$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[5]) +
  labs(x = "Industry isolates in comparison",
       y = "Absolute diff. in resistance",
       colour = "WHO Region:",
       title = "South-East Asia Region") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size=12, face = "bold")) +
  guides(colour = "none")

wp_data = x_y %>%
  filter(Region == "Western Pacific Region") %>%
  mutate(Diff = abs(Diff))
cor_wp=cor.test(wp_data$Diff, wp_data$Total.y, method = "spearman")

p6 = ggplot(wp_data) +
  geom_point(aes(Total.y, Diff, colour = Region), size = 2, alpha = 0.5) +
  geom_smooth(aes(Total.y, Diff), method = "lm", se = F, colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(x = max(Total.y)*0.8, y = max(Diff)*0.8,
                 label = paste0("cor: ", round(cor_wp$estimate, 2),
                                "\np val: ", round(cor_wp$p.value, 3))),
             alpha = 0.5, label.size = NA) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(6, "Dark2")[6]) +
  labs(x = "Industry isolates in comparison",
       y = "Absolute diff. in resistance",
       colour = "WHO Region:",
       title = "Western Pacific Region") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size=12, face = "bold")) +
  guides(colour = "none")

plot_grid(p0,
          plot_grid(p1 + theme(axis.title.x = element_blank()),
                    p2 + theme(axis.title.y = element_blank(),
                               axis.title.x = element_blank()),
                    p3 + theme(axis.title.y = element_blank(),
                               axis.title.x = element_blank()),
                    p4,
                    p5 + theme(axis.title.y = element_blank()),
                    p6 + theme(axis.title.y = element_blank()),
                    ncol=3), ncol=1, labels = c("a)", "b)"), hjust = 0, vjust = c(1,0))

ggsave(here("plots", "fig4.png"),
       width = 9, height = 10)


#####