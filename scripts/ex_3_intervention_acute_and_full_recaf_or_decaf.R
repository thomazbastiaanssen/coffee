source("subscripts/load_and_clean_data.R")


#####Coffee vs non-coffee

meta_ex_INTERVENTION <- metadata[metadata$Legend_ex1 != "NCD" & metadata$visit %in% c("V3", "T2I", "T4I", "T14I", "V4"),]
meta_ex_INTERVENTION$Legend_ex_INTERVENTION = factor(meta_ex_INTERVENTION$Legend_ex_INTERVENTION, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
                                                                                                             "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
                                                                                                             "Post-Intervention (V4)"))


species.exp_ex_INTERVENTION <- species.exp[,meta_ex_INTERVENTION$R_ID]
GBMs.exp_ex_INTERVENTION <- GBMs.exp[,meta_ex_INTERVENTION$R_ID]
GMMs.exp_ex_INTERVENTION <- GMMs.exp[,meta_ex_INTERVENTION$R_ID]

alpha_div_ex_INTERVENTION <- alpha_div[meta_ex_INTERVENTION$R_ID,]


#Apply the base R principal component analysis function on our CLR-transformed data.
data.a.pca  <- prcomp(t(species.exp_ex_INTERVENTION))

#Extract the amount of variance the first four components explain for plotting. 
pc1 <- round(data.a.pca$sdev[1]^2/sum(data.a.pca$sdev^2),4) * 100
pc2 <- round(data.a.pca$sdev[2]^2/sum(data.a.pca$sdev^2),4) * 100
pc3 <- round(data.a.pca$sdev[3]^2/sum(data.a.pca$sdev^2),4) * 100
pc4 <- round(data.a.pca$sdev[4]^2/sum(data.a.pca$sdev^2),4) * 100

#Extract the scores for every sample for the first four components for plotting. 
pca  = data.frame(PC1 = data.a.pca$x[,1], 
                  PC2 = data.a.pca$x[,2], 
                  PC3 = data.a.pca$x[,3], 
                  PC4 = data.a.pca$x[,4])

#Add relevant information from the metadata
pca$ID                  = meta_ex_INTERVENTION$participant_ID
pca$Legend              = meta_ex_INTERVENTION$Legend_ex_INTERVENTION 
pca$visit               = meta_ex_INTERVENTION$visit
pca$batch               = meta_ex_INTERVENTION$batch
pca$Treatment           = meta_ex_INTERVENTION$Treatment

#First, the main plot. Plot the first two components of the PCA

ex_INTERVENTIONpca <- pca %>% 
  
  arrange(Legend) %>% 
  ggplot() +
  
  aes(x       = PC1, 
      y       = PC2, 
      fill    = Legend,
      group   = Legend) +
  
  #Create the points and ellipses
  #stat_ellipse(geom = "polygon", alpha = 1/4, colour = "black") +
  geom_path(aes(group = ID)) +
  geom_point(size = 3, col = "black", shape = 21) + 
  
  #Adjust appearance
  scale_fill_manual(values = c("Pre-Intervention (V3)"         = "#ffffcc", 
                               "Day 2 of intervention (T2I)"   = "#d9f0a3",
                               "Day 4 of intervention (T4I)"   = "#78c679",
                               "Day 14 of intervention (T14I)" = "#006837", 
                               "Post-Intervention (V4)"        = "#004529")) +
  
  facet_wrap(~Treatment) +
  #Adjust labels
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) + 
  theme_bw() + ggtitle("Principal Component Analysis (Beta Diversity)")

#ex_INTERVENTIONpca




dis_ait = dist(t(species.exp_ex_INTERVENTION), method = "euclidean")


ex_INTERVENTION_PERMANOVA <- adonis2(dis_ait ~ Legend_ex_INTERVENTION * Treatment, 
                                 data = meta_ex_INTERVENTION, 
                                 method = "euclidean", 
                                 strata = meta_ex_INTERVENTION$participant_ID,
                                 permutations = 10000)


# dis_ait %>% 
#   as.matrix %>% 
#   data.frame() %>% 
#   rownames_to_column("sample1") %>% 
#   pivot_longer(!sample1, names_to = "sample2") %>% 
#   filter(sample1 != sample2) %>% 
#   
#   left_join(., meta_ex_INTERVENTION %>% 
#               dplyr::select(c("R_ID", "participant_ID", "Legend_ex_INTERVENTION")), 
#             by = c("sample1" = "R_ID")) %>% 
#   
#   left_join(., meta_ex_INTERVENTION %>% 
#               dplyr::select(c("R_ID", "participant_ID", "Legend_ex_INTERVENTION", "Treatment")), 
#             by = c("sample2" = "R_ID")) %>% 
#   
#   filter(participant_ID.x == participant_ID.y) %>% 
#   
#   dplyr::select(!c(sample1, sample2)) %>% 
#   
#   filter((Legend_ex_INTERVENTION.x == "Pre-Intervention (T0I)" & Legend_ex_INTERVENTION.y == "Day 2 of intervention (T2I)")|
#            (Legend_ex_INTERVENTION.x == "Day 2 of intervention (T2I)" & Legend_ex_INTERVENTION.y == "Day 4 of intervention (T4I)") |
#            (Legend_ex_INTERVENTION.x == "Day 4 of intervention (T4I)" & Legend_ex_INTERVENTION.y == "Day 14 of intervention (T14I)")) %>% 
#   
#   mutate(Legend = paste(Legend_ex_INTERVENTION.x, "vs", Legend_ex_INTERVENTION.y)) %>% 
#   ggplot() +
#   
#   aes(x = Legend, y = value) +
#   geom_boxplot() +
#   geom_point() +
#   facet_wrap(~Treatment) +
#   theme_bw()

ex_INTERVENTIONalpha <- alpha_div_ex_INTERVENTION %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex_INTERVENTION, levels = levels(meta_ex_INTERVENTION$Legend_ex_INTERVENTION))) %>% 
  
  ggplot()+
  
  aes(x = Legend, 
      y = value, 
      fill  = Legend, 
      group = Legend) + 
  
  geom_boxplot(alpha = 1/2, coef = Inf, show.legend = F)+
  geom_point(shape = 21, show.legend = F) +
  
  
  #Adjust appearance
  scale_fill_manual(values = c("Pre-Intervention (V3)"        = "#ffffcc", 
                               "Day 2 of intervention (T2I)"   = "#d9f0a3",
                               "Day 4 of intervention (T4I)"   = "#78c679",
                               "Day 14 of intervention (T14I)" = "#006837", 
                               "Post-Intervention (V4)"        = "#004529")) +
  
  facet_grid(name ~ Treatment, scales = "free_y") +
  ylab("") + xlab("") + theme_bw() + ggtitle("Alpha Diversity") +
  scale_x_discrete(labels = c( "0\n(Post washout)", "2", "4", "14", 21)) +
  theme(text = element_text(size = 12))

#ex_INTERVENTIONalpha

alpha_div_ex_INTERVENTION %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex_INTERVENTION, levels = levels(meta_ex_INTERVENTION$Legend_ex_INTERVENTION))) %>% 
  
  group_by(name) %>% 
  
  reframe(
    
    lm(value ~ Legend * Treatment, data = across(everything())) %>% car::Anova() %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  
  write.csv(., "stats/ex_INTERVENTION/alpha_div_glms.csv")


species.exp_ex_INTERVENTION <- genefilter::varFilter(species.exp_ex_INTERVENTION, var.cutoff = 0.5)

species.glmer_ex_INTERVENTION <- fw_glmer(x = species.exp_ex_INTERVENTION, 
                                      f = ~ Legend_ex_INTERVENTION * Treatment + (1|participant_ID), 
                                      metadata = meta_ex_INTERVENTION, 
                                      order = "ac", verbose = FALSE) 



(species.exp_ex_INTERVENTION/log(2)) %>% 
  as.data.frame() %>% 
  rownames_to_column("feature") %>% 
  
  left_join(., species.glmer_ex_INTERVENTION[,c("feature", "anovas.Legend_ex_INTERVENTION Pr(>F).BH", 
                                            "coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I) Pr(>|t|)",
                                            "coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I) Pr(>|t|)" )], by = c("feature" = "feature")) %>% 
  
  filter(`anovas.Legend_ex_INTERVENTION Pr(>F).BH` < 0.2) %>% 
  dplyr::select(!c("anovas.Legend_ex_INTERVENTION Pr(>F).BH","anovas.Legend_ex_INTERVENTION Pr(>F).BH", 
                   "coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I) Pr(>|t|)",
                   "coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I) Pr(>|t|)"  )) %>% 
  
  pivot_longer(!c(feature)) %>% 
  
  
  
  left_join(., meta_ex_INTERVENTION[,c("Legend_ex_INTERVENTION","R_ID", "participant_ID", "Treatment")], by = c("name" = "R_ID")) %>% 
  
  dplyr::select(!name) %>% 
  
  mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
  
  filter(!is.na(value)) %>% 
  
  # group_by(Legend_ex_withdraw, feature) %>% 
  # mutate(mean = mean(value, na.rm = T), 
  #        SEM   =   std(value)) %>% 
  # ungroup() %>% 
  
  mutate(Legend_ex_INTERVENTION = factor(Legend_ex_INTERVENTION, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
                                                                            "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
                                                                            "Post-Intervention (V4)")
  )) %>% 
  
  ggplot() +
  
  aes(x = Legend_ex_INTERVENTION, y = value, fill = Legend_ex_INTERVENTION) +
  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
  
  geom_boxplot(alpha = 1/2, coef = Inf)+
  geom_point(shape = 21) +
  
  # geom_errorbar(aes(x = Legend_ex_withdraw, 
  #                   ymin = mean - SEM, 
  #                   ymax = mean + SEM), 
  #               colour = "black", width = 1/4, position = position_dodge(1/3)) +
  
  #geom_point(aes(x = Legend_ex_withdraw, y = mean, fill = Legend_ex_withdraw), shape = 21, size = 3) +
  
  #scale_y_discrete(position = "right") +
  scale_fill_manual(values = c("Pre-Intervention (V3)"        = "#ffffcc", 
                               "Day 2 of intervention (T2I)"   = "#d9f0a3",
                               "Day 4 of intervention (T4I)"   = "#78c679",
                               "Day 14 of intervention (T14I)" = "#006837", 
                               "Post-Intervention (V4)"        = "#004529"), "Legend") +
  
  scale_shape_manual(values = c("CD" = 21, "NCD" = 22)) +
  guides(shape = FALSE, fill = guide_legend(override.aes = list(shape = c(21)))) +
  
  scale_x_discrete(labels = c( "0", "2", "4", "14", 21)) +
  #facet_grid(feature ~ Treatment , scales = "free", switch = "y") +
  facet_grid(feature ~ Treatment, scales = "free_y") +
  theme_bw() + xlab(NULL) + ylab("Abundance (CLR)") +  
  ggtitle("Bacterial species altered following coffee reintroduction") +
  theme(text = element_text(size = 12), 
        legend.position = c(1, -1/2), legend.justification = c(1, 0), 
        legend.background = element_rect(fill = "white", colour = NA))



base_exp_species <- (species.exp_ex_INTERVENTION/log(2)) %>% 
  as.data.frame() %>% 
  rownames_to_column("feature") %>% 
  
  left_join(., species.glmer_ex_INTERVENTION[,c("feature", "anovas.Legend_ex_INTERVENTION Pr(>F).BH", 
                                             "coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|)",
                                             "coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF Pr(>|t|)" )], by = c("feature" = "feature")) %>% 
  
  filter(`anovas.Legend_ex_INTERVENTION Pr(>F).BH` < 0.2) %>% 
  mutate(interaction = `coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|)` < 0.05 |
           `coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF Pr(>|t|)` < 0.05) %>% 
  dplyr::select(!c("anovas.Legend_ex_INTERVENTION Pr(>F).BH", 
                   "coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|)",
                   "coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF Pr(>|t|)" )) %>% 
  pivot_longer(!c(feature, interaction)) %>% 
  
  left_join(., meta_ex_INTERVENTION[,c("Legend_ex_INTERVENTION", "Treatment","R_ID", "participant_ID")], by = c("name" = "R_ID")) %>% 
  dplyr::select(!name) %>% 
  
  # group_by(participant_ID, Legend_ex_INTERVENTION) %>% 
  # mutate(individual_effect = mean(value, na.rm = TRUE)) %>% 
  # ungroup() %>% 
  # 
  # mutate(value = value - individual_effect) %>% 
  # dplyr::select(!individual_effect) %>% 
  # 
  pivot_wider(names_from = Legend_ex_INTERVENTION, values_from = c(value)) %>% 
  
  group_by(feature) %>% 
  mutate(baseline = mean(`Pre-Intervention (V3)`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  mutate(`Pre-Intervention (V3)`         = `Pre-Intervention (V3)`         - baseline,
         `Day 2 of intervention (T2I)`   = `Day 2 of intervention (T2I)`   - baseline, 
         `Day 4 of intervention (T4I)`   = `Day 4 of intervention (T4I)`   - baseline,
         `Day 14 of intervention (T14I)` = `Day 14 of intervention (T14I)` - baseline, 
         `Post-Intervention (V4)`        = `Post-Intervention (V4)`        - baseline) %>% 
  
  dplyr::select(!baseline) %>% 
  
  pivot_longer(!c(feature, Treatment, participant_ID, interaction))  



species_a <- base_exp_species %>% 
  group_by(feature, Treatment, name, interaction) %>% 
  reframe(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Treatment = Treatment, 
         participant_ID = "average") %>% 
  rbind(base_exp_species) %>% 
  
  rename("Timepoint" = name) %>% 
  # pivot_longer(!c(feature, participant_ID, Timepoint)) %>% 
  mutate(Timepoint = factor(Timepoint, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
                                                  "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
                                                  "Post-Intervention (V4)")))  %>% 
  
  
  
  #   group_by(feature, Treatment, name) %>%
  # 
  # reframe(value = mean(value, na.rm = TRUE)) %>%
  
  mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
  mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF", "avg_CAFF", "avg_DECAF"))) %>% 
  
  
  filter(!is.na(value)) %>% 
  
  filter(interaction) %>% 
  filter(participant_ID == "average") %>% 
  
  ggplot() +
  
  aes(x = Timepoint, y = feature, fill = value, label = round(value,2)) +
  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 3, show.legend = F) +
  
  
  scale_fill_gradientn(colours = c(
    "#053061", 
    "#2166ac", 
    "#4393c3",   
    "#f7f7f7",   
    "#d6604d",   
    "#d73027", 
    "#a50026"
  ),  limits =  c(-3,3), "log2 fold change vs Baseline") +
  scale_y_discrete(position = "right") +
  scale_x_discrete(labels = c("0", "2", "4", "14", "21")) +
  #facet_grid(feature ~ Treatment , scales = "free", switch = "y") +
  facet_grid( ~ Treatment, scales = "free") +
  theme_bw() + xlab(NULL) + ylab(NULL) +  
  ggtitle("Bacterial species altered following coffee reintroduction, depending on caffeine content") +
  theme(text = element_text(size = 12))

species_b <-  base_exp_species %>% 
  group_by(feature, Treatment, name, interaction) %>% 
  reframe(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Treatment = Treatment, 
         participant_ID = "average") %>% 
  rbind(base_exp_species) %>% 
  
  rename("Timepoint" = name) %>% 
  # pivot_longer(!c(feature, participant_ID, Timepoint)) %>% 
  mutate(Timepoint = factor(Timepoint, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
                                                  "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
                                                  "Post-Intervention (V4)")))  %>% 
  
  
  
  #   group_by(feature, Treatment, name) %>%
  # 
  # reframe(value = mean(value, na.rm = TRUE)) %>%
  
  mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
  mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF", "avg_CAFF", "avg_DECAF"))) %>% 
  
  
  filter(!is.na(value)) %>% 
  
  filter(!interaction) %>% 
  filter(participant_ID == "average") %>% 
  
  ggplot() +
  
  aes(x = Timepoint, y = feature, fill = value, label = round(value,2)) +
  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 3, show.legend = F) +
  
  
  scale_fill_gradientn(colours = c(
    "#053061", 
    "#2166ac", 
    "#4393c3",   
    "#f7f7f7",   
    "#d6604d",   
    "#d73027", 
    "#a50026"
  ),  limits =  c(-3,3), "log2 fold change vs Baseline") +
  scale_y_discrete(position = "right") +
  scale_x_discrete(labels = c("0", "2", "4", "14", "21")) +
  #facet_grid(feature ~ Treatment , scales = "free", switch = "y") +
  facet_grid( ~ Treatment, scales = "free") +
  theme_bw() + xlab(NULL) + ylab(NULL) +  
  ggtitle("Bacterial species altered following coffee reintroduction") +
  theme(text = element_text(size = 12))



GBMs.glmer_ex_INTERVENTION <- fw_glmer(x = GBMs.exp_ex_INTERVENTION, 
                                   f = ~ Legend_ex_INTERVENTION * Treatment + (1|participant_ID), 
                                   metadata = meta_ex_INTERVENTION, 
                                   order = "ac", verbose = FALSE) 

base_exp_GBM <- (GBMs.exp_ex_INTERVENTION/log(2)) %>% 
  as.data.frame() %>% 
  rownames_to_column("feature") %>% 
  
  left_join(., GBMs.glmer_ex_INTERVENTION[,c("feature", "anovas.Legend_ex_INTERVENTION Pr(>F).BH", 
                                             "coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|)",
                                             "coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF Pr(>|t|)" )], by = c("feature" = "feature")) %>% 
  
  filter(`anovas.Legend_ex_INTERVENTION Pr(>F).BH` < 0.2) %>% 
  mutate(interaction = `coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|)` < 0.05 |
           `coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF Pr(>|t|)` < 0.05) %>% 
  dplyr::select(!c("anovas.Legend_ex_INTERVENTION Pr(>F).BH", 
                   "coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|)",
                   "coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF Pr(>|t|)" )) %>% 
  pivot_longer(!c(feature, interaction)) %>% 
  
  left_join(., meta_ex_INTERVENTION[,c("Legend_ex_INTERVENTION", "Treatment","R_ID", "participant_ID")], by = c("name" = "R_ID")) %>% 
  dplyr::select(!name) %>% 
  
  # group_by(participant_ID, Legend_ex_INTERVENTION) %>% 
  # mutate(individual_effect = mean(value, na.rm = TRUE)) %>% 
  # ungroup() %>% 
  # 
  # mutate(value = value - individual_effect) %>% 
  # dplyr::select(!individual_effect) %>% 
  # 
  pivot_wider(names_from = Legend_ex_INTERVENTION, values_from = c(value)) %>% 
  
  group_by(feature) %>% 
  mutate(baseline = mean(`Pre-Intervention (V3)`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  mutate(`Pre-Intervention (V3)`         = `Pre-Intervention (V3)`         - baseline,
         `Day 2 of intervention (T2I)`   = `Day 2 of intervention (T2I)`   - baseline, 
         `Day 4 of intervention (T4I)`   = `Day 4 of intervention (T4I)`   - baseline,
         `Day 14 of intervention (T14I)` = `Day 14 of intervention (T14I)` - baseline, 
         `Post-Intervention (V4)`        = `Post-Intervention (V4)`        - baseline) %>% 
  
  dplyr::select(!baseline) %>% 
  
  pivot_longer(!c(feature, Treatment, participant_ID, interaction))  



GBM_a <- base_exp_GBM %>% 
  group_by(feature, Treatment, name, interaction) %>% 
  reframe(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Treatment = Treatment, 
         participant_ID = "average") %>% 
  rbind(base_exp_species) %>% 
  
  rename("Timepoint" = name) %>% 
  # pivot_longer(!c(feature, participant_ID, Timepoint)) %>% 
  mutate(Timepoint = factor(Timepoint, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
                                                  "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
                                                  "Post-Intervention (V4)")))  %>% 
  
  
  
  #   group_by(feature, Treatment, name) %>%
  # 
  # reframe(value = mean(value, na.rm = TRUE)) %>%
  
  mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
  mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF", "avg_CAFF", "avg_DECAF"))) %>% 
  
  
  filter(!is.na(value)) %>% 
  
  filter(interaction) %>% 
  filter(participant_ID == "average") %>% 
  
  ggplot() +
  
  aes(x = Timepoint, y = feature, fill = value, label = round(value,2)) +
  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 3, show.legend = F) +
  
  
  scale_fill_gradientn(colours = c(
    "#053061", 
    "#2166ac", 
    "#4393c3",   
    "#f7f7f7",   
    "#d6604d",   
    "#d73027", 
    "#a50026"
  ),  limits =  c(-3,3), "log2 fold change vs Baseline") +
  scale_y_discrete(position = "right") +
  scale_x_discrete(labels = c("0", "2", "4", "14", "21")) +
  #facet_grid(feature ~ Treatment , scales = "free", switch = "y") +
  facet_grid( ~ Treatment, scales = "free") +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  ggtitle("Gut-Brain modules altered following coffee reintroduction, depending on caffeine content") +
  theme(text = element_text(size = 12))


GBM_b <- base_exp_GBM %>% 
  group_by(feature, Treatment, name, interaction) %>% 
  reframe(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Treatment = Treatment, 
         participant_ID = "average") %>% 
  rbind(base_exp_species) %>% 
  
  rename("Timepoint" = name) %>% 
  # pivot_longer(!c(feature, participant_ID, Timepoint)) %>% 
  mutate(Timepoint = factor(Timepoint, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
                                                  "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
                                                  "Post-Intervention (V4)")))  %>% 
  
  
  
  #   group_by(feature, Treatment, name) %>%
  # 
  # reframe(value = mean(value, na.rm = TRUE)) %>%
  
  mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
  mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF", "avg_CAFF", "avg_DECAF"))) %>% 
  
  
  filter(!is.na(value)) %>% 
  
  filter(!interaction) %>% 
  filter(participant_ID == "average") %>% 
  
  ggplot() +
  
  aes(x = Timepoint, y = feature, fill = value, label = round(value,2)) +
  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 3, show.legend = F) +
  
  
  scale_fill_gradientn(colours = c(
    "#053061", 
    "#2166ac", 
    "#4393c3",   
    "#f7f7f7",   
    "#d6604d",   
    "#d73027", 
    "#a50026"
  ),  limits =  c(-3,3), "log2 fold change vs Baseline") +
  scale_y_discrete(position = "right") +
  scale_x_discrete(labels = c("0", "2", "4", "14", "21")) +
  #facet_grid(feature ~ Treatment , scales = "free", switch = "y") +
  facet_grid( ~ Treatment, scales = "free") +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  ggtitle("Gut-Brain modules altered following coffee reintroduction") +
  theme(text = element_text(size = 12))


#GBM_b  + plot_spacer() + GBM_a + plot_layout(guides = 'collect', widths = c(5,1,5))

GMMs.glmer_ex_INTERVENTION <- fw_glmer(x = GMMs.exp_ex_INTERVENTION, 
                                       f = ~ Legend_ex_INTERVENTION * Treatment + (1|participant_ID), 
                                       metadata = meta_ex_INTERVENTION, 
                                       order = "ac") 


base_exp_GMM <- (GMMs.exp_ex_INTERVENTION/log(2)) %>% 
  as.data.frame() %>% 
  rownames_to_column("feature") %>% 
  
  left_join(., GMMs.glmer_ex_INTERVENTION[,c("feature", "anovas.Legend_ex_INTERVENTION Pr(>F).BH", 
                                             "coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|)",
                                             "coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF Pr(>|t|)" )], by = c("feature" = "feature")) %>% 
  
  filter(`anovas.Legend_ex_INTERVENTION Pr(>F).BH` < 0.2) %>% 
  mutate(interaction = `coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|)` < 0.05 |
           `coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF Pr(>|t|)` < 0.05) %>% 
  dplyr::select(!c("anovas.Legend_ex_INTERVENTION Pr(>F).BH", 
                   "coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|)",
                   "coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF Pr(>|t|)" )) %>% 
  pivot_longer(!c(feature, interaction)) %>% 
  
  left_join(., meta_ex_INTERVENTION[,c("Legend_ex_INTERVENTION", "Treatment","R_ID", "participant_ID")], by = c("name" = "R_ID")) %>% 
  dplyr::select(!name) %>% 
  
  # group_by(participant_ID, Legend_ex_INTERVENTION) %>% 
  # mutate(individual_effect = mean(value, na.rm = TRUE)) %>% 
  # ungroup() %>% 
  # 
  # mutate(value = value - individual_effect) %>% 
  # dplyr::select(!individual_effect) %>% 
  # 
  pivot_wider(names_from = Legend_ex_INTERVENTION, values_from = c(value)) %>% 
  
  group_by(feature) %>% 
  mutate(baseline = mean(`Pre-Intervention (V3)`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  mutate(`Pre-Intervention (V3)`         = `Pre-Intervention (V3)`         - baseline,
         `Day 2 of intervention (T2I)`   = `Day 2 of intervention (T2I)`   - baseline, 
         `Day 4 of intervention (T4I)`   = `Day 4 of intervention (T4I)`   - baseline,
         `Day 14 of intervention (T14I)` = `Day 14 of intervention (T14I)` - baseline, 
         `Post-Intervention (V4)`        = `Post-Intervention (V4)`        - baseline) %>% 
  
  dplyr::select(!baseline) %>% 
  
  pivot_longer(!c(feature, Treatment, participant_ID, interaction))  



GMM_a <- base_exp_GMM %>% 
  group_by(feature, Treatment, name, interaction) %>% 
  reframe(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Treatment = Treatment, 
         participant_ID = "average") %>% 
  rbind(base_exp_species) %>% 
  
  rename("Timepoint" = name) %>% 
  # pivot_longer(!c(feature, participant_ID, Timepoint)) %>% 
  mutate(Timepoint = factor(Timepoint, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
                                                  "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
                                                  "Post-Intervention (V4)")))  %>% 
  
  
  
  #   group_by(feature, Treatment, name) %>%
  # 
  # reframe(value = mean(value, na.rm = TRUE)) %>%
  
  mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
  mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF", "avg_CAFF", "avg_DECAF"))) %>% 
  
  
  filter(!is.na(value)) %>% 
  
  filter(interaction) %>% 
  filter(participant_ID == "average") %>% 
  
  ggplot() +
  
  aes(x = Timepoint, y = feature, fill = value, label = round(value,2)) +
  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 3, show.legend = F) +
  
  
  scale_fill_gradientn(colours = c(
    "#053061", 
    "#2166ac", 
    "#4393c3",   
    "#f7f7f7",   
    "#d6604d",   
    "#d73027", 
    "#a50026"
  ),  limits =  c(-3,3), "log2 fold change vs Baseline") +
  scale_y_discrete(position = "right") +
  scale_x_discrete(labels = c("0", "2", "4", "14", "21")) +
  #facet_grid(feature ~ Treatment , scales = "free", switch = "y") +
  facet_grid( ~ Treatment, scales = "free") +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  ggtitle("Gut-Metabolic modules altered following coffee reintroduction, depending on caffeine content") +
  theme(text = element_text(size = 12))


GMM_b <- base_exp_GMM %>% 
  group_by(feature, Treatment, name, interaction) %>% 
  reframe(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Treatment = Treatment, 
         participant_ID = "average") %>% 
  rbind(base_exp_species) %>% 
  
  rename("Timepoint" = name) %>% 
  # pivot_longer(!c(feature, participant_ID, Timepoint)) %>% 
  mutate(Timepoint = factor(Timepoint, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
                                                  "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
                                                  "Post-Intervention (V4)")))  %>% 
  
  
  
  #   group_by(feature, Treatment, name) %>%
  # 
  # reframe(value = mean(value, na.rm = TRUE)) %>%
  
  mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
  mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF", "avg_CAFF", "avg_DECAF"))) %>% 
  
  
  filter(!is.na(value)) %>% 
  
  filter(!interaction) %>% 
  filter(participant_ID == "average") %>% 
  
  ggplot() +
  
  aes(x = Timepoint, y = feature, fill = value, label = round(value,2)) +
  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
  
  geom_tile() +
  geom_text(colour = "black", size = 3, show.legend = F) +
  
  
  scale_fill_gradientn(colours = c(
    "#053061", 
    "#2166ac", 
    "#4393c3",   
    "#f7f7f7",   
    "#d6604d",   
    "#d73027", 
    "#a50026"
  ),  limits =  c(-3,3), "log2 fold change vs Baseline") +
  scale_y_discrete(position = "right") +
  scale_x_discrete(labels = c("0", "2", "4", "14", "21")) +
  #facet_grid(feature ~ Treatment , scales = "free", switch = "y") +
  facet_grid( ~ Treatment, scales = "free") +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  ggtitle("Gut-Metabolic modules altered following coffee reintroduction") +
  theme(text = element_text(size = 12))




mb_a <- do.call(rbind, 
        list(
          base_exp_species %>% mutate(type = paste0("Species")), 
          base_exp_GBM     %>% mutate(type = paste0("GBM"  )), 
          base_exp_GMM     %>% mutate(type = paste0("GMM")))
        ) %>%   
  mutate(type = factor(type, levels = c("Species", "GBM", "GMM"))) %>% 
  group_by(feature, Treatment, name, interaction, type) %>% 
  reframe(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Treatment = Treatment, 
         participant_ID = "average") %>% 
  rename("Timepoint" = name) %>% 
  # pivot_longer(!c(feature, participant_ID, Timepoint)) %>% 
  mutate(Timepoint = factor(Timepoint, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
                                                  "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
                                                  "Post-Intervention (V4)")))  %>% 
  
  mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
  mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF"))) %>% 
  
  
  filter(!is.na(value)) %>% 
  
  filter(participant_ID == "average") %>% 
  filter(interaction) %>% 
  ggplot() +
  
  aes(x = Timepoint, y = feature, fill = value, label = round(value,2)) +
  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
  
  geom_tile() +
  #geom_text(colour = "black", size = 3, show.legend = F) +
  
  
  scale_fill_gradientn(colours = c(
    "#053061", 
    "#2166ac", 
    "#4393c3",   
    "#f7f7f7",   
    "#d6604d",   
    "#d73027", 
    "#a50026"
  ),  limits =  c(-3,3), "log2 fold change vs Baseline") +
  scale_y_discrete(position = "right") +
  scale_x_discrete(labels = c("0", "2", "4", "14", "21")) +
  #facet_grid(feature ~ Treatment , scales = "free", switch = "y") +
  facet_grid(type ~  Treatment, scales = "free", space = "free") +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  ggtitle("Microbiome features altered following\n coffee reintroduction in a caffeine-dependent manner") +
  theme(text = element_text(size = 12))
  

mb_b <- do.call(rbind, 
                list(
                  base_exp_species %>% mutate(type = paste0("Species")), 
                  base_exp_GBM     %>% mutate(type = paste0("GBM"  )), 
                  base_exp_GMM     %>% mutate(type = paste0("GMM")))
) %>%   
  mutate(type = factor(type, levels = c("Species", "GBM", "GMM"))) %>% 
  group_by(feature, Treatment, name, interaction, type) %>% 
  reframe(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Treatment = Treatment, 
         participant_ID = "average") %>% 
  rename("Timepoint" = name) %>% 
  # pivot_longer(!c(feature, participant_ID, Timepoint)) %>% 
  mutate(Timepoint = factor(Timepoint, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
                                                  "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
                                                  "Post-Intervention (V4)")))  %>% 
  
  mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
  mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF"))) %>% 
  
  
  filter(!is.na(value)) %>% 
  
  filter(participant_ID == "average") %>% 
  filter(!interaction) %>% 
  ggplot() +
  
  aes(x = Timepoint, y = feature, fill = value, label = round(value,2)) +
  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
  
  geom_tile() +
  #geom_text(colour = "black", size = 3, show.legend = F) +
  
  
  scale_fill_gradientn(colours = c(
    "#053061", 
    "#2166ac", 
    "#4393c3",   
    "#f7f7f7",   
    "#d6604d",   
    "#d73027", 
    "#a50026"
  ),  limits =  c(-3,3), "log2 fold change vs Baseline") +
  scale_y_discrete(position = "right") +
  scale_x_discrete(labels = c("0", "2", "4", "14", "21")) +
  #facet_grid(feature ~ Treatment , scales = "free", switch = "y") +
  facet_grid(type ~  Treatment, scales = "free", space = "free") +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  ggtitle("Microbiome features altered following\n coffee reintroduction independently of caffeine") +
  theme(text = element_text(size = 12))


#####Coffee vs non-coffee
meta_ex3 <- metadata[metadata$coffee_group == "CD" & metadata$visit %in% c("V3", "V4"),]

meta_ex3$Legend_ex3 = factor(meta_ex3$Legend_ex3, levels = c("Pre-Intervention (CAFF)","Post-Intervention (CAFF)",
                                                             "Pre-Intervention (DECAF)","Post-Intervention (DECAF)"))

metabs.exp_ex3  <- metabs.exp[,meta_ex3$Metab_ID]


metab.glmer_ex3 <- fw_glmer(x = metabs.exp_ex3, 
                            f = ~ visit * Treatment + (1|participant_ID), 
                            metadata = meta_ex3, 
                            order = "ac", verbose = FALSE) 

metab_BH_ex3 <- metabs.exp_ex3[metab.glmer_ex3[metab.glmer_ex3$`anovas.visit:Treatment Pr(>F).BH`< 0.2,"feature"],]

ex3metab <- metab_BH_ex3 %>%
  t() %>%
  as.data.frame() %>%
  add_column(Legend    = meta_ex3$Legend_ex3,
             Treatment = meta_ex3$Treatment)  %>%
  pivot_longer(!c("Legend", "Treatment"))  %>%
  left_join(., metab_trans, by = c("name" = "Compound_ID")) %>% 
  
  mutate(name = str_replace(name, ".*ales_", "")) %>% 
  ggplot(aes(x     = Legend, 
             y     = value, 
             fill  = Legend, 
             group = Legend, 
             shape = Treatment)) + 
  
  geom_boxplot(alpha = 1/2, coef = Inf, show.legend = F) +
  geom_point(colour = "black") + 
  
  #Adjust appearance
  scale_fill_manual(values = c("Pre-Intervention (CAFF)" = "#ffa0a0", 
                               "Post-Intervention (CAFF)" = "#5757f9", 
                               "Pre-Intervention (DECAF)" = "#ffa0a0", 
                               "Post-Intervention (DECAF)" = "#5757f9")) +
  scale_x_discrete(labels = c("Pre",  "Post", "Pre", "Post")) +
  scale_shape_manual(values = c("CAFF" = 21, "DECAF" = 22))+
  guides(shape = FALSE, fill = guide_legend(override.aes = list(shape = c(21,21,22,22)))) +
  facet_wrap(Name ~ ., scales = "free",ncol = 2) +
  ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12)) +
  ggtitle("Metabolites differentially affected by caffeine")
ex3metab

(mb_b | (mb_a / ex3metab)) + plot_layout(guides = 'collect', heights = c(1,2))



#GMM_b  + plot_spacer() + GMM_a + plot_layout(guides = 'collect', widths = c(5,1,5))

# hist(GBMs.glmer_ex_INTERVENTION$`anovas.Legend_ex_INTERVENTION Pr(>F).BH`, breaks = 20)
# hist(GBMs.glmer_ex_INTERVENTION$`anovas.Legend_ex_INTERVENTION:Treatment Pr(>F).BH`, breaks = 20)
# 
# GBMs_BH_ex_INTERVENTION <- GBMs.exp_ex_INTERVENTION[GBMs.glmer_ex_INTERVENTION[GBMs.glmer_ex_INTERVENTION$`coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|).BH`< 0.2,"feature"],]
# 
# base_ex_int <- (GBMs.exp_ex_INTERVENTION/log(2)) %>% 
#   as.data.frame() %>% 
#   rownames_to_column("feature") %>% 
# 
#   left_join(., GBMs.glmer_ex_INTERVENTION[,c("feature", "anovas.Legend_ex_INTERVENTION Pr(>F).BH")], by = c("feature" = "feature")) %>% 
#   
#   filter(`anovas.Legend_ex_INTERVENTION Pr(>F).BH` < 0.2) %>% 
#   dplyr::select(!"anovas.Legend_ex_INTERVENTION Pr(>F).BH") %>% 
#   pivot_longer(!feature) %>% 
#   
#   left_join(., meta_ex_INTERVENTION[,c("Legend_ex_INTERVENTION", "Treatment","R_ID", "participant_ID")], by = c("name" = "R_ID")) %>% 
#   dplyr::select(!name) %>% 
#   
#   # group_by(participant_ID, Legend_ex_INTERVENTION) %>% 
#   # mutate(individual_effect = mean(value, na.rm = TRUE)) %>% 
#   # ungroup() %>% 
#   # 
#   # mutate(value = value - individual_effect) %>% 
#   # dplyr::select(!individual_effect) %>% 
#   # 
#   pivot_wider(names_from = Legend_ex_INTERVENTION, values_from = c(value)) %>% 
# 
#   group_by(feature) %>% 
#   mutate(baseline = mean(`Pre-Intervention (V3)`, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   
#   mutate(`Pre-Intervention (V3)`         = `Pre-Intervention (V3)`         - baseline,
#          `Day 2 of intervention (T2I)`   = `Day 2 of intervention (T2I)`   - baseline, 
#          `Day 4 of intervention (T4I)`   = `Day 4 of intervention (T4I)`   - baseline,
#          `Day 14 of intervention (T14I)` = `Day 14 of intervention (T14I)` - baseline, 
#          `Post-Intervention (V4)`        = `Post-Intervention (V4)`        - baseline) %>% 
#   
#   dplyr::select(!baseline) %>% 
#   
#   pivot_longer(!c(feature, Treatment, participant_ID)) %>% 
#   mutate(name = factor(name, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", 
#                                         "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", 
#                                         "Post-Intervention (V4)")))  %>% 
#   
#   
#   
# #   group_by(feature, Treatment, name) %>%
# # 
# # reframe(value = mean(value, na.rm = TRUE)) %>%
# 
#   mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
#   filter(!is.na(value))
#   
#   ex_hm_caf_tot = base_ex_int %>% 
#     
#       group_by(feature, Treatment, name) %>%
# 
#     reframe(value = mean(value, na.rm = TRUE)) %>%
#     
#     filter(Treatment == "CAFF") %>% 
# 
#   ggplot() +
#   
#   aes(x = name, y = Treatment, fill = value,label = round(value,2)) +
#  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
# 
#   geom_tile(colour = "black") +
#   geom_text(colour = "black") +
#   
#   
#   scale_fill_gradientn(colours = c(
#     "#053061", "#053061","#053061", 
#     "#2166ac", "#2166ac",
#     "#4393c3",    "#4393c3",
#     "#f7f7f7",   "#f7f7f7",  
#     "#d6604d",    "#d6604d",
#     "#d73027", "#d73027",
#     "#a50026", "#a50026", "#a50026"
#   ),  limits =  c(-5.25, 5.25), "log2 fold change vs Baseline") +
#   scale_y_discrete(position = "right") +
#   scale_x_discrete(labels = c("0", "2", "4", "14", "21"))+
#   facet_grid(Treatment ~ feature, scales = "free") +
#   #ggh4x::facet_grid2( ~ Treatment, scales = "free", independent = "x") +
#  # theme_bw() + xlab("Time since start of intervention (days)") + ylab(NULL) +  
#     theme_bw() + xlab(NULL) + ylab(NULL) + 
#     theme(text = element_text(size = 12), 
#           axis.ticks.x = element_blank(), 
#           axis.text.x = element_blank())
# 
#   
# ex_hm_caf_ind = base_ex_int %>% 
#     
#     filter(Treatment == "CAFF") %>% 
#     
#     ggplot() +
#     
#     aes(x = name, y = participant_ID, fill = value,label = round(value,2)) +
#     # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
#     
#     geom_tile(colour = "black") +
#     geom_text(colour = "black") +
#     
#     
#     scale_fill_gradientn(colours = c(
#       "#053061", "#053061","#053061", 
#       "#2166ac", "#2166ac",
#       "#4393c3",    "#4393c3",
#       "#f7f7f7",   "#f7f7f7",  
#       "#d6604d",    "#d6604d",
#       "#d73027", "#d73027",
#       "#a50026", "#a50026", "#a50026"
#     ),  limits =  c(-5.25, 5.25), "log2 fold change vs Baseline") +
#     scale_y_discrete(position = "right") +
#     #scale_x_discrete(labels = c("0", "2", "4", "14", "21"))+
#     facet_grid(Treatment ~ feature, scales = "free") +
#     #ggh4x::facet_grid2( ~ Treatment, scales = "free", independent = "x") +
#     theme_bw() + xlab(NULL) + ylab(NULL) + 
#     theme(text = element_text(size = 12), 
#           axis.ticks = element_blank(), 
#           axis.text = element_blank(), 
#           strip.background = element_blank(), 
#           strip.text = element_blank())
#   
# 
# 
# ex_hm_decaf_tot = base_ex_int %>% 
#   
#   group_by(feature, Treatment, name) %>%
#   
#   reframe(value = mean(value, na.rm = TRUE)) %>%
#   
#   filter(Treatment == "DECAF") %>% 
#   
#   ggplot() +
#   
#   aes(x = name, y = Treatment, fill = value,label = round(value,2)) +
#   # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
#   
#   geom_tile(colour = "black") +
#   geom_text(colour = "black") +
#   
#   
#   scale_fill_gradientn(colours = c(
#     "#053061", "#053061","#053061", 
#     "#2166ac", "#2166ac",
#     "#4393c3",    "#4393c3",
#     "#f7f7f7",   "#f7f7f7",  
#     "#d6604d",    "#d6604d",
#     "#d73027", "#d73027",
#     "#a50026", "#a50026", "#a50026"
#   ),  limits =  c(-5.25, 5.25), "log2 fold change vs Baseline") +
#   scale_y_discrete(position = "right") +
#   scale_x_discrete(labels = c("0", "2", "4", "14", "21"))+
#   facet_grid(Treatment ~ feature, scales = "free") +
#   #ggh4x::facet_grid2( ~ Treatment, scales = "free", independent = "x") +
#   # theme_bw() + xlab("Time since start of intervention (days)") + ylab(NULL) +  
#   theme_bw() + xlab(NULL) + ylab(NULL) + 
#   theme(text = element_text(size = 12), 
#         axis.ticks = element_blank(), 
#         axis.text = element_blank(), 
#         strip.background = element_blank(), 
#         strip.text = element_blank())
# 
# 
# ex_hm_decaf_ind = base_ex_int %>% 
#   
#   filter(Treatment == "DECAF") %>% 
#   
#   ggplot() +
#   
#   aes(x = name, y = participant_ID, fill = value,label = round(value,2)) +
#   # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
#   
#   geom_tile(colour = "black") +
#   geom_text(colour = "black") +
#   
#   
#   scale_fill_gradientn(colours = c(
#     "#053061", "#053061","#053061", 
#     "#2166ac", "#2166ac",
#     "#4393c3",    "#4393c3",
#     "#f7f7f7",   "#f7f7f7",  
#     "#d6604d",    "#d6604d",
#     "#d73027", "#d73027",
#     "#a50026", "#a50026", "#a50026"
#   ),  limits =  c(-5.25, 5.25), "log2 fold change vs Baseline") +
#   scale_y_discrete(position = "right") +
#   #scale_x_discrete(labels = c("0", "2", "4", "14", "21"))+
#   facet_grid(Treatment ~ feature, scales = "free") +
#   #ggh4x::facet_grid2( ~ Treatment, scales = "free", independent = "x") +
#   theme_bw() + xlab(NULL) + ylab(NULL) + 
#   theme(text = element_text(size = 12), 
#         axis.ticks = element_blank(), 
#         axis.text = element_blank(), 
#         strip.background = element_blank(), 
#         strip.text = element_blank())

#  ex_hm_caf_tot / ex_hm_caf_ind /  ex_hm_decaf_tot / ex_hm_decaf_ind + plot_layout(guides = 'collect', heights = c(1,3, 1, 3))
  
  
  






  

# 
# GBMs.glmer_ex_INTERVENTION %>%
#   as.data.frame() %>%
#   # rownames_to_column("name") %>% 
# 
#   filter(`coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|).BH`< 0.2) %>% 
#   dplyr::select(!contains('anovas')) %>% 
#   dplyr::select(!contains('Intercept')) %>% #colnames()
#   pivot_longer(c(`coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF 97.5 %`,  `coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF 97.5 %`,  `coefs.Legend_ex_INTERVENTIONDay 14 of intervention (T14I):TreatmentDECAF 97.5 %`,  `coefs.Legend_ex_INTERVENTIONPost-Intervention (V4):TreatmentDECAF 97.5 %`,
#                  `coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I) 97.5 %`,                 `coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I) 97.5 %`,                 `coefs.Legend_ex_INTERVENTIONDay 14 of intervention (T14I) 97.5 %`,                 `coefs.Legend_ex_INTERVENTIONPost-Intervention (V4) 97.5 %`,
#                  `coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF 2.5 %`,   `coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF 2.5 %`,   `coefs.Legend_ex_INTERVENTIONDay 14 of intervention (T14I):TreatmentDECAF 2.5 %`,   `coefs.Legend_ex_INTERVENTIONPost-Intervention (V4):TreatmentDECAF 2.5 %`,
#                  `coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I) 2.5 %`,                  `coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I) 2.5 %`,                  `coefs.Legend_ex_INTERVENTIONDay 14 of intervention (T14I) 2.5 %`,                  `coefs.Legend_ex_INTERVENTIONPost-Intervention (V4) 2.5 %`,
#                  `coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Estimate`,`coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I):TreatmentDECAF Estimate`,`coefs.Legend_ex_INTERVENTIONDay 14 of intervention (T14I):TreatmentDECAF Estimate`,`coefs.Legend_ex_INTERVENTIONPost-Intervention (V4):TreatmentDECAF Estimate`, 
#                  `coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I) Estimate`,               `coefs.Legend_ex_INTERVENTIONDay 4 of intervention (T4I) Estimate`,               `coefs.Legend_ex_INTERVENTIONDay 14 of intervention (T14I) Estimate`,               `coefs.Legend_ex_INTERVENTIONPost-Intervention (V4) Estimate`)) %>% 
#   mutate(Legend = case_when(grepl("T2I",  name) ~ "vs day 2", 
#                             grepl("T4I",  name) ~ "vs day 4",
#                             grepl("T14I", name) ~ "vs day 14",
#                             grepl("V4",   name) ~ "vs day 21")) %>%    
# 
#   
#   mutate(name = str_replace(name, pattern = ".*\\) ", replacement = "CAFF@"),
#          name = str_replace(name, pattern = ".*TreatmentDECAF ", replacement = "DECAF@"),
#          name = str_remove(name, pattern = "_.*")) %>%  
#   
#   separate(name, sep = "@", into = c("Treatment", "name")) %>% 
#   
#   pivot_wider(names_from = name, values_from = value) %>% 
#     
#   mutate(`2.5 %`  = `2.5 %`  * -1) %>% 
#   mutate(`97.5 %` = `97.5 %` * -1) %>% 
#   mutate(Estimate = Estimate * -1) %>% 
#   arrange(Estimate)  %>% 
#   mutate(Legend = factor(Legend, levels = rev(c("vs day 2","vs day 4","vs day 14","vs day 21")))) %>%   #
#   
#   mutate(feature = factor(feature, levels = unique(feature))) %>% 
#   dplyr::select(!contains('coef')) %>%  
#   pivot_wider(names_from = Treatment, values_from = c(`2.5 %`, `97.5 %`, `Estimate`)) %>% 
#   #mutate(ci_CAFF =  `97.5 %_CAFF` - `2.5 %_CAFF` )
#   mutate(Estimate_DECAF = Estimate_CAFF + Estimate_DECAF) %>% 
#   
#   pivot_longer(!c(feature, Legend)) %>% 
#   separate(name, sep = "_", into = c("name", "Treatment")) %>% 
#   
#   pivot_wider(names_from = name, values_from = value) %>% 
#   
# 
#   # filter(Plot_category %in% c("Bile acids", "Coffee-associated compounds", 
#   #                             "Neuroactive compounds & derivatives", "Phytochemical compounds")) %>% 
#   # mutate(Plot_category = factor(Plot_category,  levels = c("Coffee-associated compounds", "Neuroactive compounds & derivatives",
#   #                                                          "Bile acids", "Phytochemical compounds"))) %>% 
#   ggplot() +
#   
#   aes(y = Estimate/log(2), 
#       x = feature, 
#       group = Legend, 
#       fill = Legend) +
#   
#   geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
#   
#   geom_errorbar(aes(ymin = `2.5 %`/log(2), 
#                     ymax = `97.5 %`/log(2)), 
#                 colour = "black", width = 3/4 , position = position_dodge(1/3)) +
#   
#   geom_point(size = 3, shape = 21, position = position_dodge(1/3)) +
#   coord_flip() +
#   scale_fill_manual(values = c("vs day 2"  = "#d9f0a3",
#                                "vs day 4"  = "#78c679",
#                                "vs day 14" = "#006837", 
#                                "vs day 21" = "#004529"), limits = rev) +
#   scale_x_discrete(position = "top", limits=rev) +
#   facet_wrap(~Treatment) +
#   #ggforce::facet_col(~Plot_category, strip.position = "top", space = "free", scale = "free_y") +
#   xlab(NULL) +
#   ylab(NULL) +
#   theme_bw() 
# ex_INTERVENTIONDA_GBM <- GBMs_BH_ex_INTERVENTION %>%
#   t() %>%
#   as.data.frame() %>%
#   add_column(Legend = meta_ex_INTERVENTION$Legend_ex_INTERVENTION, 
#              Treatment = meta_ex_INTERVENTION$Treatment)  %>%
#   pivot_longer(!c("Legend", Treatment))  %>%
#   mutate(name = str_replace(name, ".*ales_", "")) %>% 
#   ggplot(aes(x     = Legend, 
#              y     = value, 
#              fill  = Legend, 
#              group = Legend)) + 
#   
#   geom_boxplot(alpha = 1/2, coef = 100) +
#   geom_beeswarm(size = 3, cex = 3, shape = 21) + 
#   
#   #Adjust appearance
#   scale_fill_manual(values = c("Pre-Intervention (V3)"        = "#ffffcc", 
#                                "Day 2 of intervention (T2I)"   = "#d9f0a3",
#                                "Day 4 of intervention (T4I)"   = "#78c679",
#                                "Day 14 of intervention (T14I)" = "#006837", 
#                                "Post-Intervention (V4)"        = "#004529")) +
#   
#   
#   ggh4x::facet_nested(name ~ Treatment, scales = "free") +
#   ylab("") + xlab("") + theme_bw()  + 
#   theme(text = element_text(size = 12), axis.text.x = element_text(angle = 330, hjust = 0))

#ex_INTERVENTIONDA_GBM


# #hist(GMMs.glmer_ex_INTERVENTION$`anovas.Legend_ex_INTERVENTION Pr(>F).BH`, breaks = 20)
# hist(GMMs.glmer_ex_INTERVENTION$`anovas.Legend_ex_INTERVENTION:Treatment Pr(>F)`, breaks = 20)
# 
# GMM_BH_ex_INTERVENTION <- GMMs.exp_ex_INTERVENTION[GMMs.glmer_ex_INTERVENTION[GMMs.glmer_ex_INTERVENTION$`coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|).BH`< 0.2,"feature"],]
# 
# ex_INTERVENTIONDA_GMM <- GMM_BH_ex_INTERVENTION %>%
#   t() %>%
#   as.data.frame() %>%
#   add_column(Legend = meta_ex_INTERVENTION$Legend_ex_INTERVENTION, 
#              Treatment = meta_ex_INTERVENTION$Treatment)  %>%
#   pivot_longer(!c("Legend", Treatment))  %>%
#   mutate(name = str_replace(name, ".*ales_", "")) %>% 
#   ggplot(aes(x     = Legend, 
#              y     = value, 
#              fill  = Legend, 
#              group = Legend)) + 
#   
#   geom_boxplot(alpha = 1/2, coef = 100) +
#   geom_beeswarm(size = 3, cex = 3, shape = 21) + 
#   
#   #Adjust appearance
#   scale_fill_manual(values = c("Pre-Intervention (V3)"        = "#ffffcc", 
#                                "Day 2 of intervention (T2I)"   = "#d9f0a3",
#                                "Day 4 of intervention (T4I)"   = "#78c679",
#                                "Day 14 of intervention (T14I)" = "#006837", 
#                                "Post-Intervention (V4)"        = "#004529")) +
#   
#   
#   ggh4x::facet_nested(name ~ Treatment, scales = "free_x") +
#   ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))



#hist(GMMs.glmer_ex_INTERVENTION$`anovas.visit Pr(>F).BH`, breaks = 20)



# ex_INTERVENTIONpca    %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_INTERVENTION/", "pca_beta_div", ".png"), 
#          device = "png", width = 6.42, height = 4.52, bg ="white")

# ex_INTERVENTIONbar    %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_INTERVENTION/", "stck_barplot", ".png"), 
#          device = "png", width = 16, height = 12, units = "in", bg ="white")
# 
# ex_INTERVENTIONDA     %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_INTERVENTION/", "diff_species", ".png"),
#          device = "png", width = 16, height = 8, bg ="white")
# 
#  ex_INTERVENTIONDA_GMM %>% 
#    ggsave(plot = ., filename = paste0("figures/ex_INTERVENTION/", "diff_GMMs",    ".png"),
#           device = "png", width = 8, height = 4.52, bg ="white")
# 
#  ex_INTERVENTIONDA_GBM %>% 
#    ggsave(plot = ., filename = paste0("figures/ex_INTERVENTION/", "diff_GBMs",    ".png"),
#           device = "png", width = 8, height = 4.52, bg ="white")
#  
#  
#  
# ex_INTERVENTIONalpha %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_INTERVENTION/", "alpha_div",    ".png"),
#          device = "png", width = 16, height = 4, bg ="white")


write.csv(species.glmer_ex_INTERVENTION,  file = "stats/ex_INTERVENTION/species_glmer.csv")
write.csv(GMMs.glmer_ex_INTERVENTION,     file = "stats/ex_INTERVENTION/GMMs_glmer.csv")
write.csv(GBMs.glmer_ex_INTERVENTION,     file = "stats/ex_INTERVENTION/GBMs_glmer.csv")
capture.output(ex_INTERVENTION_PERMANOVA, file = "stats/ex_INTERVENTION/PERMANOVA.txt")
