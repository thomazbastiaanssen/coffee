source("subscripts/load_and_clean_data.R")


#####Coffee vs non-coffee

meta_ex_INTERVENTION <- metadata[metadata$Legend_ex1 != "NCD" & metadata$visit %in% c("V3", "T2I", "T4I", "T14I", "V4"),]
meta_ex_INTERVENTION$Legend_ex_INTERVENTION = factor(meta_ex_INTERVENTION$Legend_ex_INTERVENTION, levels = c("Pre-Intervention (V3)", "Day 2 of intervention (T2I)", "Day 4 of intervention (T4I)", "Day 14 of intervention (T14I)", "Post-Intervention (V4)"))


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
  scale_fill_manual(values = c("Pre-Intervention (V3)"        = "#ffffcc", 
                               "Day 2 of intervention (T2I)"   = "#d9f0a3",
                               "Day 4 of intervention (T4I)"   = "#78c679",
                               "Day 14 of intervention (T14I)" = "#006837", 
                               "Post-Intervention (V4)"        = "#004529")) +
  
  facet_wrap(~Treatment) +
  #Adjust labels
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) + 
  theme_bw() 
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




ex_INTERVENTIONalpha <- (alpha_div_ex_INTERVENTION %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex_INTERVENTION, levels = levels(meta_ex_INTERVENTION$Legend_ex_INTERVENTION))) %>% 
  filter(name == "Chao1") %>% 
  
  ggplot() +
  
  aes(x = Legend, 
      y = value, 
      fill  = Legend, 
      group = Legend) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
  geom_beeswarm(size = 3, cex = 3, shape = 21) + 
  
  
    #Adjust appearance
    scale_fill_manual(values = c("Pre-Intervention (V3)"        = "#ffffcc", 
                                 "Day 2 of intervention (T2I)"   = "#d9f0a3",
                                 "Day 4 of intervention (T4I)"   = "#78c679",
                                 "Day 14 of intervention (T14I)" = "#006837", 
                                 "Post-Intervention (V4)"        = "#004529")) +
  
    ggh4x::facet_nested(name ~ Treatment, scales = "free_x") +
    ylab("") + xlab("") + theme_bw() + 
    theme(text = element_text(size = 12), axis.ticks.x = element_blank(), axis.text.x = element_blank())) /
  (alpha_div_ex_INTERVENTION %>% 
     pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
     mutate(Legend = factor(Legend_ex_INTERVENTION, levels = levels(meta_ex_INTERVENTION$Legend_ex_INTERVENTION))) %>% 
     filter(name == "Shannon") %>% 
     
     ggplot() +
     
     aes(x = Legend, 
         y = value, 
         fill  = Legend, 
         group = Legend) + 
     
     geom_boxplot(alpha = 1/2, coef = 100) +
     geom_beeswarm(size = 3, cex = 3, shape = 21) + 
     
     
     #Adjust appearance
     scale_fill_manual(values = c("Pre-Intervention (V3)"        = "#ffffcc", 
                                  "Day 2 of intervention (T2I)"   = "#d9f0a3",
                                  "Day 4 of intervention (T4I)"   = "#78c679",
                                  "Day 14 of intervention (T14I)" = "#006837", 
                                  "Post-Intervention (V4)"        = "#004529")) +
     
     ggh4x::facet_nested(name ~ Treatment, scales = "free_x") +
     ylab("") + xlab("") + theme_bw() + 
     theme(text = element_text(size = 12), axis.ticks.x = element_blank(), axis.text.x = element_blank())) /
  (alpha_div_ex_INTERVENTION %>% 
     pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
     mutate(Legend = factor(Legend_ex_INTERVENTION, levels = levels(meta_ex_INTERVENTION$Legend_ex_INTERVENTION))) %>% 
     filter(name == "Simpson") %>% 
     
     filter(value > 0.85) %>% 
     
     ggplot() +
     
     aes(x = Legend, 
         y = value, 
         fill  = Legend, 
         group = Legend) + 
     
     geom_boxplot(alpha = 1/2, coef = 100) +
     geom_beeswarm(size = 3, cex = 3, shape = 21) + 
     
     
     #Adjust appearance
     scale_fill_manual(values = c("Pre-Intervention (V3)"        = "#ffffcc", 
                                  "Day 2 of intervention (T2I)"   = "#d9f0a3",
                                  "Day 4 of intervention (T4I)"   = "#78c679",
                                  "Day 14 of intervention (T14I)" = "#006837", 
                                  "Post-Intervention (V4)"        = "#004529")) +
     
     ggh4x::facet_nested(name ~ Treatment, scales = "free_x") +
     ylab("") + xlab("") + theme_bw()  + 
     theme(text = element_text(size = 12), axis.text.x = element_text(angle = 345, hjust = 0))) +
  
  plot_layout(guides = 'collect')
  
#ex_INTERVENTIONalpha

alpha_div_ex_INTERVENTION %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex_INTERVENTION, levels = levels(meta_ex_INTERVENTION$Legend_ex_INTERVENTION))) %>% 
  
  group_by(name) %>% 
  
  summarise(
    
    lm(value ~ Legend * Treatment, data = cur_data()) %>% car::Anova() %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  
  write.csv(., "stats/ex_INTERVENTION/alpha_div_glms.csv")


species.exp_ex_INTERVENTION <- genefilter::varFilter(species.exp_ex_INTERVENTION, var.cutoff = 0.5)

species.glmer_ex_INTERVENTION <- fw_glmer(x = species.exp_ex_INTERVENTION, 
                                      f = ~ Legend_ex_INTERVENTION * Treatment + (1|participant_ID), 
                                      metadata = meta_ex_INTERVENTION, 
                                      order = "ac", verbose = FALSE) 


speBH_ex_INTERVENTION <- species.exp_ex_INTERVENTION[species.glmer_ex_INTERVENTION[species.glmer_ex_INTERVENTION$`anovas.Legend_ex_INTERVENTION:Treatment Pr(>F).BH`< 0.2,"feature"],] %>% data.frame

# ex_INTERVENTIONDA <- speBH_ex_INTERVENTION %>%
#   t() %>%
#   as.data.frame() %>%
#   add_column(Legend = meta_ex_INTERVENTION$Legend_ex_INTERVENTION)  %>%
#   pivot_longer(!c("Legend"))  %>%
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
#   facet_wrap(~name, scales = "free_y", ncol = 3) +
#   ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))
# 
# ex_INTERVENTIONDA

# hist(species.glmer_ex_INTERVENTION$`anovas.Legend_ex_INTERVENTION:Treatment Pr(>F)`, breaks = 20)

GBMs.glmer_ex_INTERVENTION <- fw_glmer(x = GBMs.exp_ex_INTERVENTION, 
                                   f = ~ Legend_ex_INTERVENTION * Treatment + (1|participant_ID), 
                                   metadata = meta_ex_INTERVENTION, 
                                   order = "ac", verbose = FALSE) 

#hist(GBMs.glmer_ex_INTERVENTION$`anovas.Legend_ex_INTERVENTION Pr(>F).BH`, breaks = 20)

GBMs_BH_ex_INTERVENTION <- GBMs.exp_ex_INTERVENTION[GBMs.glmer_ex_INTERVENTION[GBMs.glmer_ex_INTERVENTION$`coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|).BH`< 0.2,"feature"],]


ex_INTERVENTIONDA_GBM <- GBMs_BH_ex_INTERVENTION %>%
  t() %>%
  as.data.frame() %>%
  add_column(Legend = meta_ex_INTERVENTION$Legend_ex_INTERVENTION, 
             Treatment = meta_ex_INTERVENTION$Treatment)  %>%
  pivot_longer(!c("Legend", Treatment))  %>%
  mutate(name = str_replace(name, ".*ales_", "")) %>% 
  ggplot(aes(x     = Legend, 
             y     = value, 
             fill  = Legend, 
             group = Legend)) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
  geom_beeswarm(size = 3, cex = 3, shape = 21) + 
  
  #Adjust appearance
  scale_fill_manual(values = c("Pre-Intervention (V3)"        = "#ffffcc", 
                               "Day 2 of intervention (T2I)"   = "#d9f0a3",
                               "Day 4 of intervention (T4I)"   = "#78c679",
                               "Day 14 of intervention (T14I)" = "#006837", 
                               "Post-Intervention (V4)"        = "#004529")) +
  
  
  ggh4x::facet_nested(name ~ Treatment, scales = "free") +
  ylab("") + xlab("") + theme_bw()  + 
  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 330, hjust = 0))

#ex_INTERVENTIONDA_GBM

GMMs.glmer_ex_INTERVENTION <- fw_glmer(x = GMMs.exp_ex_INTERVENTION, 
                                   f = ~ Legend_ex_INTERVENTION * Treatment + (1|participant_ID), 
                                   metadata = meta_ex_INTERVENTION, 
                                   order = "ac") 

#hist(GMMs.glmer_ex_INTERVENTION$`anovas.Legend_ex_INTERVENTION Pr(>F).BH`, breaks = 20)

GMM_BH_ex_INTERVENTION <- GMMs.exp_ex_INTERVENTION[GMMs.glmer_ex_INTERVENTION[GMMs.glmer_ex_INTERVENTION$`coefs.Legend_ex_INTERVENTIONDay 2 of intervention (T2I):TreatmentDECAF Pr(>|t|).BH`< 0.2,"feature"],]

ex_INTERVENTIONDA_GMM <- GMM_BH_ex_INTERVENTION %>%
  t() %>%
  as.data.frame() %>%
  add_column(Legend = meta_ex_INTERVENTION$Legend_ex_INTERVENTION, 
             Treatment = meta_ex_INTERVENTION$Treatment)  %>%
  pivot_longer(!c("Legend", Treatment))  %>%
  mutate(name = str_replace(name, ".*ales_", "")) %>% 
  ggplot(aes(x     = Legend, 
             y     = value, 
             fill  = Legend, 
             group = Legend)) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
  geom_beeswarm(size = 3, cex = 3, shape = 21) + 
  
  #Adjust appearance
  scale_fill_manual(values = c("Pre-Intervention (V3)"        = "#ffffcc", 
                               "Day 2 of intervention (T2I)"   = "#d9f0a3",
                               "Day 4 of intervention (T4I)"   = "#78c679",
                               "Day 14 of intervention (T14I)" = "#006837", 
                               "Post-Intervention (V4)"        = "#004529")) +
  
  
  ggh4x::facet_nested(name ~ Treatment, scales = "free_x") +
  ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))



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
