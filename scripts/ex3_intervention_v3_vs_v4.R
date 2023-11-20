source("subscripts/load_and_clean_data.R")


#####Coffee vs non-coffee
meta_ex3 <- metadata[metadata$coffee_group == "CD" & metadata$visit %in% c("V3", "V4"),]

meta_ex3$Legend_ex3 = factor(meta_ex3$Legend_ex3, levels = c("Pre-Intervention (CAFF)","Post-Intervention (CAFF)",
                                                             "Pre-Intervention (DECAF)","Post-Intervention (DECAF)"))
species.exp_ex3 <- species.exp[,meta_ex3$R_ID]
GBMs.exp_ex3 <- GBMs.exp[,meta_ex3$R_ID]
GMMs.exp_ex3 <- GMMs.exp[,meta_ex3$R_ID]

metabs.exp_ex3  <- metabs.exp[,meta_ex3$Metab_ID]

alpha_div_ex3 <- alpha_div[meta_ex3$R_ID,]


#Apply the base R principal component analysis function on our CLR-transformed data.
data.a.pca  <- prcomp(t(species.exp_ex3))

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
pca$ID                  = meta_ex3$participant_ID
pca$Legend              = meta_ex3$Legend_ex3
pca$Treatment           = meta_ex3$Treatment
pca$visit               = meta_ex3$visit
pca$batch               = meta_ex3$batch

#First, the main plot. Plot the first two components of the PCA
ex3pca <- ggplot(pca) +
  
  aes(x       = PC1, 
      y       = PC2, 
      fill    = Legend,
      group   = visit) +
  
  #Create the points and ellipses
  #stat_ellipse(geom = "polygon", alpha = 1/4, colour = "black") +
  geom_line(aes(group = ID)) +
  geom_point(size = 3, col = "black", shape = 21) + 
  
  #Adjust appearance
  scale_fill_manual(values = c("Pre-Intervention (CAFF)" = "#ffa0a0", 
                               "Post-Intervention (CAFF)" = "#5757f9", 
                               "Pre-Intervention (DECAF)" = "#ffa0a0", 
                               "Post-Intervention (DECAF)" = "#5757f9")) +
  #Adjust labels
  facet_wrap(~Treatment) +
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) + 
  theme_bw() 
#ex3pca


dis_ait = dist(t(species.exp_ex3), method = "euclidean")



dis_ait %>% 
  as.matrix %>% 
  data.frame() %>% 
  rownames_to_column("sample1") %>% 
  pivot_longer(!sample1, names_to = "sample2") %>% 
  filter(sample1 != sample2) %>% 
  
  left_join(., meta_ex3 %>% 
              dplyr::select(c("R_ID", "participant_ID", "Legend_ex3")), 
            by = c("sample1" = "R_ID")) %>% 
  
  left_join(., meta_ex3 %>% 
              dplyr::select(c("R_ID", "participant_ID", "Legend_ex3", "Treatment")), 
            by = c("sample2" = "R_ID")) %>% 
  
  filter(participant_ID.x == participant_ID.y) %>% 
  
  dplyr::select(!c(sample1, sample2))  %>% 
  
  filter(grepl(x = as.character(Legend_ex3.x), pattern = "Pre") & grepl(x = as.character(Legend_ex3.y), pattern = "Post")) %>% 
  
  #lm(value ~ Treatment, data = .) %>% summary
  ggplot() +
  
  aes(x = Treatment, y = value) +
  geom_boxplot(coef = 10) +
  geom_point() +
  
  theme_bw()


ex3_PERMANOVA = adonis2(dis_ait ~ visit * Treatment, 
        data = meta_ex3, 
        method = "euclidean", 
        strata = meta_ex3$participant_ID,
        permutations = 10000)




ex3alpha <- (alpha_div_ex3 %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex3, levels = levels(meta_ex3$Legend_ex3))) %>% 
  mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF"))) %>% 
  filter(name == "Chao1") %>% 
  
  
  ggplot() +
  
  aes(x = Legend, 
      y = value, 
      fill  = Legend, 
      group = Legend) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
    geom_dotplot(dotsize = 1.5, binaxis = "y", stackdir = "center") + 
    
    #Adjust appearance
    scale_fill_manual(values = c("Pre-Intervention (CAFF)" = "#ffa0a0", 
                                 "Post-Intervention (CAFF)" = "#5757f9", 
                                 "Pre-Intervention (DECAF)" = "#ffa0a0", 
                                 "Post-Intervention (DECAF)" = "#5757f9")) +
  
  ggh4x::facet_nested(name ~ Treatment, scales = "free_x") +
  ylab("") + xlab("") + theme_bw() + 
    theme(text = element_text(size = 12), axis.ticks.x = element_blank(), axis.text.x = element_blank())) /
  (alpha_div_ex3 %>% 
     pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
     mutate(Legend = factor(Legend_ex3, levels = levels(meta_ex3$Legend_ex3))) %>% 
     mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF"))) %>% 
     filter(name == "Shannon") %>% 
     
     
     ggplot() +
     
     aes(x = Legend, 
         y = value, 
         fill  = Legend, 
         group = Legend) + 
     
     geom_boxplot(alpha = 1/2, coef = 100) +
     geom_dotplot(dotsize = 1.5, binaxis = "y", stackdir = "center") + 
     
     #Adjust appearance
     scale_fill_manual(values = c("Pre-Intervention (CAFF)" = "#ffa0a0", 
                                  "Post-Intervention (CAFF)" = "#5757f9", 
                                  "Pre-Intervention (DECAF)" = "#ffa0a0", 
                                  "Post-Intervention (DECAF)" = "#5757f9")) +
     
     ggh4x::facet_nested(name ~ Treatment, scales = "free_x") +
     ylab("") + xlab("") + theme_bw() + 
     theme(text = element_text(size = 12), axis.ticks.x = element_blank(), axis.text.x = element_blank())) /
  (alpha_div_ex3 %>% 
     pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
     mutate(Legend = factor(Legend_ex3, levels = levels(meta_ex3$Legend_ex3))) %>% 
     mutate(Treatment = factor(Treatment, levels = c("CAFF", "DECAF"))) %>% 
     filter(name == "Simpson") %>% 
     
     
     ggplot() +
     
     aes(x = Legend, 
         y = value, 
         fill  = Legend, 
         group = Legend) + 
     
     geom_boxplot(alpha = 1/2, coef = 100) +
     geom_dotplot(dotsize = 1.5, binaxis = "y", stackdir = "center") + 
     
     #Adjust appearance
     scale_fill_manual(values = c("Pre-Intervention (CAFF)" = "#ffa0a0", 
                                  "Post-Intervention (CAFF)" = "#5757f9", 
                                  "Pre-Intervention (DECAF)" = "#ffa0a0", 
                                  "Post-Intervention (DECAF)" = "#5757f9")) +
     
     ggh4x::facet_nested(name ~ Treatment, scales = "free_x") +
     ylab("") + xlab("") + theme_bw() + 
     theme(text = element_text(size = 12))) +
  
  plot_layout(guides = "collect")
  
  
#ex3alpha


alpha_div_ex3 %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex3, levels = levels(meta_ex3$Legend_ex3))) %>% 
  
  group_by(name) %>% 
  
  reframe(
    
    lm(value ~ visit * Treatment, data = across(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  
  write.csv(., "stats/ex3/alpha_div_glms.csv")



species.exp_ex3 <- genefilter::varFilter(species.exp_ex3, var.cutoff = 0.5)

species.glmer_ex3 <- fw_glmer(x = species.exp_ex3, 
                              f = ~ visit * Treatment + (1|participant_ID), 
                              metadata = meta_ex3, 
                              order = "ac", verbose = FALSE) 

GBMs.glmer_ex3 <- fw_glmer(x = GBMs.exp_ex3, 
                           f = ~ visit * Treatment+ (1|participant_ID), 
                           metadata = meta_ex3, 
                           order = "ac", verbose = FALSE) 


GMMs.glmer_ex3 <- fw_glmer(x = GMMs.exp_ex3, 
                           f = ~ visit * Treatment + (1|participant_ID), 
                           metadata = meta_ex3, 
                           order = "ac", verbose = FALSE) 

metab.glmer_ex3 <- fw_glmer(x = metabs.exp_ex3, 
                           f = ~ visit * Treatment + (1|participant_ID), 
                           metadata = meta_ex3, 
                           order = "ac", verbose = FALSE) 

# hist(species.glmer_ex3$`anovas.visit:Treatment Pr(>F)`, breaks = 20)
# hist(GBMs.glmer_ex3$`anovas.visit:Treatment Pr(>F).BH`, breaks = 20)
# hist(GMMs.glmer_ex3$`anovas.visit:Treatment Pr(>F).BH`, breaks = 20)
# hist(metab.glmer_ex3$`anovas.visit:Treatment Pr(>F).BH`, breaks = 20)

metab_BH_ex3 <- metabs.exp_ex3[metab.glmer_ex3[metab.glmer_ex3$`anovas.visit:Treatment Pr(>F).BH`< 0.2,"feature"],]

ex3metab <- metab_BH_ex3 %>%
  t() %>%
  as.data.frame() %>%
  add_column(Legend = meta_ex3$Legend_ex3)  %>%
  pivot_longer(!c("Legend"))  %>%
  left_join(., metab_trans, by = c("name" = "Compound_ID")) %>% 
  
  mutate(name = str_replace(name, ".*ales_", "")) %>% 
  ggplot(aes(x     = Legend, 
             y     = value, 
             fill  = Legend, 
             group = Legend)) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
  geom_dotplot(dotsize = 1.5, stackdir = "center", binaxis = "y") + 

  
  #Adjust appearance
  scale_fill_manual(values = c("Pre-Intervention (CAFF)" = "#ffa0a0", 
                               "Post-Intervention (CAFF)" = "#5757f9", 
                               "Pre-Intervention (DECAF)" = "#ffa0a0", 
                               "Post-Intervention (DECAF)" = "#5757f9")) +
  
  facet_wrap(~Name, scales = "free_y", ncol = 3) +
  ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))


# 
# ex3pca    %>% 
#   ggsave(plot = ., filename = paste0("figures/ex3/", "pca_beta_div", ".png"), 
#          device = "png", width = 6.42, height = 4.52, bg ="white")
# 
# ex3alpha %>% 
#   ggsave(plot = ., filename = paste0("figures/ex3/", "alpha_div",    ".png"),
#          device = "png", width = 12, height = 8, bg ="white")

write.csv(species.glmer_ex3, file = "stats/ex3/species_glmer.csv")
write.csv(GMMs.glmer_ex3, file = "stats/ex3/GMMs_glmer.csv")
write.csv(GBMs.glmer_ex3, file = "stats/ex3/GBMs_glmer.csv")
write.csv(metab.glmer_ex3, file = "stats/ex3/metab_glmer.csv")

capture.output(ex3_PERMANOVA, file = "stats/ex3/PERMANOVA.txt")
