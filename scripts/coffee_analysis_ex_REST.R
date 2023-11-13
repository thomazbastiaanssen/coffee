source("subscripts/load_and_clean_data.R")


#####Coffee vs non-coffee
meta_ex_REST <- metadata[metadata$visit %in% c("V2", "V3"),]
meta_ex_REST$Legend_ex_REST = factor(meta_ex_REST$Legend_ex_REST, levels = c("Baseline (NCD)", "Baseline (CD)", "Post-washout (CD)"))


species.exp_ex_REST <- species.exp[,meta_ex_REST$R_ID]
GBMs.exp_ex_REST <- GBMs.exp[,meta_ex_REST$R_ID]
GMMs.exp_ex_REST <- GMMs.exp[,meta_ex_REST$R_ID]

alpha_div_ex_REST <- alpha_div[meta_ex_REST$R_ID,]


#Apply the base R principal component analysis function on our CLR-transformed data.
data.a.pca  <- prcomp(t(species.exp_ex_REST))

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
pca$ID                  = meta_ex_REST$participant_ID
pca$Legend              = meta_ex_REST$Legend_ex_REST 
pca$visit               = meta_ex_REST$visit
pca$batch               = meta_ex_REST$batch

#First, the main plot. Plot the first two components of the PCA
ex_RESTpca <- ggplot(pca) +
  
  aes(x       = PC1, 
      y       = PC2, 
      fill    = Legend,
      group   = Legend) +
  
  #Create the points and ellipses
  #stat_ellipse(geom = "polygon", alpha = 1/4, colour = "black") +
  geom_line(aes(group = ID)) +
  geom_point(size = 3, col = "black", shape = 21) + 
  
  #Adjust appearance
  #Adjust appearance

  scale_fill_manual(values = c("Baseline (NCD)"    = "#ece6ca",
                               "Baseline (CD)"     = "#ff0000", 
                               "Post-washout (CD)" = "#ffa0a0")) +
  #Adjust labels
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) + 
  theme_bw() 
ex_RESTpca


dis_ait = dist(t(species.exp_ex_REST), method = "euclidean")


ex_REST_PERMANOVA <- adonis2(dis_ait ~ Legend_ex_REST, 
                         data = meta_ex_REST, 
                         method = "euclidean", 
                         strata = meta_ex_REST$participant_ID,
                         permutations = 10000)



ex_RESTalpha <- alpha_div_ex_REST %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex_REST, levels = levels(meta_ex_REST$Legend_ex_REST))) %>% 
  
  
  ggplot() +
  
  aes(x = Legend, 
      y = value, 
      fill  = Legend, 
      group = Legend) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
  geom_beeswarm(size = 3, cex = 3, shape = 21) + 
  
  #Adjust appearance
  scale_fill_manual(values = c("Baseline (NCD)"    = "#ece6ca",
                               "Baseline (CD)"     = "#ff0000", 
                               "Post-washout (CD)" = "#ffa0a0")) +
  
  facet_wrap(~name, scales = "free_y", ncol = 4) +
  ylab("") + xlab("") + theme_bw() + 
  theme(text = element_text(size = 12))


alpha_div_ex_REST %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex_REST, levels = levels(meta_ex_REST$Legend_ex_REST))) %>% 
  
  group_by(name) %>% 
  
  reframe(
    
    lm(value ~ Legend, data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  
  write.csv(., "stats/ex_REST/alpha_div_glms.csv")


species.exp_ex_REST <- genefilter::varFilter(species.exp_ex_REST, var.cutoff = 0.5)

species.glmer_ex_REST <- fw_glmer(x = species.exp_ex_REST, 
                              f = ~ Legend_ex_REST + (1|participant_ID), 
                              metadata = meta_ex_REST, 
                              order = "ac") 


speBH_ex_REST <- species.exp_ex_REST[species.glmer_ex_REST[species.glmer_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`< 0.2,"feature"],]

ex_RESTDA <- speBH_ex_REST %>%
  t() %>%
  as.data.frame() %>%
  add_column(Legend = meta_ex_REST$Legend_ex_REST)  %>%
  pivot_longer(!c("Legend"))  %>%
  mutate(name = str_replace(name, ".*ales_", "")) %>% 
  ggplot(aes(x     = Legend, 
             y     = value, 
             fill  = Legend, 
             group = Legend)) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
  geom_beeswarm(size = 3, cex = 3, shape = 21) + 
  
  #Adjust appearance
  scale_fill_manual(values = c("Baseline (NCD)"    = "#ece6ca",
                               "Baseline (CD)"     = "#ff0000", 
                               "Post-washout (CD)" = "#ffa0a0")) +
  
  facet_wrap(~name, scales = "free_y", ncol = 3) +
  ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))

ex_RESTDA

hist(species.glmer_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`, breaks = 20)

GBMs.glmer_ex_REST <- fw_glmer(x = GBMs.exp_ex_REST, 
                           f = ~ Legend_ex_REST + (1|participant_ID), 
                           metadata = meta_ex_REST, 
                           order = "ac") 

hist(GBMs.glmer_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`, breaks = 20)

GBMs_BH_ex_REST <- GBMs.exp_ex_REST[GBMs.glmer_ex_REST[GBMs.glmer_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`< 0.2,"feature"],]



GMMs.glmer_ex_REST <- fw_glmer(x = GMMs.exp_ex_REST, 
                           f = ~ Legend_ex_REST + (1|participant_ID), 
                           metadata = meta_ex_REST, 
                           order = "ac") 

# hist(GMMs.glmer_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`, breaks = 20)
# 
# GMM_BH_ex_REST <- GMMs.exp_ex_REST[GMMs.glmer_ex_REST[GMMs.glmer_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`< 0.2,"feature"],]
# 
# ex_RESTDA_GMM <- GMM_BH_ex_REST %>%
#   t() %>%
#   as.data.frame() %>%
#   add_column(Legend = meta_ex_REST$Legend_ex_REST)  %>%
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
#   scale_fill_manual(values = c("Baseline (NCD)"    = "#ece6ca",
#                                "Baseline (CD)"     = "#ff0000", 
#                                "Post-washout (CD)" = "#ffa0a0")) +
#   
#   facet_wrap(~name, scales = "free_y", ncol = 4) +
#   ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))



# hist(GMMs.glmer_ex_REST$`anovas.visit Pr(>F).BH`, breaks = 20)

# 
# 
# # ex_RESTpca    %>% 
# #   ggsave(plot = ., filename = paste0("figures/ex_REST/", "pca_beta_div", ".png"), 
# #          device = "png", width = 6.42, height = 4.52, bg ="white")
# 
# # ex_RESTbar    %>% 
# #   ggsave(plot = ., filename = paste0("figures/ex_REST/", "stck_barplot", ".png"), 
# #          device = "png", width = 16, height = 12, units = "in", bg ="white")
# 
# ex_RESTDA     %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_REST/", "diff_species", ".png"),
#          device = "png", width = 16, height = 8, bg ="white")
# 
# # ex_RESTDA_GMM %>% 
# #   ggsave(plot = ., filename = paste0("figures/ex_REST/", "diff_GMMs",    ".png"),
# #          device = "png", width = 8, height = 4.52, bg ="white")
# 
# 
# ex_RESTalpha %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_REST/", "alpha_div",    ".png"),
#          device = "png", width = 16, height = 4, bg ="white")
# 
# 
# write.csv(species.glmer_ex_REST,  file = "stats/ex_REST/species_glmer.csv")
# write.csv(GMMs.glmer_ex_REST,     file = "stats/ex_REST/GMMs_glmer.csv")
# write.csv(GBMs.glmer_ex_REST,     file = "stats/ex_REST/GBMs_glmer.csv")
# capture.output(ex_REST_PERMANOVA, file = "stats/ex_REST/PERMANOVA.txt")
