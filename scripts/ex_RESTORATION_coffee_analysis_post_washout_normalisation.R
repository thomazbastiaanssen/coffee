source("subscripts/load_and_clean_data.R")


#####Coffee vs non-coffee
meta_ex_REST <- metadata[metadata$visit %in% c("V2", "V3"),]
meta_ex_REST$Legend_ex_REST = factor(meta_ex_REST$Legend_ex_REST, levels = c("Baseline (NCD)", "Baseline (CD)", "Post-washout (CD)"))


species.exp_ex_REST <- species.exp[,meta_ex_REST$R_ID]
GBMs.exp_ex_REST    <- GBMs.exp[,meta_ex_REST$R_ID]
GMMs.exp_ex_REST    <- GMMs.exp[,meta_ex_REST$R_ID]
metabs.exp_ex_REST  <- metabs.exp[,meta_ex_REST$Metab_ID]

alpha_div_ex_REST   <- alpha_div[meta_ex_REST$R_ID,]


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
#ex_RESTpca


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

#ex_RESTDA

#hist(species.glmer_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`, breaks = 20)

GBMs.glmer_ex_REST <- fw_glmer(x = GBMs.exp_ex_REST, 
                           f = ~ Legend_ex_REST + (1|participant_ID), 
                           metadata = meta_ex_REST, 
                           order = "ac") 

#hist(GBMs.glmer_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`, breaks = 20)

GBMs_BH_ex_REST <- GBMs.exp_ex_REST[GBMs.glmer_ex_REST[GBMs.glmer_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`< 0.2,"feature"],]



GMMs.glmer_ex_REST <- fw_glmer(x = GMMs.exp_ex_REST, 
                           f = ~ Legend_ex_REST + (1|participant_ID), 
                           metadata = meta_ex_REST, 
                           order = "ac") 


meta_ex_REST$Legend_ex_REST = factor(meta_ex_REST$Legend_ex_REST, levels = c("Baseline (CD)", "Baseline (NCD)", "Post-washout (CD)"))


metab.glm_ex_REST <- fw_glmer(x = metabs.exp_ex_REST, 
                            f = ~ Legend_ex_REST + (1|participant_ID), 
                            metadata = meta_ex_REST, 
                            order = "ac") 
meta_ex_REST$Legend_ex_REST = factor(meta_ex_REST$Legend_ex_REST, levels = c("Baseline (NCD)", "Baseline (CD)", "Post-washout (CD)"))

metab_BH_REST <- metabs.exp_ex_REST[metab.glm_ex_REST[metab.glm_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`< 0.2,"feature"],]

ex_REST_metab_forest <- metab.glm_ex_REST %>%
  as.data.frame() %>%
  # rownames_to_column("name") %>% 
  #
  left_join(., metab_trans, by = c("feature" = "Compound_ID")) %>% 

  filter(`anovas.Legend_ex_REST Pr(>F).BH` < 0.2) %>% 
  dplyr::select(!contains('anovas')) %>% 
  dplyr::select(!contains('Intercept')) %>% 
  pivot_longer(c(`coefs.Legend_ex_RESTPost-washout (CD) 97.5 %`, `coefs.Legend_ex_RESTBaseline (NCD) 97.5 %`,
                 `coefs.Legend_ex_RESTPost-washout (CD) 2.5 %`, `coefs.Legend_ex_RESTBaseline (NCD) 2.5 %`,
                 `coefs.Legend_ex_RESTPost-washout (CD) Estimate`,`coefs.Legend_ex_RESTBaseline (NCD) Estimate`)) %>% 
  mutate(Legend = case_when(grepl("NCD",name) ~ "vs NCD baseline", 
                          grepl("CD", name) ~ "vs CD post-washout")) %>% 
  mutate(name = str_remove(name, pattern = ".*\\) "),
         name = str_remove(name, pattern = "_.*")) %>% 
  
  pivot_wider(names_from = name, values_from = value) %>% 
  
  ggplot() +
  
  aes(y = Estimate/log(2), 
      x = Name, 
      group = Legend, 
      fill = Legend) +
  
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  
  geom_errorbar(aes(ymin = `2.5 %`/log(2), 
                    ymax = `97.5 %`/log(2)), 
                colour = "black", width = 3/4 , position = position_dodge(1/3)) +
  
  geom_point(size = 3, shape = 21, position = position_dodge(1/3)) +
  coord_flip() +
  scale_fill_manual(values = c("vs NCD baseline"  = "#ece6ca", 
                    "vs CD post-washout" = "#ffa0a0")) +
  scale_x_discrete(position = "top", limits=rev) +
  #facet_wrap(~Compound_class, strip.position = "right", scales = "free_y", ncol = 5) +
  xlab(NULL) +
  ylab("log2(FoldChange) vs Baseline Coffee Drinkers") +
  theme_bw() +
  ggtitle("Differentially abundant faecal metabolites between\nnon-coffee drinkers and coffee drinkers post-washout vs baseline coffee drinker levels")




ex_REST_DA_metab <- metab.glm_ex_REST %>%
  as.data.frame() %>%
  # rownames_to_column("name") %>% 
  #
  left_join(., metab_trans, by = c("feature" = "Compound_ID")) %>% 
  mutate(`coefs.Legend_ex_RESTPost-washout (CD) Estimate` = (`coefs.Legend_ex_RESTPost-washout (CD) Estimate`/log(2))) %>% 
  mutate(`coefs.Legend_ex_RESTBaseline (NCD) Estimate` = (`coefs.Legend_ex_RESTBaseline (NCD) Estimate`/log(2))) %>% 
  
  pivot_longer(c(`coefs.Legend_ex_RESTBaseline (NCD) Estimate`, `coefs.Legend_ex_RESTPost-washout (CD) Estimate`)) %>% 
  
  mutate(direction = case_when(`anovas.Legend_ex_REST Pr(>F).BH` > 0.2 ~ "ns",
                               (value < 0 & `anovas.Legend_ex_REST Pr(>F).BH` < 0.2) ~ "Down",
                               (value > 0 & `anovas.Legend_ex_REST Pr(>F).BH` < 0.2) ~ "Up")) %>% 
  
  ggplot() + 
  
  aes(x     = `value`, 
      y     = `anovas.Legend_ex_REST Pr(>F)`, 
      #fill  = `Legend_ex1CD Pr(>|t|).BH` < 0.1,
      alpha = `anovas.Legend_ex_REST Pr(>F).BH` < 0.05, 
      fill = direction) +
  
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  
  scale_fill_manual(values = c("Up" = "red", "Down" = "blue", "ns" = "gray"), "Directionality") +
  
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 1/3), guide = 'none') +
  scale_y_continuous(trans=reverselog_trans(10)) +
  geom_point(shape = 21,colour = "black") +
  ggtitle("Differentially abundant metabolites in coffee drinkers") +
  ylab(expression(paste('p-values (log'[10],'-scale)'))) +
  xlab(expression(paste('\u03B2 Estimate (log'[2],'-fold change)'))) +
  theme_bw() + 
  facet_wrap(~name)
  
  
  
  
  # 
  # metab.glm_ex_REST %>%
  #   rename(`coefs.Legend_ex_RESTBaseline_(NCD) Estimate` = `coefs.Legend_ex_RESTBaseline (NCD) Estimate`) %>% 
  #   rename(`coefs.Legend_ex_RESTPost-washout_(CD) Estimate` = `coefs.Legend_ex_RESTPost-washout (CD) Estimate`) %>% 
  #   rename(`coefs.Legend_ex_RESTBaseline_(NCD) Pr(>|t|)` = `coefs.Legend_ex_RESTBaseline (NCD) Pr(>|t|)`) %>% 
  #   rename(`coefs.Legend_ex_RESTPost-washout_(CD) Pr(>|t|)` = `coefs.Legend_ex_RESTPost-washout (CD) Pr(>|t|)`) %>% 
  #   rename(`coefs.Legend_ex_RESTBaseline_(NCD) Pr(>|t|).BH` = `coefs.Legend_ex_RESTBaseline (NCD) Pr(>|t|).BH`) %>% 
  #   rename(`coefs.Legend_ex_RESTPost-washout_(CD) Pr(>|t|).BH` = `coefs.Legend_ex_RESTPost-washout (CD) Pr(>|t|).BH`) %>% 
  #   rename(`coefs.Legend_ex_RESTBaseline_(NCD) 97.5 %` = `coefs.Legend_ex_RESTBaseline (NCD) 97.5 %`) %>% 
  #   rename(`coefs.Legend_ex_RESTPost-washout_(CD) 2.5 %` = `coefs.Legend_ex_RESTPost-washout (CD) 2.5 %`) %>% 
  #   
  #   pivot_longer(!"feature") %>%
  #   separate(col = name, sep = " ", into = c("Effect", "Parameter") ) %>% 
  #   filter(Effect != "coefs.(Intercept)") %>%  
  #   
  #   filter(feature %in% row.names(metab_BH_REST)) %>% 
  #   
  #   filter(Parameter %in% c("Estimate", "Pr(>|t|)", "2.5", "97.5")) %>% 
  # 
  #   
  #   
  #   mutate(feature = factor(feature), 
  #          feature = factor(feature, levels = rev(levels(feature)))) %>%
  #   
  #   pivot_wider(names_from = Parameter, values_from = value) %>% 
  #   
  #   ggplot(aes(x = Estimate, y = rev(feature), fill = Effect)) + 
  #   
  #   geom_vline(xintercept = 0, linetype = "dashed", colour = "red") +
  #   
  #   geom_errorbar(aes(alpha = `Pr(>|t|)` < 0.1, 
  #                     xmin = `2.5`, 
  #                     xmax = `97.5`), 
  #                 colour = "black", width = 1/2) +
  #   geom_point(aes(alpha = `Pr(>|t|)` < 0.1), shape = 21) + 
  #   scale_alpha_manual(values = c("TRUE"  = 1, 
  #                                 "FALSE" = 1/8)) +
  #   scale_y_discrete(position = "right")+
  #   scale_x_continuous(limits = c(-9, 12))+
  #   #facet_wrap(~microbe, ncol = 5) + 
  #   theme_bw() + ylab("") + guides(alpha = "none", fill = "none")
  
  
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
