source("subscripts/load_and_clean_data.R")


#####Coffee vs non-coffee

meta_ex_withdraw <- metadata[metadata$visit %in% c("V2", "T2W", "T4W", "V3"),]
meta_ex_withdraw$Legend_ex_withdraw = factor(meta_ex_withdraw$Legend_ex_withdraw, 
                                             levels = c("Baseline (CD)", "Day 2 of washout (T2W)", "Day 4 of washout (T4W)", "Post-washout (T14W)", "Baseline (NCD)"))


species.exp_ex_withdraw <- species.exp[,meta_ex_withdraw$R_ID]
GBMs.exp_ex_withdraw <- GBMs.exp[,meta_ex_withdraw$R_ID]
GMMs.exp_ex_withdraw <- GMMs.exp[,meta_ex_withdraw$R_ID]

alpha_div_ex_withdraw <- alpha_div[meta_ex_withdraw$R_ID,]


#Apply the base R principal component analysis function on our CLR-transformed data.
data.a.pca  <- prcomp(t(species.exp_ex_withdraw))

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
pca$ID                  = meta_ex_withdraw$participant_ID
pca$Legend              = meta_ex_withdraw$Legend_ex_withdraw 
pca$visit               = meta_ex_withdraw$visit
pca$batch               = meta_ex_withdraw$batch
pca$coffee              = meta_ex_withdraw$coffee_group
pca$head                = "Aitchison Distance"

#First, the main plot. Plot the first two components of the PCA
ex_RESTpca <- ggplot(pca) +
  
  aes(x       = PC1, 
      y       = PC2, 
      fill    = Legend,
      group   = Legend, 
      shape   = coffee) +
  
  #Create the points and ellipses
  #stat_ellipse(geom = "polygon", alpha = 1/4, colour = "black") +
  geom_path(aes(group = ID)) +
  geom_point(size = 3, col = "black") + 
  
  #Adjust appearance
  scale_fill_manual(values = c("Baseline (CD)"  = "#543005",
                               "Day 2 of washout (T2W)" = "#bf812d", 
                               "Day 4 of washout (T4W)" = "#dfc27d", 
                               "Post-washout (T14W)" = "#ffa0a0", 
                               "Baseline (NCD)"  = "#ece6ca")) +
  
  scale_shape_manual(values = c("CD" = 21, "NCD" = 22)) +
  guides(shape = FALSE, fill = guide_legend(override.aes = list(shape = c(21,21,21,21,22)))) +
  guides(fill="none") +
  
  #Adjust labels
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) + 
  ggtitle("Principal Component Analysis (Beta Diversity)") +
  theme_bw() +
  facet_wrap(~head)

#ex_RESTpca




dis_ait = dist(t(species.exp_ex_withdraw), method = "euclidean")


ex_withdraw_PERMANOVA <- adonis2(dis_ait ~ Legend_ex_withdraw, 
                                 data = meta_ex_withdraw, 
                                 method = "euclidean", 
                                 strata = meta_ex_withdraw$participant_ID,
                                 permutations = 10000)


ex_RESTalpha <- alpha_div_ex_withdraw %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex_withdraw, levels = c( "Baseline (NCD)", "Baseline (CD)" ,
                                                         "Day 2 of washout (T2W)", 
                                                         "Day 4 of washout (T4W)" , 
                                                         "Post-washout (T14W)"  
  ))) %>% 
  
  ggplot() +
  
  aes(x = Legend, 
      y = value, 
      fill  = Legend, 
      group = Legend,
      shape = coffee_group) + 
  
  geom_boxplot(alpha = 1/2, coef = Inf) +
  geom_point(colour = "black") + 
  geom_vline(xintercept  = 1.5, linetype = "dashed", colour = "black") +
  
  scale_fill_manual(values = c("Baseline (CD)"  = "#543005",
                               "Day 2 of washout (T2W)" = "#bf812d", 
                               "Day 4 of washout (T4W)" = "#dfc27d", 
                               "Post-washout (T14W)" = "#ffa0a0", 
                               "Baseline (NCD)"  = "#ece6ca")) +
  
  scale_shape_manual(values = c("CD" = 21, "NCD" = 22)) +
  guides(shape = FALSE, fill = guide_legend(override.aes = list(shape = c(21,21,21,21,22)))) +
  
  scale_x_discrete(labels = c( "0\n(NCD)", "0\n(CD)", "2", "4", "14")) +
  
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  ylab("") + xlab("") + theme_bw() + 
  guides(fill="none") +
  ggtitle("Alpha Diversity") +
  theme(text = element_text(size = 12))


species.exp_ex_withdraw <- genefilter::varFilter(species.exp_ex_withdraw, var.cutoff = 0.5)

species.glmer_ex_withdraw <- fw_glmer(x = species.exp_ex_withdraw, 
                                      f = ~ Legend_ex_withdraw + (1|participant_ID), 
                                      metadata = meta_ex_withdraw, 
                                      order = "ac", verbose = FALSE) 


ex_RESTDA <- (species.exp_ex_withdraw/log(2)) %>% 
  as.data.frame() %>% 
  rownames_to_column("feature") %>% 
  
  left_join(., species.glmer_ex_withdraw[,c("feature", "anovas.Legend_ex_withdraw Pr(>F).BH", 
                                            "coefs.Legend_ex_withdrawDay 2 of washout (T2W) Pr(>|t|)",
                                            "coefs.Legend_ex_withdrawDay 4 of washout (T4W) Pr(>|t|)" )], by = c("feature" = "feature")) %>% 
  
  filter(`anovas.Legend_ex_withdraw Pr(>F).BH` < 0.2) %>% 
  dplyr::select(!c("anovas.Legend_ex_withdraw Pr(>F).BH", 
                   "coefs.Legend_ex_withdrawDay 2 of washout (T2W) Pr(>|t|)",
                   "coefs.Legend_ex_withdrawDay 4 of washout (T4W) Pr(>|t|)" )) %>% 
  
  pivot_longer(!c(feature)) %>% 
  
  
  
  left_join(., meta_ex_withdraw[,c("Legend_ex_withdraw","R_ID", "participant_ID", "coffee_group")], by = c("name" = "R_ID")) %>% 
  
  dplyr::select(!name) %>% 
  
  mutate(feature = str_remove(feature, " \\(alternative pathway\\: futalosine pathway\\)")) %>% 
  
  filter(!is.na(value)) %>% 
  
  # group_by(Legend_ex_withdraw, feature) %>% 
  # mutate(mean = mean(value, na.rm = T), 
  #        SEM   =   std(value)) %>% 
  # ungroup() %>% 
  
  mutate(Legend_ex_withdraw = factor(Legend_ex_withdraw, levels = c( "Baseline (NCD)", "Baseline (CD)" ,
                                                                     "Day 2 of washout (T2W)", 
                                                                     "Day 4 of washout (T4W)" , 
                                                                     "Post-washout (T14W)"  
  ))) %>% 
  
  ggplot() +
  
  aes(x = Legend_ex_withdraw, y = value, fill = Legend_ex_withdraw, shape = coffee_group) +
  # aes(x = name, y = participant_ID, fill = value, label = round(value,2)) +
  
  geom_boxplot(alpha = 1/2, coef = Inf, show.legend = FALSE)+
  geom_point() +
  
  # geom_errorbar(aes(x = Legend_ex_withdraw, 
  #                   ymin = mean - SEM, 
  #                   ymax = mean + SEM), 
  #               colour = "black", width = 1/4, position = position_dodge(1/3)) +
  
  #geom_point(aes(x = Legend_ex_withdraw, y = mean, fill = Legend_ex_withdraw), shape = 21, size = 3) +
  
  #scale_y_discrete(position = "right") +
  geom_vline(xintercept  = 1.5, linetype = "dashed", colour = "black") +
  
  scale_fill_manual(values = c("Baseline (CD)"  = "#543005",
                               "Day 2 of washout (T2W)" = "#bf812d", 
                               "Day 4 of washout (T4W)" = "#dfc27d", 
                               "Post-washout (T14W)" = "#ffa0a0", 
                               "Baseline (NCD)"  = "#ece6ca"), "Legend") +
  
  scale_shape_manual(values = c("CD" = 21, "NCD" = 22)) +
  guides(shape = FALSE, fill = guide_legend(override.aes = list(shape = c(22,21,21,21,21), size = 5))) +
  
  scale_x_discrete(labels = c( "0\n(NCD)", "0\n(CD)", "2", "4", "14")) +
  #facet_grid(feature ~ Treatment , scales = "free", switch = "y") +
  facet_wrap( ~ feature, scales = "free", ncol = 2) +
  theme_bw() + xlab(NULL) + ylab("Abundance (CLR)") +  
  ggtitle("Bacterial species altered following washout") +
  theme(text = element_text(size = 12), 
        legend.position = c(1, -1/16), legend.justification = c(1, 0), 
        legend.background = element_rect(fill = "white", colour = NA))



#####Coffee vs non-coffee
meta_ex_REST <- metadata[metadata$visit %in% c("V2", "V3"),]
meta_ex_REST$Legend_ex_REST = factor(meta_ex_REST$Legend_ex_REST, levels = c("Baseline (NCD)", "Baseline (CD)", "Post-washout (CD)"))


metabs.exp_ex_REST  <- metabs.exp[,meta_ex_REST$Metab_ID]


meta_ex_REST$Legend_ex_REST = factor(meta_ex_REST$Legend_ex_REST, levels = c("Baseline (CD)", "Baseline (NCD)", "Post-washout (CD)"))


metab.glm_ex_REST <- fw_glmer(x = metabs.exp_ex_REST, 
                            f = ~ Legend_ex_REST + (1|participant_ID), 
                            metadata = meta_ex_REST, 
                            order = "ac", verbose = FALSE) 
meta_ex_REST$Legend_ex_REST = factor(meta_ex_REST$Legend_ex_REST, levels = c("Baseline (NCD)", "Baseline (CD)", "Post-washout (CD)"))

metab_BH_REST <- metabs.exp_ex_REST[metab.glm_ex_REST[metab.glm_ex_REST$`anovas.Legend_ex_REST Pr(>F).BH`< 0.2,"feature"],]

ex_REST_metab_forest_a <- metab.glm_ex_REST %>%
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
  mutate(`2.5 %` = `2.5 %` * -1) %>% 
  mutate(`97.5 %` = `97.5 %` * -1) %>% 
  mutate(Estimate = Estimate * -1) %>% 
  arrange(Estimate)  %>% 
  mutate(Name = factor(Name, levels = unique(Name))) %>% 
  
  filter(Plot_category %in% c("Bile acids", "Coffee-associated compounds", 
                              "Neuroactive compounds & derivatives", "Phytochemical compounds")) %>% 
  mutate(Plot_category = factor(Plot_category,  levels = c("Coffee-associated compounds", "Neuroactive compounds & derivatives",
                                                           "Bile acids", "Phytochemical compounds"))) %>% 
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
  ggforce::facet_col(~Plot_category, strip.position = "top", space = "free", scale = "free_y") +
  xlab(NULL) +
  ylab(NULL) +
  guides(fill="none") +
  theme_bw() + ggtitle("Coffee & Microbiome associated metabolites")

ex_REST_metab_forest_b <- metab.glm_ex_REST %>%
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
  mutate(`2.5 %` = `2.5 %` * -1) %>% 
  mutate(`97.5 %` = `97.5 %` * -1) %>% 
  mutate(Estimate = Estimate * -1) %>% 
  
  arrange(Estimate)  %>% 
  mutate(Name = factor(Name, levels = unique(Name))) %>% 
  
  filter(!Plot_category %in% c("Bile acids", "Coffee-associated compounds", 
                              "Neuroactive compounds & derivatives", "Phytochemical compounds")) %>% 
  mutate(Plot_category = factor(Plot_category,  levels = c("Carbohydrates", "Lipids & organic acids",
                                                           "Peptides, nucleic acids & nucleosides", 
                                                           "Vitamins, nutrients and cofactors", "Other"))) %>% 
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
  ggforce::facet_col(~Plot_category, strip.position = "top", space = "free", scale = "free_y") +
  xlab(NULL) +
  ylab(NULL) +  
  guides(fill="none") +
  theme_bw() + ggtitle("Other metabolites")


# 
# ex_REST_DA_metab <- metab.glm_ex_REST %>%
#   as.data.frame() %>%
#   # rownames_to_column("name") %>% 
#   #
#   left_join(., metab_trans, by = c("feature" = "Compound_ID")) %>% 
#   mutate(`coefs.Legend_ex_RESTPost-washout (CD) Estimate` = (`coefs.Legend_ex_RESTPost-washout (CD) Estimate`/log(2))) %>% 
#   mutate(`coefs.Legend_ex_RESTBaseline (NCD) Estimate` = (`coefs.Legend_ex_RESTBaseline (NCD) Estimate`/log(2))) %>% 
#   
#   pivot_longer(c(`coefs.Legend_ex_RESTBaseline (NCD) Estimate`, `coefs.Legend_ex_RESTPost-washout (CD) Estimate`)) %>% 
#   
#   mutate(direction = case_when(`anovas.Legend_ex_REST Pr(>F).BH` > 0.2 ~ "ns",
#                                (value < 0 & `anovas.Legend_ex_REST Pr(>F).BH` < 0.2) ~ "Down",
#                                (value > 0 & `anovas.Legend_ex_REST Pr(>F).BH` < 0.2) ~ "Up")) %>% 
#   
#   ggplot() + 
#   
#   aes(x     = `value`, 
#       y     = `anovas.Legend_ex_REST Pr(>F)`, 
#       #fill  = `Legend_ex1CD Pr(>|t|).BH` < 0.1,
#       alpha = `anovas.Legend_ex_REST Pr(>F).BH` < 0.05, 
#       fill = direction) +
#   
#   geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
#   
#   scale_fill_manual(values = c("Up" = "red", "Down" = "blue", "ns" = "gray"), "Directionality") +
#   
#   scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 1/3), guide = 'none') +
#   scale_y_continuous(trans=reverselog_trans(10)) +
#   geom_point(shape = 21,colour = "black") +
#   ggtitle("Differentially abundant metabolites in coffee drinkers") +
#   ylab(expression(paste('p-values (log'[10],'-scale)'))) +
#   xlab(expression(paste('\u03B2 Estimate (log'[2],'-fold change)'))) +
#   theme_bw() + 
#   facet_wrap(~name)
  
# (ex_RESTpca + ex_RESTalpha / ex_RESTDA + plot_layout(guides = 'collect'))/
# 
# ((ex_REST_metab_forest_a + ex_REST_metab_forest_b + plot_spacer() + plot_layout(guides = 'collect'))) + plot_layout(heights = c(2,4)) 
 




# plot_annotation(title = "Differentially abundant faecal metabolites between non-coffee drinkers and coffee drinkers post-washout (L) vs baseline coffee drinker levels (R)")
  
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
