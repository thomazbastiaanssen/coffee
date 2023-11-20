source("subscripts/load_and_clean_data.R")


#####Coffee vs non-coffee

meta_ex_withdraw <- metadata[metadata$Legend_ex1 != "NCD" & metadata$visit %in% c("V2", "T2W", "T4W"),]
meta_ex_withdraw$Legend_ex_withdraw = factor(meta_ex_withdraw$Legend_ex_withdraw, levels = c("Baseline (T0W)", "Day 2 of washout (T2W)", "Day 4 of washout (T4W)"))


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

#First, the main plot. Plot the first two components of the PCA
ex_withdrawpca <- ggplot(pca) +
  
  aes(x       = PC1, 
      y       = PC2, 
      fill    = Legend,
      group   = Legend) +
  
  #Create the points and ellipses
  #stat_ellipse(geom = "polygon", alpha = 1/4, colour = "black") +
  geom_path(aes(group = ID)) +
  geom_point(size = 3, col = "black", shape = 21) + 
  
  #Adjust appearance
  scale_fill_manual(values = c("Baseline (T0W)"  = "#543005",
                               "Day 2 of washout (T2W)" = "#bf812d", 
                               "Day 4 of washout (T4W)" = "#dfc27d")) +
  #Adjust labels
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) + 
  theme_bw() 

#ex_withdrawpca




dis_ait = dist(t(species.exp_ex_withdraw), method = "euclidean")


ex_withdraw_PERMANOVA <- adonis2(dis_ait ~ Legend_ex_withdraw, 
                             data = meta_ex_withdraw, 
                             method = "euclidean", 
                             strata = meta_ex_withdraw$participant_ID,
                             permutations = 10000)


# dis_ait %>% 
#   as.matrix %>% 
#   data.frame() %>% 
#   rownames_to_column("sample1") %>% 
#   pivot_longer(!sample1, names_to = "sample2") %>% 
#   filter(sample1 != sample2) %>% 
#   
#   left_join(., meta_ex_withdraw %>% 
#               dplyr::select(c("R_ID", "participant_ID", "Legend_ex_withdraw")), 
#                             by = c("sample1" = "R_ID")) %>% 
#   
#   left_join(., meta_ex_withdraw %>% 
#               dplyr::select(c("R_ID", "participant_ID", "Legend_ex_withdraw")), 
#             by = c("sample2" = "R_ID")) %>% 
#   
#   filter(participant_ID.x == participant_ID.y) %>% 
#   
#   dplyr::select(!c(sample1, sample2)) %>% 
#   
#   filter((Legend_ex_withdraw.x == "Baseline (T0W)" & Legend_ex_withdraw.y == "Day 2 of washout (T2W)")|
#            (Legend_ex_withdraw.x == "Day 2 of washout (T2W)" & Legend_ex_withdraw.y == "Day 4 of washout (T4W)") ) %>% 
#   
#   mutate(Legend = paste(Legend_ex_withdraw.x, "vs", Legend_ex_withdraw.y)) %>% 
#   ggplot() +
#   
#   aes(x = Legend, y = value) +
#   geom_boxplot() +
#   geom_point() +
#   
#   theme_bw()
  
  


ex_withdrawalpha <- alpha_div_ex_withdraw %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex_withdraw, levels = levels(meta_ex_withdraw$Legend_ex_withdraw))) %>% 
  
  
  ggplot() +
  
  aes(x = Legend, 
      y = value, 
      fill  = Legend, 
      group = Legend) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
  geom_beeswarm(size = 3, cex = 3, shape = 21) + 
  
  
  scale_fill_manual(values = c("Baseline (T0W)"  = "#543005",
                               "Day 2 of washout (T2W)" = "#bf812d", 
                               "Day 4 of washout (T4W)" = "#dfc27d")) +
  
  facet_wrap(~name, scales = "free_y", ncol = 4) +
  ylab("") + xlab("") + theme_bw() + 
  theme(text = element_text(size = 12))


alpha_div_ex_withdraw %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex_withdraw, levels = levels(meta_ex_withdraw$Legend_ex_withdraw))) %>% 
  
  group_by(name) %>% 
  
  reframe(
    
    lm(value ~ Legend, data = across(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  
  write.csv(., "stats/ex_acute_withdrawal/alpha_div_glms.csv")


species.exp_ex_withdraw <- genefilter::varFilter(species.exp_ex_withdraw, var.cutoff = 0.5)

species.glmer_ex_withdraw <- fw_glmer(x = species.exp_ex_withdraw, 
                                  f = ~ Legend_ex_withdraw + (1|participant_ID), 
                                  metadata = meta_ex_withdraw, 
                                  order = "ac", verbose = FALSE) 


# speBH_ex_withdraw <- species.exp_ex_withdraw[species.glmer_ex_withdraw[species.glmer_ex_withdraw$`anovas.Legend_ex_withdraw Pr(>F).BH`< 0.2,"feature"],]
# 
# ex_withdrawDA <- speBH_ex_withdraw %>%
#   t() %>%
#   as.data.frame() %>%
#   add_column(Legend = meta_ex_withdraw$Legend_ex_withdraw)  %>%
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
#   scale_fill_manual(values = c("Baseline (NCD)"  = "#ece6ca",
#                                "Baseline (CD)" = "#ff0000", 
#                                "Post-washout (CD)" = "#ffa0a0")) +
#   
#   facet_wrap(~name, scales = "free_y", ncol = 3) +
#   ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))
# 
# ex_withdrawDA
# 
# hist(species.glmer_ex_withdraw$`anovas.Legend_ex_withdraw Pr(>F).BH`, breaks = 20)

GBMs.glmer_ex_withdraw <- fw_glmer(x = GBMs.exp_ex_withdraw, 
                               f = ~ Legend_ex_withdraw + (1|participant_ID), 
                               metadata = meta_ex_withdraw, 
                               order = "ac", verbose = FALSE) 

#hist(GBMs.glmer_ex_withdraw$`anovas.Legend_ex_withdraw Pr(>F).BH`, breaks = 20)

GBMs_BH_ex_withdraw <- GBMs.exp_ex_withdraw[GBMs.glmer_ex_withdraw[GBMs.glmer_ex_withdraw$`anovas.Legend_ex_withdraw Pr(>F).BH`< 0.2,"feature"],]



GMMs.glmer_ex_withdraw <- fw_glmer(x = GMMs.exp_ex_withdraw, 
                               f = ~ Legend_ex_withdraw + (1|participant_ID), 
                               metadata = meta_ex_withdraw, 
                               order = "ac", verbose = FALSE) 

#hist(GMMs.glmer_ex_withdraw$`anovas.Legend_ex_withdraw Pr(>F).BH`, breaks = 20)

# GMM_BH_ex_withdraw <- GMMs.exp_ex_withdraw[GMMs.glmer_ex_withdraw[GMMs.glmer_ex_withdraw$`anovas.Legend_ex_withdraw Pr(>F).BH`< 0.2,"feature"],]
# 
# ex_withdrawDA_GMM <- GMM_BH_ex_withdraw %>%
#   t() %>%
#   as.data.frame() %>%
#   add_column(Legend = meta_ex_withdraw$Legend_ex_withdraw)  %>%
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
#   scale_fill_manual(values = c("Baseline (NCD)"  = "#ece6ca",
#                                "Baseline (CD)" = "#ff0000", 
#                                "Post-washout (CD)" = "#ffa0a0")) +
#   
#   facet_wrap(~name, scales = "free_y", ncol = 4) +
#   ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))
# 


# hist(GMMs.glmer_ex_withdraw$`anovas.visit Pr(>F).BH`, breaks = 20)



# ex_withdrawpca    %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_withdraw/", "pca_beta_div", ".png"), 
#          device = "png", width = 6.42, height = 4.52, bg ="white")

# ex_withdrawbar    %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_withdraw/", "stck_barplot", ".png"), 
#          device = "png", width = 16, height = 12, units = "in", bg ="white")

# ex_withdrawDA     %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_withdraw/", "diff_species", ".png"),
#          device = "png", width = 16, height = 8, bg ="white")

# ex_withdrawDA_GMM %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_withdraw/", "diff_GMMs",    ".png"),
#          device = "png", width = 8, height = 4.52, bg ="white")

# 
# ex_withdrawalpha %>% 
#   ggsave(plot = ., filename = paste0("figures/ex_withdraw/", "alpha_div",    ".png"),
#          device = "png", width = 16, height = 4, bg ="white")


write.csv(species.glmer_ex_withdraw,  file = "stats/ex_acute_withdrawal/species_glmer.csv")
write.csv(GMMs.glmer_ex_withdraw,     file = "stats/ex_acute_withdrawal/GMMs_glmer.csv")
write.csv(GBMs.glmer_ex_withdraw,     file = "stats/ex_acute_withdrawal/GBMs_glmer.csv")
capture.output(ex_withdraw_PERMANOVA, file = "stats/ex_acute_withdrawal/PERMANOVA.txt")
