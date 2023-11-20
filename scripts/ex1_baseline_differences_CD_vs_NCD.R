source("subscripts/load_and_clean_data.R")

#####Coffee vs non-coffee
meta_ex1 <- metadata[metadata$visit == "V2",]
meta_ex1$Legend_ex1 <- factor(meta_ex1$Legend_ex1, levels = c("NCD", "CD"))

species.exp_ex1 <- species.exp[,meta_ex1$R_ID]
GBMs.exp_ex1    <- GBMs.exp[,meta_ex1$R_ID]
GMMs.exp_ex1    <- GMMs.exp[,meta_ex1$R_ID]
metabs.exp_exp1 <- metabs.exp[,meta_ex1$Metab_ID]

alpha_div_ex1   <- alpha_div[meta_ex1$R_ID,]

#Fork off form the untransformed counts table
bargenus   <- counts[,meta_ex1$R_ID]

#Make into relative abundance
bargenus   <- apply(bargenus, 2, function(i) i/sum(i)) 

#Define a cutoff for rare taxa in several steps:
#first, determine the max % abundance every feature ever shows up at 
maxabundances <- apply(bargenus, 1, max)

#Meanwhile, transpose the count table for future wrangling.
bargenus      <- data.frame(t(bargenus))

#For every sample, sum up all rare taxa ( < 1% at their highest in this case)
bargenus$`zzzRare Taxa` <- rowSums(bargenus[,maxabundances < 0.05], na.rm = TRUE)

#Remove the individual rare taxa now that they're summed up
bargenus = bargenus[,c(maxabundances > 0.05, T) ] #`T` to include the `Rare Taxa`.  

#Prepare the data for ggplot by adding in metadata here
bargenus$Group       = meta_ex1$Legend_ex1
bargenus$ID          = meta_ex1$participant_ID

#Wrangle the data to long format for easy plotting
barlong = bargenus %>% 
  pivot_longer(!c(ID, Group), names_to = c("Microbe"), values_to = "value") %>%
  mutate(Microbe = str_replace(Microbe, ".*_or_", "")) %>% 
  arrange(Microbe) %>% 
  mutate(Microbe = str_replace(Microbe, pattern = "zzzRare", replacement = "Rare"))

#Change the colour for the rare taxa to gray to make them stand out
cols = metafolio::gg_color_hue(length(unique(barlong$Microbe)))
cols[unique(barlong$Microbe)=="Rare Taxa"]="dark gray"

#Create the stacked barplots using ggplot2

ex1bar <- barlong %>%
  ggplot(aes(x = ID, y = value, fill = Microbe)) + 
  geom_bar(stat = "identity", col = "black", linewidth = .2, width = 1) + 
  facet_row(~Group, scales = "free_x") +
  #Adjust layout and appearance
  scale_fill_manual(values = cols, labels = unique(sub(".*ales_", "", barlong$Microbe))) + 
  scale_y_continuous(expand = c(0, 0)) +
  
  guides(fill = guide_legend(ncol = 1, keyheight = 1, title = "Legend")) + 
  theme_bw() + xlab("") +  ylab("Proportion") + 
  theme(text = element_text(size = 14), axis.text.x = element_blank())



#Apply the base R principal component analysis function on our CLR-transformed data.
data.a.pca  <- prcomp(t(species.exp_ex1))

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
pca$ID                  = meta_ex1$participant_ID
pca$Legend              = meta_ex1$Legend_ex1
pca$visit               = meta_ex1$visit
pca$batch               = meta_ex1$batch

#First, the main plot. Plot the first two components of the PCA
ex1pca <- ggplot(pca) +
  
  aes(x       = PC1, 
      y       = PC2, 
      fill    = Legend,
      group   = Legend) +
  
  #Create the points and ellipses
  stat_ellipse(geom = "polygon", alpha = 1/4, colour = "black") +
  geom_point(size = 3, col = "black", shape = 21) + 
  

  #Adjust appearance
  scale_fill_manual(values = c("CD" = "#94641f", 
                               "NCD"  = "#ece6ca")) +
  #Adjust labels
  xlab(paste("PC1: ", pc1,  "%", sep="")) + 
  ylab(paste("PC2: ", pc2,  "%", sep="")) + 
  theme_bw() 
ex1pca

dis_ait = dist(t(species.exp_ex1), method = "euclidean")


ex1_PERMANOVA <- adonis2(dis_ait ~ coffee_group, 
        data = meta_ex1, 
        method = "euclidean", permutations = 10000)


ex1alpha <- alpha_div_ex1 %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex1, levels = levels(meta_ex1$Legend_ex1))) %>% 
  
  
  ggplot() +
  
  aes(x = Legend, 
      y = value, 
      fill  = Legend, 
      group = Legend) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
  geom_beeswarm(size = 3, cex = 3, shape = 21) + 
  
  #Adjust appearance
  scale_fill_manual(values = c("CD" = "#94641f", 
                               "NCD"  = "#ece6ca")) +
  
  facet_wrap(~name, scales = "free_y", ncol = 4) +
  ylab("") + xlab("") + theme_bw() + 
  theme(text = element_text(size = 12))

alpha_div_ex1 %>% 
  pivot_longer(c("Chao1", "Shannon", "Simpson")) %>% 
  mutate(Legend = factor(Legend_ex1, levels = levels(meta_ex1$Legend_ex1))) %>% 
  
  group_by(name) %>% 
  
  reframe(
    
    lm(value ~ Legend, data = pick(everything())) %>% 
         tidy()
    ) %>% 
  
  ungroup() %>% 
  
  write.csv(., "stats/ex1/alpha_div_glms.csv")
  



species.exp_ex1 <- genefilter::varFilter(species.exp_ex1, var.cutoff = 0.5)

species.glm_ex1 <- fw_glm(x = species.exp_ex1, 
                          f = ~ Legend_ex1, 
                          metadata = meta_ex1, 
                          order = "c", verbose = FALSE) 

speBH_ex1 <- species.exp_ex1[species.glm_ex1[species.glm_ex1$`Legend_ex1CD Pr(>|t|).BH`< 0.2,"feature"],]

ex1DA <- speBH_ex1 %>%
  t() %>%
  as.data.frame() %>%
  add_column(Legend = meta_ex1$Legend_ex1)  %>%
  pivot_longer(!c("Legend"))  %>%
  mutate(name = str_replace(name, ".*ales_", "")) %>% 
  ggplot(aes(x     = Legend, 
             y     = value, 
             fill  = Legend, 
             group = Legend)) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
  geom_beeswarm(size = 3, cex = 3, shape = 21) + 
  
  #Adjust appearance
  scale_fill_manual(values = c("CD" = "#94641f", 
                               "NCD"  = "#ece6ca")) +
  
  facet_wrap(~name, scales = "free_y", ncol = 4) +
  ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))





GBMs.glm_ex1 <- fw_glm(x = GBMs.exp_ex1, 
                          f = ~ Legend_ex1, 
                          metadata = meta_ex1, 
                          order = "c", verbose = FALSE) 

GBM_BH_ex1 <- GBMs.exp_ex1[GBMs.glm_ex1[GBMs.glm_ex1$`Legend_ex1CD Pr(>|t|).BH`< 0.2,"feature"],]

ex1DA_GBM <- GBM_BH_ex1 %>%
  t() %>%
  as.data.frame() %>%
  add_column(Legend = meta_ex1$Legend_ex1)  %>%
  pivot_longer(!c("Legend"))  %>%
  mutate(name = str_replace(name, ".*ales_", "")) %>% 
  ggplot(aes(x     = Legend, 
             y     = value, 
             fill  = Legend, 
             group = Legend)) + 
  
  geom_boxplot(alpha = 1/2, coef = 100) +
  geom_beeswarm(size = 3, cex = 3, shape = 21) + 
  
  #Adjust appearance
  scale_fill_manual(values = c("CD" = "#94641f", 
                               "NCD"  = "#ece6ca")) +
  
  facet_wrap(~name, scales = "free_y", ncol = 4) +
  ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))



GMMs.glm_ex1 <- fw_glm(x = GMMs.exp_ex1, 
                       f = ~ Legend_ex1, 
                       metadata = meta_ex1, 
                       order = "c", verbose = FALSE) 

metab.glm_ex1 <- fw_glm(x = metabs.exp_exp1, 
                        f = ~ Legend_ex1, 
                        metadata = meta_ex1, 
                        order = "ac", verbose = FALSE)

#hist(metab.glm_ex1$`Legend_ex1CD Pr(>|t|)`)

metab_BH_ex1 <- metabs.exp_exp1[metab.glm_ex1[metab.glm_ex1$`coefs.Legend_ex1CD Pr(>|t|).BH`< 0.2,"feature"],]


#Too manyu metabolites to show as boxplots. 
# ex1DA_metab <- metab_BH_ex1 %>%
#   t() %>%
#   as.data.frame() %>%
#   add_column(Legend = meta_ex1$Legend_ex1)  %>%
#  # rownames_to_column("name") %>% 
#   #
#   
#   pivot_longer(!c("Legend"))  %>%
#   left_join(., metab_trans, by = c("name" = "Compound_ID")) %>% 
# 
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
#   scale_fill_manual(values = c("CD" = "#94641f", 
#                                "NCD"  = "#ece6ca")) +
#   
#   facet_wrap(~Name, scales = "free_y", ncol = 4) +
#   ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))

ex1_metab_forest <- metab.glm_ex1 %>%
  as.data.frame() %>%
  # rownames_to_column("name") %>% 
  #
  left_join(., metab_trans, by = c("feature" = "Compound_ID")) %>% 
  mutate(Plot_category = replace_na(Plot_category, "Other"))  %>% 
  mutate(`log2(FoldChange)` = (`coefs.Legend_ex1CD Estimate`/log(2))) %>% 
  filter(`coefs.Legend_ex1CD Pr(>|t|).BH` < 0.2) %>% 
  
  arrange(`log2(FoldChange)`)  %>% 
  mutate(Name = factor(Name, levels = unique(Name))) %>% 
  
  ggplot() +
  aes(x = `log2(FoldChange)`, 
      y = (Name)) +
  
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red")+
  
  geom_errorbar(aes(xmin = `coefs.Legend_ex1CD 2.5 %`/log(2), 
                    xmax = `coefs.Legend_ex1CD 97.5 %`/log(2)), 
                colour = "black", width = 1/2) +
  geom_point(shape = 21, fill = "red") +
  scale_y_discrete(position = "right", limits=rev) +
  #facet_wrap(~Class, ncol = 2, strip.position = "right", scales = "free_y") +
  ggforce::facet_col(~Plot_category, strip.position = "top", space = "free", scale = "free_y") +
  ylab(NULL) +
  theme_bw() +
  ggtitle("Differentially abundant faecal metabolites between non-coffee drinkers (L) and coffee drinkers (R)") 



ex1DA_metab <- metab.glm_ex1 %>%
  as.data.frame() %>%
  # rownames_to_column("name") %>% 
  #
  left_join(., metab_trans, by = c("feature" = "Compound_ID")) %>% 
  mutate(`Legend_ex1CD Estimate` = (`coefs.Legend_ex1CD Estimate`/log(2))) %>% 
  #mutate(direction = `GroupLithium Estimate` < 0) %>% 
  mutate(direction = case_when(`coefs.Legend_ex1CD Pr(>|t|).BH` > 0.05 ~ "ns",
                               (`coefs.Legend_ex1CD Estimate` < 0 & `coefs.Legend_ex1CD Pr(>|t|).BH` < 0.05) ~ "Down",
                               (`coefs.Legend_ex1CD Estimate` > 0 & `coefs.Legend_ex1CD Pr(>|t|).BH` < 0.05) ~ "Up")) %>% 
  
  ggplot() + 
  
  aes(x     = `coefs.Legend_ex1CD Estimate`, 
      y     = `coefs.Legend_ex1CD Pr(>|t|)`, 
      #fill  = `Legend_ex1CD Pr(>|t|).BH` < 0.1,
      alpha = `coefs.Legend_ex1CD Pr(>|t|).BH` < 0.05, 
      fill = direction) +
  
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  
  scale_fill_manual(values = c("Up" = "red", "Down" = "blue", "ns" = "gray"), "Directionality") +
  
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 1/3), guide = 'none') +
  scale_y_continuous(trans=reverselog_trans(10)) +
  geom_point(shape = 21,colour = "black") +
  ggtitle("Differentially abundant metabolites in coffee drinkers") +
  ylab(expression(paste('p-values (log'[10],'-scale)'))) +
  xlab(expression(paste('\u03B2 Estimate (log'[2],'-fold change)'))) +
  theme_bw()

# GMM_BH_ex1 <- GMMs.exp_ex1[GMMs.glm_ex1[GMMs.glm_ex1$`coffee_groupCD Pr(>|t|).BH`< 0.2,"feature"],]
# 
# GMM_BH_ex1 %>%
#   t() %>%
#   as.data.frame() %>%
#   add_column(Legend = meta_ex1$coffee_group)  %>%
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
#   scale_fill_manual(values = c("CD" = "#94641f", 
#                                "NCD"  = "#ece6ca")) +
#   
#   facet_wrap(~name, scales = "free_y", ncol = 4) +
#   ylab("") + xlab("") + theme_bw() + theme(text = element_text(size = 12))




# 
# ex1pca    %>% 
#   ggsave(plot = ., filename = paste0("figures/ex1/", "pca_beta_div", ".png"), 
#          device = "png", width = 6.42, height = 4.52, bg ="white")
# 
# ex1bar    %>% 
#   ggsave(plot = ., filename = paste0("figures/ex1/", "stck_barplot", ".png"), 
#          device = "png", width = 16, height = 12, units = "in", bg ="white")
# 
# ex1DA     %>% 
#   ggsave(plot = ., filename = paste0("figures/ex1/", "diff_species", ".png"),
#          device = "png", width = 10, height = 4.52, bg ="white")
# 
# ex1DA_GBM %>% 
#   ggsave(plot = ., filename = paste0("figures/ex1/", "diff_GBMs",    ".png"),
#          device = "png", width = 4, height = 4.52, bg ="white")
# 
# ex1alpha %>% 
#   ggsave(plot = ., filename = paste0("figures/ex1/", "alpha_div",    ".png"),
#          device = "png", width = 12, height = 4, bg ="white")
# 
# 
# write.csv(species.glm_ex1,    file = "stats/ex1/species_glm.csv")
# write.csv(GMMs.glm_ex1,       file = "stats/ex1/GMMs_glm.csv")
# write.csv(GBMs.glm_ex1,       file = "stats/ex1/GBMs_glm.csv")
# capture.output(ex1_PERMANOVA, file = "stats/ex1/PERMANOVA.txt")

