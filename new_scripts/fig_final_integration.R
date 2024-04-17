library(ggalluvial)
library(modelsummary)

df_long_cog
df_long_MB
df_long_MX


df_tot <- df_long_MX %>% 
  dplyr::select(c(ID, Coffee_Type, visit, name, value, caffeine)) %>% 
  filter(Coffee_Type != "NCD") %>% 
  group_by(name) %>% 
  mutate(value = c(scale(value))) %>% 
  ungroup() %>% 
  pivot_wider(values_from = value, names_from = name) %>% 
  
  left_join(., df_long_MB %>% 
              dplyr::select(c(ID, Coffee_Type, visit, name, value, caffeine)) %>% 
              filter(Coffee_Type != "NCD") %>% 
              group_by(name) %>% 
              mutate(value = c(scale(value))) %>% 
              ungroup() %>% 
              pivot_wider(values_from = value, names_from = name), 
            by = c("ID" = "ID",  "Coffee_Type" = "Coffee_Type", "visit" = "visit", "caffeine" = "caffeine")) %>% 
  
  left_join(., df_long_cog %>% 
              dplyr::select(c(ID, Coffee_Type, visit, name, value, caffeine)) %>% 
              filter(Coffee_Type != "NCD") %>% 
              group_by(name) %>% 
              mutate(value = c(scale(value))) %>%
              ungroup() %>% 
              pivot_wider(values_from = value, names_from = name), 
            by = c("ID" = "ID", "Coffee_Type" = "Coffee_Type", "visit" = "visit", "caffeine" = "caffeine"))


MX = df_tot[,c("1,7-Dimethylxanthine",  "Caffeine", "Fumaric acid", 
               "Hippuric acid", "Indole-3-carboxyaldehyde", "Indole-3-propionic acid", 
               "Pentose", "Theophylline", "γ-Aminobutyric acid")]

MB = df_tot[,c("Eggerthella sp. CAG:209", "Firmicutes bacterium CAG:94", "Eggerthella sp. 51_9",  "Veillonella parvula", 
               "Haemophilus parainfluenzae", "Cryptobacterium curtum", "Veillonella sp. ACP1")] 

cog = df_tot[,c("(PASAT) Tot", "(ModRey) Tot", "(PSS)", "(BDI) Tot", "(GIS-VAS) Tot", 
                "(UPPS) Tot", "(ERS) Tot", "(STAI-T) Tot", "(PSQI) Tot", "(IPAQ) MET-minutes")]
 
#                "(VASF) Fatigue", "(VASF) Energy", "(CWSQ) Tot", "(QCC) Tot")]


mbmx_R_marg <- matrix(NA, nrow = ncol(MX), 
                     ncol = ncol(MB), 
                     dimnames = list(colnames(MX), colnames(MB)))


mbmx_P_marg <- mbmx_R_marg


mxcog_R_marg <- matrix(NA, nrow = ncol(cog), 
                      ncol = ncol(MX), 
                      dimnames = list(colnames(cog), colnames(MX)))


mxcog_P_marg <- mxcog_R_marg

df_tot_mbmx  <- df_tot
df_tot_mxcog <- df_tot

for(cg in 1:ncol(cog)){
  df_tot$y = unlist(cog[,cg])
  mod.0 <- lmer(y ~ 1 + (1|ID), data = df_tot, REML = TRUE)
  for(mx in 1:ncol(MX)){
    df_tot$x = unlist(MX[,mx])
    mod.1 <- lmer(y ~ x * visit + (1|ID), data = df_tot, REML = TRUE)
    mxcog_P_marg[cg,mx] = anova(mod.0, mod.1)["mod.1", "Pr(>Chisq)"]
    mxcog_R_marg[cg,mx] = modelsummary::get_gof(mod.1)[["r2.marginal"]]
  }
}


mbmx_P_marg %>% 
  as.data.frame() %>% 
  rownames_to_column("Metabolite") %>% 
  pivot_longer(!"Metabolite", names_to = "Species", values_to = "p.val") %>% 
  left_join(., mbmx_R_marg %>% as.data.frame() %>% 
              rownames_to_column("Metabolite") %>% 
              pivot_longer(!"Metabolite", names_to = "Species", values_to = "Rsq"), 
            by = c("Metabolite", "Species")) %>% 
  
  mutate(p.adj = p.adjust(p.val, method = "bonferroni")) %>% 
  filter(p.adj < 0.001) %>% 
  filter(Rsq > 0.3) %>% 
  mutate(Freq = 1) %>% 
  
  
  ggplot() +
  aes(axis1 = Species, axis2 = Metabolite, y = Freq) +
  scale_x_discrete(limits = c("Species", "Metabolite"), expand = c(.2, .05)) +
  
  geom_alluvium(aes(fill = Metabolite)) +
  
  geom_stratum(aes(fill = after_stat(stratum))) +
  
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal()



