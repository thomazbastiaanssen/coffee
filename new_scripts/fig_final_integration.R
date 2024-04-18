library(ggalluvial)
library(modelsummary)

# df_long_cog
# df_long_MB
# df_long_MX


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


for(mx in 1:ncol(MX)){
  df_tot$y = unlist(MX[,mx])
  mod.0 <- lmer(y ~ 1 + (1|ID), data = df_tot, REML = TRUE)
  for(mb in 1:ncol(MB)){
    df_tot$x = unlist(MB[,mb])
    mod.1 <- lmer(y ~ x * visit + (1|ID), data = df_tot, REML = TRUE)
    mbmx_P_marg[mx,mb] = anova(mod.0, mod.1)["mod.1", "Pr(>Chisq)"]
    mbmx_R_marg[mx,mb] = modelsummary::get_gof(mod.1)[["r2.marginal"]]
  }
}




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



    fig_alluvial <-    mxcog_P_marg %>% 
      as.data.frame() %>% 
      rownames_to_column("Cognition") %>% 
      pivot_longer(!"Cognition", names_to = "Metabolites", values_to = "p.val") %>% 
      left_join(., mxcog_R_marg %>% as.data.frame() %>% 
                  rownames_to_column("Cognition") %>% 
                  pivot_longer(!"Cognition", names_to = "Metabolites", values_to = "Rsq"), 
                by = c("Metabolites", "Cognition")) %>% 
      
      mutate(p.adj = p.adjust(p.val, method = "bonferroni")) %>%
      group_by(Metabolites) %>% 
      mutate(nr = paste(Metabolites, row_number(), "mxcog")) %>% 
      #mutate(nr = paste(Metabolites, row_number())) %>% 
      
      ungroup() %>% 
      mutate(sig = case_when((Rsq > 0.3 & p.adj < 0.001) ~ "sig",
                             (Rsq > 0.1 & p.adj < 0.001) ~ "weaksig",
                             
                             .default = "nonsig")) %>% 
      dplyr::select(Metabolites, Cognition, nr, sig) %>% 
      mutate(fill_colour = Metabolites) %>% 
      pivot_longer(!c(nr, fill_colour, sig), values_to = "strata", names_to = "x") %>% 
      distinct() %>% 
      
      
      rbind(., 
            mbmx_P_marg %>% 
              as.data.frame() %>% 
              rownames_to_column("Metabolites") %>% 
              pivot_longer(!"Metabolites", names_to = "Species", values_to = "p.val") %>% 
              left_join(., mbmx_R_marg %>% as.data.frame() %>% 
                          rownames_to_column("Metabolites") %>% 
                          pivot_longer(!"Metabolites", names_to = "Species", values_to = "Rsq"), 
                        by = c("Metabolites", "Species")) %>% 
              
              mutate(p.adj = p.adjust(p.val, method = "bonferroni")) %>% 
              group_by(Metabolites) %>% 
              mutate(nr = paste(Metabolites, row_number(), "mbmx")) %>% 
              #mutate(nr = paste(Metabolites, row_number())) %>% 
              ungroup() %>% 
              mutate(sig = case_when((Rsq > 0.3 & p.adj < 0.001) ~ "sig",
                                     (Rsq > 0.1 & p.adj < 0.001) ~ "weaksig",
                                     
                                     .default = "nonsig")) %>% 
              
              
              dplyr::select(Metabolites, Species, nr, sig) %>% 
              mutate(fill_colour = Metabolites) %>% 
              pivot_longer(!c(nr, fill_colour, sig), values_to = "strata", names_to = "x") %>% 
              distinct() 
            
      ) %>% 
      arrange(nr) %>% 
      mutate(x = factor(x, levels = c("Species", "Metabolites", "Cognition"))) %>% 
      distinct() %>% 
      filter(sig != "nonsig") %>% 
      rbind(., data.frame(nr = letters[1:8], 
                          sig = "sig", 
                          fill_colour = "Caffeine", 
                          x = "Cognition", 
                          strata = c("(IPAQ) MET-minutes", "(ModRey) Tot", "(PSS)", 
                                     "(BDI) Tot", "(GIS-VAS) Tot", "(ERS) Tot", "(STAI-T) Tot",  "(PSQI) Tot"))
            ) %>% 
      mutate(freq = 1) %>% 
      group_by(x) %>% 
      mutate(prop = freq / sum(freq)) %>% 
      ungroup() %>% 
      

      ggplot() +
      
      aes(x = x, y = prop, 
          stratum = strata, 
          alluvium = nr, 
          label = strata) +
      
      geom_stratum() +
      
      
      geom_flow(aes(fill = fill_colour, alpha = sig), colour = "black") +
      
      
      
      geom_label(stat = "stratum") +
      
      #appearnce
      scale_alpha_manual(values = c("sig" = 1, "weaksig" = 0.5,  "nonsig" = 0))+
      
      theme_test() +
      
      xlab(NULL) +
      ylab(NULL) +
      guides(alpha = "none", fill = guide_legend("Legend")) +

      theme(axis.ticks.y  = element_blank(), 
            axis.text.y = element_blank(), 
            axis.text.x = element_text(size = 14))
    


    
    