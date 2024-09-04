#library(ggalluvial)
library(modelsummary)
library(tidygraph)
library(ggraph)
# df_long_cog
# df_long_MB
# df_long_MX

df_tot <- fecmet_df_long %>% 
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


# fecmet_df_long %>% 
#   filter(plot_label %in% c("moderate", "high")) %>% pull(name) %>% 
#   as.character() %>% unique() %>% dput()


fecMX = df_tot[,c("3-Hydroxybenzoic acid", "4-Hydroxybenzoic acid", "3-(3′,4′-Dihydroxyphenyl)propanoic acid", 
               "3’,4’-Dihydroxycinnamic acid")]

MB = df_tot[,c("Eggerthella sp. CAG:209", "Firmicutes bacterium CAG:94", "Eggerthella sp. 51_9",  "Veillonella parvula", 
               "Haemophilus parainfluenzae", "Cryptobacterium curtum", "Veillonella sp. ACP1")] 

cog = df_tot[,c("(PASAT) Tot", "(ModRey) Tot", "(PSS)", "(BDI) Tot", "(GIS-VAS) Tot", 
                "(UPPS) Tot", "(ERS) Tot", "(STAI-T) Tot", "(PSQI) Tot", "(IPAQ) MET-minutes")]

#                "(VASF) Fatigue", "(VASF) Energy", "(CWSQ) Tot", "(QCC) Tot")]

fecMX ~ MB ~ cog

fecmxmb_R_marg <- matrix(NA, nrow = ncol(MB), 
                      ncol = ncol(fecMX), 
                      dimnames = list(colnames(MB), colnames(fecMX)))


fecmxmb_P_marg <- fecmxmb_R_marg


mbcog_R_marg <- matrix(NA, nrow = ncol(MB), 
                       ncol = ncol(cog), 
                       dimnames = list(colnames(MB), colnames(cog)))


mbcog_P_marg <- mbcog_R_marg

df_tot$y = NA
df_tot$x = NA

df_tot_fecmxmb  <- df_tot
df_tot_mbcog <- df_tot


for(mb in 1:ncol(MB)){
  df_tot_fecmxmb$y = unlist(MB[,mb])
  mod.0 <- lmer(y ~ 1 * visit + (1|ID), data = df_tot_fecmxmb, REML = FALSE)
  for(fm in 1:ncol(fecMX)){
    df_tot_fecmxmb$x = unlist(fecMX[,fm])
    mod.1 <- lmer(y ~ x * visit + (1|ID), data = df_tot_fecmxmb, REML = FALSE)
    fecmxmb_P_marg[mb,fm] = anova(mod.0, mod.1)["mod.1", "Pr(>Chisq)"]
    fecmxmb_R_marg[mb,fm] = modelsummary::get_gof(mod.1)[["r2.marginal"]]
  }
}


for(mb in 1:ncol(MB)){
  df_tot_mbcog$y = unlist(MB[,mb])
  mod.0        <- lmer(y ~ 1  * visit + (1|ID), data = df_tot_mbcog, REML = FALSE)
  for(cg in 1:ncol(cog)){
    df_tot_mbcog$x = unlist(cog[,cg])
    
    if(identical(unname(is.na(df_tot_mbcog$x)), unname(is.na(df_tot_mbcog$y)))){
      mod.1 <- lmer(y ~ x * visit + (1|ID), data = df_tot_mbcog, REML = FALSE)
      mbcog_P_marg[mb,cg] = anova(mod.0, mod.1)["mod.1", "Pr(>Chisq)"]
      mbcog_R_marg[mb,cg] = modelsummary::get_gof(mod.1)[["r2.marginal"]]}  
    
    
    if(!identical(unname(is.na(df_tot_mbcog$x)), unname(is.na(df_tot_mbcog$y)))){
      df_tot_mbcog_tmp = df_tot_mbcog[(!is.na(df_tot_mbcog$x) & !is.na(df_tot_mbcog$y)) ,]
      mod.0.temp   <- lmer(y ~ 1 * visit + (1|ID), data = df_tot_mbcog_tmp, REML = FALSE)
      mod.1        <- lmer(y ~ x * visit + (1|ID), data = df_tot_mbcog_tmp, REML = FALSE)
      mbcog_P_marg[mb,cg] = anova(mod.0.temp, mod.1)["mod.1", "Pr(>Chisq)"]
      mbcog_R_marg[mb,cg] = modelsummary::get_gof(mod.1)[["r2.marginal"]]}  
  }
}




# colour_key = c("3-Hydroxybenzoic acid" = "#e41a1c", 
#                "4-Hydroxybenzoic acid" = "#377eb8", 
#                "3-(3′,4′-Dihydroxyphenyl)propanoic acid" = "#4daf4a", 
#                "3’,4’-Dihydroxycinnamic acid" = "#984ea3")
#"#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00" "#FFFF33" "#A65628" "#F781BF" 


colour_key = c(
  "Eggerthella sp. CAG:209"     = "#e41a1c",
  "Firmicutes bacterium CAG:94" = "#377eb8",
  "Eggerthella sp. 51_9"        = "#4daf4a",
  "Veillonella parvula"         = "#984ea3",
  "Haemophilus parainfluenzae"  = "#ff7f00",
  "Cryptobacterium curtum"      = "#ffff33",
  "Veillonella sp. ACP1"        = "#a65628")


fig_int_fecmet_MB_cog <- rbind(mbcog_P_marg %>% 
                   as.data.frame() %>% 
                   rownames_to_column("Microbes") %>% 
                   pivot_longer(!"Microbes", names_to = "non_mb", values_to = "p.val") %>% 
                   left_join(., mbcog_R_marg %>% as.data.frame() %>% 
                               rownames_to_column("Microbes") %>% 
                               pivot_longer(!"Microbes", names_to = "non_mb", values_to = "Rsq"), 
                             by = c("Microbes", "non_mb")) %>% 
                   
                   mutate(p.adj = p.adjust(p.val, method = "bonferroni")) %>% 
                   dplyr::select(c("Microbes", "non_mb", "p.val", "Rsq", "p.adj")) %>% 
                   mutate(type = "cognition"),
                 
                 
                 fecmxmb_P_marg %>% 
                   as.data.frame() %>% 
                   rownames_to_column("Microbes") %>% 
                   pivot_longer(!"Microbes", names_to = "non_mb", values_to = "p.val") %>% 
                   left_join(., fecmxmb_R_marg %>% as.data.frame() %>% 
                               rownames_to_column("Microbes") %>% 
                               pivot_longer(!"Microbes", names_to = "non_mb", values_to = "Rsq"), 
                             by = c("Microbes", "non_mb")) %>% 
                   
                   mutate(p.adj = p.adjust(p.val, method = "bonferroni")) %>% 
                   mutate(type = "fecal metabolites")) %>% 
  arrange(type, non_mb, Microbes) %>% 
  mutate(sig = case_when((Rsq > 0.15 & p.adj < 0.1) ~ "sig",
                         (Rsq > 0.075 & p.adj < 0.1) ~ "weaksig",
                         
                         .default = "nonsig")) %>% 
  
  
  as_tbl_graph() %>% 
  mutate(label = name, 
         xpos = case_when(name %in% colnames(MB)  ~ 2, 
                          name %in% colnames(cog) ~ 3, 
                          name %in% colnames(fecMX)  ~ 1),
         ypos = c(
           (1:7)/7*1.5,
           (1:10)/10*1.5,
           (1:4)/4*1.5)
         ) %>%
  
  activate(., "edges") %>% 
  
  
  mutate(metab_name    = .N()$label[from] %>% factor, 
         nonmetab_name = .N()$label[to] %>% factor) %>% 
  
  #filter(sig != "nonsig") %>% 
  
  group_by(metab_name, type) %>% 
  mutate(rel_Rsq = Rsq / sum(Rsq)) %>% 
  ungroup() %>%
  mutate(nudge = 0.25, 
         left_nudge = case_when( type == "cognition"  ~ 2 + nudge, 
                                 type == "fecal metabolites" ~ 1 + nudge), 
         right_nudge = case_when(type == "cognition"  ~ 2.5 + nudge, 
                                 type == "fecal metabolites" ~ 1.5 + nudge)) %>% 
  
  activate(., "nodes") %>% 
  mutate(node_fill = case_when(label %in% colnames(MX) ~ label, 
                               .default = NA)) %>%
  
  activate(., "edges") %>%  #data.frame %>% View
  
  mutate(left_y = case_when(type == "cognition" ~ .N()$ypos[from], 
                            type == "fecal metabolites" ~ .N()$ypos[to]), 
         right_y = case_when(type == "cognition" ~ .N()$ypos[to], 
                             type == "fecal metabolites" ~ .N()$ypos[from]), 
  ) %>% data.frame() %>% 
  
  group_by(metab_name, type) %>%  
  mutate(mx_cnt = 1:length(unique(nonmetab_name)), 
         mx_sum_rel_Rsq = sum(rel_Rsq), 
         mx_mrg = 0.1 * (rel_Rsq/mx_sum_rel_Rsq),
         mx_cs_mrg = cumsum(mx_mrg)) %>% # -  (1/2)*mx_mrg[1]) %>% 
  ungroup() %>%  
  
  group_by(nonmetab_name, type) %>%  
  mutate(nx_cnt = 1:length(unique(metab_name)), 
         nx_sum_rel_Rsq = sum(rel_Rsq), 
         nx_mrg = 0.1 * (rel_Rsq/nx_sum_rel_Rsq),
         nx_cs_mrg = cumsum(nx_mrg)) %>% # - (1/2)*nx_mrg[1]) %>% 
  ungroup()  %>% 
  
  
  pivot_longer(c(left_y, right_y), values_to = "y_val") %>% 
  mutate(count = 2) %>% uncount(count) %>%
  mutate(name = paste0(name, c("_top", "_bot"))) %>% 
  
  mutate(y_val = case_when(name == "left_y_top"  & type == "cognition"  ~ y_val + mx_cs_mrg, #+ (1/2)*(mx_mrg), 
                           name == "left_y_bot"  & type == "cognition"  ~ y_val + mx_cs_mrg - (mx_mrg), 
                           name == "right_y_top" & type == "cognition"  ~ y_val + nx_cs_mrg , 
                           name == "right_y_bot" & type == "cognition"  ~ y_val + nx_cs_mrg - (nx_mrg),
                           name == "left_y_top"  & type == "fecal metabolites" ~ y_val + nx_cs_mrg , 
                           name == "left_y_bot"  & type == "fecal metabolites" ~ y_val + nx_cs_mrg - (nx_mrg), 
                           name == "right_y_top" & type == "fecal metabolites" ~ y_val + mx_cs_mrg, # + (1/2)*(mx_mrg), 
                           name == "right_y_bot" & type == "fecal metabolites" ~ y_val + mx_cs_mrg - (mx_mrg)
  )) %>% 
  mutate(x_val = case_when(name == "left_y_top"  ~ left_nudge, 
                           name == "left_y_bot"  ~ left_nudge, 
                           name == "right_y_top" ~ right_nudge, 
                           name == "right_y_bot" ~ right_nudge
  )) %>% 
  
  mutate(id = paste(from, to)) %>% 
  group_by(id, type) %>% 
  mutate(Rsq_y = case_when(type == "fecal metabolites" ~ mean(y_val[name %in% c("left_y_top",  "left_y_bot" )]), 
                           type == "cognition"  ~ mean(y_val[name %in% c("right_y_top", "right_y_bot")])), 
         Rsq_x = case_when(type == "fecal metabolites" ~ mean(x_val[name %in% c("left_y_top",  "left_y_bot" )]) * 0.958, 
                           type == "cognition"  ~ mean(x_val[name %in% c("right_y_top", "right_y_bot")]) * 1.005)
  ) %>% 
  ungroup() %>% 
  # dplyr::select(c(id, p.adj, Rsq_x, Rsq_y, Rsq, metab_name, nonmetab_name, type, sig, y_val, name)) %>% View
  
  ggplot() + 
  
  geom_diagonal_wide(data = . %>% filter(sig %in% c("weaksig", "sig")),aes(x = x_val, y = y_val, group = id, fill = metab_name, alpha = sig), colour = "black") +
  #microbiome
  geom_rect(data = . %>% pluck("metab_name") %>% levels() %>% tibble() %>% dplyr::rename("metab_name" = ".") %>% 
              mutate(
                xmin = 1.75, 
                xmax = 2.25,
                ymin = (1:7/7*1.5),# + (1.5 * 0.1) - 0.075,
                ymax = (1:7/7*1.5) + 0.1),
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax,
                fill = metab_name), 
            colour ="black") +
  
  geom_label(data = . %>% pluck("metab_name") %>% levels() %>% tibble() %>% 
               dplyr::rename("metab_name" = ".") %>% 
               mutate(
                 x = 2,  
                 y = (1:7/7*1.5) + 0.05), 
             aes(x = x, y = y, label = metab_name), alpha = 0.75, size = 5) + 
  
  #fecal metabolites
  geom_rect(data = . %>% filter(type == "fecal metabolites") %>% dplyr::select(nonmetab_name) %>% distinct() %>% 
              
              mutate(
                xmin = 0.75  - 0.025, 
                xmax = 1.25,
                ymin = (1:4)/4*1.5       - 0.005,# + (1.5 * 0.1) - 0.075,
                ymax = (1:4)/4*1.5 + 0.1 + 0.005),
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax),
            fill = "white", colour = "black") +
  
  geom_label(data = . %>% filter(type == "fecal metabolites") %>% dplyr::select(nonmetab_name) %>% distinct() %>% 
               mutate(nonmetab_name = str_replace(nonmetab_name, pattern = "bacterium", replacement = "sp.")) %>%  
               mutate(x = 1,  
                      y = (1:4/4*1.5) + 0.05), 
             aes(x = x, y = y, label = nonmetab_name), alpha = 0.75, size = 5, nudge_x = -0.0375) + 
  
  #cognition
  geom_rect(data = . %>% filter(type == "cognition") %>% dplyr::select(nonmetab_name) %>% distinct() %>% 
              mutate(xmin = 2.75, 
                     xmax = 3.25 + 0.025,
                     ymin = (1:10/10*1.5)         - 0.005,# + (1.5 * 0.1) - 0.075,
                     ymax = ((1:10/10*1.5) + 0.1) + 0.005),
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax),
            fill = "white", colour = "black") +
  
  geom_label(data = . %>% filter(type == "cognition") %>% dplyr::select(nonmetab_name) %>% distinct() %>% 
               mutate(x = 3,  
                      y = (1:10/10*1.5) + 0.05), 
             aes(x = x, y = y, label = nonmetab_name), alpha = 0.75, size = 5, nudge_x = 0.0375) + 
  
  geom_text(data = . %>% dplyr::select(c(id, Rsq_x, Rsq_y, Rsq, metab_name, nonmetab_name, sig)) %>%
              filter(sig != "nonsig") %>% 
              distinct(id, Rsq_x, Rsq_y, Rsq, metab_name, nonmetab_name),
              
            aes(x = Rsq_x, y = Rsq_y, 
                #label = paste0("~R^{2}==", round(Rsq, 2))
                label = round(Rsq, 2)
                
            ), 
            hjust = 0, vjust = 1/2, size = 3, parse = TRUE) +
  
  #Labels
  geom_rect(data = data.frame(l = c("Fecal Polyphenols", "Microbial Species", "Cognition and Behaviour"), 
                              xmin = c(2/3, 5/3, 8/3),
                              xmax = c(4/3, 7/3, 10/3),
                              ymin = c(-0.1, -0.1, -0.1) *0.8, 
                              ymax = c(0.1, 0.1, 0.1)*0.8), aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), colour = "black", fill = "lightgray")+
  geom_text(data = data.frame(l = c("Fecal Polyphenols", "Microbial Species",  "Cognition and Behaviour"), 
                              x = c(1, 2, 3), 
                              y = c(0, 0, 0)), aes(x = x, y = y, label = l), size = 8) +
  
  geom_text(data = data.frame(lab = c("~R^{2}"), 
                              x = c(1.25 - 0.03125, 2.75 + 0.03125), 
                              y = c(1.650)), aes(x = x, y = y, label = lab), size = 5, parse = TRUE, hjust = 1/2) +
  
  
  scale_alpha_manual(values = c("sig" = 1, "weaksig" = 0.5)) +
  
  guides(fill = "none", alpha = "none", size = "none") +
  theme_void()  



