#library(ggalluvial)
library(modelsummary)
library(tidygraph)
library(ggraph)
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


mxcog_R_marg <- matrix(NA, nrow = ncol(MX), 
                      ncol = ncol(cog), 
                      dimnames = list(colnames(MX), colnames(cog)))


mxcog_P_marg <- mxcog_R_marg

df_tot$y = NA
df_tot$x = NA

df_tot_mbmx  <- df_tot
df_tot_cogmx <- df_tot


for(mx in 1:ncol(MX)){
  df_tot_mbmx$y = unlist(MX[,mx])
  mod.0 <- lmer(y ~ 1 * visit + (1|ID), data = df_tot_mbmx, REML = FALSE)
  for(mb in 1:ncol(MB)){
    df_tot_mbmx$x = unlist(MB[,mb])
    mod.1 <- lmer(y ~ x * visit + (1|ID), data = df_tot_mbmx, REML = FALSE)
    mbmx_P_marg[mx,mb] = anova(mod.0, mod.1)["mod.1", "Pr(>Chisq)"]
    mbmx_R_marg[mx,mb] = modelsummary::get_gof(mod.1)[["r2.marginal"]]
  }
}


for(mx in 1:ncol(MX)){
  df_tot_cogmx$y = unlist(MX[,mx])
  mod.0        <- lmer(y ~ 1  * visit + (1|ID), data = df_tot_cogmx, REML = FALSE)
  for(cg in 1:ncol(cog)){
    df_tot_cogmx$x = unlist(cog[,cg])
    
    if(identical(unname(is.na(df_tot_cogmx$x)), unname(is.na(df_tot_cogmx$y)))){
      mod.1 <- lmer(y ~ x * visit + (1|ID), data = df_tot_cogmx, REML = FALSE)
      mxcog_P_marg[mx,cg] = anova(mod.0, mod.1)["mod.1", "Pr(>Chisq)"]
      mxcog_R_marg[mx,cg] = modelsummary::get_gof(mod.1)[["r2.marginal"]]}  
    
    
    if(!identical(unname(is.na(df_tot_cogmx$x)), unname(is.na(df_tot_cogmx$y)))){
      df_tot_cogmx_tmp = df_tot_cogmx[(!is.na(df_tot_cogmx$x) & !is.na(df_tot_cogmx$y)) ,]
      mod.0.temp   <- lmer(y ~ 1 * visit + (1|ID), data = df_tot_cogmx_tmp, REML = FALSE)
      mod.1        <- lmer(y ~ x * visit + (1|ID), data = df_tot_cogmx_tmp, REML = FALSE)
      mxcog_P_marg[mx,cg] = anova(mod.0.temp, mod.1)["mod.1", "Pr(>Chisq)"]
      mxcog_R_marg[mx,cg] = modelsummary::get_gof(mod.1)[["r2.marginal"]]}  
  }
}



  
colour_key = c(
  "γ-Aminobutyric acid"      = "#e41a1c",
  "Theophylline"             = "#377eb8",
  "Pentose"                  = "#4daf4a",
  "Indole-3-propionic acid"  = "#984ea3",
  "Indole-3-carboxyaldehyde" = "#ff7f00",
  "Hippuric acid"            = "#ffff33",
  "Fumaric acid"             = "#a65628",
  "Caffeine"                 = "#f781bf", 
  "1,7-Dimethylxanthine"     = "#999999")

fig_int <- rbind(mxcog_P_marg %>% 
                   as.data.frame() %>% 
                   rownames_to_column("Metabolites") %>% 
                   pivot_longer(!"Metabolites", names_to = "non_mb", values_to = "p.val") %>% 
                   left_join(., mxcog_R_marg %>% as.data.frame() %>% 
                               rownames_to_column("Metabolites") %>% 
                               pivot_longer(!"Metabolites", names_to = "non_mb", values_to = "Rsq"), 
                             by = c("Metabolites", "non_mb")) %>% 
                   
                   mutate(p.adj = p.adjust(p.val, method = "bonferroni")) %>% 
                   dplyr::select(c("Metabolites", "non_mb", "p.val", "Rsq", "p.adj")) %>% 
                   mutate(type = "cognition"),
                 
                 
                 mbmx_P_marg %>% 
                   as.data.frame() %>% 
                   rownames_to_column("Metabolites") %>% 
                   pivot_longer(!"Metabolites", names_to = "non_mb", values_to = "p.val") %>% 
                   left_join(., mbmx_R_marg %>% as.data.frame() %>% 
                               rownames_to_column("Metabolites") %>% 
                               pivot_longer(!"Metabolites", names_to = "non_mb", values_to = "Rsq"), 
                             by = c("Metabolites", "non_mb")) %>% 
                   
                   mutate(p.adj = p.adjust(p.val, method = "bonferroni")) %>% 
                   mutate(type = "microbiome")) %>% 
  arrange(type, non_mb, Metabolites) %>% 
  mutate(sig = case_when((Rsq > 0.3 & p.adj < 0.001) ~ "sig",
                         (Rsq > 0.1 & p.adj < 0.001) ~ "weaksig",
                         
                         .default = "nonsig")) %>% 
  
  
  as_tbl_graph() %>% 
  mutate(label = name, 
         xpos = case_when(name %in% colnames(MB)  ~ 1, 
                          name %in% colnames(cog) ~ 3, 
                          name %in% colnames(MX)  ~ 2), 
         ypos = c(
           (1:9)/9*1.5,
           (1:10)/10*1.5, 
           (1:7)/7*1.5)) %>%
  
  activate(., "edges") %>% 
  
  
  mutate(metab_name    = .N()$label[from] %>% factor, 
         nonmetab_name = .N()$label[to] %>% factor) %>% 
  
  filter(sig != "nonsig") %>% 
  
  group_by(metab_name, type) %>% 
  mutate(rel_Rsq = Rsq / sum(Rsq)) %>% 
  ungroup() %>%
  mutate(nudge = 0.25, 
         left_nudge = case_when( type == "cognition"  ~ 2 + nudge, 
                                 type == "microbiome" ~ 1 + nudge), 
         right_nudge = case_when(type == "cognition"  ~ 2.5 + nudge, 
                                 type == "microbiome" ~ 1.5 + nudge)) %>% 
  
  activate(., "nodes") %>% 
  mutate(node_fill = case_when(label %in% colnames(MX) ~ label, 
                               .default = NA)) %>%
  
  activate(., "edges") %>%  #data.frame %>% View
  
  mutate(left_y = case_when(type == "cognition" ~ .N()$ypos[from], 
                            type == "microbiome" ~ .N()$ypos[to]), 
         right_y = case_when(type == "cognition" ~ .N()$ypos[to], 
                             type == "microbiome" ~ .N()$ypos[from]), 
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
                           name == "left_y_top"  & type == "microbiome" ~ y_val + nx_cs_mrg , 
                           name == "left_y_bot"  & type == "microbiome" ~ y_val + nx_cs_mrg - (nx_mrg), 
                           name == "right_y_top" & type == "microbiome" ~ y_val + mx_cs_mrg, # + (1/2)*(mx_mrg), 
                           name == "right_y_bot" & type == "microbiome" ~ y_val + mx_cs_mrg - (mx_mrg)
  )) %>% 
  mutate(x_val = case_when(name == "left_y_top"  ~ left_nudge, 
                           name == "left_y_bot"  ~ left_nudge, 
                           name == "right_y_top" ~ right_nudge, 
                           name == "right_y_bot" ~ right_nudge
  )) %>% 
  
  mutate(id = paste(from, to)) %>% 
  group_by(id, type) %>% 
  mutate(Rsq_y = case_when(type == "microbiome" ~ mean(y_val[name %in% c("left_y_top",  "left_y_bot" )]), 
                           type == "cognition"  ~ mean(y_val[name %in% c("right_y_top", "right_y_bot")])), 
         Rsq_x = case_when(type == "microbiome" ~ mean(x_val[name %in% c("left_y_top",  "left_y_bot" )]) * 0.958, 
                           type == "cognition"  ~ mean(x_val[name %in% c("right_y_top", "right_y_bot")]) * 1.005)
         ) %>% 
    ungroup() %>% 
 # dplyr::select(c(id, p.adj, Rsq_x, Rsq_y, Rsq, metab_name, nonmetab_name, type, sig, y_val, name)) %>% View
  
  ggplot() + 
  
  geom_diagonal_wide(aes(x = x_val, y = y_val, group = id, fill = metab_name, alpha = sig), colour = "black") +
  #metabolites
  geom_rect(data = . %>% pluck("metab_name") %>% levels() %>% tibble() %>% dplyr::rename("metab_name" = ".") %>% 
              mutate(
    xmin = 1.75, 
    xmax = 2.25,
    ymin = (1:9/9*1.5),# + (1.5 * 0.1) - 0.075,
    ymax = (1:9/9*1.5) + 0.1),
    aes(xmin = xmin, xmax = xmax, 
        ymin = ymin, ymax = ymax,
        fill = metab_name), 
    colour ="black") +

    geom_label(data = . %>% pluck("metab_name") %>% levels() %>% tibble() %>% 
                 dplyr::rename("metab_name" = ".") %>% 
               mutate(
                 x = 2,  
    y = (1:9/9*1.5) + 0.05), 
    aes(x = x, y = y, label = metab_name), alpha = 0.75, size = 5) + 
  
  #microbiome
  geom_rect(data = . %>% filter(type == "microbiome") %>% dplyr::select(nonmetab_name) %>% distinct() %>% 
              
              mutate(
                xmin = 0.75  - 0.025, 
                xmax = 1.25,
                ymin = (1:7/7*1.5)         - 0.005,# + (1.5 * 0.1) - 0.075,
                ymax = ((1:7/7*1.5) + 0.1) + 0.005) ,
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax),
            fill = "white", colour = "black") +
  
  geom_label(data = . %>% filter(type == "microbiome") %>% dplyr::select(nonmetab_name) %>% distinct() %>% 
               mutate(nonmetab_name = str_replace(nonmetab_name, pattern = "bacterium", replacement = "sp.")) %>%  
               mutate(x = 1,  
                      y = (1:7/7*1.5) + 0.05), 
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
  
  geom_text(data = . %>% dplyr::select(c(id, Rsq_x, Rsq_y, Rsq, metab_name, nonmetab_name)) %>% 
              distinct(id, Rsq_x, Rsq_y, Rsq, metab_name, nonmetab_name),
            
            aes(x = Rsq_x, y = Rsq_y, 
                #label = paste0("~R^{2}==", round(Rsq, 2))
                label = round(Rsq, 2)
                
                ), 
            hjust = 0, vjust = 1/2, size = 3, parse = TRUE) +
  
  #Labels
  geom_rect(data = data.frame(l = c("Microbial Species", "Metabolites", "Cognition and Behaviour"), 
                              xmin = c(2/3, 5/3, 8/3),
                              xmax = c(4/3, 7/3, 10/3),
                              ymin = c(-0.1, -0.1, -0.1) *0.8, 
                              ymax = c(0.1, 0.1, 0.1)*0.8), aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), colour = "black", fill = "lightgray")+
  geom_text(data = data.frame(l = c("Microbial Species", "Metabolites", "Cognition and Behaviour"), 
                              x = c(1, 2, 3), 
                              y = c(0, 0, 0)), aes(x = x, y = y, label = l), size = 8) +
  
  geom_text(data = data.frame(lab = c("~R^{2}"), 
                              x = c(1.25 - 0.03125, 2.75 + 0.03125), 
                              y = c(1.650)), aes(x = x, y = y, label = lab), size = 5, parse = TRUE, hjust = 1/2) +
  
  
  scale_alpha_manual(values = c("sig" = 1, "weaksig" = 0.5)) +
  
  guides(fill = "none", alpha = "none", size = "none") +
  theme_void()  



