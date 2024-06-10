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
                   rownames_to_column("non_mb") %>% 
                   pivot_longer(!"non_mb", names_to = "Metabolites", values_to = "p.val") %>% 
                   left_join(., mxcog_R_marg %>% as.data.frame() %>% 
                               rownames_to_column("non_mb") %>% 
                               pivot_longer(!"non_mb", names_to = "Metabolites", values_to = "Rsq"), 
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
  
  filter(sig != "nonsig") %>% 
  
  mutate(metab_name    = .N()$label[from], 
         nonmetab_name = .N()$label[to]) %>% 
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
  
  ggplot() + 
  
  geom_diagonal_wide(aes(x = x_val, y = y_val, group = id, fill = metab_name, alpha = sig), colour = "black") +
  
  #metabolites
  geom_rect(data = data.frame(
    metab_names = colnames(MX), 
    xmin = 1.75, 
    xmax = 2.25,
    ymin = (1:9/9*1.5),# + (1.5 * 0.1) - 0.075,
    ymax = (1:9/9*1.5) + 0.1),
    aes(xmin = xmin, xmax = xmax, 
        ymin = ymin, ymax = ymax,
        fill = metab_names), 
    colour ="black") +
  
  geom_label(data = data.frame(
    met_labs = colnames(MX),
    x = 2,  
    y = (1:9/9*1.5) + 0.05), 
    aes(x = x, y = y, label = met_labs), alpha = 0.75, size = 5) + 
  #cognition
  geom_rect(data = data.frame(microbes = colnames(MB), 
                              xmin = 0.75, 
                              xmax = 1.25,
                              ymin = (1:7/7*1.5),# + (1.5 * 0.1) - 0.075,
                              ymax = (1:7/7*1.5) + 0.1),
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax),
            fill = "white", colour = "black") +
  
  geom_label(data = data.frame(
    mic_labs = colnames(MB),
    x = 1,  
    y = (1:7/7*1.5) + 0.05), 
    aes(x = x, y = y, label = mic_labs), alpha = 0.75, size = 5) + 
  
  #microbiome
  geom_rect(data = data.frame(cognition = colnames(cog), 
                              xmin = 2.75, 
                              xmax = 3.25,
                              ymin = (1:10/10*1.5),# + (1.5 * 0.1) - 0.075,
                              ymax = (1:10/10*1.5) + 0.1),
            aes(xmin = xmin, xmax = xmax, 
                ymin = ymin, ymax = ymax),
            fill = "white", colour = "black") +
  
  geom_label(data = data.frame(
    cog_labs = colnames(cog),
    x = 3,  
    y = (1:10/10*1.5) + 0.05), 
    aes(x = x, y = y, label = cog_labs), alpha = 0.75, size = 5) + 
  
  #Labels
  geom_rect(data = data.frame(l = c("Microbial Species", "Metabolites", "Cognition and Behaviour"), 
                              xmin = c(2/3, 5/3, 8/3),
                              xmax = c(4/3, 7/3, 10/3),
                              ymin = c(-0.1, -0.1, -0.1) *0.8, 
                              ymax = c(0.1, 0.1, 0.1)*0.8), aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), colour = "black", fill = "lightgray")+
  geom_text(data = data.frame(l = c("Microbial Species", "Metabolites", "Cognition and Behaviour"), 
                              x = c(1, 2, 3), 
                              y = c(0, 0, 0)), aes(x = x, y = y, label = l), size = 8) +
  
  scale_alpha_manual(values = c("sig" = 1, "weaksig" = 0.5)) +
  
  guides(fill = "none", alpha = "none") +
  theme_void()  



