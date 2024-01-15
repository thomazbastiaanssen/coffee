source("subscripts/prep_cognition.R")
################# EX1 ################ 

source("scripts/ex1_baseline_differences_CD_vs_NCD.R")

cog_df_ex1  <- cog_df[cog_df$Visit == "V2",]

cog_df_ex1 = cog_df_ex1 %>% 
  mutate(ID = str_replace(ID, pattern = "APC115-", replacement = "X")) %>% 
  
  pivot_wider(values_from = value, names_from = name)

meta_ex1_cog = meta_ex1 %>% 
  left_join(., cog_df_ex1, by = c("participant_ID" = "ID"))

res_list <- list()

for(cog in 4:(ncol(cog_df_ex1)-3)){

  meta_ex1_cog$X <- scale(unlist(meta_ex1_cog[, colnames(cog_df_ex1)[cog]]))
  
  tot <- fw_glm(t(scale(t(species.exp_ex1))), 
              f = ~ X, 
              metadata = meta_ex1_cog)[,c("feature", 
                                          "X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  # 
  # CD <- fw_glm(species.exp_ex1[,meta_ex1_cog$coffee_group == "CD"], 
  #               f = ~ X, 
  #               metadata = meta_ex1_cog[meta_ex1_cog$coffee_group == "CD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  # 
  # NCD <- fw_glm(species.exp_ex1[,meta_ex1_cog$coffee_group == "NCD"], 
  #              f = ~ X, 
  #              metadata = meta_ex1_cog[meta_ex1_cog$coffee_group == "NCD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  # 
  tot_df           = data.frame(tot) 
  colnames(tot_df) = c("feature", "B_tot", "p_tot","q_tot")
  tot_df$measure   = colnames(cog_df_ex1)[cog]
  res_list[[cog-3]] <- tot_df
  names(res_list)[[cog-3]] <- colnames(cog_df_ex1)[cog]
}


spec_long <- do.call(rbind, res_list) %>% 
  pivot_longer(!c(measure, feature)) %>% 
  
  separate(col = name, into = c("parameter", "group"), sep = "_") %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  
  filter(group == "tot") %>% 

  add_column(type = "Species")
  

################# EX1 metabs ################ 
res_list <- list()

for(cog in 4:(ncol(cog_df_ex1)-3)){
  
  meta_ex1_cog$X <- scale(unlist(meta_ex1_cog[, colnames(cog_df_ex1)[cog]]))
  
  tot <- fw_glm(t(scale(t(metabs.exp_exp1))), 
                f = ~ X, 
                metadata = meta_ex1_cog)[,c("feature",
                                            "X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  CD <- fw_glm(metabs.exp_exp1[,meta_ex1_cog$coffee_group == "CD"], 
               f = ~ X, 
               metadata = meta_ex1_cog[meta_ex1_cog$coffee_group == "CD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  NCD <- fw_glm(metabs.exp_exp1[,meta_ex1_cog$coffee_group == "NCD"], 
                f = ~ X, 
                metadata = meta_ex1_cog[meta_ex1_cog$coffee_group == "NCD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  tot_df           = data.frame(tot) 
  colnames(tot_df) = c("feature", "B_tot", "p_tot","q_tot")
  tot_df$measure   = colnames(cog_df_ex1)[cog]
  res_list[[cog-3]] <- tot_df
  names(res_list)[[cog-3]] <- colnames(cog_df_ex1)[cog]
}


metab_long <- do.call(rbind, res_list) %>% 
  pivot_longer(!c(measure, feature)) %>% 
  
  separate(col = name, into = c("parameter", "group"), sep = "_") %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  
  filter(group == "tot") %>% 
  
  left_join(., metab_trans[,c("Compound_ID", "Name")], by = c("feature" = "Compound_ID")) %>% 
  mutate(feature = Name) %>%  
  dplyr::select(!"Name") %>% 
  add_column(type = "Metabolomics")

################# EX1 GBMs ################ 
res_list <- list()

for(cog in 4:(ncol(cog_df_ex1)-3)){
  
  meta_ex1_cog$X <- scale(unlist(meta_ex1_cog[, colnames(cog_df_ex1)[cog]]))
  
  tot <- fw_glm(t(scale(t(GBMs.exp_ex1))), 
                f = ~ X, 
                metadata = meta_ex1_cog)[,c("feature",
                                            "X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  # CD <- fw_glm(GBMs.exp_ex1[,meta_ex1_cog$coffee_group == "CD"], 
  #              f = ~ X, 
  #              metadata = meta_ex1_cog[meta_ex1_cog$coffee_group == "CD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  # 
  # NCD <- fw_glm(GBMs.exp_ex1[,meta_ex1_cog$coffee_group == "NCD"], 
  #               f = ~ X, 
  #               metadata = meta_ex1_cog[meta_ex1_cog$coffee_group == "NCD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  tot_df           = data.frame(tot) 
  colnames(tot_df) = c("feature",  "B_tot", "p_tot","q_tot")
  tot_df$measure   = colnames(cog_df_ex1)[cog]
  res_list[[cog-3]] <- tot_df
  names(res_list)[[cog-3]] <- colnames(cog_df_ex1)[cog]
}


GBM_long <- do.call(rbind, res_list) %>% 
  pivot_longer(!c(measure, feature)) %>% 
  
  separate(col = name, into = c("parameter", "group"), sep = "_") %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  
  filter(group == "tot") %>% 
  
  add_column(type = "GBMs")


ex1_res <- do.call(rbind, list(spec_long, metab_long, GBM_long)) %>% 
  add_column(Experiment = "Ex1")

  



################# EX2 ################ 

source("scripts/ex_2_acute_and_full_withdrawal.R")


cog_df_ex2  <- cog_df[cog_df$Visit %in% c("V2", "T2W", "T4W", "V3"),]

cog_df_ex2 = cog_df_ex2 %>% 
  mutate(ID = str_replace(ID, pattern = "APC115-", replacement = "X")) %>% 
  
  pivot_wider(values_from = value, names_from = name) %>% 
  mutate(link = paste(ID, Visit))

meta_ex2_cog = meta_ex_withdraw %>% 
  mutate(link = paste(participant_ID, visit)) %>% 
  left_join(., cog_df_ex2, by = c("link" = "link")) %>% 
  dplyr::select(-"link")

cog_df_ex2 = cog_df_ex2 %>% 
  dplyr::select(-"link")


res_list <- list()

for(cog in 4:(ncol(cog_df_ex2)-3)){
  
  meta_ex2_cog$X <- scale(unlist(meta_ex2_cog[, colnames(cog_df_ex2)[cog]]))
  
  tot <- fw_glmer(t(scale(t(species.exp_ex_withdraw))), 
                f = ~ X + (1|participant_ID), 
                metadata = meta_ex2_cog)[,c("feature", "X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  # CD <- fw_glm(species.exp_ex_withdraw[,meta_ex2_cog$coffee_group == "CD"], 
  #              f = ~ X, 
  #              metadata = meta_ex2_cog[meta_ex2_cog$coffee_group == "CD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  # 
  # NCD <- fw_glm(species.exp_ex_withdraw[,meta_ex2_cog$coffee_group == "NCD"], 
  #               f = ~ X, 
  #               metadata = meta_ex2_cog[meta_ex2_cog$coffee_group == "NCD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  tot_df           = data.frame(tot) 
  colnames(tot_df) = c("feature", "B_tot", "p_tot","q_tot")
  tot_df$measure   = colnames(cog_df_ex2)[cog]
  res_list[[cog-3]] <- tot_df
  names(res_list)[[cog-3]] <- colnames(cog_df_ex2)[cog]
}


spec_long <- do.call(rbind, res_list) %>% 
  pivot_longer(!c(measure, feature)) %>% 
  
  separate(col = name, into = c("parameter", "group"), sep = "_") %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  
  filter(group == "tot") %>% 

  add_column(type = "Species")


################# EX2 metabs ################ 
res_list <- list()

for(cog in 4:(ncol(cog_df_ex2)-3)){
  
  meta_ex2_cog$X <- scale(unlist(meta_ex2_cog[, colnames(cog_df_ex2)[cog]]))
  
  tot <- fw_glm(t(scale(t(metabs.exp_ex_REST))), 
                f = ~ X, 
                metadata = meta_ex2_cog[meta_ex2_cog$visit %in% c("V2", "V3"),])[,c("feature", "X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  
  # CD <- fw_glm(metabs.exp_exp2[,meta_ex2_cog$coffee_group == "CD"], 
  #              f = ~ X, 
  #              metadata = meta_ex2_cog[meta_ex2_cog$coffee_group == "CD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  # 
  # NCD <- fw_glm(metabs.exp_exp2[,meta_ex2_cog$coffee_group == "NCD"], 
  #               f = ~ X, 
  #               metadata = meta_ex2_cog[meta_ex2_cog$coffee_group == "NCD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  tot_df           = data.frame(tot) 
  colnames(tot_df) = c("feature", "B_tot", "p_tot","q_tot")
  tot_df$measure   = colnames(cog_df_ex2)[cog]
  res_list[[cog-3]] <- tot_df
  names(res_list)[[cog-3]] <- colnames(cog_df_ex2)[cog]
}


metab_long <- do.call(rbind, res_list) %>% 
  pivot_longer(!c(measure, feature)) %>% 
  
  separate(col = name, into = c("parameter", "group"), sep = "_") %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  
  filter(group == "tot") %>%
  
  left_join(., metab_trans[,c("Compound_ID", "Name")], by = c("feature" = "Compound_ID")) %>% 
  mutate(feature = Name) %>%  
  dplyr::select(!"Name") %>% 
  add_column(type = "Metabolomics")

################# EX2 GBMs ################ 
res_list <- list()

for(cog in 4:(ncol(cog_df_ex2)-3)){
  
  meta_ex2_cog$X <- scale(unlist(meta_ex2_cog[, colnames(cog_df_ex2)[cog]]))
  
  tot <- fw_glmer(t(scale(t(GBMs.exp_ex_withdraw))), 
                  f = ~ X + (1|participant_ID), 
                  metadata = meta_ex2_cog)[,c("feature", "X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  # CD <- fw_glm(GBMs.exp_ex_withdraw[,meta_ex2_cog$coffee_group == "CD"], 
  #              f = ~ X, 
  #              metadata = meta_ex2_cog[meta_ex2_cog$coffee_group == "CD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  # 
  # NCD <- fw_glm(GBMs.exp_ex_withdraw[,meta_ex2_cog$coffee_group == "NCD"], 
  #               f = ~ X, 
  #               metadata = meta_ex2_cog[meta_ex2_cog$coffee_group == "NCD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  tot_df           = data.frame(tot) 
  colnames(tot_df) = c("feature", "B_tot", "p_tot","q_tot")
  tot_df$measure   = colnames(cog_df_ex2)[cog]
  res_list[[cog-3]] <- tot_df
  names(res_list)[[cog-3]] <- colnames(cog_df_ex2)[cog]
}


GBM_long <- do.call(rbind, res_list) %>% 
  pivot_longer(!c(measure, feature)) %>% 
  
  separate(col = name, into = c("parameter", "group"), sep = "_") %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  
  filter(group == "tot") %>% 
  add_column(type = "GBMs")


ex2_res <- do.call(rbind, list(spec_long, metab_long, GBM_long)) %>% 
  add_column(Experiment = "Ex2")





################# EX3a ################ 

source("scripts/ex_3_intervention_acute_and_full_recaf_or_decaf.R")


cog_df_ex3 <- cog_df[cog_df$Visit %in% c("V3", "T2I", "T4I", "T14I", "V4"),]

cog_df_ex3 = cog_df_ex3 %>% 
  mutate(ID = str_replace(ID, pattern = "APC115-", replacement = "X")) %>% 
  
  pivot_wider(values_from = value, names_from = name) %>% 
  mutate(link = paste(ID, Visit)) 

meta_ex3_cog = meta_ex_INTERVENTION %>% 
  mutate(link = paste(participant_ID, visit)) %>% 
  left_join(., cog_df_ex3, by = c("link" = "link")) %>% 
  dplyr::select(-"link")

cog_df_ex3 = cog_df_ex3 %>% 
  dplyr::select(-"link")

res_list <- list()

for(cog in 4:(ncol(cog_df_ex3)-3)){
  
  meta_ex3_cog$X <- scale(unlist(meta_ex3_cog[, colnames(cog_df_ex3)[cog]]))
  
  submeta <- meta_ex3_cog[!is.na(meta_ex3_cog$X),]
  subtabl <- species.exp_ex_INTERVENTION[,submeta$R_ID]
  
  tot <- fw_glmer(t(scale(t(subtabl))), 
                f = ~ X + (1|participant_ID), 
                metadata = submeta)[,c("feature", "X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  # CD <- fw_glm(species.exp_ex_withdraw[,meta_ex3_cog$coffee_group == "CD"], 
  #              f = ~ X, 
  #              metadata = meta_ex3_cog[meta_ex3_cog$coffee_group == "CD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  # 
  # NCD <- fw_glm(species.exp_ex_withdraw[,meta_ex3_cog$coffee_group == "NCD"], 
  #               f = ~ X, 
  #               metadata = meta_ex3_cog[meta_ex3_cog$coffee_group == "NCD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  tot_df           = data.frame(tot) 
  colnames(tot_df) = c("feature", "B_tot", "p_tot","q_tot")
  tot_df$measure   = colnames(cog_df_ex3)[cog]
  res_list[[cog-3]] <- tot_df
  names(res_list)[[cog-3]] <- colnames(cog_df_ex3)[cog]
}


spec_long <- do.call(rbind, res_list) %>% 
  pivot_longer(!c(measure, feature)) %>% 
  
  separate(col = name, into = c("parameter", "group"), sep = "_") %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  
  filter(group == "tot") %>% 
  
  add_column(type = "Species")


################# ex3 metabs ################ 
res_list <- list()

for(cog in 4:(ncol(cog_df_ex3)-3)){
  
  meta_ex3_cog$X <- scale(unlist(meta_ex3_cog[, colnames(cog_df_ex3)[cog]]))
  
  submeta <- meta_ex3_cog[!is.na(meta_ex3_cog$X) & meta_ex3_cog$Metab_ID != "",]
  subtabl <- metabs.exp_ex3[,submeta$Metab_ID]
  
  tot <- fw_glm(t(scale(t(subtabl))), 
                f = ~ X, 
                metadata = submeta)[,c("feature", "X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  # CD <- fw_glm(metabs.exp_exp2[,meta_ex3_cog$coffee_group == "CD"], 
  #              f = ~ X, 
  #              metadata = meta_ex3_cog[meta_ex3_cog$coffee_group == "CD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  # 
  # NCD <- fw_glm(metabs.exp_exp2[,meta_ex3_cog$coffee_group == "NCD"], 
  #               f = ~ X, 
  #               metadata = meta_ex3_cog[meta_ex3_cog$coffee_group == "NCD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  tot_df           = data.frame(tot) 
  colnames(tot_df) = c("feature", "B_tot", "p_tot","q_tot")
  tot_df$measure   = colnames(cog_df_ex3)[cog]
  res_list[[cog-3]] <- tot_df
  names(res_list)[[cog-3]] <- colnames(cog_df_ex3)[cog]
}


metab_long <- do.call(rbind, res_list) %>% 
  pivot_longer(!c(measure, feature)) %>% 
  
  separate(col = name, into = c("parameter", "group"), sep = "_") %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  
  filter(group == "tot") %>%
  
  left_join(., metab_trans[,c("Compound_ID", "Name")], by = c("feature" = "Compound_ID")) %>% 
  mutate(feature = Name) %>%  
  dplyr::select(!"Name") %>% 
  add_column(type = "Metabolomics")

################# EX3 GBMs ################ 
res_list <- list()

for(cog in 4:(ncol(cog_df_ex3)-3)){

  meta_ex3_cog$X <- scale(unlist(meta_ex3_cog[, colnames(cog_df_ex3)[cog]]))
  
  submeta <- meta_ex3_cog[!is.na(meta_ex3_cog$X),]
  subtabl <- GBMs.exp_ex_INTERVENTION[,submeta$R_ID]
  
  tot <- fw_glmer(t(scale(t(subtabl))), 
                f = ~ X + (1|participant_ID), 
                metadata = submeta)[,c("feature", "X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  # CD <- fw_glm(GBMs.exp_ex_withdraw[,meta_ex3_cog$coffee_group == "CD"], 
  #              f = ~ X, 
  #              metadata = meta_ex3_cog[meta_ex3_cog$coffee_group == "CD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  # 
  # NCD <- fw_glm(GBMs.exp_ex_withdraw[,meta_ex3_cog$coffee_group == "NCD"], 
  #               f = ~ X, 
  #               metadata = meta_ex3_cog[meta_ex3_cog$coffee_group == "NCD",])[,c("X Estimate","X Pr(>|t|)","X Pr(>|t|).BH")]
  
  tot_df           = data.frame(tot) 
  colnames(tot_df) = c("feature", "B_tot", "p_tot","q_tot")
  tot_df$measure   = colnames(cog_df_ex3)[cog]
  res_list[[cog-3]] <- tot_df
  names(res_list)[[cog-3]] <- colnames(cog_df_ex3)[cog]
}


GBM_long <- do.call(rbind, res_list) %>% 
  pivot_longer(!c(measure, feature)) %>% 
  
  separate(col = name, into = c("parameter", "group"), sep = "_") %>% 
  pivot_wider(names_from = parameter, values_from = value) %>% 
  
  filter(group == "tot") %>% 
  add_column(type = "GBMs")


ex3_res <- do.call(rbind, list(spec_long, metab_long, GBM_long)) %>% 
  add_column(Experiment = "Ex3")

fig_1 <- do.call(rbind, list(ex1_res, ex2_res, ex3_res)) %>% 
  
  filter(Experiment == "Ex1") %>% 
  #filter(measure != "ERS") %>% 
  group_by(type, measure) %>% 
  mutate(q = p.adjust(p, method = "BH")) %>% 
  ungroup() %>% 
  
  group_by(feature) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 0.1) %>% 
  ungroup() %>% 
  group_by(measure) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 0.1) %>% 
  ungroup() %>% 
  
  mutate(star = case_when(
    p < 0.001 & q < 0.1 ~ "⁂",
    p < 0.01  & q < 0.1 ~ "⁎⁎", 
    p < 0.05  & q < 0.1 ~ "⁎", 
    .default = "")) %>% 
  

  
  ggplot() +
  
  aes(x = measure, y = feature, fill = B, label = star) +
  geom_tile() +
  #geom_point(shape = 21, size = 8) +
  geom_text(colour = "black")+
  
  scale_y_discrete(position = "right") +
  
  scale_fill_gradientn(colours = c(
    "#053061",
    "#2166ac", "#2166ac",
    "#4393c3","#4393c3", "#4393c3",
    "#f7f7f7","#f7f7f7",  "#f7f7f7",  "#f7f7f7",  
    "#d6604d","#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026" 
  ), limits = c(-0.75, 0.75), breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), "Association\n(Standardized β)")+
  ggforce::facet_col(~type,  scales = "free", space = "free") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_bw()



fig_2 <- do.call(rbind, list(ex1_res, ex2_res, ex3_res)) %>% 
  
  #filter(measure != "ERS") %>% 
  filter(Experiment == "Ex2") %>% 
  group_by(type, measure) %>% 
  mutate(q = p.adjust(p, method = "BH")) %>% 
  ungroup() %>% 
  
  group_by(feature) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 0.1) %>% 
  ungroup() %>% 
  group_by(measure) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 0.1) %>% 
  ungroup() %>% 
  
  mutate(star = case_when(
    p < 0.001 & q < 0.1 ~ "⁂",
    p < 0.01  & q < 0.1 ~ "⁎⁎", 
    p < 0.05  & q < 0.1 ~ "⁎", 
    .default = "")) %>% 
  
  ggplot() +
  
  aes(x = measure, y = feature, fill = B, label = star) +
  geom_tile() +
  #geom_point(shape = 21, size = 8) +
  geom_text(colour = "black")+
  
  scale_y_discrete(position = "right") +
  
  scale_fill_gradientn(colours = c(
    "#053061",
    "#2166ac", "#2166ac",
    "#4393c3","#4393c3", "#4393c3",
    "#f7f7f7","#f7f7f7",  "#f7f7f7",  "#f7f7f7",  
    "#d6604d","#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026" 
  ), limits = c(-0.75, 0.75), breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), "Association\n(Standardized β)")+
  ggforce::facet_col(~type,  scales = "free", space = "free") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_bw()

fig_3a <- do.call(rbind, list(ex1_res, ex2_res, ex3_res)) %>% 
  
  filter(measure != "ERS") %>% 
  filter(!measure %in% c("QCC_AnticM2", "QCC_StrDes1")) %>% 
  filter(Experiment == "Ex3") %>% 
  group_by(type, measure) %>% 
  mutate(q = p.adjust(p, method = "BH")) %>% 
  ungroup() %>% 

  group_by(feature) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 1/3) %>% 
  ungroup() %>% 
  group_by(measure) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 1/3) %>% 
  ungroup() %>% 
  
  mutate(star = case_when(
    p < 0.001 & q < 0.1 ~ "⁂",
    p < 0.01  & q < 0.1 ~ "⁎⁎", 
    p < 0.05  & q < 0.1 ~ "⁎", 
    .default = "")) %>% 
  
  ggplot() +
  
  aes(x = measure, y = feature, fill = B, label = star) +
  geom_tile() +
  #geom_point(shape = 21, size = 8) +
  geom_text(colour = "black")+
  
  scale_y_discrete(position = "right") +
  
  scale_fill_gradientn(colours = c(
    "#053061",
    "#2166ac", "#2166ac",
    "#4393c3","#4393c3", "#4393c3",
    "#f7f7f7","#f7f7f7",  "#f7f7f7",  "#f7f7f7",  
    "#d6604d","#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026" 
  ), limits = c(-0.75, 0.75), breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), "Association\n(Standardized β)")+
  ggforce::facet_col(~type,  scales = "free", space = "free") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_bw()


fig_3b <- do.call(rbind, list(ex1_res, ex2_res, ex3_res)) %>% 
  

  filter(Experiment == "Ex3") %>% 
  filter(measure == "ERS") %>% 
  group_by(type, measure) %>% 
  mutate(q = p.adjust(p, method = "BH")) %>% 
  ungroup() %>% 
  
  group_by(feature) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 1/3) %>% 
  ungroup() %>% 
  group_by(measure) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 1/3) %>% 
  ungroup() %>% 
  
  mutate(star = case_when(
    p < 0.001 & q < 0.1 ~ "⁂",
    p < 0.01  & q < 0.1 ~ "⁎⁎", 
    p < 0.05  & q < 0.1 ~ "⁎", 
    .default = "")) %>% 
  
  ggplot() +
  
  aes(x = measure, y = feature, fill = B, label = star) +
  geom_tile() +
  #geom_point(shape = 21, size = 8) +
  geom_text(colour = "black")+
  
  scale_y_discrete(position = "right") +
  
  scale_fill_gradientn(colours = c(
    "#053061",
    "#2166ac", "#2166ac",
    "#4393c3","#4393c3", "#4393c3",
    "#f7f7f7","#f7f7f7",  "#f7f7f7",  "#f7f7f7",  
    "#d6604d","#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026" 
  ), limits = c(-0.75, 0.75), breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), "Association\n(Standardized β)")+
  ggforce::facet_col(~type,  scales = "free", space = "free") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_bw()


fig_3b <- do.call(rbind, list(ex1_res, ex2_res, ex3_res)) %>% 
  
  filter(type != "Species") %>% 
  filter(Experiment == "Ex3") %>% 
  filter(measure == "ERS") %>% 
  group_by(type, measure) %>% 
  mutate(q = p.adjust(p, method = "BH")) %>% 
  ungroup() %>% 
  
  group_by(feature) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 1/3) %>% 
  ungroup() %>% 
  group_by(measure) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 1/3) %>% 
  ungroup() %>% 
  
  mutate(star = case_when(
    p < 0.001 & q < 0.1 ~ "⁂",
    p < 0.01  & q < 0.1 ~ "⁎⁎", 
    p < 0.05  & q < 0.1 ~ "⁎", 
    .default = "")) %>% 
  
  ggplot() +
  
  aes(x = measure, y = feature, fill = B, label = star) +
  geom_tile() +
  #geom_point(shape = 21, size = 8) +
  geom_text(colour = "black")+
  
  scale_y_discrete(position = "right") +
  
  scale_fill_gradientn(colours = c(
    "#053061",
    "#2166ac", "#2166ac",
    "#4393c3","#4393c3", "#4393c3",
    "#f7f7f7","#f7f7f7",  "#f7f7f7",  "#f7f7f7",  
    "#d6604d","#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026" 
  ), limits = c(-0.75, 0.75), breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), "Association\n(Standardized β)")+
  ggforce::facet_col(~type, scales = "free", space = "free") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_bw()

fig_3c <- do.call(rbind, list(ex1_res, ex2_res, ex3_res)) %>% 
  
  filter(type == "Species") %>% 
  filter(Experiment == "Ex3") %>% 
  filter(measure == "ERS") %>% 
  group_by(type, measure) %>% 
  mutate(q = p.adjust(p, method = "BH")) %>% 
  ungroup() %>% 
  
  group_by(feature) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 1/3) %>% 
  ungroup() %>% 
  group_by(measure) %>% 
  filter(min(q) < 0.1 & min(p) < 0.05 & max(abs(B), na.rm = TRUE) > 1/3) %>% 
  ungroup() %>% 
  
  mutate(star = case_when(
    p < 0.001 & q < 0.1 ~ "⁂",
    p < 0.01  & q < 0.1 ~ "⁎⁎", 
    p < 0.05  & q < 0.1 ~ "⁎", 
    .default = "")) %>% 
  
  ggplot() +
  
  aes(x = measure, y = feature, fill = B, label = star) +
  geom_tile() +
  #geom_point(shape = 21, size = 8) +
  geom_text(colour = "black")+
  
  scale_y_discrete(position = "right") +
  
  scale_fill_gradientn(colours = c(
    "#053061",
    "#2166ac", "#2166ac",
    "#4393c3","#4393c3", "#4393c3",
    "#f7f7f7","#f7f7f7",  "#f7f7f7",  "#f7f7f7",  
    "#d6604d","#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026" 
  ), limits = c(-0.75, 0.75), breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75), "Association\n(Standardized β)")+
  facet_grid(~type, scales = "free", space = "free", switch = "y") +
  xlab(NULL) + 
  ylab(NULL) +
  theme_bw()


((
  (fig_1 + ggtitle("Associations at Baseline"))/ 
    (fig_2 + ggtitle("Associations during Washout"))/ 
    (fig_3a + ggtitle("Associations during Intervention (non-ERS)")) + plot_layout(heights = c(1,2,3))
  ) | 
    fig_3b + ggtitle("Associations during Intervention (ERS)")) +
  plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")



f_1_2 <- ((fig_1 + ggtitle("Associations at Baseline") + guides(fill="none"))/ 
    (fig_2 + ggtitle("Associations during Washout") + guides(fill="none"))) + plot_layout(guides = "collect") 

f_3tot <- (fig_3a + ggtitle("Associations during Intervention (non-ERS)") | 
  fig_3b + ggtitle("Associations during Intervention (ERS)") |
  fig_3c + ggtitle("Associations during Intervention (ERS)")) + plot_layout(guides = "collect", widths = c(4,1,1))




