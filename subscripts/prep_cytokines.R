
unstim <- read.delim("raw/cytokines/cytokines_unstim.csv", sep = ",")
stim   <- read.delim("raw/cytokines/cytokines_stim.csv",   sep = ",")



unstim_df <- unstim %>% 

  pivot_longer(!c(Screening.ID, Group, Coffee.ID, Timepoint)) %>% 
  mutate(Legend = case_when(Group == 1   ~ "NCD",
                            Group == 2   ~ "CD", 
                            is.na(Group) ~ "CD")) %>%
  mutate(stim = "unstim") %>% 
  
  mutate(Legend = factor(Legend, levels = c("NCD", "CD"))) %>% 
  mutate(name = str_replace(name, "\\.", "-")) %>% 
  mutate(name = paste0(name, " (pg/mL)")) %>%
  mutate(name = str_replace(string = name, pattern = "CRP-.mg.L. \\(pg/mL\\)", replacement = "CRP (mg/L)")) %>% 
  mutate(name = factor(name, levels = c("IFN-γ (pg/mL)", "IL-6 (pg/mL)", "IL-8 (pg/mL)", 
                                        "IL-10 (pg/mL)", "TNF-α (pg/mL)", "IL-1β (pg/mL)", "CRP (mg/L)"))) %>% 
  dplyr::select(-c("Group"))


stim_df <- stim %>% 
  
  pivot_longer(!c(Screening.ID, Group, Coffee.ID, Timepoint)) %>% 
  mutate(Legend = case_when(Group == 1   ~ "NCD",
                            Group == 2   ~ "CD", 
                            is.na(Group) ~ "CD")) %>%
  mutate(stim = "stim") %>% 
  
  mutate(Legend = factor(Legend, levels = c("NCD", "CD"))) %>% 
  mutate(name = str_replace(name, "\\.", "-")) %>% 
  mutate(name = paste0(name, " (pg/mL)")) %>%
  mutate(name = str_replace(string = name, pattern = "CRP-.mg.L. \\(pg/mL\\)", replacement = "CRP (mg/L)")) %>% 
  mutate(name = factor(name, levels = c("IFN-γ (pg/mL)", "IL-6 (pg/mL)", "IL-8 (pg/mL)", 
                                        "IL-10 (pg/mL)", "TNF-α (pg/mL)", "IL-1β (pg/mL)", "CRP (mg/L)"))) %>% 
  dplyr::select(-c("Group")) 


ex1_cyta = rbind(stim_df, unstim_df) %>%
  
  filter(stim == "unstim") %>% 
  filter(Timepoint == "V2") %>% 

  mutate(name = factor(name, levels = c("IFN-γ (pg/mL)", "IL-6 (pg/mL)", "IL-8 (pg/mL)", 
                                        "IL-10 (pg/mL)", "TNF-α (pg/mL)", "CRP (mg/L)"))) %>% 
  
  group_by(name, Legend, stim, Timepoint) %>% 
  summarise(mean = mean(value, na.rm = T), 
            SEM   =   std(value)) %>% 
  ungroup() %>% 
  
  
  ggplot() +
  aes(y = mean, x = name, fill = Legend) +
  geom_errorbar(aes(ymin = mean - SEM, 
                    ymax = mean + SEM), 
                colour = "black", width = 1/4, position = position_dodge(1/3)) +
  geom_point(shape = 21, size = 3, position = position_dodge(1/3))+

  
  coord_flip()+
  scale_x_discrete(position = "top") +
  
  #Adjust appearance
  scale_fill_manual(values = c("CD" = "#94641f", 
                               "NCD"  = "#ece6ca")) +  
  theme_bw() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab(NULL) + xlab(NULL) +
  facet_wrap(~name, scales = "free", ncol = 1, drop = F)  +
  #ylim(c(0, NA)) + 
  scale_y_continuous(limits = c(0,NA), oob = scales::oob_keep )+

  guides(fill="none") +
  
  ggtitle("Unstimulated (plasma)")

ex1_cytb = rbind(stim_df, unstim_df) %>% 
  
  filter(stim == "stim") %>% 
  filter(Timepoint == "V2") %>% 
  
  mutate(value = value/1000) %>% 

  mutate(name = as.character(name), 
         name = str_replace(name, pattern = "pg", replacement = "ng")) %>% 
  mutate(name = factor(name, levels =  c("IL-6 (ng/mL)", "IL-8 (ng/mL)", "IL-10 (ng/mL)", 
                                         "TNF-α (ng/mL)", "IL-1β (ng/mL)", "CRP (mg/L)"))) %>% 
  
  group_by(name, Legend, stim, Timepoint) %>% 
  summarise(mean = mean(value, na.rm = T), 
            SEM   =   std(value)) %>% 
  ungroup() %>% 
  
  ggplot() +
  aes(y = mean, x = name, fill = Legend) +
  geom_errorbar(aes(ymin = mean - SEM, 
                    ymax = mean + SEM), 
                colour = "black", width = 1/4, position = position_dodge(1/3)) +
  geom_point(shape = 21, size = 3, position = position_dodge(1/3))+
  
  coord_flip()+
  scale_x_discrete(position = "top") +
  
  
  #Adjust appearance
  scale_fill_manual(values = c("CD" = "#94641f", 
                               "NCD"  = "#ece6ca")) +  
  theme_bw() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab(NULL) + xlab(NULL) +
  facet_wrap(~name, scales = "free", ncol = 1, drop = F)  +
  #ylim(c(0, NA)) + 
  scale_y_continuous(limits = c(0,NA), oob = scales::oob_keep ) +  
  guides(fill="none") +
  
  ggtitle("Stimulated (serum)")


ex2_cyta <- rbind(stim_df, unstim_df) %>% 
  
  filter(stim == "unstim") %>% 
  filter(Timepoint %in% c("V2", "V3")) %>% 
  mutate(Legend = paste(Legend, Timepoint, sep = "_"),
         Legend = case_when(Legend == "NCD_V2" ~ "Baseline (NCD)", 
                            Legend == "CD_V2"  ~ "Baseline (CD)", 
                            Legend == "CD_V3"  ~ "Post-washout (T14W)")) %>% 
  
  mutate(Legend = factor(Legend, levels = c("Baseline (CD)", "Post-washout (T14W)","Baseline (NCD)"))) %>% 
  
  mutate(name = factor(name, levels = c("IFN-γ (pg/mL)", "IL-6 (pg/mL)", "IL-8 (pg/mL)", 
                                        "IL-10 (pg/mL)", "TNF-α (pg/mL)", "CRP (mg/L)"))) %>% 
  
  group_by(name, Legend, stim, Timepoint) %>% 
  summarise(mean = mean(value, na.rm = T), 
            SEM   =   std(value)) %>% 
  ungroup() %>% 
  
  ggplot() +
  aes(y = mean, x = name, fill = Legend) +
  geom_errorbar(aes(ymin = mean - SEM, 
                    ymax = mean + SEM), 
                colour = "black", width = 1/4, position = position_dodge(2/3)) +
  geom_point(shape = 21, size = 3, position = position_dodge(2/3))+
  
  coord_flip()+
  scale_x_discrete(position = "top") +
  
  
  #Adjust appearance
  scale_fill_manual(values =   c("Baseline (CD)"       = "#543005",
                                 "Post-washout (T14W)" = "#ffa0a0", 
                                 "Baseline (NCD)"      = "#ece6ca")) +  
  
  theme_bw() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab(NULL) + xlab(NULL) +
  facet_wrap(~name, scales = "free", ncol = 1, drop = F)  +
  #ylim(c(0, NA)) + 
  scale_y_continuous(limits = c(0,NA), oob = scales::oob_keep ) +  
  guides(fill="none") +
  
  ggtitle("Unstimulated (plasma)")


ex2_cytb <- rbind(stim_df, unstim_df) %>% 
  
  mutate(value = value/1000) %>% 
  
  filter(stim == "stim") %>% 
  filter(Timepoint %in% c("V2", "V3")) %>% 
  mutate(Legend = paste(Legend, Timepoint, sep = "_"),
         Legend = case_when(Legend == "NCD_V2" ~ "Baseline (NCD)", 
                            Legend == "CD_V2"  ~ "Baseline (CD)", 
                            Legend == "CD_V3"  ~ "Post-washout (T14W)")) %>% 
  
  mutate(Legend = factor(Legend, levels = c("Baseline (CD)", "Post-washout (T14W)","Baseline (NCD)"))) %>% 
  
  mutate(name = as.character(name), 
         name = str_replace(name, pattern = "pg", replacement = "ng")) %>% 
  mutate(name = factor(name, levels =  c("IL-6 (ng/mL)", "IL-8 (ng/mL)", "IL-10 (ng/mL)", 
                                         "TNF-α (ng/mL)", "IL-1β (ng/mL)", "CRP (mg/L)"))) %>% 
  
  group_by(name, Legend, stim, Timepoint) %>% 
  summarise(mean = mean(value, na.rm = T), 
            SEM   =   std(value)) %>% 
  ungroup() %>% 
  
  ggplot() +
  aes(y = mean, x = name, fill = Legend) +
  geom_errorbar(aes(ymin = mean - SEM, 
                    ymax = mean + SEM), 
                colour = "black", width = 1/4, position = position_dodge(2/3)) +
  geom_point(shape = 21, size = 3, position = position_dodge(2/3))+
  
  coord_flip()+
  scale_x_discrete(position = "top") +
  
  
  #Adjust appearance
  scale_fill_manual(values =   c("Baseline (CD)"       = "#543005",
                                 "Post-washout (T14W)" = "#ffa0a0", 
                                 "Baseline (NCD)"      = "#ece6ca")) +  
  
  theme_bw() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab(NULL) + xlab(NULL) +
  facet_wrap(~name, scales = "free", ncol = 1, drop = F)  +
  #ylim(c(0, NA)) + 
  scale_y_continuous(limits = c(0,NA), oob = scales::oob_keep ) +  
  guides(fill="none") +
  
  ggtitle("Stimulated (serum)")



ex3_cyta <- rbind(stim_df, unstim_df) %>% 
  
  filter(stim == "unstim") %>% 
  filter(Timepoint %in% c("V3", "V4")) %>% 
  mutate(Legend = paste(Coffee.ID, Timepoint, sep = "_"),
         Legend = case_when(Legend == "F_V3"  ~ "Pre-Intervention (CAFF)", 
                            Legend == "S_V3"  ~ "Pre-Intervention (DECAF)", 
                            Legend == "F_V4"  ~ "Post-Intervention (CAFF)",
                            Legend == "S_V4"  ~ "Post-Intervention (DECAF)")) %>% 
  
  mutate(Legend = factor(Legend, levels = c("Pre-Intervention (CAFF)","Pre-Intervention (DECAF)", 
                                            "Post-Intervention (CAFF)","Post-Intervention (DECAF)"))) %>% 
  
  mutate(name = factor(name, levels = c("IFN-γ (pg/mL)", "IL-6 (pg/mL)", "IL-8 (pg/mL)", 
                                        "IL-10 (pg/mL)", "TNF-α (pg/mL)", "CRP (mg/L)"))) %>% 
  
  group_by(name, Legend, stim) %>% 
  summarise(mean = mean(value, na.rm = T), 
            SEM   =   std(value)) %>% 
  ungroup() %>% 
  
  ggplot() +
  aes(y = mean, x = name, fill = Legend) +
  geom_errorbar(aes(ymin = mean - SEM, 
                    ymax = mean + SEM), 
                colour = "black", width = 1/4, position = position_dodge(1)) +
  geom_point(shape = 21, size = 3, position = position_dodge(1))+
  
  coord_flip()+
  scale_x_discrete(position = "top") +
  
  
  #Adjust appearance
  scale_fill_manual(values =   c("Pre-Intervention (CAFF)"         = "#ffa0a0", 
                                 "Post-Intervention (CAFF)"        = "#5757f9",
                                 "Pre-Intervention (DECAF)"         = "#ffa0a0", 
                                 "Post-Intervention (DECAF)"        = "#5757f9")) +  
  
  theme_bw() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab(NULL) + xlab(NULL) +
  facet_wrap(~name, scales = "free", ncol = 1, drop = F)  +
  #ylim(c(0, NA)) + 
  scale_y_continuous(limits = c(0,NA), oob = scales::oob_keep )+
  guides(fill="none")+
  
  ggtitle("Unstimulated (plasma)")


ex3_cytb <- rbind(stim_df, unstim_df) %>% 
  
  filter(stim == "stim") %>% 
  filter(Timepoint %in% c("V3", "V4")) %>% 
  
  mutate(value = value/1000) %>% 
  
  mutate(Legend = paste(Coffee.ID, Timepoint, sep = "_"),
         Legend = case_when(Legend == "F_V3"  ~ "Pre-Intervention (CAFF)", 
                            Legend == "S_V3"  ~ "Pre-Intervention (DECAF)", 
                            Legend == "F_V4"  ~ "Post-Intervention (CAFF)",
                            Legend == "S_V4"  ~ "Post-Intervention (DECAF)")) %>% 
  
  mutate(Legend = factor(Legend, levels = c("Pre-Intervention (CAFF)","Pre-Intervention (DECAF)", 
                                            "Post-Intervention (CAFF)","Post-Intervention (DECAF)"))) %>% 
  
  mutate(name = as.character(name), 
         name = str_replace(name, pattern = "pg", replacement = "ng")) %>% 
  mutate(name = factor(name, levels =  c("IL-6 (ng/mL)", "IL-8 (ng/mL)", "IL-10 (ng/mL)", 
                                         "TNF-α (ng/mL)", "IL-1β (ng/mL)", "CRP (mg/L)"))) %>% 
  
  group_by(name, Legend, stim) %>% 
  summarise(mean = mean(value, na.rm = T), 
            SEM   =   std(value)) %>% 
  ungroup() %>% 
  
  ggplot() +
  aes(y = mean, x = name, fill = Legend) +
  geom_errorbar(aes(ymin = mean - SEM, 
                    ymax = mean + SEM), 
                colour = "black", width = 1/4, position = position_dodge(1)) +
  geom_point(shape = 21, size = 3, position = position_dodge(1))+
  
  coord_flip()+
  scale_x_discrete(position = "top") +
  
  
  #Adjust appearance
  scale_fill_manual(values =   c("Pre-Intervention (CAFF)"         = "#ffa0a0", 
                                 "Post-Intervention (CAFF)"        = "#5757f9",
                                 "Pre-Intervention (DECAF)"         = "#ffa0a0", 
                                 "Post-Intervention (DECAF)"        = "#5757f9")) +  
  
  theme_bw() + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab(NULL) + xlab(NULL) +
  facet_wrap(~name, scales = "free", ncol = 1, drop = F)  +
  #ylim(c(0, NA)) + 
  scale_y_continuous(limits = c(0,NA), oob = scales::oob_keep ) +  
  guides(fill="none") +
  
  ggtitle("Stimulated (serum)")



((ex1_cyta + ex1_cytb)|
    (ex2_cyta + ex2_cytb)|
    (ex3_cyta + ex3_cytb))
