library(xlsx)
#COGNITION----
measurement_type <- "cognition"
df_long_stats_input <- df_long_cog

#Exp 1 Baseline
df_long_stats_input %>% 
  
  filter(Visit == "Baseline") %>% 
  mutate(Coffee_Type = factor(Coffee_Type, levels = c("Coffee", "NCD"))) %>% 
group_by(name) %>% 
  reframe(
    lm(value ~ Coffee_Type, data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  mutate(term = "NCD") %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "Baseline")

#Exp 2 Washout v2 vs v3
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V3")) %>% 
  
  filter(! name %in% c("(ModRey) Tot", "(CWSQ) Tot", "(VASF) Fatigue", "(VASF) Energy", "(QCC) Tot")) %>% 
    
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-washout", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V3", append = TRUE)


#Exp 3 Reintroduction v3 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V3", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  filter(! name %in% c(
    "(ModRey) Tot"#,
    #"(CWSQ) Tot", 
    #"(VASF) Fatigue", 
    #"(VASF) Energy", 
    #"(QCC) Tot"
    )) %>%
 
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V3_vs_V4", append = TRUE)



#Exp 3b Reintroduction v2 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  filter(! name %in% c(
  #  "(ModRey) Tot"#,
    "(CWSQ) Tot", 
    "(VASF) Fatigue",
    "(VASF) Energy",
    "(QCC) Tot"
  )) %>%
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 

  write.xlsx(x = ., file = paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V4", append = TRUE)

#MICROBIOME_TAXA----
measurement_type <- "microbial_taxa"

metadata %>% 
  mutate(Visit = str_remove(Visit, "Visit "),
         Coffee_Type = coffee_group) %>% 
  
  
  mutate(Coffee_Type = case_when((visit %in% c("V2") & coffee_group == "CD" ) ~ "Coffee", 
                                 (visit %in% c("V2") & coffee_group == "NCD") ~ "NCD", 
                                 (visit %in% c("V3", "T2W", "T4W")) ~ "No Coffee", 
                                 (visit %in% c("V4", "T2I", "T4I", "T14I") & Treatment == "CAFF") ~ "CAF",
                                 (visit %in% c("V4", "T2I", "T4I", "T14I") & Treatment == "DECAF")~ "DECAF")) %>% 
  mutate(Visit = case_when(visit == "V2" ~ "Baseline", 
                           visit %in% c("V3", "T2W", "T4W") ~ "Post-\nwashout", 
                           visit %in% c("V4", "T2I", "T4I", "T14I") ~ "Post-\nreintroduction")) %>% 
  mutate(ID = str_replace(participant_ID, "X", "APC115-")) %>% 
  
  dplyr::select(ID, Metab_ID, R_ID, Visit, Coffee_Type, visit) -> base_long



base_long %>% 
  
  left_join(., species.exp %>% 
              as.data.frame() %>%  
              rownames_to_column("ID") %>%
              pivot_longer(!ID),
            
            by = c("R_ID" = "name")) %>% 
  
  
  rename(species_name = ID.y, species_value = value, ID = ID.x) -> species_long



species_long %>% 
  
  dplyr::select(ID, Visit, Coffee_Type, species_name, species_value, visit) %>% 
  
  
  
  pivot_longer(cols = c(ends_with("_name"), ends_with("_value")),
               names_to = c('type','.value' ),
               names_pattern = "(.*?)_(.*)") %>% 
  mutate(type = case_when(type == "species" ~ "Species",
                          type == "metabolite" ~ "Metabolites") ) %>% 
  mutate(visit = factor(visit, levels = c("V2", "T2W", "T4W", "V3", "T2I", "T4I", "T14I", "V4"))) %>% 
  
  group_by(name) %>%
  mutate(avg_cof = mean(value[Coffee_Type == "Coffee"], na.rm = T)) %>% 
  mutate(sd_tot = sd(value, na.rm = T)) %>% 
  mutate(value = (value - avg_cof)/sd_tot) %>% 
  ungroup() %>% 
  
  distinct() -> species_long


species_long %>% 
  # group_by(name, Coffee_Type, visit) %>% 
  # mutate(avg_by_caf_decaf = mean(value)) %>% 
  # ungroup() %>% 
  mutate(caffeine = case_when(
    ID %in% c("APC115-003", "APC115-005", "APC115-014", "APC115-017", "APC115-037", 
              "APC115-038", "APC115-039", "APC115-043", "APC115-054", "APC115-069", 
              "APC115-072", "APC115-087", "APC115-101", "APC115-108", "APC115-109") ~ "DECAF",
    ID %in% c("APC115-008", "APC115-011","APC115-016", "APC115-033","APC115-036", 
              "APC115-042", "APC115-052","APC115-058", "APC115-059", "APC115-060",
              "APC115-061","APC115-063","APC115-090","APC115-103","APC115-105","APC115-113") ~ "CAF")) %>% 
  mutate(caffeine = factor(caffeine, levels  = c("CAF", "DECAF"))) %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) -> df_long_stats_input


#Exp 1 Baseline
df_long_stats_input %>% 
  
  filter(Visit == "Baseline") %>% 
  mutate(Coffee_Type = factor(Coffee_Type, levels = c("Coffee", "NCD"))) %>% 
  group_by(name) %>% 
  reframe(
    lm(value ~ Coffee_Type, data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  mutate(term = "NCD") %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "Baseline")

#Exp 2 Washout v2 vs v3
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V3")) %>% 
  
  #filter(! name %in% c("(ModRey) Tot", "(CWSQ) Tot", "(VASF) Fatigue", "(VASF) Energy", "(QCC) Tot")) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-washout", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V3", append = TRUE)


#Exp 3 Reintroduction v3 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V3", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V3_vs_V4", append = TRUE)



#Exp 3b Reintroduction v2 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  
  write.xlsx(x = ., file = paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V4", append = TRUE)
#MICROBIOME_MODULES----
measurement_type <- "microbial_functional_modules"

metadata %>% 
  mutate(Visit = str_remove(Visit, "Visit "),
         Coffee_Type = coffee_group) %>% 
  
  
  mutate(Coffee_Type = case_when((visit %in% c("V2") & coffee_group == "CD" ) ~ "Coffee", 
                                 (visit %in% c("V2") & coffee_group == "NCD") ~ "NCD", 
                                 (visit %in% c("V3", "T2W", "T4W")) ~ "No Coffee", 
                                 (visit %in% c("V4", "T2I", "T4I", "T14I") & Treatment == "CAFF") ~ "CAF",
                                 (visit %in% c("V4", "T2I", "T4I", "T14I") & Treatment == "DECAF")~ "DECAF")) %>% 
  mutate(Visit = case_when(visit == "V2" ~ "Baseline", 
                           visit %in% c("V3", "T2W", "T4W") ~ "Post-\nwashout", 
                           visit %in% c("V4", "T2I", "T4I", "T14I") ~ "Post-\nreintroduction")) %>% 
  mutate(ID = str_replace(participant_ID, "X", "APC115-")) %>% 
  
  dplyr::select(ID, Metab_ID, R_ID, Visit, Coffee_Type, visit) -> base_long



base_long %>% 
  
  left_join(., rbind(GMMs.exp, GBMs.exp) %>% 
              as.data.frame() %>%  
              rownames_to_column("ID") %>%
              pivot_longer(!ID),
            
            by = c("R_ID" = "name")) %>% 
  
  
  rename(species_name = ID.y, species_value = value, ID = ID.x) -> species_long



species_long %>% 
  
  dplyr::select(ID, Visit, Coffee_Type, species_name, species_value, visit) %>% 
  
  
  
  pivot_longer(cols = c(ends_with("_name"), ends_with("_value")),
               names_to = c('type','.value' ),
               names_pattern = "(.*?)_(.*)") %>% 
  mutate(type = case_when(type == "species" ~ "Species",
                          type == "metabolite" ~ "Metabolites") ) %>% 
  mutate(visit = factor(visit, levels = c("V2", "T2W", "T4W", "V3", "T2I", "T4I", "T14I", "V4"))) %>% 
  
  group_by(name) %>%
  mutate(avg_cof = mean(value[Coffee_Type == "Coffee"], na.rm = T)) %>% 
  mutate(sd_tot = sd(value, na.rm = T)) %>% 
  mutate(value = (value - avg_cof)/sd_tot) %>% 
  ungroup() %>% 
  
  distinct() -> species_long


species_long %>% 
  # group_by(name, Coffee_Type, visit) %>% 
  # mutate(avg_by_caf_decaf = mean(value)) %>% 
  # ungroup() %>% 
  mutate(caffeine = case_when(
    ID %in% c("APC115-003", "APC115-005", "APC115-014", "APC115-017", "APC115-037", 
              "APC115-038", "APC115-039", "APC115-043", "APC115-054", "APC115-069", 
              "APC115-072", "APC115-087", "APC115-101", "APC115-108", "APC115-109") ~ "DECAF",
    ID %in% c("APC115-008", "APC115-011","APC115-016", "APC115-033","APC115-036", 
              "APC115-042", "APC115-052","APC115-058", "APC115-059", "APC115-060",
              "APC115-061","APC115-063","APC115-090","APC115-103","APC115-105","APC115-113") ~ "CAF")) %>% 
  mutate(caffeine = factor(caffeine, levels  = c("CAF", "DECAF"))) %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) -> df_long_stats_input


#Exp 1 Baseline
df_long_stats_input %>% 
  
  filter(Visit == "Baseline") %>% 
  mutate(Coffee_Type = factor(Coffee_Type, levels = c("Coffee", "NCD"))) %>% 
  group_by(name) %>% 
  reframe(
    lm(value ~ Coffee_Type, data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  mutate(term = "NCD") %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "Baseline")

#Exp 2 Washout v2 vs v3
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V3")) %>% 
  
  #filter(! name %in% c("(ModRey) Tot", "(CWSQ) Tot", "(VASF) Fatigue", "(VASF) Energy", "(QCC) Tot")) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-washout", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V3", append = TRUE)


#Exp 3 Reintroduction v3 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V3", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V3_vs_V4", append = TRUE)



#Exp 3b Reintroduction v2 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  
  write.xlsx(x = ., file = paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V4", append = TRUE)


#INFLAMMATORY CYTOKINES----
measurement_type <- "inflammatory_cytokines"

rbind(stim_df, unstim_df) %>% 
  
  mutate(Coffee_Type = case_when((Coffee.ID == "S" & Timepoint =="V4") ~ "CAF", 
                                 (Coffee.ID == "F" & Timepoint =="V4") ~ "DECAF", 
                                 Timepoint == "V3" ~ "No Coffee", 
                                 (Timepoint == "V2" & Legend == "NCD") ~ "NCD", 
                                 (Timepoint == "V2" & Legend == "CD") ~ "Coffee", 
                                 .default = NA)
  ) %>% 
  
  mutate(ID = str_remove(string = Screening.ID, pattern = "F$|S$")) %>% 
  mutate(Visit = case_when(Timepoint == "V2" ~ "Baseline", 
                           Timepoint %in% c("V3", "T2W", "T4W") ~ "Post-\nwashout", 
                           Timepoint %in% c("V4", "T2I", "T4I", "T14I") ~ "Post-\nreintroduction"), 
         visit = Timepoint) %>% 
  mutate(type = stim) %>% 
  
  dplyr::select(c(ID, name, Visit, value, type, Coffee_Type, visit)) %>% 
  mutate(visit = factor(visit, levels = c("V2", "V3", "V4"))) %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) %>% 
  
  mutate(caffeine = case_when(
    ID %in% c("APC115-003", "APC115-005", "APC115-014", "APC115-017", "APC115-037", 
              "APC115-038", "APC115-039", "APC115-043", "APC115-054", "APC115-069", 
              "APC115-072", "APC115-087", "APC115-101", "APC115-108", "APC115-109") ~ "DECAF",
    ID %in% c("APC115-008", "APC115-011","APC115-016", "APC115-033","APC115-036", 
              "APC115-042", "APC115-052","APC115-058", "APC115-059", "APC115-060",
              "APC115-061","APC115-063","APC115-090","APC115-103","APC115-105","APC115-113") ~ "CAF")) %>% 
  mutate(caffeine = factor(caffeine, levels  = c("CAF", "DECAF"))) %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) -> df_long_stats_input

#Exp 1 Baseline
df_long_stats_input %>% 
  
  filter(Visit == "Baseline") %>% 
  mutate(Coffee_Type = factor(Coffee_Type, levels = c("Coffee", "NCD"))) %>% 
  group_by(type, name) %>% 
  reframe(
    lm(value ~ Coffee_Type, data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  mutate(term = "NCD") %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "Baseline")

#Exp 2 Washout v2 vs v3
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V3")) %>% 
  
  #filter(! name %in% c("(ModRey) Tot", "(CWSQ) Tot", "(VASF) Fatigue", "(VASF) Energy", "(QCC) Tot")) %>% 
  
  group_by(type, name) %>% 
  reframe(
    lmer(value ~ Visit + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-washout", "sd(Intercept)", "sd(observations)"), times = length(unique(paste(type, name))))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V3", append = TRUE)


#Exp 3 Reintroduction v3 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V3", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  group_by(type, name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(paste(type, name))))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V3_vs_V4", append = TRUE)



#Exp 3b Reintroduction v2 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  group_by(type, name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(paste(type, name))))) %>% 
  
  write.xlsx(x = ., file = paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V4", append = TRUE)


#METABOLOMICS UNTARG----
measurement_type <- "metabolomics_untargeted"

#generate input-
metadata %>% 
  mutate(Visit = str_remove(Visit, "Visit "),
         Coffee_Type = coffee_group) %>% 
  
  
  mutate(Coffee_Type = case_when((visit %in% c("V2") & coffee_group == "CD" ) ~ "Coffee", 
                                 (visit %in% c("V2") & coffee_group == "NCD") ~ "NCD", 
                                 (visit %in% c("V3", "T2W", "T4W")) ~ "No Coffee", 
                                 (visit %in% c("V4", "T2I", "T4I", "T14I") & Treatment == "CAFF") ~ "CAF",
                                 (visit %in% c("V4", "T2I", "T4I", "T14I") & Treatment == "DECAF")~ "DECAF")) %>% 
  mutate(Visit = case_when(visit == "V2" ~ "Baseline", 
                           visit %in% c("V3", "T2W", "T4W") ~ "Post-\nwashout", 
                           visit %in% c("V4", "T2I", "T4I", "T14I") ~ "Post-\nreintroduction")) %>% 
  mutate(ID = str_replace(participant_ID, "X", "APC115-")) %>% 
  
  dplyr::select(ID, Metab_ID, R_ID, Visit, Coffee_Type, visit) -> base_long

omics_metabs <- metabs.exp %>% 
  as.data.frame() %>%  rownames_to_column("ID") %>% pivot_longer(!ID) %>% 
  rename(metabolite_name = ID, metabolite_value = value) %>%
  left_join(., metab_trans[,1:2], by = c("metabolite_name" = "Compound_ID")) 

base_long %>% 
  filter(Metab_ID != "") %>% 
  left_join(., omics_metabs, 
            by = c("Metab_ID" = "name")) %>% 
  mutate(metabolite_name = Name) %>% 
  dplyr::select(ID, Visit, Coffee_Type, metabolite_name, metabolite_value, visit) -> metab_long


metab_long %>% 
  
  pivot_longer(cols = c(ends_with("_name"), ends_with("_value")),
               names_to = c('type','.value' ),
               names_pattern = "(.*?)_(.*)") %>% 
  mutate(type = case_when(type == "species" ~ "Species",
                          type == "metabolite" ~ "Metabolites") ) %>% 
  mutate(visit = factor(visit, levels = c("V2", "V3", "V4"))) %>% 
  
  group_by(name) %>%
  mutate(avg_cof = mean(value[Coffee_Type == "Coffee"], na.rm = T)) %>% 
  mutate(sd_tot = sd(value, na.rm = T)) %>% 
  mutate(value = (value - avg_cof)/sd_tot) %>% 
  ungroup() %>% 
  
  
  distinct() -> metab_long

metab_long %>% 
  group_by(name, Coffee_Type) %>% 
  summarise(avg = mean(value)) %>% 
  pivot_wider(names_from = Coffee_Type, values_from = avg) %>% 
  
  mutate(no_caf = mean(DECAF, NCD, `No Coffee`), 
         caf    = mean(CAF, Coffee)) %>% 
  mutate(delta = no_caf - caf) %>% 
  dplyr::select(name, delta) %>% 
  ungroup() %>% 
  arrange(desc(abs(delta))) %>% .$name -> metab_order


metab_long %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) %>% 
  
  mutate(caffeine = case_when(
    ID %in% c("APC115-003", "APC115-005", "APC115-014", "APC115-017", "APC115-037", 
              "APC115-038", "APC115-039", "APC115-043", "APC115-054", "APC115-069", 
              "APC115-072", "APC115-087", "APC115-101", "APC115-108", "APC115-109") ~ "DECAF",
    ID %in% c("APC115-008", "APC115-011","APC115-016", "APC115-033","APC115-036", 
              "APC115-042", "APC115-052","APC115-058", "APC115-059", "APC115-060",
              "APC115-061","APC115-063","APC115-090","APC115-103","APC115-105","APC115-113") ~ "CAF")) %>% 
  mutate(caffeine = factor(caffeine, levels  = c("CAF", "DECAF"))) %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF")))  -> df_long_stats_input

#Exp 1 Baseline
df_long_stats_input %>% 
  
  filter(Visit == "Baseline") %>% 
  mutate(Coffee_Type = factor(Coffee_Type, levels = c("Coffee", "NCD"))) %>% 
  group_by(name) %>% 
  reframe(
    lm(value ~ Coffee_Type, data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  mutate(term = "NCD") %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "Baseline")

#Exp 2 Washout v2 vs v3
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V3")) %>% 
  
  #filter(! name %in% c("(ModRey) Tot", "(CWSQ) Tot", "(VASF) Fatigue", "(VASF) Energy", "(QCC) Tot")) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-washout", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V3", append = TRUE)


#Exp 3 Reintroduction v3 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V3", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V3_vs_V4", append = TRUE)



#Exp 3b Reintroduction v2 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  
  write.xlsx(x = ., file = paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V4", append = TRUE)




#METABOLOMICS URINE----
measurement_type <- "metabolomics_urine"

#generate input-
urmet_df_long <- urmet %>% 
  pivot_longer(!c(ID, visit, Coffee_Type))  %>% 
  mutate(Visit = case_when(visit == "V2" ~ "Baseline", 
                           visit %in% c("V3", "T2W", "T4W") ~ "Post-\nwashout", 
                           visit %in% c("V4", "T2I", "T4I", "T14I") ~ "Post-\nreintroduction"), 
         type = "Urine\nMetabolome") %>% 
  
  dplyr::select(c(ID, name, Visit, value, type, Coffee_Type, visit)) %>% 
  mutate(visit = factor(visit, levels = c("V2", "V3", "V4"))) %>% 
  
  group_by(name) %>%
  mutate(avg_cof = mean(value[Coffee_Type == "Coffee"], na.rm = T)) %>% 
  mutate(sd_tot = sd(value, na.rm = T)) %>% 
  mutate(value = (value - avg_cof)/sd_tot) %>% 
  ungroup()


urmet_df_long %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) %>% 
  
  mutate(caffeine = case_when(
    ID %in% c("APC115-003", "APC115-005", "APC115-014", "APC115-017", "APC115-037", 
              "APC115-038", "APC115-039", "APC115-043", "APC115-054", "APC115-069", 
              "APC115-072", "APC115-087", "APC115-101", "APC115-108", "APC115-109") ~ "DECAF",
    ID %in% c("APC115-008", "APC115-011","APC115-016", "APC115-033","APC115-036", 
              "APC115-042", "APC115-052","APC115-058", "APC115-059", "APC115-060",
              "APC115-061","APC115-063","APC115-090","APC115-103","APC115-105","APC115-113") ~ "CAF")) %>% 
  mutate(caffeine = factor(caffeine, levels  = c("CAF", "DECAF"))) %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) -> df_long_stats_input


#Exp 1 Baseline
df_long_stats_input %>% 
  
  filter(Visit == "Baseline") %>% 
  mutate(Coffee_Type = factor(Coffee_Type, levels = c("Coffee", "NCD"))) %>% 
  group_by(name) %>% 
  reframe(
    lm(value ~ Coffee_Type, data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  mutate(term = "NCD") %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "Baseline")

#Exp 2 Washout v2 vs v3
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V3")) %>% 
  
  #filter(! name %in% c("(ModRey) Tot", "(CWSQ) Tot", "(VASF) Fatigue", "(VASF) Energy", "(QCC) Tot")) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-washout", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V3", append = TRUE)


#Exp 3 Reintroduction v3 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V3", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  write.xlsx(x = ., paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V3_vs_V4", append = TRUE)



#Exp 3b Reintroduction v2 vs v4
df_long_stats_input %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V4")) %>% 
  
  mutate(caffeine = factor(caffeine, levels = c("DECAF", "CAF"))) %>% 
  
  group_by(name) %>% 
  reframe(
    lmer(value ~ Visit * caffeine + (1|ID), data = pick(everything())) %>% 
      tidy()
  ) %>% 
  
  ungroup() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(term) %>% 
  mutate(p.adj = p.adjust(p.value, method = "BH")) %>% 
  ungroup() %>% 
  mutate(term = rep(c("Post-reintroduction", "Caffeinated coffee",
                      "time x caffeine interaction", "sd(Intercept)", "sd(observations)"), times = length(unique(name)))) %>% 
  
  write.xlsx(x = ., file = paste0("new_stats/", measurement_type, "_stats_output.xlsx"), sheetName = "V2_vs_V4", append = TRUE)


