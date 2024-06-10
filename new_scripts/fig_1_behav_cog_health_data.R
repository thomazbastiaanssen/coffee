# 1=NCD	0=F
# 2=CD	1=M

s1 = readxl::read_xlsx("raw/cognition/raw_xlsx/V2 - SPSS Template.xlsx")
s2 = readxl::read_xlsx("raw/cognition/raw_xlsx/V2 vs V3 - SPSS Template.xlsx")
s3 = readxl::read_xlsx("raw/cognition/raw_xlsx/V2 vs V4 - SPSS Template.xlsx")
s4 = readxl::read_xlsx("raw/cognition/raw_xlsx/V3 vs V4 - SPSS Template.xlsx")
si = readxl::read_xlsx("raw/cognition/raw_xlsx/Intervention timepoints - SPSS template.xlsx")
sw = readxl::read_xlsx("raw/cognition/raw_xlsx/Washout timepoints - SPSS template.xlsx")


colnames(s1) <- str_remove(colnames(s1), pattern = "V2_|v2_|V2-|v2-")

s1 <- s1 %>%   mutate(BMI  = as.numeric(BMI),
                      BP_D = as.numeric(BP_D))
s2 <- s2 %>%   mutate(BMI  = as.numeric(BMI),
                      BP_D = as.numeric(BP_D))
s3 <- s3 %>%   mutate(BMI  = as.numeric(BMI),
                      BP_D = as.numeric(BP_D))
s4 <- s4 %>%   mutate(BMI  = as.numeric(BMI),
                      BP_D = as.numeric(BP_D))


do.call(rbind, 
        list(s1[,1:3],
             s2[,1:3],
             s3[,c(1,3,2)],
             s4[,c(1,3,2)], 
             sw[,c(1,3,2)],
             si[,c(1,3,2)])) %>% 
  distinct() %>% 
  
  left_join(., s1, by = c("ID" = "ID", "visit" = "visit", "Coffee_Type" = "Coffee_Type"), keep = FALSE) %>% 
  left_join(., s2, by = c("ID" = "ID", "visit" = "visit", "Coffee_Type" = "Coffee_Type"), keep = FALSE) %>% 
  left_join(., s3, by = c("ID" = "ID", "visit" = "visit", "Coffee_Type" = "Coffee_Type"), keep = FALSE) %>% 
  left_join(., s4, by = c("ID" = "ID", "visit" = "visit", "Coffee_Type" = "Coffee_Type"), keep = FALSE) %>%
  left_join(., sw, by = c("ID" = "ID", "visit" = "visit", "Coffee_Type" = "Coffee_Type"), keep = FALSE) %>%
  left_join(., si , by = c("ID" = "ID", "visit" = "visit", "Coffee_Type" = "Coffee_Type"), keep = FALSE) %>% 
  
  
  
  pivot_longer(!c(ID, Coffee_Type, visit, Gender)) %>% 
  mutate(name = str_remove(name, "\\.x.*|\\.y.*")) %>% 
  mutate(name = str_remove(name, "V2_|v2_|V3_|v3_")) %>% 
  mutate(name = str_replace(name, "IPAQ-", "IPAQ_")) %>% 
  
  distinct() %>% 
  
  filter(!is.na(value)) %>% 
  
  mutate(visit = factor(visit, levels = c("V2", "T2W", "T4W", "V3", "T2I", "T4I", "T14", "V4"))) %>% 
  
  mutate(Visit = case_when(visit == "V2" ~ "Baseline", 
                           visit %in% c("T2W", "T4W", "V3") ~ "Post-\nwashout", 
                           visit %in% c("T2I", "T4I", "T14", "V4") ~ "Post-\nreintroduction")) %>% 
  dplyr::select(!Gender) %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) %>%
  
  
  filter(!str_detect(name, "7DFD")) %>% 
  filter(!str_detect(name, "BLVAS")) %>% 
  filter(!str_detect(name, "post-pre")) %>% 
  filter(str_detect(name, "UPPS|ERS|PASAT|MdR|STAI|PSS|BDI|GIS|SCL|PSQI|IPAQ|VAS|QCC|CWSQ")) %>% 
  mutate(name =  str_remove(name, "preSECPT_")) %>% 
  
  
  mutate(meas_group = case_when(str_detect(name, "7DFD_")  ~ "Dietary",
                                str_detect(name, "Blood")  ~ "Blood",
                                str_detect(name, "Plasma") ~ "Blood", 
                                str_detect(name, "Plasma") ~ "Blood", 
                                str_detect(name, "BP_D|BDI") ~ "Mood",
                                str_detect(name, "UPPS")       ~ "Impulsivity", 
                                str_detect(name, "SCL")        ~ "Mental Health", 
                                str_detect(name, "PASA")    ~ "Stress\n&\nAnxiety",
                                str_detect(name, "PASAT")      ~ "Attention", 
                                str_detect(name, "PSQI")       ~ "Sleep",
                                str_detect(name, "BMI|BSC|GIS_VAS")    ~ "Gastrointestinal\nHealth",
                                str_detect(name, "CAR|STAI|PSS")    ~ "Stress\n&\nAnxiety",
                                str_detect(name, "MdR")    ~ "Memory",
                                str_detect(name, "ERS")    ~ "Emotional Reactivity",
                                str_detect(name, "IPAQ")    ~ "Physical Activity",
                                str_detect(name, "CWSQ_Tot")     ~ "Coffee Withdrawal", 
                                str_detect(name, "VASF_Fatigue")  ~ "Coffee Withdrawal",
                                str_detect(name, "VASF_Energy")   ~ "Coffee Withdrawal",
                                str_detect(name, "QCC_Tot")       ~ "Coffee Withdrawal",
                                
                                
                                
                                .default = NA)
  ) %>% 
  filter(!is.na(meas_group)) %>% 
  
  
  mutate(name =  str_replace(name, "UPPS_|UPPS", "(UPPS) ")) %>% 
  mutate(name =  str_replace(name, "ERS_|ERS", "(ERS) ")) %>% 
  mutate(name =  str_replace(name, "MdR_", "(ModRey) ")) %>% 
  mutate(name =  str_replace(name, "SCL_", "(HSCL) ")) %>% 
  mutate(name =  str_replace(name, "GIS_VAS_Total", "(GIS-VAS) Tot ")) %>% 
  mutate(name =  str_replace(name, "PASAT_|PASAT", "(PASAT) ")) %>% 
  mutate(name =  str_replace(name, "STAI_TRAIT", "(STAI-T) ")) %>% 
  mutate(name =  str_replace(name, "PSS", "(PSS)")) %>% 
  mutate(name =  str_replace(name, "BDITot", "(BDI) Tot")) %>% 
  mutate(name =  str_replace(name, "IPAQ_MET", "(IPAQ) MET-minutes")) %>% 
  mutate(name =  str_replace(name, "PSQI_GlobalScore", "(PSQI) Tot")) %>% 
  mutate(name =  str_replace(name, "TotalA_F1", "Tot")) %>% 
  mutate(name =  str_replace(name, "Tot ", "Tot")) %>% 
  mutate(name =  str_replace(name, "CWSQ_Tot", "(CWSQ) Tot")) %>% 
  mutate(name =  str_replace(name, "QCC_Tot", "(QCC) Tot")) %>% 
  
  
  
  filter(!str_detect(name, "\\) [^TM]")) %>% 
  mutate(name =  str_replace(name, "VASF_", "(VASF) ")) %>% 
  
  mutate(name =  str_replace(name, "IPAQ-|IPAQ_", "(IPAQ) ")) %>% 
  #mutate(value = case_when(is.na(value)~ 0, .default = value)) %>% 
  
  mutate(meas_group = factor(meas_group, levels = c("Impulsivity", "Emotional Reactivity",
                                                    "Attention","Memory", "Stress\n&\nAnxiety", 
                                                    "Mood", "Mental Health", "Gastrointestinal\nHealth",
                                                    "Sleep", "Physical Activity", "Coffee Withdrawal"))) %>%
  filter(!is.na(meas_group)) %>% 
  
  

  group_by(name) %>%
  mutate(avg_cof = case_when(name  %in% c("(CWSQ) Tot", "(VASF) Fatigue", "(VASF) Energy", "(QCC) Tot") ~ mean(value[Coffee_Type == "CAF" & visit == "V4"], na.rm = T),
                             !name %in% c("(CWSQ) Tot", "(VASF) Fatigue", "(VASF) Energy", "(QCC) Tot") ~ mean(value[Coffee_Type == "Coffee"], na.rm = T), 
                             .default = NA)
         ) %>% 
  mutate(sd_tot = sd(value, na.rm = T)) %>% 
  mutate(value = (value - avg_cof)/sd_tot) %>% 
  ungroup() %>% 
  mutate(name = factor(name, levels = (c("(UPPS) Tot", "(ERS) Tot", "(ModRey) Tot", "(PASAT) Tot",
                                         "(PSS)", "(STAI-T) Tot", "(BDI) Tot", "(GIS-VAS) Tot", 
                                         "(PSQI) Tot", "(IPAQ) MET-minutes", "(CWSQ) Tot", "(VASF) Fatigue", "(VASF) Energy", "(QCC) Tot")))) %>% 
  
  mutate(caffeine = case_when(
    ID %in% c("APC115-003", "APC115-005", "APC115-014", "APC115-017", "APC115-037", 
              "APC115-038", "APC115-039", "APC115-043", "APC115-054", "APC115-069", 
              "APC115-072", "APC115-087", "APC115-101", "APC115-108", "APC115-109") ~ "DECAF",
    ID %in% c("APC115-008", "APC115-011","APC115-016", "APC115-033","APC115-036", 
              "APC115-042", "APC115-052","APC115-058", "APC115-059", "APC115-060",
              "APC115-061","APC115-063","APC115-090","APC115-103","APC115-105","APC115-113") ~ "CAF")) %>% 
  mutate(caffeine = factor(caffeine, levels  = c("CAF", "DECAF"))) %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) %>% 
  
  mutate(mdlab = case_when(Coffee_Type == "NCD" ~ "<img \n src='raw/icons/NCD.png' width='50' />",
                           Coffee_Type == "Coffee" ~ "<img \n src='raw/icons/CD.png' width='50' />",
                           Coffee_Type == "No Coffee" ~ "<img \n src='raw/icons/WASHOUT.png' width='50' />",
                           Coffee_Type == "DECAF" ~ "<img \n src='raw/icons/REINTRO.png' width='50' />", 
                           Coffee_Type == "CAF"   ~ "<img \n src='raw/icons/REINTRO.png' width='50' />"), 
         mdlab = factor(mdlab, levels = c("<img \n src='raw/icons/NCD.png' width='50' />", 
                                          "<img \n src='raw/icons/CD.png' width='50' />", 
                                          "<img \n src='raw/icons/WASHOUT.png' width='50' />", 
                                          "<img \n src='raw/icons/REINTRO.png' width='50' />"))
  )  %>% 
  
  group_by(name, visit, Coffee_Type) %>% 
  mutate(avg_by_caf_timepoint = mean(value)) %>%
  ungroup() %>% #filter(name == "Cryptobacterium curtum") %>% View
  
  group_by(name, visit, mdlab) %>% 
  mutate(avg_delta_caf_decaf = case_when(visit == "V2" ~ 0,
                                         visit != "V2" ~ abs(
                                           mean(avg_by_caf_timepoint[caffeine == "CAF"]) - mean(avg_by_caf_timepoint[caffeine == "DECAF"])
                                         ),
                                         .default = NA)
  ) %>% 
  
  mutate(plot_label = case_when((max(abs(avg_by_caf_timepoint)) > 0.5 | max(abs(avg_delta_caf_decaf)) > 0.5 ) ~ 'moderate', 
                                .default = "less")) %>% 
  ungroup() %>% 
  
  group_by(name, mdlab) %>%
  mutate(max_per_group = max(abs(avg_by_caf_timepoint))) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(abs_delta_NCD = mean(abs(avg_by_caf_timepoint[Coffee_Type == "NCD"]))) %>% 
  
  #Currently no filters for cognition
  # filter(any(plot_label == "moderate")) %>% 
  # filter(any(max_per_group > 0.5) ) %>% 
  # 
  # filter(abs_delta_NCD > 0.5) %>% 
  ungroup() -> df_long_cog


CAF_DECAF_order <- c(df_long_cog %>% 
                       filter(Visit == "Post-\nreintroduction") %>% 
                       filter(Coffee_Type == "CAF") %>% .$ID %>% sort %>% unique,
                     df_long_cog %>% 
                       filter(Visit == "Post-\nreintroduction") %>% 
                       filter(Coffee_Type == "DECAF") %>% .$ID %>% sort %>% unique)

  
plot_cog_NCD <- df_long_cog %>% 
  filter(Coffee_Type =="NCD") %>% 
  mutate(mdlab = factor("<img \n src='raw/icons/NCD.png' width='50' />", levels = c("<img \n src='raw/icons/NCD.png' width='50' />"))) %>%
  mutate(visit = factor("V2", levels = c("V2"))) %>% 
  mutate(lab = "NCD") %>% 
  
  group_by(Coffee_Type, name) %>% 
  mutate(avg_by_group = round(mean(value, na.rm = T), digits = 2)) %>%
  ungroup() %>% 
  # rbind(., c(ID = "APC115-001", visit = "V2", Coffee_Type = "NCD",  name = "(CWSQ) Tot", value = 0, Visit = "Baseline", meas_group = "Coffee Withdrawal", mdlab = "<img \n src='raw/icons/NCD.png' width='50' />", lab = "NCD")) %>%
  # rbind(., c(ID = "APC115-001", visit = "V2", Coffee_Type = "NCD",  name = "(VASF) Energy", value = 0, Visit = "Baseline", meas_group = "Coffee Withdrawal", mdlab = "<img \n src='raw/icons/NCD.png' width='50' />", lab = "NCD")) %>% 
  # rbind(., c(ID = "APC115-001", visit = "V2", Coffee_Type = "NCD",  name = "(VASF) Fatigue", value = 0, Visit = "Baseline", meas_group = "Coffee Withdrawal", mdlab = "<img \n src='raw/icons/NCD.png' width='50' />", lab = "NCD")) %>% 
  # rbind(., c(ID = "APC115-001", visit = "V2", Coffee_Type = "NCD",  name = "(QCC) Tot", value = 0, Visit = "Baseline", meas_group = "Coffee Withdrawal", mdlab = "<img \n src='raw/icons/NCD.png' width='50' />", lab = "NCD")) %>% 
  # 
  # mutate(value = as.numeric(value)) %>% 

  ggplot() +
  aes(y = ID, x = visit, fill = value, label = avg_by_group) + 
  geom_tile() +
  geom_label(data = . %>% group_by(visit, Coffee_Type, name, mdlab, meas_group) %>% 
               summarise(n = length(unique(ID)), 
                         avg_by_group = mean(avg_by_group)) %>% ungroup(), 
             # x = 1,
             aes(x = visit, y = n/2, fill = avg_by_group, label = avg_by_group)) +  
  #geom_point(shape = 24) +
  
  scale_fill_gradientn(colours = c(
    "#053061","#053061",
    "#2166ac","#2166ac",
    "#4393c3","#4393c3",
    "#f7f7f7","#f7f7f7", 
    "#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026", "#a50026"
    
    
  ), 
  limits = c(-4.5, 4.5), "Effect size (d)"
  ) +
  scale_y_discrete(position = "right") +
  scale_x_discrete(expand = expansion(mult = c(0))) +
  ggh4x::facet_nested(meas_group*name~mdlab, scales = "free", switch = "y",
                      strip = ggh4x::strip_nested(
                      #  text_y = list(element_text(), element_blank()),
                        background_y = list(element_blank(), element_rect()), 
                        by_layer_y = TRUE
                      )
  ) +
  
  # geom_image(x = 1, y = 3, image = "/home/thomaz/Downloads/Cryan, John 2015.png", size = 3)+
  xlab(NULL) + ylab(NULL) + 
  theme_test() +
  theme(strip.text.y.left = element_text(angle =0), 
        strip.text.x = element_markdown(angle = 1),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
       # axis.text.x = element_text(angle = 330, hjust = 0),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
       panel.spacing.x = unit(0, "lines")
  )



plot_cog_CD <- df_long_cog %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(visit %in% c("V2", "V3","V4")) %>% 
  filter(meas_group != "Coffee Withdrawal") %>% 
  mutate(caffeine = case_when(
    ID %in% c("APC115-003", "APC115-005", "APC115-014", "APC115-017", "APC115-037", 
              "APC115-038", "APC115-039", "APC115-043", "APC115-054", "APC115-069", 
              "APC115-072", "APC115-087", "APC115-101", "APC115-108", "APC115-109") ~ "DECAF",
    ID %in% c("APC115-008", "APC115-011","APC115-016", "APC115-033","APC115-036", 
              "APC115-042", "APC115-052","APC115-058", "APC115-059", "APC115-060",
              "APC115-061","APC115-063","APC115-090","APC115-103","APC115-105","APC115-113") ~ "CAF")) %>% 
  mutate(caffeine = factor(caffeine, levels  = c("CAF", "DECAF"))) %>% 
  mutate(lab = case_when(Visit == "Baseline" ~ "Coffee\n(baseline)", .default = Visit),
         lab = factor(lab, levels = c("Coffee\n(baseline)", "Post-\nwashout", "Post-\nreintroduction"))) %>%
  
  mutate(facet_lab = case_when(Visit == "Baseline" ~ "CD", 
                               Visit == "Post-\nwashout" ~ "wash\nout", 
                               Visit == "Post-\nreintroduction" ~ "inter\nvention"), 
         facet_lab = factor(facet_lab, levels = c("CD","wash\nout", "inter\nvention"))) %>% 
  
  group_by(Coffee_Type, caffeine, name) %>% 
  mutate(avg_by_group = round(mean(value, na.rm = T), digits = 2)) %>%
  
  ungroup() %>% 
  

  
  ggplot() +
  aes(y = ID, x = visit, fill = value, label = avg_by_group)+ 
  geom_tile() +
  
  geom_label(data = . %>% group_by(visit, Coffee_Type, caffeine, name, mdlab, plot_label) %>% 
               summarise(n = length(unique(ID)), 
                         avg_by_group = mean(avg_by_group)) %>% 
               ungroup() %>% group_by(name, mdlab) 
             %>% filter(any(plot_label == "moderate")) %>% ungroup(), 
             
             aes(x = visit, y = n/2, fill = avg_by_group, label = avg_by_group)) +

  scale_fill_gradientn(colours = c(
    "#053061","#053061",
    "#2166ac","#2166ac",
    "#4393c3","#4393c3",
    "#f7f7f7","#f7f7f7", 
    "#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026", "#a50026"
    

  ), 
  limits = c(-4.5, 4.5), "Effect size (d)"
  ) +
  scale_y_discrete(position = "right"  ) +

  scale_x_discrete(expand = expansion(mult = c(0))) +
  ggh4x::facet_nested(name * caffeine ~ mdlab, scales = "free", space = "free_x",
                      strip = ggh4x::strip_nested(background_y = list(element_blank(), element_rect(), element_rect()), 
                                                  text_y       = list(element_blank(), element_text(), element_text()), 
                                                  by_layer_y = TRUE )) +
  xlab(NULL) + ylab(NULL) + 
  theme_test() +
  theme(#strip.text.y = element_blank(), 
    strip.text.x = element_markdown(angle = 1), #,
    # strip.text.x = element_blank(), strip.background.x = element_blank()
    strip.text.y = element_text(angle =0),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
    #axis.text.x = element_text(angle = 330, hjust = 0)#,   panel.spacing.x =unit(0, "lines")
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(), 
    panel.spacing.x = unit(0, "lines")
  ) 


plot_cog_craving <- df_long_cog %>% 
  filter(Coffee_Type != "NCD") %>% 
  filter(name %in% c("(CWSQ) Tot", "(VASF) Fatigue", "(VASF) Energy", "(QCC) Tot")) %>% 
  mutate(caffeine = case_when(
    ID %in% c("APC115-003", "APC115-005", "APC115-014", "APC115-017", "APC115-037", 
              "APC115-038", "APC115-039", "APC115-043", "APC115-054", "APC115-069", 
              "APC115-072", "APC115-087", "APC115-101", "APC115-108", "APC115-109") ~ "DECAF",
    ID %in% c("APC115-008", "APC115-011","APC115-016", "APC115-033","APC115-036", 
              "APC115-042", "APC115-052","APC115-058", "APC115-059", "APC115-060",
              "APC115-061","APC115-063","APC115-090","APC115-103","APC115-105","APC115-113") ~ "CAF")) %>% 
  mutate(caffeine = factor(caffeine, levels  = c("CAF", "DECAF"))) %>% 
  mutate(lab = case_when(Visit == "Baseline" ~ "Coffee\n(baseline)", .default = Visit),
         lab = factor(lab, levels = c("Coffee\n(baseline)", "Post-\nwashout", "Post-\nreintroduction"))) %>%
  
  mutate(facet_lab = case_when(Visit == "Baseline" ~ "CD", 
                               Visit == "Post-\nwashout" ~ "wash\nout", 
                               Visit == "Post-\nreintroduction" ~ "inter\nvention"), 
         facet_lab = factor(facet_lab, levels = c("CD","wash\nout", "inter\nvention"))) %>% 
  
  group_by(Coffee_Type, caffeine, name, visit) %>% 
  mutate(avg_by_group = round(mean(value, na.rm = T), digits = 2)) %>%
  
  ungroup() %>% 
  
  
  
  ggplot() +
  aes(y = ID, x = visit, fill = value, label = avg_by_group)+ 
  geom_tile() +
  
  geom_label(data = . %>% group_by(visit, Coffee_Type, caffeine, name, mdlab, plot_label) %>% 
               summarise(n = length(unique(ID)), 
                         avg_by_group = mean(avg_by_group)) %>% 
               ungroup() %>% group_by(name, mdlab) 
             %>% filter(any(plot_label == "moderate")) %>% ungroup(), 
             
             aes(x = visit, y = n/2, fill = avg_by_group, label = avg_by_group)) +
  
  scale_fill_gradientn(colours = c(
    "#053061","#053061",
    "#2166ac","#2166ac",
    "#4393c3","#4393c3",
    "#f7f7f7","#f7f7f7", 
    "#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026", "#a50026"
    
    
  ), 
  limits = c(-4.5, 4.5), "Effect size (d)"
  ) +
  scale_y_discrete(position = "right"  ) +
  
  scale_x_discrete(expand = expansion(mult = c(0))) +
  ggh4x::facet_nested(name * caffeine ~ mdlab, scales = "free", space = "free_x", switch = "y",
                      strip = ggh4x::strip_nested(background_y = list(element_rect(), element_rect(), element_rect()), 
                                                  text_y       = list(element_text(), element_text(), element_text()), 
                                                  by_layer_y = TRUE )) +
  xlab(NULL) + ylab(NULL) + 
  theme_test() +
  theme(
    strip.text.y.left = element_text(angle =0), 
    strip.text.x = element_markdown(angle = 1), #,
    # strip.text.x = element_blank(), strip.background.x = element_blank()
    #strip.text.y = element_text(angle =0),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
    #axis.text.x = element_text(angle = 330, hjust = 0)#,   panel.spacing.x =unit(0, "lines")
    #axis.text.x = element_blank(), 
    #axis.ticks.x = element_blank(), 
    panel.spacing.x = unit(0, "lines")
  ) 
