# 1=NCD	0=F
# 2=CD	1=M

# #1
# c("Cholic acid", "Piperine", "Pipecolinic acid", "Agmatine", 
#   "3-(3-Hydroxyphenyl)propanoic acid", "ω-Muricholic acid", "Indole-3-carboxyaldehyde", 
#   "N1-Methyl-2-pyridone-5-carboxamide", "Tetradecanedioic acid", 
#   "Indole-3-propionic acid", "2,5-’ or 3,4-Dihydroxybenzoic acid", 
#   "2-’ or 3-Hydroxybutyric acid", "γ-Aminobutyric acid", "Fumaric acid", 
#   "Tryptophan", "4-Guanidinobutyric acid", "3-Methylxanthine", 
#   "Xanthosine", "N-Acetylglutamic acid", "4-Hydroxybenzaldehyde", 
#   "N-Methylaspartic acid", "Xanthine", "Pimelic acid", "4-Hydroxy-2,5-dimethyl-3(2H)-furanone", 
#   "Serotonin", "Theobromine", "Hippuric acid", "Dopamine", "Trigonelline", 
#   "Kynurenine", "Norepinephrine", "Homovanilic acid", "Caffeine", 
#   "Ferulic acid", "1,7-Dimethylxanthine", "Theophylline", "Epinephrine", 
#   "Quinic acid")
# #2
# c("Cholic acid", "Pipecolinic acid", "3-(3-Hydroxyphenyl)propanoic acid", 
#   "ω-Muricholic acid", "N1-Methyl-2-pyridone-5-carboxamide", "β-Muricholic acid", 
#   "Indole-3-propionic acid", "2,5-’ or 3,4-Dihydroxybenzoic acid", 
#   "Indole-3-carboxyaldehyde", "2-’ or 3-Hydroxybutyric acid", 
#   "γ-Aminobutyric acid", "Fumaric acid", "Tryptophan", "4-Hydroxy-2,5-dimethyl-3(2H)-furanone", 
#   "Acetylcholine", "Hippuric acid", "Pimelic acid", "Trigonelline", 
#   "Dopamine", "Theobromine", "Homovanilic acid", "Kynurenine", 
#   "Norepinephrine", "Caffeine", "Quinic acid", "1,7-Dimethylxanthine", 
#   "Theophylline", "Ferulic acid", "Epinephrine")
# #3
# c("1,7-Dimethylxanthine", "2-Methylbutyrylglycine", "Hexamethylene bisacetamide", 
#   "Pentose", "Piperine", "Theophylline")

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
   left_join(., metab_trans[,1:2], by = c("metabolite_name" = "Compound_ID")) %>% 
   filter(Name %in% c("Cholic acid", "Piperine", "Pipecolinic acid", "Agmatine", 
                      "3-(3-Hydroxyphenyl)propanoic acid", "ω-Muricholic acid", "Indole-3-carboxyaldehyde", 
                      "N1-Methyl-2-pyridone-5-carboxamide", "Tetradecanedioic acid", 
                      "Indole-3-propionic acid", "2,5-’ or 3,4-Dihydroxybenzoic acid", 
                      "2-’ or 3-Hydroxybutyric acid", "γ-Aminobutyric acid", "Fumaric acid", 
                      "Tryptophan", "4-Guanidinobutyric acid", "3-Methylxanthine", 
                      "Xanthosine", "N-Acetylglutamic acid", "4-Hydroxybenzaldehyde", 
                      "N-Methylaspartic acid", "Xanthine", "Pimelic acid", "4-Hydroxy-2,5-dimethyl-3(2H)-furanone", 
                      "Serotonin", "Theobromine", "Hippuric acid", "Dopamine", "Trigonelline", 
                      "Kynurenine", "Norepinephrine", "Homovanilic acid", "Caffeine", 
                      "Ferulic acid", "1,7-Dimethylxanthine", "Theophylline", "Epinephrine", 
                      "Quinic acid", "β-Muricholic acid", "Acetylcholine", "2-Methylbutyrylglycine", 
                      "Hexamethylene bisacetamide", "Pentose")) 

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
  filter(abs(delta) > 0.4) %>% 
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
  filter(any(plot_label == "moderate")) %>% 
  filter(any(max_per_group > 0.5) ) %>% 
  
  filter(abs_delta_NCD > 0.5) %>% 
  ungroup() %>% 
  filter(name %in% metab_order) %>% 
  mutate(name  = factor(name, levels = metab_order)) -> df_long_MX


CAF_DECAF_order <-  c(df_long_MX %>% 
                        filter(Visit == "Post-\nreintroduction") %>% 
                        filter(Coffee_Type == "CAF") %>% .$ID %>% sort %>% unique,
                      df_long_MX %>% 
                        filter(Visit == "Post-\nreintroduction") %>% 
                        filter(Coffee_Type == "DECAF") %>% .$ID %>% sort %>% unique)
 

plot_mt_NCD <- df_long_MX %>% 
  filter(Coffee_Type =="NCD") %>% 
  mutate(lab = "NCD") %>% 
  filter(!is.na(value)) %>% 
  
  group_by(ID) %>% 
  
  mutate(avg_val = mean(value)) %>% 
  ungroup() %>% 
  
  arrange(avg_val) %>% 
  mutate(ID = factor(ID, levels = unique(ID))) %>% 
  
  group_by(Coffee_Type, visit, name) %>% 
  mutate(avg_by_group = round(mean(value, na.rm = T), digits = 2)) %>%
  ungroup() %>% 
  
  ggplot() +
  aes(y = ID, x = visit, fill = value, label = avg_by_group) + 
  geom_tile() +

  geom_label(data = . %>% group_by(visit, Coffee_Type, name, mdlab, type, plot_label) %>% 
               summarise(n = length(unique(ID)), 
                         avg_by_group = mean(avg_by_group)) %>% ungroup(), 
             # x = 1,
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
  scale_y_discrete(position = "right")+
  ggh4x::facet_nested(name~mdlab, scales = "free", switch = "y", 
                      strip = ggh4x::strip_nested(
                        #text_y = list(element_text(), element_blank()),
                        background_y = list(element_rect()), 
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
        axis.text.x = element_blank(), axis.ticks.x = element_blank()
  )



plot_mt_CD <- df_long_MX %>% 
  filter(Coffee_Type != "NCD") %>% 
  
  group_by(ID) %>% 
  
  mutate(avg_val = mean(value)) %>% 
  ungroup() %>% 
  
  arrange(avg_val) %>% 
  mutate(ID = factor(ID, levels = unique(ID))) %>% 
  
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
  
  
  
  group_by(Coffee_Type, visit, caffeine, name) %>% 
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
  scale_y_discrete(position = "right"
               #   , labels = c(rep("",7), "DECAF",rep("",15),  "CAF", rep("",7))
                   ) +
  scale_x_discrete(expand = expansion(mult = c(0))) +
  ggh4x::facet_nested(name * caffeine ~ mdlab, scales = "free", 
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
        axis.text.x = element_blank(), axis.ticks.x = element_blank(), 
        panel.spacing.x = unit(0, "lines")
  )


  


