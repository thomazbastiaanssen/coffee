urmet <- read.delim("raw/urine_reclustered/urine_metabs_classified.csv", sep = ",", header = FALSE)


urmet <- urmet %>% 
  separate(V1, into = c("visit", "ID"), sep = "_", remove = FALSE) %>% 
  mutate(visit = toupper(visit), 
         ID = case_when(visit == "SAMPLE" ~ "ID", .default = ID), 
         ID = case_when(ID != "" & ID != "ID" ~ paste0("X", ID), .default = ID), 
         visit = case_when(visit == "SAMPLE" ~ "visit", .default = visit))


urmet_info <- urmet %>% 
  t() %>% 
  as.data.frame() %>% 
  dplyr::select(c(V1, V2, V3)) %>% 
  data.frame() %>% 
  dplyr::filter(!V3 %in% c("Sample","visit","ID","Visit","Group")) %>% 
  mutate(V1 = na_if(V1, ""), 
         V2 = na_if(V2, "")) %>%
  fill(V1) %>% 
  mutate(V2 = case_when(V2 == "-" ~ V1, .default = V2)) %>% 
  fill(V2) 

urmet <- urmet %>% 
  filter(V1!="") %>% 
  janitor::row_to_names(1) %>%
  dplyr::select(!c(Visit, Sample)) %>% 
  rename(Coffee_Type = Group) %>% 
  pivot_longer(!c(ID, visit, Coffee_Type)) %>% 
  # 
  mutate(
#ID = paste0("APC0115-", str_remove(ID, "X")), 
          ID = str_remove(ID, "F"), 
          ID = str_remove(ID, "S")) 

 urmet <- urmet %>% 
    left_join(., urmet_info, by = c("name" = "V3")) %>% 
    mutate(value = as.numeric(value)) %>% 
    group_by(ID, V2, visit) %>% 
    mutate(value = sum(value)) %>% 
    ungroup() %>% 
    
    dplyr::select(ID, visit, Coffee_Type, name = V2, value) %>%
    
    distinct() %>% 
    
    pivot_wider(names_from = name, values_from = value)
  


urmet[,-c(1:3)] <- urmet[,-c(1:3)] %>% t() %>% Tjazi::clr_c() %>% t()

urmet_df_long <-  urmet %>% 
  left_join(., metadata %>% 
              dplyr::select(participant_ID, visit, coffee_group,Treatment), 
            by = c("ID" = "participant_ID", "visit" = "visit")) %>% 
  
  mutate(Coffee_Type = case_when(
    coffee_group == "CD" & visit == "V2" ~ "Coffee", 
    visit        == "V3"   ~ "No Coffee",
    visit        == "V4"   ~ Treatment, 
    .default = coffee_group)) %>% 
    dplyr::select(!c(coffee_group, Treatment)) %>% 
  pivot_longer(!c(ID, visit, Coffee_Type)) %>% 
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
  ungroup() %>% 
  filter(!is.na(Coffee_Type)) %>% 
  mutate(Coffee_Type = str_replace(Coffee_Type, "CAFF", "CAF"))



urmet_df_long %>% 
  
  mutate(caf_status = paste(Coffee_Type, visit)) %>% 
  group_by(name, caf_status) %>% 
  summarise(avg = mean(value, na.rm = T)) %>% 
  pivot_wider(names_from = caf_status, values_from = avg) %>% 
  
  mutate(no_caf = mean(`DECAF V4`, `NCD V2` , `No Coffee V3`, na.rm = T), 
         caf    = mean(`CAF V4`, `Coffee V2`, na.rm = T)) %>% 
  mutate(delta = no_caf - caf) %>% 
  dplyr::select(name, delta) %>% 
  ungroup() %>% 
  arrange(desc(abs(delta))) %>% .$name -> urmet_order

urmet_df_long %>% 
  
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-\nwashout", "Post-\nreintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) %>% 
  mutate(ID = str_replace(ID, "X", "APC115-")) %>% 
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
  filter(name %in% urmet_order) %>% 
  mutate(name  = factor(name, levels = urmet_order)) -> urmet_df_long


CAF_DECAF_order <-  c(urmet_df_long %>% 
                        filter(Visit == "Post-\nreintroduction") %>% 
                        filter(Coffee_Type == "CAF") %>% .$ID %>% sort %>% unique,
                      urmet_df_long %>% 
                        filter(Visit == "Post-\nreintroduction") %>% 
                        filter(Coffee_Type == "DECAF") %>% .$ID %>% sort %>% unique)


plot_urmet_reclass_NCD <- urmet_df_long %>% 
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
                        background_y = list( element_rect()), 
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
        strip.clip = "off"
        
  )



plot_urmet_reclass_CD <-  urmet_df_long %>% 
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
    panel.spacing.x = unit(0, "lines"), 
    strip.clip = "off"
  )


#(plot_urmet_NCD + plot_urmet_CD) + plot_layout(guides = 'collect')
