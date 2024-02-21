library(tidyverse)
library(ggimage)

source("subscripts/load_and_clean_data.R")

# 1=NCD	0=F
# 2=CD	1=M
long_omics = metadata[metadata$Visit!= "",] %>% 
  mutate(Visit = str_remove(Visit, "Visit "),
         Coffee_Type = coffee_group) %>% 
  
  
  mutate(Coffee_Type = case_when((Visit == "2" & coffee_group == "CD" ) ~ "Coffee", 
                                 (Visit == "2" & coffee_group == "NCD") ~ "NCD", 
                                 (Visit == "3") ~ "No Coffee", 
                                 (Visit == "4" & Treatment == "CAFF") ~ "CAF",
                                 (Visit == "4" & Treatment == "DECAF")~ "DECAF")) %>% 
  mutate(Visit = case_when(Visit == "2" ~ "Baseline", 
                           Visit == "3" ~ "Post-washout", 
                           Visit == "4" ~ "Post-reintroduction")) %>% 
  mutate(ID = str_replace(participant_ID, "X", "APC115-")) %>% 
  
  dplyr::select(ID, Metab_ID, R_ID, Visit, Coffee_Type) %>% 
  
  left_join(., species.exp %>% as.data.frame() %>%  rownames_to_column("ID") %>% pivot_longer(!ID),
            by = c("R_ID" = "name")) %>% 
  rename(species_name = ID.y, species_value = value ) %>% 
  
  left_join(., metabs.exp %>% as.data.frame() %>%  rownames_to_column("ID") %>% pivot_longer(!ID),
            by = c("Metab_ID" = "name")) %>% 
  
  rename(ID = ID.x, metabolite_name = ID, metabolite_value = value) %>% 
  left_join(., metab_trans[,1:2], by = c("metabolite_name" = "Compound_ID")) %>% 
  mutate(metabolite_name = Name) %>% 
  
  dplyr::select(ID, Visit, Coffee_Type, species_name, species_value, metabolite_name, metabolite_value) %>% 
  
  
  filter(species_name %in% c("Eggerthella sp. CAG:209", "Firmicutes bacterium CAG:94")) %>% 
  filter(metabolite_name %in% c("Caffeine")) %>% 
      
  pivot_wider(names_from = species_name, values_from = species_value) %>% 
  pivot_wider(names_from = metabolite_name, values_from = metabolite_value) %>% 
  
  pivot_longer(!c(ID, Visit, Coffee_Type)) %>% 
  
  mutate(meas_group = case_when(name %in% c("Eggerthella sp. CAG:209", "Firmicutes bacterium CAG:94") ~ "Species",
                               name %in% c("Caffeine") ~ "Metabolites")) %>% 
  
  group_by(name) %>% 
  mutate(value = scale(value)) %>% 
  ungroup() %>% 
  
  group_by(Visit, name, Coffee_Type, meas_group) %>%
  
  reframe(value = mean(value, na.rm = T)) %>% 
  
  ungroup() %>% 
  
  dplyr::select(  Visit,name,Coffee_Type,value,meas_group)
  
  
  






s1 = readxl::read_xlsx("raw/cognition/raw_xlsx/V2 - SPSS Template.xlsx")
s2 = readxl::read_xlsx("raw/cognition/raw_xlsx/V2 vs V3 - SPSS Template.xlsx")
s3 = readxl::read_xlsx("raw/cognition/raw_xlsx/V2 vs V4 - SPSS Template.xlsx")
s4 = readxl::read_xlsx("raw/cognition/raw_xlsx/V3 vs V4 - SPSS Template.xlsx")

colnames(s1) <- str_remove(colnames(s1), pattern = "V2_|v2_|V2-|v2-")

s1 <- s1 %>%   mutate(BMI  = as.numeric(BMI),
                      BP_D = as.numeric(BP_D))
s2 <- s2 %>%   mutate(BMI  = as.numeric(BMI),
                      BP_D = as.numeric(BP_D))
s3 <- s3 %>%   mutate(BMI  = as.numeric(BMI),
                      BP_D = as.numeric(BP_D))
s4 <- s4 %>%   mutate(BMI  = as.numeric(BMI),
                      BP_D = as.numeric(BP_D))


# c_cols <- intersect(intersect(colnames(s1), colnames(s2)), 
#                     intersect(colnames(s3), colnames(s4)))
# setdiff(colnames(s1), c_cols)
# setdiff(colnames(s2), c_cols)
# setdiff(colnames(s3), c_cols)
# setdiff(colnames(s4), c_cols)
# all_colname <- unique(c(colnames(s1),colnames(s2),colnames(s3),colnames(s4)))



long_cog <- do.call(rbind, 
        list(s1[,1:3],
             s2[,1:3],
             s3[,c(1,3,2)],
             s4[,c(1,3,2)])) %>% 
  distinct() %>% 
  
  left_join(., s1, by = c("ID" = "ID", "Visit" = "Visit", "Coffee_Type" = "Coffee_Type"), keep = FALSE) %>% 
  left_join(., s2, by = c("ID" = "ID", "Visit" = "Visit", "Coffee_Type" = "Coffee_Type"), keep = FALSE) %>% 
  left_join(., s3, by = c("ID" = "ID", "Visit" = "Visit", "Coffee_Type" = "Coffee_Type"), keep = FALSE) %>% 
  left_join(., s4, by = c("ID" = "ID", "Visit" = "Visit", "Coffee_Type" = "Coffee_Type"), keep = FALSE) %>%

  
  pivot_longer(!c(ID, Coffee_Type, Visit, Gender)) %>% 
  mutate(name = str_remove(name, "\\.x.*|\\.y.*")) %>% 
  mutate(name = str_remove(name, "V2_|v2_|V3_|v3_")) %>% 
  mutate(name = str_replace(name, "IPAQ-", "IPAQ_")) %>% 
  
  distinct() %>% 
  
  mutate(Visit = case_when(Visit == "2" ~ "Baseline", 
                           Visit == "3" ~ "Post-washout", 
                           Visit == "4" ~ "Post-reintroduction")) %>% 
  dplyr::select(!Gender) %>% 
    
  mutate(Visit       = factor(Visit,       levels = c("Baseline","Post-washout", "Post-reintroduction")),
         Coffee_Type = factor(Coffee_Type, levels = c("NCD", "Coffee", "No Coffee", "DECAF", "CAF"))) %>%
  
  group_by(name) %>% 
  mutate(value = scale(value)) %>% 
  ungroup() %>% 
  
  group_by(Visit, name, Coffee_Type) %>%
  
  reframe(value = mean(value, na.rm = T)) %>% 
  
  ungroup() %>% 
  
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
                                str_detect(name, "CWSQ")    ~ "Coffee Withdrawal"
  )) %>% 
  

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
  
  filter(!str_detect(name, "\\) [^TM]")) %>% 
  mutate(name =  str_replace(name, "IPAQ-|IPAQ_", "(IPAQ) ")) %>% 
  mutate(value = case_when(is.na(value)~ 0, .default = value)) 
  
do.call(rbind, list(long_cog, 
                     long_omics)) %>%   

  
  mutate(meas_group= factor(meas_group, levels = c("Impulsivity", "Emotional Reactivity","Attention", 
                                                   "Memory", "Stress\n&\nAnxiety", "Mood", "Mental Health", "Gastrointestinal\nHealth", 
                                                   "Sleep", "Physical Activity", "Coffee Withdrawal", "Species", "Metabolites"))) %>% 
ggplot() +
  aes(x = Coffee_Type, y = name, fill = value)+ 
  geom_tile() +
  #geom_point(shape = 24) +
  
  scale_fill_gradientn(colours = c(
    "#053061",
    "#2166ac", "#2166ac",
    "#4393c3","#4393c3", "#4393c3",
    "#f7f7f7","#f7f7f7", 
    "#d6604d","#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026" 
  ), limits = c(-1.5, 1.5), breaks = c(-1.5, -1, -0.5, 0, 0.5, 1), "z-score") +
  scale_y_discrete(position = "right")+
  facet_grid(meas_group~Visit, scales = "free", space = "free", switch = "y") +
  
  # geom_image(x = 1, y = 3, image = "/home/thomaz/Downloads/Cryan, John 2015.png", size = 3)+
  xlab(NULL) + ylab(NULL) + 
  theme_bw() +
  theme(strip.text.y.left = element_text(angle =0))



