library(tidyverse)
library(patchwork)
library(png)
library(ggtext)

s1 = readxl::read_xlsx("raw/cognition/raw_xlsx/V2 - SPSS Template.xlsx")
s2 = readxl::read_xlsx("raw/cognition/raw_xlsx/V2 vs V3 - SPSS Template.xlsx")
s3 = readxl::read_xlsx("raw/cognition/raw_xlsx/V2 vs V4 - SPSS Template.xlsx")
s4 = readxl::read_xlsx("raw/cognition/raw_xlsx/V3 vs V4 - SPSS Template.xlsx")

ncd   = readPNG("raw/icons/NCD.png")
cd    = readPNG("raw/icons/CD.png")
wash  = readPNG("raw/icons/WASHOUT.png")
decaf = readPNG("raw/icons/DECAF.png")
caff  = readPNG("raw/icons/CAFF.png")


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
filter(!is.na(value)) %>% 

mutate(Visit = case_when(Visit == "2" ~ "Baseline", 
                         Visit == "3" ~ "Post-\nwashout", 
                         Visit == "4" ~ "Post-\nreintroduction")) %>% 
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
mutate(name =  str_replace(name, "Tot ", "Tot")) %>% 


filter(!str_detect(name, "\\) [^TM]")) %>% 
mutate(name =  str_replace(name, "IPAQ-|IPAQ_", "(IPAQ) ")) %>% 
#mutate(value = case_when(is.na(value)~ 0, .default = value)) %>% 

mutate(meas_group = factor(meas_group, levels = c("Impulsivity", "Emotional Reactivity",
                                                  "Attention","Memory", "Stress\n&\nAnxiety", 
                                                  "Mood", "Mental Health", "Gastrointestinal\nHealth",
                                                  "Sleep", "Physical Activity", "Coffee Withdrawal", "Species", "Metabolites"))) %>%

group_by(name) %>% 
mutate(value = (value - mean(value, na.rm = T))/sd(value, na.rm = T)) %>% 
ungroup() %>% 
  mutate(name = factor(name, levels = (c("(UPPS) Tot", "(ERS) Tot", "(ModRey) Tot", "(PASAT) Tot",
                                        "(PSS)", "(STAI-T) Tot", "(BDI) Tot", "(GIS-VAS) Tot", 
                                        "(PSQI) Tot", "(IPAQ) MET-minutes")))) %>% 
  
  mutate(mdlab = case_when(Coffee_Type == "NCD" ~ "<img \n src='raw/icons/NCD.png' width='80' />",
                           Coffee_Type == "Coffee" ~ "<img \n src='raw/icons/CD.png' width='80' />",
                           Coffee_Type == "No Coffee" ~ "<img \n src='raw/icons/WASHOUT.png' width='80' />",
                           Coffee_Type == "DECAF" ~ "<img \n src='raw/icons/REINTRO.png' width='80' />", 
                           Coffee_Type == "CAF"   ~ "<img \n src='raw/icons/REINTRO.png' width='80' />"), 
         mdlab = factor(mdlab, levels = c("<img \n src='raw/icons/NCD.png' width='80' />", 
                                                 "<img \n src='raw/icons/CD.png' width='80' />", 
                                                 "<img \n src='raw/icons/WASHOUT.png' width='80' />", 
                                                 "<img \n src='raw/icons/REINTRO.png' width='80' />"))
         ) -> df_long_cog
#df_long_cog$name

CAF_DECAF_order <- c(df_long_cog %>% 
                       filter(Visit == "Post-\nreintroduction") %>% 
                       filter(Coffee_Type == "CAF") %>% .$ID %>% sort %>% unique,
                     df_long_cog %>% 
                       filter(Visit == "Post-\nreintroduction") %>% 
                       filter(Coffee_Type == "DECAF") %>% .$ID %>% sort %>% unique)

  
plot_NCD <- df_long_cog %>% 
  filter(Coffee_Type =="NCD") %>% 
  mutate(lab = "NCD") %>% 
  filter(!is.na(value)) %>% 
  # group_by(Visit, name, Coffee_Type, meas_group) %>%
  # 
  # reframe(value = mean(value, na.rm = T)) %>% 
  # 
  # ungroup() %>% 
  #df_long_cog %>% 
    
  #  filter(Coffee_Type == "NCD") %>% 
  
  ggplot() +
  aes(y = ID, x = lab, fill = value) + 
  geom_tile() +
  #geom_point(shape = 24) +
  
  scale_fill_gradientn(colours = c(
    "#053061",
    "#2166ac","#2166ac",
    "#4393c3","#4393c3",
    "#f7f7f7","#f7f7f7", "#f7f7f7", 
    "#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026",
    
    "#a50026","#a50026",
    "#a50026","#a50026",
    "#a50026"
    
    
  ), 
  limits = c(-2.60, 4.55), "z-score"
  ) +
  scale_y_discrete(position = "right")+
  ggh4x::facet_nested(meas_group*name~mdlab, scales = "free", switch = "y", 
                      strip = ggh4x::strip_nested(
                        #text_y = list(element_text(), element_blank()),
                        background_y = list(element_blank(), element_rect()), 
                        by_layer_y = TRUE
                      )
  ) +
  
  # geom_image(x = 1, y = 3, image = "/home/thomaz/Downloads/Cryan, John 2015.png", size = 3)+
  xlab(NULL) + ylab(NULL) + 
  theme_test() +
  theme(strip.text.y.left = element_text(angle =0), 
        strip.text.x = element_markdown(angle = 0),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
       # axis.text.x = element_text(angle = 330, hjust = 0),
        axis.text.x = element_blank(), axis.ticks.x = element_blank()
  )



plot_CD <- df_long_cog %>% 
  filter(Coffee_Type != "NCD") %>% 
  
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


  # group_by(Visit, name, Coffee_Type, meas_group) %>%
  # 
  # reframe(value = mean(value, na.rm = T)) %>% 
  # 
  # ungroup() %>% 
  #df_long_cog %>% 
  
  #  filter(Coffee_Type == "NCD") %>% 
  
  ggplot() +
  aes(y = ID, x = lab, fill = value)+ 
  geom_tile() +
  #geom_hline(aes(alpha = Visit, yintercept = 16), show.legend = FALSE)+
  #geom_point(shape = 24) +
  
  scale_fill_gradientn(colours = c(
    "#053061",
    "#2166ac","#2166ac",
    "#4393c3","#4393c3",
    "#f7f7f7","#f7f7f7", "#f7f7f7", 
    "#d6604d","#d6604d",
    "#d73027","#d73027",
    "#a50026",
    
    "#a50026","#a50026",
    "#a50026","#a50026",
    "#a50026"
    

  ), 
  limits = c(-2.60, 4.55), "z-score"
  ) +
  scale_alpha_manual(values = c("Baseline" = 0, "Post-\nwashout" = 0, "Post-\nreintroduction" = 1))+
  scale_y_discrete(position = "right"
                   #   , labels = c(rep("",7), "DECAF",rep("",15),  "CAF", rep("",7))
  ) +
  ggh4x::facet_nested(name * caffeine ~ mdlab, scales = "free", 
                      strip = ggh4x::strip_nested(background_y = list(element_blank(), element_rect(), element_rect()), 
                                                  text_y       = list(element_blank(), element_text(), element_text()), 
                                                  by_layer_y = TRUE )) +
  xlab(NULL) + ylab(NULL) + 
  theme_test() +
  theme(#strip.text.y = element_blank(), 
    strip.text.x = element_markdown(angle = 0), #,
    # strip.text.x = element_blank(), strip.background.x = element_blank()
    strip.text.y = element_text(angle =0),
    axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
    #axis.text.x = element_text(angle = 330, hjust = 0)#,   panel.spacing.x =unit(0, "lines")
    axis.text.x = element_blank(), axis.ticks.x = element_blank()
  )
plot_CD


(plot_NCD | 
    #plot_spacer() | #+ theme(plot.background = element_rect(fill = "black")) | 
    plot_CD) + 
  plot_layout(guides = 'collect', widths = c(1,3))

