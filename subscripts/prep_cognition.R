v2_2 <- read.delim("raw/cognition/V2.csv",       sep = ",")
v2_3 <- read.delim("raw/cognition/V2 vs V3.csv", sep = ",")
v2_4 <- read.delim("raw/cognition/V2 vs V4.csv", sep = ",")

inter <- read.delim("raw/cognition/intervention.csv", sep = ",")
wash  <- read.delim("raw/cognition/washout.csv", sep = ",")


colnames(v2_2) <- colnames(v2_2) %>% str_remove(., "v2_|V2_")

cog_df = v2_3 %>% 
  pivot_longer(!c(ID, Visit, Coffee_Type)) %>%

  rbind(v2_4 %>% pivot_longer(!c(ID, Visit,Coffee_Type))) %>% 
  rbind(v2_2 %>% pivot_longer(!c(ID, Visit,Coffee_Type))) %>% 
  rbind(inter %>% pivot_longer(!c(ID, Visit,Coffee_Type))) %>% 
  rbind(wash %>% pivot_longer(!c(ID, Visit,Coffee_Type))) %>% 
  
  mutate(Visit = case_when(Visit == 2   ~ "V2",
                           Visit == 3   ~ "V3",
                           Visit == 4   ~ "V4", 
                           .default = as.character(Visit))) %>%
  
  filter(!duplicated(paste(ID, Coffee_Type, Visit, name))) %>% 
  
                                                        #ModRey*
  filter(name %in% c("UPPSTot", "ERSTot","PASAT_Tot",#"MdR_TotalA_F1", 
                     
                     "STAI_TRAITTot", "PSS","BDITot", "GIS_VAS_Total", 
                     "PSQI_GlobalScore", "IPAQ_MET", "VASF_Fatigue", "VASF_Energy", 
                                                                                #HSCL
                     "CWSQ_Tot", "QCC_StrDes1", "QCC_AnticM2", "QCC_AnticF3", "SCL_PSTI")) %>% 
  
  mutate(name = case_when(name == "UPPSTot" ~         "UPPS",
                         name == "ERSTot" ~          "ERS",
                         name == "PASAT_Tot" ~       "PASAT",

                         name == "STAI_TRAITTot" ~   "STAI-T",
                         name == "PSS" ~             "PSS",
                         name == "BDITot" ~          "BDI",
                         name == "GIS_VAS_Total" ~   "GIS",
                         name == "PSQI_GlobalScore"~ "PSQI",
                         name == "IPAQ_MET"~         "IPAQ", 
                         #name == "VASF_Fatigue", 
                         #name == "VASF_Energy", 
                         name == "CWSQ_Tot" ~        "CWQS",
                        # name == "QCC_StrDes1"~      "QCC",
                        # name == "QCC_AnticM2" ~     "QCC_A"
                        # name == "QCC_AnticF3"~      "QCC_A",
                       #  name == "SCL_PSTI" ~        "SCL"
                         
                         .default = as.character(name)))
  
#clean up
rm(list = c("inter", "wash", "v2_2", "v2_3", "v2_4"))  


table(cog_df[,c("Visit", "name")])
