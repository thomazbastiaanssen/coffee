library(PubChemR)
library(RefMet)
library(tidyverse)
res <- get_aids(urmets, namespace = "name")

a <- AIDs(res)

setdiff(urmets, unique(a$NAME))


write.csv(data.frame(NAME = urmets %>% 
                       str_remove("Paraxanthine / ") %>%
                       str_replace_all(pattern = "`|′|’", replacement = "'") %>% 
                       str_remove(pattern = "'") %>% 
                       
                       str_remove(" II") %>% 
                       str_remove("-3'-glucuronide") %>% 
                       str_remove("-4'-glucuronide") %>% 
                       str_remove("-glucuronide") %>% 
                       str_remove("-4'-sulfate") %>% 
                       str_remove("-3'-sulfate")
                     ), file = "raw/keys/RefMet_input.txt", row.names = FALSE)
RefMet_mapped <- RefMet::refmet_map("raw/keys/RefMet_input.txt")
RefMet_mapped
?RefMet
