#run on server as very slow

library(iNEXT)

setwd("/data2/rawdata/thomaz/coffee/reads/merged/woltka_o/R_alpha/")

counts <- read.delim(file = "species.tsv", row.names = 1)

row.names(counts) = counts$Name
counts <- counts[,-276]

print('running Chao1')
chao1   <- ChaoRichness(counts)

write.csv(chao1, "chao1.csv")

print('running Simpson Index')
simps   <- ChaoSimpson(counts)

write.csv(simps, "simpson.csv")

print('running Shannon Entropy')
shannon <- ChaoShannon(counts)

write.csv(shannon, "shannon.csv")

print("Finished!")


