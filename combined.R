library(tidyr)
library(dplyr)

path <- "D:/14DEC/Outputs_chat/Output_chat_8426/"
almost_name_files <- list.files(path,pattern="\\.csv$",full.names=TRUE)
name_files <- basename(almost_name_files)

dir.create("Output_review_8426", showWarnings = FALSE)

for (file in name_files){
  #For step by step debugging:

cell<-8426


name<-paste0(path,file)
# Charger les données
df <- read.csv(file = name)

# Renommer les colonnes en enlevant "evspsblpot_"
colnames(df) <- gsub("evspsblpot_", "", colnames(df))

# en deux parties : une pour FAO et l'autre pour Hg0175
df_fao <- df %>% select(contains("FAO"))
df_hg0175 <- df %>% select(contains("Hg0175"))

#renommer
colnames(df_fao) <- gsub("FAO.", "", colnames(df_fao))
colnames(df_hg0175) <- gsub("Hg0175.", "", colnames(df_hg0175))

# Ajouter une colonne "Cell" aux deux dataframes
df_fao$Cell <- df$Cell
df_hg0175$Cell <- df$Cell

chat_combined <- rbind(df_fao , df_hg0175)
name_folder<-paste0("Output_review_8426/",file)
write.csv(chat_combined, name_folder, row.names = FALSE)
}

