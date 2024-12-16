# Load required libraries
library(QUALYPSO)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define the paths to the CSV folders
folders <- list.dirs("D:/IA Montpellier/JRL/Group project/R codes/14DEC/Outputs_chat", recursive = FALSE)

# Process each folder separately
for (folder in folders) {
  # List all CSV files in the folder
  liste_fichiers <- list.files(path = folder, pattern = "*.csv", full.names = TRUE)
  
  # Initiate an empty list to store the combined data
  data_combinee <- list()
  
  # Loop on all scenarios to read them and add a "Scénario" column
  for (i in seq_along(liste_fichiers)) {
    fichier <- liste_fichiers[i]
    donnees <- read.csv(fichier, stringsAsFactors = FALSE)
    
    # Add column "Scénario"
    donnees$Scénario <- paste0(substr(basename(fichier), 1, nchar(basename(fichier)) - 17), "_", donnees$ETo)
    
    # Add the data to the list
    data_combinee[[i]] <- donnees
  }
  
  # Combine the results in one dataframe for the current folder
  assign(paste0("resultat_combine_", basename(folder)), do.call(rbind, data_combinee))
}

# Combine all resultat_combine_ dataframes into one
all_results <- do.call(rbind, mget(ls(pattern = "^resultat_combine_")))

# Filter and rename specific crops
all_results <- all_results %>% 
  filter(crop %in% c("SB2023-vignes", "FAO-W-WHEAT", "SB2023-vergers")) %>% 
  mutate(crop = recode(crop,
                       "SB2023-vignes" = "Vineyard",
                       "FAO-W-WHEAT" = "Wheat",
                       "SB2023-vergers" = "Orchard"))

# Create variation curves by scenario showing irrigation_mm by selected crops over the years 1971-2099
dir.create("Variation_Curves_Scenarios", showWarnings = FALSE)

scenarios <- unique(all_results$Scénario)

for (scenario_name in scenarios) {
  scenario_data <- all_results %>% 
    filter(Scénario == scenario_name, year >= 1971, year <= 2099) %>% 
    group_by(year, crop) %>% 
    summarize(irrigation_mm = sum(irrigation_mm), .groups = 'drop')
  
  p <- ggplot(scenario_data, aes(x = year, y = irrigation_mm, color = crop)) +
    geom_line(linewidth = 1) +
    labs(title = paste("Irrigation Variation for", scenario_name),
         x = "Year", y = "Irrigation (mm)") +
    theme_minimal()
  
  ggsave(filename = paste0("Crops_variations/", scenario_name, ".png"), plot = p, width = 10, height = 6)
}
