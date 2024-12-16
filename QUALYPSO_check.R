# Load required libraries
library(tidyverse)
library(QUALYPSO)

# Define what are the scenarios to read
scenAvail <- readr::read_csv("scenAvail.csv")
scenAvail_files <- scenAvail %>% select(-ETo) %>% distinct_all
scenAvail_files <- scenAvail_files %>% mutate(file = sprintf("%s_%s_%s_water_demand.csv",RCP, GCM, RCM))

# Define the paths to the CSV folders
folders <- list.dirs("Output_review", recursive = FALSE)

# Read and combine all results into a single data.frame
l <- lapply(folders, function(folder) {
  l1 <- lapply(seq(nrow(scenAvail_files)), function(i) {
    df <- readr::read_csv(file.path(path_cell, scenAvail_files$file[i]))
    df$RCP <- scenAvail_files$RCP[i]
    df$GCM <- scenAvail_files$GCM[i]
    df$RCM <- scenAvail_files$RCM[i]
    return(df)
  })
  return(do.call(rbind, l1))
})
all_res <- do.call(rbind, l)

# Sum results for all cells to get total amount of irrigation for the
# global territory
df_tot <- all_res %>% 
  filter(year %in% 1976:2099) %>%
  group_by(RCP, GCM, RCM, ETo, year) %>% 
  summarize(irrigation_m3 = sum(irrigation_m3), .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = irrigation_m3, values_fill = NA)

Y <- df_tot[, 5:ncol(df_tot)] %>% as.matrix() 



# Load available scenarios
scenAvail <- df_tot %>% select(RCP, GCM, RCM, ETo) %>% as.data.frame()

# Start QUALYPSO
listOption = list(
  typeChangeVariable = 'abs', # relative evolution
  ANOVAmethod = "QUALYPSO"
)
QUALYPSO.synth <- QUALYPSO(
  Y = Y,
  scenAvail = scenAvail,
  X = as.numeric(colnames(Y)),
  listOption = listOption
)

# Plot grand mean
plotQUALYPSOgrandmean(QUALYPSO.synth,xlab="Years")

# Plot the relative effect of each parameter on Irrigation 
plotQUALYPSOeffect(QUALYPSO.synth, includeMean = TRUE, nameEff = "RCP", xlab = "Years", main = "Relative effect of each RCP on Irrigation")
plotQUALYPSOeffect(QUALYPSO.synth, includeMean = TRUE, nameEff = "GCM", xlab = "Years", main = "Relative effect of each GCM on Irrigation")
plotQUALYPSOeffect(QUALYPSO.synth, includeMean = TRUE, nameEff = "RCM", xlab = "Years", main = "Relative effect of each RCM on Irrigation")
plotQUALYPSOeffect(QUALYPSO.synth, includeMean = TRUE, nameEff = "ETo", xlab = "Years", main = "Relative effect of each ETo formula on Irrigation")

# Plot the total decomposition of variance
plotQUALYPSOMeanChangeAndUncertainties(QUALYPSO.synth, xlab = "Years")
plotQUALYPSOTotalVarianceDecomposition(QUALYPSO.synth,xlab="Years")
