install.packages("remotes")
remotes::install_git("https://forgemia.inra.fr/umr-g-eau/cropwat.git")



# save or save rds, multicorps
#data table package


library(CropWat)
library(dplyr)
# library(future)
# library(future.apply)
# log_layout(layout_glue_colors)
# log_threshold(INFO)

# Configurer le backend parallèle
#plan(multicore)  # Utilise plusieurs cœurs pour le calcul
#CREATE CROP DICTIONARY---------------------------------------------------------
cropcode <- c(
  "Ble" = "FAO-W-WHEAT",
  "Autres cereales" = "FAO-SORGHUM",#sorgho
  "Colza" = "SB2023-colza",
  "Culture industrielle"= "SB2023-colza", #colza for fuel
  "Fourrages"= "FAO-ALFALFA1",#ALFALFA 1 year=fodder plant
  "Fruits a coque"= "SB2023-CP",#amandiers--> cultures perennes
  "Lavande"= "SB2023-CP",#culture pérenne
  "Legumes (Serre)"= "FAO-VEGETABL",#lower than the reality bc not greenhouse
  "Legumes"= "FAO-VEGETABL",
  "Legumineuses"= "FAO-PULSES",
  "Mais"= "SB2023-maisT",
  "Oliviers"= "SB2023-CP", #culture pérenne
  "Orge"= "FAO-BARLEY",
  "Plantes a fibres"= "SB2023-colza",
  "Plantes aromatiques"= "SB2023-CP", #culture pérenne
  "Prairie permanente"= "SB2023-prairie",
  "Prairie"= "SB2023-prairie",
  "Prairie temporaire"= "SB2023-prairie",
  "Proteagineux"= "FAO-PULSES",
  "SPH"= "SB2023-prairie",
  "Soja"= "SB2023-soja",
  "Tournesol"= "SB2023-tour",
  "Vergers"=  "SB2023-vergers",
  "Vignes"= "SB2023-vignes"
)

#CREATE A SOWING DATE TABLE-----------------------------------------------------
sowing_date=data.frame(Crop=c("FAO-W-WHEAT", "FAO-SORGHUM",
                              "FAO-VEGETABL", "FAO-ALFALFA1",
                              "FAO-PULSES", "FAO-BARLEY"),Date=c('10-25','05-15','02-15','03-01','03-01','02-20'))
#PROCESS THE CROPS DATA---------------------------------------------------------
p<-read.csv("TABLE2_QGIS.csv", header = TRUE, sep = ",", quote = "\"", dec = ".",)
p$pro <- p$pro/100 #soil depth cm->m
p$Cell <- p$cell
p$Depth <- p$pro
p$AWC <- p$rupro
p$Crop <-cropcode[p$CULTURE]
TABLE2 <- p[, c("Cell","Depth", "AWC", "Crop","Surface_ha")]
TABLE2<-TABLE2 %>% group_by(Cell,Depth,AWC, Crop)  %>%
  summarise(Surface_ha=sum(Surface_ha,na.rm=TRUE),.groups='drop') #group the duplicated values
#TABLE2<-TABLE2[TABLE2$Cell==8427,]

#CODE---------------------------------------------------------------------------

#irrigation time from 15/03 to 16/10
irrig_date<-c()
for (year in 1951:2100) {
  start_date <- as.Date(paste(year, "03", "15", sep = "-"))
  end_date <- as.Date(paste(year, "10", "16", sep = "-"))
  date_sequence =c(seq.Date(from = start_date, to = end_date, by = "day"))
  irrig_date <- c(as.Date(irrig_date), date_sequence)
}

#PROCESS DRIAS DATA

path <- "12DEC_fast_test"
almost_name_files<-list.files(path,pattern="\\.csv$",full.names=TRUE)
name_files <- basename(almost_name_files)

# Setting parallel processes
future::plan(future::multisession)
# future::plan(future::sequential) # For debugging

# Set up outputs
dir.create("Output_fast_test", showWarnings = FALSE)
logger::log_appender(logger::appender_file("CropWat_Drais_test150years.log"))

#BEGINNING OF THE OUTER LOOP
for (file in name_files){
  #For step by step debugging:
  file <- name_files[1]
  logger::log_info("Processing scenario: ", file)
  drias_data<-read.csv(paste0(path,"/",file), header = TRUE, sep = ",", quote = "\"", dec = ".",)
  drias_data<-drias_data[,-1]
  drias_data$Date<-as.Date(drias_data$Date)
  Date<-drias_data$Date

  water_demand<- data.frame(matrix(ncol = length(1951:2100), nrow = 14)) #create the data frame to save by crop and year for each scenario
  colnames(water_demand)<- as.character(1951:2100)
  rownames(water_demand) <- c("FAO-W-WHEAT","FAO-SORGHUM","SB2023-colza","FAO-ALFALFA1","SB2023-CP",
                              "FAO-VEGETABL","FAO-PULSES","SB2023-maisT","FAO-BARLEY",
                              "SB2023-prairie","SB2023-soja","SB2023-tour","SB2023-vergers",
                              "SB2023-vignes")
  water_demand[is.na(water_demand)] <- 0



  #beggining of the inner loop

  l <- future.apply::future_lapply(seq(nrow(TABLE2)), function(n) {
    # For step by step debugging:      n <- 1
    Name_cell<-paste0("SAFRAN_CELL_",TABLE2[n,'Cell'])
    Name_crop<-as.character(TABLE2[n, 'Crop'])
    climate <- drias_data
    #climate<-drias_data[lubridate::year(drias_data$Date)==1951,]
    logger::log_info("Cell: ", Name_cell)
    logger::log_info("Crop: ", Name_crop)
    logger::log_info("start CW_create_input")
    if(grepl("FAO", Name_crop)){
      cw_input <- CW_create_input(Name_crop,
                                  DatesR = climate$Date,
                                  ETo = climate$ETP,
                                  P = climate$Ptot,
                                  soil_depth = as.numeric(TABLE2[n,'Depth']),
                                  AWC = as.numeric(TABLE2[n,'AWC']),
                                  sowing_date = sowing_date[sowing_date$Crop==Name_crop,'Date'])

    } else{
      cw_input <- CW_create_input(Name_crop,
                                  DatesR = climate$Date,
                                  ETo = climate$ETP,
                                  P = climate$Ptot,
                                  soil_depth = as.numeric(TABLE2[n,'Depth']),
                                  AWC = as.numeric(TABLE2[n,'AWC'])
      )
    }#END OF THE ELSE
    # Set up the initial state of the model
    logger::log_info("start CW_create_state")
    X0_initial_state <- CW_create_state(cw_input = cw_input)
    logger::log_info("start CW_irrig_fun_factory")
    # Define irrigation management strategy
    irrig_1pc_RAW <- CW_irrig_fun_factory(RAW_ratio = 1, apply_Dr = TRUE, dates_irrig = irrig_date)#the plant is never stressed

    # Simulate the water balance with the chosen irrigation management
    logger::log_info("start CW_run_simulation")
    cw_output <- CW_run_simulation(X0_initial_state, cw_input, FUN_IRRIG =irrig_1pc_RAW)

    logger::log_info("start yearly water_demand computation")
    irrig_output <- cw_output %>%
      mutate(year = format(DatesR, "%Y")#,month = format(DatesR, "%m")
             ) %>%
      group_by(year) %>%
      summarise(
        irrigation_mm = sum(Ir)
        # ,spring_irrigation = sum(Ir[month %in% c("03", "04", "05")]),
        # summer_irrigation = sum(Ir[month %in% c("06", "07", "08", "09")])
      )
    irrig_output$crop <- Name_crop
    irrig_output$cell <- TABLE2$Cell[n]
    irrig_output$irrigation_m3 <- irrig_output$irrigation_mm * TABLE2$Surface_ha[n] * 10
    logger::log_info("End of the loop")
    return(irrig_output)
  }) #END OF THE LOOP

  water_demand <- do.call(rbind, l)

  file_out <- file.path("Output", file)
  logger::log_info("write ", file_out)
  write.csv(water_demand, file_out, row.names = FALSE)
}

