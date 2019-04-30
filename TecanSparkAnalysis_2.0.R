### IMPORT LIBRARIES ### 

library(openxlsx)
library(tidyr)
library(ggplot2)
library(dplyr)
library(reshape2)

store_wd <- getwd()

setwd('\\\\imsbnas.d.ethz.ch\\sauer1\\users\\Mauro\\Cell_culture_data')


# Parsing excel processed Tecan data --------------------------------------

Read_TecanData <- function(FileDirectory,
                           FileName = "Confluence_for_Tecan.xlsx",
                           start_position = 4,
                           Number_data_levels = 4,
                           read_platemap = T){
  
  #Reember that the Confluence_for_Tecan has a specific layour
  # start position = position of well A1 in Tecan file
  # read plate map <- tick this option to import a plate map
  # Number data levels = how many layers of information available in plate map (only valid if read_platemap is set to T)
  
  setwd(FileDirectory)
  
  data <- openxlsx::read.xlsx(FileName, colNames = T)
  
  data.filtered <- data  
  
  nletters <- seq(1,6) #iterate over letters B-G in plate
  
  start_position = start_position + 12 #skip letter A, not used
  
  store_indexes <- list()
  
  for (x in nletters){
    store_indexes[x] = list(seq(start_position+1,start_position+10))
    start_position = start_position + 12
  }
  
  data.filtered <- data.filtered[unlist(store_indexes),]
  
  colnames(data.filtered)[1] <- "Well"
  
  Tecan_data <- gather(data.filtered, time, conf, -Well) #FIXME why do we gather on time, I shall gather based on the columns displayed in the header,
  
  #FIXME find a way to gather this information and name the variables properly, else I will always have this problem 
  
  #FIXME check if it works with other datasets
  
  Tecan_data <- Tecan_data[!is.na(Tecan_data$conf),]
  
  if (read_platemap == T){
    
    Map_tecan <- Read_PlateMap(FileDirectory, Number_data_levels = Number_data_levels)
    
    Tecan_data <- data.frame(Tecan_data, stringsAsFactors = F)
    
    colnames(Tecan_data)[1] <- "Well"
    
    Map_tecan <- data.frame(Map_tecan, stringsAsFactors = F)
    
    colnames(Map_tecan)[1] <- "Well"
    
    dataset <- dplyr::inner_join(Tecan_data, Map_tecan, by = "Well")
    
    dataset$time <- as.numeric(dataset$time)
    
    print(paste("Tecan data from experiment:", FileDirectory))
    
    
    
  }else{
    
    dataset = Tecan_data
    
    print(paste("Tecan data from experiment:", FileDirectory))
    
  }
  
  return(data.frame(dataset))
}  

# Parsing plate map data --------------------------------------------------


Read_PlateMap <- function(FileDirectory,
                          FileName = "PlateMaps_tecan.xlsx",
                          Number_data_levels,
                          start_position = 0){
  
  Plate_annotations <- matrix(nrow = 96, ncol = Number_data_levels+1) #store matrix vectors of each level of information in Plate Maps
  
  setwd(FileDirectory)
  
  data <- openxlsx::read.xlsx(FileName, colNames = T)
  
  
  nletters <- seq(1,8) #iterate over letters B-G in plate
  
  for (n in seq(Number_data_levels)){
    
    data.filtered <- data  
    
    store_indexes <- list()
  
    store_indexes = list(seq(start_position+1,start_position+8))
    
    data.filtered <- data.filtered[unlist(store_indexes),]
    
    data.filtered <- gather(data.filtered, row, cellline, -cell )
    
    data.filtered <- data.filtered %>% mutate(well_number = paste0(cell, row)) %>%
      select(well_number, cellline)
    
    Plate_annotations[,1] <- data.filtered$well_number[1:96]
    
    Plate_annotations[,n+1] <- data.filtered$cellline[1:96]
    
    start_position <- start_position + 9
    
    
    }
  
  Plate_annotations <- Plate_annotations[!is.na(Plate_annotations[,2]),]
  return(Plate_annotations)
    
  }


# Read Tecan M200 96wp ----------------------------------------------------


Read_Tecan_M200 <- function(FileDirectory,
                            FileName_Tecan,
                            read_platemap = F,
                            start_header = F,
                            Plate_mapM200 = "PlateMaps_standardBCA.xlsx"){
  
  # read plate map = if set to T, Will read and parse the file listed in PlatemapM200
  # start_header, which row is the header of the 96wp, aka <>, if select as F it will search automatically and assign.
  
  #FileDirectory = paste0(getwd(),"//")
  #FileName_Tecan = file_name
  #read_platemap = F
  #start_header = F
  
  store_wd <- getwd()
  
  setwd(FileDirectory)
  
  data <- openxlsx::read.xlsx(FileName_Tecan, colNames = F)
  
  if(start_header == F){
    start_header <- grep("<>", data$X1) 
  }
  
  data <- data[seq(start_header,start_header+8),]
  
  colnames(data) <- data[1,]
  
  data <- data[-1,]
  
  data <- gather(data, key = 'Well', value = "my_vals", -`<>`)
  
  data <- data %>%
    dplyr::mutate(Well = paste0(`<>`, Well))%>%
    dplyr::select(-`<>`)
  
  data <- data[data$Well %in% unlist(lapply(LETTERS[1:8], function(x) paste(x, 1:12, sep=""))),]
  
  if(read_platemap == T){
  
    std_curve <- openxlsx::read.xlsx(Plate_mapM200, colNames = T)
  
    std_curve <- gather(std_curve, key = "Well", value = "abs", -std)
  
    std_curve <- std_curve %>%
      dplyr::mutate(Well = paste0(std, Well))%>%
      dplyr::select(-1)%>%
      na.omit()%>%
      inner_join(data, by = "Well")%>%
      mutate(my_vals = as.numeric(my_vals))
  
      Tecan_data <- list()
  
      Tecan_data$std_curve <- std_curve  
  
      Tecan_data$data <- data
  
      return(Tecan_data)
  
  }else{
    
    Tecan_data <- list()
    
    Tecan_data$data <- data
    
    return(Tecan_data)
    
    
  }
  
  setwd(store_wd)
  
  rm(store_wd)
  
}


# function to plot equation in graph -------------------------------------

lm_eqn <- function(df, x, y, log = F){
  
  #function to plot equation in graph
  
  df = data.frame(x = df[,x], y = df[,y], cl = df[,"X2"])
  
  if(log == F){
    m <- lm(y ~ x, data = data.frame(df));
  }else{
    m <- lm(log(y) ~ x, data = data.frame(df));
  }
  
  show(summary(m))
  
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, #full equation
                    list(a = format(coef(m)[1], digits = 3),
                         b = format(coef(m)[2], digits = 3),
                         r2 = format(summary(m)$r.squared, digits = 3)))

  # eq <- substitute(italic(Beta) == b, #beta equation only
  #                  list(b = format(exp(coef(m)[2]), digits = 5)))
  
  as.character(as.expression(eq));                 
  
  
}



setwd(store_wd)

rm(store_wd)