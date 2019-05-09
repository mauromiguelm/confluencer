### Functions to handle and manipulate cell culture data ###

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
                          start_position = 0,
                          Plate_size = c(96, 384)[1]){
  
  Plate_annotations <- matrix(nrow = Plate_size, ncol = Number_data_levels+1) #store matrix vectors of each level of information in Plate Maps
  
  setwd(FileDirectory)
  
  data <- openxlsx::read.xlsx(FileName, colNames = T)
  
  if(Plate_size == 96){
    
    nletters <- seq(1,8) #iterate over letters B-G in plate
    
    jump <- 9
  
  }else{
    
    nletters <- seq(1,16) #iterate over letters B-G in plate
    
    jump <- 17
    
  }
  
  for (n in seq(Number_data_levels)){
    
    #n = 4 #FIXME DELETEME
    
    data.filtered <- data  
    
    store_indexes <- list()
  
    store_indexes = list(seq(start_position+1,start_position + jump - 1))
    
    data.filtered <- data.filtered[unlist(store_indexes),]
    
    data.filtered <- gather(data.filtered, row, cellline, -cell )
    
    data.filtered <- data.filtered %>% mutate(well_number = paste0(cell, row)) %>%
      select(well_number, cellline)
    
    Plate_annotations[,1] <- data.filtered$well_number[1: Plate_size]
    
    Plate_annotations[,n+1] <- data.filtered$cellline[1:Plate_size]
    
    start_position <- start_position + jump
    
    
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

# Read and parse IncuCyte Data --------------------------------------------

ReadIncuCyteData <- function(FileDirectory = FileDirectory,
                             FileName_IncuCyte = "Name_IncuCyteFile",
                             read_platemap = read_platemap,
                             start_header = F,
                             FileName = FileName,
                             Number_data_levels = Number_data_levels,
                             start_position = 0,
                             Plate_size = Plate_size){
  
  #FileDirectory = getwd()
  #FileName_IncuCyte = "HCT15_Conf.txt", 
  # Incucyte data should have row-by-row output format, without experiment details in header
  #read_platemap = T
  #FileName = "PlateMaps_tecan_HCT15_96.xlsx"
  #Number_data_levels = 4
  #Plate_size = c(96, 384)[1]
  
  # function to read IncuCyte data 
  
  data <- read.table(FileName_IncuCyte,
                    stringsAsFactors = F,
                    header = T,
                    sep = "\t",
                    skip = 1)
  
  time <- data$Elapsed
  
  data <- data.frame(t(data[,3:ncol(data)]), stringsAsFactors = F)
  
  colnames(data) <- time
  
  data$WELL <- rownames(data)
  
  data <- tidyr::gather(data, key = Time, value = Conf, -WELL)
  
  if(read_platemap == T){
    
    Map_tecan <- Read_PlateMap(FileName = FileName, FileDirectory = FileDirectory, Number_data_levels = Number_data_levels, Plate_size = Plate_size)
      
    data <- data.frame(data, stringsAsFactors = F)
      
    colnames(data)[1] <- "Well"
    
    Map_tecan <- data.frame(Map_tecan, stringsAsFactors = F)
    
    colnames(Map_tecan)[1] <- "Well"
    
    dataset <- dplyr::inner_join(data, Map_tecan, by = "Well")
    
    dataset$Time <- as.numeric(dataset$Time)
      
    print(paste("IncuCyte Data from experiment:", FileDirectory))
    
    }else{
      
      dataset = data
      
      print(paste("IncuCyte Data from experiment:", FileDirectory))
      
    }
    
    return(data.frame(dataset))
    
    
  }



# Function to Plot in 384 or 96 wp ----------------------------------------

# This function takes a long data frame with time, confluence and other variables and plot as 96 or 384 binned plot

plot_multi_well <-

function(is_96 = c(T,F)[2],
         input_df = NULL,
         max_timepoint = NULL){
  
 
  # is_96 = False if 384 plate, True if 96
  # input_df = input data frame, with tipical format (readIncuCyteData)
  # max_timepoint = max_timepoint that should be used in 
  
  #FIXME save_format = c("PDF", "PNG") 
  #FIXME save_image #True if want to save image, needs to be implemented
  #FIXME # save_path this has to be implemented, path where to save image
  #FIXME # save_format = option to  save plot, this has to be implemented
  
  
  # Comment
  #The order of the columns in input dataframe should be (in long format):
  # Well, Time, Conf 
  
  if(hasArg(max_timepoint)) {
    
    max_time = max_timepoint 
    
  }else{
    
    max_time <- max(input_df[,'Time'])  
    
  }
  
  if(max_time < min(input_df[,'Time']) | max_time > max(input_df[,'Time'])){
    
    stop("max_timepoint not valid")
    
  }
  
  
  #remove NAs from confluence
  
  input_df <- input_df[!is.na(input_df$Conf), ]
  
  # Setting useful args for plots
  
  list(
  
  min_time = min(input_df[,'Time']),
  
  max_time = max_time,
  
  max_y = max(input_df[,'Conf']),
  
  min_y = min(input_df[,'Conf'])
  
  ) -> params
  
  

if(is_96 == F){  # adjust graphical parameters, add new plots by colum, in format of a 384wp or 96wp (else)
  
  row_idx = LETTERS[1:16] 
  
  col_idx = seq(1,24,1)
  
  well_order <- paste0(rep(row_idx, each = 24), col_idx)
  
  par(mfrow = c(16,24))
  
  par(mar = c(0.3,0.4,0.4,0.4)+ .6)
  
  
  }else if(if_96 == T){
    
    row_idx = LETTERS[1:8]
    
    col_idx = seq(1,12,1)
    
    well_order <- paste0(rep(row_idx, each = 12), col_idx)
    
    par(mfrow = c(8,12))
    
    par(mar = c(0.10,0.25,0.25,0.25))
    
    
    
  }else{
    
    stop(paste(c("is_96 is not a defined cell culture plate, choose 384 or 96.")))
}


for (row_name in well_order) {
  
  #row_name = well_order[1]
  
  if(row_name %in% input_df$Well){ #if yes, plot the values
    
    tmp_df <- subset(input_df, Well == row_name)
    
    plot(tmp_df[,'Time'], tmp_df[,"Conf"], 
         pch = 20,
         ylim = c(params[['min_y']],params[['max_y']]),
         xlim = c(params[['min_time']], params[['max_time']]),
         cex = 0.2) #, xaxt = "n", yaxt = "n" 
    
    lines(lowess(tmp_df$Time, tmp_df$Conf, f = .2), col = "red")
    
    text(x = params[["max_time"]] / 4, y = params[["max_y"]], labels = row_name, pos = 1)
    
    
    
  }else{
    
    #plot null BLACK plot, as this well was not used
    
    plot.new()
    
  }
}
  }


# Function to Filter Growth outliers --------------------------------------

#returns a list including statistics and wells that passed the treshold.


filter_growth_outliers <- function(data, time_control = 24){
  
  # data format is a long dataframe
  # the filtering will remove the mean +/- 1 * sd
  # time_cotrol is the last time point before addition of drugs.
  # return of this function is a list of wells that pass the quality criteria, together with other statistics
  
  if(!require(rcompanion)){
    
    stop("rcompanion not installed")
    
  }
  
  data_384 = data
  
  max_time <- max(data_384$Time[data_384$Time <= time_control])
  
  # Bellow are two easier ways to subset data, without having to attach variables to environment, both will create the same results
  
  # attach(data_384)
  # new_data_384 <- data_384[ which(Time=='24'),]
  # detach(data_384)
  
  
  new_data_384 <- data_384[data_384$Time == max_time,] #this uses R indexing of matrices, where [row,col], and by using "data_384$Time == 24", you get the right number of rows
  
  new_data_384 <- subset(data_384, Time == max_time) #this uses the subset function
  
  
  Mean_all_wells_time24 <- mean(new_data_384$Conf, na.rm = TRUE)
  
  Pre_Filter_Mean <- groupwiseMean(Conf ~ 1, 
                                   data   = new_data_384, 
                                   conf   = 0.99, 
                                   digits = 3)
  
  # Variance seen in cells at 384 well plate. Then use this variance and a forward loop to remove wells until this variance is reached
  # Variance might be cell line dependent
  
  sdev <- sd(new_data_384$Conf)
  
  subset_data_384 <- subset(data_384, data_384[ data_384$Time == max_time,]$Conf > (Mean_all_wells_time24-sdev) & data_384[ data_384$Time==max_time,]$Conf < (Mean_all_wells_time24+sdev))
  
  data <- subset_data_384
  
  
  return_data <- list(filtered_wells = unique(data$Well), summary_stats = Pre_Filter_Mean)
  
  # a function in R can only return a single object.
  # if you omit return, the function will return the last evaluated expression
  # it is recommended to include return so that we clearly know what is the output of a function
  # return() can only have one output, hence I created a list with several output within that lsit.
  
  return(return_data)
}

setwd(store_wd)

rm(store_wd)




