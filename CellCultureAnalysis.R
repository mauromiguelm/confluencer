### Functions to handle and manipulate cell culture data ###

### IMPORT LIBRARIES ### 

library(openxlsx)
library(tidyr)
library(ggplot2)
library(dplyr)
library(reshape2)

store_wd <- getwd()

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
  
  Tecan_data <- tidyr::gather(data.filtered, time, conf, -Well) #FIXME why do we gather on time, I shall gather based on the columns displayed in the header,
  
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
  
  store_wd <- getwd()
  
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
    
    data.filtered <- tidyr::gather(data.filtered, row, cellline, -cell )
    
    data.filtered <- data.filtered %>% dplyr::mutate(well_number = paste0(cell, row)) %>%
      dplyr::select(well_number, cellline)
    
    Plate_annotations[,1] <- data.filtered$well_number[1: Plate_size]
    
    Plate_annotations[,n+1] <- data.filtered$cellline[1:Plate_size]
    
    start_position <- start_position + jump
    
    }
  
  Plate_annotations <- Plate_annotations[!is.na(Plate_annotations[,2]),]
  
  setwd(store_wd)
  
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
  
  data <- tidyr::gather(data, key = 'Well', value = "my_vals", -`<>`)
  
  data <- data %>%
    dplyr::mutate(Well = paste0(`<>`, Well))%>%
    dplyr::select(-`<>`)
  
  data <- data[data$Well %in% unlist(lapply(LETTERS[1:8], function(x) paste(x, 1:12, sep=""))),]
  
  if(read_platemap == T){
  
    std_curve <- openxlsx::read.xlsx(Plate_mapM200, colNames = T)
  
    std_curve <- tidyr::gather(std_curve, key = "Well", value = "abs", -std)
  
    std_curve <- std_curve %>%
      dplyr::mutate(Well = paste0(std, Well))%>%
      dplyr::select(-1)%>%
      na.omit()%>%
      dplyr::inner_join(data, by = "Well")%>%
      dplyr::mutate(my_vals = as.numeric(my_vals))
  
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
                             correct_init_seeding = T,
                             Plate_size = Plate_size,
                             time_output = c("GMT", "elapsed")[2]){
  
  #FileDirectory = getwd()
  #FileName_IncuCyte = "20190925/results/SF539_CL1_P2.txt", 
  # Incucyte data should have row-by-row output format, without experiment details in header
  #read_platemap = F
  #FileName = "PlateMaps_tecan_HCT15_96.xlsx"
  #Number_data_levels = 4
  #Plate_size = c(96, 384)[2]
  #
  
  # function to read IncuCyte data 
  
  data <- read.table(paste(FileDirectory,FileName_IncuCyte, sep = "/"),
                    stringsAsFactors = F,
                    header = T,
                    sep = "\t",
                    skip = 1)
  
  if(correct_init_seeding == T){
    
    data[3:ncol(data)] <- apply(data[3:ncol(data)], 2, function(x){
      
      min_val <- x[1]
      
      x = x - min_val
      
      return(x)
      
      })
   
    
    
  }
  
  if(time_output == "GMT"){
    
    time <- data$Date.Time
    
    if(all(grepl(data$Date.Time, pattern = "\\."))){
      
      time <- strptime(time, format = c("%d.%m.%Y %H:%M:%S"))
      
    }else{
      
      time <- strptime(time, format = c("%m/%d/%Y %I:%M:%S %p"))
      
    }
    
    time <- as.POSIXct(time, tz="GMT") 
    
  }else if(time_output == "elapsed"){
    
    time <- data$Elapsed
    
  }else{
    stop("Time output not defined, check time_output")
  }
  
  data <- data.frame(t(data[,3:ncol(data)]), stringsAsFactors = F)
  
  colnames(data) <- time
  
  data$Well <- rownames(data)
  
  data <- tidyr::gather(data, key = Time, value = Conf, -Well)
  
  if(read_platemap == T){
    
    Map_tecan <- Read_PlateMap(FileName = FileName, FileDirectory = FileDirectory, Number_data_levels = Number_data_levels, Plate_size = Plate_size)
      
    data <- data.frame(data, stringsAsFactors = F)
      
    colnames(data)[1] <- "Well"
    
    Map_tecan <- data.frame(Map_tecan, stringsAsFactors = F)
    
    colnames(Map_tecan)[1] <- "Well"
    
    dataset <- dplyr::inner_join(data, Map_tecan, by = "Well")
    
    #dataset$Time <- as.numeric(dataset$Time)
    
    #dataset$Time <- time
    
    print(paste("IncuCyte Data from experiment:", FileDirectory))
    
    }else{
      
      dataset = data
      
      #dataset$Time <- time
      
      print(paste("IncuCyte Data from experiment:", FileDirectory))
      
    }
  
  
  return(data.frame(dataset))
  
  }



# Function to Plot in 384 or 96 wp ----------------------------------------

# This function plots all wells of a 384 plate grouped together

plot_merge_384 <- 
  
  function(data, log_e = T, plot_title = NULL){
    
    # data should have three columns, Well, Time and Conf,
    # and be in the wide/long format
    
    #log_e sets to T will log transform confluence
    
    if(log_e == T){
      data[,"Conf"] <-  log(data[,"Conf"])
    }
    
    
    if (dim(data)[1] == 0){
      plot.new()
      
      warning("the plot is an empty plot, since nrow == 0")
      
    }else{
      
      store_args <- list()
      
      store_args$max_time <- max(data$Time, na.rm = T)
      
      store_args$min_time <- min(data$Time, na.rm = T)
      
      store_args$max_conf <- max(data$Conf, na.rm = T)
      
      store_args$min_conf <- min(data$Conf, na.rm = T)
      
      plot(store_args$min_time:store_args$max_time, 
           ylim = c(store_args$min_conf, store_args$max_conf),
           type = "n",
           xlab = "Time[h]",
           ylab = "% Confluence",
           main = plot_title)
      
      time.values <- split(data[,"Time"], data[,"Well"])
      
      conf.values <- split(data[,"Conf"], data[,"Well"])
      
      
      
      mapply(lines, y = conf.values, x = time.values, col = c("blue", "green", "red"),
             type="o")
    }
    
    
    
  }


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


filter_growth_outliers <- function(plate_name = NULL,
                                   data,
                                   time_control = 24,
                                   slope_cutoff = NULL,
                                   p.val_cutoff = NULL,
                                   intercept_sd_cutoff = NULL,
                                   save_diag_plots = F,
                                   save_plots_directory = getwd(),
                                   return_stats = F){
  
  
  # data format is a long dataframe, which has the columns "Well", "Conf" and "Time"
  # the filtering will remove the mean +/- mean * sd (sd = incercept_sd_cutoff)
  # time_cotrol is the last time point before addition of drugs.
  # return of this function is a list of wells that pass the quality criteria, together with other statistics
  
  #slope_cutoff = 0.05, original vlaues
  #p.val_cutoff = 0.05, original vlaue
  #intercept_sd_cutoff = 2.5, original vlaues
  
  
  data$Time <- as.numeric(as.character(data$Time))
  
  data$Conf <- as.numeric(as.character(data$Conf))
  
  data_384 = subset(data, Time <= time_control)
  
  data_384.original = data
  
  store_args <- list()
  
  store_args$max_time <- max(data_384$Time[data_384$Time <= time_control])
  
  store_args$min_time <- min(data_384$Time)
  
  store_args$max_conf <- max(data_384$Conf)
  
  store_args$min_conf <- min(data_384$Conf)
  
  store_args$wells_origin <- unique(data_384$Well)
  
  store_args$wells_origin_number <- length(unique(data_384$Well))
  
  store_args$wells_exception <- vector()
  
  store_args$wells_exception_number <- vector()
  
  
 
# low pass filter -------------------------------------------------------
  
  # uses regression to remove obvious outliers 
  
  store_args$regression_metrics <- lapply(store_args$wells_origin, function(well) {
    
    fit <- lm(Conf~Time ,data_384[data_384$Well == well,])
    
    fit.summary <- summary(fit)
    
    output <- data.frame(
      
      Well = well,
      adj.r.squared = fit.summary[[9]],
      p.value = fit.summary$coefficients[[8]],
      slope = fit.summary$coefficients[[2]],
      intercept = fit.summary$coefficients[[1]]
    )
    
    return(output)
    
  })
  

  store_args$regression_metrics <- do.call(rbind, store_args$regression_metrics)
  
  stats_filter <- data.frame("slope_cutoff" = integer(), "p.val_cutoff" = integer(),"intercept_cutoff" = integer()) 
  
  if(!is.null(slope_cutoff)){
    store_args$wells_exception <- append(store_args$wells_exception,
                                         as.character(store_args$regression_metrics[store_args$regression_metrics$slope
                                                                                    < slope_cutoff,"Well"]))
    stats_filter[plate_name,'slope_cutoff'] <- sum(store_args$regression_metrics$slope <= slope_cutoff)
    
    }
  if(!is.null(p.val_cutoff)){
    store_args$wells_exception <- append(store_args$wells_exception,
                                         as.character(store_args$regression_metrics[store_args$regression_metrics$p.value
                                                                                    >= p.val_cutoff,"Well"]))
    stats_filter[plate_name,'p.val_cutoff'] <- sum(store_args$regression_metrics$p.value>= p.val_cutoff)
    }
  if(!is.null( intercept_sd_cutoff)){
    intercept_cutoff <-  mean(store_args$regression_metrics$intercept) + intercept_sd_cutoff * sd(store_args$regression_metrics$intercept)
    
    store_args$wells_exception <- append(store_args$wells_exception,
                                         as.character(store_args$regression_metrics[store_args$regression_metrics$intercept
                                                                                    >= intercept_cutoff,"Well"]))
    stats_filter[plate_name,'intercept_cutoff'] <- sum(store_args$regression_metrics$intercept>= intercept_cutoff)
    }
  
  store_args$wells_exception_number <- length(store_args$wells_exception)
  
  #save diagnostic plots
  
  if(save_diag_plots == T){
    
    plate_name_string = paste0(plate_name,"_", gsub(" ", "_",as.character(Sys.time())) ,".png")
    
    plate_name_string = gsub(pattern = ":", replacement = "_", x = plate_name_string)
    
    png(filename = paste(save_plots_directory, plate_name_string , sep = "/"),
        width = 1000,
        height = 1500)
    
    par(mfcol = c(3,2), cex = 1.5)
    plot_merge_384(data = data_384, log_e = F, plot_title = "before_ttm_all")
    plot_merge_384(data_384[!(data_384$Well %in% store_args$wells_exception), ], log_e = F, plot_title = "before_ttm_pass")
    plot_merge_384(data_384[(data_384$Well %in% store_args$wells_exception), ], log_e = F, plot_title = "before_ttm_exceptions")
    
    
    plot_merge_384(data = data_384.original, log_e = F, plot_title = "fullTime_all")
    plot_merge_384(data_384.original[!(data_384.original$Well %in% store_args$wells_exception), ], log_e = F, plot_title = "fullTime_pass")
    plot_merge_384(data = data_384.original[(data_384.original$Well %in% store_args$wells_exception), ], log_e = F, plot_title = "fullTime_exceptions")
    
    dev.off()
    
  }
  
  if(return_stats==TRUE){
    return(list(store,args,stats_arg))
  }else{
    return(store_args)
  }
}

setwd(store_wd)

rm(store_wd)




