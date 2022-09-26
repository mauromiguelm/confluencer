#' Parsing excel processed Tecan data
#'
#' @param FileName file path
#' @param start_position position of well A1 in Tecan file
#' @param Number_data_levels Number of metadata layer in platemap
#' @param read_platemap True refers to option to import a plate map with metadata
#'
#' @return
#' @export
#' @examples
Read_TecanData <- function(FileName,
                           start_position = 4,
                           Number_data_levels = 4,
                           read_platemap = T){

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

  Tecan_data <- tidyr::gather(data.filtered, `time`, `conf`, -`Well`)

  Tecan_data <- Tecan_data[!is.na(Tecan_data$conf),]

  if (read_platemap == T){

    Map_tecan <- Read_PlateMap(FileName = FileName, Number_data_levels = Number_data_levels)

    Tecan_data <- data.frame(Tecan_data, stringsAsFactors = F)

    colnames(Tecan_data)[1] <- "Well"

    Map_tecan <- data.frame(Map_tecan, stringsAsFactors = F)

    colnames(Map_tecan)[1] <- "Well"

    dataset <- tidyr::inner_join(Tecan_data, Map_tecan, by = `Well`)

    dataset$time <- as.numeric(dataset$time)

    print(paste("Tecan data from experiment:", FileName))

  }else{

    dataset = Tecan_data

    print(paste("Tecan data from experiment:", FileName))

  }

  return(data.frame(dataset))
}

#' Read platemap metadata
#'
#' @param FileName filepath
#' @param Number_data_levels
#' @param start_position
#' @param Plate_size
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"
#' @examples
Read_PlateMap <- function(FileName,
                          Number_data_levels,
                          start_position = 0,
                          Plate_size = c(96, 384)[1]){

  Plate_annotations <- matrix(nrow = Plate_size, ncol = Number_data_levels+1) #store matrix vectors of each level of information in Plate Maps

  data <- openxlsx::read.xlsx(FileName, colNames = T)

  if(Plate_size == 96){

    nletters <- seq(1,8) #iterate over letters B-G in plate

    jump <- 9

  }else{

    nletters <- seq(1,16) #iterate over letters B-G in plate

    jump <- 17

  }

  for (n in seq(Number_data_levels)){

    data.filtered <- data

    store_indexes <- list()

    store_indexes = list(seq(start_position+1,start_position + jump - 1))

    data.filtered <- data.filtered[unlist(store_indexes),]

    data.filtered <- tidyr::gather(data.filtered, `row`, `cellline`, -`cell`)

    data.filtered <- data.filtered %>% dplyr::mutate(well_number = paste0(`cell`, `row`)) %>%
      dplyr::select(`well_number`, `cellline`)

    Plate_annotations[,1] <- data.filtered$well_number[1: Plate_size]

    Plate_annotations[,n+1] <- data.filtered$cellline[1:Plate_size]

    start_position <- start_position + jump

    }

  Plate_annotations <- Plate_annotations[!is.na(Plate_annotations[,2]),]

  setwd(store_wd)

  return(Plate_annotations)

  }


#' Read Tecan M200 96wp
#'
#' @param FileName_Tecan filepath
#' @param read_platemap
#' @param start_headerwhich row is the header of the 96wp, aka <>, if select as F it will search automatically and assign.
#' @param Plate_mapM200
#'
#' @return
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
Read_Tecan_M200 <- function(FileName_Tecan,
                            read_platemap = F,
                            start_header = F,
                            Plate_mapM200){

  data <- openxlsx::read.xlsx(FileName_Tecan, colNames = F)

  if(start_header == F){
    start_header <- base::grep("<>", data$X1)
  }

  data <- data[seq(start_header,start_header+8),]

  colnames(data) <- data[1,]

  data <- data[-1,]

  data <- tidyr::gather(data, key = `Well`, value = `my_vals`, -`<>`)

  data <- data %>%
    tidyr::mutate(Well = paste0(`<>`, `Well`))%>%
    tidyr::select(-`<>`)

  data <- data[data$Well %in% unlist(lapply(LETTERS[1:8], function(x) paste(x, 1:12, sep=""))),]

  if(read_platemap == T){

    std_curve <- openxlsx::read.xlsx(Plate_mapM200, colNames = T)

    std_curve <- tidyr::gather(std_curve, key = `Well`, value = `abs`, -`std`)

    std_curve <- std_curve %>%
      dplyr::mutate(Well = paste0(`std`, `Well`))%>%
      dplyr::select(-1)%>%
      stats::na.omit()%>%
      dplyr::inner_join(data, by = `Well`)%>%
      dplyr::mutate(my_vals = as.numeric(`my_vals`))

      Tecan_data <- list()

      Tecan_data$std_curve <- std_curve

      Tecan_data$data <- data

      return(Tecan_data)

  }else{

    Tecan_data <- list()

    Tecan_data$data <- data

    return(Tecan_data)


  }

}

#' Read and parse IncuCyte Data file
#'
#' @param FileName_IncuCyte path to incucyte data
#' @param read_platemap True if should read metadata plate
#' @param start_header where the data starts
#' @param FileName Metadata filename
#' @param Number_data_levels number of data levels in metadata plate
#' @param start_position
#' @param correct_init_seeding T if should subtract initial seeding for each well
#' @param skip_first_time True to skip first time point recorded
#' @param Plate_size 384 or 96
#' @param time_output should function return GMT currected time points or elapsed
#'
#' @return
#' @export
#' @examples
ReadIncuCyteData <- function(FileName_IncuCyte,
                             read_platemap = read_platemap,
                             start_header = F,
                             FileName = FileName,
                             Number_data_levels = Number_data_levels,
                             start_position = 0,
                             correct_init_seeding = T,
                             skip_first_time = F,
                             Plate_size,
                             time_output = c("GMT", "elapsed")[2]){

  data <- utils::read.table(paste(FileName_IncuCyte, sep = "/"),
                    stringsAsFactors = F,
                    header = T,
                    sep = "\t",
                    skip = 1)

  if(skip_first_time==T){
    data = data[-which(data$Elapsed == min(data$Elapsed)),]

  }

  if(correct_init_seeding == T){

    data[3:ncol(data)] <- apply(data[3:ncol(data)], 2, function(x){

      min_val <- x[1]

      x = x - min_val

      return(x)

      })

  }

  if(time_output == "GMT"){

    time <- data$Date.Time

    if(all(base::grepl(data$Date.Time, pattern = "\\."))){

      time <- base::strptime(time, format = c("%d.%m.%Y %H:%M:%S"))

    }else{

      time <- base::strptime(time, format = c("%m/%d/%Y %I:%M:%S %p"))

    }

    time <- base::as.POSIXct(time, tz="GMT")

  }else if(time_output == "elapsed"){

    time <- data$Elapsed

  }else{
    stop("Time output not defined, check time_output")
  }

  data <- data.frame(t(data[,3:ncol(data)]), stringsAsFactors = F)

  colnames(data) <- time

  data$Well <- rownames(data)

  data <- tidyr::gather(data, key = `Time`, value = `Conf`, -`Well`)

  if(read_platemap == T){

    Map_tecan <- Read_PlateMap(FileName = FileName, Number_data_levels = Number_data_levels, Plate_size = Plate_size)

    data <- data.frame(data, stringsAsFactors = F)

    colnames(data)[1] <- "Well"

    Map_tecan <- data.frame(Map_tecan, stringsAsFactors = F)

    colnames(Map_tecan)[1] <- "Well"

    dataset <- dplyr::inner_join(data, Map_tecan, by = "Well")

    print(paste("IncuCyte Data from experiment:", FileName_IncuCyte))

    }else{

      dataset = data

      print(paste("IncuCyte Data from experiment:", FileName_IncuCyte))

    }

  return(data.frame(dataset))

  }





#' Function to filter outliers in growth curves
#' @description returns a list including statistics and wells that passed the treshold.
#' @param plate_name
#' @param data data format is a long dataframe, which has the columns "Well", "Conf" and "Time"
#' @param time_control what time drug was added
#' @param slope_cutoff slope cutoff
#' @param p.val_cutoff pval cutoff
#' @param intercept_sd_cutoff the filtering will remove the mean +/- mean * sd (sd = incercept_sd_cutoff)
#' @param save_diag_plots T to save diagnostic plots
#' @param save_plots_directory dir to save diagnostic plots
#' @param return_stats T if should return statistics from wells
#'
#' @return
#' @export
#'
#' @examples
filter_growth_outliers <- function(plate_name = NULL,
                                   data,
                                   time_control = 24,
                                   slope_cutoff = NULL,
                                   p.val_cutoff = NULL,
                                   intercept_sd_cutoff = NULL,
                                   save_diag_plots = F,
                                   save_plots_directory = getwd(),
                                   return_stats = F){

  data$Time <- as.numeric(as.character(data$Time))

  data$Conf <- as.numeric(as.character(data$Conf))

  data_384 = subset(data, `Time` <= time_control)

  data_384.original = data

  data_384$Conf <- data_384$Conf+0.0001 #avoid zero values

  store_args <- list()

  store_args$max_time <- max(data_384$Time[data_384$Time <= time_control])

  store_args$min_time <- min(data_384$Time)

  store_args$max_conf <- max(data_384$Conf)

  store_args$min_conf <- min(data_384$Conf)

  store_args$wells_origin <- unique(data_384$Well)

  store_args$wells_origin_number <- length(unique(data_384$Well))

  store_args$wells_exception <- vector()

  store_args$wells_exception_number <- vector()

  # uses regression to remove obvious outliers

  store_args$regression_metrics <- lapply(store_args$wells_origin, function(well) {

    fit <- stats::lm(`Conf`~`Time` ,data_384[data_384$Well == well,])

    fit.summary <- base::summary(fit)

    output <- data.frame(

      Well = well,
      adj.r.squared = fit.summary[[9]],
      p.value = fit.summary$coefficients[[8]],
      slope = fit.summary$coefficients[[2]],
      intercept = fit.summary$coefficients[[1]]
    )

    return(output)

  })


  store_args$regression_metrics <- base::do.call(base::rbind, store_args$regression_metrics)

  summary_filter <- data.frame("slope_cutoff" = integer(), "p.val_cutoff" = integer(),"intercept_cutoff" = integer(), "total_exclusions" = integer())

  if(!is.null(slope_cutoff)){
    store_args$wells_exception <- append(store_args$wells_exception,
                                         as.character(store_args$regression_metrics[store_args$regression_metrics$slope
                                                                                    < slope_cutoff,"Well"]))
    summary_filter[plate_name,'slope_cutoff'] <- sum(store_args$regression_metrics$slope <= slope_cutoff)

    }
  if(!is.null(p.val_cutoff)){
    store_args$wells_exception <- append(store_args$wells_exception,
                                         as.character(store_args$regression_metrics[store_args$regression_metrics$p.value
                                                                                    >= p.val_cutoff,"Well"]))
    summary_filter[plate_name,'p.val_cutoff'] <- sum(store_args$regression_metrics$p.value>= p.val_cutoff)
    }
  if(!is.null( intercept_sd_cutoff)){
    intercept_cutoff <-  mean(store_args$regression_metrics$intercept) + intercept_sd_cutoff * stats::sd(store_args$regression_metrics$intercept)

    store_args$wells_exception <- append(store_args$wells_exception,
                                         as.character(store_args$regression_metrics[store_args$regression_metrics$intercept
                                                                                    >= intercept_cutoff,"Well"]))
    summary_filter[plate_name,'intercept_cutoff'] <- sum(store_args$regression_metrics$intercept>= intercept_cutoff)
    }

  store_args$wells_exception_number <- length(store_args$wells_exception)

  summary_filter[plate_name,'total_exclusions'] = store_args$wells_exception_number

  #save diagnostic plots

  if(save_diag_plots == T){

    plate_name_string = paste0(plate_name,"_", gsub(" ", "_",as.character(Sys.time())) ,".png")

    plate_name_string = gsub(pattern = ":", replacement = "_", x = plate_name_string)

    grDevices::png(filename = paste(save_plots_directory, plate_name_string , sep = "/"),
        width = 1000,
        height = 1500)

    graphics::par(mfcol = c(3,2), cex = 1.5)

    plot_merge_384(data = data_384, log_e = F, plot_title = "All data")
    plot_merge_384(data_384[!(data_384$Well %in% store_args$wells_exception), ], log_e = F, plot_title = "Clean data")
    plot_merge_384(data_384[(data_384$Well %in% store_args$wells_exception), ], log_e = F, plot_title = "Exclusions")

    plot_merge_384(data = data_384.original, log_e = F, plot_title = "All data")
    plot_merge_384(data_384.original[!(data_384.original$Well %in% store_args$wells_exception), ], log_e = F, plot_title = "Clean data")
    plot_merge_384(data = data_384.original[(data_384.original$Well %in% store_args$wells_exception), ], log_e = F, plot_title = "Exclusions")

    grDevices::dev.off()

  }

  if(return_stats==TRUE){
    return(list(store_args,summary_filter))
  }else{
    return(store_args)
  }
}


