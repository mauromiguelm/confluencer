#' Plot 384 well plates with curves collapsed in a single plot
#'
#' @param plate384 plate 384 in standard 3 columns format
#'
#' @return
#' @export
#' @examples
plot_384 <- function(plate384){
  grDevices::png("plate.png", h = 400, w = 600)
  ggplot2::ggplot(plate384, ggplot2::aes(`Col`, `Row`, fill=factor(`return_values`))) + ggplot2::geom_raster() + ggplot2::ggtitle("random Ct Data")
  grDevices::dev.off()
  paste("384 layout image saved in:", getwd())

}


#' Function to plot 384 or 96 well plates
#' @description This function plots all wells of a 384 plate grouped together in a single plot
#' @param data data should have three columns, Well, Time and Conf in long format
#' @param log_e  log_e set to True will natural log transform confluence
#' @param plot_title title of plot
#'
#' @return
#' @export
#'
#' @examples
plot_merge_384 <-

  function(data, log_e = T, plot_title = NULL){

    if(log_e == T){
      data[,"Conf"] <-  log(data[,"Conf"])
    }

    if(dim(data)[1] == 0){
      graphics::plot.new()

      warning("the plot is an empty plot, since nrow == 0")

    }else{

      store_args <- list()

      store_args$max_time <- max(data$Time, na.rm = T)

      store_args$min_time <- min(data$Time, na.rm = T)

      store_args$max_conf <- max(data$Conf, na.rm = T)

      store_args$min_conf <- min(data$Conf, na.rm = T)

      base::plot(NULL,
                 xlim=c(store_args$min_time, store_args$max_time),
                 ylim = c(store_args$min_conf, store_args$max_conf),
                 type = "n",
                 xlab = "Time[h]",
                 ylab = "% Confluence",
                 main = plot_title)
      time.values <- split(data[,"Time"], data[,"Well"])

      conf.values <- split(data[,"Conf"], data[,"Well"])
      base::mapply(graphics::lines, y = conf.values, x = time.values, col = c("blue", "green", "red"),
                   type="o")
    }



  }

#' Plot as 96 or 384 binned plot
#'
#' @param is_96 True if plate is 96, False i splate is 384
#' @param input_df input data frame, with tipical format from readIncuCyteData
#' @param max_timepoint max time point to be plotted
#'
#' @return
#' @export
#'
#' @examples

plot_multi_well <- function(is_96 = c(T,F)[2],
                            input_df,
                            max_timepoint = NULL){

  if(methods::hasArg(max_timepoint)) {

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

  if(is_96 == F){  # adjust graphical parameters, add new plots by column, in format of a 384wp or 96wp (else)

    row_idx = LETTERS[1:16]

    col_idx = seq(1,24,1)

    well_order <- paste0(rep(row_idx, each = 24), col_idx)

    graphics::par(mfrow = c(16,24),
                  mar = c(0.3,0.4,0.4,0.4)+ .6)

  }else if(is_96 == T){

    row_idx = LETTERS[1:8]

    col_idx = seq(1,12,1)

    well_order <- paste0(rep(row_idx, each = 12), col_idx)

    graphics::par(mfrow = c(8,12))

    graphics::par(mar = c(0.10,0.25,0.25,0.25))

  }else{

    stop(paste(c("is_96 is not a defined cell culture plate, choose 384 or 96.")))
  }


  for (row_name in well_order) {

    if(row_name %in% input_df$Well){ #if yes, plot the values

      tmp_df <- base::subset(input_df, Well == row_name)

      base::plot(tmp_df[,'Time'], tmp_df[,"Conf"],
                 pch = 20,
                 ylim = c(params[['min_y']],params[['max_y']]),
                 xlim = c(params[['min_time']], params[['max_time']]),
                 cex = 0.2) #, xaxt = "n", yaxt = "n"

      graphics::lines(stats::lowess(tmp_df$Time, tmp_df$Conf, f = .2), col = "red")

      graphics::text(x = params[["max_time"]] / 4, y = params[["max_y"]], labels = row_name, pos = 1)

    }else{

      #plot null BLACK plot, as this well was not used

      graphics::plot.new()

    }
  }
}
