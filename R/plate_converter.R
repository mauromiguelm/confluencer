#' covert 96wp to 384 wp
#' @description This function takes a data.frame with two columns, the first being the well of a 96 wp, the second being some values of interest, and rearranges them into a 384 plate.
#' @param p1_96 data.frame with two columns, the first being the well of a 96 wp, the second value to be distributed in 384 plate
#' @param p2_96 data.frame with two columns, the first being the well of a 96 wp, the second value to be distributed in 384 plate
#' @param p3_96 data.frame with two columns, the first being the well of a 96 wp, the second value to be distributed in 384 plate
#' @param p4_96 data.frame with two columns, the first being the well of a 96 wp, the second value to be distributed in 384 plate
#' @param gap384 add gap between 384 wells
#' @param plotValues
#' @param isvars_factor
#'
#' @return
#' @export
#'
#' @examples
conv_96_384 <- function(p1_96 = NULL,
                        p2_96 = NULL,
                        p3_96 = NULL,
                        p4_96 = NULL,
                        gap384 = c(T,F)[2],
                        plotValues = c(T,F)[1],
                        isvars_factor = c(T,F)[1]
                        ){

  # 384 well plate template

  store_df = data.frame(Well = paste0(rep(LETTERS[1:16], each = 24), 1:24),
                         Row = rep(LETTERS[1:16], each = 24),
                         Col = rep(1:24, 16),
                         idx = 1:384,
                         idx_p1 = NA,
                         idx_p2 = NA,
                         idx_p3 = NA,
                         idx_p4 = NA,
                         return_values = NA)

  wells_96p1 <- paste0(LETTERS[1:16][c(T,F)], rep(seq(1,24,2), each = 8))
  wells_96p2 <- paste0(LETTERS[1:16][c(T,F)], rep(seq(2,24,2), each = 8))
  wells_96p3 <- paste0(LETTERS[1:16][c(F,T)], rep(seq(1,24,2), each = 8))
  wells_96p4 <- paste0(LETTERS[1:16][c(F,T)], rep(seq(2,24,2), each = 8))

  store_df$idx_p1[ store_df$Well %in% wells_96p1 ] = paste0(LETTERS[1:8],rep(1:12, each = 8))
  store_df$idx_p2[ store_df$Well %in% wells_96p2 ] = paste0(LETTERS[1:8],rep(1:12, each = 8))
  store_df$idx_p3[ store_df$Well %in% wells_96p3 ] = paste0(LETTERS[1:8],rep(1:12, each = 8))
  store_df$idx_p4[ store_df$Well %in% wells_96p4 ] = paste0(LETTERS[1:8],rep(1:12, each = 8))

  # check the plates that were provided and store them in a list

  plates_96 <- c("p1_96", "p2_96", "p3_96", "p4_96")

  plates_96[
    sapply(c("p1_96", "p2_96", "p3_96", "p4_96"), function(x) !is.null(get(x)))
    ] -> plates_96

  plates_96 <- lapply(plates_96, function(x) get(x))

  # return an error if one of the plate args is not a df

  lapply(plates_96, function(x) if(!is.data.frame(x)){
    stop("One of the plates provided as arg is not a df")
  })

  # order Wells to that they match my 96 template

  plates_96 <-

  lapply(seq_along(plates_96), function(x,y){

    order <- paste0(rep(LETTERS[1:8], each = 12), 1:12)

    order <- order[order %in% y[[x]][,"Well"]]

    order <- order(match(y[[x]][,"Well"], order))

    y[[x]][order,]

  }, y = plates_96)

  # combining 96 into 384, checking which wells from 96wp match 384

  if(gap384 == T){ #this will create a one-well gap between the pipetting, as with conventional pipettes

    lapply(seq_along(plates_96), function(x, y){

      match_wells = store_df[,paste0("idx_p", x)] %in% (y[[x]][,"Well"])

    }, y = plates_96) -> match_wells


    for(x in seq_along(plates_96)){

      store_df[match_wells[[x]],"return_values"] = as.character(plates_96[[x]][,2])

    }}else{ #this will create no gap between 96 and 384, and one quadrant of 384 will match 96, as in Echo robot.

    quadrants_384 =
      list(
        q1_384 = paste0(LETTERS[1:8], rep(1:12, each = 8)),

        q2_384 = paste0(LETTERS[1:8], rep(13:24, each = 8)),

        q3_384 = paste0(LETTERS[9:16], rep(1:12, each = 8)),

        q4_384 = paste0(LETTERS[9:16], rep(13:24, each = 8)))

    match_wells <-
    lapply(seq_along(plates_96), function(x,y){

      match_wells <-
      store_df[,"Well"] %in% quadrants_384[[x]]

    }, y = plates_96)


    for(x in seq_along(plates_96)){

      store_df[match_wells[[x]],"return_values"] = as.character(plates_96[[x]][,2])

    }




    }

  return(store_df[,c(1,2,3,4,9)])

  }

#' convert 384 to 96wp
#'
#' @param data_384 long data frame where first colum is the 384 well plate and the others are information
#'
#' @return return the same format but separated as 96wp in lists.
#' @export
#'
#' @examples
conv_384_96 <- function(data_384){

  dict <-
  list(
    data.frame(q_384 = paste0(rep(LETTERS[1:16][c(T,F)], each = 12), seq(1,24,2)),
                    wells_96 =  paste0(rep(LETTERS[1:8], each = 12), 1:12),
               stringsAsFactors = F),

  data.frame(q_384 = paste0(rep(LETTERS[1:16][c(T,F)], each = 12), seq(2,24,2)),
                    wells_96 =  paste0(rep(LETTERS[1:8], each = 12), 1:12),
             stringsAsFactors = F),

  data.frame(q_384 = paste0(rep(LETTERS[1:16][c(F,T)], each = 12), seq(1,24,2)),
                    wells_96 =  paste0(rep(LETTERS[1:8], each = 12), 1:12),
             stringsAsFactors = F),

  data.frame(q_384 = paste0(rep(LETTERS[1:16][c(F,T)], each = 12), seq(2,24,2)),
                    wells_96 =  paste0(rep(LETTERS[1:8], each = 12), 1:12),
             stringsAsFactors = F))

  wells_384 <- as.character(do.call(rbind,dict)[[1]])

  if(!all(as.character(data_384[[1]]) %in% wells_384)){
    stop("wells 384 do not match 96")}

  tmp <-
  lapply(1:length(dict), function(quart){

    #quart = 2
    #data_384 <- data_corrected[[10]][sample(1:20000,200),]

    dict_subset <- dict[[quart]]

    match_idx <- base::match(data_384[,1], as.character(dict_subset[,1]))

    match_wells <-  dict_subset[base::na.omit(match_idx),]

    match_wells[,"quart"] = quart

    data_subset <- data_384[!base::is.na(match_idx),]

    return(cbind(data_subset, match_wells[,c(2,3)]))

    })

  tmp <- do.call(rbind,tmp)

  return(tmp)
}

#' 96wp row switch
#'
#' @param data data to be corrected
#' @param cell cell type
#' @param plate plate number
#' @param row_flip1 row 1 to be switched with row 2
#' @param row_flip2 row 2 to be switched with row 1
#' @param cols
#'
#' @return
#' @export
#'
#' @examples
switch_row <- function(data, cell, plate,row_flip1, row_flip2, cols){

  data = data

  grouping <- paste(data$cell, data$source_plate,data$well384)

  wells_flip1 <- paste0(c(row_flip1), rep(cols, each = 1))

  wells_flip1 <- paste(cell, plate,wells_flip1)

  idx_flip1 <- sapply(wells_flip1, function(x){which(grouping == x)})

  wells_flip2 <- paste0(c(row_flip2), rep(cols, each = 1))

  wells_flip2 <- paste(cell, plate,wells_flip2)

  idx_flip2 <- sapply(wells_flip2, function(x){which(grouping == x)})

  save_data_flip1 <- lapply(idx_flip1, function(x) data[x,2:7])

  save_data_flip2 <- lapply(idx_flip2, function(x) data[x,2:7])

  for(idx in 1:length(idx_flip2)){
    #idx = 2
    data[idx_flip2[[idx]],2:7] <- save_data_flip1[[idx]]

    data[idx_flip1[[idx]],2:7] <- save_data_flip2[[idx]]
  }
  return(data)
}


#' Flip 96 well plate upside down
#'
#' @param data data to be corrected
#' @param cell cell type
#' @param plate plate number
#' @param wells wells within plate
#'
#' @return
#' @export
#'
#' @examples
invert_plate <- function(data, cell,plate, wells){

  data = data

  grouping <- paste(data$cell, data$source_plate,data$well384)

  wells <- paste(cell, plate, wells)

  well_matrix <- matrix(wells, ncol = 12,byrow = T)

  well_matrix_rev <- well_matrix

  rotate <- function(x) t(apply(x, 2, rev))

  well_matrix_rev <- t(rotate(well_matrix_rev))

  well_matrix_rev <- well_matrix_rev[,rev(1:ncol(well_matrix_rev))]

  for(idx in 1:(dim(well_matrix)[1]*dim(well_matrix)[2])){

    idx_flip1 <- which(grouping == well_matrix[idx])
    idx_flip2 <- which(grouping == well_matrix_rev[idx])

    save_data_flip1 = data[idx_flip1,2:7]
    save_data_flip2 = data[idx_flip2,2:7]

    if(nrow(save_data_flip2)==0){
      data[idx_flip1,2:7] <- NA
    }else{
      data[idx_flip1,2:7] <- save_data_flip2
    }

    if(nrow(save_data_flip1)==0){
      data[idx_flip2,2:7] <- NA
    }else{
      data[idx_flip2,2:7] <- save_data_flip1
    }
  }

  return(data)
}

#' get 96 wells within 384 quadrants
#'
#' @description   get quadrant of 384 wells to identify which well is in which quadrant. Quadrants are spaced by gaps.
#'
#' @param quadrant which quadrant we want wells from
#'
#' @return
#' @export
#'
#' @examples
get_quadrant_wells= function(quadrant){
  if(quadrant==1){
    return(data.frame(q_384 = paste0(rep(LETTERS[1:16][c(T,F)], each = 12), seq(1,24,2)),
                      wells_96 =  paste0(rep(LETTERS[1:8], each = 12), 1:12),
                      stringsAsFactors = F))

  }else if(quadrant==2){
    return(data.frame(q_384 = paste0(rep(LETTERS[1:16][c(T,F)], each = 12), seq(2,24,2)),
                      wells_96 =  paste0(rep(LETTERS[1:8], each = 12), 1:12),
                      stringsAsFactors = F)

    )
  }else if(quadrant==3){
    return(data.frame(q_384 = paste0(rep(LETTERS[1:16][c(F,T)], each = 12), seq(1,24,2)),
                      wells_96 =  paste0(rep(LETTERS[1:8], each = 12), 1:12),
                      stringsAsFactors = F))
  }else if (quadrant==4){

    return(data.frame(q_384 = paste0(rep(LETTERS[1:16][c(F,T)], each = 12), seq(2,24,2)),
                      wells_96 =  paste0(rep(LETTERS[1:8], each = 12), 1:12),
                      stringsAsFactors = F))
  }
}

#' Plot 384 well plates
#'
#' @param plate384 plate 384 in standard 3 columns format
#'
#' @return
#' @export
#' @examples
plot_384 <- function(plate384){f

  grDevices::png("plate.png", h = 400, w = 600)
  ggplot2::ggplot(plate384, ggplot2::aes(Col, Row, fill=factor(return_values))) + ggplot2::geom_raster() + ggplot2::ggtitle("random Ct Data")
  grDevices::dev.off()

  paste("384 layout image saved in:", getwd())

}


# # mock plates in case I need --------------------------------------------

# plate1 <- data.frame( Well = paste0(LETTERS[1:8],rep(1:12, each = 8)),
#                      Val = 1)
#
# plate2 <- data.frame( Well = paste0(LETTERS[1:8],rep(1:12, each = 8)),
#                       Val = 2)
#
# plate3 <- data.frame( Well = paste0(LETTERS[1:8],rep(1:12, each = 8)),
#                       Val = 3)
#
# plate4 <- data.frame( Well = paste0(LETTERS[1:8],rep(1:12, each = 8)),
#                       Val = 4)

