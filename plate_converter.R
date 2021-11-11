#
#
#
# This script takes a 96 well plate and fit in a 384 format, in different ways
#
#
#

# import libraries

library(ggplot2)


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



# function to covert 96wp to 384 wp ########

conv_96_384 <- function(p1_96 = NULL,
                        p2_96 = NULL,
                        p3_96 = NULL,
                        p4_96 = NULL,
                        gap384 = c(T,F)[2],
                        plotValues = c(T,F)[1],
                        isvars_factor = c(T,F)[1]
                        ){
  
  ### This function takes a data.frame with two columns, the first being the well of a 96 wp, the second being
  ### some values of interest, and rearranges them into a 384 plate.
  ### 
  
  # pX_96, you need to provide a df with two columns, wells (eg. A1, B1, C1...) in the first column and
  # any value on the second colum
  
  # gap384 tells whether 96 to 384 should be pipetted in the conventional way (mirorred) or by quadrants 
  
  # all 96 wells must be included, even if values are empty in second column
  
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




# function to plot 384 result  ------------------------------------------------------



plot_384 <- function(plate384){
 
  png("plate.png", h = 400, w = 600)
  ggplot(plate384, aes(Col, Row, fill=factor(return_values))) + geom_raster() + ggtitle("random Ct Data")
  dev.off()
  
  paste("384 layour image saved in:", getwd())
  
}

# convert 384 to 96wp #####

conv_384_96 <- function(data_384){
  
  #this functioin takes a long data frame where first colum is the 384 well plate and the others are information,
  #and return the same format but separated as 96wp in lists.
  
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
    
    match_idx <- match(data_384[,1], as.character(dict_subset[,1]))
    
    match_wells <-  dict_subset[na.omit(match_idx),]
    
    match_wells[,"quart"] = quart
    
    data_subset <- data_384[!is.na(match_idx),]
    
    return(cbind(data_subset, match_wells[,c(2,3)]))
    
    })
  
  tmp <- do.call(rbind,tmp)
  
  return(tmp)
}


# mock result to play with ----------------------------------------------

# tmp <-
#   conv_96_384(p1_96 = plate1,
#               p2_96 = plate2,
#               p3_96 = plate3,
#               p4_96 = plate4
#   )


#png("plate.png", h = 400, w = 600)
# ggplot(tmp, aes(Col, factor(Row, levels = rev(levels(Row))), fill=factor(return_values))) + geom_raster() + ggtitle("384wp map of drugs")+
#   geom_text(aes(label = return_values), size = 2.3)
#dev.off()

