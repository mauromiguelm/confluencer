#'functions to obtain growth rate and other metrics

#' get derivative from fitted curves
#'
#'@description this function takes the first derivative, or the growth rate equation
#' @param x a regression model
#'
#' @return
#' @export
#'
#' @examples
polynomial_deriv <- function(x) {  #

  terms <- stats::coef(x)

  base::stopifnot(names(terms)[1]=="(Intercept)")

  filter_terms <- terms[-1] #remove intercept

  stopifnot(all(grepl("^poly", names(filter_terms))))

  degrees <- as.numeric(gsub(pattern = "poly\\(.*\\)", replacement = "", names(filter_terms)))

  terms_deriv <- stats::setNames(c(filter_terms * degrees, 0), names(terms))

  terms_deriv[is.na(terms_deriv)] <- 0

  return(matrix(terms_deriv, ncol = 1))

}

#' Title
#'
#' @param data matrix where its first column is time and second column is confluence or other metric
#' @param degree polynomial degree to be used when fitting
#' @param interpolated_points number of time points to be interpolated by regression
#'
#' @return
#' @export
#' @examples
get_growthMetrics <-

  function(data, degree, interpolated_points=10000){

    growth_rates <- list()

    growth_rates$tmp_data_format <- data.frame(Conf = data[,2], Time = data[,1])

    growth_rates$tmp_time <-  seq(0,max(data[,1]),length.out = interpolated_points)

    growth_rates$model <-stats::lm(Conf ~ poly(Time, degree), data = growth_rates$tmp_data_format)

    growth_rates$pred_Conf <- stats::predict(growth_rates$model, newdata = data.frame(Time = growth_rates$tmp_time))

    if(min(growth_rates$pred_Conf)<1){
      growth_rates$pred_Conf <- growth_rates$pred_Conf + abs(min(growth_rates$pred_Conf))+1
    }

    growth_rates$tmp_growth_rate$deriv <- c((diff(growth_rates$pred_Conf)/diff(growth_rates$tmp_time))[1], diff(growth_rates$pred_Conf, differences = 1)/diff(growth_rates$tmp_time,differences = 1))

    growth_rates$tmp_growth_rate$GR <- growth_rates$tmp_growth_rate$deriv / growth_rates$pred_Conf

    # calculating AUC from derivative

    growth_rates$deriv_gcurve <-
      stats::model.matrix(growth_rates$model) %*% polynomial_deriv(growth_rates$model)

    # calculating AUC of growth rate

    # calculate the area under the curve (AUC or integral) of growth curves

    predic_conf <- function(time) stats::predict(growth_rates$model, newdata = data.frame("Time" = time)) #this function takes a time point, and predicts the confluence

    growth_rates$AUC_growthCurve <-

      stats::integrate(  #we use the predic_conf function to calculate the AUC between  the lower and upper bounds, in this case time 0h and 72h.
        predic_conf,0,72
      )

    # calculate the area under the curve (AUC or integral) of first derivative


    id <- order(growth_rates$tmp_time)

    growth_rates$AUC_deriv <- sum(diff(growth_rates$tmp_time[id])*rollmean(growth_rates$tmp_growth_rate$deriv[id],2))

    # calculate the area under the curve (AUC or integral) of growth curve

    id <- order(growth_rates$tmp_time)

    growth_rates$AUC_GR <- sum(diff(growth_rates$tmp_time[id])*rollmean(growth_rates$tmp_growth_rate$GR[id],2))

    # output Data

    return_df <- data.frame(GR = growth_rates$tmp_growth_rate$GR, pref_conf = growth_rates$pred_Conf, deriv = growth_rates$tmp_growth_rate$deriv,
                            time = growth_rates$tmp_time)


    return_metrics <- c(growth_rates$AUC_deriv, growth_rates$AUC_growthCurve[[1]], growth_rates$AUC_GR)

    return_metrics <- stats::setNames(return_metrics, c("AUC_deriv", "AUC_GC", "AUC_GR"))

    model_conf <- growth_rates$model

    return(list(return_metrics, return_df, model_conf))

  }
