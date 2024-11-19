# slick.R - DESC
# FLjjm/R/slick.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# slick_performance {{{

#' Compute performance metrics for MPs and OM in slick format
#'
#' This function computes performance metrics for MPs over future years and for OM over past years.
#'
#' @param x A list of MP or OM projections runs, of class 'list' or 'FLmse'.
#' @param om An OM, of class 'FLom' or 'FLombf'.
#' @param statistics A list of performance statistics to compute, 'list'.
#' @param metrics Metrics to compute, 'list'.
#' @return A combined data.table with performance metrics
#'
#' @examples
#' slick_performance(x = my_mps_data, om = my_om_data, statistics = my_statistics)
#'
#' @export

slick_performance <- function(x, om, statistics,
  metrics=list(C=catch, SB=ssb, F=fbar)) {

  # SET 'x' as list
  if(!is(x, 'list'))
     x <- list(x)

  # dims
  fyrs <- dims(x[[1]])[c('minyear', 'maxyear')]
  pyrs <- dims(om)[c('minyear', 'maxyear')]

  # COMPUTE perf for MPs over future years
  fut <- performance(x, metrics=metrics, statistics=statistics,
    year = seq(fyrs$minyear, fyrs$maxyear))

  # ADD om name
  fut[, om := name(om)]

  # COMPUTE perf for OM over past years
  pas <- performance(om, metrics=metrics,
    statistics=statistics[c("SB", "SBMSY", "C")],
    year = seq(pyrs$minyear, fyrs$minyear))

  # ADD om name
  pas[, om := name(om)]

  # ADD mp names
  res <- cbind(pas, data.table(mp=rep(names(x), each=dim(pas)[1])))

  # COMBINE
  res <- rbindlist(list(fut, res), use.names=TRUE)

  # ORDER columns
  setcolorder(res, neworder=c('om', 'mp', 'biol', 'statistic', 'year',
    'iter', 'data', 'name', 'desc'))

  # SET search index
  setorder(res, om, mp, year, iter)

  return(res)
}
# }}}
