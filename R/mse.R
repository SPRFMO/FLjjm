# mse.R - DESC
# FLjjm/R/mse.R

# Copyright (c) WUR, 2021-24.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# cjm.oem {{{

cjm.oem <- function(stk, deviances, observations, stability=1,
  wts=TRUE, jjms=TRUE, F3sel, args, tracking) {

  # SET dimension args
  spread(args)
  dyrs <- ac(seq(dy - frq + 1, dy))

  # CALL sampling.oem for stk & idx
  res <- sampling.oem(stk=stk, deviances=deviances,
    observations=observations, args=args, tracking=tracking)

  #
  nidx <- names(observations$idx)

  # DROP 2022 from idx[[2]]
  if("Chile_AcousN" %in% nidx)
    res$idx[[2]] <- res$idx[[2]][, -39]

  # UPDATE idx: 2, 3, 6, 7
  for(i in nidx[nidx %in% c("Chile_AcousN", "Chile_CPUE", "Peru_CPUE", "Offshore_CPUE")])
    observations$idx[[i]][, dyrs] <- res$idx[[i]][, dyrs]

  # DROP last year from idx[[7]]
  if("Offshore_CPUE" %in% nidx)
    res$idx[["Offshore_CPUE"]] <- window(res$idx[["Offshore_CPUE"]], end=dy-1)

  if(jjms) {

    # Add F3 length samples
    f3lengths <- lapply(seq(it), function(j) {
      cjmage2len(iter(landings.n(res$stk)[,,,,3][, dyrs], j),
        iter(expand(F3sel, year = dyrs), j))
    })

    # CREATE dat & ctl
    dat <- observations$dat
    ctl <- observations$ctl

    for(i in seq(it)) {
      ctl[[i]] <- buildjjmctl(iter(res$stk, i), iter(res$idx, i), dat[[i]],
        ctl[[i]])
      dat[[i]] <- buildjjmdata(iter(res$stk, i), iter(res$idx, i), dat[[i]],
        lengthcomp_F3=f3lengths[[i]])
    }

    # STORE new observations in observations
    observations$stk[, dyrs] <- res$stk[, dyrs]

    # KEEP new dat & ctl
    observations[c('dat', 'ctl')]  <- list(dat, ctl)

    # ATTACH to stk
    attr(res$stk, "ctl") <- ctl
    attr(res$stk, "dat") <- dat
  }

  return(list(stk = res$stk, idx = res$idx,
    observations = observations, tracking = res$tracking))
}
# }}}

# jjms.sa {{{

#' JJMS Stock Assessment
#'
#' Performs a JJMS stock assessment, modifying and running the JJMS model with provided arguments.
#'
#' @param stk Stock data.
#' @param idx Index data.
#' @param args Argument list.
#' @param tracking Tracking information.
#' @param ... Additional arguments.
#'
#' @return A list with the resulting stock data, tracking information, and arguments.
#' @export
#' @examples

jjms.sa <- function(stk, idx, args, tracking, ...) {

  # ASSEMBLE args, names as for jjms()
  sargs <- list(...)
  sargs$stock <- stk
  sargs$indices <- idx
  sargs$dat <- attr(stk, "dat")
  sargs$ctl <- attr(stk, "ctl")
  sargs$mp <- TRUE

  # CALL jjms
  res <- do.call(jjms, sargs)

  # SET convergence  if run
  track(tracking, "conv.est", args$ay) <- 1
  
  list(stk = res, tracking = tracking, args=list(dat=attr(res, "dat"),
    ctl=attr(res, "ctl")))

} # }}}

# cjmfwc {{{

#' Converts FLQuants with Fleets as Areas into a fwdControl Object
#'
#' Converts `FLQuants` objects, typically returned by a Harvest Control Rule (HCR) module, 
#' into a `fwdControl` object with different behavior based on the number of stocks.
#'
#' @param flqs An `FLQuants` object as returned by a HCR module.
#' @param quant The quant to use, defaults to 'catch'.
#' @param nstocks Number of stocks, defaults to 1.
#'
#' @return A `fwdControl` object with the corresponding FCB slot.
#' @export
cjmfwc <- function(flqs, quant = "catch", nstocks = 1) {
  yr <- dimnames(flqs[[1]])$year

  if (nstocks == 1) {
    # One stock scenario
    return(fwdControl(
      list(year = yr, quant = quant, value = c(flqs[[1]][,,,,1]), fishery = 1, catch = 1),
      list(year = yr, quant = quant, value = c(flqs[[1]][,,,,2]), fishery = 2, catch = 1),
      list(year = yr, quant = quant, value = c(flqs[[1]][,,,,3]), fishery = 3, catch = 1),
      list(year = yr, quant = quant, value = c(flqs[[1]][,,,,4]), fishery = 4, catch = 1),
      FCB = FCB(F = 1:4, C = 1, B = 1)
    ))
  } else {
    # Multiple stocks scenario
    return(fwdControl(
      list(year = yr, quant = quant, value = c(flqs[[1]][,,,,1]), fishery = 1, catch = 1),
      list(year = yr, quant = quant, value = c(flqs[[1]][,,,,2]), fishery = 2, catch = 1),
      list(year = yr, quant = quant, value = c(flqs[[2]][,,,,1]), fishery = 3, catch = 1),
      list(year = yr, quant = quant, value = c(flqs[[1]][,,,,3]), fishery = 4, catch = 1),
      FCB = FCB(F = 1:4, C = 1, B = c(1, 1, 2, 1))
    ))
  }
}
# }}}

# fwdmov.om {{{

#' Forward Movement Operation Model
#'
#' Simulates the movement of fish populations between two areas in a fishery 
#' operation model. Adjusts population numbers based on movement rates, fishing 
#' mortality, and natural mortality.
#'
#' @param om A fishery operation model object.
#' @param ctrl Control measures data frame or matrix.
#' @param FCB Fishing mortality, default is derived from `ctrl`.
#' @param rates Movement rates matrix.
#' @param time Time step for the simulation, default is 0.
#' @param ... Additional arguments.
#'
#' @return A list containing the updated fishery operation model object.
#' @export
fwdmov.om <- function(om, ctrl, FCB = FCB(ctrl), rates, time = 0, ...) {

  args <- list(...)
  yr <- unique(ctrl$year)

  # Convert to proportions moving 'from'
  rates <- prop.table(rates, c(1:2))

  # Movement rates per age
  n2s <- expand(FLQuant(rates[, 'North', 'Southern'], dimnames = list(age = 1:12)), year = yr)
  n2s[10:12, ] <- n2s[10, ]
  s2n <- expand(FLQuant(rates[, 'Southern', 'North'], dimnames = list(age = 1:12)), year = yr)
  s2n[10:12, ] <- s2n[10, ]

  # Extract numbers at age
  movn <- lapply(biols(om), function(x) n(x)[, ac(yr - 1)])
  
  if (time == 0) {
    # Direct correction of numbers at age
    SN <- movn[['Southern']] %*% s2n
    NS <- movn[['North']] %*% n2s
    n(om@biols[["Southern"]])[, ac(yr - 1)] <- movn[['Southern']] + NS - SN
    n(om@biols[["North"]])[, ac(yr - 1)] <- movn[['North']] + SN - NS
  } else {
    # Compute Z for time > 0
    fs <- FLQuants(harvest(om[, ac(yr - 1)]))
    ms <- lapply(biols(om), function(x) m(x)[, ac(yr - 1)])
    zs <- lapply(fs + ms, '*', time)
    movn <- Map('*', movn, lapply(zs, function(x) exp(-x)))

    # Calculate moving fish
    SN <- movn[['Southern']] %*% s2n
    NS <- movn[['North']] %*% n2s
    movn[['Southern']] <- movn[['Southern']] + NS - SN
    movn[['North']] <- movn[['North']] + SN - NS
    
    # Backtrack numbers to start of the year
    movn <- Map('*', movn, lapply(zs, function(x) exp(x)))
    n(om@biols[["Southern"]])[, ac(yr - 1)] <- movn[['Southern']]
    n(om@biols[["North"]])[, ac(yr - 1)] <- movn[['North']]
  }

  # Update fishery operation model with results
  args$object <- om
  args$control <- ctrl
  om <- do.call("fwd", args)

  return(list(om = om))
}
# }}}

# fwdmov {{{

#' Forward Movement Simulation
#'
#' This function performs a forward movement simulation on a fishery model object 
#' using specified control measures and rates over time.
#'
#' @param object The fishery model object to be simulated.
#' @param control A data frame or matrix containing control measures for each year.
#' @param rates A list or vector of rates used in the simulation.
#' @param time An optional time parameter, default is 0.
#'
#' @return The updated fishery model object after applying the forward movement simulation.
#' @export
fwdmov <- function(object, control, rates, time = 0) {
  # Unique years from control data
  yrs <- unique(control$year)

  # Progress bar initialization
  p <- progressor(along = yrs, offset = 0L)

  # Extract initial population numbers for Southern and North stocks
  origs <- n(object@biols[["Southern"]])[, ac(yrs[1] - 1)]
  orign <- n(object@biols[["North"]])[, ac(yrs[1] - 1)]

  # Loop over each year
  for (y in yrs) {
    id <- which(control$year == y)
    ctrl <- control[id, ]

    # Perform forward movement operation
    object <- fwdmov.om(object, ctrl, rates = rates, time = time)[[1]]

    # Update progress bar
    p(message = sprintf("year: %s", y))
  }

  # Reassign initial population numbers for the first year
  n(object@biols[["Southern"]])[, ac(yrs[1] - 1)] <- origs
  n(object@biols[["North"]])[, ac(yrs[1] - 1)] <- orign

  return(object)
}
# }}}

# cjmage2len {{{

#' Convert Age Composition to Length Composition for Landings
#'
#' This function converts age composition data from landings to length composition 
#' using specified biological parameters and selectivity.
#'
#' @param landings Matrix of landings at age.
#' @param selex Matrix of selectivity at age.
#' @param ess Effective sample size for each year, default is 100.
#' @param L_inf Asymptotic length, default is 80.4.
#' @param k Growth coefficient, default is 0.16.
#' @param L_0 Theoretical length at age zero, default is 18.
#' @param M Natural mortality rate, default is 0.33.
#' @param CVlen Coefficient of variation in length, default is 0.09.
#' @param ages Vector of ages, default is derived from landings.
#' @param sample_type Type of sampling, default is 'catch'.
#'
#' @return A matrix representing length composition for each year.
#' @export
cjmage2len <- function(landings, selex, ess = 100, L_inf = 80.4, k = 0.16,
  L_0 = 18, M = 0.33, CVlen = 0.09, ages = an(dimnames(landings)$age),
  sample_type = "catch") {

  # Convert landings and selectivity to matrices
#  N_at <- as.matrix(landings, drop = TRUE)
#  S_a <- as.matrix(selex, drop = TRUE)

  # BUG:
  nys <- dim(landings)[2]
  N_at <- matrix(c(landings), nrow=12, ncol=nys)
  S_a <- matrix(c(selex), nrow=12, ncol=nys)

  # Number of years
  tyears <- ncol(landings)

  # Set up consistent sample sizes
  comp_sample <- rep(ess, tyears)

  # Calculate length at age
  L_a <- L_inf - (L_inf - L_0) * exp(-k * (ages - 1))

  # Define length-height relationship
  lh <- list(highs = seq(10.5, 50.5), lows = seq(9.5, 49.5), L_a = L_a, CVlen = CVlen)

  # Convert age to length composition
  lens <- AgeToLengthComp(lh, S_a, tyears, N_at, comp_sample,
    sample_type = "catch")

  # Extract and format the results
  res <- lens$LF
  dimnames(res) <- list(year = dimnames(landings)$year, len = lh$highs - 0.5)

  return(res)
}
# }}}

# AgeToLengthComp {{{

#' Convert Age Composition to Length Composition
#'
#' This function converts age composition data to length composition data based on 
#' provided length-height (lh) relationships, selectivity (S_a), and other parameters.
#'
#' @param lh A data frame or list containing length-height relationship data.
#' @param S_a Matrix of selectivity at age.
#' @param tyears Total number of years in the data.
#' @param N_at Matrix of numbers at age over time.
#' @param comp_sample Vector of sample sizes for each year.
#' @param sample_type Type of sample, default is 'catch'. Can be 'catch' or other types.
#'
#' @return A list containing matrices: probabilities being in a length bin given age (plba),
#'         probabilities being harvested at an age (page), probabilities of sampling a 
#'         given length bin (plb), and length frequencies (LF).
#' @export
AgeToLengthComp <- function(lh, S_a, tyears, N_at, comp_sample, sample_type = 'catch') {
  # Probability being in a length bin given age
  lbprobs <- function(mnl, sdl) {
    pnorm(lh$highs, mnl, sdl) - pnorm(lh$lows, mnl, sdl)
  }
  vlprobs <- Vectorize(lbprobs, vectorize.args = c("mnl", "sdl"))
  plba <- t(vlprobs(lh$L_a, lh$L_a * lh$CVlen))
  plba <- plba / rowSums(plba)

  # Probability being in harvested at an age
  page <- matrix(ncol = dim(plba)[1], nrow = tyears)
  for (y in 1:tyears) {
    if (sample_type == "catch") {
      page[y, ] <- N_at[, y] * S_a[, y]
    } else {
      page[y, ] <- N_at[, y]
    }
  }
  page <- page / rowSums(page)

  # Probability of sampling a given length bin
  plb <- matrix(ncol = length(lh$highs), nrow = tyears)
  for (y in 1:tyears) {
    plb[y, ] <- page[y, ] %*% plba
  }
  plb <- plb / rowSums(plb)

  # Length frequencies
  LF <- matrix(NA, ncol = length(lh$highs), nrow = tyears)
  rownames(LF) <- 1:tyears
  for (y in 1:tyears) {
    if (!is.na(sum(plb[y, ]))) {
      LF[y, ] <- rmultinom(n = 1, size = comp_sample[y], prob = plb[y, ])
    }
  }

  # Output
  Outs <- list(plba = plba, plb = plb, page = page, LF = LF)
  return(Outs)
}
# }}}

# metrics(FLStocks) {{{

#' Calculate Average Fishing Mortality across FLStocks
#'
#' This method calculates the average fishing mortality (fbar) across multiple 
#' FLStock objects contained within an FLStocks object.
#'
#' @param object An FLStocks object.
#' @return A numeric value representing the average fishing mortality.
#' @export
setMethod("fbar", signature(object="FLStocks"), function(object) {
  total_fbar <- Reduce(`+`, lapply(object, "fbar"))
  avg_fbar <- total_fbar / length(object)
  return(avg_fbar)
})


#' Calculate Total Spawning Stock Biomass across FLStocks
#'
#' This method calculates the total spawning stock biomass (ssb) by summing 
#' the ssb of each FLStock object within an FLStocks object.
#'
#' @param object An FLStocks object.
#' @return A numeric value representing the total spawning stock biomass.
#' @export
setMethod("ssb", signature(object="FLStocks"), function(object) {
  total_ssb <- Reduce(`+`, lapply(object, "ssb"))
  return(total_ssb)
})
#' Calculate Total Catch across FLStocks
#'
#' This method calculates the total catch by summing the catch (accounting for 
#' area sums) of each FLStock object within an FLStocks object.
#'
#' @param object An FLStocks object.
#' @return A numeric value representing the total catch.
#' @export
setMethod("catch", signature(object="FLStocks"), function(object) {
  total_catch <- Reduce(`+`, lapply(object, function(x) areaSums(catch(x))))
  return(total_catch)
})
# }}}
