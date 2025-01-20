# jjm.R - BUILD jjm data and control objects from FLR objects
# FLjjm/R/jjm.R

# Copyright WMR, 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#         Karolina MOLLA GAZI (WMR) <karolina.mollagazi@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# buildjjmdata {{{

# UPDATED data list elements
# - years: dims$minyear, dims$maxyear
# - Fcaton: landings()
# - Fcatonerr: extra row
# - FnumyearsA: update
# - FnumyearsL: update
# - Fageyears: extra row
# - Flengthyears: extra row
# - Fagesample: extra row
# - Flengthsample: extra row
# - Fagecomp: landings.n()
# - Flengthcomp: landings.n() * vB()
# - Fwtatage: extra row
#
# - Inumyears: update
# - Iyears: extra row
# - Index: index()
# - Indexerr: index.var()
# - Inumageyears: update
# - Inumlengthyears: update
# - Iyearslength: update
# - Iyearsage: extra row 
# - Iagesample: extra row 
# - Ipropage: catch.n()
# - Ilengthsample: extra row
# - Iproplength: does not need updating ?
# - Iwtatage: catch.wt()

buildjjmdata <- function(stk, idx, data, lengthcomp_F3=NULL, ...) {
  
  if (is(stk, "FLStocks")) {

    # GET names and dims
    fnms <- dimnames(stk[[1]])$area
    maxy <- dims(stk[[1]])$maxyear
    miny <- dims(stk[[1]])$minyear
    dyrs <- ac(seq(miny, maxy))
 
    # GET single FLQuants

    lan <- Reduce(abind, lapply(stk, landings))[,,,,c(1,2,4,3)]
    lana <- Reduce(abind, lapply(stk, landings.n))[,,,,c(1,2,4,3)]
    lanw <- Reduce(abind, lapply(stk, landings.wt))[,,,,c(1,2,4,3)]

  } else if(is(stk, "FLStock")) {

    # GET names and dims
    fnms <- dimnames(stk)$area
    maxy <- dims(stk)$maxyear
    miny <- dims(stk)$minyear
    dyrs <- ac(seq(miny, maxy))

    lan <- landings(stk)
    lana <- landings.n(stk)
    lanw <- landings.wt(stk)

  } else {
    stop("buildjjmdata requires an 'FLStock' or 'FLStocks' as 'stk'.")
  }
  
  # ARE there any new years? 
  if(data$years[2] < maxy) {
    newys <- seq(data$years[2] + 1, maxy)
    nys <- length(newys)
  } else {
    newys <- NULL
    nys <- 0
  }
  
  # -- EXTRACT slots for .dat
  
  # CREATE output list
  res <- data
  
  # -- UPDATE F elements
 
  # REWRITE years
  res$years <- matrix(c(miny, maxy), ncol = 2, nrow = 1, 
    dimnames = list("years", c("first year", "last year")))

  # UPDATE or SUBSTITUTE Fcaton
  res$Fcaton <- dmatrix(lan)

  # RECREATE Fcatonerr
  res$Fcatonerr <- dmatrix(lan %=% data$Fcatonerr[1,1])
   
  # UPDATE or SUBSTITUTE Flengthyears
  if(!is.null(newys)) {
    res$Flengthyears <- rbind(res$Flengthyears,
      matrix(c(rep(NA, nys), rep(NA, nys), newys, rep(NA, nys)), ncol=4,
      nrow=length(newys), dimnames=list(years=newys,
      dimnames(res$Flengthyears)[[2]])))
  } else {
    res$Flengthyears <- res$Flengthyears[dimnames(lan)$year,]
  }
  
  # UPDATE FnumyearsL
  res$FnumyearsL[] <- apply(res$Flengthyears, 2, function(x) sum(!is.na(x)))
  
  # UPDATE Flengthsample
  if(!is.null(newys)) {
    flsm <- flsl <- res$Flengthyears
    flsm[] <- rep(res$Flengthsample['2018',], each=dim(flsm)[1])
    flsm[is.na(flsl)] <- NA
    res$Flengthsample <- flsm
  } else {
    res$Flengthsample <- res$Flengthsample[dimnames(lan)$year,]
  }

  # UPDATE Fagecomp, F1, 2, 4
  laa <- aperm(lana[drop=TRUE], c(2,1,3))
  names(dimnames(laa)) <- names(dimnames(res$Fagecomp))
  dimnames(laa)[3] <- dimnames(res$Fagecomp)[3]

  # UPDATE Flengthcomp
  if(!is.null(newys)) {
    dm <- dim(data$Flengthcomp)
    dmn <- dimnames(data$Flengthcomp)
    dmn$years <- dimnames(laa)$years

    # CREATE array
    len <- array(NA, dim=dm + c(nys, 0, 0), dimnames=dmn)
    
    # ADD old data
    len[1:dm[1],,3] <- data$Flengthcomp[,,3]
    
    # REPEAT last year data, JIC
    len[seq(dm[1] + 1, dm[1] + nys),,3] <- rep(data$Flengthcomp[dm[1],,3], each=nys)

    # ADD new data for F3
    if(!is.null(lengthcomp_F3)) {
      if(length(newys) > 2)
        len[ac(newys), ,3] <- lengthcomp_F3[ac(newys),]
    }

    res$Flengthcomp <- len

  } else {
    res$Flengthcomp <- data$Flengthcomp[dyrs,,]
  }

  # DROP F3 agecomp
  laa[,,3] <- NA

  # ADD landings as Fagecomp for newys
  # TODO: CHANGE past for hindcasting
  if(!is.null(newys)) {
    laa[dimnames(res$Fagecomp)$years,,] <- res$Fagecomp
    res$Fagecomp <- laa
  }

  # UPDATE or SUBSTITUTE Fageyears
  res$Fageyears <- arryears(res$Fagecomp)

  # UPDATE Fagesample
  if(!is.null(newys)) {
    fasm <- fasl <- res$Fageyears
    fasm[] <- rep(res$Fagesample['2018',], each=dim(fasm)[1])
    fasm[is.na(fasl)] <- NA
    res$Fagesample <- fasm
  } else {
    res$Fagesample <- res$Fagesample[dimnames(lan)$year,]
  }

  # UPDATE FnumyearsA 
  res$FnumyearsA[] <- apply(res$Fageyears, 2, function(x) sum(!is.na(x)))
 
  # UPDATE Fwtatage
  lawt <- aperm(lanw[drop=TRUE], c(2,1,3))
  dimnames(lawt) <- dimnames(laa)
  
  res$Fwtatage <- lawt

  # -- UPDATE I elements
  
  # -- INDEX

  # UPDATE Index [FLQs - model.frame - matrix]
  index <- as.matrix(model.frame(lapply(idx,
    function(x) window(index(x), start=miny, end=maxy)), drop=TRUE)[,-1])
  dimnames(index) <- list(years=seq(miny, maxy),
    paste0("index", seq(length(idx))))
  
  res$Index <- index

  # UPDATE Iyears: Years where Index !NA
  iyrs <- res$Index
  iyrs[] <- as.numeric(rownames(res$Index))
  iyrs[is.na(res$Index)] <- NA

  res$Iyears <- iyrs

  # UPDATE Inumyears:  No. of years where Index !NA
  res$Inumyears[] <- apply(res$Iyears, 2, function(x) sum(!is.na(x)))

  # UPDATE Indexerr
  inder <- as.matrix(model.frame(
    lapply(idx, function(x) window(index.var(x), start=miny, end=maxy)),
      drop=TRUE)[,-1])

  dimnames(inder) <- list(years=seq(miny, maxy),
    paste0("index", seq(length(idx))))

  # BUG:
  inder[is.na(iyrs)] <- NA

  res$Indexerr <- inder

  # -- AGE
  # UPDATE Ipropage [FLQs - array]
  prop <- Reduce(abind, lapply(idx, function(x)
    window(catch.n(x), start=miny, end=maxy)))
  prop <- aperm(prop[drop=TRUE], c(2, 1, 3))
  dimnames(prop) <- list(years=seq(miny, maxy), age=1:12,
    paste0("index", seq(length(idx))))

  res$Ipropage <- prop

  # UPDATE Iyearsage
  iysa <- apply(res$Ipropage, 3, function(x) rowSums(is.na(x)) != 0)
  iyrs[] <- as.numeric(rownames(iysa))
  iyrs[iysa] <- NA

  res$Iyearsage <- iyrs

  # UPDATE Inumageyears
  res$Inumageyears[] <- apply(res$Iyearsage, 2, function(x) sum(!is.na(x)))

  # UPDATE Iagesample [c(1,2,4)]
  iisa <- res$Iyearsage[ac(seq(miny, maxy)),]
  samp <- unlist(apply(res$Iagesample, 2, function(x) unique(na.omit(x))))
  for(i in names(samp))
    iisa[!is.na(iisa[, i]), i] <- c(samp[i])

  res$Iagesample <- iisa

  # UPDATE Iwtatage
  # NOTE  - UGLY HACK. Iwtatage has data for years where Iyearsage is NA. This
  # is not in catch.wt(FLIndex). So if no newys, trim, else extend and apend.
  if(is.null(newys))
    res$Iwtatage <- data$Iwtatage[dyrs,,]
  else {
    adms <- list(years=dyrs, age=1:12, paste0("index", seq(length(idx))))
    iwta <- array(NA, dimnames=adms, dim=unlist(lapply(adms, length)))
    # ASSIGN existing data
    iwta[dimnames(data$Iwtatage)[[1]],,] <- data$Iwtatage
    iwta[ac(newys),,] <- c(aperm(Reduce(abind, lapply(idx, function(x)
      window(catch.wt(x), start=miny, end=maxy)[, ac(newys)])), c(2,1,3,4,5,6)))

    # DEBUG
    iwta[is.na(iwta)] <- 0.01
    
    res$Iwtatage <- iwta
  }

  # -- LENGTH
  
  # UPDATE Iproplength size
  ldms <- list(years=ac(seq(miny, maxy)), lengths=ac(seq(data$lengths[1], data$lengths[2])),
    paste0("index", seq(length(idx))))
  res$Iproplength <- array(NA, dimnames=ldms, dim=unname(lapply(ldms, length)))

  # UPDATE Iyearslength
  res$Iyearslength <- matrix(NA, dimnames=ldms[c(1, 3)], nrow=length(ldms[[1]]), 
    ncol=length(ldms[[3]]))

  # UPDATE Inumlengthyears
  res$Inumlengthyears[] <- apply(res$Iyearslength, 2, function(x) sum(!is.na(x)))
  
  # UPDATE Ilengthsample
  res$Ilengthsample <- res$Iyearslength

  return(res)
  
} # }}}

# buildjjmctl {{{

# UPDATED ctl list elements
# - F1_info: update last element 
# - F2_info: update last element 
# - F3_info: update last element
# - F4_info: update last element

#' Updating dat and ctl file lists from FLstock(s) and FLIndices
#'
#' @param stk [TODO:description]
#' @param idx [TODO:description]
#' @param dat [TODO:description]
#' @param ctl [TODO:description]
#'
#' @return [TODO:description]
#' @export
#'
#' @examples
#' data(cjmstk)
#' ctl <- buildjjmctl(stk, idx, mod$data, mod$control)
#' data(cjmstks)
#' ctl <- buildjjmctl(stks, idxs, mods$data, mods$control)

buildjjmctl <- function(stk, idx, dat, ctl, ...){
  
  if (is(stk, "FLStocks")) {
    maxy <- dims(stk[[1]])$maxyear
    miny <- dims(stk[[1]])$minyear
  } else if (is(stk, "FLStock")) {
    maxy <- dims(stk)$maxyear
    miny <- dims(stk)$minyear
  }

  if(dat$years[2] < maxy) {
    newys <- seq(dat$years[2] + 1, maxy)
    nys <- length(newys)
  } else {
    newys <- NULL
  }

  yrs <- seq(miny, maxy)
  
  res <- ctl

  # UPDATE Nyrs_sr_

  res$Nyrs_sr_1 <- seq(res$Nyrs_sr_1[1], yrs[length(yrs) - 3])
  res$Nyrs_sr[1] <- length(res$Nyrs_sr_1)

  if (is(stk, "FLStocks")) {
    res$Nyrs_sr_3 <- seq(res$Nyrs_sr_1[3], yrs[length(yrs) - 3])
    res$Nyrs_sr[3] <- length(res$Nyrs_sr_3)
  }

  # UPDATE F_selchangeYear
  if(is.null(newys)) {
    res$F1_selchangeYear <- ctl$F1_selchangeYear[ctl$F1_selchangeYear %in% yrs]
    res$F2_selchangeYear <- ctl$F2_selchangeYear[ctl$F2_selchangeYear %in% yrs]
    res$F3_selchangeYear <- ctl$F3_selchangeYear[ctl$F3_selchangeYear %in% yrs]
    res$F4_selchangeYear <- ctl$F4_selchangeYear[ctl$F4_selchangeYear %in% yrs]
  } else {
    res$F1_selchangeYear <- c(res$F1_selchangeYear,
      res$F1_selchangeYear[length(res$F1_selchangeYear)] + seq(nys))
    res$F2_selchangeYear <- c(res$F2_selchangeYear,
      res$F2_selchangeYear[length(res$F2_selchangeYear)] + seq(nys))
    res$F3_selchangeYear <- c(res$F3_selchangeYear,
      res$F3_selchangeYear[length(res$F3_selchangeYear)] + seq(nys))
    res$F4_selchangeYear <- c(res$F4_selchangeYear,
      res$F4_selchangeYear[length(res$F4_selchangeYear)] + seq(nys))
  }

  # UPDATE F_info
  res$F1_info[length(res$F1_info)] <- length(res$F1_selchangeYear)
  res$F2_info[length(res$F2_info)] <- length(res$F2_selchangeYear)
  res$F3_info[length(res$F3_info)] <- length(res$F3_selchangeYear)
  res$F4_info[length(res$F4_info)] <- length(res$F4_selchangeYear)
  
  # UPDATE F_selchange
  res$F1_selchange <- rep_len(ctl$F1_selchange, res$F1_info[length(res$F1_info)])
  res$F2_selchange <- rep_len(ctl$F2_selchange, res$F2_info[length(res$F2_info)])
  res$F3_selchange <- rep_len(ctl$F3_selchange, res$F3_info[length(res$F3_info)])
  res$F4_selchange <- rep_len(ctl$F4_selchange, res$F4_info[length(res$F4_info)])

  return(res)
} # }}}

# auxiliary functions {{{

# dmatrix()
  dmatrix <- function(x) {
    df <- model.frame(divide(x, dim=5), drop=TRUE)
    res <- as.matrix(df)[,-1]
    dimnames(res) <- list(years=df[,1], paste0("fishery", seq(ncol(res))))
    return(res)
  }
  
  # arryears()
  arryears <- function(x) {
    arryrs <- arrdat <- apply(x, 3, function(y) rowSums(is.na(y)) != 0)
    arryrs[] <- as.numeric(rownames(arryrs))
    arryrs[arrdat] <- NA
    # DROP years dimnames name
    names(dimnames(arryrs)) <- c("", "")
    return(arryrs)
  }
# }}}
