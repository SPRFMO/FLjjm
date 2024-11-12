# build.R - DESC
# FLjjm/R/build.R

# Copyright (c) WUR, 2024.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


#' Build Functions for FLjjm Package
#'
#' These functions are used to build various FLR (Fisheries Library in R) objects 
#' from the output of the JJMS (Just Another Management Strategy) stock assessment 
#' model. The functions handle different types of outputs including biological, 
#' fishery, stock, and index data.
#'
#' The primary functions include:
#' \itemize{
#'   \item \code{buildFLBjjm}: Constructs an FLBiol object from jjm.output.
#'   \item \code{buildFLBsjjm}: Builds multiple FLBiol objects from jjm.output.
#'   \item \code{buildFLFsjjm}: Creates FLFisheries objects from jjm.output.
#'   \item \code{buildFLIsjjm}: Generates FLIndices objects from jjm.output.
#'   \item \code{buildFLRPsjjm}: Constructs FLPar objects with reference points from jjm.output.
#'   \item \code{buildFLSojjm}: Builds FLStock objects from jjm.output considering areas.
#'   \item \code{buildFLSjjm}: Constructs FLStock objects from jjm.output.
#'   \item \code{buildFLSsjjm}: Creates multiple FLStock objects from jjm.output.
#'}
#'
#' Each function takes a jjm.output object as input, which is the result of running 
#' the JJMS model. They process this output into various FLR objects for further 
#' analysis and use within the FLR framework.
#' @name build
#'
#' @author Iago Mosqueira (WMR) \email{iago.mosqueira@wur.nl}
#'
#' @seealso \code{\link[FLCore]{FLBiol}}, \code{\link[FLFishery]{FLFisheries}}, 
#' \code{\link[FLCore]{FLIndices}}, \code{\link[FLCore]{FLStock}}
NULL

# buildFLBjjm {{{

#' Build an FLBiol from a jjm.output object
#'
#' @param out A *jjm.output* object, as returned by readJJM.
#' @param stock Stock to extract, of relevance on 2-stock model runs, *numeric*.
#' @param spwn Proportion of the year when spawning takes place.
#'
#' @return An object of class FLBiol.
#'
#' @seealso [FLCore::FLBiol]

buildFLBjjm <- function(out, stock=1, name="CJM") {
  
  out <- out[[1]]

  # EXTRACT
  info <- out$info
  data <- out$data
  output <- out$output[[stock]]
  control <- out$control
  params <- out$parameters

  # SET dimnames
  dmns <- list(
    # age
    age=seq(c(data$age)[1], c(data$age)[2]),
    # year
    year=seq(c(data$year)[1], c(data$year)[2])
    # unit
    # season
    # area
  )

  # n -- output$N
  n <- FLQuant(t(output$N[,-1]), dimnames=dmns, units="1e6")

  # m -- output$M
  m <- FLQuant(t(output$M), dimnames=dmns, units="m")

  # wt -- output$wt_a_pop
  wt <- FLQuant(output$wt_a_pop, dimnames=dmns, units="kg")

  # spwn -- mid-November
  spwn <- FLQuant((c(data$Pspwn) - 1) / 12, dimnames=dmns["year"], quant="age", units="")

  # mat -- output$mature_a
  mat <- predictModel(model=~mat, mat=FLQuant(output$mature_a, dimnames=dmns, units=""))

  # fec

  # rec
  # TODO SET params by regime

  rec <- predictModel(
    # control$SrType == 1, bevholtss3
    model=bevholtss3()$model,
    
    # params: s = params$steepness[1], R0 = params$log_Rzero, 
    #   v=output$msy_mt[1, 11]
    params=FLPar(s=params$steepness[stock], R0=exp(params$log_Rzero[stock]),
      v=output$msy_mt[1, 11], units=c("", "1e6", "1000 t")),
    
    # deviances
    deviances=FLQuant(exp(output$rec_dev[output$rec_dev[,1] %in% dmns$year, 2]),
      dimnames=list(age=1, year=dmns$year), units="")
  )

  bio <- FLBiol(name=name,
    desc=paste(info$output$model, info$data$version, sep=" - "),
    n = n, m = m, wt = wt, spwn = spwn, mat = mat, rec = rec)

  # dev(s,i)=mod_rec(s,i)-SRecruit(Sp_Biom(s,i-rec_age),cum_regs(s)+yy_sr(s,i))
  # output$Stock_Rec[,4][output$Stock_Rec[,1] %in% dmns$year] - rec(bio)
  rec$deviances <- residuals(
    FLQuant(output$Stock_Rec[,4][output$Stock_Rec[,1] %in% dmns$year],
    dimnames=list(age=1, year=dmns$year)), rec(bio))

  return(bio)
} # }}}

# buildFLBsjjm : FLBiols {{{
buildFLBsjjm <- function(out) {

  nstks <- out[[1]]$info$output$nStock
  
  bios <- FLBiols(lapply(seq(nstks), buildFLBjjm, out=out))
      names(bios) <- c("Southern", "North")

  return(bios)
}
# }}}

# buildFLFsjjm : FLFisheries {{{

# FLFishery
# |-- capacity: NA
# |-- effort: F_fsh_* [, 3]
# |-- hperiod: data$Imonths
# |-- vcost: NA
# |-- fcost: NA
# |-- orevenue: NA
# |-- crewshare: NA
# `-- CJM
#     |-- landings.n: output$C_fsh_*
#     |-- landings.wt: output$wt_fsh_*
#     |-- discards.n: 0
#     |-- discards.wt: NA
#     |-- catch.sel: output$sel_fsh_
#     `-- price: NA

#' Build FLFisheries from JJMS Model Output
#'
#' Constructs an `FLFisheries` object from a JJMS model output, processing data for 
#' each fishery and creating corresponding `FLCatch` and `FLFishery` objects.
#'
#' @param out JJMS model output object.
#' @param stock The stock number to extract fisheries data for, default is 1.
#'
#' @return An `FLFisheries` object constructed from the JJMS model output.
#' @export
buildFLFsjjm <- function(out, stock=1) {

  out <- out[[1]]

  # EXTRACT
  info <- out$info
  data <- out$data
  outp <- out$output[[stock]]
  control <- out$control
  params <- out$parameters

  # FISHERIES

  # GET fishery names for stock
  fis <- c(outp$Fshry_names)
  # GET all fishery names
  fnms <- data$Fnames
  # SUBSET to loop over
  idx <- match(fis, fnms)

  res <- setNames(nm=fis)

  for(i in idx) {

    # FLCatch

    # landings.n: outp$C_fsh_#
    lan <- FLQuant(t(outp[[paste0("C_fsh_", i)]][,-1]),
      dimnames=list(age=1:12, year=outp[[paste0("C_fsh_", i)]][,1]), 
      units="1e6")
    # landings.wt: outp$wt_fsh_#
    lawt <- FLQuant(t(outp[[paste0("wt_fsh_", i)]][,-1]),
      dimnames=list(age=1:12, year=outp[[paste0("wt_fsh_", i)]][,1]), 
      units="kg")
    # catch.sel: outp$sel_fhs_#
    # BUG: sel + catch.q
    csel <- FLQuant(t(outp[[paste0("sel_fsh_", i)]][,-c(1, 2)]),
      dimnames=list(age=1:12, year=outp[[paste0("sel_fsh_", i)]][,2]), units="")
    # RESCALE catch.sel
    csel <- csel %/% apply(csel, 2, max)

    flc <- FLCatch(name="CJM", landings.n=lan, landings.wt=lawt,
      catch.sel=csel)

    # discards = 0
    discards.n(flc) <- 0
    units(discards.n(flc)) <- units(lan)
    discards.wt(flc) <- landings.wt(flc)

    # FLFishery
    
    # effort = Faa / Saa
    eff <- quantMeans(FLQuant(t(outp[[paste0("F_age_", i)]][,-1]),
      dimnames=list(age=1:12, year=outp[[paste0("F_age_", i)]][,1]),
      units="") / csel)
    
    # capacity

    # hperiod
    res[[fnms[i]]] <- FLFishery(name=fnms[i],
      desc=paste(info$output$model, info$data$version, sep=" - "),
      effort=eff, CJM=flc)
    }

  return(FLFisheries(res[outp$Fshry_names]))
} # }}}

# buildFLIsjjm : FLIndices {{{

# FLIndex
# |-- index: data$Index
# |-- index.var: data$Indexerr
# |-- catch.n: data$Ipropage
# |-- catch.wt: data$Iwtatage
# |-- effort: NA
# |-- sel.pattern: output$sel_ind_*
# `-- index.q: output$q_*

#' Build an FLIndices object from a jjm.output list
#'
#' This function creates an `FLIndices`, a list of `FLIndexBiomass` objects,
#' each of them containing the data and estimates of a single index of abundance.
#' Slots on each `FLIndexBiomass` object are filled with the following information:
#' - index: `data$Index`.
#' - index.var: `data$Indexerr`.
#' - catch.n: `data$Ipropage`.
#' - catch.wt: `data$Iwtatage`.
#' - effort: Empty.
#' - sel.pattern: `output@sel_ind_i`, where `i` refers to the index number.
#' - index.q: `output@q_i`, where `i` refers to the index number.
#' - range 'startf' and 'endf': (`data$Imonths` - 1) / 12.
#'
#' @param out A one or two-stock, single run, jjm.output object.
#' @param stock=1 Stock for which indices are to be loaded.
#'
#' @return An `FLIndices` object.
#'
#' @examples
#' idx <- readFLIsjjm("h1_1.07",
#'   path = system.file("ext-data", "single_stock", package="FLjjm"))
#' summary(idx)
#' plot(idx)

buildFLIsjjm <- function(out) {

  out <- out[[1]]

  # EXTRACT
  info <- out$info
  data <- out$data
  outp <- out$output
  control <- out$control
  params <- out$parameters

  # GET no. indices
  nidx <- data$Inum[1]
  # NAMES in output
  nms <- data$Inames
  # INDEX positions
  idx <- match(nms, data$Inames)

  # LOAD data$Index - @index
  idxs <- FLQuants(lapply(idx, function(x) {
    dat <- data$Index[,x]
    FLQuant(dat, dimnames=list(age="all", year=names(dat)),
      units="")
  }))
  names(idxs) <- nms

  # LOAD data$Indexerr - @index.var
  idxe <- FLQuants(lapply(idx, function(x) {
    dat <- data$Indexerr[,x]
    FLQuant(dat, dimnames=list(age="all", year=names(dat)),
      units="")
  }))
  names(idxe) <- nms

  # LOAD data$Iwtatage - @catch.wt
  idxw <- FLQuants(lapply(idx, function(x) {
    dat <- t(data$Iwtatage[,,x])
    names(dimnames(dat))[2] <- "year"
    FLQuant(dat, units = "kg")
  }))
  names(idxw) <- nms

  # LOAD data$Ipropage - @catch.n
  idxpa <- FLQuants(lapply(idx, function(x) {
    dat <- t(data$Ipropage[,,x])
    names(dimnames(dat))[2] <- "year"
    FLQuant(dat, units = "")
  }))
  names(idxpa) <- nms

  # LOAD output$sel_ind_* - @sel.pattern
  if(info$output$nStock == 2) {

    # BUG: index by stock
   idxsp1 <- lapply(paste0("sel_ind_", c(1:4, 7)), function(x) {
      FLQuant(t(outp[[1]][[x]][, -c(1:2)]),
        dimnames=list(year=outp[[1]][[x]][,2]), units="", quant="age")
    })
    idxsp2 <- lapply(paste0("sel_ind_", c(5, 6)), function(x) {
      FLQuant(t(outp[[2]][[x]][, -c(1:2)]),
        dimnames=list(year=outp[[2]][[x]][,2]), units="", quant="age")
    })
    idxsp <- FLQuants(c(idxsp1[c(1, 2, 3, 4)], idxsp2[c(1, 2)], idxsp1[c(5)]))
  } else {
    idxsp <- lapply(paste0("sel_ind_", idx), function(x) {
      FLQuant(t(outp[[1]][[x]][, -c(1:2)]),
        dimnames=list(year=outp[[1]][[x]][,2]), units="", quant="age")
    })
  }
  names(idxsp) <- nms


  # GET output$q_* - @index.q
  if(info$output$nStock == 2) {
    
    idxq1 <- lapply(paste0("q_", c(1:4, 7)), function(x) {
      FLQuant(outp[[1]][[x]][,2], dimnames=list(year=outp[[1]][[x]][,1]),
        units="")
    })
    idxq2 <- lapply(paste0("q_", c(5, 6)), function(x) {
      FLQuant(outp[[2]][[x]][,2], dimnames=list(year=outp[[2]][[x]][,1]), 
        units="")
    })
    idxq <- FLQuants(c(idxq1[c(1, 2, 3, 4)], idxq2[c(1, 2)], idxq1[c(5)]))
  } else {
    idxq <- lapply(paste0("q_", idx), function(x) {
      FLQuant(outp[[1]][[x]][,2], dimnames=list(year=outp[[1]][[x]][,1]), units="")
    })
  }
  
  names(idxq) <- nms

  # GET index timing
  idxf <- data$Imonths[idx]

  # CREATE FLIndex objects
  inds <- mapply(FLIndexBiomass, index=idxs, index.var=idxe,
    catch.wt=idxw, catch.n=idxpa,
    sel.pattern=idxsp, index.q=idxq,
    name=nms, range=lapply(idxf, function(x) c(startf=(x-1)/12, endf=(x-1)/12)),
    SIMPLIFY=FALSE)
  
  # SET year limits from idxq
  inds <- mapply(window, x=inds, start=lapply(idxq, dims, 'minyear'),
    end=lapply(idxq, dims, 'maxyear'))

  return(FLIndices(inds))

} # }}}

# buildFLRPsjjm : FLPar refpts {{{

#' Construct an FLPar contaning reference points from a jjm.output object
#'
#' An object of class *FLPar* is created from the information in a jjm.output
#' object and for a given stock, if more than one is present.
#' The function currently extracts the following reference points from the
#' `msy_mt` data.frame in the `output` element of `jjm.output`:
#' - MSY: Maximum Sustainable Yield, in thousands of tonnes.
#' - SB0: Virgin spawning biomass, in thousands of tonnes.
#' - SBmsy: Spawning biomass at MSY, in thousands of tonnes.
#' - Fmsy: Fishing mortality at MSY.
#'
#' @param name Name of the ctl model file, *character*.
#' @param path Path ot the model folder structure, *character*.
#' @param stock=1 Stock to extract, of relevance on 2 stocks model runs, *numeric*.
#'
#' @return An object of class FLPar.
#'
#' @seealso [jjmR::readJJM] [FLCore::FLBiol] buildFLBjjm
#' @examples
#' rps <- readFLRPsjjm(name = "h1_1.07",
#'   path = system.file("ext-data", "single_stock", package="FLjjm"))
#' summary(rps)

buildFLRPsjjm <- function(out, stock=1) {

  out <- out[[1]]

  # EXTRACT
  outp <- out$output[[stock]]

  msy_mt <- outp$msy_mt

  len <- dim(msy_mt)[1]
  
  FLPar(
    # MSY (Mt): 8 (MSY)
    MSY=msy_mt[len, 'msy'],

    # SB0: 11 (Bzero)
    SB0=msy_mt[len, 'bzero'],

    # Bmsy: 10 (Bmsy)
    SBMSY=msy_mt[len, 'bmsy'],

    # Fmsy: 5 (Fmsy)
    FMSY=msy_mt[len, 'fmsy'],

    units=c(rep("1000 t", 3), "f"))
} # }}}

# buildFLSojjm : FLStock output {{{

# FLStock output w/areas
# |-- landings.n: output$C_fsh_*
# |-- landings.wt: output$wt_fsh_*
# |-- discards.n: 0
# |-- discards.wt: landings.wt
# |-- stock.n: output$N
# |-- stock.wt: output$wt_a_pop
# |-- m: output$M
# |-- mat: output$mature_a
# |-- harvest:
# |-- harvest.spwn: data$Pspwn
# `-- m.spwn: data$Pspwn

buildFLSojjm <- function(out, stock=1, name="CJM") {
  
  out <- out[[1]]

  # EXTRACT
  info <- out$info
  data <- out$data
  output <- out$output[[stock]]
  control <- out$control
  params <- out$parameters

  # SET dimnames
  dmns <- list(
    # age
    age=seq(c(data$age)[1], c(data$age)[2]),
    # year
    year=seq(c(data$year)[1], c(data$year)[2])
    # unit
    # season
    # area
  )

  # stock.n -- output$N
  stock.n <- FLQuant(t(output$N[,-1]), dimnames=dmns, units="1e6")

  # stock.wt -- output$wt_a_pop
  stock.wt <- FLQuant(output$wt_a_pop, dimnames=dmns, units="kg")

  # m -- output$M
  m <- FLQuant(t(output$M), dimnames=dmns, units="m")

  # mat -- output$mature_a
  mat <- FLQuant(output$mature_a, dimnames=dmns, units="")

  # spwn -- data$Pspwn
  spwn <- FLQuant((c(data$Pspwn) - 1) / 12,
    dimnames=dmns, units="")

  stk <- FLStock(name=name,
    desc=paste(info$output$model, info$data$version, sep=" - "),
    stock.n=stock.n, stock.wt=stock.wt, m=m, mat=mat,
    harvest.spwn=spwn, m.spwn=spwn)

  stock(stk) <- computeStock(stk)

  # GET fishery names for stock
  fis <- c(output$Fshry_names)
  # GET all fishery names
  fnms <- data$Fnames
  # SUBSET to loop over
  idx <- match(fis, fnms)

  # areas
  res <- setNames(nm=fis)

  # landings.n: outp$C_fsh_#
  lan <- lapply(idx, function(i) {
    FLQuant(t(output[[paste0("C_fsh_", i)]][,-1]),
      dimnames=list(age=dmns$age, year=output[[paste0("C_fsh_", i)]][,1]), units="1e6")
  })

  lawt <- lapply(idx, function(i) {
    # landings.wt: outp$wt_fsh_#
    FLQuant(t(output[[paste0("wt_fsh_", i)]][,-1]),
      dimnames=list(age=dmns$age, year=output[[paste0("wt_fsh_", i)]][,1]), units="kg")
  })

  landings.n <- Reduce(abind, lan)
  landings.wt <- Reduce(abind, lawt)
  dimnames(landings.n)$area <- dimnames(landings.wt)$area <- fis

  discards.n <- landings.n %=% 0
  discards.wt <- landings.wt

  landings.n(stk) <- landings.n
  landings.wt(stk) <- landings.wt
  discards.n(stk) <- discards.n
  discards.wt(stk) <- discards.wt

  landings(stk) <- computeLandings(stk)
  discards(stk) <- computeDiscards(stk)
  catch(stk) <- computeCatch(stk, "all")

  # COMPUTE harvest
  harvest(stk) <- harvest(stock.n, areaSums(catch.n(stk)), m)

  range(stk, c("minfbar", "maxfbar")) <- c(2, 6)

  return(stk)

} # }}}

# buildFLSjjm : FLStock input {{{

# FLStock w/areas
# |-- landings: data$Fcaton
# |-- landings.n: data$Fagecomp
# |-- landings.wt: data$Fwtatage
# |-- discards.n: 0
# |-- discards.wt: landings.wt
# |-- stock.wt: control$Pwtatage
# |-- m: control$N_Mort
# |-- mat: control$Pmatatage
# |-- harvest.spwn: data$Pspwn
# `-- m.spwn: data$Pspwn

buildFLSjjm <- function(out, stock=1, name="CJM") {
  
  out <- out[[1]]

  # EXTRACT
  info <- out$info
  data <- out$data
  control <- out$control

  # SET dimnames
  dmns <- list(
    # age
    age=seq(c(data$age)[1], c(data$age)[2]),
    # year
    year=seq(c(data$year)[1], c(data$year)[2])
    # unit
    # season
    # area
  )
  
  # stock.wt -- control$Pwtatage
  stock.wt <- FLQuant(c(control$Pwtatage[stock,]), dimnames=dmns, units="kg")

  # m -- control$N_Mort
  m <- FLQuant(control$N_Mort[1,stock], dimnames=dmns, units="m")

  # mat -- control$Pmatatage
  mat <- FLQuant(c(control$Pmatatage[stock,]), dimnames=dmns, units="")

  stk <- FLStock(name=name,
    desc=paste(info$data$version, sep=" - "),
    m=m, mat=mat,
    stock.wt=stock.wt,
    harvest.spwn=mat %=% (c(data$Pspwn) - 1) / 12,
    m.spwn=mat %=% (c(data$Pspwn) - 1) / 12)

  # GET fisheries for stock
  idx <- which(control$SelMatrix[1, seq(c(data$Fnum))] == stock)

  # GET names
  fis <- data$Fnames[idx]

  # areas
  res <- setNames(nm=fis)

  # landings.n: data$Fagecomp
  landings.n <- FLQuant(c(aperm(data$Fagecomp[,,idx, drop=FALSE], c(2,1,3))),
    dimnames=c(dmns, list(area=fis)), units="1e6")

  # landings.wt: data$Fwtatage
  landings.wt <- FLQuant(c(aperm(data$Fwtatage[,,idx, drop=FALSE], c(2,1,3))),
    dimnames=c(dmns, list(area=fis)), units="kg")

  discards.n <- landings.n %=% 0
  discards.wt <- landings.wt

  landings.n(stk) <- landings.n
  landings.wt(stk) <- landings.wt
  discards.n(stk) <- discards.n
  discards.wt(stk) <- discards.wt

  # landings
  landings(stk) <- FLQuant(c(data$Fcaton[, idx]), dimnames=list(age="all",
    year=dmns$year, area=fis), units="1000 t")
  discards(stk) <- computeDiscards(stk)
  
  # catch(stk) <- computeCatch(stk, "all")[1:3]
  catch(stk) <- metrics(stk, metrics=list(catch='landings',
    catch.n='landings.n', catch.wt='landings.wt'))

  return(stk)

} # }}}

# buildFLSsjjm {{{

#' Build Multiple FLStock Objects from a jjm.output Object
#'
#' This function creates a collection of FLStock objects from a jjm.output object. 
#' It is particularly useful for multi-stock assessments.
#'
#' @param out A jjm.output object, typically the output of a stock assessment model.
#'
#' @return An FLStocks object containing multiple FLStock objects.
#'
#' @seealso \code{\link[FLCore]{FLStock}}, \code{\link[FLCore]{FLStocks}}
#'
#' @export
buildFLSsjjm <- function(out) {
  
  # Validate input
  if (!inherits(out, "jjm.output")) {
    stop("The 'out' parameter should be a 'jjm.output' object.")
  }

  # Number of stocks
  nstks <- out[[1]]$info$output$nStock
  
  if (nstks <= 0) {
    stop("The 'out' object does not contain any stock information.")
  }

  # Build FLStock objects for each stock
  stks <- FLStocks(lapply(seq_len(nstks), function(idx) {
    buildFLSjjm(out, stock=idx)
  }))
  
  # Assigning names (Optional)
  # If specific names are known for the stocks, they can be assigned here.
  # Example: names(stks) <- c("Stock1", "Stock2", ...)

  return(stks)
}
# }}}
