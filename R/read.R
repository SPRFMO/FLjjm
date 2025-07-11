# read.R - CONzMRUCT FLR objects from files in a path containing jjms output
# FLjjm/R/read.R

# Copyright WMR, 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


globalVariables(c("data", "age", ".", "unit"))

# readFLBjjm (FLBiol) {{{

#' Create an FLBiol from a JJMS model run directory
#'
#' An object of class *FLBiol* is created from the information in the various
#' files in a *jjms* folder structure. See *buildFLBjjm* for how each slot is
#' populated from the inputs and outputs of *jjms*.
#'
#' @param name Name of the ctl model file, *character*.
#' @param path Path ot the model folder structure, *character*.
#' @param stock=1 Stock to extract, of relevance on 2 stocks model runs, *numeric*.
#'
#' @return An object of class FLBiol.
#'
#' @seealso [jjmR::readJJM] [FLCore::FLBiol] buildFLBjjm
#' @examples
#' bio <- readFLBjjm(name="h1_1.07",
#'   path=system.file("ext-data", "single_stock", package="FLjjm"))
#' summary(bio)

readFLBjjm <- function(name, path, stock=1) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  buildFLBjjm(mod, stock=1)
} # }}}

# readFLIsjjm (FLIndices) {{{

#' Create an FLIndices from a JJMS model run directory
#'
#' An object of class *FLIndices*, a list of *FLIndex* objects, is created
#' from the information in the various files in a *jjms* folder structure.
#' See *buildFLIsjjm* for how each slot is populated from the inputs and
#' outputs of *jjms*.
#'
#' @param name Name of the ctl model file, *character*.
#' @param path Path ot the model folder structure, *character*.
#'
#' @return An object of class FLIndices.
#'
#' @seealso [jjmR::readJJM] [FLCore::FLIndices] [FLCore::FLIndex] buildFLIsjjm
#' @examples
#' indices <- readFLIsjjm(name="h1_1.07",
#'   path = system.file("ext-data", "single_stock", package="FLjjm"))
#' summary(indices)

readFLIsjjm <- function(name, path) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  buildFLIsjjm(mod)
} # }}}

# readFLFsjjm (FLBiol) {{{

#' Create an FLFisheries from a JJMS model run directory
#'
#' An object of class *FLFisheries*, a list of *FLFishery* objects, is created
#' from the information in the various files in a *jjms* folder structure.
#' See *buildFLFsjjm* for how each slot is populated from the inputs and
#' outputs of *jjms*.
#'
#' @param name Name of the ctl model file, *character*.
#' @param path Path ot the model folder structure, *character*.
#'
#' @return An object of class FLIndices.
#'
#' @seealso [jjmR::readJJM] [FLFishery::FLFisheries] [FLFishery::FLFishery] buildFLFsjjm
#' @examples
#' fisheries <- readFLFsjjm(name="h1_1.07",
#'   path = system.file("ext-data", "single_stock", package="FLjjm"))
#' summary(fisheries)

readFLFsjjm <- function(name, path) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  buildFLFsjjm(mod)
} # }}}

# readFLSjjm (FLStock) {{{

#' Create an FLStock from a JJMS model run directory
#'
#' An object of class *FLStock* is created from the information in the various
#' files in a *jjms* folder structure. See *buildFLSjjm* for how each slot is
#' populated from the inputs and outputs of *jjms*.
#'
#' @param name Name of the ctl model file, *character*.
#' @param path Path ot the model folder structure, *character*.
#' @param stock=1 Stock to extract, of relevance on 2 stocks model runs, *numeric*.
#'
#' @return An object of class FLStock.
#'
#' @seealso [jjmR::readJJM] [FLCore::FLStock] buildFLSjjm
#' @examples
#' bio <- readFLSjjm(name="h1_1.07",
#'   path = system.file("ext-data", "single_stock", package="FLjjm"))
#' summary(bio)

readFLSjjm <- function(name, path, stock=1, output=TRUE) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  if(output)
    buildFLSojjm(mod, stock=1)
  else
    buildFLSjjm(mod, stock=1)
} # }}}

# readFLSsjjm (FLStock) {{{

#' Create an FLStocks from a two-stock JJMS model run directory
#'
#' An object of class *FLStocks* is created from the information in the various
#' files in a *jjms* folder structure. See *buildFLSjjm* for how each slot is
#' populated from the inputs and outputs of *jjms*.
#'
#' @param name Name of the ctl model file, *character*.
#' @param path Path ot the model folder structure, *character*.
#'
#' @return An object of class FLStock.
#'
#' @seealso [jjmR::readJJM] [FLCore::FLStock] buildFLSjjm
#' @examples
#' bio <- readFLSsjjm(name="h2_1.07",
#'   path=system.file("ext-data", "two_stock", package="FLjjm"), output=TRUE)
#' summary(bio)

readFLSsjjm <- function(name, path, output=FALSE) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  if(output)
    return(FLStocks(Southern=buildFLSojjm(mod, stock=1, name="Southern"),
      North=buildFLSojjm(mod, stock=2, name="North")))
  else
    return(FLStocks(Southern=buildFLSjjm(mod, stock=1, name="Southern"),
      North=buildFLSjjm(mod, stock=2, name="North")))
} # }}}

# readFLRPsjjm (FLBiol) {{{

#' Create an FLPar contaning reference points from a JJMS model run directory
#'
#' An object of class *FLPar* is created from the information in the output
#' files in a *jjms* folder structure. See *buildFLRPsjjm* to check what
#' reference points are being extracted from the inputs outputs of *jjms*.
#'
#' @param name Name of the ctl model file, *character*.
#' @param path Path ot the model folder structure, *character*.
#' @param stock=1 Stock to extract, of relevance on 2 stocks model runs, *numeric*.
#'
#' @return An object of class FLPar.
#'
#' @seealso [jjmR::readJJM] [FLCore::FLBiol] buildFLBjjm
#' @examples
#' rps <- readFLRPsjjm(name="h1_1.07",
#'   path=system.file("ext-data", "single_stock", package="FLjjm"))
#' summary(rps)

readFLRPsjjm <- function(name, path, stock=1) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  buildFLRPsjjm(mod, stock=1)
} # }}}

# readFLomjjm (FLombf) {{{

#' Create an FLombf from a JJMS model run directory
#'
#' An object of class *FLombf* is created from the information in the various
#' files in a *jjms* folder structure. Three slots are created from the
#' corresponding calls to *buildFLjjm* functions.
#' - @biols, of class *FLBiols* from a call to *buildFLBsjjm*.
#' - @fisheries, of class *FLFisheries*, from a call to *buildFLFsjjm*.
#' - @refpts, of classs *FLPars*, from a call to *buildFLRPsjjm*.
#'
#' @param name Name of the ctl model file, *character*.
#' @param path Path to the model folder structure, *character*.
#'
#' @return An object of class FLBiol.
#'
#' @seealso [jjmR::readJJM] FLombf buildFLBjjm buildFLFsjjm
#' @examples
#' # One stock OM
#' om <- readFLomjjm(name="h1_1.07",
#'   path=system.file("ext-data", "single_stock", package="FLjjm"))
#' summary(om)
#' # Two stock OM
#' omtwo <- readFLomjjm(name="h2_1.07",
#'   path=system.file("ext-data", "two_stock", package="FLjjm"))
#' summary(omtwo)

readFLomjjm <- function(name, path, iter=NULL, ...) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  # GET No. stocks
  nstks <- mod[[1]]$info$output$nStock

  # BUILD FLBiols
  if(nstks == 1) {
    biols <- FLBiols(CJM=buildFLBjjm(mod))
  } else if (nstks == 2) {
    biols <- buildFLBsjjm(mod)
  }

  # BUILD FLFisheries
  if(nstks == 1) {
    fisheries <- buildFLFsjjm(mod)
  } else {
    fisheries <- lapply(seq(nstks), buildFLFsjjm, out=mod)

    fisheries[[1]] <- lapply(fisheries[[1]], function(x) {
      names(x) <- "Southern"
      return(x)
    })
    fisheries[[2]] <- lapply(fisheries[[2]], function(x) {
      names(x) <- "North"
      return(x)
    })
    # ORDER as in 1 stock: N_Chile, SC_Chile_PS, FarNorth, Offshore_Trawl
    fisheries <- FLFisheries(Reduce("c", fisheries)[c(1, 2, 4, 3)])
  }

  # BUILD refpts
  if(nstks == 1) {
    refpts <- FLPars(CJM=buildFLRPsjjm(mod))
  } else if (nstks == 2) {
    refpts <- FLPars(Southern=buildFLRPsjjm(mod, 1), North=buildFLRPsjjm(mod, 2))
  }

  om <- FLombf(name=name, biols=biols, fisheries=fisheries,
    refpts=refpts, FCB=FCB(fcb2int(guessfcb(biols,
          fisheries), biols, fisheries)))

  # McMC
  if(!is.null(iter)) {
    outmc <- readMCeval(path, iters=iter)
 
    # biols
    for(i in seq(nstks)) {
    
      # refpts
      om@refpts[[i]] <- outmc$refpts[[i]]
    
      # N
      om@biols[[i]]@n <- outmc$n[[i]]

      # params
      om@biols[[i]]@rec@params <- FLPar(s=om@biols[[i]]@rec@params$s,
        R0=om@refpts[[i]]$R0, v=om@refpts[[i]]$SB0)

      # deviances
      deviances(om@biols[[i]]) <- outmc$deviances[[i]]
    }
 
    # fisheries
    for(i in seq(4)) {

      # PROPAGATE
      om@fisheries[[i]] <- propagate(om@fisheries[[i]], iter)
      
      # ASSIGN catch.q
      om@fisheries[[i]][[1]]@catch.q['alpha',] <-
        apply(outmc$catch.sel[[i]], 2:6, max)

      # ASSIGN rescaled selex
      om@fisheries[[i]][[1]]@catch.sel <- outmc$catch.sel[[i]] %/% 
        apply(outmc$catch.sel[[i]], 2:6, max)

      # ASSIGN catch.n
      # om@fisheries[[i]][[1]]@landings.n <- outmc$landings.n[[i]]
      
      # CALCULATE effort  = sum(F / sel)
      effort(om@fisheries[[i]]) <- quantMeans(outmc$partfs[[i]] /
        (outmc$catch.sel[[i]]))
    }
    om <- propagate(om, iter)
  }

  args <- list(...)

  if(length(args) > 0)
    for(i in names(args))
      slot(om, i) <- args[[i]]

  return(om)

} # }}}

# readFLoemjjm (FLoem) {{{

#' Create an FLoem from a JJMS model run directory
#'
#' An object of class *FLoem* is created from the information in the various
#' files in a *jjms* folder structure. The *@observations* slot is created
#' containing the past observations of catch and biology (as an *FLStock* or 
#' *FLStocks*) and for the indices of abundance (as an *FLIndices*)
#'
#' @param name Name of the ctl model file, *character*.
#' @param path Path to the model folder structure, *character*.
#'
#' @return An object of class FLoem.
#'
#' @seealso [jjmR::readJJM] [mse::FLoem] buildFLSjjm buildFLIsjjm
#' @examples
#' # One stock OM
#' oem <- readFLoemjjm(name="h1_1.07",
#'   path=system.file("ext-data", "single_stock", package="FLjjm"))
#' oem
#' # Two stock OM
#' oemtwo <- readFLoemjjm(name="h2_1.07",
#'   path=system.file("ext-data", "two_stock", package="FLjjm"))
#' oemtwo

readFLoemjjm <- function(name, path, method=cjm.oem, iter=NULL, ...) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  if(is.null(iter))
    iter <- 1

  # dat & ctl
  dat <- mod[[1]]$data
  ctl <- mod[[1]]$control

  dat <- setNames(rep(list(dat), iter), nm=seq(iter))
  ctl <- setNames(rep(list(ctl), iter), nm=seq(iter))

  # GET No. stocks
  nstks <- mod[[1]]$info$output$nStock

  # BUILD idx observations
  idx <- lapply(buildFLIsjjm(mod), propagate, iter=iter)

  # McMC
  if(iter > 1) {
    outmc <- readMCeval(path, iters=iter)
    for(i in seq(idx)) {
      index.q(idx[[i]]) <- outmc$index.q[[i]]
      sel.pattern(idx[[i]]) <- outmc$index.sel[[i]][, dimnames(idx[[i]])$year]
    }
  }

  # BUILD observations
  if(nstks == 1) {

    stk <- propagate(buildFLSjjm(mod), iter=iter)
    obs <- list(CJM=list(stk=stk, idx=idx, dat=dat, ctl=ctl))

  } else if (nstks == 2) {
    stk <- lapply(seq(2), function(i)
      propagate(buildFLSojjm(mod, i), iter=iter))
    Southern <- list(stk=stk[[1]], idx=idx[c(1,2,3,4,7)])
    North <- list(stk=stk[[2]], idx=idx[c(5,6)])
    obs <- list(Southern=Southern, North=North, dat=dat, ctl=ctl)
  }

  return(FLoem(method=method, observations=obs, ...))

} # }}}

# readRep (stock.n, harvest, refpts) {{{

readRep <- function(file) {

  # READ file
  fil <- readLines(file)

  # FIND section names
  idx <- grep("\\$", fil)
  nms <- fil[idx]

  # N
  ini <- grep("\\$N$", nms)

  n <- fil[seq(idx[ini] + 1, idx[ini + 1] - 1)]

  n <- do.call(rbind, lapply(n, function(x)
    as.numeric(strsplit(trimws(x), " +")[[1]])))

  N <- FLQuant(t(n[, -1]),
    dimnames = list(age = 1:12, year = n[, 1]), units = "1e6")

  # TotF
  ini <- grep("\\$TotF$", nms)

  totf <- fil[seq(idx[ini] + 1, idx[ini + 1] - 1)]

  TotF <- FLQuant(t(do.call(rbind, lapply(totf, function(x)
    as.numeric(strsplit(trimws(x), " ")[[1]])))),
    dimnames = list(age=1:12, year = n[, 1]), units = "f")

  # SB0
  
  ini <- grep("\\$msy_mt$", nms)
  
  msymt <- fil[seq(idx[ini] + 1, idx[ini + 1] - 1)]

  msymt <- do.call(rbind, lapply(msymt, function(x)
    as.numeric(strsplit(trimws(x), " ")[[1]])))

  rps <- FLPar(SB0=msymt[1, 11])

  return(list(stock.n=N, harvest=TotF, refpts=rps))

} # }}}

# readMod {{{

readMod <- function(name, path) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  return(mod)
} # }}}

# readMCeval {{{

readMCeval <- function(path, file=list.files(path, pattern='mceval*')[1], 
  iters=max(tab$iter)) {

  # LOAD file
  tab <- fread(file.path(path, file), verbose=FALSE,
    showProgress=FALSE) 

  setnames(tab, c("iter", "name", "unit", "year", "age", "data"))

  nits <- max(tab$iter)

  # SUBSET (THIN)
  if(iters != nits) {
    # tab <- tab[iter %in% seq.int(1, nits, by=round(nits / iters)),]
    tab <- tab[iter %in% seq.int(1, iters),]
  }

  # EXTRACT refpts
  rps <- tab[name %in% c("R0", "B0", "SB0", "MSY", "SBMSY", "FMSY"), 
    list(unit, name, iter, data)]
  setnames(rps, "name", "params")

  # BUG: CONVERT B0 to SB0
  rps <- rps[params == "B0", params:="SB0"]

  refpts <- lapply(split(rps, by="unit"), function(x) as(x[, -1], "FLPar"))

  refpts <- lapply(refpts, function(x) {
    units(x) <- c("1e6", "1000 t", "f","1000", "1000 t")
    return(x)
  })

  # COMPUTE dynamic SBMSY, last 10 years
  lasten <- seq(tab[name == "Fbar", max(as.numeric(year))] - 9, length=10)
  meansbmsy <- tab[name == "SBMSYy" & year %in% lasten, .(data=mean(data)),
    by=iter]

  # ASSIGN as SBMSY
  refpts[[1]]$SBMSY <- meansbmsy$data

  # EXTRACT n
  ns <- tab[name == "N_stock", list(iter, unit, year, age, data)]
 
  nqs <- FLQuants(lapply(split(ns, by="unit"), function(x)
    as.FLQuant(x[, .(iter, year, age, data)], units="1e6")))

  # EXTRACT SRR deviances
  des <- tab[name == "Deviances", list(iter, unit, year, age, data)]
  deqs <- FLQuants(lapply(split(des, by="unit"), function(x)
    as.FLQuant(x[, .(iter, year, age, data)], units="")))

  # EXTRACT catch.n (landings.n)
  caas <- tab[name == "C_fsh", .(iter, unit, year, age, data)]

  caaqs <- FLQuants(lapply(split(caas, by="unit"), function(x)
    as.FLQuant(x[, .(iter, year, age, data)], units="1e6")))

  # EXTRACT catch.sel
  sels <- tab[name == "Sel_fsh", .(iter, unit, year, age, data)]
 
  selqs <- FLQuants(lapply(split(sels, by="unit"), function(x)
    as.FLQuant(x[, list(iter, year, age, data)], units="")))

  # EXTRACT index.sel
  isels <- tab[name == "Sel_ind", .(iter, unit, year, age, data)]
 
  iselqs <- FLQuants(lapply(split(isels, by="unit"), function(x)
    as.FLQuant(x[, list(iter, year, age, data)], units="")))

  # EXTRACT index.q 
  qs <- tab[name == "Q_ind", .(iter, unit, year, age, data)]

  qqs <- FLQuants(lapply(split(qs, by="unit"), function(x)
    as.FLQuant(x[, list(iter, year, age, data)], units="")))

  # EXTRACT pred ind
  pids <- tab[name == "Pred_ind", .(iter, unit, year, age, data)]

  pidqs <- FLQuants(lapply(split(pids, by="unit"), function(x)
    as.FLQuant(x[, list(iter, year, age, data)], units="")))

  # EXTRACT F by fishery
  ffish <- tab[name == "F_faa", .(iter, unit, year, age, data)]
 
  ffishs <- FLQuants(lapply(split(ffish, by="unit"), function(x)
    as.FLQuant(x[, list(iter, year, age, data)], units="f")))

  # EXTRACT LOO lkhds
  lookhd <- tab[grepl("LOO", name), .(name, iter, unit, year, data)]

  return(list(refpts=refpts, n=nqs, deviances=deqs, catch.sel=selqs,
    landings.n=caaqs, index.sel=iselqs, index.q=qqs, index.hat=pidqs,
    partfs=ffishs, lookhd=lookhd))
}
# }}}
