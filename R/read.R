# read.R - CONSTRUCT FLR objects from files in a path containing jjms output
# FLjjm/R/read.R

# Copyright WMR, 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


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
#' bio <- readFLBjjm(name="mod1.00",
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
#' indices <- readFLIsjjm(name="mod1.00",
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
#' fisheries <- readFLFsjjm(name="mod1.00",
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
#' bio <- readFLSjjm(name="mod1.00",
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
#' bio <- readFLSsjjm(name="mod1.00_2stk",
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
#' rps <- readFLRPsjjm(name="mod1.00",
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
#' om <- readFLomjjm(name="mod1.00",
#'   path=system.file("ext-data", "single_stock", package="FLjjm"))
#' summary(om)
#' # Two stock OM
#' omtwo <- readFLomjjm(name="mod1.00_2stk",
#'   path=system.file("ext-data", "two_stock", package="FLjjm"))
#' summary(omtwo)

readFLomjjm <- function(name, path, iter=NULL, ...) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  # GET No. stocks
  nstks <- mod[[1]]$info$output$nStock

  # BUILD FLBiols
  if(nstks == 1)
    biols <- FLBiols(CJM=buildFLBjjm(mod))
  else if (nstks == 2)
    biols <- buildFLBsjjm(mod)

  # BUILD FLFisheries
  if(nstks == 1)
    fisheries <- buildFLFsjjm(mod)
  else {
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
  if(nstks == 1)
    refpts <- FLPars(CJM=buildFLRPsjjm(mod))
  else if (nstks == 2)
    refpts <- FLPars(Southern=buildFLRPsjjm(mod, 1), North=buildFLRPsjjm(mod, 2))

  om <- FLombf(name=name, biols=biols, fisheries=fisheries,
    refpts=refpts, FCB=FCB(fcb2int(guessfcb(biols,
          fisheries), biols, fisheries)))

  # McMC
  if(!is.null(iter)) {
    outmc <- readMCeval(path, iters=iter)
 
    for(i in seq(nstks)) {
    
      # refpts
      om@refpts[[i]] <- outmc$refpts[[i]]
    
      # N
      om@biols[[i]]@n <- outmc$n[[i]]

      # deviances
      deviances(om@biols[[i]]) <- outmc$deviances[[i]]

      # selex
    }
    for(i in seq(4)) {
      # ASSIGN selex
      om@fisheries[[i]][[1]]@catch.sel <- outmc$catch.sel[[i]] %/%
        apply(outmc$catch.sel[[i]], 2, max)
      # CALCULATE effort
      effort(om@fisheries[[i]]) <- quantMeans(outmc$partfs[[i]] /
        outmc$catch.sel[[i]])
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
#' oem <- readFLoemjjm(name="mod1.00",
#'   path=system.file("ext-data", "single_stock", package="FLjjm"))
#' oem
#' # Two stock OM
#' oemtwo <- readFLoemjjm(name="mod1.00_2stk",
#'   path=system.file("ext-data", "two_stock", package="FLjjm"))
#' oemtwo

readFLoemjjm <- function(name, path, method=cjm.oem, iter=1, ...) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  # GET No. stocks
  nstks <- mod[[1]]$info$output$nStock

  # BUILD observations
  if(nstks == 1) {
    stk <- propagate(buildFLSjjm(mod), iter=iter)
    idx <- lapply(buildFLIsjjm(mod), propagate, iter=iter)
    obs <- list(CJM=list(stk=stk, idx=idx))
  } else if (nstks == 2) {
    stk <- lapply(buildFLSsjjm(mod), propagate, iter=iter)
    idx <- lapply(buildFLIsjjm(mod), propagate, iter=iter)
    Southern <- list(stk=stk$Southern, idx=idx[c(1,2,3,4,7)])
    North <- list(stk=stk$North, idx=idx[c(5,6)])
    obs <- list(Southern=Southern, North=North)
  }

  # dat & ctl
  dat <- mod[[1]]$data
  ctl <- mod[[1]]$control

  if(iter > 1) {
    dat <- setNames(rep(list(dat), iter), nm=seq(iter))
    ctl <- setNames(rep(list(ctl), iter), nm=seq(iter))
  }

  obs$dat <- dat
  obs$ctl <- ctl

  return(FLoem(method=method, observations=obs, ...))

} # }}}

# readRep (FLQuants stock.n, harvest) {{{

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

  return(FLQuants(stock.n=N, harvest=TotF))

} # }}}

# readMod {{{

readMod <- function(name, path) {

  mod <- readJJM(name, path = file.path(path, "config"),
    input = file.path(path, "input"), output = file.path(path, "results"))

  return(mod)
} # }}}

# readMCeval {{{

readMCeval <- function(path, file="mceval.rep", iters=max(tab$iter)) {

  # LOAD file
  tab <- fread(file.path(path, file), verbose=FALSE,
    showProgress=FALSE) 
  setnames(tab, c("iter", "name", "unit", "year", "age", "data"))

  nits <- max(tab$iter)

  # THIN
  if(iters != nits) {
    tab <- tab[iter %in% seq.int(1, nits, by=round(nits / iters)),]
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
    as.FLQuant(x[, .(iter, year, age, data)], units="1000 t")))

  # EXTRACT catch.sel
  sels <- tab[name == "Sel_fsh", .(iter, unit, year, age, data)]
 
  selqs <- FLQuants(lapply(split(sels, by="unit"), function(x)
    as.FLQuant(x[, list(iter, year, age, data)], units="")))

  # EXTRACT index.sel
  isels <- tab[name == "Sel_ind", .(iter, unit, year, age, data)]
 
  iselqs <- FLQuants(lapply(split(isels, by="unit"), function(x)
    as.FLQuant(x[, list(iter, year, age, data)], units="")))

  # EXTRACT F by fishery
  ffish <- tab[name == "F_faa", .(iter, unit, year, age, data)]
 
  ffishs <- FLQuants(lapply(split(ffish, by="unit"), function(x)
    as.FLQuant(x[, list(iter, year, age, data)], units="f")))

  return(list(refpts=refpts, n=nqs, deviances=deqs, catch.sel=selqs,
    landings.n=caaqs, index.sel=iselqs, partfs=ffishs))
}
# }}}
