# load.R - DESC
# FLjjm/R//load.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# loadFLSjjms {{{

#' Load FLStocks from JJMS Model
#'
#' This function loads FLStocks from JJMS model directories located at a specified path. 
#' It can handle both single and multiple stock scenarios and offers an option to combine 
#' the loaded stocks into a single object.
#'
#' @param path The path to the directory containing the JJMS model data.
#' @param combine Boolean, if TRUE, combine the loaded FLStocks into a single object.
#'
#' @return FLStocks object(s) loaded from the specified path.
#' @export
loadFLSjjms <- function(path, combine = FALSE) {
  # Get directories and names
  dirs <- list.dirs(path, recursive = FALSE)
  names <- list.files(path)

  # Get model name from the first directory
  modelName <- namejjms(dirs[1])

  # Check the number of stocks
  numStocks <- as.numeric(nStocksjjms(modelName, dirs[1]))

  # Load FLStocks
  if (numStocks == 1) {
    flss <- foreach(dir = dirs, .errorhandling = "stop", 
                    .final = function(x) FLStocks(x, names = names)) %dopar% {
      readFLSjjm(modelName, dir)
    }
    if (combine) {
      flss <- Reduce(combine, flss)
    }
  } else {
    # Multiple stocks scenario (TODO: Returns list of FLStocks(2))
    flss <- foreach(dir = dirs, .errorhandling = "stop") %dopar% {
      readFLSsjjm(modelName, dir)
    }
    names(flss) <- names
  }

  return(flss)
}


#' Get Number of Stocks in JJMS Model
#'
#' Reads a JJMS model control file to determine the number of stocks.
#'
#' @param name The name of the JJMS model.
#' @param path The path to the directory containing the JJMS model data.
#'
#' @return Numeric value representing the number of stocks in the JJMS model.
#' @export
nStocksjjms <- function(name, path) {
  ctlFilePath <- file.path(path, "config", paste0(name, ".ctl"))
  nlines <- readLines(ctlFilePath, n = 6)
  return(as.numeric(nlines[6]))
}

#' Get Name of JJMS Model
#'
#' Reads the first configuration file in a JJMS model directory to extract the model's name.
#'
#' @param path The path to the directory containing the JJMS model data.
#'
#' @return A string with the name of the JJMS model.
#' @export
namejjms <- function(path) {
  configFile <- list.files(file.path(path, "config"), full.names = TRUE)[1]
  nlines <- readLines(configFile, n = 4)
  return(trimws(nlines[4]))
}

#' Construct Components of Fisheries Model
#'
#' This function constructs various components of a fisheries model from a given model object.
#' It builds biological data, fisheries data, indices, reference points, and stock data.
#'
#' @param mod The model object from which components are to be constructed.
#'
#' @return A list containing biological data, fisheries data, indices, reference points, 
#'         stock data, and model data and control settings.
#' @export
yourFunctionName <- function(mod) {
  # Build biological data from the model
  bio <- FLBiols(CJM = buildFLBjjm(mod))
  
  # Build fisheries data
  fis <- buildFLFsjjm(mod)

  # Build indices
  ins <- buildFLIsjjm(mod)

  # Build reference points
  rps <- buildFLRPsjjm(mod)

  # Build stock data
  stk <- buildFLSjjm(mod)

  # Return constructed components
  return(list(biols = bio, fisheries = fis, indices = ins, refpts = rps, stk = stk,
              dat = mod[[1]]$dat, ctl = mod[[1]]$ctl))
}

#' Load JJMS Model Components
#'
#' Loads a JJMS model from a given path and constructs various components of a fisheries 
#' model, including biological data, fisheries data, indices, reference points, and stock data.
#'
#' @param name The name of the JJMS model.
#' @param path The path to the directory containing the JJMS model data.
#'
#' @return A list containing biological data, fisheries data, indices, reference points, 
#'         stock data, and model data and control settings.
#' @export
loadJJMS <- function(name, path) {
  # Load model run
  mod <- readJJM(name, path = file.path(path, "config"),
                 input = file.path(path, "input"), output = file.path(path, "results"))

  # Get number of stocks
  nstks <- mod[[1]]$info$output$nStock
  
  # Build object(s) based on number of stocks
  if (nstks > 1) {
    bio <- FLBiols(lapply(seq(nstks), buildFLBjjm, out = mod))
    names(bio) <- c("Southern", "North")

    fis <- FLFisheries(Reduce(c, lapply(seq(nstks), buildFLFsjjm, out = mod)))
    fis <- fis[c("N_Chile", "SC_Chile_PS", "FarNorth", "Offshore_Trawl")]
    names(fis[[1]]) <- "Southern"
    names(fis[[2]]) <- "Southern"
    names(fis[[3]]) <- "North"
    names(fis[[4]]) <- "Southern"

    ins <- buildFLIsjjm(out = mod)
    ins <- ins[c("Chile_AcousCS", "Chile_AcousN", "Chile_CPUE", "DEPM", 
                 "Peru_Acoustic", "Peru_CPUE", "Chinese_CPUE", "Offshore_CPUE")]
    
    rps <- lapply(setNames(seq(nstks), nm = c("Southern", "North")), 
                  buildFLRPsjjm, out = mod)
    stk <- buildFLSsjjm(mod)
  } else {
    bio <- FLBiols(CJM = buildFLBjjm(mod))
    fis <- buildFLFsjjm(mod)
    ins <- buildFLIsjjm(mod)
    rps <- buildFLRPsjjm(mod)
    stk <- buildFLSjjm(mod)
  }

  return(list(biols = bio, fisheries = fis, indices = ins, refpts = rps, stk = stk,
              dat = mod[[1]]$dat, ctl = mod[[1]]$ctl))
}

