# run.R - RUN jjms
# FLjjm/R/run.R

# Copyright WMR, 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# .combinejjmsout {{{
.combinejjmsout <- function(x, y) {
  
  # HACK First combine has x$dat as dat list
  if(!is.null(names(x$dat)[[1]]))
    if(names(x$dat)[[1]] == "years") {
    x$dat <- list(x$dat)
    x$ctl <- list(x$ctl)
  }

  # ONE stock
  if(length(x$stock.n) == 1) {
    return(list(stock.n=combine(x$stock.n, y$stock.n),
      harvest=combine(x$harvest, y$harvest),
      dat=c(x$dat, list(y$dat)), ctl=c(x$ctl, list(y$ctl))))
  
  # TWO stocks
  } else {
    return(list(
      stock.n=combine(x[[1]], y[[1]]),
      harvest=combine(x[[2]], y[[2]]),
      dat=c(x$dat, list(y$dat)), ctl=c(x$ctl, list(y$ctl))))
  }
} # }}}


#' Combine Fisheries Model Results
#'
#' Combines results from two sources of fisheries model runs, handling both single and 
#' multiple stock scenarios. It combines data such as stock numbers, harvest, data, and control.
#'
#' @param x First set of fisheries model results.
#' @param y Second set of fisheries model results.
#'
#' @return A combined list of fisheries model results.
#' @export
.combinejjmsout <- function(x, y) {
  # Handle case where x$dat is a list with the first element named 'years'
  if (!is.null(names(x$dat)[[1]]) && names(x$dat)[[1]] == "years") {
    x$dat <- list(x$dat)
    x$ctl <- list(x$ctl)
  }

  # Combine results for one or two stocks
  if (length(x$stock.n) == 1) {
    # Single stock scenario
    return(list(
      stock.n = combine(x$stock.n, y$stock.n),
      harvest = combine(x$harvest, y$harvest),
      dat = c(x$dat, list(y$dat)), 
      ctl = c(x$ctl, list(y$ctl))
    ))
  } else {
    # Multiple stocks scenario
    return(list(
      stock.n = combine(x[[1]], y[[1]]),
      harvest = combine(x[[2]], y[[2]]),
      dat = c(x$dat, list(y$dat)), 
      ctl = c(x$ctl, list(y$ctl))
    ))
  }
}


#' Run JJMS Model for Fisheries Stock Assessment
#'
#' Executes the JJMS model for fisheries stock assessment. Handles both single and multiple 
#' stocks and can operate in parallel or sequentially. Updates the stock data with the 
#' results from the model runs.
#'
#' @param stock FLStock or list of FLStocks for assessment.
#' @param indices Indices data for the model.
#' @param dat Data for each iteration of the model.
#' @param ctl Control settings for each iteration.
#' @param path Path to save temporary files, defaults to a temporary file path.
#' @param mp If TRUE, runs in parallel; otherwise, runs sequentially.
#' @param clean If TRUE, cleans up temporary files after execution.
#' @param lengthcomp_F3 Optional length composition data for F3.
#'
#' @return The updated FLStock or list of FLStocks after model execution.
#' @export
jjms <- function(stock, indices, dat, ctl, path = tempfile(), mp = FALSE, clean = mp, lengthcomp_F3 = NULL) {
  # Determine the number of iterations
  its <- if(is(stock, "FLStock")) {
    dims(stock)$iter } else { dims(stock[[1]])$iter }

  # MP runs using %dopar%
  if(mp) {

    out <- foreach(i=seq(its), .combine=.combinejjmsout,
      .multicombine=FALSE) %dopar% {
      
      # CREATE new dat & ctl
      nctl <- buildjjmctl(iter(stock, i), iter(indices, i), dat[[i]], ctl[[i]])
      ndat <- buildjjmdata(iter(stock, i), iter(indices, i), dat[[i]],
        lengthcomp_F3=lengthcomp_F3[[i]])

      # CREATE jjm.output
      mod <- list(mod=list(data=ndat, control=nctl,
        info=list(data=list(version="2015MS")),
        parameters=list(), output=list()))

      names(mod) <- ctl[[i]]$modelName
      class(mod) <- "jjm.output"

      # CALL jjms
      run <- runjjms(mod, path=file.path(path, i), args="-nohess", verbose=FALSE)

      # ONE stock
      if(nctl$nStocks == 1) {
        res <- readRep(file.path(run, "results", paste0(ctl[[i]]$modelName, "_1_R.rep")))
      # TWO stocks
      } else {
        # LOAD from each rep file
        res <- list(
          readRep(file.path(run, "results", paste0(ctl[[i]]$modelName, "_1_R.rep"))),
          readRep(file.path(run, "results", paste0(ctl[[i]]$modelName, "_2_R.rep"))))

        # RESHAPE as stock.n, harvest FLQuants
        res <- list(
          stock.n=FLQuants(Southern=res[[1]]$stock.n, North=res[[2]]$stock.n),
          harvest=FLQuants(Southern=res[[1]]$harvest, North=res[[2]]$harvest))
      }

      if(clean)
        unlink(file.path(path, i), recursive = TRUE, force = TRUE)
      c(res, list(dat=ndat, ctl=nctl))
    }
    
    # COMBINE & assign to stock
    if(length(stock) == 1) {
      stock.n(stock) <- out$stock.n
      harvest(stock) <- out$harvest
      
      attr(stock, "dat") <- out$dat
      attr(stock, "ctl") <- out$ctl
    } else {
      stock.n(stock[[1]]) <- out$stock.n[[1]]
      stock.n(stock[[2]]) <- out$stock.n[[2]]
      harvest(stock[[1]]) <- out$harvest[[1]]
      harvest(stock[[2]]) <- out$harvest[[2]]
      
      attr(stock, "dat") <- out$dat
      attr(stock, "ctl") <- out$ctl
    }

  # OM runs using for()
  } else {
    for(i in seq(its)) {
      # CREATE new dat & ctl
      nctl <- buildjjmctl(iter(stock, i), iter(indices, i), dat[[i]], ctl[[i]])
      ndat <- buildjjmdata(iter(stock, i), iter(indices, i), dat[[i]],
        lengthcomp_F3=lengthcomp_F3[[i]])

      mod <- list(mod=list(data=ndat, control=nctl,
        info=list(data=list(version="2015MS")),
        parameters=list(), output=list()))

      names(mod) <- ctl[[i]]$modelName
      class(mod) <- "jjm.output"

      run <- runjjms(mod, path=path)

      # TODO CHECK convergence (R0 grad, covar)
      if(nctl$nStocks == 1) {

        stk <- readFLSjjm(ctl[[i]]$modelName, path = run, output=TRUE)

        # UPDATE slots
        stock@harvest@.Data[,,,,,i] <- stk@harvest@.Data[,,,,,i]
        units(stock@harvest) <- "f"
        stock@stock.n@.Data[,,,,,i] <- stk@stock.n@.Data[,,,,,i]
        units(stock@stock.n) <- "1e6"
        stock@stock@.Data[,,,,,i] <- stk@stock@.Data[,,,,,i]
        units(stock@stock) <- "1000 t"
        stock@landings.n@.Data[,,,,,i] <- stk@landings.n@.Data[,,,,,i]
        stock@catch.n@.Data[,,,,,i] <- stk@catch.n@.Data[,,,,,i]
        stock@catch@.Data[,,,,,i] <- stk@catch@.Data[,,,,,i]
      
      } else {

        stks <- readFLSsjjm(ctl[[i]]$modelName, path = run, output=TRUE)
        # UPDATE slots
        for(j in seq(nctl$nStocks)) {
          stock[[j]]@harvest@.Data[,,,,,i] <- stks[[j]]@harvest@.Data[,,,,,i]
          units(stock[[j]]@harvest) <- "f"
          stock[[j]]@stock.n@.Data[,,,,,i] <- stks[[j]]@stock.n@.Data[,,,,,i]
          units(stock[[j]]@stock.n) <- "1e6"
          stock[[j]]@stock@.Data[,,,,,i] <- stks[[j]]@stock@.Data[,,,,,i]
          units(stock[[j]]@stock) <- "1000 t"
          stock[[j]]@landings.n@.Data[,,,,,i] <- stks[[j]]@landings.n@.Data[,,,,,i]
          stock[[j]]@catch.n@.Data[,,,,,i] <- stks[[j]]@catch.n@.Data[,,,,,i]
          stock[[j]]@catch@.Data[,,,,,i] <- stks[[j]]@catch@.Data[,,,,,i]
        }
      }
    }
  }

  # REMOVE folder
  if(clean)
    unlink(path, recursive = TRUE, force = TRUE)

  return(stock)
} 

#' Run JJMS Model
#'
#' Executes the JJMS model with the given model data, saving the model files to a specified path.
#' The function creates necessary directories, writes model files, and calls the JJMS model.
#'
#' @param mod The JJMS model data to be executed.
#' @param path The path to save model files and run the JJMS model, defaults to a temporary file path.
#' @param args Additional arguments for the JJMS model execution.
#' @param verbose If TRUE, execution details are printed; otherwise, they are suppressed.
#'
#' @return The path where the JJMS model was executed.
#' @export
runjjms <- function(mod, path = tempfile(), args = "", verbose = TRUE) {
  # Extract model name
  modnm <- mod[[1]]$control$modelName

  # Create directory for model files
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  # Save model files to the directory
  writeJJM(mod, path = path)

  # Execute JJMS model
  executedPath <- exejjms(modnm, path, args = args, verbose = verbose)

  return(executedPath)
}


#' Execute JJMS Model
#'
#' Executes the JJMS model with specified arguments, manages file operations for the model run,
#' and optionally cleans up the output directory.
#'
#' @param name The name of the JJMS model.
#' @param path The path to the directory where the JJMS model is located.
#' @param args Additional arguments for the JJMS model execution.
#' @param verbose If TRUE, prints the execution log to the console.
#' @param clean If TRUE, cleans up the output directory after model execution.
#'
#' @return Invisible path where the model was executed.
#' @export
exejjms <- function(name, path, args = "", verbose = TRUE, clean = TRUE) {
  # GET data file name from ctl:1
  datf <- jjmR:::.getDatFile(file.path(path, paste0(name, ".ctl")))
  datn <- basename(datf)

  # CALL jjms
  if (get_os() %in% c("linux", "osx")) {
    exe <- paste("jjms -nox -ind", paste0(name, ".ctl"), "-iprint 100", args)
    if(verbose)
      echoc <- system(paste0("cd ", path, "; ", exe), wait=TRUE)
    else
      echoc <- system(paste0("cd ", path, "; ", exe, " > logfile.txt"),
        wait=TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  } else if(get_os() == "windows") {
    exe <- paste("jjms.exe -nox -ind", paste0(name, ".ctl"), "-iprint 100", 
      args)
    if (verbose) {
      echoc <- shell(paste0("cd /D", shQuote(path), " & ", exe))
    } else {
      echoc <- shell(paste0("cd /D", shQuote(path), " & ", exe, " > logfile.txt"))
    }
  }

  # STOP if failed
  if (echoc == 127)
    stop("jjms could not be run, check 'input.log'")

  # STOP if run failed: CHECK for cor
  if(!grepl("-nohess", args))
    if(!file.exists(file.path(path, "jjms.std")))
      stop("jjms did not converge")

  # TODO CHECK gradient.dat$R0

  # ORGANIZE files in folders

  # - config: mod.ctl
  dir.create(file.path(path, "config"), showWarnings = FALSE)
  file.copy(file.path(path, paste0(name, ".ctl")),
    file.path(path, "config", paste0(name, ".ctl")), overwrite=TRUE)

  # - input: mod.dat
  dir.create(file.path(path, "input"), showWarnings = FALSE)
  file.copy(datf,
    file.path(path, "input", datn), overwrite=TRUE)

  # - results: .yld, .cor, .par, .rep, .std
  dir.create(file.path(path, "results"), showWarnings = FALSE)
  file.copy(file.path(path, "Fprof.yld"),
    file.path(path, "results", paste0(name, ".yld")), overwrite=TRUE)
  file.copy(file.path(path, "jjms.par"),
    file.path(path, "results", paste0(name, ".par")), overwrite=TRUE)
  file.copy(file.path(path, "jjms.rep"),
    file.path(path, "results", paste0(name, ".rep")), overwrite=TRUE)
  file.copy(file.path(path, "jjms.std"),
    file.path(path, "results", paste0(name, ".std")), overwrite=TRUE)

  # For_R_*rep
  file.copy(file.path(path, "For_R_1.rep"),
    file.path(path, "results", paste0(name, "_1_R.rep")), overwrite=TRUE)
  if(file.exists(file.path(path, "For_R_2.rep")))
    file.copy(file.path(path, "For_R_2.rep"),
      file.path(path, "results", paste0(name, "_2_R.rep")), overwrite=TRUE)

  # DELETE files, not folders
  if(clean)
    packjjmsrun(path)

  # RETURN path
  invisible(path)
} 

#' Clean Up JJMS Model Run Directory
#'
#' Removes unnecessary files from a JJMS model run directory, keeping only essential files.
#'
#' @param path The path to the JJMS model run directory.
#'
#' @return Invisible TRUE if all files are successfully removed, FALSE otherwise.
#' @export
packjjmsrun <- function(path) {
  # List all files in the path
  files <- list.files(path, full.names = TRUE)

  # Define files and directories to keep
  keep <- c(file.path(path, c("results", "input", "config")), file.path(path, "mceval.rep"))

  # Remove files not in the 'keep' list
  removalResult <- file.remove(files[!files %in% keep])

  # Return TRUE if all files were successfully removed, FALSE otherwise
  invisible(all(removalResult))
}

