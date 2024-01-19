# zzz.R - DESC
# FLjjm/R/zzz.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# .onAttach: set path to executable

.onAttach <- function(libname, pkgname) {

  # GET OS and sep
  os <- get_os()
  sep <- get_sep(os)

  # SET path
  path <- system.file("bin", os, package="FLjjm")
  Sys.setenv(PATH=paste(path, Sys.getenv("PATH"), sep=sep))
}

# get_os

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# get_sep

get_sep <- function(os) {
  switch(os,
    linux = ":",
    osx = ":",
    windows = ";",
    ",")
}

# METRICS to collapse multiple catches

jmets <- list(REC=rec, SSB=ssb, CATCH=function(x) areaSums(catch(x)),
  F=fbar)

