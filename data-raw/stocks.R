# stocks.R - DESC
# /stocks.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(FLjjm)

# SINGLE stock

stk <- readFLSjjm("mod1.00", "single_stock_run", output=TRUE)

# TWO stocks

stks <- readFLSsjjm("mod1.00_2stk", "two_stock_run", output=TRUE)

# INDICES

idx <- readFLIsjjm("mod1.00", "single_stock_run")

idxs <- readFLIsjjm("mod1.00_2stk", "two_stock_run")

# dat and ctl

mod <- readMod("mod1.00", "single_stock_run")
mod <- mod[[1]][c("data", "control")]

mods <- readMod("mod1.00_2stk", "two_stock_run")
mods <- mods[[1]][c("data", "control")]

# ADD iters

m(stk) <- rnorm(5, m(stk), 0.1)

m(stks[[1]]) <- rnorm(5, m(stks[[1]]), 0.1)
m(stks[[2]]) <- rnorm(5, m(stks[[2]]), 0.1)

# SAVE

save(stk, idx, mod, file="../data/cjmstk.RData", compress="xz")

save(stks, idxs, mods, file="../data/cjmstks.RData", compress="xz")
