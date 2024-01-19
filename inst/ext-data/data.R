# data.R - DESC
# /data.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(FLjjm)


# EXECUTE models

exejjms('mod1.00', 'single_stock_run')
exejjms('mod1.00_2stk', 'two_stock_run')

# CREATE data objects

om <- loadJJMS(name="mod1.0", path="single_stock")

biol <- om$biol

fiss <- om$fisheries

inds <- om$indices

save(biol, fiss, inds, file="../../data/cjm.RData", compress="xz")
