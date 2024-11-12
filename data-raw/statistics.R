# statistics.R - DESC
# /statistics.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# SB0, SBMSY, FMSY, PSBMSY

data(statistics, package="mse")

statistics <- statistics[c("SBMSY", "FMSY", "PSBMSY", "PSBlim", "C", "F", "SB",
  "IACC", "PC0", "green")]

statistics$PSBlim[[1]]  <- ~yearMeans((SB/(SBMSY * 0.10)) > 1)

save(statistics, file="../data/statistics.RData", compress="xz")
