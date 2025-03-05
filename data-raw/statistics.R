# statistics.R - DESC
# /statistics.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# SB0, SBMSY, FMSY, PSBMSY

library(mse)

data(statistics, package="mse")

statistics <- statistics[c("SBMSY", "FMSY", "PSBMSY", "PSBlim", "C", "F", "SB",
  "IACC", "PC0", "green", "red")]

# DEFINE Blim
statistics$PSBlim[[1]]  <- ~yearMeans((SB/(SBMSY * 0.10)) > 1)

# P(C < target)
statistics$PCtarget <- list(~yearMeans(C < target), name="P(C<target)",
  desc="Probability of catch falling below HCR target level")

save(statistics, file="../data/statistics.RData", compress="xz")
