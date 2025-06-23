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

# DEFINE Blim as 10% B0
statistics$PSBlim[[1]]  <- ~yearMeans((SB/(SB0 * 0.10)) > 1)

# P(C < target)
statistics$PCtarget <- list(~yearMeans(C < target), name="P(C<target)",
  desc="Probability of catch falling below HCR target level")

# P(TAC limited)
statistics$PTAClimit <- list(~yearMeans(FLQuant(tac.hcr < hcr)), name="P(TAClimit)",
  desc="Probability of TAC change being limited")

save(statistics, file="../data/statistics.RData", compress="xz")
