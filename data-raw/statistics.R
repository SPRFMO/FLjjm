# statistics.R - DESC
# /statistics.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# SB0, SBMSY, FMSY, PSBMSY

library(mse)

data(statistics, package="mse")

statistics <- statistics[c("SBMSY", "PSBMSY", "PSBlim", "SB", "SB0", "R",
  "F", "FMSY", "green", "red", "orange", "yellow", "C", "IACC", "PC0")]

# DEFINE Blim as 10% B0
statistics$PSBlim[[1]]  <- ~yearMeans((SB/(SB0 * 0.10)) > 1)

# P(C < target), needs tracking['decision.hcr',]
statistics$PCtarget <- list(~yearMeans(decision.hcr > 1), name="P(C<target)",
  desc="Probability of catch falling below HCR target level")

# P(TAC limited by dupp), needs tracking['rule.hcr',]
statistics$PTAClimit <- list(~yearMeans(FLQuant(rule.hcr < hcr)), name="P(TAClimit)",
  desc="Probability of TAC being limited")

# F/F_recent
statistics$Frecent <- list(~yearMeans(F), name="Frecent",
  desc="Mean fishing mortality over recent values")

save(statistics, file="../data/statistics.RData", compress="xz")
