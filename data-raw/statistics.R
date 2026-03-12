# statistics.R - DESC
# FLjjm/data-raw/statistics.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(mse)

data(statistics, package="mse")

statistics <- statistics[c("SB", "SBMSY", "PSBMSY", "PSBlim", "SB0", "R",
  "F", "FMSY", "PFMSY", "green", "red", "orange", "yellow", "C", "CPUE", "IACC", "PC0")]

# DEFINE Blim as 10% B0
statistics$PSBlim[[1]]  <- ~yearMeans((SB/(SB0 * 0.10)) > 1)

# P(C < target), needs tracking['decision.hcr',]
statistics$PCtarget <- list(~yearMeans(decision.hcr > 1), name="P(C<target)",
  desc="Probability of catch falling below HCR target level")

# P(TAC limited by dupp), needs tracking['rule.hcr',]
statistics$PTAClimit <- list(~yearMeans(FLQuant(rule.hcr < hcr)), name="P(TAClimit)",
  desc="Probability of TAC being limited")

save(statistics, file="../data/statistics.RData", compress="xz")
