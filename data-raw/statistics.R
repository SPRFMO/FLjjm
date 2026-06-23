# statistics.R - DESC
# FLjjm/data-raw/statistics.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(mse)

data(statistics, package="mse")

statistics <- statistics[c("SB", "SBMSY", "PSBMSY", "PSBlim", "SB0", "R", 
  "F", "FMSY", "HR", "PFMSY", "green", "red", "orange", "yellow", "C",
  "CPUE", "IACC")]

# DEFINE Blim as 10% B0
statistics$PSBlim[[1]]  <- ~yearMeans((SB/(SB0 * 0.10)) > 1)

# P(C < target), needs tracking['decision.hcr',]
statistics$PCtarget <- list(~yearMeans(rule.hcr > 1), name="P(C<target)",
  desc="Probability of catch falling below HCR target level")

# P(TAC limited by dupp), needs tracking['decision.hcr',]
statistics$PTACupp <- list(~yearMeans(FLQuant(decision.hcr < hcr)), name="P(TACupp)",
  desc="Probability of TAC being limited from increasing")

statistics$PTAClow <- list(~yearMeans(FLQuant(decision.hcr > hcr)), name="P(TAClow)",
  desc="Probability of TAC being limited from decreasing")

# F38
# statistics$F38 <- list(~F38, name="F38", desc="Mean fishing mortality ages 3-8")

# FN, Fbar weighted by N
# statistics$FN <- list(~FN, name="FN",
#  desc="Mean fishing mortality weighted by abundance")

# VB relative to iy
#statistics$relVB <- list(~VB2025, name="VB/VB[2025]",
#  desc="Vulnerable biomass relative to that in 2025")

# TODO: ADD MSY, needs VB in metrics(om)
# statistics$UMSY <- list(~VB/MSY, name="UMSY", desc="Vulnerable biomass relative to MSY level")

save(statistics, file="../data/statistics.RData", compress="xz")


#


