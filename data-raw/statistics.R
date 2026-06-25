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
statistics$F38 <- list(~F38, name="Fbar38", desc="Mean fishing mortality ages 3-8")

# FN, Fbar weighted by N
statistics$FN <- list(~FN, name="FbarbyN",
 desc="Mean fishing mortality weighted by abundance")

# VB relative to iy
statistics$VB2025 <- list(~VB2025, name="VB/VB[2025]",
  desc="Vulnerable biomass relative to that in 2025")

statistics$UMSY <- list(~VB/MSY, name="UMSY", desc="Vulnerable biomass relative to MSY level")

# SB0MSY
statistics$SB0MSY <- list(~SB / ((SBMSY / SB0) * dSB0), name="SB0MSY",
  desc="Spawning biomass relative to MSY computed over dynamic unfished SB0")

statistics$SB0green <- list(~iterMeans(FLQuant((SB/((SBMSY / SB0) * dSB0)) >= 1 &
  (F/FMSY) <= 1)), name="P(SB0Green)",
  desc="Probability of being in SB0MSY-based Kobe green quadrant")

# SAVE
save(statistics, file ="../data/statistics.RData", compress="xz")
