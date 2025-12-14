#' FLjjm: Roxygen documentation for main functions
#'
#' This file contains roxygen2 documentation blocks for the main user-facing
#' functions in the FLjjm package. The blocks are documentation-only (no
#' @export tags) and are intended to be used to generate Rd files with
#' roxygen2. The documented functions are implemented elsewhere in the
#' package; these blocks provide full descriptions, parameter documentation,
#' return values, examples and references.
#'
#' Package author and license information are included at the top of each
#' block where appropriate. License: EUPL 1.2.
#'
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @keywords internal
NULL


#' Create an FLBiol from a JJMS model run directory
#'
#' An object of class \code{FLBiol} is created from the information in the
#' various files in a \code{jjms} folder structure. The function reads the
#' model run (control, input and results folders) and constructs the
#' biological component that can be used within the FLR framework. See
#' \code{buildFLBjjm} for details about how each slot is populated.
#'
#' @param name Character. Name of the control (.ctl) model file (without
#'   extension) present in the config folder of the model run.
#' @param path Character. Path to the root model folder containing the
#'   subfolders config, input and results produced by \code{jjms}.
#' @param stock Numeric. Stock index to extract when the run contains multiple
#'   stocks. Default is 1.
#' @return An object of class \code{FLBiol} constructed from the jjm output
#'   (with slots such as \code{n}, \code{wt}, \code{m}, \code{mat} filled
#'   according to the model input and output files).
#' @details The function delegates the heavy lifting to \code{jjmR::readJJM}
#'   to import the model run and then to \code{buildFLBjjm} which maps the
#'   jjm inputs/outputs to the corresponding FLBiol slots. Typical jjm
#'   outputs used include population numbers by age (N), weight-at-age,
#'   natural mortality (M) and maturity. When a multi-stock jjm run is used,
#'   specify \code{stock} to select the correct output component.
#' @examples
#' \dontrun{
#' biol <- readFLBjjm(name = "h1_1.07",
#'   path = system.file("ext-data", "single_stock", package = "FLjjm"))
#' summary(biol)
#' }
#' @seealso \code{buildFLBjjm}, \code{jjmR::readJJM}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @references EUPL 1.2
#' @name readFLBjjm
NULL


#' Create an FLIndices object from a JJMS model run directory
#'
#' Read jjms output and construct an \code{FLIndices} object containing the
#' abundance indices used/produced by the model. The function maps index
#' selectivity patterns, catchability (q) and range/start-end fractions to
#' FLIndex objects and combines them into an FLIndices list.
#'
#' @param name Character. Name of the control model file.
#' @param path Character. Path to the model run folder (containing config,
#'   input and results subfolders).
#' @return An \code{FLIndices} object with one or more \code{FLIndex}
#'   elements representing survey/observation time series extracted from the
#'   model output.
#' @details The function calls \code{jjmR::readJJM} to read the run and
#'   \code{buildFLIsjjm} to construct the FLIndices object. The mapping
#'   follows the conventions used in the package: selectivity patterns are
#'   read from \code{output@sel_ind_i}, index catchability from
#'   \code{output@q_i} and month ranges are converted to fractions using the
#'   \code{Imonths} input.
#' @examples
#' \dontrun{
#' idx <- readFLIsjjm(name = "h1_1.07",
#'   path = system.file("ext-data", "single_stock", package = "FLjjm"))
#' plot(idx)
#' }
#' @seealso \code{buildFLIsjjm}, \code{jjmR::readJJM}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @name readFLIsjjm
NULL


#' Create FLFisheries objects from a JJMS model run directory
#'
#' Construct the \code{FLFisheries} (or individual \code{FLFishery}) objects
#' representing fisheries in the jjms output. For each fishery the function
#' creates a \code{FLCatch} with slots \code{landings}, \code{landings.n},
#' \code{landings.wt}, discards and related weights, using the model outputs
#' C_fsh_* and wt_fsh_*.
#'
#' @param name Character. Name of the control model file.
#' @param path Character. Path to the jjms run directory.
#' @param stock Numeric. Stock index to extract when more than one stock is
#'   present. Default is 1.
#' @return A list (or an \code{FLFisheries}/\code{FLFishery} object) with
#'   fisheries built for the given stock.
#' @details The function extracts fishery names from \code{out$output[[stock]]$Fshry_names}
#'   and loops through the fisheries building \code{FLQuant} objects for
#'   landings numbers (C_fsh_i), weights at age (wt_fsh_i) and other slots.
#' @examples
#' \dontrun{
#' fis <- readFLFsjjm(name = "h1_1.07",
#'   path = system.file("ext-data", "single_stock", package = "FLjjm"))
#' str(fis)
#' }
#' @seealso \code{buildFLFsjjm}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @name readFLFsjjm
NULL


#' Construct FLIndices from a jjm.output object
#'
#' Transform a single-run jjm.output object into an \code{FLIndices}
#' collection. This lower-level builder is used internally by
#' \code{readFLIsjjm} and other wrapper helpers. It maps the jjm output
#' structure into FLIndex objects including selectivity, index q and time
#' ranges.
#'
#' @param out A jjm.output object (typically the return value of
#'   \code{jjmR::readJJM} for a single run) or a list containing such an
#'   object.
#' @param stock Numeric. Stock index to process; for single-stock runs this
#'   is 1. Default is 1.
#' @return An \code{FLIndices} object built from the provided output.
#' @details The function expects the jjm output to include objects named
#'   \code{info}, \code{data}, \code{output}, \code{control} and
#'   \code{parameters}. It will extract indices, their selectivity patterns
#'   (sel.pattern), catchability (index.q) and transform start and end
#'   months into fractional positions within a year.
#' @examples
#' \dontrun{
#' mod <- jjmR::readJJM("h1_1.07", path = system.file("ext-data","single_stock", package="FLjjm"))
#' idx <- buildFLIsjjm(mod)
#' }
#' @seealso \code{readFLIsjjm}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @name buildFLIsjjm
NULL


#' Build reference points (FLPar) from jjm.output
#'
#' Create an \code{FLPar} holding reference points such as MSY, SB0, SBMSY
#' and FMSY using the output slot \code{msy_mt} in the jjm output. Units are
#' set to match the package conventions (e.g. "1000 t" for biomass-based
#' quantities and "f" for fishing mortality).
#'
#' @param out A jjm.output object (single-run) or a wrapper list typically
#'   returned by \code{jjmR::readJJM}.
#' @param stock Numeric. Stock index to select the correct element in the
#'   output list when multiple stocks are present. Default is 1.
#' @return An \code{FLPar} with named reference points: MSY, SB0, SBMSY and
#'   FMSY.
#' @details The function reads the last row of the \code{msy_mt} table in
#'   the output and maps the columns msy, bzero, bmsy and fmsy to the
#'   corresponding FLPar elements. The units are set explicitly in the
#'   returned object.
#' @examples
#' \dontrun{
#' mod <- jjmR::readJJM("h1_1.07", path = system.file("ext-data","single_stock", package="FLjjm"))
#' rps <- buildFLRPsjjm(mod)
#' rps
#' }
#' @seealso \code{jjmR::readJJM}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @name buildFLRPsjjm
NULL


#' Build FLStock with areas from jjm.output
#'
#' Convert a jjm.output into an \code{FLStock} object taking into account
#' area-specific outputs when available. The function fills standard stock
#' slots (landings, stock.n, stock.wt, m, mat, etc.) using the model outputs
#' and sets appropriate dimnames for ages and years.
#'
#' @param out A jjm.output object (single-run) or the list structure
#'   returned by \code{jjmR::readJJM}.
#' @param stock Numeric. Index of the stock to build (default 1).
#' @param name Character. Optional name to assign to the returned \code{FLStock}.
#' @return An \code{FLStock} object reflecting the model's output for the
#'   requested stock, including age and year dimensions derived from the
#'   model input metadata.
#' @details The function expects the jjm output to contain lists named
#'   \code{info}, \code{data}, \code{output}, \code{control} and
#'   \code{parameters}. Age and year dimensions are derived from
#'   \code{data$age} and \code{data$year} ranges respectively. Fisheries
#'   information, if present, will be used to create catch objects per area.
#' @examples
#' \dontrun{
#' mod <- jjmR::readJJM("h1_1.07", path = system.file("ext-data","single_stock", package="FLjjm"))
#' stk <- buildFLSojjm(mod)
#' summary(stk)
#' }
#' @seealso \code{buildFLSjjm}, \code{buildFLSsjjm}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @name buildFLSojjm
NULL


#' Build FLStock from jjm.output
#'
#' Build a standard \code{FLStock} object (without explicit area breakdown)
#' from jjm.output. The returned object contains landings, stock numbers,
#' weights, natural mortality, maturity, and other standard FLStock slots.
#'
#' @param out A jjm.output object (single-run) or similar list.
#' @param stock Numeric. Stock index to extract (default 1).
#' @param name Character. Optional name for the resulting FLStock. Default "CJM".
#' @return An \code{FLStock} object constructed from the jjm model output.
#' @details The function constructs age and year dimnames from the input
#'   metadata (\code{data$age} and \code{data$year}) and fills relevant
#'   slots using the output matrices such as \code{output$N} for
#'   stock.n and \code{output$wt_a_pop} for stock weights. Units are set
#'   according to the package conventions (e.g. "1e6" for counts where
#'   appropriate, or "1000 t" for biomass-based numbers).
#' @examples
#' \dontrun{
#' mod <- jjmR::readJJM("h1_1.07", path = system.file("ext-data","single_stock", package="FLjjm"))
#' stk <- buildFLSjjm(mod)
#' }
#' @seealso \code{buildFLSojjm}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @name buildFLSjjm
NULL


#' Build multiple FLStock objects from jjm.output
#'
#' Create a named list of \code{FLStock} objects for each stock present in a
#' multi-stock jjm.output. This is a convenience wrapper that calls
#' \code{buildFLSjjm} for each stock and organizes the results into a
#' list, assigning names where available.
#'
#' @param out A jjm.output object from a multi-stock run.
#' @return A named list of \code{FLStock} objects, one per stock present in
#'   the input.
#' @examples
#' \dontrun{
#' mod <- jjmR::readJJM("multi_stock_run", path = "/path/to/run")
#' stks <- buildFLSsjjm(mod)
#' }
#' @seealso \code{buildFLSjjm}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @name buildFLSsjjm
NULL


#' Create an FLombf object from a JJMS model run directory
#'
#' Construct an \code{FLombf} (operating model for biomass/forces framework)
#' from the outputs of a jjms run. The resulting object contains three main
#' slots populated by the corresponding \code{buildFL*jjm} helper functions:
#' \code{biols} (from \code{buildFLBjjm}), \code{fisheries} (from
#' \code{buildFLFsjjm}) and \code{refpts} (from \code{buildFLRPsjjm}).
#'
#' @param name Character. Control model file name.
#' @param path Character. Root path to the jjms model run folders.
#' @return An \code{FLombf} object ready to be used as an operating model
#'   within the FLR/MSE framework.
#' @details The wrapper reads the jjm run with \code{jjmR::readJJM} and then
#'   assembles the subcomponents using the specialized builder functions.
#'   The function is useful to convert an existing jjms assessment run into
#'   an FLR-compatible operating model for simulation or MSE work.
#' @examples
#' \dontrun{
#' om <- readFLomjjm(name = "h1_1.07",
#'   path = system.file("ext-data", "single_stock", package = "FLjjm"))
#' summary(om)
#' }
#' @seealso \code{buildFLBjjm}, \code{buildFLFsjjm}, \code{buildFLRPsjjm}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @name readFLomjjm
NULL


#' Observation Error Model wrapper for CJM within MSE workflows
#'
#' Run the CJM Observation Error Model (OEM) for a given \code{FLStock} and
#' observation configuration. This function prepares inputs, calls the
#' sampling.oem helper and post-processes results to match the expected
#' structure for later MSE steps.
#'
#' @param stk An \code{FLStock} object used as the operating model.
#' @param deviances A list or object with deviance specifications for the
#'   OEM sampling routine.
#' @param observations List containing observation specifications (e.g.
#'   indices and their sampling properties) used by the OEM.
#' @param stability Numeric. Stability parameter for the OEM (default 1).
#' @param wts Logical. Whether to use weights (default TRUE).
#' @param jjms Logical. Whether to run the jjms-specific OEM branch
#'   (default TRUE).
#' @param F3sel (internal) Selection pattern for F3 (passed to the sampler).
#' @param args List. Additional arguments controlling dimension settings such
#'   as dy, frq and other temporal helpers used by the OEM wrapper.
#' @param tracking Logical or list. Optional tracking and diagnostic settings.
#' @return A list with sampled observation time series and diagnostics as
#'   prepared by the \code{sampling.oem} helper and post-processing steps.
#' @details This function is tailored for the CJM/OEM integration inside the
#'   MSE flow used by the package. It sets up the temporal dimension arguments
#'   (using \code{dy} and \code{frq} from \code{args}), calls
#'   \code{sampling.oem} and performs light post-processing such as dropping
#'   data columns not required for downstream routines.
#' @examples
#' \dontrun{
#' res <- cjm.oem(stk, deviances = list(), observations = list(), args = list(dy=2022, frq=1))
#' }
#' @seealso \code{sampling.oem}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @name cjm.oem
NULL


#' Read FLIndices with OEM iterations and MC outputs
#'
#' Read jjm output and construct indices suitable for OEM simulations.
#' When \code{iter > 1} the function will attempt to read Monte Carlo
#' evaluation output to populate index catchability (q) and other stochastic
#' elements. The function returns indices propagated to the requested
#' number of iterations.
#'
#' @param name Character. Control file name.
#' @param path Character. Path with config, input and results folders.
#' @param method Function. The OEM sampling function to be used; default is
#'   \code{cjm.oem}.
#' @param iter Integer. Number of iterations to return/propgate. If NULL,
#'   defaults to 1.
#' @param ... Additional arguments passed to \code{method}.
#' @return A list of \code{FLIndex} objects propagated to \code{iter}
#'   iterations and augmented with MC-derived catchabilities when available.
#' @examples
#' \dontrun{
#' idx <- readFLoemjjm("h1_1.07", path = system.file("ext-data","single_stock", package="FLjjm"), iter = 10)
#' }
#' @seealso \code{readFLIsjjm}, \code{readMCeval}
#' @author Iago Mosqueira <iago.mosqueira@wur.nl>
#' @name readFLoemjjm
NULL


# EOF