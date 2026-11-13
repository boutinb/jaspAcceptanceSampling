#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Bayesian State-Space Sampling
#'
#' *Bayesian State-Space Sampling* performs acceptance sampling with a Bayesian state-space model for defect rates over lots.
#'
#' @param advancedMcmcAdaptDelta, Target acceptance probability for HMC adaptation (default 0.97). A higher value generally produces smaller integration steps and may help resolve divergences, at the cost of longer computation.
#' @param advancedMcmcBurnin, Warmup iterations per chain.
#' @param advancedMcmcChains, Number of parallel chains.
#' @param advancedMcmcMaxTreeDepth, Limit on the depth of HMC's trajectory construction (default 15). Reaching this limit can reduce sampling efficiency; increasing it permits longer trajectories and can increase computation time.
#' @param advancedMcmcSamples, Post-warmup iterations per chain, before thinning.
#' @param advancedMcmcSeed, Random seed.
#' @param advancedMcmcThinning, Thinning interval.
#' 
#' With Thinning greater than 1, divergence and treedepth counts cover only retained transitions; events in omitted iterations cannot be counted. E-BFMI is unavailable because saved energies are not consecutive. Use Thinning = 1 for complete sampler checks.
#' @param covariates, Assign one or more numeric covariates to include their additive effects in the model. Leave this field empty to fit the model without predictors. Adding or removing a covariate refits the model. The common coefficient prior scale can be set under Model before assigning covariates; coefficient plots and diagnostic controls are shown when covariates are assigned. Categorical predictors and interaction terms are not supported.
#' @param defectCount, Observed defects in each sampled lot.
#' @param lotNumber, Unique numeric sequence/order index for each lot, with measurement type **Scale** or **Ordinal**. Ordinal columns must have numeric labels; their numeric values determine the order, regardless of category-level order. Ordered text labels are not supported.
#' @param lotSize, Total items per lot.
#' @param mcmcDiagnosticOverviewBeta, Include the predictor coefficients in the overview table.
#'    Defaults to \code{FALSE}.
#' @param mcmcDiagnosticOverviewPrediction, Include the prediction for the total defects in the current lot in the overview table.
#'    Defaults to \code{FALSE}.
#' @param mcmcDiagnosticOverviewSigma, Include the baseline innovation scale σ<sub>η</sub> in the overview table.
#'    Defaults to \code{FALSE}.
#' @param mcmcDiagnosticOverviewTable, Show the mean, SD, ESS, and R-hat for the selected parameters.
#' 
#' The **Overview table** summarizes the parameters selected below it. **Sigma (state noise)** refers to the baseline innovation scale σ<sub>η</sub>. This table's ESS (`n_eff`) and R-hat columns use RStan's traditional raw-draw ESS and unranked split R-hat, which differ from the rank-based diagnostics in the full computational report. Undefined diagnostics for constant generated quantities should not be read as successful convergence checks.
#'    Defaults to \code{FALSE}.
#' @param mcmcDiagnosticOverviewTheta, Include the defect probability for every lot in the overview table.
#'    Defaults to \code{FALSE}.
#' @param mcmcDiagnosticReport, Show sampler and convergence checks for all sampled parameters.
#' 
#' Under **MCMC Diagnostics**, **Diagnostic report** displays the full computational diagnostic report and is off by default. The checks always run, and warnings about problems or unavailable checks are shown regardless of this option. **Overview table** separately displays posterior summaries and traditional MCMC diagnostics for the selected parameters.
#'    Defaults to \code{FALSE}.
#' @param modelInitialBaselineAlpha, First shape parameter of the beta prior for the initial baseline probability. The default shape parameters are 1 and 1, giving a uniform prior.
#' @param modelInitialBaselineBeta, Second shape parameter of the beta prior for the initial baseline probability. The default shape parameters are 1 and 1, giving a uniform prior.
#' @param modelInnovationPriorScale, The **Half-normal prior scale** is the SD of the underlying zero-centered normal distribution before restricting it to positive values; it is not the SD of the resulting half-normal distribution. This control sets the width of the prior for σ<sub>η</sub>, which is estimated from the data. The half-normal prior favors smaller baseline scales while allowing larger values.
#' @param modelPredictorCauchyScale, Common scale of the independent zero-centered Cauchy priors for the predictor coefficients. You can set this before assigning covariates. Each coefficient is the change in log odds per one standard deviation increase in that covariate, holding the other covariates and baseline fixed.
#' 
#' The **Cauchy scale** control applies separately to every coefficient; the coefficients are not constrained to be equal. Each prior is expressed per one SD of its covariate. In original covariate units, the coefficient is β<sub>k</sub> / SD(X<sub>k</sub>), and its prior scale is the chosen Cauchy scale divided by SD(X<sub>k</sub>). Because covariates are standardized over the supplied dataset at each fit, appending lots can change these original-unit priors.
#' @param plotLatestDefectRate, Posterior distribution of latest defect rate.
#'    Defaults to \code{FALSE}.
#' @param plotPosteriorPredictive, Posterior predictive distribution of total defects in the current lot: observed defects plus predicted defects among its uninspected items. It cannot contain fewer defects than already observed; after inspecting the entire lot, its total defect count is known.
#'    Defaults to \code{TRUE}.
#' @param plotPredictorCoefficient, Posterior distributions of predictor coefficients, labeled by covariate.
#'    Defaults to \code{FALSE}.
#' @param plotStateTimeSeries, Defect-rate state trajectory over lots: posterior means with pointwise 95% credible intervals.
#'    Defaults to \code{TRUE}.
#' @param sampleSize, Number of tested items per lot.
#' @param settingsAcceptableQualityLevel, Acceptable quality level (AQL), expressed as a proportion defective between 0 and 1.
#' @param settingsAcceptanceThreshold, Acceptance threshold a (default 0.10). Consider to Accept when H &lt; a, where H is the posterior predictive probability that the total defects in the current lot exceed RQL times its size.
#' @param settingsRejectableQualityLevel, Rejectable quality level (RQL), also called the lot tolerance percent defective (LTPD). Enter a proportion between 0 and 1 (for example, 0.10 means 10%).
#' @param settingsRejectionThreshold, Rejection threshold r (default 0.05). Consider to Reject when H &gt; a and L &lt; r, where H is the posterior predictive probability that total defects exceed RQL times the lot size and L is the probability that they are below AQL times the lot size. Equality to the acceptance threshold gives Continue.
BayesianSSMforAS <- function(
          data = NULL,
          version = "1",
          advancedMcmcAdaptDelta = 0.97,
          advancedMcmcBurnin = 2000,
          advancedMcmcChains = 3,
          advancedMcmcMaxTreeDepth = 15,
          advancedMcmcSamples = 5000,
          advancedMcmcSeed = 1,
          advancedMcmcThinning = 1,
          covariates = list(types = list(), value = list()),
          defectCount = list(types = list(), value = ""),
          lotNumber = list(types = list(), value = ""),
          lotSize = list(types = list(), value = ""),
          mcmcDiagnosticOverviewBeta = FALSE,
          mcmcDiagnosticOverviewPrediction = FALSE,
          mcmcDiagnosticOverviewSigma = FALSE,
          mcmcDiagnosticOverviewTable = FALSE,
          mcmcDiagnosticOverviewTheta = FALSE,
          mcmcDiagnosticReport = FALSE,
          modelInitialBaselineAlpha = 1,
          modelInitialBaselineBeta = 1,
          modelInnovationPriorScale = 1,
          modelPredictorCauchyScale = 0.707,
          plotHeight = 320,
          plotLatestDefectRate = FALSE,
          plotPosteriorPredictive = TRUE,
          plotPredictorCoefficient = FALSE,
          plotStateTimeSeries = TRUE,
          plotWidth = 480,
          sampleSize = list(types = list(), value = ""),
          settingsAcceptableQualityLevel = 0.05,
          settingsAcceptanceThreshold = 0.1,
          settingsRejectableQualityLevel = 0.1,
          settingsRejectionThreshold = 0.05) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("covariates", "defectCount", "lotNumber", "lotSize", "sampleSize")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspAcceptanceSampling", "BayesianSSMforAS", "BayesianSSMforAS.qml", options, version, TRUE))
}