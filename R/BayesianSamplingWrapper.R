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

#' Bayesian Sampling
#'
#' Bayesian Sampling supports Bayesian planning and inference for acceptance sampling with a Beta prior on the lot proportion defective. The procedure text describes the hypotheses and workflow. Planning finds candidate sampling plans; inference updates the selected prior using the observed sample and predicts defects in the lot.
#'
#' @param inferenceDecisionTable, Show posterior Bayes-factor evidence and target status. Evidence labels describe support for a proportion defective below versus at or above the RQL; whether the selected target is reached is reported separately. Both use the unrounded Bayes factor. The acceptance number c is the largest observed defect count at the specified inference sample size that meets the target. "Not available" means that no count meets it at that sample size.
#'    Defaults to \code{FALSE}.
#' @param inferenceLotSize, Total number of items in the lot, including inspected and uninspected items.
#' @param inferenceObservedDefectives, Number of defective items observed in the inspected sample. This must not exceed the sample size.
#' @param inferencePosterior, Update the selected Beta prior using the sample size n and observed defectives d. Counts must satisfy 0 <= d <= n <= N. The input fields retain inconsistent counts so that the analysis can explain the problem. If d exceeds n or n exceeds N, correct whichever entry was mistaken; dependent inference results return after the inputs are valid.
#'    Defaults to \code{FALSE}.
#' @param inferencePosteriorPlot, Plot the updated Beta posterior and its Bayes-factor evidence and target status. Adaptive grids resolve narrow peaks and both tails. If the density diverges at 0 or 1, its curve continues beyond the finite vertical display range; this affects only the display, not probabilities or Bayes factors.
#'    Defaults to \code{FALSE}.
#' @param inferencePosteriorPredictivePlot, Plot the posterior predictive distribution of lot-level defectives, conditional on the observed sample data.
#'    Defaults to \code{FALSE}.
#' @param inferenceSampleSize, Number of inspected items in the sample. This must not exceed the lot size.
#' @param planningAcceptableQualityLevel, Acceptable quality level (AQL), expressed as a proportion defective between 0 and 1.
#' 
#' For the default unimodal two-hypothesis impartial prior, AQL sets the mode (peak) unless a custom mode is specified. The prior assigns 50% probability below RQL and 50% above it. For a U-shaped impartial prior, AQL instead sets the interior minimum unless a custom minimum is specified. In the three-hypothesis workflow, AQL separates the Good and Middle regions.
#' @param planningMaximumSampleSize, Maximum sample size searched when finding candidate sampling plans.
#' @param planningMinimumBayesFactor, Minimum target Bayes factor in favor of a proportion defective below the RQL. This target is used for planning and for the acceptance number and target status reported under Inference. Equality meets the target; not reaching it does not by itself imply rejection. A target at or below 1 can be reached without evidence favoring a proportion defective below the RQL.
#' @param planningPlanTable, Show candidate sampling plans meeting the selected minimum Bayes-factor target within the maximum sample size.
#'    Defaults to \code{FALSE}.
#' @param planningPriorAlpha, First shape parameter (alpha) of the custom Beta prior.
#' @param planningPriorBeta, Second shape parameter (beta) of the custom Beta prior.
#' @param planningPriorDistribution, Select a Beta prior for the lot proportion defective. The selected prior is used for both planning and inference.
#' \itemize{
#'   \item \code{"impartial"}: Assigns 50% prior probability below RQL and 50% above it. Choose a unimodal prior with an interior peak or a U-shaped prior with an interior minimum between the endpoint peaks. The location defaults to AQL. Incompatible combinations produce an error; the analysis does not automatically change the shape, location or probability split.
#'   \item \code{"impartialThreeRegion"}: Assigns one third of the prior probability to each of Good (below AQL), Middle (between AQL and RQL), and Bad (above RQL). The limits must satisfy 0 < AQL < RQL < 1.
#'   \item \code{"uniform"}: Uses a Beta(1, 1) prior.
#'   \item \code{"custom"}: Specify the two positive shape parameters of the Beta prior.
#' }
#' @param planningPriorImpartialCustomLocation, When unchecked, AQL is used as the location. Changing shape retains the location; incompatible combinations show an error.
#'    Defaults to \code{FALSE}.
#' @param planningPriorImpartialLocation, Location of the interior peak for Unimodal or the interior minimum for U-shaped, between 0 and 1. Switching shape retains this value.
#' @param planningPriorImpartialShape, Both shapes assign half of the prior probability below RQL. For Unimodal (the default), the location is the peak and RQL must lie strictly between that mode and 0.5. For U-shaped, the location is the interior minimum and the minimum and RQL must lie on opposite sides of 0.5. Both the location and RQL must be strictly between 0 and 1.
#' 
#' A location and RQL both equal to 0.5 leave infinitely many priors of either shape possible. Choose Custom to specify the desired parameters.
#' @param planningPriorPlot, Plot the selected Beta prior. The plot remains available even if the sample-size limit and Bayes-factor target yield no feasible plan. Adaptive grids resolve narrow peaks and both tails. If the density diverges at 0 or 1, its curve continues beyond the finite vertical display range; this affects only the display, not probabilities or Bayes factors.
#'    Defaults to \code{FALSE}.
#' @param planningPriorThreeRegionShapeConstraint, Any shape (the default) applies no additional shape restriction. Alternatively, exclude U-shapes (alpha >= 1 or beta >= 1), or require both parameters to be at least 1. The equal-thirds prior must satisfy the selected constraint. Incompatible quality limits and shape constraints produce an error. Change the limits or constraint; the probability split is not changed.
#' @param planningRejectableQualityLevel, Proportion-defective threshold for unacceptable quality and the main threshold used in two-hypothesis planning and inference.
#' 
#' Also called the lot tolerance percent defective (LTPD). Enter a proportion between 0 and 1 (for example, 0.10 means 10%).
#' @param planningThreeHypothesisBayesFactorTable, Show Bayes-factor comparisons for the Good, Middle and Bad regions defined by AQL and RQL.
#'    Defaults to \code{FALSE}.
BayesianSampling <- function(
          data = NULL,
          version = "1",
          inferenceDecisionTable = FALSE,
          inferenceLotSize = 100,
          inferenceObservedDefectives = 0,
          inferencePosterior = FALSE,
          inferencePosteriorPlot = FALSE,
          inferencePosteriorPredictivePlot = FALSE,
          inferenceSampleSize = 30,
          planningAcceptableQualityLevel = 0.05,
          planningMaximumSampleSize = 40,
          planningMinimumBayesFactor = 30,
          planningPlanTable = FALSE,
          planningPriorAlpha = 1,
          planningPriorBeta = 1,
          planningPriorDistribution = "impartial",
          planningPriorImpartialCustomLocation = FALSE,
          planningPriorImpartialLocation = 0.05,
          planningPriorImpartialShape = "unimodal",
          planningPriorPlot = FALSE,
          planningPriorThreeRegionShapeConstraint = "none",
          planningRejectableQualityLevel = 0.15,
          planningThreeHypothesisBayesFactorTable = FALSE,
          plotHeight = 320,
          plotWidth = 480) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("planningPriorImpartialShape", "planningPriorThreeRegionShapeConstraint")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspAcceptanceSampling", "BayesianSampling", "BayesianSampling.qml", options, version, TRUE))
}