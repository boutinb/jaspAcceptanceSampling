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

#' Analyze Attribute Plan
#'
AnalyzeAttributePlan <- function(
          data = NULL,
          version = "1",
          multiplePlanAssessment = FALSE,
          multiplePlanAssessmentAcceptableQualityLevel = 0.05,
          multiplePlanAssessmentConsumerRisk = 0.1,
          multiplePlanAssessmentProducerRisk = 0.05,
          multiplePlanAssessmentRejectableQualityLevel = 0.15,
          multiplePlanAverageOutgoingQualityCurve = FALSE,
          multiplePlanAverageSampleNumberCurve = FALSE,
          multiplePlanAverageTotalInspectionCurve = FALSE,
          multiplePlanDistribution = "binomial",
          multiplePlanLotSize = 1000,
          multiplePlanOperatingCharacteristicCurve = FALSE,
          multiplePlanProportionNonconformingFrom = 0,
          multiplePlanProportionNonconformingStep = 0.01,
          multiplePlanProportionNonconformingTo = 0.15,
          multiplePlanStages = list(list(value = "1"), list(value = "2")),
          multiplePlanStagesCount = 2,
          multiplePlanSummaryTable = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          singlePlanAcceptanceNumber = 4,
          singlePlanAssessment = FALSE,
          singlePlanAssessmentAcceptableQualityLevel = 0.05,
          singlePlanAssessmentConsumerRisk = 0.1,
          singlePlanAssessmentProducerRisk = 0.05,
          singlePlanAssessmentRejectableQualityLevel = 0.15,
          singlePlanAverageOutgoingQualityCurve = FALSE,
          singlePlanAverageSampleNumberCurve = FALSE,
          singlePlanAverageTotalInspectionCurve = FALSE,
          singlePlanDistribution = "binomial",
          singlePlanLotSize = 1000,
          singlePlanOperatingCharacteristicCurve = FALSE,
          singlePlanProportionNonconformingFrom = 0,
          singlePlanProportionNonconformingStep = 0.01,
          singlePlanProportionNonconformingTo = 0.15,
          singlePlanRejectionNumber = 5,
          singlePlanSampleSize = 100,
          singlePlanSummaryTable = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("multiplePlanStages")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspAcceptanceSampling", "AnalyzeAttributePlan", "AnalyzeAttributePlan.qml", options, version, FALSE))
}