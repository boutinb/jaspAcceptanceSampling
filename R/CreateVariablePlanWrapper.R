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

#' Create Variable Plan
#'
CreateVariablePlan <- function(
          data = NULL,
          version = "1",
          acceptableQualityLevel = 0.05,
          averageOutgoingQualityCurve = FALSE,
          averageSampleNumberCurve = FALSE,
          averageTotalInspectionCurve = FALSE,
          consumerRisk = 0.1,
          historicalSdKnown = TRUE,
          lotSize = 1000,
          operatingCharacteristicCurve = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          producerRisk = 0.05,
          proportionNonconformingFrom = 0,
          proportionNonconformingStep = 0.01,
          proportionNonconformingTo = 0.15,
          rejectableQualityLevel = 0.15,
          summaryTable = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }


   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspAcceptanceSampling", "CreateVariablePlan", "CreateVariablePlan.qml", options, version, FALSE))
}