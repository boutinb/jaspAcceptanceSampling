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

#' Accept/Reject Lots (k-method)
#'
DecideVariableLots <- function(
          data = NULL,
          version = "1",
          acceptableQualityLevel = 0.05,
          historicalSd = 1,
          historicalSdKnown = FALSE,
          kValue = 1.309,
          lowerSpecificationLimit = FALSE,
          lowerSpecificationLimitValue = 0,
          measurement = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          rejectableQualityLevel = 0.15,
          sampleMean = 1.5,
          sampleSd = 1,
          sampleSize = 24,
          sampleStatistics = TRUE,
          upperSpecificationLimit = FALSE,
          upperSpecificationLimitValue = 1) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("measurement")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspAcceptanceSampling", "DecideVariableLots", "DecideVariableLots.qml", options, version, FALSE))
}