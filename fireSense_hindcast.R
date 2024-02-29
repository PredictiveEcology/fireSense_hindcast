defineModule(sim, list(
  name = "fireSense_hindcast",
  description = paste("allow running fireSense for historic climate years by replacing",
                      "the projected climate raster data with sampled historic data."),
  keywords = "",
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(fireSense_hindcast = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "fireSense_hindcast.Rmd"),
  loadOrder = list(after = c("canClimateData"),
                   before = c("fireSense_dataPrepFit")),
  reqdPkgs = list(
    "PredictiveEcology/SpaDES.core@sequentialCaching (>= 2.0.3.9007)", ## TODO: use development
    "terra"
  ),
  parameters = bindrows(
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    expectsInput("historicalClimateRasters", "list", sourceURL = NA,
                 paste("named list of `SpatRasters` of historical climate variables.",
                       "list named after the variable and raster layers named as `year<numeric year>`")),
    expectsInput("projectedClimateRasters", "list", sourceURL = NA,
                 paste("named list of `SpatRasters` of projected climate variables.",
                       "list named after the variable and raster layers named as `year<numeric year>`"))
  ),
  outputObjects = bindrows(
    createsOutput("historicalClimateRasters", "list",
                  paste("named list of `SpatRasters` of historical climate variables.",
                        "list named after the variable and raster layers named as `year<numeric year>`")),
    createsOutput("projectedClimateRasters", "list",
                  paste("named list of `SpatRasters` of **resampled** historic climate variables.",
                        "list named after the variable and raster layers named as `year<numeric year>`"))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.fireSense_hindcast = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ## schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "fireSense_hindcast", "SampleHistoric")
    },
    SampleHistoric = {
      sim <- HistoricAsProjected(sim)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

HistoricAsProjected <- function(sim) {
  browser()
  climateVariables <- names(sim$projectedClimateRasters)
  sampledHistoric <- lapply(climateVariables, function(climVar) {
    rndsmp <- sample(x = terra::nlyr(sim$historicalClimateRasters[[climVar]]),
                     size = terra::nlyr(sim$projectedClimateRasters[[climVar]]),
                     replace = TRUE)
    sim$historicalClimateRasters[[climVar]][rndsmp]
  })
  names(sampledHistoric) <- climateVariables

  sim$projectedClimateRasters <- sampledHistoric

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  if (!suppliedElsewhere("historicalClimateRasters", sim) ||
      !suppliedElsewhere("projectedClimateRasters", sim)) {
    stop("climate rasters must be supplied, e.g., via canClimateData")
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
