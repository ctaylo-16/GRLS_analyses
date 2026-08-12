#!/usr/bin/env Rscript

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Run renv::restore().", call. = FALSE)
}

source(here::here("R", "pipelines.R"))

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) {
  stop(
    "Usage: Rscript scripts/render_pipeline.R <hsa|lymphoma|mct>",
    call. = FALSE
  )
}

render_analysis_pipeline(tolower(arguments[[1]]))
