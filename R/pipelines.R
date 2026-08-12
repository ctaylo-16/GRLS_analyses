analysis_pipeline_steps <- function() {
  list(
    hsa = list(
      list(path = c("Code", "Creating HSA study population.qmd")),
      list(
        path = c("Code", "GRLS cox HSA time to diagnosis.qmd"),
        params = c(run_cox_analysis = "false")
      ),
      list(path = c("Code", "GRLS HSA updated analysis_all_cases.qmd")),
      list(path = c("Code", "GRLS HSA updated analysis confirmed cases.qmd")),
      list(path = c("Code", "GRLS HSA cohort descriptives.qmd"))
    ),
    lymphoma = list(
      list(path = c("Code", "lymphoma GRLS", "lymphoma dataset variable creation.qmd"))
    ),
    mct = list(
      list(path = c("Code", "MCT GRLS", "MCT denom creation.qmd")),
      list(path = c("Code", "MCT GRLS", "MCT dataset variable creation.qmd"))
    )
  )
}

get_analysis_pipeline <- function(name) {
  pipelines <- analysis_pipeline_steps()

  if (length(name) != 1L || is.na(name) || !name %in% names(pipelines)) {
    stop(
      "Unknown pipeline. Choose one of: ",
      paste(names(pipelines), collapse = ", "),
      call. = FALSE
    )
  }

  pipelines[[name]]
}

render_analysis_pipeline <- function(name, quarto = Sys.which("quarto")) {
  if (!nzchar(quarto)) {
    stop("Quarto is not installed or is not available on PATH.", call. = FALSE)
  }

  steps <- get_analysis_pipeline(name)

  for (step in steps) {
    input <- do.call(here::here, as.list(step$path))
    if (!file.exists(input)) {
      stop("Pipeline notebook does not exist: ", input, call. = FALSE)
    }

    arguments <- c("render", shQuote(input))
    if (!is.null(step$params)) {
      parameter_arguments <- unlist(
        lapply(
          names(step$params),
          function(parameter) c("-P", paste0(parameter, ":", step$params[[parameter]]))
        ),
        use.names = FALSE
      )
      arguments <- c(arguments, parameter_arguments)
    }

    message("Rendering ", input)
    status <- system2(quarto, args = arguments)
    if (!identical(status, 0L)) {
      stop("Quarto render failed for: ", input, call. = FALSE)
    }
  }

  invisible(TRUE)
}
