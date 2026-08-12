source(here::here("R", "pipelines.R"))

test_that("pipeline manifests reference existing notebooks", {
  pipelines <- analysis_pipeline_steps()

  expect_setequal(names(pipelines), c("hsa", "lymphoma", "mct"))

  notebook_paths <- unlist(
    lapply(
      pipelines,
      function(steps) vapply(
        steps,
        function(step) do.call(here::here, as.list(step$path)),
        character(1)
      )
    ),
    use.names = FALSE
  )

  expect_true(all(file.exists(notebook_paths)))
})

test_that("the HSA pipeline stops before inactive Cox modelling", {
  hsa <- get_analysis_pipeline("hsa")

  expect_identical(
    hsa[[1]]$path,
    c("Code", "Creating HSA study population.qmd")
  )
  expect_identical(hsa[[2]]$params, c(run_cox_analysis = "false"))
})

test_that("unknown pipelines fail", {
  expect_error(get_analysis_pipeline("melanoma"), "Unknown pipeline")
})
