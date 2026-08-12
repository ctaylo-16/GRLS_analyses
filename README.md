# Golden Retriever Lifetime Study analyses

This R and Quarto project derives datasets and analyses for cancers in the Golden Retriever Lifetime Study (GRLS), principally haemangiosarcoma, lymphoma, and mast cell tumours.

The source data consist of annual owner and veterinarian questionnaire extracts covering demographics, lifestyle, environment, health, medication, reproductive history, and clinical outcomes. Source data are not stored in Git.

## Start here

- Read `HANDOVER.md` for setup, local-data requirements, and the fail-fast operating policy.
- Read `docs/analysis-register.md` before choosing a notebook to run.
- Read `docs/pipelines.md` for the current execution order, handoff datasets, and censoring dates.
- Open `GRLS_analyses.Rproj` and run `Rscript scripts/check_setup.R` from the project root.
- Run a documented cancer workflow with `Rscript scripts/render_pipeline.R <hsa|lymphoma|mct>`.

The current lymphoma and MCT routes, plus the active preparation portion of the
HSA route, use project-relative paths. Some historical notebooks and the
inactive Cox-modelling section retain links to the author's former external
repositories or manually curated files; their status is recorded in the
analysis register and handover notes.

## Repository layout

```text
Code/       Quarto analyses and legacy shared R helpers
R/          reusable project code and setup checks
Data/       local source data; ignored by Git
Output/     generated datasets, tables, and figures; ignored by Git
docs/       analysis status and project documentation
scripts/    command-line project checks and execution entry points
```

This remains a single multi-cancer repository: HSA, lymphoma, and MCT have distinct analysis routes but reuse substantial feature engineering. The intended structure is cancer-specific entry points backed by shared implementations under `R/`.

## Reproducibility status

The main lymphoma and MCT feature domains now use shared, tested helpers under
`R/`, including endpoint, deprivation, environment, smoke/sleep, lifestyle,
household, medication summaries, reproduction, weight, activity, and final
dataset handling. The author's R 4.4.1 package environment is recorded in
`renv.lock`, and cancer-specific command-line entry points are available.

The important remaining refactor work is narrower but still real: HSA feature
preparation consumes legacy generated inputs, while the active lymphoma/MCT
comorbidity assembly and cancer-specific medication sections still contain
positional column selections. These are documented rather than hidden.

Until those stages are complete, run notebooks only in the documented dependency order and do not assume similarly named dated or `updated` files are interchangeable.

## Data access

GRLS data access is managed separately from this code repository. A reproducible handover must include the exact approved data snapshot or instructions for retrieving it, together with its release date and any manually curated inputs.
