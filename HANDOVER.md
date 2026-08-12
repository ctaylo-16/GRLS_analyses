# GRLS analyses handover

## Purpose

This repository derives analysis datasets and reports for cancers in the Golden Retriever Lifetime Study. Its current subject areas are haemangiosarcoma (HSA), lymphoma, and mast cell tumours (MCT).

The repository contains both active and historical exploratory Quarto notebooks. The confirmed notebook roles are recorded in `docs/analysis-register.md`, and the current execution routes and censoring dates are recorded in `docs/pipelines.md`. Do not infer that the most recently named or longest notebook is authoritative.

## Fail-fast policy

The refactored project should stop when an input, package, key, or expected column is missing. It should not:

- substitute a different input file;
- silently create missing analytical categories;
- discard duplicate subjects without an explicit aggregation rule;
- rely on row order to combine datasets;
- catch an analysis error and continue producing later outputs.

Warnings that indicate data-definition changes should be resolved before outputs are accepted.

Domain annotations, source notes, and comments explaining analytical rationale are part of the handover record. Preserve them when moving implementation into shared functions; update wording only when it is obsolete or misleading.

## Local data

`Data/` and `Output/` are intentionally excluded from Git. A handover therefore requires the repository and the corresponding data snapshot. Record the date or release identifier of that snapshot when transferring it.

Expected local directories:

```text
Data/       source GRLS extracts and manually curated inputs
Output/     generated intermediate datasets, tables, and figures
```

Do not rename columns or source files in `Data/` without updating the consuming code. The current notebooks use exact file and column names.

## Initial setup

1. Install R, RStudio if desired, and Quarto.
2. Open `GRLS_analyses.Rproj` so project-relative paths resolve consistently.
3. Run `renv::restore()` to restore the package versions recorded from the author's R 4.4.1 environment.
4. Place the agreed source-data snapshot in `Data/` and create `Output/`.
5. Run `Rscript scripts/check_setup.R` from the project root.

The setup check deliberately fails on missing packages, directories, or Quarto.

Run a cancer workflow with `Rscript scripts/render_pipeline.R <hsa|lymphoma|mct>`. The exact notebook sequence and required handoffs are documented in `docs/pipelines.md`.

The HSA command begins by assembling `HSA_cohort_all_RFs_data.csv` from the
legacy generated feature files listed in `docs/pipelines.md`; it does not yet
regenerate all of those feature files from raw data.

The assembly now sources cohort status, endpoint date, endpoint study year, and
record date directly from the HSA cohort definition. Legacy handoffs contribute
features only; their saved row-number and duplicate metadata columns are not
propagated into the assembled dataset.

## Before rerunning analyses

- Check the notebook statuses in `docs/analysis-register.md` and the execution route in `docs/pipelines.md`.
- Confirm the source-data snapshot and the analysis censoring date.
- Check `git status` and record the commit used for the run.
- Do not run MCT, lymphoma, and HSA notebooks concurrently because some historical notebooks still share intermediate filenames.

For HSA, the current reports are:

- `Code/GRLS HSA updated analysis_all_cases.qmd`
- `Code/GRLS HSA updated analysis confirmed cases.qmd`
- `Code/GRLS HSA cohort descriptives.qmd`

The preparation section of `Code/GRLS cox HSA time to diagnosis.qmd` produced the dataset used by the current HSA analyses. Its default render stops after writing that dataset. The subsequent Cox modelling is a retained, inactive alternative analysis route that was not pursued through publication and requires the explicit parameter `run_cox_analysis: true`. The two earlier HSA logistic-analysis notebooks are historical.

The current lymphoma workflow uses the frozen `Data/lymphoma_cohort_241201.csv` cohort. It is censored at 1 December 2024 and should be refreshed from updated manually coded records before future substantive lymphoma analysis.

## Remaining refactor work

- Extract the active HSA dataset preparation from its legacy generated feature
  files and separate it fully from the inactive Cox analysis.
- Parameterise duplicated all-case and confirmed-case HSA analyses.
- Restore `renv.lock` and run all routes on a clean handover machine, including
  Quarto and any operating-system dependencies.
- Revisit machine-specific paths only if the associated historical or inactive
  analyses are reactivated; they are not dependencies of the current active
  preparation routes.

Known current data-definition blockers are documented beside the affected
pipeline steps. In particular, subject `094-000461` has no endpoint study year
in either frozen lymphoma or MCT cohort. Do not restore the historical default
of study year 1; correct the cohort definition explicitly before accepting a
new analysis dataset.

The reproduction workflow also stops on biologically or temporally inconsistent
dates rather than converting negative intervals to zero. The current snapshot has
two neuter dates before birth (`094-000441`, `094-023124`) and ten neuter dates
after the MCT endpoint. Four of the latter also affect lymphoma (`094-006352`,
`094-000677`, `094-026390`, `094-027529`). Resolve these in the source/cohort
definition before accepting regenerated outputs.
