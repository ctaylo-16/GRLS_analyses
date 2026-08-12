# Analysis pipelines

This document records the currently intended execution routes and the provenance of their handoff datasets. It distinguishes source data under `Data/` from reproducible generated artifacts under `Output/`.

After restoring the R environment and installing Quarto, run a documented route from the project root with:

```powershell
Rscript scripts/render_pipeline.R hsa
Rscript scripts/render_pipeline.R lymphoma
Rscript scripts/render_pipeline.R mct
```

Each command renders its notebooks sequentially and stops on the first failed render. It does not generate missing upstream prerequisites or substitute alternative inputs.

## Haemangiosarcoma

Analysis censoring date: **1 December 2024**.

| Order | Notebook | Main handoff |
|---|---|---|
| 1 | Shared feature notebooks and `Code/Creating HSA study population.qmd` | `Output/HSA_cohort_all_RFs_data.csv` |
| 2 | Dataset-preparation section of `Code/GRLS cox HSA time to diagnosis.qmd` | `Output/HSA_cohort_all_RFs_data_tidied_from_cox.csv` |
| 3 | `Code/GRLS HSA updated analysis_all_cases.qmd` | `Output/HSA_GRLS_updated_analysis_df.csv` and all-case results |
| 4a | `Code/GRLS HSA updated analysis confirmed cases.qmd` | Confirmed-case results |
| 4b | `Code/GRLS HSA cohort descriptives.qmd` | Descriptive, incidence, and survival outputs |

The Cox notebook has two distinct roles. Everything through the write of `HSA_cohort_all_RFs_data_tidied_from_cox.csv` is active dataset provenance and was used to construct the later logistic-regression analysis dataset. Its default `run_cox_analysis: false` parameter stops rendering at that point. The factor setup, Kaplan-Meier work, and Cox modelling after that handoff are retained but inactive and were not pursued through publication; they require an explicit `run_cox_analysis: true` render.

The active preparation section should be extracted into a dedicated HSA dataset-building workflow. Until that extraction is verified, do not delete or skip the preparation section merely because the later Cox modelling is inactive.

The confirmed-case analysis deliberately consumes `HSA_GRLS_updated_analysis_df.csv` created by the all-case analysis, so the all-case notebook currently precedes it even when only confirmed-case results are required.

## Lymphoma

Current cohort censoring date: **1 December 2024**.

The routine current workflow begins with the frozen input `Data/lymphoma_cohort_241201.csv`, then runs `Code/lymphoma GRLS/lymphoma dataset variable creation.qmd` to create `Output/GRLS_lymphoma_variables_dataset.csv`.

`Code/lymphoma GRLS/lymphoma denom creation.qmd` is a future cohort-refresh workflow, not a prerequisite for reproducing the current analysis. It depends on manually coded clinical-record data that has not yet been transferred into this repository's local `Data/` snapshot. Its current cohort is therefore expected to become out of date as later GRLS records accrue. Before the next substantive lymphoma analysis:

1. obtain an updated manually coded lymphoma case file;
2. choose and record a new censoring date;
3. update the denominator workflow to use a project-local input;
4. regenerate and quality-check the frozen cohort before rebuilding variables.

The notebook records 163 cases and 2,881 controls in the 1 December 2024 population.

## Mast cell tumours

Current cohort censoring date: **31 May 2025**.

| Order | Notebook | Main handoff |
|---|---|---|
| 1 | `Code/MCT GRLS/MCT denom creation.qmd` | `Output/MCT_cohort_250531.csv` |
| 2 | `Code/MCT GRLS/MCT dataset variable creation.qmd` | `Output/GRLS_MCT_variables_dataset.csv` |

The denominator currently uses `Data/dog_profile_for_MCT.csv`, `Data/study_endpoints_MCT.csv`, `Data/dog_profile.csv`, and `Data/medications.csv`.

## Shared prerequisites

Shared functions under `R/` now cover cancer-cohort preparation, deprivation,
environmental exposure windows, smoke dosage, sleep location, comorbidities,
and medication features for the lymphoma and MCT routes. The variable notebooks
still contain duplicated derivations for lifestyle, household exposures,
reproduction, weight, and activity; these are the next candidates for extraction,
with cancer-specific cohort definitions and output names kept in the entry-point
notebooks.

Important generated prerequisites include:

| Generated prerequisite | Producer or source |
|---|---|
| `Output/GRLS_dogs_MDI_2019.csv` | `Code/GRLS clinic and deprivation indices linkup.qmd` using `Data/vet_address.csv`, `Data/house_details.csv`, `Data/uszips.csv`, and `Data/MDI_2019.csv` |
| `Output/GRLS_medications_initial_tidy.csv` | `Code/GRLS medications.qmd` |
| Condition-domain features | Raw condition extracts under `Data/`, read by `R/comorbidities.R` |

`Code/GRLS clinic and deprivation indices linkup.qmd` reads all four inputs from the project-local `Data/` directory. Owner deprivation uses the modal primary home ZIP from `house_details.csv`, matching the existing 3,044-dog output. Clinic ZIP+4 values are deliberately reduced to five digits; this corrects historical missing clinic MDI values for 205 dogs. Because `Data/` is ignored by Git, all four inputs must be transferred as part of the approved data snapshot rather than committed.

## Execution rules

- Treat every filename and censoring date above as part of the analysis definition.
- Fail if a required source or generated prerequisite is absent; do not substitute a similarly named file.
- Do not run cancer pipelines concurrently while they share intermediate filenames.
- Record the Git commit and data-snapshot identifier for every accepted analysis run.
