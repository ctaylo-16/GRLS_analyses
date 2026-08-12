# Analysis register

This register is the source of truth for notebook status. Entries marked **author confirmation required** must not be deleted or treated as canonical solely from their filenames.

## Haemangiosarcoma

| Notebook | Apparent role | Status |
|---|---|---|
| `Code/Creating HSA study population.qmd` | Assemble the HSA cohort and feature-domain outputs | Upstream workflow; exact dependency mapping still required |
| `Code/GRLS cox HSA time to diagnosis.qmd` | Build the tidied HSA handoff dataset, then perform the alternative Cox analysis | Dataset-building section is active provenance; subsequent Cox modelling is retained but inactive and unpublished |
| `Code/GRLS HSA updated analysis_all_cases.qmd` | Updated all-case modelling | Current |
| `Code/GRLS HSA updated analysis confirmed cases.qmd` | Confirmed-case modelling | Current |
| `Code/GRLS HSA cohort descriptives.qmd` | Descriptive, incidence, and survival outputs | Current |
| `Code/GRLS HSA updated analysis.qmd` | Earlier HSA analysis | Historical |
| `Code/GRLS log reg HSA.qmd` | Earlier logistic-regression analysis | Historical |

## Lymphoma

| Notebook | Apparent role | Status |
|---|---|---|
| `Code/lymphoma GRLS/lymphoma denom creation.qmd` | Refresh the lymphoma study denominator/endpoints from manually coded records | Future refresh route; current routine workflow uses the frozen end-of-2024 cohort |
| `Code/lymphoma GRLS/lymphoma dataset variable creation.qmd` | Create lymphoma analysis variables from `Data/lymphoma_cohort_241201.csv` | Current; input cohort is frozen at 1 December 2024 and must be refreshed before future substantive analysis |

## Mast cell tumours

| Notebook | Apparent role | Status |
|---|---|---|
| `Code/MCT GRLS/MCT denom creation.qmd` | Create MCT study denominator/endpoints | Current |
| `Code/MCT GRLS/MCT dataset variable creation.qmd` | Create MCT analysis variables | Current |

## Shared feature notebooks

The environmental, activity, reproductive, medication, comorbidity,
deprivation, and exploratory notebooks mix derivation, checking, and reporting.
Their shared lymphoma/MCT implementations have largely been mapped into `R/`,
but HSA still consumes several legacy generated outputs and the active
lymphoma/MCT comorbidity and cancer-specific medication blocks remain partly
positional. Keep these notebooks as provenance until those remaining routes are
extracted and verified.

HSA, lymphoma, and MCT remain in one repository because they reuse substantial feature-engineering and helper code. Refactoring should create clear cancer-specific entry points while keeping genuinely shared implementations under `R/`; it should not split the repository and duplicate those implementations.

## Output policy

Quarto source files belong under `Code/`, not `Output/`. The `Output/` directory is reserved for generated figures, data, tables, and rendered analysis products, which are not tracked in Git.
