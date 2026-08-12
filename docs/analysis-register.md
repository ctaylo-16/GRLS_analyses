# Analysis register

This register is the source of truth for notebook status. Entries marked **author confirmation required** must not be deleted or treated as canonical solely from their filenames.

## Haemangiosarcoma

| Notebook | Apparent role | Status |
|---|---|---|
| `Code/Creating HSA study population.qmd` | Assemble the HSA cohort and feature-domain outputs | Upstream workflow; exact dependency mapping still required |
| `Code/GRLS cox HSA time to diagnosis.qmd` | Alternative Cox-analysis route | Retained but inactive; not pursued through publication |
| `Code/GRLS HSA updated analysis_all_cases.qmd` | Updated all-case modelling | Current |
| `Code/GRLS HSA updated analysis confirmed cases.qmd` | Confirmed-case modelling | Current |
| `Code/GRLS HSA cohort descriptives.qmd` | Descriptive, incidence, and survival outputs | Current |
| `Code/GRLS HSA updated analysis.qmd` | Earlier HSA analysis | Historical |
| `Code/GRLS log reg HSA.qmd` | Earlier logistic-regression analysis | Historical |

## Lymphoma

| Notebook | Apparent role | Status |
|---|---|---|
| `Code/lymphoma GRLS/lymphoma denom creation.qmd` | Create lymphoma study denominator/endpoints | Active candidate; author confirmation required |
| `Code/lymphoma GRLS/lymphoma dataset variable creation.qmd` | Create lymphoma analysis variables | Active candidate; author confirmation required |

## Mast cell tumours

| Notebook | Apparent role | Status |
|---|---|---|
| `Code/MCT GRLS/MCT denom creation.qmd` | Create MCT study denominator/endpoints | Active candidate; author confirmation required |
| `Code/MCT GRLS/MCT dataset variable creation.qmd` | Create MCT analysis variables | Active candidate; author confirmation required |

## Shared feature notebooks

The environmental, activity, reproductive, medication, comorbidity, deprivation, and exploratory notebooks currently mix derivation, checking, and reporting. They should remain in place until their outputs have been mapped to the active cancer pipelines.

HSA, lymphoma, and MCT remain in one repository because they reuse substantial feature-engineering and helper code. Refactoring should create clear cancer-specific entry points while keeping genuinely shared implementations under `R/`; it should not split the repository and duplicate those implementations.

## Output policy

Quarto source files belong under `Code/`, not `Output/`. The `Output/` directory is reserved for generated figures, data, tables, and rendered analysis products, which are not tracked in Git.
