# Analysis register

This register is the source of truth for notebook status. Entries marked **author confirmation required** must not be deleted or treated as canonical solely from their filenames.

## Haemangiosarcoma

| Notebook | Apparent role | Status |
|---|---|---|
| `Code/Creating HSA study population.qmd` | Assemble the HSA cohort and feature-domain outputs | Active upstream candidate; author confirmation required |
| `Code/GRLS cox HSA time to diagnosis.qmd` | Historical Cox analysis and creation of a tidied analysis dataset | Intermediate/historical status requires confirmation |
| `Code/GRLS HSA updated analysis_all_cases.qmd` | Updated all-case modelling | Current candidate; author confirmation required |
| `Code/GRLS HSA updated analysis confirmed cases.qmd` | Confirmed-case modelling | Current candidate; author confirmation required |
| `Code/GRLS HSA cohort descriptives.qmd` | Descriptive, incidence, and survival outputs | Current candidate; author confirmation required |
| `Code/GRLS HSA updated analysis.qmd` | Near-duplicate of the all-case analysis | Superseded status requires confirmation |
| `Code/GRLS log reg HSA.qmd` | Earlier logistic-regression analysis | Historical status requires confirmation |

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

## Generated or duplicate source files

`Output/GRLS_HSA_updated_analysis_all_cases.qmd` and `Output/GRLS_HSA_updated_analysis_confirmed_cases.qmd` differ from the similarly named files under `Code/`. Their origin and authority must be confirmed before either copy is archived.
