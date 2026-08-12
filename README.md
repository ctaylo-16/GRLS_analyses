# Golden Retriever Lifetime Study analyses

This R and Quarto project derives datasets and analyses for cancers in the Golden Retriever Lifetime Study (GRLS), principally haemangiosarcoma, lymphoma, and mast cell tumours.

The source data consist of annual owner and veterinarian questionnaire extracts covering demographics, lifestyle, environment, health, medication, reproductive history, and clinical outcomes. Source data are not stored in Git.

## Start here

- Read `HANDOVER.md` for setup, local-data requirements, and the fail-fast operating policy.
- Read `docs/analysis-register.md` before choosing a notebook to run.
- Open `GRLS_analyses.Rproj` and run `Rscript scripts/check_setup.R` from the project root.

All file references now use `here::here()` and resolve from the repository root. A notebook should not depend on a particular Windows username or working directory.

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

The project is undergoing a handover refactor. Machine-specific paths have been removed, obvious cross-analysis output errors corrected, shared helpers made fail-fast, and the author's R 4.4.1 package environment recorded in `renv.lock`. The current HSA reports have been identified. The next stages are to document the lymphoma and MCT execution routes, extract the remaining shared feature engineering, and create cancer-specific execution entry points.

Until those stages are complete, run notebooks only in the documented dependency order and do not assume similarly named dated or `updated` files are interchangeable.

## Data access

GRLS data access is managed separately from this code repository. A reproducible handover must include the exact approved data snapshot or instructions for retrieving it, together with its release date and any manually curated inputs.
