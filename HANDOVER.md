# GRLS analyses handover

## Purpose

This repository derives analysis datasets and reports for cancers in the Golden Retriever Lifetime Study. Its current subject areas are haemangiosarcoma (HSA), lymphoma, and mast cell tumours (MCT).

The repository contains both active and historical exploratory Quarto notebooks. Until the analysis register has been confirmed by the original author, do not infer that the most recently named or longest notebook is authoritative.

## Fail-fast policy

The refactored project should stop when an input, package, key, or expected column is missing. It should not:

- substitute a different input file;
- silently create missing analytical categories;
- discard duplicate subjects without an explicit aggregation rule;
- rely on row order to combine datasets;
- catch an analysis error and continue producing later outputs.

Warnings that indicate data-definition changes should be resolved before outputs are accepted.

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
3. Restore the project's package environment once `renv.lock` has been created from the author's working installation.
4. Place the agreed source-data snapshot in `Data/` and create `Output/`.
5. Run `Rscript scripts/check_setup.R` from the project root.

The setup check deliberately fails on missing packages or directories.

## Before rerunning analyses

- Confirm the canonical notebook list in `docs/analysis-register.md`.
- Confirm the source-data snapshot and the analysis censoring date.
- Check `git status` and record the commit used for the run.
- Do not run MCT, lymphoma, and HSA notebooks concurrently because some historical notebooks still share intermediate filenames.

## Known refactor work

- Convert shared feature creation into functions under `R/`.
- Replace order-dependent `cbind()` operations with explicit keyed joins.
- Replace positional column selection with named selections.
- Parameterise duplicated all-case and confirmed-case HSA analyses.
- Create `renv.lock` from the original author's known-working R environment.
- Add a single documented execution entry point after the canonical analysis list is confirmed.
