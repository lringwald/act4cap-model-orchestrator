---
# Claude Code Path Trigger:
paths:
  - "data/**/*"
  - "pipelines/**/*.{R,py}"
  - "scripts/**/*.{R,py}"
  - "notebooks/**/*.{ipynb,qmd,Rmd}"
  - "**/*.{csv,tsv,parquet,json,yaml,yml}"
  - "**/{SOURCES,DATA,LINEAGE,MODEL_LINEAGE}*.md"
# Antigravity Model Decision Trigger:
trigger: model_decision
description: "Data integrity, provenance and integration: no fabricated or hallucinated data, values, mappings or sources; no ad hoc or manual manipulation of data or results; immutable raw data; verified identifiers; vetting, registering and staged integration of new datasets; honest reporting of what was run versus assumed."
---

# Data Integrity, Provenance & Integration Rulebook

Scientific results are only as trustworthy as the data path behind them. This rulebook covers two risks: **data that is made up or quietly altered**, and **new data that is pulled in too quickly**, without knowing exactly what it is. It applies to people and coding agents alike; the agent-specific duties are in Principle 3.

It complements `data-pipelines.md`, which covers the mechanics of running pipelines consistently.

---

## Principle 1: Never Fabricate Data

1. **Never invent values** to fill a gap or make code run: observations, coefficients, priors, conversion factors, thresholds, class mappings, crosswalk weights, region codes, or totals.
2. **Never invent sources:** dataset names, versions, URLs, DOIs, citations, licences, file names, column names, or file contents. If a source cannot be verified, say so.
3. **Never present invented or synthetic results as real.** Synthetic data belongs in tests and fixtures, is clearly labelled as synthetic, and is never written to locations where real inputs or outputs live.
4. **Placeholders must be explicit and must fail if used.** Use `NA`, a sentinel, or a `TODO` that a validation check rejects, never a "plausible" number. A placeholder that silently flows into results is fabrication.
5. **Literature or expert values are sourced, not recalled.** A parameter taken from a paper or report is recorded with the reference, the table or page, the units, and the conditions it applies to.

---

## Principle 2: No Ad Hoc Manipulation

1. **Raw data is immutable.** Never edit raw inputs in place, by hand, or with a one-off script. Every change is a derived dataset produced by versioned code.
2. **No manual edits of values or outputs.** If something is wrong, fix it in code, as a documented and reproducible step, never by editing a CSV, a raster, or a results table.
3. **Every correction is a documented step:** what was changed, why, the rule applied (not a list of hand-picked rows), and the before/after effect (row counts, totals, affected regions). Record it in the lineage document.
4. **No result-driven cleaning.** Do not drop, clip, smooth, winsorize, or rescale data because the results look better afterwards. Outlier and error handling follows rules fixed *before* looking at the results (see the statistical rulebook, Principle 0).
5. **No silent tuning to validation data.** Calibrating parameters, thresholds, or class mappings against the data later used to evaluate the result is declared, and evaluation then uses independent data.
6. **Inconsistencies are reported, not smoothed over.** When two sources disagree, the size of the disagreement is part of the result.
7. **Missing, suppressed, and zero are three different states.**
   - Official statistics often suppress values for confidentiality or reliability, marked with flags (e.g. `:` or `c`).
   - A suppressed value is never coded as 0, silently dropped, or treated like ordinary missing data. Keep the flag through the pipeline.
   - Handle suppression explicitly: use bounds (e.g. from known totals of the remaining cells), a documented imputation with its uncertainty, or aggregation to a level where values are published. Record the choice in lineage.

---

## Principle 3: Agent Conduct

1. **Verify before using.** Check identifiers (column names, class codes, scenario names, file paths, function names) by reading the data, schema, or code. Do not guess them from memory or from what is usual.
2. **When data is missing, ambiguous, or contradictory, stop and ask,** or flag it explicitly. Never resolve it by assumption in a way that changes results.
3. **Separate verified facts from inferences** in every report: what was read in the code or data, what was run, and what is assumed or inferred. Cite file and line, or the data checked.
4. **Never claim a check passed unless it was run,** and say what it was run on (synthetic fixtures vs. real data, one country vs. all).
5. **Report failures and limitations plainly,** including steps that were skipped and inputs that were not available.

---

## Principle 4: Sourcing New Data

Before a new dataset is used for anything beyond exploration, establish what it is.

1. **Use authoritative, primary sources.** Prefer the original provider over mirrors, re-uploads, or derived copies of unknown provenance. Do not scrape unverified copies.
2. **Record a source card** for every dataset (e.g. in `data/SOURCES.md` or a registry file):

   | Field | Content |
   | :--- | :--- |
   | Name, provider | Official name and producing organization |
   | Version, release date | Exact version; date accessed or downloaded |
   | Access | URL or DOI; access method |
   | Licence & citation | Licence terms and required citation; compatibility with the project's use and outputs |
   | Coverage | Spatial extent, temporal coverage, resolution |
   | Reference system | CRS, grid, admin geometry version |
   | Definitions | Variables, units, class legend, method; known limitations and quality flags |
   | Checksums | Of the files as received |
   | Contact / owner in the project | Who vetted and maintains it |
3. **Check fitness for purpose:** do the definitions, units, coverage, and resolution match how the data will be used? A dataset that is "close enough" is documented as such, with the consequences.
4. **No hasty sourcing.** Time pressure is not a reason to skip the source card. If a dataset must be used provisionally, it is marked as provisional in the configuration and in the outputs.

---

## Principle 5: Integrating New Data (Staged)

New data enters the pipeline in stages, each with a written result:

1. **Ingest:** store the files as received, read-only, with checksums and the source card.
2. **Inspect:** an inspection report covering:
   - schema, types, units, and missing values;
   - value ranges and distributions against expectations;
   - spatial and temporal coverage, with a map where relevant;
   - CRS and alignment with the project grid.
3. **Harmonize** with documented, reproducible steps: crosswalks, resampling by variable type, unit conversions, gap handling (see the spatial rulebook, Principle 2).
4. **Validate** against independent references: official totals, overlap with existing project data, a previous version of the same dataset. Explain the differences.
5. **Wire in behind a configuration switch.** The new dataset is selected explicitly; it never silently replaces an existing input under the same name or path.
6. **Compare outputs before and after** on at least one representative case, and document the effect.
7. **Record the integration** in the lineage document: version, harmonization steps, validation results, and the configuration that selects it.

Rules that apply throughout:
- **Never mix versions** of the same dataset within one run.
- **Replacing a dataset is a versioned change,** with the old version kept for reproducibility.
- **No partial integration:** a dataset is either fully integrated (all stages above) or clearly marked as experimental.

---

## Principle 6: Provenance & Lineage

1. **Every derived dataset can be traced** to its inputs (with checksums), the code version, and the parameters that produced it.
2. **Register every input a run uses** in the run manifest (`data-pipelines.md`, Principle 9), including auxiliary files such as crosswalks, masks, and mapping tables.
3. **Lineage documents are kept current.** A change to a source, a crosswalk, or a correction step updates the lineage in the same change.

---

## Data Integrity Checklist

Before using new data or publishing results:

- [ ] **No fabrication:** no invented values, mappings, identifiers, or sources; placeholders fail if used; synthetic data only in tests.
- [ ] **No manipulation:** raw data unchanged; every correction is versioned code with rationale and before/after effect.
- [ ] **No result-driven cleaning or tuning:** cleaning rules fixed in advance; calibration against evaluation data declared.
- [ ] **Missing / suppressed / zero:** kept distinct, suppression flags preserved, handling documented.
- [ ] **Verified identifiers:** column names, codes, and paths checked against the data or code.
- [ ] **Source cards:** complete for every dataset used, including licence and version.
- [ ] **Staged integration:** ingest, inspect, harmonize, validate, configure, compare, record.
- [ ] **Lineage:** every derived dataset traceable; manifest lists all inputs with checksums.
- [ ] **Honest reporting:** verified vs. inferred separated; what was run, on which data, is stated.
