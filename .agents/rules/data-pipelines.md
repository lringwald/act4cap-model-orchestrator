---
# Claude Code Path Trigger:
paths:
  - "pipelines/**/*.{R,py,sh,ps1}"
  - "scripts/**/*.{R,py,sh}"
  - "workflow/**/*.{R,py,smk}"
  - "Snakefile"
  - "**/*.smk"
  - "src/**/*.{R,py}"
  - "R/**/*.R"
  - "**/config*.{R,py,yaml,yml,toml}"
  - "**/PIPELINE_LINEAGE*.md"
# Antigravity Model Decision Trigger:
trigger: model_decision
description: "Data processing and pipeline consistency: stage contracts, joins and keys, conservation ledgers, single source of truth, no silent fallbacks in critical steps (identifier resolution, year/scenario selection, joins, units, totals), error handling, idempotent outputs, run manifests, testing, scale and performance (deterministic chunking, parallel seeds, bounded memory), versioned releases, protection against double counting (run-scoped intermediates, resolved inputs, expected-count and uniqueness assertions before aggregation, control totals), pipeline lineage (PIPELINE_LINEAGE.md tracing which models, repositories, datasets, caches and side-loads are effectively called, versus planned), and production runs (one production script and run ID for the main model and its declared variants, variant table, enforced configuration differences, no partial reruns) in multi-stage data pipelines."
---

# Data Processing & Pipeline Consistency Rulebook

Large data pipelines rarely fail loudly. They fail by **silently continuing with something slightly different**: the closest year instead of the requested one, the first of several matching scenarios, a zero where a join found nothing, a tag that matches no row. Each step looks fine on its own, and the error only surfaces, if at all, in the final maps. This rulebook makes every stage state what it expects, check it, and say so when it deviates.

It is model- and domain-agnostic, and complements the statistical and spatial rulebooks. Concrete numbers here (tolerances, thresholds, counts) are recommended defaults that a project may adapt in its project profile; the rules themselves (no silent fallbacks, contracts, no double counting) are not optional. See the statistical rulebook, "How to Read the Numbers in This Rulebook".

---

## Principle 1: Stages With Explicit Contracts

1. **A pipeline is a chain of named stages.** Each stage has declared inputs, outputs, and parameters. The run order is explicit (orchestrator, Snakemake, `targets`, Make); it is never implied by which script happens to be sourced first.
2. **Every stage boundary has a contract,** checked when data enters and when it leaves:
   - **Schema:** required columns, types, and units (in column names, e.g. `area_km2`).
   - **Keys:** which columns identify a row, and that they are unique and non-missing.
   - **Domain:** allowed values (class codes, scenario names, years), value ranges, and no unexpected `NA`.
   - **Invariants:** totals that must be conserved, shares that must sum to 1, non-negativity.
3. **A contract violation stops the stage** with a message naming the stage, the check, what was expected, what was found, and how to fix it. A stage never "repairs" a violated contract on its own.

---

## Principle 2: No Silent Fallbacks at Critical Points

A fallback replaces the requested input or behaviour with something else when the request cannot be met. Some fallbacks are legitimate. A **silent** fallback at a critical point is a defect.

1. **Critical points**, where every fallback must be explicit:
   - Resolving identifiers: scenario, region, class, model, year or period, file versions.
   - Selecting inputs: "latest" files, base years, target years, reference datasets.
   - Joins on keys, and filling what a join did not find.
   - Unit conversions and any step that changes totals (rescaling, clipping, rounding, dropping rows).
2. **A fallback is acceptable only if all of the following hold:**
   - **Correct or marked:** the result stays correct, or is explicitly marked as approximate.
   - **Logged:** the log states what was requested, what was used instead, and why.
   - **Configured:** if it changes results, it is a named option in the configuration (opt-in, with a documented default), not a hidden branch in the code.
   - **Recorded:** it is written to the run manifest (Principle 9), so it is visible in the outputs, not only in a console that nobody reads.
   - **Bounded:** e.g. a year fallback within a maximum gap, beyond which the run stops.
3. **Forbidden patterns** at critical points (each was found in real pipelines):

   | Pattern | Why it is dangerous | Do instead |
   | :--- | :--- | :--- |
   | `matches[1]` on several candidates (scenario, file, class) | Result depends on row or file order | Exact match; a partial match only when it is **unique**; otherwise stop and list the candidates |
   | Substring or fuzzy matching of identifiers in both directions | Short names match unrelated ones | Exact keys, or a declared prefix rule |
   | "Closest available" year or version without a bound | The run silently describes another year | Explicit fallback option, logged, with a maximum gap |
   | Placeholder tags that match nothing (`"ALL"`, `"*"`, `""`) | Filters drop every row; the feature is silently off | Engine-defined wildcards, or omit the dimension; validate that every tag matches something |
   | `tryCatch(..., error = function(e) NULL)` around required inputs | Missing data becomes "no data" | Catch only expected, optional inputs; required inputs fail |
   | `if (exists("x")) x else <default>` with defaults repeated in several files | Different scripts use different defaults (e.g. base year 2020 vs 2018) | One default in one configuration |
   | Filling unmatched join rows with 0 | A failed join reads as "zero area" | Fill only when absence truly means zero (e.g. sparse data), and count what was filled |
   | Proportional rescaling to make inconsistent totals add up | Hides a mismatch across every class | Route the gap to one documented balancing item, or report the inconsistency |
   | Lags built as `year + step` with an assumed step | Irregular periods pair with nothing | Pair by position in the ordered period list; assert an expected step if one is known |
   | R's partial matching of `$` names | Works until a second similar name appears | Exact names |
   | Collecting inputs by globbing a shared directory (`list.files(temp, "^prefix_.*")`) | Picks up files from earlier runs, which are then summed (double, triple counting) | Resolve inputs from the run manifest or by explicit run ID, and assert the expected count (Principle 13) |
   | "Newest file" or "latest run directory" selection (`tail(sort(dirs), 1)`, maximum modification time) | Picks another run when the current one is incomplete, or when a later or parallel run exists | Pass the run ID explicitly; stop if it is missing (Principle 13) |
   | A wildcard for the run date or version inside an otherwise anchored pattern (`_[0-9]{8}_lu_`) | Still matches every run in the directory: a glob in disguise | Build the exact name from the run ID with the producer's naming function |
   | Aggregating whatever inputs were found when expected ones are missing | A partial total looks complete | Stop and list the missing keys; partial aggregation only as a configured option, flagged in the outputs |
   | `if (length(a) > 0) a else b` to choose between two input locations | Silently switches the source of a quantity between runs | Configure one source; if both exist, verify that they agree |
   | `install.packages()` or downloads during a run | Run depends on network and current package versions | Declared dependencies (`renv`, lock files) |
   | Hard-coded user or drive paths as fallbacks (`P:/...`, `/Users/...`) | Works on one machine, silently wrong elsewhere | Configured data roots |

4. **Do not hide a mismatch by "fixing" it.** When two sources disagree (model vs. map, target vs. capacity), the default is to **report** the disagreement and its size. Resolving it (rerouting, rebalancing, carrying forward) is an explicit, documented option.

---

## Principle 3: Joins & Keys

1. **Join on explicit keys,** never on row order or position.
2. **Normalize keys once,** at ingestion (type, case, padding, version of the code list), not ad hoc in each join.
3. **Assert cardinality** (1:1, m:1, 1:m) before the join and check the row count after it. An unexpected many-to-many join is an error.
4. **Report unmatched rows on both sides** (anti-joins): counts and the affected totals (area, mass, value). Unmatched rows are a result to look at, not something to fill and forget.
5. **Every key in a filter or join must match something.** A filter that returns zero rows for a requested scenario, region, or period is an error unless "none" is a valid outcome.

---

## Principle 4: Conservation Ledger Across Stages

1. **Track conserved quantities through every stage** (total area, population, mass, money), so that the change at each stage is known.
2. **Every change is explained:** dropped, clipped, rounded, rerouted, or rescaled, each with its size. An unexplained difference is a defect.
3. **Use relative tolerances:** $|\Delta| \le \epsilon_{\text{rel}} \cdot \text{total}$. A variable named "relative" is never compared against an absolute difference.
4. **Two-tier checks:** expected noise (rounding, floating point) is reported; an excessive discrepancy stops the run. A check that fires on every run is a bug in the check: either the tolerance is wrong or the data are.
5. **Thresholds that drop small values** (e.g. flows below 0.1 ha) are counted and logged with the total removed. Where the removal breaks an invariant, the data are re-balanced afterwards.

---

## Principle 5: Single Source of Truth

1. **One implementation per function.** No shadow copies that override a package function depending on sourcing order. A performance or behaviour change goes into the canonical implementation.
2. **One definition per default and constant,** in the configuration. Magic numbers in scripts are named and documented.
3. **Shared steps are shared code.** When several pipelines perform the same step (harmonization, period pairing, relabelling), they call the same helper. Legitimate differences are configuration parameters, not divergent copies.
4. **Dependencies are pinned** (submodule commits, lock files), and the version that actually ran is recorded. A pinned commit must be reachable from the dependency's main or release branch: a pin to a feature-branch commit silently depends on code that the dependency's main does not have, and is lost on the next bump.

---

## Principle 6: State & Side Effects

1. **Do not mutate a caller's data in place** unless that is the documented contract. With reference semantics (`data.table`, pandas views), copy before renaming or modifying inputs that the caller still uses.
2. **Avoid global-environment coupling.** Stages take explicit arguments and return explicit results; reading `exists("x")` from a shared environment is a last resort, and never for critical parameters.
3. **Sourcing order must not change results.** Loading the same scripts in a different order must give the same functions and the same outputs.
4. **Clean up between stages,** so that objects from one stage or scenario cannot leak into the next.

---

## Principle 7: Errors, Warnings & Logs

1. **Fail early and specifically:** stage, check, expected vs. found, and the likely fix.
2. **Never print-and-continue on a broken invariant.** Messages and warnings are for expected, harmless deviations; invariant breaches stop the run.
3. **Keep a warning budget.** A warning that appears in every run is either a bug or noise; fix it or demote it, so that new warnings are seen.
4. **Structured, per-stage logs** with counts (rows in/out, matched/unmatched, totals), written to files next to the outputs.

---

## Principle 8: Idempotent, Atomic Outputs

1. **Re-running a stage with the same inputs gives the same result.** No dependence on the outputs of a previous, different run.
2. **Clear or version the outputs at the start of a run;** never append to files left over from another run.
3. **Write atomically:** write to a temporary file, then rename, so that an interrupted run leaves no half-written file that looks complete.
4. **Appended chunks share one schema.** When outputs are appended in chunks, every chunk has the same columns in the same order, checked against the existing header.
5. **"Latest file" selection is explicit.** Resolve inputs by run ID, version, or manifest; picking the newest file by date is a fallback (Principle 2) and is recorded. Collecting *all* matching files is worse: it double counts (Principle 13).

---

## Principle 9: Configuration & Run Manifest

1. **Every choice that changes results lives in the configuration,** with validated allowed values. Unknown or misspelled options are errors.
2. **Each run writes a manifest** next to its outputs:
   - code versions (git commit and dirty flag, for the pipeline and every pinned dependency);
   - the full resolved configuration;
   - input files with checksums or versions;
   - every fallback taken, and every threshold-based removal with its size;
   - start/end time and machine.
3. **Caches and intermediates are keyed by content and code version,** so that a changed input or a fixed bug invalidates them.
4. **The manifest is the evidence for `PIPELINE_LINEAGE.md`** (Principle 14): what it records is what was effectively used.
5. **Production runs record their variants.** For a production run (Principle 15), the manifest holds the run ID, the variant table, and per variant its resolved configuration, so that the declared changes can be checked.

---

## Principle 10: Testing

1. **Each stage has a small synthetic fixture** with a known answer: conservation, key uniqueness, edge cases (missing rows, irregular periods, empty groups, ambiguous identifiers).
2. **Regression tests for every fixed bug,** shown to fail on the old code and pass on the new.
3. **Refactors are compared before/after.** Where the behaviour is meant to be unchanged, the outputs must be identical, or the differences are explained.
4. **Tests never use silent fallbacks to pass.** A test that passes only because a missing input became an empty table is a failing test.

---

## Principle 11: Scale & Performance

1. **Partition work deterministically:** by region, tile, scenario, or period, with the partition defined in the configuration. The set of chunks and their order do not depend on the machine or the number of workers.
2. **Results must not depend on how the work is split.** Running with one worker or many, or with different chunk sizes, gives the same outputs. Test this on a small case (1 chunk vs. several).
3. **Random numbers per chunk:** derive each chunk's seed from a master seed and the chunk identifier, or use independent parallel streams (e.g. `parallel::nextRNGStream()` / L'Ecuyer-CMRG in R). Never rely on the global random state across parallel workers.
4. **Bounded memory:** load only what a chunk needs, release it afterwards, and avoid full-table joins inside loops (split or index once, before the loop). A run that only works with all data in memory is a scaling failure waiting to happen.
5. **Temporary files on fast local disk,** in a run-specific directory that is cleaned up at the end, and never in the output directory.
6. **Resumable chunks:** each chunk writes its own complete, atomic output (Principle 8), so a failed run resumes by redoing only the missing chunks.

---

## Principle 12: Releases

1. **Versioned and immutable:** every published output set has a version (semantic or date-based). A released version is never overwritten; corrections are a new version with a changelog entry.
2. **Citable:** each version gets a persistent identifier (e.g. a DOI through Zenodo) and a recommended citation.
3. **A complete bundle:**
   - the data files and their JSON sidecars;
   - the class tables and legends;
   - the run manifest (Principle 9) and the validation report;
   - a README describing contents, units, and known limitations;
   - the licence, and a checksum file for all files.
4. **Reproducible from the repository:** the release records the code versions and configuration it was produced with, so it can be regenerated.
5. **A release is one production run** (Principle 15): the main specification and its declared variants, from one run ID. Outputs of different runs are not combined into one release.

---

## Principle 13: Input Discovery, Run Isolation & No Double Counting

**The failure mode:** a post-processing step collects its inputs with a file pattern over a shared temporary directory, e.g. every file matching `^production_downscaled_.*\.qs$`. Each run adds its own files there. The second run therefore loads its own files *and* the first run's, and an aggregation by `(pixel, scenario, indicator)` sums them. Every total doubles, then triples with the third run. Quantities loaded from a run-specific directory are not affected, so the totals stay plausible-looking, ratios such as €/ha are inflated, and nothing fails.

The same pattern also catches **other pipelines' files** that use the same prefix in a shared directory, and **format twins** of one dataset (the `.parquet` and the `.csv` copy of the same scenario). A date wildcard in the pattern (`_[0-9]{8}_`) does not help: it matches every run. And when the step finds fewer files than expected, it aggregates whatever is there, so partial totals look complete as well.

No single check prevents this reliably. Apply all of the following layers.

1. **Run-scoped storage.**
   - Every run writes its intermediates into a run-specific location (e.g. `<temp>/<run_id>/`), or at least puts the run ID in every file name.
   - Shared directories hold only versioned, immutable reference data, never run-scoped intermediates.
   - Create the run directory fresh at the start of a run, or verify that it is empty.
   - Cleaning up old files is good hygiene, but not a safeguard: concurrent runs, interrupted runs, and manual copies leave files behind.
2. **Inputs are resolved, not discovered.**
   - A consuming stage takes the exact list of input files from the producing stage's manifest (Principle 9), or from a resolver called with the explicit run ID and the expected keys (scenario, chunk, period).
   - Collecting inputs by globbing a directory is forbidden for shared locations.
   - Where a pattern is unavoidable, anchor it on the full identity of the file, e.g. `^production_downscaled_<scenario>_.*_<run_id>\.qs$`, never on a prefix alone. A wildcard for the run ID or date is not an anchor.
   - **Producer and consumer use one naming function.** The function that builds the file name when writing is the one that builds it when reading, so the two cannot drift apart.
   - **Names carry their namespace.** Where a directory is shared across pipelines or projects, the pipeline or project name is part of every file name, so equal prefixes from different pipelines cannot collide.
3. **The run ID is required.** It is passed to the stage explicitly. If it is missing, the stage stops. It is never derived silently from a fallback such as `if (exists("run_date")) ... else <parse it from a path>`, nor from "the newest file" or "the latest run directory" (Principle 2). The output directory a stage writes to is checked to belong to that run ID.
4. **Assert the expected inputs.**
   - The number of input files equals the expected number (e.g. scenarios × chunks, from the manifest or the configuration), and every expected key is present exactly once.
   - On a mismatch, stop and list the missing and the unexpected files by name. Aggregating the subset that happens to exist is never a silent default; if partial results are needed, it is a configured option, and the outputs say which keys are missing.
   - **One physical copy per logical input.** When a stage writes the same data in several formats (e.g. `.qs` and `.csv`, `.parquet` and `.csv`), the consumer reads exactly one copy per key, chosen by a fixed, configured preference. A pattern that can match both copies is forbidden. A fallback to the second copy is logged per key (Principle 2).
   - Do not silently prefer one location over another (e.g. temporary vs. output directory). Configure the source, or, if both exist, check that they agree.
5. **Tag every row with its source on load:** file name, run ID, and version. Before combining, assert that the data come from a single run ID, or from exactly the configured set of runs. Keep these columns until the aggregation is complete.
6. **Guard every aggregation.**
   - Before a sum over rows, assert that keys are unique on the full identity, including the source columns. Two rows for the same pixel, scenario, and indicator from different files or runs are an error, not something to add up.
   - A `group by` that drops a source dimension (run, version, file) without first asserting that it is constant is forbidden.
   - Chunked inputs must be disjoint partitions: check that no key appears in more than one chunk file.
7. **Reconcile control totals.**
   - The producing stage writes control totals (e.g. per indicator and scenario) to its manifest.
   - The consuming stage recomputes them after loading and compares them with a relative tolerance (Principle 4).
   - A ratio close to 2, 3, or another integer, against the producer's totals or against the previous run, is the signature of duplication.
   - **Signed quantities** (profit, net flows, changes) can cancel, so a signed total may hide a duplication or flag a false one. Compare absolute sums, or the positive and negative parts separately.
   - **Subsets are bounded.** Where a stage filters (e.g. to one country or one land-use group), the retained absolute total can never exceed the producer's. A ratio above 1 stops the stage.
8. **Consistent provenance in combined quantities.** When quantities are combined into ratios or shares (e.g. € per ha, loss per area), assert that all inputs come from the same run. Inputs from different sources or runs can produce a wrong ratio even when each total looks plausible on its own.
9. **External plausibility anchors.**
   - Compare headline aggregates with an independent reference (official statistics, the previous release) and report the deviation.
   - A large jump between runs without a code or configuration change that explains it blocks publication until it is explained.
10. **Regression test:** a fixture directory that also contains files from an older run, a file of another pipeline with the same prefix, a format twin, and a duplicated chunk. The test asserts that they are excluded, that the totals equal the current run's inputs, and that the count, uniqueness, and run-context checks fire. Run it once against the old code too, and record the multiplier it produced (Principle 10).

---

## Principle 14: Pipeline Lineage (`PIPELINE_LINEAGE.md`): What Is Effectively Called

Configuration files, READMEs, and plans say what a pipeline is *meant* to use. What it actually uses can differ: a fallback picks another file, a dev checkout of an engine is loaded instead of the pinned version, a script is sourced from a sibling repository, a cached intermediate from an earlier run is reused, a hard-coded network path wins over the configured one. Every pipeline therefore keeps a `PIPELINE_LINEAGE.md` that records which models, repositories, and exogenous datasets are **effectively** called or side-loaded, stage by stage.

1. **"Effectively" means traced, not planned.**
   - Every entry is backed by evidence from a real run: the resolved path, package location, commit, or checksum as logged during that run (Principle 9 manifest, I/O trace, run log).
   - Configuration, documentation, code reading, and intentions are not evidence of what ran. They go in the "planned" column (item 4), not in the "effective" one.
   - An entry that has not been traced is marked **unverified**, never filled in from what the code should do. This applies to agents in particular: do not write lineage entries from memory or from a plan.
2. **What is recorded.** Everything that enters the computation from outside the stage's own code:
   - **Models and engines:** estimation and downscaling engines, macro-model outputs, solvers. Record the package or binary actually loaded, its location (installed library vs. development checkout vs. vendored copy) and version or commit.
   - **Repositories and code:** submodules, vendored copies, and any code sourced or imported from outside the repository, with the resolved path and commit.
   - **Exogenous datasets:** files, databases, APIs, network drives. Record the resolved path or URL, version, checksum or size and modification time, and rows/extent read. Link the dataset's source card (data-integrity rulebook).
   - **Reused intermediates and caches:** which cached file was read, and which run produced it.
   - **Environment:** environment variables, working-directory–relative paths, and machine-specific roots that changed what was read.
3. **Side-loads are made visible.** A side-load is any input that does not come through the declared configuration or dependency list. Examples:
   - `source()` / `import` of a script by a path outside the repository;
   - `if (file.exists(a)) a else b` location fallbacks, and environment-variable overrides of data roots;
   - `load_all()` of a development checkout where an installed, pinned package is declared;
   - a globbed or "latest file" input (Principle 13);
   - defaults hard-coded in scripts that bypass the configuration.

   Each side-load gets its own row, flagged as such. It is either declared in the configuration, or removed.
4. **Format.** One table per stage, plus a header with the date, pipeline commit, and the reference run ID the trace comes from:

   | Stage | Component | Kind (model / repo / dataset / cache / env) | Effective source (resolved path, URL, package location) | Version / commit / checksum | How loaded (package, submodule, `source()`, side-load, fallback, cache) | Evidence (run ID, manifest or log line) | Planned source (configuration / docs) | Match |
   | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |

   A mismatch between the effective and the planned source is a finding: fix the pipeline, or update the configuration and documentation, with a reason.
5. **How to trace.**
   - Route reads through a small set of I/O helpers that log the resolved path, size, modification time, and checksum (or rows read) to the run manifest. Direct reads that bypass them are themselves a side-load.
   - Log package loading (name, version, library path or development path, commit) and every sourced or imported external file.
   - Where helpers cannot cover everything, check with a language- or OS-level trace of file opens on a reference run, and compare it with the document.
   - Configuration variants and conditional branches get their own entries. Branches the reference run did not exercise are marked **not traced**.
6. **Verification, not authorship.**
   - Regenerate or re-verify the document against a fresh reference run whenever a stage's inputs, a dependency, or a data root changes, and before every release (Principle 12).
   - The check fails on an effective input missing from the document, and on a documented input that was not effectively used.
   - The document is versioned with the code; each verification records its date and run ID.
7. **Relationship to other records.** The run manifest (Principle 9) is the raw per-run evidence. `PIPELINE_LINEAGE.md` is the curated, cross-stage view verified against it. Source cards (data-integrity rulebook) describe each dataset, and `MODEL_LINEAGE.md` (statistical rulebook) describes the mathematical transformations. The lineage links to both rather than repeating them.

---

## Principle 15: Production Runs & Declared Variants

Results that are reported together, typically a main model and its robustness or sensitivity variants, are produced together, by one **production run**. This principle defines the mechanics. What variants to run, and how to read them, is in the statistical rulebook (D.4); how documents use them is in the reporting rulebook (Principle 2).

1. **One production script, one run.**
   - A production script runs the main specification and every declared variant. It is the only entry point for results that are released or reported.
   - The run has one run ID. Each variant has a variant ID; the main specification is the variant `main`.
   - All variants share the code commit(s) of the pipeline and every engine, the input-data versions, and everything in the configuration that is not declared as a change.
2. **The variant table is part of the manifest** (Principle 9). One row per variant: variant ID, the changed setting, main value → variant value, the kind of change (prior, setting, input, algorithm, or architecture), and its purpose. Architecture changes (a different model structure) are allowed, and labelled as such. For example:

   | variant_id | Changed setting | Main value → variant value | Kind | Purpose |
   | :--- | :--- | :--- | :--- | :--- |
   | `prior_wide` | prior scale on β | 1 → 2.5 | prior | prior sensitivity |
   | `alt_mask` | policy mask source | A → B | input | data robustness |
   | `no_spatial` | spatial random effect | on → off | architecture | structural robustness |
3. **Declared changes are enforced.** Before estimation, the production script resolves the full configuration and specification of every variant and compares it with `main`. The difference must equal the declared change. Any other difference stops the run, and the message lists the undeclared differences.
4. **Every variant is a complete, isolated run of the pipeline.** Each writes its outputs, manifest entries, and pipeline lineage (Principle 14) under its variant ID, in run-scoped storage (Principle 13). File names and keys carry both IDs, so outputs of different variants can never be combined by accident.
5. **Variants may run as separate jobs.** Expensive variants may run on other machines, in other jobs, or at other times, as long as the same production script launches them with the same code commit, configuration, and inputs. At assembly, the manifests are checked: same run ID, same commit and input versions, declared changes only. A variant whose manifest does not match does not join the run.
6. **No partial reruns.**
   - A change of code, input, or configuration produces a new run of the production script, for `main` and all variants, with a new run ID.
   - Rerunning a single variant or a single stage later, under changed code or inputs, and merging it into an existing run is forbidden. Either rerun through the production script, or treat the result as a separate run.
   - Every output downstream of a rerun stage is regenerated; caches are keyed by run and variant ID.
7. **The variant set is fixed before results are seen.** The production script declares the variants up front. Variants added later are marked as added later, with the reason, and all declared variants are kept in the outputs, including failed ones (statistical rulebook D.4; data-integrity rulebook, Principle 2).

---

## Pipeline Review Checklist

Before a pipeline change is merged or a run is published:

- [ ] **Contracts:** every changed stage checks its inputs and outputs (schema, keys, domain, invariants).
- [ ] **Fallbacks:** no new silent fallback at a critical point; every fallback is configured, logged, bounded, and in the manifest.
- [ ] **Joins:** keys explicit, cardinality asserted, unmatched rows reported on both sides.
- [ ] **No double counting:** run-scoped intermediates; one naming function shared by producer and consumer, with pipeline namespace; inputs resolved from the manifest or by explicit run ID (no globbing of shared directories, no date wildcards, no "latest" heuristics); expected inputs asserted, missing keys stop the stage; one copy per logical input (no format twins); single run ID and key uniqueness asserted before every aggregation; control totals reconciled on absolute values, subsets bounded by the producer; combined quantities from the same run.
- [ ] **Ledger:** conserved totals reconciled stage by stage; every difference explained; tolerances relative.
- [ ] **Single source:** no duplicated implementations or defaults; shared steps use shared helpers.
- [ ] **State:** no in-place mutation of caller data; results independent of sourcing order.
- [ ] **Outputs:** idempotent, atomic, one schema per file, no leftovers from earlier runs.
- [ ] **Manifest:** code versions, resolved configuration, input checksums, and fallbacks recorded.
- [ ] **Pipeline lineage:** `PIPELINE_LINEAGE.md` verified against a reference run of this change: every effectively called model, repository, dataset, cache, and side-load listed with its resolved source and version; no effective input undocumented, no documented input unused; planned-vs-effective mismatches resolved.
- [ ] **Tests:** fixtures for the changed stages; a regression test for each fixed bug.
- [ ] **Scale:** deterministic chunking, results independent of the number of workers and chunk size, per-chunk seeds, bounded memory.
- [ ] **Production run:** main and variants from one production script and run ID; variant table in the manifest; resolved configurations differ from `main` only by the declared changes; separate jobs checked at assembly; no partial reruns merged into a run.
- [ ] **Releases:** versioned and immutable, citable, complete bundle with checksums; one production run per release.
