---
# Claude Code Path Trigger:
paths:
  # Analysis-project layout
  - "src/{spatial,downscaling,gridwork,harmonization}/**/*.{R,py,cpp}"
  - "scripts/**/*.{R,py}"
  - "pipelines/**/*.{R,py}"
  - "models/{downscale,nested_cut,prior}/**/*.{R,py,cpp}"
  # R package layout (estimation / downscaling engines, data hubs):
  # R sources, Rcpp/C++ solvers & samplers, raw-data prep, tests
  - "R/**/*.R"
  - "src/**/*.{cpp,cc,c,h,hpp}"
  - "inst/include/**/*.{h,hpp}"
  - "inst/**/*.{R,py}"
  - "data-raw/**/*.{R,py}"
  - "tests/testthat/**/*.R"
  # Snakemake orchestration
  - "Snakefile"
  - "workflow/Snakefile"
  - "**/*.smk"
  - "workflow/{rules,scripts}/**/*.{R,py}"
  # Vendored / submodule copies of engine packages
  - "external/**/*.{R,cpp,cc,c,h,hpp,stan,smk}"
# Antigravity Model Decision Trigger:
trigger: model_decision
description: "Spatial data processing, high-resolution downscaling, and Bayesian land-use/livestock spatial priors: multi-root data staging, nested-cut MNL trees, const-sum compositional closures, zero-sum reference coding, resampling/aggregation semantics, class crosswalks, start-map harmonization to the model base year, feasibility checks, multi-period land-use change dynamics (chaining, gross transitions, allowed transitions, dynamic covariates, unmet demand), validation and uncertainty of spatial products (reference-data accuracy assessment, multi-resolution agreement, benchmarks, allocation disagreement, hindcasts, ensembles or declared point estimates), outputs and spatial engineering (Cloud-Optimized GeoTIFF, data types and NoData, grid alignment, class tables, raster-table consistency, JSON sidecars, library versions, visual QA, R tooling), and IPF/NLOPTR optimization."
---

# Spatial Data Processing & Downscaling Rulebook

> **Scope.** This rulebook is model- and project-agnostic. It refers to an *estimation engine* (spatial priors), a *downscaling engine* (constrained allocation and harmonization), and a *data / orchestration hub*. A project rulebook, if installed (e.g. `.claude/rules/<project>-project.md`), maps these roles and the concepts below onto concrete packages, functions, and files.
>
> **Numbers are recommended defaults** (e.g. coverage screens, tolerances, stop thresholds, sample or resolution choices), unless marked as hard. Projects adapt them in their project profile, with a short reason, before results are seen. See the statistical rulebook, "How to Read the Numbers in This Rulebook".

---

## Principle 1: Multi-Root Data Staging & Storage Invariants

1. **Multi-Root Resolution Order:**
   - Raw rasters, master pixel lookup tables, and environmental covariates span across fast local SSDs, shared cluster filesystems, and cloud storages.
   - Always resolve input files via an explicit tiered search path:
     $$\text{Search Paths} = \left( \text{Local Fast Scratch}, \, \text{Project Staging Data}, \, \text{Central Data Repository} \right)$$
   - Never hardcode user-specific absolute storage paths (e.g., `/Users/.../OneDrive` or `/Users/.../GoogleDrive`). Parameterize paths via environment variables with safe defaults (e.g. `<PROJECT>_DATA_ROOTS` as an ordered search path, `<PROJECT>_HOME`).
2. **Cloud Storage Placeholder & Hydration Auditing:**
   - On synced drives (OneDrive, Google Drive, iCloud), operating systems silently de-hydrate large Parquet, RDS, and GeoTIFF files into zero-byte or sparse placeholder files.
   - **Mandatory Guard:** Before reading any spatial binary/parquet file, verify physical hydration. Do **not** test `size == allocated_bytes`: allocation is rounded up to the filesystem block size and changed by APFS/NTFS compression and sparse regions, so the equality fails on healthy files. Use, in order of reliability:
     1. **OS placeholder flags:** macOS `SF_DATALESS` (`stat -f %Xf`, or `ls -lO` shows `dataless`); Windows `FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS` / `FILE_ATTRIBUTE_OFFLINE` / `FILE_ATTRIBUTE_RECALL_ON_OPEN`.
     2. **Allocation ratio heuristic** (POSIX `st_blocks`, 512-byte units):
        $$\text{size}(F) > 0 \quad \text{AND} \quad \frac{512 \cdot \text{st\_blocks}(F)}{\text{size}(F)} \ge 0.5$$
        A placeholder has `st_blocks` $\approx 0$ while `size` reports the full logical length. (Threshold leaves room for compression; treat a failure as "possibly dehydrated", not proof.)
     3. **Format magic bytes** (definitive, but reading triggers hydration): Parquet starts and ends with `PAR1`; GeoTIFF starts with `II*\0`/`MM\0*` (or BigTIFF `II+\0`/`MM\0+`); `.rds` is gzip/xz/bzip2-compressed by default (`1f 8b`, `fd 37 7a 58 5a 00`, `BZh`).
     Catch de-hydrated placeholders proactively and either trigger hydration explicitly (e.g. `brctl download`, OneDrive "Always keep on this device") or fail with an actionable message. Never allow native C++ / Arrow readers to fail with cryptic timeout or I/O errors (`Errno 60`).
3. **Submodule & Package Loading Canonicality:**
   - When multiple checkouts of an engine package exist (e.g. a vendored submodule `external/<pkg>` vs a sibling checkout `../<pkg>`), enforce deterministic package resolution.
   - In package loaders (e.g. `devtools::load_all`), systematically prefer the explicit project-pinned dependency (`external/...`) first. Never allow a later script invocation to silently overwrite exported namespaces, and log the resolved path and git SHA of every engine package.

---

## Principle 2: Spatial Data Engineering & Covariate Harmonization

1. **Master Geometry & Coordinate Reference System (CRS) Invariants:**
   - All spatial pixels must be indexed by a permanent, non-shifting unique pixel identifier (e.g. an EEA / INSPIRE reference-grid cell code).
   - Standardize strictly on the standard European Equal Area grid (EPSG:3035 / ETRS89-LAEA) at 1 km / 5 km / 10 km resolutions.
   - Design matrices must align identically on spatial extent, resolution, and row ordering. Never join spatial variables purely on row indices; always join via the pixel identifier or composite spatial coordinates `(x, y)`.
   - Never compute areas, densities, or distances in a geographic CRS (EPSG:4326). Cell area in lat/lon grids shrinks with $\cos(\text{latitude})$ (a 5-arcminute cell is $\approx 85$ km² at 35°N but $\approx 45$ km² at 65°N).
2. **Resampling & Aggregation Semantics (variable type decides the method):**
   Every layer must be tagged with its variable type in the ingestion config, and the type alone determines the resampling method:

   | Variable type | Examples | Aggregate (fine $\to$ 1 km) | Reproject / align | Never |
   | :--- | :--- | :--- | :--- | :--- |
   | **Categorical** | CORINE (100 m), LUCAS points, soil type codes | Area-weighted **class shares** $s_{ik} = \sum_{f \in i} a_f \, \mathbb{I}(c_f = k) / \sum_{f \in i} a_f$ (e.g. `terra::segregate` + `aggregate(fun = "mean")`, or `exactextractr` coverage fractions) | Nearest-neighbour at native or finer resolution *first*, then aggregate to shares | Bilinear / cubic / average on class codes; majority (`mode`) unless a single class per pixel is explicitly intended |
   | **Intensive** | Temperature, precipitation, slope, densities, shares | Area-weighted mean over valid fine cells | `average` when coarsening; `bilinear` only for smooth fields at similar resolution | `sum`; including `NODATA` in the denominator |
   | **Extensive** | Hectares, livestock heads, population, production | **Sum** | Convert to density (per ha) $\to$ reproject as intensive $\to$ multiply by target cell area | `bilinear` / `near` on totals; averaging |

   - **Conservation assertion for extensive layers:** after every aggregation or reprojection, check $\left|\sum_{\text{out}} x - \sum_{\text{in}} x\right| \le 10^{-6} \sum_{\text{in}} x$ over the common extent, and log the result.
   - **Coverage fraction:** store the valid-data fraction per output cell for intensive aggregates; flag cells with coverage $< 0.5$ (or a documented threshold) instead of silently trusting a mean of a few fine cells.
   - **Re-closure:** after aggregating or reprojecting shares, renormalize so $\sum_k s_{ik} = 1$ on land pixels and log the maximum correction.
   - **Lat/lon sources (global climate rasters, macro-model grids such as 5-arcminute simulation units):** reproject to EPSG:3035 with area weighting (for extensive variables: per-ha density $\times$ area, or exact polygon-cell overlap weights). Never treat lat/lon cells as equal-area.
3. **Units & Scales:**
   - Canonical internal units: area in **hectares** (double precision), animals in **heads** (convert to livestock units, LSU, only with documented, versioned coefficients), shares as **fractions in $[0, 1]$** (never mix with 0–100 %).
   - Convert at the ingestion boundary only. Macro models commonly report in **1000 ha** or km²; official statistics tables often mix ha and 1000 ha; a nominal 1 km EPSG:3035 pixel is **100 ha**, but always use the actual land capacity $\text{Cap}_i$ (below), not 100 ha.
   - Carry units in column names (`area_ha`, `cattle_heads`, `cattle_lsu`, `forest_share`) and assert ranges on load: shares in $[0, 1]$, $0 \le$ area $\le \text{Cap}_i$, totals within plausible bounds of the source tables.
4. **Pixel Capacity, Region Membership & Area Reconciliation:**
   - **Pixel capacity** $\text{Cap}_i$ (ha) is the allocatable land area of pixel $i$: nominal pixel area minus sea / inland water / out-of-territory fractions, derived from a fine-resolution land mask. Coastal and border pixels have $\text{Cap}_i < 100$ ha. $\text{Cap}_i$ is the single area definition used by estimation weights, downscaling budgets, and the verification checklist.
   - **Region membership:** document one method in `MODEL_LINEAGE.md`:
     - *Fractional (preferred for NUTS3 and small regions):* split border pixels into $(i, r)$ fragments with coverage-fraction weights (`exactextractr`), $\text{Cap}_{ir} = \text{Cap}_i \cdot \text{cov}_{ir}$; the allocation unit is the fragment `pixel_id × region_id`, recombined at output.
     - *Centroid assignment:* acceptable only when border pixels are a negligible share of region area; quantify that share and log it.
   - **Geometry version:** the NUTS (or other admin) version (2013 / 2016 / 2021) must be identical for region geometries, targets, and census/statistical inputs; mixing versions is the most common cause of reconciliation failures.
   - **Area reconciliation:** compute grid area $G_r = \sum_{i \in r} \text{Cap}_{ir}$ and compare with the region land area $A_r^{\text{src}}$ implied by the target source. Rescale source-model quantities to the grid, $f_r = G_r / A_r^{\text{src}}$, preserving their class proportions (see Principle 5, item 2), and log every $f_r$. Flag $|f_r - 1| > 0.01$ (or a documented threshold) as a likely geometry, version, or unit error. Never let a reconciliation gap reach the solver, where Stage 3–4 would silently absorb it.
5. **Land-Cover Class Crosswalks:**
   - Every mapping between legends (observed land-cover products such as CORINE / LUCAS $\to$ each macro model's land-use classes, and between macro models) is a versioned crosswalk matrix $\mathbf{C} \in [0, 1]^{K_{\text{src}} \times K_{\text{tgt}}}$ stored as a file in the repository (e.g. `crosswalks/corine_to_globiom_v2.csv`), with rows summing to 1 so area is conserved: $\mathbf{s}_i^{\text{tgt}} = \mathbf{s}_i^{\text{src}} \mathbf{C}$.
   - Many-to-many splits (e.g. CORINE 2.4.x heterogeneous agriculture, 3.2.x shrub/herbaceous) need documented and cited weights; prefer spatially varying weights (e.g. from LUCAS) over a single global split.
   - Unmapped source codes raise an error; never drop them silently.
   - The **same** crosswalk version must be used for the estimation outcomes, the observed base-year land cover, and the target harmonization. Record its version and hash in lineage.
6. **Compositional Variables & Constant-Sum Closures:**
   - Land cover shares, soil textures, or focal neighborhood fractions within a pixel strictly satisfy constant-sum constraints:
     $$\sum_{k=1}^K s_{ik} = 1 \quad \text{or} \quad \sum_{k=1}^K s_{ik} = 100\%$$
   - **The Collinearity Trap:** Passing all $K$ raw components of a compositional block creates exact design matrix singularity ($\text{rank}(\mathbf{X}) < P$).
   - **Handling Protocol:**
     - Identify compositional blocks prior to estimation.
     - Apply zero-sum reference coding or drop a designated reference component during design matrix assembly.
     - When using automated constant-sum detection, the reference-rotation offset **must** be compensated back into the class-specific intercept. For block $b$ with components $k \in b$ satisfying $\sum_{k \in b} s_{ik} = C_b$ (e.g. $C_b = 1$ or $100$), the full-rank utility of class $c$ is unchanged only under
       $$\eta_{ic} = \beta_{0,c} + \sum_{k \in b} \beta_{k,c} s_{ik} = \underbrace{\left(\beta_{0,c} + C_b \, \beta_{r_b,c}\right)}_{\beta_{0,c}^{\text{compensated}}} + \sum_{k \in b,\, k \neq r_b} \left(\beta_{k,c} - \beta_{r_b,c}\right) s_{ik},$$
       where $r_b$ is the dropped reference component. Summed over blocks: $\beta_{0,c}^{\text{compensated}} = \beta_{0,c} + \sum_{b} C_b \, \beta_{r_b,c}$. For zero-sum (deviation) coding with $\bar{\beta}_{b,c} = \frac{1}{|b|}\sum_{k \in b}\beta_{k,c}$, the shift is $C_b \, \bar{\beta}_{b,c}$ and slopes become $\beta_{k,c} - \bar{\beta}_{b,c}$. If predictors were standardized first, apply this on the standardized scale *before* the $\mathcal{G}^{-1}$ back-transform (the constraint becomes $\sum_{k} \sigma_k \tilde{s}_{ik} = C_b - \sum_k \mu_k$). Failure to compensate the intercept breaks prediction invariance, causing severe distortion in downstream utility calculations.
     - Alternative: log-ratio coordinates (Aitchison). ALR and ILR ($K-1$ coordinates) remove the constraint by construction; CLR coordinates still sum to zero, so drop one (or use ILR) before estimation. All log-ratio transforms require an explicit zero-replacement policy (e.g. multiplicative replacement with a documented detection limit) logged in `MODEL_LINEAGE.md`.
7. **Focal & Neighborhood Spatial Lags:**
   - Compute moving-window focal averages (e.g., surrounding forest fraction, agricultural proximity) before estimation.
   - Differentiate strictly between real zeros (true absence of land use) and `NODATA` (water bodies, territorial boundaries, ocean pixels). Exclude `NODATA` from focal denominators to prevent edge-attenuation bias.

---

## Principle 3: Econometric Spatial Priors (Estimation Engine)

1. **Fractional-Share Outcomes (Multinomial Fractional Logit):**
   - Observed land cover per pixel is a share vector $\mathbf{y}_i = (y_{i1}, \dots, y_{iK})$, $\sum_k y_{ik} = 1$ (after the crosswalk). Never collapse pixels to their majority class: it discards information and systematically under-represents rare and mixed classes.
   - Estimate with the share-weighted multinomial (quasi-)log-likelihood
     $$\ell(\boldsymbol{\theta}) = \sum_i w_i \sum_k y_{ik} \log p_{ik}(\boldsymbol{\theta}).$$
   - **Pixel weights $w_i$ set the posterior's certainty, so choose them deliberately and document them:** default $w_i = 1$ per pixel, or $w_i = \text{Cap}_i / 100\,\text{ha}$ so partial coastal/border pixels count proportionally. Do **not** use the number of fine source cells (e.g. 100 CORINE cells per km²) as a multinomial count: neighbouring fine cells are strongly dependent, and this inflates the effective sample size and shrinks credible intervals by roughly $\sqrt{100}$.
   - **Pólya-Gamma implementation:** in the class-vs-rest conditional for class $k$, the PG draw and pseudo-response use non-integer counts, $\omega_{ik} \sim \text{PG}(w_i, \eta_{ik})$ and $\kappa_{ik} = w_i (y_{ik} - \tfrac{1}{2})$. Verify that the PG sampler supports non-integer $b$ (hybrid / saddle-point samplers do; pure Devroye-type samplers assume integer $b$).
   - Because $\ell$ is a quasi-likelihood for continuous shares, PSIS-LOO / WAIC values are comparable only between models fitted on the same shares with the same weights; validate calibration on held-out spatial blocks (statistical-modeling C.3).
   - A Dirichlet likelihood is an alternative but cannot handle exact zero shares without a zero-replacement policy; prefer the fractional MNL for land cover.
2. **Nested-Cut Multinomial Logit (Nested-Cut MNL):**
   - For complex, high-dimensional land systems (e.g. legends with $\approx 20$ land-use classes), flat MNL suffers from Independence of Irrelevant Alternatives (IIA) violations.
   - Organize land categories into hierarchical decision trees:
     $$\text{Root} \longrightarrow \{\text{Forests}, \, \text{Cropland}, \, \text{Grassland}, \, \text{Natural}, \, \text{Urban}\}$$
     $$\text{Cropland} \longrightarrow \{\text{Arable Cereal}, \, \text{Arable Oilseed}, \, \text{Permanent Orchards}, \dots\}$$
   - **The Cut-Posterior Invariant:** Estimate sub-nests conditionally using the "Cut" algorithm:
     - Each node is an independent MNL fitted on the observations reaching it; class probabilities compose multiplicatively down the tree:
       $$P_i(k) = P_i(\text{nest}(k)) \cdot P_i(k \mid \text{nest}(k)),$$
       applied recursively for deeper trees. Propagate uncertainty by combining draws of each node's posterior (independently drawn per node).
     - No information flows bottom-up: noisy, rare micro-classes cannot feed back into primary macro-class allocations (e.g., rare vineyard distributions must not distort the global forest-vs-cropland allocation).
     - Consequently, the cut model estimates **no inclusive-value / dissimilarity parameter**: upper-level utilities do not contain $\lambda_m \log \sum_{k \in m} e^{V_k/\lambda_m}$. Do not attach a $\lambda$ to the cut tree. If a genuine nested logit with inclusive values is required, estimate it jointly (not cut) and constrain $\lambda_m \in (0, 1]$ for consistency with random-utility maximization.
     - Within every node, identify utilities with a reference class (or zero-sum constraint across the node's alternatives).
     - **Share data at each node:** node $m$ is fitted on the within-nest conditional shares $y_{ik}^{(m)} = y_{ik} / y_{im}$, $y_{im} = \sum_{j \in m} y_{ij}$, with weights $w_i^{(m)} = w_i \, y_{im}$; pixels with $y_{im} = 0$ drop out of that node. With these weights the share likelihood factorizes exactly, $\sum_k y_{ik} \log p_{ik} = \sum_m y_{im} \log P_i(m) + \sum_m y_{im} \sum_{k \in m} y_{ik}^{(m)} \log P_i(k \mid m)$. If each node has its own parameters, the tree therefore gives the same posterior as the joint tree model; the "cut" only changes results for parameters shared across nodes (e.g. common RE variances or a global shrinkage scale), which must not receive feedback from lower nodes. Using unweighted conditional shares at a node is wrong: it gives pixels with a trace of the nest the same weight as pixels fully inside it.
3. **Spatial Hierarchies & Random Effects (RE):**
   - Spatial unobserved heterogeneity (administrative policies, localized micro-climates) must be modeled hierarchically (e.g., NUTS2, NUTS3, or country-level random intercepts $\mathbf{u}_g$).
   - **Avoid RE-Horseshoe Aliasing:**
     - When global slopes $\boldsymbol{\mu}$ carry horseshoe sparsity priors while random effects $\mathbf{u}_g$ carry unpenalized variances, common spatial effects can artificially migrate entirely out of $\boldsymbol{\mu}$ into the empirical mean of $\mathbf{u}_g$.
     - Enforce centered random effects ($\sum_g \mathbf{u}_g = \mathbf{0}$) or recenter within each sampling sweep to guarantee that global covariate signals remain fully identified.
     - Recentering is only valid if it leaves every linear predictor unchanged: in the same step move the removed mean into the matching global term, $\bar{\mathbf{u}} = \frac{1}{G}\sum_g \mathbf{u}_g$, $\mathbf{u}_g \leftarrow \mathbf{u}_g - \bar{\mathbf{u}}$, $\boldsymbol{\mu} \leftarrow \boldsymbol{\mu} + \bar{\mathbf{u}}$ (for random intercepts: the class intercept). Recentering without compensation changes the target posterior (see statistical-modeling B.1).
4. **Livestock & Density Count Models:**
   - Model stocking rates and animal counts (Bovine, Sheep, Goats, Swine) using overdispersed hurdle Poisson or negative binomial models.
   - Include spatial pasture/cropland capacity as an explicit exposure offset ($E_i$), ensuring estimated intensities correspond to stocking densities rather than absolute land mass:
     $$\mathbb{E}[Y_i \mid \mathbf{X}_i] = E_i \cdot \exp(\mathbf{X}_i^\top \boldsymbol{\beta} + u_{g[i]}) = \exp(\log E_i + \mathbf{X}_i^\top \boldsymbol{\beta} + u_{g[i]})$$
     The offset enters the linear predictor as $\log E_i$ with its coefficient fixed at 1 (never as a free covariate, never unlogged). Pixels with $E_i = 0$ have $\log E_i = -\infty$ and structurally zero expected counts: exclude them from the likelihood (logging the drop in $\mathcal{S}$) and assign zero in prediction; flag any $E_i = 0$ pixel with observed $Y_i > 0$ as a data inconsistency. For hurdle models, decide and document whether $\log E_i$ enters the zero part, the positive-count part, or both.

---

## Principle 4: Computational MCMC Execution & Fault Tolerance

1. **Streaming Posterior Serialization:**
   - In high-resolution spatial sampling ($N > 60{,}000$ pixels, $P > 50$ covariates, $K \ge 19$ classes), storing all MCMC draws in RAM causes immediate Out-Of-Memory (OOM) fatal crashes.
   - Stream parameter draws to batch files on disk (e.g. `posterior_batch_*.qs` or `.rds`) at fixed iteration intervals. Discard large intermediate latent utility matrices from memory immediately after sufficient statistics update.
2. **Kill-Safe Resumable Node Stores:**
   - Multi-chain nested tree estimations take significant wall-clock time (often 12–48 hours).
   - Implement deterministic, config-hashed node stores (e.g. `<store_dir>/nodes/<node_path>.rds`).
   - Fingerprint each node's cache with a hash of: input data checksum, covariate selection, tree topology, chain iterations, random seed, **and code version**: the estimation engine's git commit SHA (plus a dirty-tree flag/diff hash) and the R/Rcpp and key package versions (`sessionInfo()` or `renv.lock` hash). Without the code fingerprint, a sampler fix silently reuses stale cached nodes.
   - Upon process interruption or preemption, re-launching must automatically load completed leaves/nests, executing only incomplete sub-models.
3. **Zero-Tolerance Process Termination:**
   - Never issue broad, untargeted termination commands (e.g., `pkill -f R` or `pkill -f python`) on shared compute nodes or workstation environments. Target solely the specific Child Process IDs (PIDs) initiated by the active run.

---

## Principle 5: The Downscaling Engine (Solver Architecture)

1. **Constrained Spatial Allocation Pipeline:**
   - Downscaling translates coarse regional targets (e.g., NUTS2 total cropland hectares) into pixel-level simulated land uses while respecting:
     - **Priors:** Pixel-level probability vectors $\mathbf{P}_i = (p_{i1}, \dots, p_{iK})^\top$ from the estimation engine.
     - **Initial Land Cover:** Existing baseline areas $A_{ik}^{(0)}$.
     - **Hard Physical Restrictions:** Slope limits, protected areas, water masks ($R_{ik} \in \{0, 1\}$), generalized to upper bounds $0 \le A_{ik}^{(t)} \le U_{ik} = R_{ik} \cdot \text{Cap}_i$ (or a fractional bound for partial restrictions).
     - **Pixel Closure (equality, not $\le$):** $\sum_{k=1}^K A_{ik}^{(t)} = \text{Cap}_i$ (Principle 2, item 4). Land not claimed by any modelled class goes to an explicit residual class (e.g. `OTHER` / other natural land) with $U_{i,\text{OTHER}} = \text{Cap}_i$, so closure is always attainable and Stage 4's exact-conservation guarantee is well-defined.
     - **Macro Targets:** $\sum_{i \in \text{Region}_r} A_{ik}^{(t)} = \text{Target}_{rk}^{(t)}$, with $\sum_k \text{Target}_{rk}^{(t)} = G_r$ (the region's grid capacity, including the residual class).
     - **Outside-Model Area (exogenous `_other` classes):** land the macro model does not cover keeps its observed start-map land cover and is never dropped, so the outputs have no gaps. This covers pixels outside the model's spatial domain, zones the model assigns no area, and whole regions the model does not represent separately (e.g. city states merged into or absent from the model's regions).
       - **Relabel** such land in the start map from its endogenous class to the exogenous counterpart of its *primary* class, `<primary>_other`. Subclasses map through their primary class (e.g. `Cropland_arable_wheat` $\to$ `Cropland_other`, short-rotation plantations $\to$ `Forests_other`). Do this before harmonization, so that endogenous classes describe only model-covered land.
       - **`_other` classes are exogenous:** they receive no targets, no transition priors, and no restriction rules, and they are excluded from harmonization fidelity checks. The solver carries them unchanged as stay rows in every period, and they count towards pixel closure.
       - **Whole regions without targets** must still be written to the outputs, with their start-map land cover in every period of every scenario (a static region, flagged as such). A region that is skipped because it has no targets must not disappear from the land-use outputs or the rasters.
2. **Start-Map Harmonization (Base-Year Level Matching):**
   The observed land-cover map never equals the macro model's base-year land use. Before any projection period is downscaled, the observed start map is **harmonized**: it is transformed, with as little change as possible, into a map whose zone-by-class areas match the model's base year. Projection targets then follow directly from the model's changes. The harmonized map replaces the observed map as $A^{(0)}$.

   **Notation.** Harmonization zones $z$ are the model's finest spatial units intersected with the grid (e.g. country $\times$ simulation-unit strata, or administrative regions). $S_{zk}$ is the start-map area of class $k$ in $z$ after the crosswalk, $M_{zk}^{(t_0)}$ the model's base-year area, and $b$ the designated balancing class.

   **Procedure (all pipelines must implement the same steps):**
   1. *Compile the start map.* Select the observed product for the base year $t_0$, apply the legend crosswalk (Principle 2, item 5), split classes by protection status from protected-area shares **at fine resolution, before thematic aggregation**, and assign fine pixels to zones with area weights normalized per pixel (fractional membership, Principle 2, item 4). Aggregate to the downscaling resolution, with an area-conservation check after every step.
   2. *Keep the overlap.* The area common to both maps, $D_{zk} = \min(S_{zk}, M_{zk}^{(t_0)})$, stays unchanged (diagonal). Only the residuals $S_{zk} - D_{zk}$ (surplus classes) and $M_{zk}^{(t_0)} - D_{zk}$ (deficit classes) are reallocated.
   3. *Balance zone totals.* When $\sum_k S_{zk} \ne \sum_k M_{zk}^{(t_0)}$, route the difference through the balancing class $b$ (e.g. unmanaged natural land, or an explicit `NODATA` / outside-model class).
   4. *Estimate a transition prior* $\Pi_{kl}$ (preference for start class $k$ becoming model class $l$). For example, regress model areas on start-map areas across zones with non-negative least squares, row-normalize, add a small smoothing $\varepsilon$, and up-weight legend-consistent paths (coarse $\to$ fine classes of the same hierarchy).
   5. *Solve gross transitions per zone* by bi-proportional fitting (Fienberg / IPF) of $\Pi$ to the residual margins (surplus classes $\to$ deficit classes).
   6. *Allocate flows spatially.* Convert zone flows to per-start-class shares and allocate them to pixels by IPF on a spatial seed. For example, a gravity seed: the smoothed neighbourhood density of the destination class plus a floor, so pixels near existing destination land receive more of the flow. The allocation must conserve pixel totals and zone flow budgets.
   7. *Numeric hygiene.* Snap values to a fixed quantum (e.g. 0.01 km²), consolidating fractional tails into pixels where the class already exists. Protect classes that are below the quantum everywhere, and rescue pixels that would otherwise vanish.
   8. *Validate and close.* Compare harmonized zone-class areas with $M_{zk}^{(t_0)}$ before and after hygiene. Relabel zones the model does not cover (zero target area) to the exogenous `<primary>_other` classes (item 1, Outside-Model Area). Merge back pixels outside the model's domain, and keep zones skipped by the coverage screening unharmonized rather than dropping them.

   **Invariants & watch-outs:**
   - **Explicit base year and scenario.**
     - A year fallback (closest available start-map year, earliest model year) is acceptable when the chosen year and the gap $|t_{\text{map}} - t_0|$ are logged; bound the gap where possible.
     - Scenario names match exactly, or by a partial match (e.g. a run prefix) only when it is **unique**. Several candidates stop the run; they are never resolved by row order.
   - **Skipped zones are a reported result, not a silent default.** Zones excluded from harmonization (e.g. start-map area $< 95\%$ of model area, or missing from the targets) pass through unharmonized. List them with their area gaps, mark them in the output, and carry them into the feasibility pre-check (item 3): later periods in those zones start from a map that does not match the model.
   - **One balancing policy across all pipelines.** Put the imbalance on one side only (as extra target area for $b$ when the start map has surplus; as reduced target area for $b$ when it has a deficit). Never add area to the *start* side of a class that has no pixels, because the spatial allocator can only move area that exists in pixels. If $b$ cannot absorb a deficit, flag the zone as infeasible. Do not let the gross solver rescale all targets proportionally to the start-side total, which makes every class in the zone miss its target.
   - **Margins are checked, not assumed.** After step 5, assert that the to-margins match $M_{zk}^{(t_0)}$ within the target-hit tolerance, and record the solver's stop reason (converged vs. iteration cap). After step 6, treat a worst relative flow-budget deviation above tolerance as a failure, not an informational message.
   - **Net-flow invariant.** If gross flows are netted ($N_{kl} = \max(F_{kl} - F_{lk}, 0)$) before converting to shares, the result is only exact while surplus and deficit classes are disjoint. That holds after step 2, unless the balancing step puts $b$ on both sides. Assert per zone and start class that $\sum_l N_{kl}$ equals the start-map area of $k$. Otherwise, normalizing to shares silently inflates the off-diagonal flows.
   - **Prior semantics.** Smoothing $\varepsilon$ on every cell permits every transition, including implausible ones (e.g. artificial surfaces $\to$ cropland). Encode impossible transitions as **structural zeros**, which bi-proportional fitting preserves, and then rely on the feasibility check. NNLS on raw areas is dominated by large zones and gives unstable coefficients for collinear start classes, so weight or normalize zones, and save $\Pi$ as a run artifact. All pipelines must use the same prior construction, including hierarchy bonuses.
   - **Hygiene tolerances.** Quantum snapping conserves zone totals only to within one quantum per zone, and moves pixel-class values by up to one quantum. Run the pixel-closure check with a tolerance of at least the quantum, or re-close pixels afterwards, and log the total area moved.
   - **Two-tier area checks.** Some rounding noise is expected in compilation and harmonization (rounded zone aggregates, millions of summed rows).
     - Small discrepancies are reported (message or warning).
     - The run stops only on an **excessive** discrepancy, measured relatively: $|\Delta| > \epsilon_{\text{stop}} \cdot \text{total}$, with $\epsilon_{\text{stop}}$ configurable (e.g. 1%).
     - Compare against the right reference total.
   - **One canonical implementation.** The harmonization and allocation functions live in the downscaling engine package. A project-level redefinition that overrides the package version depending on sourcing order breaks Principle 1, item 3: the implementation that actually ran must be unambiguous and recorded in lineage.
   - **Report how much the map was changed.** Save the harmonization deviation (harmonized minus observed start map) as a QA layer, and report the total area moved and its share of zone area. Large deviations point to crosswalk, geometry, or base-year problems, not to land-use change.

   **Projection targets after harmonization.** Period targets add the model's change to the harmonized start map:
   $$\text{Target}_{zk}^{(t)} = A_{zk}^{(0)} + \Delta M_{zk}^{(t)}, \qquad \Delta M_{zk}^{(t)} = M_{zk}^{(t)} - M_{zk}^{(t_0)}.$$
   - In harmonized zones this reproduces the model's levels for every class except the balancing class $b$, which keeps carrying the base-year area gap. Do not replace this with the model's raw levels $M_{zk}^{(t)}$, which would reintroduce that gap.
   - Passthrough zones and non-harmonized classes use the same formula on the unharmonized map. A negative result is clipped to 0 and the deficit redistributed over the zone's other changing classes, keeping $\sum_k \text{Target}_{zk}^{(t)} = G_z$. Log every clip.
   - Scale $\Delta M$ by the reconciliation factor $f_z$ only as a documented choice. If $|f_z - 1|$ is large, fix the geometry, version, or crosswalk instead.
   - Model units are mapped to zones with area weights (Principle 2, item 2) *before* differencing. The model's zone total must stay fixed over time, so $\sum_k \Delta M_{zk}^{(t)} = 0$; any drift is assigned to the residual class and logged.
3. **Feasibility Pre-Check (before any solver stage):**
   For each region $r$ and period $t$, verify that the targets are attainable under the restrictions *before* running IPF:
   - Necessary checks: $\text{Target}_{rk}^{(t)} \le \sum_{i \in r} U_{ik}$ for every $k$; $\sum_k \text{Target}_{rk}^{(t)} = G_r$; $\sum_k U_{ik} \ge \text{Cap}_i$ for every pixel (guaranteed by the residual class).
   - Definitive check: the allocation is a transportation problem (pixel supplies $\text{Cap}_i$, class demands $\text{Target}_{rk}$, arc capacities $U_{ik}$), and it is feasible iff the max flow equals $G_r$. Solve it per region with an LP or max-flow solver (e.g. HiGHS, `lpSolve`, `igraph::max_flow`); this is cheap compared to the solver stages.
   - **On infeasibility:** stop, or apply an explicit, documented policy in a fixed priority order (e.g. reduce the infeasible target and move the excess to the residual class; or relax a soft restriction). Log the region, class, shortfall, and the action taken. Never pass an infeasible region to the solver, where Stages 3–4 would hide the problem.
4. **Multi-Stage Solver Execution Order:**
   Downscaling must proceed through a robust four-tier convergence sequence:
   $$\text{Stage 1 (IPF)} \longrightarrow \text{Stage 2 (Multi-NLOPTR)} \longrightarrow \text{Stage 3 (Sequential Boost)} \longrightarrow \text{Stage 4 (Mop-Up)}$$
   - **Target-hit criterion (used by every stage and the final checklist):** a region-class target is met when its relative error is below $\epsilon_{\text{rel}}$, with an absolute floor $\epsilon_{\text{abs}}$ for tiny targets:
     $$\left| \sum_{i \in r} A_{ik} - \text{Target}_{rk} \right| \le \max\left(\epsilon_{\text{rel}} \cdot \text{Target}_{rk}, \; \epsilon_{\text{abs}}\right), \qquad \text{e.g. } \epsilon_{\text{rel}} = 10^{-8}, \; \epsilon_{\text{abs}} = 10^{-6}\,\text{ha}.$$
   - **Stage 1: Generalized Iterative Proportional Fitting (IPF):**
     Rapid cross-entropy scaling matching marginal row/column targets. Resolves $>90\%$ of well-conditioned regions.
   - **Stage 2: Multi-Target NLOPTR Optimization:**
     If IPF fails the target-hit criterion, invoke derivative-free non-linear optimization (`NLOPT_LN_BOBYQA` or `COBYLA`) on log-scale multiplier vectors. Note that nloptr's `xtol_rel` / `ftol_rel` are stopping rules on the parameter step / objective change, **not** target-hit tolerances: an optimizer can stop "converged" while targets are still missed. Always re-check the target-hit criterion after the solver returns and set `xtol_rel` (e.g. `1e-10`) only as a stopping rule.
   - **Stage 3: Sequential Single-Target Boost:**
     Isolate specifically failing land categories or stubborn regional sub-targets and optimize them sequentially with localized penalty lambdas.
   - **Stage 4: Residual Mop-Up Layer:**
     Allocate floating-point rounding residuals strictly to dynamically unconstrained pixels (those with $A_{ik} < U_{ik}$), guaranteeing exact integer or double-precision conservation of land mass across every administrative zone. Mop-up is for rounding residuals only: if the residual to allocate exceeds the target-hit tolerance, the region failed earlier and must be reported, not mopped up.
5. **Numerical Safety & Numerical Limits:**
   - Compute MNL probabilities with the log-sum-exp / softmax shift, never with raw `exp()` on utilities:
     $$p_{ik} = \exp\left(V_{ik} - m_i - \log\textstyle\sum_{j} \exp(V_{ij} - m_i)\right), \qquad m_i = \max_j V_{ij}.$$
     This cannot overflow and makes ad-hoc exponent clamps unnecessary. Keep exponent bounds and probability floors as separate, clearly named constants on their own scales: e.g. `LOG_P_MIN = log(1e-12)` (floor applied to log-probabilities before IPF/log-multiplier steps, so no prior is exactly zero) vs. any cap on utilities, which must stay below `log(.Machine$double.xmax)` $\approx 709.78$. Renormalize after flooring so $\sum_k p_{ik} = 1$.
   - Apply strict positive thresholds on area transitions to prevent negative land allocations.

---

## Principle 6: Land-Use Change Dynamics (Multi-Period Downscaling)

Downscaling a projection is a **chain** of periods $t_0 < t_1 < \dots < t_T$, not a set of independent snapshots. Each period starts from the map the previous period produced.

1. **Explicit, Ordered Time Axis:**
   - Every scenario carries an explicit ordered vector of periods (base year first). The predecessor of $t_j$ is $t_{j-1}$ in that vector.
   - Never construct lags as `year - step` with a step taken from the first two periods, or with a hard-coded step. Macro-model reporting years are often irregular (e.g. 2020, 2025, 2030, 2050), and a wrong lag join silently pairs mismatched periods or drops them.
   - Keep the period length $\Delta t_j = t_j - t_{j-1}$ as data. Anything expressed as a rate (annual conversion rates, inertia, growth of restricted areas) must be scaled by $\Delta t_j$.
   - Scenarios never share a chain: each scenario runs its own chain from the same harmonized $A^{(0)}$.
2. **Path Dependence (Chaining Invariant):**
   - The start state of period $t_j$ is the **downscaled pixel map** of $t_{j-1}$, aggregated over destination classes: $A_{ik}^{(t_{j-1})} = \sum_{k'} X_{i,k' \to k}^{(t_{j-1})}$. Never restart from the base map, and never substitute the model's or the observed map for an intermediate period.
   - The update must preserve pixel closure (Principle 5, item 1), including the residual class. Negative values may be clipped only within the numerical tolerance; anything larger is an error, not a clip.
   - **Chain fingerprint:** every period's checkpoint is hashed together with its predecessor's hash (inputs, targets, priors, restrictions, code version). A changed earlier period invalidates all later ones, so a resumed run never mixes periods from different chains.
3. **Transition Targets (Gross from Net):**
   Macro models usually report net levels per region and period. Downscaling needs region-level **gross transition matrices** $\mathbf{T}_r^{(t)} \in \mathbb{R}_{\ge 0}^{K \times K}$ with
   $$\sum_l T_{r,kl}^{(t)} = L_{rk}^{(t_{j-1})}, \qquad \sum_k T_{r,kl}^{(t)} = L_{rl}^{(t)},$$
   built with the same machinery as start-map harmonization (Principle 5, item 2): overlap retention ($T_{r,kk} \ge \min(L_{rk}^{(t_{j-1})}, L_{rk}^{(t)})$), one balancing class, a transition prior with structural zeros, and bi-proportional fitting.
   - **Model–map mismatch is reported, not resolved by default.**
     - Transition targets are built from the macro model's levels. When the achieved map differs (an earlier period was under-allocated, or a zone passed through unharmonized), the mismatch is real: the downscaling has no authority to resolve it by default.
     - It must be visible: report unmet demand per period (item 7) and the cumulative gap to the model per region and class.
     - Rebuilding a period's transitions from the achieved totals, or carrying the gap forward, are **optional strategies**, enabled explicitly and recorded in lineage.
   - **Gross flows are under-identified.** Net changes fix only the margins, and the prior decides how much gross turnover is assumed. Report the gross-to-net ratio $\sum_{k \ne l} T_{kl} \,/\, \tfrac{1}{2}\sum_k |L_k^{(t)} - L_k^{(t_{j-1})}|$ per region and period. Unless observed transition data justify more, prefer minimal gross turnover (ratio near 1).
   - **Thresholding breaks margins.** Dropping small flows after fitting (e.g. $T_{kl} < 10^{-3}$ km²) changes both margins. Either snap with a conserving quantum method (Principle 5, item 2, step 7) or re-fit after thresholding, and log the area dropped.
4. **Allowed Transitions & Restrictions:**
   - Maintain a versioned **allowed-transition matrix** $Z \in \{0,1\}^{K \times K}$ per legend. It sets structural zeros in transition priors, in target construction, and in the solver. Typical zeros: outflows from irreversible classes (artificial surfaces), direct transitions the legend makes impossible, and transitions into classes a scenario excludes.
   - **Chronological validity:** a class with zero area in a region at $t_{j-1}$ cannot be a from-class there at $t_j$. Check every period before solving. A target from a non-existent class is an error in target construction, not a "solver residual".
   - **Pixel restrictions are time-indexed** ($R_{ik}^{(t)}$, e.g. protected-area expansion, sea-level or urban growth masks) and use the same scenario as the targets.
   - **Transition-specific rules** (e.g. no deforestation inside protected areas while afforestation is allowed there) need restrictions on the pair $k \to l$, not only on the destination class $l$. If the engine supports only destination restrictions, encode the pair rule in the targets and priors, and verify it after solving.
   - **Optional residence-time rules** (e.g. minimum rotation before plantation harvest, abandonment before reforestation) require tracking class age per pixel. Document them as such rather than approximating them with static restrictions.
5. **Persistence & Flicker:**
   - Land use is persistent. Pixel-level priors should make staying more likely than switching, and the solver should not create gross pixel turnover beyond what the region-level transitions require.
   - Report the **reversal rate**: the area of pixel-class cells that switch $k \to l$ in one period and $l \to k$ in a later one, per region and scenario. High reversal ("flicker") means the priors or targets create unnecessary gross change.
6. **Dynamic & Projected Covariates:**
   - **Land-use-derived covariates** (own-class shares, neighbourhood shares, distance to the nearest pixel of a class) are recomputed every period from the downscaled $A^{(t_{j-1})}$ with the **same** definition, kernel, and `NODATA` handling as in estimation. They are never computed from the target period, the macro model, or a future observed map. In estimation, the matching variables enter lagged by one period, $x_{i}^{(t-1)}$, or the model is specified as a spatial-lag model (statistical-modeling B.3); otherwise they create a feedback loop with the outcome.
   - **Exogenous projected covariates** (climate, population, accessibility) use the projection that matches the scenario and period of the targets. Document how projection epochs map to periods (e.g. a 2041–2070 climate mean used for 2050).
   - If the priors are deliberately held static over time (estimated and applied with base-year covariates only), state this explicitly in lineage: spatial patterns then ignore projected climate and socio-economic change. Loading projected covariates and not passing them to the solver is a silent version of this choice and is not allowed.
   - **Extrapolation check:** for each period, flag pixels whose covariates fall outside the estimation range. Use a per-covariate envelope check or a multivariate similarity measure (e.g. MESS). Report the share of affected area and decide explicitly whether to clamp covariates, accept the extrapolation, or exclude the pixels from prior updates.
7. **Unmet Demand & Rebalancing:**
   - When the solver caps targets (restrictions block area, or targets exceed available area), it must record the **unmet demand** per region, transition, and period. A warning alone is not enough.
   - Choose and document one policy: drop it (and report the cumulative shortfall against the macro model), carry it forward (add it to the next period's targets), or reroute it (to another class or region by a documented rule). Proportional down-scaling of all targets is a valid policy only if it is the documented one.
   - **Dynamic feasibility pre-check:** before solving any period, check cumulatively over the whole chain that each region's outflow demand from class $k$ never exceeds the area of $k$ available at that point: $\sum_{l \ne k} T_{r,kl}^{(t_j)} \le L_{rk}^{(t_0)} + \sum_{0 < m < j} \Delta L_{rk}^{(t_m)}$, where $\Delta L_{rk}^{(t_m)}$ is the net change of $k$ in period $t_m$. Treat violations like the single-period feasibility pre-check (Principle 5, item 3): stop, or apply the documented policy, never pass them silently to the solver.
8. **Scenario & Version Consistency:**
   - Scenario identifiers are matched exactly across targets, projected covariates, restrictions, and priors (no substring matching). Each output carries its scenario identifier.
   - The same versions apply in every period: admin geometry, legend and crosswalk, grid, and restriction layers. A version change mid-chain (e.g. new administrative boundaries from 2030 on) requires an explicit remapping step, logged in lineage.

---

## Principle 7: Validation & Uncertainty of Spatial Products

The checks in Principles 5–6 establish **internal consistency**: targets are hit, pixels close, and restrictions hold. They do not show that land use is allocated to the right places, or how certain each pixel is. This principle covers the assessment of the spatial **products**, however they were made.

How well a fitted model predicts (spatial-block cross-validation, predictive checks, benchmark models) is covered by the statistical rulebook (C.3). Propagating parameter uncertainty is covered there too (D.3). This principle adds what is specific to maps.

1. **Accuracy Assessment Against Independent Reference Data:**
   - **Independence:** reference data must not have been used to train, calibrate, or harmonize the product. They must not be derived from the same source either, e.g. a land-cover product validated against its own input layer.
   - **Probability sampling design:** sample reference locations with a documented design, typically stratified by mapped class and/or region, with a sample size per stratum that supports the accuracy targets. Reference labels come from a source of higher quality (field survey, photo-interpretation) under a documented protocol, in the same legend via the crosswalk (Principle 2, item 5).
   - **Error matrix and area estimates:**
     - report the error matrix weighted by stratum area;
     - report user's and producer's accuracy (commission and omission errors) per class, and overall accuracy;
     - report **error-adjusted area estimates with confidence intervals** (stratified estimator, e.g. following Olofsson et al. 2014).

     When map accuracy is known, the class areas of a map (pixel counts) are not area estimates by themselves.
   - **Fractional outputs (shares):** use soft agreement per pixel, $\sum_k \min(a_{ik}, o_{ik})$ for predicted shares $a$ and observed shares $o$. Report errors in share points per class.
   - **Convenience reference data** (existing points that do not come from a probability design) support only an indicative assessment. Declare it as such, and do not use it for area estimation.
2. **Agreement Across Resolutions:** compute agreement at the native resolution and at coarser aggregations, e.g. 1, 5, 10 and 25 km and administrative regions. Report the agreement curve, and state the finest resolution at which the product is fit for use. Errors at the pixel level often vanish at coarser scales, and a product should not be used at resolutions where it has no demonstrated skill.
3. **Assessing Change:**
   - Measure change from products that are **consistent across dates**: same legend, version, and method, preferably dedicated change layers. Differencing two independently produced status maps creates spurious change from classification noise.
   - Change is rare. Validate it with a reference sample stratified on mapped change, and report change accuracy separately from the accuracy of the status maps.
4. **Spatial Benchmarks:** compare every spatial product with simple baselines that fit the product:
   - persistence (the previous map);
   - nearest-neighbour or plain interpolation;
   - spreading the regional value uniformly over the region's pixels, or in proportion to an obvious covariate.

   Report the skill relative to the best baseline (statistical rulebook, C.3).
5. **Spatial Uncertainty:**
   - **Every product states its uncertainty.** Either distribute uncertainty layers with it (per-pixel intervals, class probabilities, probabilities of change), or declare that it is a point estimate, which summary it is (statistical rulebook, D.3), and which sources of uncertainty are not quantified. A declared point estimate is the default; ensembles are optional.
   - **Aggregating uncertainty needs spatial dependence.** The variance of a regional total is $\text{Var}\left(\sum_i x_i\right) = \sum_i \sum_j \text{Cov}(x_i, x_j)$. Summing pixel variances as if pixels were independent understates regional uncertainty whenever errors are spatially correlated. With ensembles, aggregate within each member first, then summarize across members.
   - **Process ensemble members, not their summaries.** Resampling, aggregation, and masking are applied to each member. Quantile maps cannot be added, averaged, or resampled to give the quantiles of the result.
   - **Sensitivity to processing choices:** vary the key processing and tuning parameters (resolution, neighbourhood or kernel sizes, thresholds, smoothing or seed parameters, crosswalk splits) one at a time or in a small design. Report which outputs change and by how much, and flag conclusions that depend on the setting.
6. **Downscaling With Regional Targets:**
   Downscaling fixes the regional totals, so validation focuses on *where* the land use goes within each region.
   - **Quantity vs. allocation disagreement** (Pontius & Millones 2011): with targets met, quantity disagreement is close to zero at the region level by construction. **Allocation disagreement** is the informative part. Report it per region and class, and across resolutions (item 2).
   - **Figure of merit for change:** $\text{FoM} = B / (A + B + C + D)$, with misses $A$, hits $B$, wrong hits $C$ (change observed and predicted, but to the wrong class), and false alarms $D$. For fractional outputs, compute the components from share overlaps. Because change is rare, overall agreement is dominated by persistence; FoM measures the skill on change.
   - **Hindcast with observed targets:** run the full pipeline over a past period, starting from the observed map at $t_a$ with the **observed** regional changes as targets, and compare with the observed map at $t_b$ (consistent products, item 3). This isolates the skill of the allocation from errors of the macro model. A second hindcast with model-produced targets shows the combined error.
   - **Allocation benchmarks:**
     - uniform allocation within each region;
     - allocation in proportion to current class shares;
     - allocation by the priors alone, without the solver.

     Note that persistence has no change to score. The downscaling adds skill only where it beats these.
   - **Ensembles (optional):** each member downscales one posterior draw of the priors through the full chain (Principle 6), and meets the targets in every member. Outputs are per-pixel quantiles of class areas, probabilities of change, and agreement maps. With the default declared point estimate, the priors are the posterior mean probabilities (statistical rulebook, D.3).

---

## Principle 8: Outputs & Spatial Engineering

Outputs are what others use, often without access to the code. Every file must be readable, correctly placed, and interpretable on its own.

1. **Raster Format: Cloud-Optimized GeoTIFF (COG):**
   - Write rasters as COG: internal tiling, compression, and overviews, so they can be read partially and over the network. In R:
     ```r
     terra::writeRaster(r, "out.tif", filetype = "COG", datatype = "FLT4S", NAflag = -9999,
                        gdal = c("COMPRESS=DEFLATE", "PREDICTOR=YES", "BLOCKSIZE=512",
                                 "OVERVIEW_RESAMPLING=AVERAGE"))
     ```
   - **Overview resampling follows the variable type** (Principle 2, item 2): `AVERAGE` for shares and continuous fields, `NEAREST` or `MODE` for categorical class maps. Overviews are for display only. Analytical aggregation (e.g. summing areas to coarser cells or regions) is always computed from the full-resolution data, never read from overviews.
   - **Validate the layout** after writing: `gdalinfo` reports `LAYOUT=COG`, or use GDAL's COG validation script.
   - **One layer per band, named:** band names state the class or variable (`names(r) <- classes`), with scenario and period in the file name and the sidecar.
2. **Data Types, Precision, and NoData:**
   - **Choose the data type deliberately:** `FLT4S` (float32) for shares and areas per pixel, and unsigned integers (`INT1U`, `INT2U`) for categorical class maps with a class table.
   - **Accumulate in double precision.** Totals, conservation checks, and regional aggregates are computed in float64 from the stored values. Summing millions of float32 values loses precision.
   - **One NoData value per product** (e.g. `-9999` for floats, an unused code for integers), written to the file and to the sidecar.
   - **Distinct output states:** NoData means outside the grid, land mask, or territory. `0` means true absence. Land outside the macro model's coverage is shown by its exogenous `_other` classes (Principle 5, item 1), never as NoData. Flags such as static regions are a separate band or file, not a special value hidden inside a data band.
3. **Units and Value Semantics:** every band states whether it holds a **share** (fraction of the pixel, $[0, 1]$) or an **area** (km² or ha per pixel), and its units. Mixing shares and areas between bands or files of one product is not allowed.
4. **Grid Alignment:**
   - Every product declares its reference grid (CRS, origin, resolution, extent), and snaps to it exactly. Check against the template with `terra::compareGeom()`; never resample implicitly on write.
   - Keep a documented mapping from the internal pixel identifier to the official reference-grid cell codes (e.g. INSPIRE/EEA codes), so external users can join without spatial operations.
5. **Legends and Class Tables:**
   - Every class code or band name in an output exists in a class table distributed with it (e.g. `classes.csv`): code, name, primary class, endogenous or exogenous, units, colour.
   - For categorical rasters, also embed the categories and colours (`levels()`, `coltab()` in `terra`).
   - **Check completeness when writing:** a class present in the data but missing from the class table stops the export. This catches new classes, such as `_other` classes, that were never added to the legends.
6. **Rasters Match Tables:** when a product exists both as a table and as rasters, the rasters aggregated to regions (`exactextractr::exact_extract()` or `terra::zonal()`) must reproduce the tabular totals within a relative tolerance. Check this as part of writing the rasters, and log the largest deviation.
7. **JSON Sidecar per File:** every output file has a sidecar `<file>.json` next to it, validated against a JSON schema kept in the repository. Required fields:

   | Group | Fields |
   | :--- | :--- |
   | Identity | product name, version, file name, file checksum, creation time |
   | Content | scenario, period, bands (name, class or variable, share/area, units, data type) |
   | Spatial | CRS (EPSG code and WKT), reference grid (origin, resolution, extent), NoData value and its meaning |
   | Legend | class table file and its checksum |
   | Lineage | run ID, pointer to the run manifest, code versions, configuration hash, input dataset versions |
   | Uncertainty | ensemble size and summaries, or the declared point estimate and the sources not quantified (Principle 7, item 5) |
   | Validation | pointer to the validation report and its headline measures (Principle 7) |
   | Software | R and package versions; GDAL, PROJ, and GEOS versions |
   | Terms | licence, citation, DOI |
8. **Geospatial Library Versions:** coordinate transformations and some raster operations depend on GDAL, PROJ, and GEOS versions and on the PROJ transformation grids available. Record the versions (`terra::gdal(lib = "all")`, `sf::sf_extSoftVersion()`) in the sidecar and the run manifest. Pin them in the environment where results must be reproducible.
9. **Visual QA Before Release:** generate quick-look maps automatically for every scenario and period, and review them before release:
   - maps of change hotspots;
   - difference maps against the previous run or release;
   - outside-model and static regions (not empty);
   - NoData and land-mask edges (no stripes, shifts, or holes along tile or region borders).
10. **Tooling (R):**

    | Task | Package |
    | :--- | :--- |
    | Rasters (read, write, COG, zonal, focal) | `terra` |
    | Vectors, CRS handling | `sf` |
    | Area-weighted extraction, coverage fractions | `exactextractr` |
    | Tables (Parquet; larger-than-memory queries) | `arrow`, `duckdb`, `data.table` |
    | Multi-dimensional cubes (optional) | `stars` |

    Do not use the superseded `raster` package or the retired `rgdal`, `rgeos`, and `maptools`. Process large rasters by tile or region (`terra` processes out of memory automatically; set `terraOptions(tempdir = ...)` to fast local disk) instead of converting whole rasters to data frames.

---

## Downscaling Workflow Verification Checklist

Every spatial pipeline run must pass this verification gate before publishing or downstream model consumption:

- [ ] **Area Conservation:** Every region-class target meets the target-hit criterion of Principle 5 (relative tolerance with absolute floor), and the worst relative error $\max_{R,k} |\sum_{i \in R} A_{ik} - \text{Target}_{R,k}| / \text{Target}_{R,k}$ is reported:
      $$\left| \sum_{i \in R} A_{ik} - \text{Target}_{R, k} \right| \le \max\left(\epsilon_{\text{rel}} \cdot \text{Target}_{R,k}, \; \epsilon_{\text{abs}}\right) \quad \forall R, k$$
- [ ] **Pixel Closure:** Every pixel (or fragment) is exactly filled, including the residual class:
      $$\left| \sum_k A_{ik} - \text{Cap}_i \right| \le \varepsilon \quad \forall i$$
- [ ] **Non-Negativity & Restriction Compliance:** $A_{ik} \ge -\varepsilon$ everywhere, $A_{ik} \le U_{ik} + \varepsilon$, and $A_{ik} = 0$ wherever $R_{ik} = 0$. Report the number of violations (must be zero) and the worst violation.
- [ ] **Harmonization Log:** Reconciliation factors $f_r$ (with any $|f_r - 1| > 0.01$ flagged), start-map harmonization results (start-map year vs. base year, skipped/passthrough zones with their area gaps, balancing-class adjustments, gross-solver stop reasons and margin residuals, worst flow-budget deviation, area moved by harmonization and by quantum snapping, and the transition prior $\Pi$ as an artifact), target clips and redistributed deficits, feasibility pre-check results and the actions taken, extensive-layer conservation checks, and share re-closure corrections are written to the run log.
- [ ] **Prior Adherence:** Report, per region (and as an area-weighted total), the KL divergence between normalized allocation shares $a_{ik} = A_{ik} / \sum_j A_{ij}$ and the econometric priors $p_{ik}$:
      $$D_r = \frac{\sum_{i \in r} w_i \sum_k a_{ik} \log \frac{a_{ik}}{p_{ik}}}{\sum_{i \in r} w_i}, \qquad w_i = \sum_k A_{ik}, \qquad 0 \log 0 \equiv 0,$$
      compare it against the previous run / baseline, and flag regions whose $D_r$ exceeds a documented threshold or jumps between runs (typically Stage 3–4 regions forced far from the prior).
- [ ] **Outside-Model Area Preserved:** Per region and scenario, the area of every `_other` class equals its start-map area in every period. Every start-map region appears in the land-use outputs and rasters, including regions without targets (no gaps).
- [ ] **Harmonized Start Map Fidelity:** Every harmonized zone-class area matches $M_{zk}^{(t_0)}$ within the target-hit tolerance (plus one quantum after snapping), except the balancing class. The deviation layer (harmonized minus observed start map) is saved with the outputs.
- [ ] **Temporal Chain Integrity:** For every scenario and period: the start state equals the previous period's downscaled map exactly (pixel by pixel, including the residual class); pixel closure holds; achieved region-level transitions match the transition targets within the target-hit tolerance; no flows leave classes that did not exist at $t_{j-1}$; flows marked zero in $Z$ are zero; irreversible classes never lose area.
- [ ] **Dynamics Report:** Per region and period: gross-to-net ratio, unmet demand and the policy applied, cumulative shortfall versus the macro model, reversal (flicker) rate, and the share of area with extrapolated covariates.
- [ ] **Validation Report:** accuracy against independent reference data (design, error matrix, user's and producer's accuracy, error-adjusted areas with confidence intervals), agreement across resolutions, skill relative to spatial benchmarks, and, for downscaling, allocation disagreement, figure of merit, and the hindcast results.
- [ ] **Uncertainty Statement:** uncertainty layers distributed with the products, or the declared point estimate (which summary, which sources not quantified); regional uncertainty aggregated with spatial dependence; sensitivity of key processing parameters reported.
- [ ] **Output Standards:** rasters are valid COGs with overview resampling by variable type; data types, NoData values, units, and share/area semantics are declared; outputs are aligned to the reference grid; every class code is in the distributed class table; rasters reproduce the tabular totals; every file has a schema-valid JSON sidecar; quick-look maps reviewed.
- [ ] **Lineage Tracking:** Input target versions (macro-model run ID and scenario), base year $t_0$ and observed land-cover product/version, crosswalk version and hash, NUTS/geometry version, region-membership method, pixel weights $w_i$, prior model checkpoint IDs, the ordered period vector, the allowed-transition matrix version, the unmet-demand policy, the covariate projection used per period (or the statement that priors are static), the reference data and sampling design used for validation, the uncertainty mode (ensemble size or declared point estimate), and the full solver options are serialized alongside final downscaled rasters.
