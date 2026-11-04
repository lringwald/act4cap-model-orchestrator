---
# Claude Code Path Trigger:
paths:
  # Analysis-project layout
  - "src/{models,stats,estimation,workflows}/**/*.{R,py,cpp,stan}"
  - "models/**/*.{R,py,cpp,stan}"
  - "scripts/**/*.{R,py}"
  - "notebooks/**/*.{ipynb,qmd,Rmd}"
  # R package layout (estimation engines): R sources, Rcpp/C++ samplers, Stan, tests
  - "R/**/*.R"
  - "src/**/*.{cpp,cc,c,h,hpp}"
  - "inst/include/**/*.{h,hpp}"
  - "inst/stan/**/*.stan"
  - "tests/testthat/**/*.R"
  - "vignettes/**/*.{Rmd,qmd}"
  # Vendored / submodule copies of engine packages
  - "external/**/*.{R,cpp,cc,c,h,hpp,stan}"
# Antigravity Model Decision Trigger:
trigger: model_decision
description: "Statistical and econometric modeling invariants: data lineage (MODEL_LINEAGE.md), bijective scaling, Gibbs/BlackJAX/Stan efficiency, exhaustive convergence triage, residual forensics, continuous credibility, horseshoe shrinkage, posterior predictive checks (accuracy, calibration/coverage, sharpness, tails; LOO measures; benchmark models), uncertainty propagation to derived and downstream quantities, and prior plausibility (elicitation of signs and effect-size ranges, joint prior predictive checks, prior-posterior sensitivity, documented prior revision)."
---

# Statistical & Econometric Modeling Guidelines

## How to Read the Numbers in This Rulebook

Concrete statistics and thresholds in this rulebook (e.g. the soft zones for $\hat{R}$ and relative ESS, MCSE/SD < 0.05, E-BFMI > 0.3, $\kappa > 30$, VIF > 10, the $p_d$ tiers, the Pareto-$\hat{k}$ threshold, coverage levels, $\Delta\widehat{\text{elpd}}$ of about 4, the 99% prior predictive bound, the choice of predictive measures in C.3) are **recommended defaults**, not gates.

1. **Hard limits are the exception and are marked as such.** Currently there are two, both in C.1: $\hat{R} > 1.1$, and a relative effective sample size $\text{ESS}/S < 0.01$. Hard limits are not relaxed per project.
2. **Projects adapt the defaults.** A project may set other values, choose a subset of the measures, or add its own, in a **project profile**: a section in the project rulebook (e.g. `.claude/rules/<project>-project.md`) or in `MODEL_LINEAGE.md`. Each deviation gets one line of reasoning (data size, model class, use of the results). The agent follows the project profile without asking again, and uses the defaults only where the profile is silent.
3. **Values near a threshold are read continuously.** A relative ESS of 9% instead of 10%, or a coverage of 93% instead of 95%, is not a cliff. Report the value and interpret it with the diagnostics, as in C.2. Thresholds mark where scrutiny increases, not where results switch from valid to invalid.
4. **Deviations are fixed before results are seen** wherever they could change conclusions, and are recorded with the model version. Loosening a criterion *after* a model failed it is a revision that must be reported as such (C.4; `data-integrity.md`, Principle 2).
5. **The qualitative rules are not defaults:** evaluating all data-informed parameters, out-of-sample evaluation for predictive claims, continuous evidence instead of binary significance, and documented, versioned priors apply in every project. Only *how* they are quantified is adapted.

---

## Principle 0: End-to-End Mathematical Lineage (`MODEL_LINEAGE.md`)

Every statistical pipeline—from simple regression to complex hierarchical spatial systems—must maintain a living mathematical specification document (`MODEL_LINEAGE.md`) defining the pipeline as a four-part functional composition:

$$\mathcal{P} = \left( \mathcal{S}, \, \mathcal{T}, \, \mathcal{M}, \, \mathcal{G}^{-1} \right)$$

1. **Selection & Trimming Operator ($\mathcal{S}$):**
   Given raw dataset $\mathcal{D}_{\text{raw}}$ of size $N_{\text{raw}}$ (with cross-sectional unit $i \in \{1, \dots, N\}$, spatial support $\mathbf{s}_i$, and time $t \in \{1, \dots, T_i\}$), document every filtering, bounding-box clip, or missingness exclusion $k \in \{1, \dots, K\}$ as an indicator function:
   $$\mathcal{D} = \mathcal{S}(\mathcal{D}_{\text{raw}}) = \left\{ (i, t) \in \mathcal{D}_{\text{raw}} : \prod_{k=1}^K I_k(y_{it}^{\text{raw}}, \mathbf{x}_{it}^{\text{raw}}, \mathbf{s}_i, T_i) = 1 \right\}$$
   *Mandatory Policy:* Log observation drops ($\Delta N_{\text{obs}}$) and unit drops ($\Delta N$). Differentiate between corrupted data (missing sensor codes, impossible areas) and valid extreme empirical shocks. Arbitrary trimming or $\pm 3\sigma$ deletion of valid data is forbidden; handle heavy tails and extreme shocks via flexible error kernels in $\mathcal{M}$.

2. **Pre-Estimation Feature & Target Scaling Map ($\mathcal{T}$):**
   Document the exact bijection mapping natural-scale variables $(y, \mathbf{X}, \mathbf{Z})$ to estimation-space inputs $(\tilde{y}, \tilde{\mathbf{X}}, \tilde{\mathbf{Z}})$ parameterized by empirical state $\boldsymbol{\eta}$:
   $$\tilde{\mathbf{X}} = \psi_{\mathbf{X}}(\mathbf{X}; \boldsymbol{\eta}_X), \qquad \tilde{y} = \phi_y(y; \boldsymbol{\eta}_y)$$
   Persist $\boldsymbol{\eta}$ to disk (`transform_state.json` or `.parquet`) on every run.

3. **Data Generating Process & Prior Structure ($\mathcal{M}$):**
   State the full joint distribution over estimation-space inputs and parameters:
   $$p(\tilde{y}, \tilde{\boldsymbol{\theta}} \mid \tilde{\mathbf{X}}, \mathcal{M}) = p(\tilde{y} \mid \tilde{\mathbf{X}}, \tilde{\boldsymbol{\theta}}, \mathcal{M}) \, p(\tilde{\boldsymbol{\theta}} \mid \mathcal{M})$$

4. **Inverse Scale Recovery & Marginal Effects ($\mathcal{G}^{-1}$):**
   Document the exact back-transformation mapping posterior draws $\tilde{\boldsymbol{\theta}}^{(s)}$ and estimation-scale predictions $\tilde{y}^{*(s)}$ to natural units:
   $$y^{*(s)} = \phi_y^{-1}(\tilde{y}^{*(s)}; \boldsymbol{\eta}_y)$$
   alongside natural-scale marginal effects $\frac{\partial \mathbb{E}[y \mid \mathbf{X}]}{\partial X_j}$, elasticities, and spatial multiplier decompositions.

---

## Phase A: Pre-Processing, Contrasts & Degeneracy Audits

### A.1 Core Invariants (Mandatory for All Models)
* **Continuous Covariate Scaling:** Standardize continuous predictors ($\tilde{X}_{ij} = \frac{X_{ij} - \mu_{X_j}}{\sigma_{X_j}}$, or divide by $2\sigma_{X_j}$ under Gelman's rule when continuous predictors are modeled alongside binary $0/1$ indicators). Standardize or transform continuous targets $\tilde{y}_i = \phi_y(y_i; \boldsymbol{\eta}_y)$.
* **Serialization of Transformation State:** Never scale variables ephemerally in memory. Always serialize $\boldsymbol{\eta} = (\boldsymbol{\mu}_X, \boldsymbol{\sigma}_X, \mu_y, \sigma_y, \phi_y)$ to disk so raw-scale linear slopes and intercepts can be analytically backed out:
  For an identity-link model with z-scored target and predictors ($\tilde{X}_{ij} = (X_{ij} - \mu_{X_j})/(k\,\sigma_{X_j})$, $k = 1$ for z-scores, $k = 2$ under Gelman's rule):
  $$\beta_j^{(s)} = \tilde{\beta}_j^{(s)} \frac{\sigma_y}{k\,\sigma_{X_j}}, \qquad \beta_0^{(s)} = \mu_y + \sigma_y \tilde{\beta}_0^{(s)} - \sum_{j=1}^P \tilde{\beta}_j^{(s)} \frac{\sigma_y}{k\,\sigma_{X_j}} \mu_{X_j}$$
  For non-identity links (logit, MNL, log/Poisson, probit) the target is not scaled, so drop the $\sigma_y$ and $\mu_y$ factors: $\beta_j^{(s)} = \tilde{\beta}_j^{(s)} / (k\,\sigma_{X_j})$ and $\beta_0^{(s)} = \tilde{\beta}_0^{(s)} - \sum_j \beta_j^{(s)} \mu_{X_j}$ (on the link scale). Unscaled $\{0,1\}$ dummies need no back-transform. For any other $\phi_y$ (log, Box-Cox, logit of a share) linear back-transforms do not apply; recover natural-scale quantities by pushing each draw through $\phi_y^{-1}$ in $\mathcal{G}^{-1}$.
* **Categorical Contrasts & Reference Class Recovery:**
  * Preserve the global intercept $\beta_0$. For a categorical factor with $K$ unordered levels $\{c_1, \dots, c_K\}$, designate a baseline reference level $c_{\text{ref}}$ (by default, the highest-frequency category) and construct $K-1$ contrast dummies:
    $$\eta_i = \beta_0 + \sum_{k=2}^K \beta_k d_{ik} + \dots, \qquad d_{ik} = \mathbb{I}(c_i = c_k)$$
  * Keep dummy indicators unscaled in $\{0, 1\}$.
  * In post-processing ($\mathcal{G}^{-1}$), automatically compute:
    * Reference contrast effect: $\beta_{\text{ref}}^{(s)} \equiv 0$ with $\text{Var} \equiv 0$.
    * Absolute level intercepts: $\mu_{\text{ref}}^{(s)} = \beta_0^{(s)}$ and $\mu_k^{(s)} = \beta_0^{(s)} + \beta_k^{(s)}$ for $k = 2, \dots, K$.
    * All-to-all pairwise contrasts: $\Delta_{j, k}^{(s)} = \beta_j^{(s)} - \beta_k^{(s)}$.
    * Symmetric deviation representation: $\bar{\mu}^{(s)} = \frac{1}{K}\sum_{k=1}^K \mu_k^{(s)}$, with $\alpha_k^{(s)} = \mu_k^{(s)} - \bar{\mu}^{(s)}$ such that $\sum_{k=1}^K \alpha_k^{(s)} \equiv 0$.
* **Collinearity & Numerical Pre-Checks:** Evaluate matrix rank, condition number $\kappa(\tilde{\mathbf{X}}) = \sigma_{\max} / \sigma_{\min}$, and Variance Inflation Factors ($\text{VIF}_j$). Flag design matrices with $\text{rank}(\tilde{\mathbf{X}}) < P$, $\kappa(\tilde{\mathbf{X}}) > 30$, $\max \text{VIF}_j > 10$, or outcome separation in discrete models before invoking a sampler.
* **Prior Predictive Simulation Gate:** Draw $\tilde{\boldsymbol{\theta}}^{(s)} \sim p(\tilde{\boldsymbol{\theta}})$, simulate $y^{\text{prior}(s)} = \phi_y^{-1}(\tilde{y}^{\text{prior}(s)})$, and verify that $99\%$ of prior predictive density resides within scientifically possible domain bounds. This is the minimum; behavioural and economic plausibility is assessed in A.3.

### A.2 Optional Structural Modules
* **[Panel / Grouped Data] Random Effects vs. Fixed Effects Rationale:**
  * Justify Random Effects (partial pooling) when: (1) cluster sizes $T_i$ are small or unbalanced (prevents overfitting to noise); (2) time-invariant contextual predictors $\mathbf{Z}_i$ (geography, soil, policy) must be estimated; or (3) out-of-sample prediction to unseen clusters is required.
  * To resolve endogeneity ($\text{Cov}(u_i, \mathbf{X}_{it}) \neq \mathbf{0}$) without discarding time-invariant predictors $\mathbf{Z}_i$, apply the **Within-Between / Mundlak decomposition**:
    $$\bar{X}_{i\cdot j} = \frac{1}{T_i}\sum_{t=1}^{T_i} X_{itj}, \qquad \dot{X}_{itj} = X_{itj} - \bar{X}_{i\cdot j}$$
    $$\tilde{X}_{ij, \text{B}} = \frac{\bar{X}_{i\cdot j} - \mu_{\text{B}, j}}{\sigma_{\text{B}, j}}, \qquad \tilde{X}_{itj, \text{W}} = \frac{\dot{X}_{itj}}{\sigma_{\text{W}, j}}$$
    Specify: $\eta_{it} = \beta_0 + \tilde{\mathbf{X}}_{it, \text{W}}^\top \boldsymbol{\beta}_{\text{W}} + \tilde{\mathbf{X}}_{i, \text{B}}^\top \boldsymbol{\beta}_{\text{B}} + \tilde{\mathbf{Z}}_i^\top \boldsymbol{\gamma} + u_i$. Track the Mundlak contextual discrepancy $\boldsymbol{\pi}^{(s)} = \boldsymbol{\beta}_{\text{B}}^{(s)} - \boldsymbol{\beta}_{\text{W}}^{(s)}$.
* **[Spatial Structures] Spatial Weights ($\mathbf{W}$) Lineage:**
  * Document connectivity (Queen/Rook contiguity, $k$-NN, inverse distance with cutoff $d_{\max}$) and normalization (row-stochastic $\sum_j w_{ij} = 1$, spectral, or symmetric) in `MODEL_LINEAGE.md`.
  * Compute spatial lags $\mathbf{W}\tilde{\mathbf{X}}$ *after* within-between decomposition to preserve orthogonality, and run collinearity checks on $[\tilde{\mathbf{X}}, \mathbf{W}\tilde{\mathbf{X}}]$.
* **[Non-Linear Transformations] Jacobian Adjustment:**
  When comparing models fitted on different target transforms $\phi_a(y)$ vs. $\phi_b(y)$, add the log-Jacobian determinant to the log-likelihood: $\log p_y(y_i) = \log p_{\tilde{y}}(\phi_y(y_i)) + \log |\mathrm{d}\phi_y(y_i)/\mathrm{d}y_i|$.

### A.3 Prior Plausibility & Elicitation (Before Estimation)
Priors encode what is already known about plausible behaviour and effect sizes. They are set **before** looking at fitted results, on scales that domain experts can judge, and documented like any other model input.

1. **Elicit on interpretable scales.** For every coefficient or derived estimand, state on the natural scale:
   - **Expected sign,** and whether it is a *logical or physical necessity* (variances are positive; transport cost cannot increase accessibility) or a *behavioural hypothesis* (e.g. steeper slopes reduce the odds of cropland).
   - **A plausible magnitude range** $[L, U]$ in units people can judge: elasticities, odds ratios or relative risks per standard deviation of the covariate, marginal effects in percentage points, multiplicative effects on expected counts or densities, spatial multipliers.
   - **The source** of the range: literature or meta-analyses (with reference and table or page), earlier estimates of the same model, physical bounds, or structured expert judgement. Sources must be real and verifiable (see `data-integrity.md`); a "typical value" recalled from memory is not a source.
2. **Translate the range to the estimation scale** through the scaling map $\mathcal{T}$ (Principle 0, A.1):
   - **Identity link, z-scored $y$ and $X$:** $\tilde{\beta}_j = \beta_j \, k\,\sigma_{X_j} / \sigma_y$ (with $k = 2$ under Gelman's rule).
   - **Logit / MNL (class $c$ vs. the reference class):** $\tilde{\beta}_{cj}$ is the change in log-odds per $k\,\sigma_{X_j}$, so an elicited odds-ratio range $[\text{OR}_L, \text{OR}_U]$ per standard deviation gives $\tilde{\beta}_{cj} \in [\log \text{OR}_L, \log \text{OR}_U]$. For elasticities, use the MNL elasticity of $P_c$ with respect to $x_j$ at representative values, $\varepsilon_{cj} = x_j \left(\beta_{cj} - \sum_l P_l \beta_{lj}\right)$ with $\beta_{\text{ref},j} = 0$, to translate an elasticity range into a coefficient range.
   - **Log link (counts, densities):** $\exp(\tilde{\beta}_j)$ is the multiplicative effect per standard deviation.
   - **Normal priors covering a 95% range:** centre $m = (L + U)/2$, scale $s = (U - L) / (2 \cdot 1.96)$.
3. **Sign information is soft unless it is a necessity.** Encode a behavioural sign expectation by locating the prior mass (shifted mean, or skewed prior), so the data can still contradict it. Use hard constraints (truncation, positive-only distributions) only for logical or physical necessities. A hard constraint on a hypothesis makes the hypothesis untestable.
4. **Check plausibility jointly, not only per coefficient.** Independent, individually reasonable priors can imply implausible predictions:
   - **Linear-predictor scale:** with roughly uncorrelated standardized predictors and zero-mean priors, $\text{Var}(\eta) \approx \sum_j s_j^2$ (plus intercept and random-effect variances). With $P = 50$ predictors and $s_j = 1$, the prior standard deviation of a log-odds is about 7, which puts almost all prior mass on choice probabilities near 0 or 1. Keep the implied spread on the link scale realistic, e.g. by scaling $s_j \propto 1/\sqrt{P}$, or by using priors on explained variance or shrinkage priors (horseshoe, R2D2), whose global scale is itself elicited.
   - **Behavioural quantities from prior draws:** simulate from the prior and summarize what an expert can judge: implied class shares and transition probabilities, elasticities and marginal effects at representative covariate values, odds ratios between land uses, spatial multipliers ($1/(1 - \rho)$ for row-stochastic $\mathbf{W}$), and the share of variance attributed to random effects.
   - **Gate:** the bulk of the prior predictive distribution of these quantities (e.g. 95%) lies within broad plausible bounds, and only a small share of draws gives degenerate probabilities (e.g. $P < 10^{-6}$ or $P > 1 - 10^{-6}$ for an alternative that is clearly possible). Report the shares.
5. **Variance and heterogeneity priors are elicited too.** For a random-effect standard deviation $\sigma_u$, elicit an upper plausible value $U$ (e.g. regional log-odds differences rarely exceed $\pm 1.5$) and a tail probability $\alpha$:
   - half-normal: $P(\sigma_u > U) = \alpha \Rightarrow s = U / \Phi^{-1}(1 - \alpha/2)$;
   - exponential (penalized-complexity prior): $P(\sigma_u > U) = \alpha \Rightarrow \lambda = -\log(\alpha) / U$.
6. **Shrinkage priors:** elicit the expected number of relevant predictors $p_0$ (B.3, regularized horseshoe) from domain knowledge, and check the implied prior on the effective number of predictors $p_{\text{eff}}$ by simulation.
7. **Data-dependent priors are declared.** Using the data to set priors (empirical Bayes, scales from sample statistics beyond the standardization in $\mathcal{T}$) is allowed only when declared, and tuning of prior hyperparameters uses training or calibration folds only, never the data used for evaluation.
8. **Document a prior table** in `MODEL_LINEAGE.md`, one row per parameter block:

   | Estimand (natural scale) | Expected sign (necessity / hypothesis) | Plausible range $[L, U]$ | Source | Prior on estimation scale | Prior-predictive check result |
   | :--- | :--- | :--- | :--- | :--- | :--- |

   Version the prior specification (e.g. `priors_v1`) so that later revisions (C.4) are traceable.

---

## Phase B: Estimation & Sampler Architecture

### B.1 Core Computational Invariants (All Samplers)
1. **Precision & Cholesky Solvers Over Matrix Inversion:** Never compute explicit matrix inverses $\boldsymbol{\Sigma}^{-1}$. Parameterize Gaussian blocks via precision matrices $\mathbf{Q}$ or Cholesky factors $\mathbf{L}\mathbf{L}^\top = \mathbf{Q}$, solving linear systems via triangular back-substitution (`solve(L, b)`).
2. **Pre-Compute Static Sufficient Statistics:** Move cross-products ($\tilde{\mathbf{X}}^\top \tilde{\mathbf{X}}$, $\tilde{\mathbf{X}}^\top \tilde{\mathbf{y}}$), group aggregation indices, spatial adjacency eigenvalues, and spline bases strictly outside the MCMC sweep loop.
3. **Internal Identifiability:** Enforce sum-to-zero ($\sum_g u_g = 0$) or QR constraints during each sweep kernel, never via post-hoc drift recentering of stored draws. An in-sweep recentering step is only a valid move if it leaves the linear predictor unchanged: shift the mean into the matching intercept (or global slope) in the same step,
   $$\bar{u}^{(s)} = \tfrac{1}{G}\textstyle\sum_g u_g^{(s)}, \qquad u_g^{(s)} \leftarrow u_g^{(s)} - \bar{u}^{(s)}, \qquad \beta_0^{(s)} \leftarrow \beta_0^{(s)} + \bar{u}^{(s)},$$
   so that $\eta_i = \beta_0 + u_{g[i]} + \dots$ is unchanged. (Weight $\bar{u}$ by group size if the constraint is $\sum_g n_g u_g = 0$.) Recentering without this compensation changes the target posterior. Better still, sample under the constraint directly (e.g. conditioning the Gaussian block on $\mathbf{A}\mathbf{u} = \mathbf{0}$ via the Rue-Held "conditioning by kriging" correction).

### B.2 Engine-Specific Playbooks

#### Engine 1: Custom Gibbs & Hybrid Samplers (R / C++ / JAX)
* **Pre-Allocation & Zero-Allocation Loops:** Pre-allocate all workspace matrices and vectors outside the sweep loop; overwrite in-place.
* **Block Updating of Correlated Blocks:** Jointly sample $(\boldsymbol{\beta}, \mathbf{u})$ from combined multivariate Gaussian conditionals via Rue-Held Cholesky back-solves:
  $$\mathbf{L}\mathbf{L}^\top = \mathbf{Q}_{\boldsymbol{\theta}}, \quad \mathbf{L}\mathbf{v} = \mathbf{b}, \quad \mathbf{L}^\top \boldsymbol{\mu} = \mathbf{v}, \quad \mathbf{L}^\top \mathbf{w} = \mathbf{z} \sim \mathcal{N}(\mathbf{0}, \mathbf{I}), \quad \boldsymbol{\theta}^{(s)} = \boldsymbol{\mu} + \mathbf{w}$$
* **Conjugate Auxiliary Variable Data Augmentation:**
  * *Binomial / Logistic / Multinomial:* Use Pólya-Gamma augmentation ($\omega_i \sim \text{PG}(b_i, \eta_i)$) to render the full conditional Gaussian: $\mathbf{Q}_\beta = \tilde{\mathbf{X}}^\top \text{diag}(\boldsymbol{\omega}) \tilde{\mathbf{X}} + \mathbf{Q}_0$.
  * *Probit / Tobit:* Use Albert-Chib truncated normal latent utilities $z_i \sim \mathcal{TN}(\eta_i, 1, a_i, b_i)$.
  * *Student-$t$ & Horseshoe:* Use Inverse-Gamma scale mixtures for conjugate variance/shrinkage updates.
* **ASIS & Parameter Expansion for Variance Components:** Apply Ancillarity-Sufficiency Interweaving (ASIS) or Parameter Expansion (PX-DA) to prevent variance chains from sticking near zero: update $\sigma_u^2$ in the Centered basis, map to the Non-Centered basis $z_i = (\alpha_i - \mu_\alpha)/\sigma_u$, and redraw $\sigma_u^2 \mid \mathbf{z}, \mathbf{y}$.

#### Engine 2: `BlackJAX` (Functional JAX Samplers)
* **Pure Functional Loop Execution:** Compile transition kernels via `jax.jit` and execute sampling loops via `jax.lax.scan`. Vectorize across independent chains using `jax.vmap`.
* **Static Array Shapes:** Keep array shapes and group indexing masks static at compile time. Never use dynamic boolean masking (`x[x > 0]`) inside JIT log-densities; use `jnp.where(mask, val, 0.0)`.
* **Explicit PRNG Key Splitting:** Split PRNG keys deterministically at every transition step: `key, subkey = jax.random.split(key)`.
* **Composable Hybrid Kernels:** Pair `blackjax.nuts` or `blackjax.hmc` for continuous non-conjugate parameters with analytical JAX conditional updates for conjugate Gaussian blocks or Pólya-Gamma latents.

#### Engine 3: `rstan` (Autodiff HMC/NUTS)
* **Discrete Parameter Marginalization:** Stan cannot sample discrete latent parameters. Marginalize discrete mixture components, regime switches, or zero-inflation indicators via `log_sum_exp` in the `model` block, recovering posterior class probabilities in `generated quantities`.
* **Autodiff Tape Minimization:** Use fused GLM primitives (`normal_id_glm_lpdf`, `bernoulli_logit_glm_lpmf`, `poisson_log_glm_lpmf`, `categorical_logit_glm_lpmf`) and vectorized statements; these shrink the autodiff expression graph. Declaring intermediates locally in the `model` block instead of `transformed parameters` does not shrink the tape, but it avoids writing them to the output CSV (I/O and memory on large $N$); do it for anything you do not need to inspect.
* **Interface:** Prefer `cmdstanr` over `rstan` (current Stan releases, lower memory overhead, out-of-process sampling).
* **Thin QR in Transformed Data:** When predictors exhibit moderate collinearity ($\kappa(\tilde{\mathbf{X}}) > 15$, i.e. below the $\kappa > 30$ red flag in A.1, which must still be resolved before sampling), apply thin QR decomposition ($\tilde{\mathbf{X}} = \mathbf{Q}^* \mathbf{R}^*$) inside `transformed data`.

### B.3 Optional Modeling Modules

#### Flexible Error Kernels (Model-Based Outlier Accommodation)
* **Student-$t$ Error Kernel:**
  $$\varepsilon_i \mid \lambda_i \sim \mathcal{N}\left(0, \, \frac{\sigma^2}{\lambda_i}\right), \qquad \lambda_i \sim \text{Gamma}\left(\frac{\nu}{2}, \, \frac{\nu}{2}\right), \qquad \nu \sim \text{Gamma}(2, 0.1) \ge 2$$
  Precision weights satisfy $\mathbb{E}[\lambda_i \mid e_i] = \frac{\nu + 1}{\nu + e_i^2 / \sigma^2}$, automatically downweighting extreme shocks without trimming rows from $\mathcal{D}$.
* **Finite Mixture of Gaussians (MoG):**
  $$p(\varepsilon_i) = \sum_{k=1}^K \pi_k \mathcal{N}(\mu_k, \sigma_k^2), \qquad \sum_{k=1}^K \pi_k \mu_k = 0$$
  Enforce ordering constraints on $\mu_k$ or $\sigma_k^2$ to prevent label-switching across chains.

#### Continuous Shrinkage (The Horseshoe Family)
* **Regularized / Finnish Horseshoe:**
  $$\beta_j \mid \lambda_j, \tau, c, \sigma \sim \mathcal{N}\left(0, \, \sigma^2 \tau^2 \tilde{\lambda}_j^2\right), \qquad \tilde{\lambda}_j^2 = \frac{c^2 \lambda_j^2}{c^2 + \tau^2 \lambda_j^2}$$
  $$\lambda_j \sim \text{C}^+(0, 1), \qquad c^2 \sim \text{Inv-Gamma}\left(\frac{\nu_{\text{slab}}}{2}, \, \frac{\nu_{\text{slab}} s_{\text{slab}}^2}{2}\right), \qquad \tau_0 = \frac{p_0}{P - p_0}\frac{\sigma}{\sqrt{N}}$$
  where $p_0$ is the prior guess of active non-zero predictors.
* *Gibbs Implementation:* For the plain horseshoe ($c \to \infty$), update $\lambda_j^2, \tau^2$ via the conjugate Inverse-Gamma mixture of Makalic & Schmidt (2016) with auxiliary scales $\nu_j, \xi$. The regularized slab breaks conjugacy: $\tilde{\lambda}_j^2$ is not Inverse-Gamma given the rest. Either (i) update $\lambda_j$, $\tau$ with slice or log-scale Metropolis steps inside the Gibbs sweep, or (ii) use the closely related "shrunken shoulder" formulation of Nishimura & Suchard (2023), which multiplies the horseshoe by a separate Gaussian slab factor so that $\boldsymbol{\beta} \mid \boldsymbol{\lambda}, \tau$ stays Gaussian and the standard horseshoe updates for $\lambda_j$, $\tau$ carry over. $c^2$ given $\boldsymbol{\beta}, \boldsymbol{\lambda}, \tau$ is also non-conjugate in the Piironen-Vehtari form; update it with slice sampling or fix it.
* *HMC Implementation:* Sample independent standard normals $z_{\beta_j}, z_{\lambda_j}, z_\tau \sim \mathcal{N}(0, 1)$ to eliminate the funnel at the origin.

#### Spatial Econometric Taxonomies
* **SLX (Spatial Lag of $\mathbf{X}$):** $\tilde{\mathbf{X}}^* = [\tilde{\mathbf{X}}, \mathbf{W}\tilde{\mathbf{X}}]$ with $\rho = \lambda = 0$. Linear, fast, recommended baseline for local spillovers.
* **SAR / SDM (Spatial Lag of $\mathbf{y}$):** $\tilde{\mathbf{y}} = \rho \mathbf{W}\tilde{\mathbf{y}} + \tilde{\mathbf{X}}\boldsymbol{\beta} + \mathbf{W}\tilde{\mathbf{X}}\boldsymbol{\theta} + \boldsymbol{\varepsilon}$. Pre-compute spatial eigenvalues $\sum_{i=1}^N \ln(1 - \rho \lambda_i)$ or spline-interpolated log-determinants to avoid $\mathcal{O}(N^3)$ matrix inversions during sampling sweeps.
* **Spatial Error / Scaled BYM2:** For areal spatial random effects, use scaled BYM2: $\boldsymbol{\psi} = \sigma_\psi (\sqrt{1-\rho_{\text{sp}}}\boldsymbol{\theta}_{\text{iid}} + \sqrt{\rho_{\text{sp}}}\boldsymbol{\phi}_{\text{ICAR}}^*)$ where $\sum_i \phi_i^* = 0$.

---

## Phase C: Convergence, Stability & Residual Forensics

### C.1 Exhaustive Stability Gate (All Data-Informed Parameters)
Evaluating convergence solely on primary structural slopes is strictly forbidden. The stability gate applies to **all parameters receiving likelihood updates**: slopes ($\boldsymbol{\beta}$), spatial/temporal coefficients ($\rho, \lambda, \phi_{\text{AR}}$), variance scales ($\sigma_\varepsilon, \sigma_u, \sigma_\psi$), degrees of freedom ($\nu$), all individual group random effects ($u_1, \dots, u_N$), spatial fields ($\phi_1^*, \dots, \phi_N^*$), and latent scales ($\lambda_j, \tau, c$).

Across $C \ge 4$ chains initialized from overdispersed starting values, compute the rank-normalized split $\hat{R}$, $\text{ESS}_{\text{bulk}}$, $\text{ESS}_{\text{tail}}$, and $\text{MCSE}/\text{SD}_{\text{post}}$ for every such parameter. $\hat{R}$ is read in three zones, a soft target and a hard limit:

| Zone | $\hat{R}_{\text{split, rank}}(\theta_d)$ | Interpretation | Action |
| :--- | :--- | :--- | :--- |
| **Target** | $\le 1.01$ | Converged | None |
| **Soft zone** | $1.01 < \hat{R} \le 1.1$ | Not automatically a failure; interpreted with the C.2 diagnostics. Above 1.05, scrutiny increases | Diagnose (C.2, item 1); usually run longer or add chains, and report the parameter |
| **Hard limit** | $> 1.1$ | Not converged | The parameter, and every quantity derived from it, is not reported. Fix the model or sampler (C.2, item 2) and rerun. No exceptions |

**Effective sample size is read relative to the draws taken.** With $S$ post-warmup draws in total (all chains), the relative effective sample size $e_d = \text{ESS}(\theta_d) / S$ measures how efficiently the sampler mixes for $\theta_d$. It is computed for both $\text{ESS}_{\text{bulk}}$ and $\text{ESS}_{\text{tail}}$, and read in the same three zones:

| Zone | $e_d = \text{ESS}/S$ (bulk and tail) | Interpretation | Action |
| :--- | :--- | :--- | :--- |
| **Target** | $\ge 0.10$ | Efficient mixing | None |
| **Soft zone** | $0.01 \le e_d < 0.10$ | Slow mixing; not automatically a failure | Diagnose (C.2, item 1). More draws raise the absolute ESS, and with it the precision; re-parameterization or blocking raises $e_d$ itself |
| **Hard limit** | $< 0.01$ | Fewer than one effective draw per 100: the chain explores too little to trust its summaries | The parameter, and every quantity derived from it, is not reported. Fix the parameterization or sampler (C.2, item 2) and rerun. No exceptions |

**Precision of reported summaries** is a separate criterion. The recommended default is $\text{MCSE}/\text{SD}_{\text{post}} < 0.05$ for every reported quantity, which corresponds to an absolute ESS of about 400. In the soft zone it is reached by drawing more. In the hard zone more draws are not a remedy, because the problem is the sampler, not the run length.

**Report the distribution, not only the maximum.** With thousands of random effects or spatial-field parameters, a few values in the soft zone are expected by chance. Report, per parameter block, the maximum $\hat{R}$, the share of parameters above 1.01 and above 1.05, the minimum relative ESS (bulk and tail) and the share below 0.10, and the minimum absolute ESS, and say which parameters are in the soft zone. Clusters of soft-zone values in one block (e.g. all variance parameters, one region's random effects) point to a structural problem, not to chance.

### C.2 Nuanced Diagnostic Triage Protocol
Do not treat numerical thresholds as blind binary abort triggers. Diagnose the root cause:
1. **Soft Zone: Benign Slow Mixing or Not ($1.01 < \hat{R} \le 1.1$, or $0.01 \le \text{ESS}/S < 0.10$):** above $\hat{R} = 1.05$, or for $\text{ESS}/S < 0.05$, the evidence for benign mixing must be correspondingly stronger, and longer or additional chains are the default.
   * Compute cross-chain discrepancy: $\Delta_{\text{chain}}(\theta_d) = \frac{\max_c \hat{\theta}_{d, c} - \min_c \hat{\theta}_{d, c}}{\text{SD}_{\text{post}}(\theta_d)}$.
   * If $\Delta_{\text{chain}} < 0.1$, trace plots overlap around identical modes, and autocorrelation decays smoothly, the mode is stable. Increase post-warmup draws $S$, or verify that $\text{MCSE}$ is negligible relative to the substantive effect size (e.g. relative to the ROPE half-width $\delta_d$). Do **not** thin as a remedy: thinning never increases ESS; it only saves storage (thin for memory if needed, or stream draws to disk instead).
   * **Grey zone ($0.1 \le \Delta_{\text{chain}} \le 0.25$):** Not yet benign. Run longer chains or additional chains from new overdispersed inits and recompute. If $\Delta_{\text{chain}}$ does not fall below $0.1$ as $S$ grows, treat as a severe pathology (below).
2. **Severe Posterior Pathology (Requires Model Re-Specification):**
   * Any parameter beyond a hard limit in C.1 ($\hat{R} > 1.1$ or $\text{ESS}/S < 0.01$) lands here.
   * If $\Delta_{\text{chain}} > 0.25$ (or a grey-zone value fails to shrink with $S$), trace lines drift without bound, chains stick at parameter boundaries ($\sigma_u \to 0$), or divergent transitions cluster in specific posterior regions, do not simply sample longer. Re-specify the model: add identification constraints, switch to NCP/ASIS, or regularize priors.
3. **Engine-Specific Verification:**
   * *Custom Gibbs:* Verify newly coded conditionals on small synthetic datasets via **Simulation-Based Calibration (SBC) / Geweke tests** (confirming nominal coverage of true simulated parameters). Inspect integrated autocorrelation time $\tau_{\text{int}}$.
   * *`BlackJAX` / `rstan`:* Audit energy Bayesian Fraction of Missing Information ($\min_c \text{E-BFMI}_c > 0.30$), divergent transitions ($N_{\text{divergences}} = 0$), and saturated tree depths.

### C.3 Posterior Predictive Checks, Residual Forensics & Validation
Predictive performance is judged along four complementary dimensions, plus proper scoring rules that summarize them. No single number is sufficient: an extremely wide predictive distribution covers every observation and is still useless, and a sharp one can be confidently wrong.

| Dimension | Question | Main measures |
| :--- | :--- | :--- |
| **Accuracy** | Are predictions close to $y_{\text{obs}}$? | LOO point errors (RMSE, MAE; share-point errors for compositions), LOO-$R^2$, Brier score |
| **Calibration / coverage** | Do observations fall inside the predictive distribution at the nominal rates, with mass where the data are? | LOO-PIT uniformity, interval coverage at 50/80/95%, coverage by subgroup |
| **Sharpness** | How narrow are the predictive distributions? | Mean width of the 50/80/95% predictive intervals, in natural units |
| **Tail behaviour** | Are extremes predicted and covered? | 95/99% coverage, PIT mass in the outer 1%, tail test statistics, Pareto $\hat{k}$ |

The guiding principle is **maximal sharpness subject to calibration**. Calibration and scores come from **leave-one-out (LOO) or cross-validated** predictive distributions, because in-sample posterior predictive checks use each observation twice and are optimistic. In-sample checks remain useful for test statistics (item 4).

1. **Accuracy.** Report point errors of the LOO predictive mean or median against $y_{\text{obs}}$ (PSIS-weighted expectations, e.g. `loo::E_loo()`): RMSE and MAE in natural units, scale-free versions where units differ across groups, and share-point errors for compositional outcomes. For continuous outcomes add LOO-$R^2$; for categorical outcomes the Brier score, which is a proper score.
2. **Calibration / coverage.**
   - **LOO-PIT:** $u_i = F_{-i}(y_i) = \sum_s w_i^{(s)} \, \mathbb{I}\left(y_i^{\text{rep}(s)} \le y_i\right)$, with PSIS weights $w_i^{(s)}$. For discrete outcomes, use the randomized PIT $u_i = F_{-i}(y_i - 1) + v_i \left[F_{-i}(y_i) - F_{-i}(y_i - 1)\right]$ with $v_i \sim U(0, 1)$. The $u_i$ should be uniform; assess this with PIT-ECDF difference plots and simultaneous confidence bands, not by eye from histograms.
     - **U-shaped PIT:** the predictive is too narrow (overconfident).
     - **Hump-shaped PIT:** the predictive is too wide.
     - **Skewed PIT:** the predictive is biased.
   - **Interval coverage:** the share of observations inside the central 50%, 80% and 95% LOO predictive intervals, compared with the nominal rates *in both directions*. Over-coverage (intervals too wide) is a failure as much as under-coverage. Report the binomial uncertainty of each coverage rate.
   - **By subgroup:** coverage and PIT by region, class, period, and covariate bins. Good global coverage can hide regions that are systematically over- and under-covered.
   - **Residual structure:** map the PIT values to normal scores $e_i = \Phi^{-1}(u_i)$ and inspect binned residuals against fitted values and every covariate.
3. **Sharpness.** Report the mean width of the central 50/80/95% predictive intervals in natural units, next to their coverage. Between models with comparable calibration, prefer the sharper one.
4. **Tail behaviour and test statistics.**
   - **Tail calibration:** coverage of the 95% and 99% intervals, and the share of PIT values in $[0, 0.01] \cup [0.99, 1]$ (expected: 2%).
   - **Tail test statistics:** compare $T(y_{\text{obs}})$ with the distribution of $T(y^{\text{rep}})$ for maxima, minima, upper quantiles (95th, 99th), the number or share of zeros, and the largest changes or transitions. Report the location continuously, e.g. $P\left(T(y^{\text{rep}}) \ge T(y_{\text{obs}})\right)$, consistent with D.1, not as a significance test; values near 0 or 1 indicate misfit in that aspect.
   - **Statistics tied to the model's use:** test the quantities downstream steps rely on, e.g. region-level aggregates of pixel predictions against observed regional totals, class shares per region, transition counts, and spatial pattern statistics of predictions vs. observations.
   - **Influential and poorly predicted observations:** Pareto $\hat{k}_i$ from PSIS-LOO. Observations with $\hat{k}_i$ above the sample-size dependent threshold $\min(1 - 1/\log_{10} S, 0.7)$ are listed and mapped; clusters of high $\hat{k}$ point to local misspecification.
5. **Proper scores and LOO measures (overall).**
   - **$\widehat{\text{elpd}}_{\text{loo}}$ (log score)** with its standard error, computed on observation log-densities marginalized over latent random effects, so that random effects do not create artificial $\hat{k}_i > 0.7$.
   - **LOO-CRPS** $= \mathbb{E}|Y - y| - \tfrac{1}{2}\mathbb{E}|Y - Y'|$, in natural units and less dominated by single extreme observations than the log score. Report both, because they emphasize different parts of the distribution.
   - **$p_{\text{loo}}$** (effective number of parameters): $p_{\text{loo}}$ far above the number of parameters, or above $N/5$, signals misspecification or very weak priors.
   - **Pointwise elpd** aggregated by region, class, and period, to locate where the model predicts poorly.
   - **Model comparison:** $\Delta\widehat{\text{elpd}}$ with its standard error. Differences below about 4, or small relative to their standard error, do not distinguish the models.
   - **Benchmark models:** compare every model with simple baselines fitted and scored the same way. Depending on the outcome:
     - an intercept-only or constant model, and group or regional means;
     - persistence (the previous value or the previous map);
     - for spatial targets, nearest-neighbour or a plain spatial interpolation.

     Report each model's improvement over the best baseline, with its standard error. A model that does not beat the baselines adds no predictive information for that use, however good its in-sample fit.
   - **Unreliable LOO:** when many $\hat{k}_i$ exceed the threshold, use moment matching, exact refits for those observations, or K-fold cross-validation instead of reporting the PSIS estimate.
6. **Dependence-aware validation.** LOO assumes the observations can be exchanged. For spatial or temporal generalization, compute the same measures under **spatial block (leave-group-out) cross-validation** or **leave-future-out cross-validation**, and report them next to random LOO. The gap between the two measures how much spatial or temporal dependence inflates the apparent performance.
7. **Residual autocorrelation.** Compute the posterior distribution of Global Moran's $I^{(s)}$ (spatial) and lag-$k$ autocorrelation $\hat{\rho}_k^{(s)}$ (temporal) on residuals across draws $s = 1, \dots, S$. Judge it continuously, consistent with Phase D, not by whether an interval "excludes zero": report the posterior of $I^{(s)} - \mathbb{E}_0[I]$ with $\mathbb{E}_0[I] = -1/(N-1)$, its multi-resolution intervals, and $P\left(|I - \mathbb{E}_0[I]| > \delta_I\right)$ for a practical tolerance $\delta_I$ (e.g. $0.05$). Substantial mass beyond $\delta_I$ indicates unmodeled dependence; inspect a residual LISA map to locate it.
8. **Predictive-check report.** For every model, report one table: accuracy, coverage at 50/80/95%, sharpness, tail coverage and PIT tail mass, $\widehat{\text{elpd}}_{\text{loo}} \pm \text{SE}$, LOO-CRPS, $p_{\text{loo}}$, the share of $\hat{k}_i$ above the threshold, and the block or leave-future-out counterparts. Record it in `MODEL_LINEAGE.md` next to the prior table (A.3).

### C.4 Prior–Posterior Checks & Prior Revision (After Estimation)
The elicited ranges from A.3 are checked against the posterior, and priors are revised only for documented reasons.

1. **Compare posteriors with the elicited ranges.** For every estimand in the prior table, report the posterior mass inside $[L, U]$, and whether the posterior sign agrees with the expected sign. A posterior outside the plausible range is a finding to investigate, not something to hide:
   - **Model misspecification:** omitted variables or confounding, collinearity (A.1), wrong functional form, or unmodelled dependence (C.3).
   - **Data problems:** errors in inputs, units, crosswalks, or joins (see `data-pipelines.md`).
   - **Elicitation error:** the range or its source was wrong.
2. **Measure how much the data informed each parameter:**
   - **Posterior contraction:** $1 - \text{Var}_{\text{post}}(\theta) / \text{Var}_{\text{prior}}(\theta)$. Values near 0 mean the posterior is essentially the prior; report such parameters as prior-driven, not as estimated.
   - **Prior sensitivity and prior–data conflict:** power-scaling sensitivity analysis (e.g. the `priorsense` R package), which flags estimands that change materially when the prior or the likelihood is scaled, and distinguishes prior sensitivity from prior–data conflict.
   - For key estimands, refit under a deliberately wider and a narrower prior and report how much the substantive conclusions (D.1, D.2) move.
3. **When revising a prior is legitimate:**
   - the prior predictive check (A.3) failed and this was only noticed later;
   - a diagnosed prior–data conflict with a substantive explanation (e.g. the literature range came from a different context or scale);
   - an elicitation or translation error (wrong units, wrong scaling map).
4. **When it is not:** revising priors to obtain a desired sign, credible interval, or $p_d$; to make an implausible posterior "go away" without explaining it; or to improve performance on the data used for evaluation. These are result-driven manipulation (`data-integrity.md`, Principle 2).
5. **Every revision is versioned and reported:** new prior version (`priors_v2`), the reason, the diagnostics that motivated it, a refit, and a comparison of key estimands under both versions. Published results state which prior version they use and whether conclusions depend on the choice.

---

## Phase D: Substantive Inference, Credibility & Economic Relevance

### D.1 Continuous Tendency of Evidence (No Binary Significance)
Binary NHST language ("statistically significant" vs. "insignificant") is strictly forbidden. Characterize every estimand along continuous dimensions of evidence:
1. **Multi-Resolution Credible Intervals:** Report nested credible intervals: $50\%$ (core interquartile mass), $80\% / 90\%$ (moderate-to-strong tendency), and $95\% / 99\%$ (high-certainty bounds).
2. **Probability of Direction ($p_d$):**
   $$p_d(\theta_d) = \max\left( \frac{1}{S}\sum_{s=1}^S \mathbb{I}(\theta_d^{(s)} > 0), \; \frac{1}{S}\sum_{s=1}^S \mathbb{I}(\theta_d^{(s)} < 0) \right) \in [0.5, 1.0]$$
   * $p_d \ge 0.995$: Decisive directional evidence.
   * $p_d \in [0.975, 0.995)$: Strong directional evidence.
   * $p_d \in [0.95, 0.975)$: Moderate directional evidence.
   * $p_d \in [0.85, 0.95)$: Suggestive directional tendency (report as an informative trend, not a null).
   * $p_d < 0.85$: Diffuse / symmetric around zero.
3. **Stakes Scaling:** High-stakes, irreversible policy claims demand stringent certainty ($p_d \ge 0.975$); exploratory screening decisions warrant reporting at $80\%\text{--}90\%$ intervals.

### D.2 Mandatory Economic Relevance & ROPE Evaluation
Statistical certainty ($p_d \to 1$) measures precision relative to sample size, not substantive importance:
1. **Region of Practical Equivalence (ROPE):** Define domain-specific negligible bands $[-\delta_d, +\delta_d]$ (e.g., $|\varepsilon| < 0.001$). Compute:
   $$P_{\text{relevant}}(\theta_d) = \frac{1}{S}\sum_{s=1}^S \mathbb{I}(|\theta_d^{(s)}| > \delta_d), \qquad P_{\text{ROPE}}(\theta_d) = \frac{1}{S}\sum_{s=1}^S \mathbb{I}(\theta_d^{(s)} \in [-\delta_d, \delta_d])$$
2. **The Credibility–Relevance Matrix:**
   * *High $p_d$, Large $|\theta_d|$:* Credible & Economically Meaningful.
   * *High $p_d$, Trivial $|\theta_d|$ ($P_{\text{ROPE}} > 0.90$):* **Precisely Estimated Zero (Practically Irrelevant)**. Never celebrate a tiny elasticity as a meaningful driver simply because large $N$ pushed $p_d \to 1$.
   * *Moderate $p_d$, Large Upper Tail:* Imprecisely estimated but potentially impactful tendency; report directional mass and uncertainty candidly.
3. **Spatial Impacts (SAR / SDM):** Never interpret raw coefficients directly when $\rho \neq 0$. Use truncated Neumann power series expansions $\text{tr}(\mathbf{W}^m)$ to report the posterior distribution of:
   * **Average Direct Effect ($\text{ADE}$):** Diagonal average of $(\mathbf{I} - \rho \mathbf{W})^{-1}(\beta \mathbf{I} + \theta \mathbf{W})$.
   * **Average Total Effect ($\text{ATE}$):** Average row sum of the impact matrix; for row-stochastic $\mathbf{W}$ this reduces to $\frac{\beta + \theta}{1 - \rho}$. For other normalizations (spectral, symmetric), compute $\frac{1}{N}\mathbf{1}^\top(\mathbf{I} - \rho\mathbf{W})^{-1}(\beta\mathbf{I} + \theta\mathbf{W})\mathbf{1}$ directly.
   * **Average Indirect / Spillover Effect ($\text{AIE}$):** $\text{ATE} - \text{ADE}$.
4. **Horseshoe Shrinkage Forensics:** Report posterior shrinkage factors (Piironen & Vehtari, 2017)
   $$\kappa_j^{(s)} = \frac{1}{1 + n \, \sigma^{-2(s)} \, s_j^2 \, \tau^{2(s)} \tilde{\lambda}_j^{2(s)}},$$
   where $s_j^2$ is the sample variance of predictor $j$ ($s_j^2 = 1$ for z-scored predictors). For non-Gaussian likelihoods replace $\sigma^2$ with the pseudo-variance of the link (e.g. $\sigma^2 \approx 1/(\bar{\mu}(1-\bar{\mu}))$ for logit). Report effective dimensionality $p_{\text{eff}}^{(s)} = \sum_j (1 - \kappa_j^{(s)})$ and inclusion probability $P_{\text{active}}(\beta_j) = P(\kappa_j < 0.5)$.

### D.3 Uncertainty Propagation to Derived & Downstream Quantities
Posterior uncertainty must reach every quantity that is reported or passed on to other models and pipeline steps, or its absence must be declared.

1. **Compute derived quantities draw by draw.** For any function $g$ of the parameters (probabilities, shares, elasticities, marginal effects, totals, ratios, spatial impacts), compute $g(\theta^{(s)})$ for every draw and summarize across draws. For nonlinear $g$, $g(\mathbb{E}[\theta]) \ne \mathbb{E}[g(\theta)]$. For example, the choice probability computed from posterior-mean coefficients is not the posterior-mean choice probability, and it is biased towards the extremes.
2. **Passing results to downstream steps: two options.**
   - **Ensemble (optional):** propagate $S'$ posterior draws through every downstream step, one complete run per draw.
     - Each member uses one joint draw of all parameters. Never mix draws across parameters, which would break their dependence.
     - Choose $S'$ so that the Monte Carlo error of the reported summaries is small relative to their spread ($\text{MCSE}/\text{SD}$, C.1). Use an evenly thinned subset of the posterior.
     - Summarize downstream outputs across members only after each member is complete: quantiles, probabilities of events, agreement.
   - **Declared point estimate (default):** pass one point estimate downstream, and declare it in lineage and in the outputs.
     - Use the posterior mean of the quantity the downstream step consumes, e.g. the average over draws of each pixel's class probabilities, $\bar{p}_{ik} = \frac{1}{S}\sum_s p_{ik}(\theta^{(s)})$, not $p_{ik}(\bar{\theta})$.
     - Downstream outputs then carry no parameter uncertainty. Reports must say so, and must not present their spread across scenarios as parameter uncertainty.
3. **Name the sources of uncertainty included.** Parameter uncertainty is one source; others are input data (e.g. classification accuracy of the inputs), structural choices (model specification, processing settings), and scenarios. State which are propagated, which are assessed by sensitivity analysis, and which are not quantified.
4. **Report derived quantities with intervals** (D.1), and, for ensembles, with the Monte Carlo error from the finite ensemble size.

