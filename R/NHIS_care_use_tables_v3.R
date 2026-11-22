# =============================================================================
# Care use among community-dwelling adults aged 65 years and older with
# swallowing problems: NHIS 2022 and 2025
#
# Draft version 3 (19.11.2026): as version 2, plus reliability checks for the
# proportions in Table 1 following the NCHS Data Presentation Standards for
# Proportions (effective sample size, Korn-Graubard 95% CI, degrees of freedom).
# The reported confidence intervals remain the logit intervals; only the flag in
# Table 1 is affected. All other tables are unchanged.
#
# Draft version 2 (17.11.2026): reproduces Figures 1 and 2, Tables 1 and 2 and S1-S8 in the
# numbering of the manuscript draft. Changes against NHIS_care_use_swallowing_tables.R:
#   - reduced model (Table 2, S5) with difficulty seeing as a separate block
#   - family income in four categories (Table S6)
#   - supplementary tables renumbered in order of citation; the table of
#     missing values and estimates quoted only in the text are written as Check_*.csv
#
# Input (public-use files, https://www.cdc.gov/nchs/nhis/documentation/):
#   <data_dir>/NHIS 2022/adult22.csv, adultinc22.csv
#   <data_dir>/NHIS 2025/adult25.csv, adultinc25.csv
# Output: one CSV file per table in <data_dir>/Draft final/Results
#
# Required packages: survey, mitools
# =============================================================================

for (p in c("survey", "mitools")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(survey)
library(mitools)
options(survey.lonely.psu = "adjust")

data_dir <- Sys.getenv("NHIS_DIR", "~/Desktop/NHAT")
out_dir  <- file.path(data_dir, "Draft final", "Results")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
in_file  <- function(year, file) file.path(data_dir, paste("NHIS", year), file)

M <- 10                                   # number of income imputations
inc4_levels <- c(">=400%", "200-399%", "100-199%", "<100%")   # % of federal poverty level

# -----------------------------------------------------------------------------
# Data preparation
# -----------------------------------------------------------------------------
# NHIS codes: 1 yes, 2 no, 7/8/9 refused/not ascertained/don't know
yes_no <- function(x) ifelse(x %in% 1, 1, ifelse(x %in% 2, 0, NA))
# Washington Group items: 1 no difficulty, 2 some, 3 a lot, 4 cannot do
any_difficulty <- function(x) ifelse(x %in% 1, 0, ifelse(x %in% 2:4, 1, NA))

read_income <- function(year, hhx) {
  inc <- read.csv(in_file(year, sprintf("adultinc%02d.csv", year %% 100)))
  out <- data.frame(row.names = seq_along(hhx))
  for (m in 1:M) {
    im <- inc[inc$IMPNUM_A == m, c("HHX", "RATCAT_A")]
    stopifnot(!anyDuplicated(im$HHX), all(hhx %in% im$HHX))
    r <- im$RATCAT_A[match(hhx, im$HHX)]
    stopifnot(all(r %in% 1:14))
    out[[paste0("inc_lt200_", m)]] <- as.integer(r <= 7)
    # RATCAT_A: 1-3 <100%, 4-7 100-199%, 8-11 200-399%, 12-14 >=400% FPL
    out[[paste0("inc4_", m)]] <- factor(ifelse(r <= 3, "<100%", ifelse(r <= 7, "100-199%",
                                        ifelse(r <= 11, "200-399%", ">=400%"))), levels = inc4_levels)
  }
  out
}

read_year <- function(year) {
  d <- read.csv(in_file(year, sprintf("adult%02d.csv", year %% 100)))
  voice <- if (year == 2022) d$VSLVYR_A else d$VSLVYR1_A
  self  <- if (year == 2022) d$AVAIL_A %in% 1 else d$PROXYFLAG_A %in% 2

  x <- data.frame(
    year = year,
    w = d$WTFA_A / 2, strat = d$PSTRAT, psu = d$PPSU,
    age = d$AGEP_A,
    domain = d$AGEP_A >= 65 & d$AGEP_A <= 85 & d$VSLSWYR_A %in% 1,
    care = yes_no(d$VSLPA_A),

    year2025 = as.integer(year == 2025),
    age3 = factor(ifelse(d$AGEP_A %in% 65:74, "65-74", ifelse(d$AGEP_A %in% 75:84, "75-84",
                  ifelse(d$AGEP_A == 85, "85+", NA))), levels = c("65-74", "75-84", "85+")),
    female = ifelse(d$SEX_A %in% 2, 1, ifelse(d$SEX_A %in% 1, 0, NA)),
    educ3 = factor(ifelse(d$EDUCP_A %in% 8:10, "Bachelor+", ifelse(d$EDUCP_A %in% 5:7, "SomeCollege",
                   ifelse(d$EDUCP_A %in% 0:4, "HighSchoolOrLess", NA))),
                   levels = c("Bachelor+", "SomeCollege", "HighSchoolOrLess")),
    cost_barrier = ifelse(d$MEDDL12M_A %in% 1 | d$MEDNG12M_A %in% 1, 1,
                   ifelse(d$MEDDL12M_A %in% 2 & d$MEDNG12M_A %in% 2, 0, NA)),

    stroke       = yes_no(d$STREV_A),
    hypertension = yes_no(d$HYPEV_A),
    diabetes     = yes_no(d$DIBEV_A),

    dementia = yes_no(d$DEMENEV_A),
    memory3 = factor(ifelse(d$COGMEMDFF_A %in% 1, "None", ifelse(d$COGMEMDFF_A %in% 2, "Some",
                     ifelse(d$COGMEMDFF_A %in% 3:4, "ALotOrCannot", NA))),
                     levels = c("None", "Some", "ALotOrCannot")),

    seeing        = any_difficulty(d$VISIONDF_A),
    hearing       = any_difficulty(d$HEARINGDF_A),
    mobility      = any_difficulty(d$DIFF_A),
    communication = any_difficulty(d$COMDIFF_A),

    severe_alot = ifelse(d$VSLSWDIF_A %in% 2, 1, ifelse(d$VSLSWDIF_A %in% c(1, 3), 0, NA)),
    duration7d  = ifelse(d$VSLSWDYS_A %in% 2:6, 1, ifelse(d$VSLSWDYS_A %in% 1, 0, NA)),

    other_vsl   = as.integer(voice %in% 1 | d$VSLSPYR_A %in% 1 | d$VSLLGYR_A %in% 1),
    speech_lang = as.integer(d$VSLSPYR_A %in% 1 | d$VSLLGYR_A %in% 1),
    self_report = as.integer(self)
  )
  x$cog_any <- ifelse(x$dementia %in% 1 | d$COGMEMDFF_A %in% 2:4 | d$COMDIFF_A %in% 2:4, 1,
               ifelse(x$dementia %in% 0 & d$COGMEMDFF_A %in% 1 & d$COMDIFF_A %in% 1, 0, NA))
  x$age75   <- ifelse(is.na(x$age3), NA, as.integer(x$age3 != "65-74"))
  x$educ_hs <- ifelse(is.na(x$educ3), NA, as.integer(x$educ3 == "HighSchoolOrLess"))
  x$cog3 <- factor(ifelse(x$dementia %in% 1, "Dementia",
                   ifelse(x$memory3 %in% c("Some", "ALotOrCannot"), "MemoryNoDiagnosis",
                   ifelse(x$memory3 %in% "None", "None", NA))),
                   levels = c("None", "MemoryNoDiagnosis", "Dementia"))
  cbind(x, read_income(year, d$HHX))
}

D <- rbind(read_year(2022), read_year(2025))

model_vars <- c("care", "year2025", "age3", "female", "educ3", "cost_barrier",
                "stroke", "hypertension", "diabetes", "dementia", "memory3",
                "seeing", "hearing", "mobility", "communication", "severe_alot", "duration7d")
D$cc      <- D$domain & complete.cases(D[, model_vars])
D$cc_swal <- D$cc & D$other_vsl == 0
D$cc_self <- D$cc & D$self_report == 1

des_all <- svydesign(ids = ~psu, strata = ~strat, weights = ~w, nest = TRUE, data = D)
des     <- subset(des_all, cc)
df_des  <- degf(des)

# -----------------------------------------------------------------------------
# Model functions
# Survey-weighted modified Poisson regression (log link), fitted in each income
# imputation and combined by Rubin's rules (Barnard-Rubin degrees of freedom).
# "INC" in a formula is replaced by inc_lt200_m in imputation m.
# -----------------------------------------------------------------------------
fit_mi <- function(rhs, design) {
  fits <- lapply(1:M, function(m) {
    f <- as.formula(paste("care ~", gsub("INC", paste0("inc_lt200_", m), rhs)))
    svyglm(f, design = design, family = quasipoisson(link = "log"))
  })
  b <- lapply(fits, function(f) { x <- coef(f); names(x) <- sub("inc_lt200_[0-9]+", "inc_lt200", names(x)); x })
  v <- lapply(fits, function(f) { x <- vcov(f); n <- sub("inc_lt200_[0-9]+", "inc_lt200", rownames(x))
                                  dimnames(x) <- list(n, n); x })
  list(comb = MIcombine(b, v, df.complete = degf(design)),
       converged = all(sapply(fits, function(f) f$converged)),
       max_fitted = max(sapply(fits, function(f) max(fitted(f)))))
}

pr_table <- function(fm, model) {
  k <- fm$comb; b <- coef(k); se <- sqrt(diag(vcov(k))); q <- qt(0.975, k$df)
  data.frame(model = model, term = names(b), PR = exp(b),
             CI_lower = exp(b - q * se), CI_upper = exp(b + q * se), row.names = NULL)[-1, ]
}

# Sequential blocks; each factor is reported from the model in which its block enters
sequential <- function(blocks, design, analysis) {
  rhs <- character(0); full <- list(); entry <- list(); diag <- list()
  for (i in seq_along(blocks)) {
    rhs <- c(rhs, blocks[[i]])
    fm  <- fit_mi(paste(rhs, collapse = " + "), design)
    tab <- pr_table(fm, paste0("M", i))
    full[[i]]  <- tab
    entry[[i]] <- tab[sapply(tab$term, function(t)
      any(startsWith(t, sub("INC", "inc_lt200", blocks[[i]])))), ]
    diag[[i]] <- data.frame(analysis = analysis, model = paste0("M", i),
                            n = sum(design$prob < Inf),
                            n_care = sum(design$variables$care[design$prob < Inf]),
                            parameters = length(coef(fm$comb)) - 1,
                            converged = fm$converged, max_fitted = fm$max_fitted)
  }
  list(full = do.call(rbind, full), entry = do.call(rbind, entry), diag = do.call(rbind, diag))
}

blocks <- list(
  c("year2025", "age3", "female", "educ3", "INC", "cost_barrier"),
  c("stroke", "hypertension", "diabetes"),
  c("dementia", "memory3"),
  c("seeing", "hearing", "mobility", "communication"),
  c("severe_alot", "duration7d"))
blocks_slim <- list(
  c("year2025", "age75", "female", "educ_hs", "INC"),
  c("stroke", "hypertension", "diabetes"),
  c("cog_any"),
  c("seeing"),
  c("severe_alot", "duration7d"))
blocks_cog3 <- blocks; blocks_cog3[[3]] <- "cog3"

term_labels <- c(
  year2025 = "Survey year 2025 (ref. 2022)",
  `age375-84` = "Age 75-84 years (ref. 65-74)", `age385+` = "Age >=85 years (ref. 65-74)",
  female = "Female (ref. male)",
  educ3SomeCollege = "Education: some college/associate (ref. >= bachelor's)",
  educ3HighSchoolOrLess = "Education: <= high school (ref. >= bachelor's)",
  inc_lt200 = "Family income <200% FPL (ref. >=200%)",
  cost_barrier = "Cost barrier to medical care",
  stroke = "Stroke", hypertension = "Hypertension", diabetes = "Diabetes", dementia = "Dementia",
  memory3Some = "Memory/concentration: some difficulty (ref. none)",
  memory3ALotOrCannot = "Memory/concentration: a lot/cannot do (ref. none)",
  seeing = "Difficulty seeing", hearing = "Difficulty hearing",
  mobility = "Difficulty walking or climbing steps", communication = "Difficulty communicating",
  severe_alot = "Severity 'a lot' (ref. 'a little'/'somewhere in between')",
  duration7d = "Duration >=7 days (ref. <7 days)",
  age75 = "Age >=75 years (ref. 65-74)", educ_hs = "Education <= high school (ref. higher)",
  cog_any = "Dementia or any difficulty remembering/concentrating or communicating",
  cog3MemoryNoDiagnosis = "Memory difficulty without dementia diagnosis (ref. no cognitive problem)",
  cog3Dementia = "Dementia diagnosis (ref. no cognitive problem)")
add_label <- function(x) { x$label <- unname(term_labels[x$term]); x[, c("model", "term", "label", setdiff(names(x), c("model", "term", "label")))] }

rnd <- function(x, digits = 2) { i <- sapply(x, is.numeric); x[i] <- lapply(x[i], round, digits); x }
write_table <- function(x, name, digits = 2) write.csv(rnd(x, digits), file.path(out_dir, paste0(name, ".csv")), row.names = FALSE)

prop_ci <- function(design) {
  q <- svyciprop(~care, design, method = "logit")
  100 * c(as.numeric(coef(q)), as.numeric(confint(q)))
}

# Reliability of a proportion following the NCHS Data Presentation Standards for
# Proportions (Parker et al. 2017): effective sample size (capped at n), domain
# degrees of freedom (PSUs minus strata with domain members) and Korn-Graubard 95% CI
# (Clopper-Pearson interval based on the degrees-of-freedom adjusted effective sample size).
dom_df <- function(design) {
  v <- design$variables[design$prob < Inf, ]
  length(unique(paste(v$strat, v$psu))) - length(unique(v$strat))
}
kg_check <- function(design) {
  r <- svymean(~care, design); p <- as.numeric(coef(r)); se <- as.numeric(SE(r))
  n <- sum(design$prob < Inf); df <- dom_df(design)
  n_eff <- if (se > 0) min(n, p * (1 - p) / se^2) else n
  n_df  <- if (df > 0) n_eff * (qt(0.975, n - 1) / qt(0.975, df))^2 else n_eff
  x <- p * n_df
  lo <- if (x > 0) qbeta(0.025, x, n_df - x + 1) else 0
  hi <- if (x < n_df) qbeta(0.975, x + 1, n_df - x) else 1
  c(n_eff = n_eff, df = df, KG_lower = 100 * lo, KG_upper = 100 * hi)
}
# "*": n or effective n < 30, Korn-Graubard CI width >= 30 percentage points,
# width > 5 points with relative width > 130%, or fewer than 8 degrees of freedom
nchs_flag <- function(n, n_eff, df, lo, hi, p) {
  w <- hi - lo
  ifelse(n < 30 | n_eff < 30 | w >= 30 | (w > 5 & w / p > 1.3) | df < 8, "*", "")
}

# -----------------------------------------------------------------------------
# Figure 1
# -----------------------------------------------------------------------------
steps <- list(
  "Sample adults, NHIS 2022 and 2025"           = rep(TRUE, nrow(D)),
  "Aged 65 years and older (numeric age)"        = D$age >= 65 & D$age <= 85,
  "Swallowing problems in the past 12 months"    = D$domain,
  "Valid response on care use"                   = D$domain & !is.na(D$care),
  "Complete data on all model variables"         = D$cc,
  "Swallowing problems only"                     = D$cc_swal,
  "Self-reported interview"                      = D$cc_self)
fig1 <- do.call(rbind, lapply(names(steps), function(s) { i <- steps[[s]] %in% TRUE
  data.frame(step = s, n = sum(i), n_2022 = sum(i & D$year == 2022), n_2025 = sum(i & D$year == 2025),
             n_care = if (s %in% names(steps)[1:3]) NA else sum(D$care[i])) }))
write_table(fig1, "Figure_1")

# -----------------------------------------------------------------------------
# Table 1
# Unweighted n, weighted column percentages, weighted proportion with care use.
# Income: percentages averaged over imputations, n from imputation 1,
# proportion with care use and CI combined by Rubin's rules on the logit scale.
# flag "*": NCHS Data Presentation Standards for Proportions (see nchs_flag);
# for income, effective n and df are the minimum and the Korn-Graubard limits the
# mean over the ten imputations.
# -----------------------------------------------------------------------------
t1_vars <- c("Survey year 2025" = "year2025", "Age" = "age3", "Female" = "female",
             "Education" = "educ3", "Family income <200% FPL" = "INC",
             "Cost barrier to medical care" = "cost_barrier", "Stroke" = "stroke",
             "Hypertension" = "hypertension", "Diabetes" = "diabetes", "Dementia" = "dementia",
             "Difficulty remembering or concentrating" = "memory3",
             "Difficulty seeing" = "seeing", "Difficulty hearing" = "hearing",
             "Difficulty walking or climbing steps" = "mobility",
             "Difficulty communicating" = "communication", "Severity 'a lot'" = "severe_alot",
             "Duration >=7 days" = "duration7d",
             "Additional voice, speech, or language problems" = "other_vsl",
             "Self-reported interview" = "self_report")
dv <- des$variables[des$prob < Inf, ]
set_ind <- function(design, ind) { design$variables$ind <- NA; design$variables$ind[design$prob < Inf] <- ind; design }
row_t1 <- function(label, category, ind, design) {
  d <- set_ind(design, ind)
  pc <- coef(svyby(~ind, ~care, d, svymean))
  c(n_total = sum(ind == 1), pct_total = 100 * as.numeric(coef(svymean(~ind, d))),
    n_care = sum(ind == 1 & dv$care == 1), pct_care = 100 * as.numeric(pc[2]),
    n_no_care = sum(ind == 1 & dv$care == 0), pct_no_care = 100 * as.numeric(pc[1]))
}
t1 <- list()
ov <- prop_ci(des)
t1[[1]] <- data.frame(characteristic = "Analytic sample", category = "", n_total = nrow(dv), pct_total = 100,
                      n_care = sum(dv$care), pct_care = 100, n_no_care = sum(dv$care == 0), pct_no_care = 100,
                      care_pct = ov[1], care_CI_lower = ov[2], care_CI_upper = ov[3], t(kg_check(des)))
for (lab in names(t1_vars)) {
  v <- t1_vars[[lab]]
  if (v == "INC") {
    s <- rowMeans(sapply(1:M, function(m) row_t1(lab, "", dv[[paste0("inc_lt200_", m)]], des)))
    s[c("n_total", "n_care", "n_no_care")] <- row_t1(lab, "", dv$inc_lt200_1, des)[c(1, 3, 5)]
    est <- t(sapply(1:M, function(m) {
      d <- set_ind(des, dv[[paste0("inc_lt200_", m)]])
      r <- svymean(~care, subset(d, ind == 1)); p <- as.numeric(coef(r))
      c(qlogis(p), (as.numeric(SE(r)) / (p * (1 - p)))^2) }))
    k <- MIcombine(as.list(est[, 1]), as.list(est[, 2]), df.complete = df_des)
    tq <- qt(0.975, k$df); se <- sqrt(vcov(k)[1])
    q <- 100 * plogis(coef(k) + c(0, -1, 1) * tq * se)
    kg <- t(sapply(1:M, function(m) kg_check(subset(set_ind(des, dv[[paste0("inc_lt200_", m)]]), ind == 1))))
    KG <- c(n_eff = min(kg[, "n_eff"]), df = min(kg[, "df"]),
            KG_lower = mean(kg[, "KG_lower"]), KG_upper = mean(kg[, "KG_upper"]))
    t1[[length(t1) + 1]] <- data.frame(characteristic = lab, category = "yes", t(s),
                                       care_pct = q[1], care_CI_lower = q[2], care_CI_upper = q[3], t(KG))
    next
  }
  x <- dv[[v]]; lev <- if (is.factor(x)) levels(x) else 1
  for (l in lev) {
    ind <- as.integer(if (is.factor(x)) x == l else x == 1)
    dsub <- subset(set_ind(des, ind), ind == 1)
    q <- prop_ci(dsub); KG <- kg_check(dsub)
    t1[[length(t1) + 1]] <- data.frame(characteristic = lab, category = if (is.factor(x)) l else "yes",
                                       t(row_t1(lab, l, ind, des)),
                                       care_pct = q[1], care_CI_lower = q[2], care_CI_upper = q[3], t(KG))
  }
  if (v == "dementia") for (s in c(1, 0)) {
    ind <- as.integer(dv$dementia == 1 & dv$self_report == s)
    dsub <- subset(set_ind(des, ind), ind == 1)
    q <- prop_ci(dsub); KG <- kg_check(dsub)
    t1[[length(t1) + 1]] <- data.frame(characteristic = lab,
      category = if (s == 1) "self-reported interview" else "proxy-reported interview",
      n_total = sum(ind), pct_total = NA, n_care = sum(ind & dv$care == 1), pct_care = NA,
      n_no_care = sum(ind & dv$care == 0), pct_no_care = NA,
      care_pct = q[1], care_CI_lower = q[2], care_CI_upper = q[3], t(KG))
  }
}
t1 <- do.call(rbind, t1); rownames(t1) <- NULL
t1$flag <- nchs_flag(t1$n_total, t1$n_eff, t1$df, t1$KG_lower, t1$KG_upper, t1$care_pct)
write_table(t1, "Table_1", 1)

# -----------------------------------------------------------------------------
# Models
# -----------------------------------------------------------------------------
primary <- sequential(blocks,      des,                          "Table 2")
swal    <- sequential(blocks_slim, subset(des_all, cc_swal),     "Table 2, swallowing problems only")
ref     <- sequential(blocks_slim, des,                          "Table 2, analytic sample")
selfrep <- sequential(blocks,      subset(des_all, cc_self),     "Table S4")
cog3    <- sequential(blocks_cog3, des,                          "Table S5")

# -----------------------------------------------------------------------------
# Table 2
# -----------------------------------------------------------------------------
write_table(add_label(primary$entry), "Figure_2")

# -----------------------------------------------------------------------------
# Table 2
# -----------------------------------------------------------------------------
clin <- c("stroke", "cog_any", "seeing", "severe_alot", "duration7d")
side_by_side <- function(a, b) {
  x <- merge(a[, c("model", "term", "PR", "CI_lower", "CI_upper")],
             b[, c("term", "PR", "CI_lower", "CI_upper")], by = "term",
             suffixes = c("_swallowing_only", "_analytic_sample"), sort = FALSE)
  x <- x[match(a$term, x$term), ]
  add_label(x)
}
t3 <- side_by_side(swal$entry[swal$entry$term %in% clin, ], ref$entry[ref$entry$term %in% clin, ])
p_swal <- prop_ci(subset(des_all, cc_swal)); p_all <- prop_ci(des)
t3 <- rbind(data.frame(model = "", term = "care_pct", label = "Proportion with care use, % (95% CI)",
                       PR_swallowing_only = p_swal[1], CI_lower_swallowing_only = p_swal[2],
                       CI_upper_swallowing_only = p_swal[3], PR_analytic_sample = p_all[1],
                       CI_lower_analytic_sample = p_all[2], CI_upper_analytic_sample = p_all[3]), t3)
rownames(t3) <- NULL
write_table(t3, "Table_2")

# -----------------------------------------------------------------------------
# Table S1
# -----------------------------------------------------------------------------
s1 <- data.frame(rbind(
  c("Inclusion", "Age", "AGEP_A", "65-85 (85 = 85 and older)"),
  c("Inclusion", "Swallowing problems, past 12 months", "VSLSWYR_A", "1 = yes"),
  c("Outcome", "Care use, past 12 months", "VSLPA_A", "1 = yes vs. 2 = no"),
  c("M1", "Survey year", "file", "2025 vs. 2022"),
  c("M1", "Age", "AGEP_A", "65-74 (ref.), 75-84, 85+"),
  c("M1", "Sex", "SEX_A", "2 female vs. 1 male"),
  c("M1", "Education", "EDUCP_A", "0-4 <= high school, 5-7 some college/associate, 8-10 >= bachelor's (ref.)"),
  c("M1", "Family income", "RATCAT_A (adultinc files, IMPNUM_A 1-10)", "1-7 <200% vs. 8-14 >=200% FPL"),
  c("M1", "Cost barrier to medical care", "MEDDL12M_A, MEDNG12M_A", "yes if either = 1; no if both = 2"),
  c("Table S6", "Family income, four categories", "RATCAT_A (adultinc files, IMPNUM_A 1-10)",
    "1-3 <100%, 4-7 100-199%, 8-11 200-399%, 12-14 >=400% FPL (ref.)"),
  c("M2", "Stroke, hypertension, diabetes", "STREV_A, HYPEV_A, DIBEV_A", "1 = yes vs. 2 = no"),
  c("M3", "Dementia", "DEMENEV_A", "1 = yes vs. 2 = no"),
  c("M3", "Memory/concentration", "COGMEMDFF_A", "1 none (ref.), 2 some, 3-4 a lot/cannot do"),
  c("M4", "Seeing, hearing, walking/climbing steps, communicating",
    "VISIONDF_A, HEARINGDF_A, DIFF_A, COMDIFF_A", "2-4 any difficulty vs. 1 no difficulty"),
  c("M5", "Severity", "VSLSWDIF_A", "2 'a lot' vs. 1 'a little' or 3 'somewhere in between'"),
  c("M5", "Duration", "VSLSWDYS_A", "2-6 >=7 days vs. 1 <7 days"),
  c("Other", "Voice, speech, language problems", "VSLVYR_A (2022) / VSLVYR1_A (2025), VSLSPYR_A, VSLLGYR_A",
    "none = 1: swallowing problems only"),
  c("Other", "Self-reported interview", "AVAIL_A (2022), PROXYFLAG_A (2025)", "AVAIL_A = 1 / PROXYFLAG_A = 2"),
  c("Design", "Weight, strata, PSU", "WTFA_A, PSTRAT, PPSU", "WTFA_A / 2; strata and PSUs unchanged")))
names(s1) <- c("block", "variable", "nhis_variable", "coding")
write_table(s1, "Table_S1")

# -----------------------------------------------------------------------------
# Missing values (reported in the text; not a submitted table)
# -----------------------------------------------------------------------------
dd <- D[D$domain, ]
s2 <- data.frame(variable = model_vars, n_missing = sapply(dd[, model_vars], function(x) sum(is.na(x))))
s2 <- rbind(s2, data.frame(variable = "income (imputed)", n_missing = sum(is.na(dd$inc_lt200_1))))
s2$pct_missing <- 100 * s2$n_missing / nrow(dd)
s2 <- rbind(data.frame(variable = "Swallowing problems, aged 65+", n_missing = nrow(dd), pct_missing = 100),
            s2[order(-s2$n_missing), ],
            data.frame(variable = "Complete for all model variables", n_missing = sum(dd$cc),
                       pct_missing = 100 * mean(dd$cc)))
names(s2)[2:3] <- c("n", "pct"); rownames(s2) <- NULL
write_table(s2, "Check_missing_values", 1)

# -----------------------------------------------------------------------------
# Table S2
# -----------------------------------------------------------------------------
s3 <- add_label(primary$full)
s3 <- reshape(s3, idvar = c("term", "label"), timevar = "model", direction = "wide")
write_table(s3, "Table_S2")

# -----------------------------------------------------------------------------
# Table S7
# Predictive margins (survey::svypredmeans) from survey-weighted logistic models
# with the same adjustment as the model in which the factor's block enters;
# combined over imputations by Rubin's rules. Rows outside the analytic sample
# carry placeholder values (weight 0 in the subset design).
# -----------------------------------------------------------------------------
Dm <- D
bin <- setdiff(unlist(blocks), c("age3", "educ3", "memory3", "INC"))
for (v in bin) Dm[[paste0("f_", v)]] <- factor(Dm[[v]], levels = c(0, 1), labels = c("no", "yes"))
for (m in 1:M) Dm[[paste0("f_inc_", m)]] <- factor(Dm[[paste0("inc_lt200_", m)]], levels = c(0, 1),
                                                   labels = c(">=200%", "<200%"))
fac_var <- function(v, m) if (v == "INC") paste0("f_inc_", m) else if (v %in% bin) paste0("f_", v) else v
adj_var <- function(v, m) if (v == "INC") paste0("inc_lt200_", m) else v
fill <- c("care", setdiff(unlist(blocks), "INC"), grep("^(f_|inc_lt200_)", names(Dm), value = TRUE))
for (v in fill) { x <- Dm[[v]]; i <- !Dm$cc & is.na(x)
  if (any(i)) { x[i] <- if (is.factor(x)) levels(x)[1] else 0; Dm[[v]] <- x } }
dm <- subset(svydesign(ids = ~psu, strata = ~strat, weights = ~w, nest = TRUE, data = Dm), cc)

s4 <- list()
for (k in seq_along(blocks)) for (v in blocks[[k]]) {
  adj <- setdiff(unlist(blocks[1:k]), v)
  res <- lapply(1:M, function(m) {
    fit <- svyglm(as.formula(paste("care ~", paste(sapply(adj, adj_var, m = m), collapse = " + "))),
                  design = dm, family = quasibinomial())
    pm  <- svypredmeans(fit, as.formula(paste0("~", fac_var(v, m))))
    lev <- levels(Dm[[fac_var(v, m)]]); lev <- lev[lev %in% names(coef(pm))]
    idx <- match(lev, names(coef(pm)))
    C <- rbind(diag(length(lev)), t(sapply(lev[-1], function(l) as.numeric(lev == l) - as.numeric(lev == lev[1]))))
    nm <- c(paste0("pct|", lev), paste0("diff|", lev[-1]))
    est <- setNames(as.numeric(C %*% coef(pm)[idx]), nm)
    V <- C %*% vcov(pm)[idx, idx] %*% t(C); dimnames(V) <- list(nm, nm)
    list(est = est, V = V)
  })
  kc <- MIcombine(lapply(res, `[[`, "est"), lapply(res, `[[`, "V"), df.complete = df_des)
  b <- coef(kc); se <- sqrt(diag(vcov(kc))); q <- qt(0.975, kc$df)
  r <- data.frame(model = paste0("M", k), factor = ifelse(v == "INC", "income", v),
                  type = sub("\\|.*", "", names(b)), category = sub(".*\\|", "", names(b)),
                  est = 100 * b, lo = 100 * (b - q * se), hi = 100 * (b + q * se), row.names = NULL)
  a <- r[r$type == "pct", ]; d <- r[r$type == "diff", ]
  s4[[length(s4) + 1]] <- data.frame(model = a$model, factor = a$factor, category = a$category,
    adjusted_pct = a$est, pct_CI_lower = a$lo, pct_CI_upper = a$hi,
    difference_pp = c(NA, d$est), diff_CI_lower = c(NA, d$lo), diff_CI_upper = c(NA, d$hi))
}
write_table(do.call(rbind, s4), "Table_S7", 1)

# -----------------------------------------------------------------------------
# Table S6
# Family income in four categories (reference >=400% FPL): weighted proportion
# with care use (Rubin's rules on the logit scale) and prevalence ratios from
# M1 and M5 of the primary analysis with the four-category variable.
# -----------------------------------------------------------------------------
set_inc4 <- function(design, m) { design$variables$inc4 <- design$variables[[paste0("inc4_", m)]]; design }
fit_inc4 <- function(rhs, suffix) {
  fits <- lapply(1:M, function(m) svyglm(as.formula(paste("care ~", rhs)), design = set_inc4(des, m),
                                         family = quasipoisson(link = "log")))
  stopifnot(all(sapply(fits, function(f) f$converged)))
  k <- MIcombine(lapply(fits, coef), lapply(fits, vcov), df.complete = df_des)
  b <- coef(k); se <- sqrt(diag(vcov(k))); q <- qt(0.975, k$df); i <- grepl("^inc4", names(b))
  x <- data.frame(income = sub("^inc4", "", names(b)[i]), PR = exp(b[i]),
                  CI_lower = exp(b[i] - q[i] * se[i]), CI_upper = exp(b[i] + q[i] * se[i]), row.names = NULL)
  names(x)[-1] <- paste0(names(x)[-1], "_", suffix); x
}
pr_m1 <- fit_inc4(sub("INC", "inc4", paste(blocks[[1]], collapse = " + ")), "M1")
pr_m5 <- fit_inc4(sub("INC", "inc4", paste(unlist(blocks), collapse = " + ")), "M5")
est <- lapply(1:M, function(m) {
  r <- svyby(~care, ~inc4, set_inc4(des, m), svymean)
  p <- setNames(as.numeric(coef(r)), as.character(r$inc4)); se <- as.numeric(SE(r))
  list(b = qlogis(p), v = diag((se / (p * (1 - p)))^2, nrow = length(p)))
})
k  <- MIcombine(lapply(est, `[[`, "b"), lapply(est, `[[`, "v"), df.complete = df_des)
tq <- qt(0.975, k$df); se <- sqrt(diag(vcov(k)))
s4inc <- data.frame(income = names(coef(k)),
                    n_imputation1 = as.integer(table(dv$inc4_1)[names(coef(k))]),
                    n_care_imputation1 = as.integer(tapply(dv$care, dv$inc4_1, sum)[names(coef(k))]),
                    care_pct = 100 * plogis(coef(k)), care_CI_lower = 100 * plogis(coef(k) - tq * se),
                    care_CI_upper = 100 * plogis(coef(k) + tq * se), row.names = NULL)
s4inc <- merge(merge(s4inc, pr_m1, by = "income", all.x = TRUE), pr_m5, by = "income", all.x = TRUE)
s4inc <- s4inc[match(inc4_levels, s4inc$income), ]; rownames(s4inc) <- NULL
pct <- c("care_pct", "care_CI_lower", "care_CI_upper")
s4inc[pct] <- round(s4inc[pct], 1)
write_table(s4inc, "Table_S6")

# -----------------------------------------------------------------------------
# Estimates quoted in the text only (not submitted tables)
# Care use by severity and stroke, in the analytic sample and among respondents
# with swallowing problems only; speech or language problems by difficulty
# communicating.
# -----------------------------------------------------------------------------
chk <- list()
for (pop in c("analytic sample", "swallowing problems only")) for (v in c("severe_alot", "stroke")) {
  D$sel <- (if (pop == "analytic sample") D$cc else D$cc_swal) & D[[v]] %in% 1
  q <- prop_ci(subset(svydesign(ids = ~psu, strata = ~strat, weights = ~w, nest = TRUE, data = D), sel))
  chk[[length(chk) + 1]] <- data.frame(population = pop, factor = v, n = sum(D$sel), n_care = sum(D$care[D$sel]),
                                       care_pct = q[1], care_CI_lower = q[2], care_CI_upper = q[3])
}
write_table(do.call(rbind, chk), "Check_care_use_subgroups", 1)
r  <- svyby(~speech_lang, ~communication, des, svymean); ci <- confint(r, df = df_des)
write_table(data.frame(communication_difficulty = c("no", "yes")[r$communication + 1],
                       speech_lang_pct = 100 * as.numeric(coef(r)), CI_lower = 100 * ci[, 1],
                       CI_upper = 100 * ci[, 2], row.names = NULL), "Check_communication_speech_language", 1)

# -----------------------------------------------------------------------------
# Table S3
# -----------------------------------------------------------------------------
write_table(side_by_side(swal$entry, ref$entry), "Table_S3")

# -----------------------------------------------------------------------------
# Table S4
# -----------------------------------------------------------------------------
write_table(add_label(selfrep$entry), "Table_S4")

# -----------------------------------------------------------------------------
# Table S5
# -----------------------------------------------------------------------------
fm <- fit_mi(paste(unlist(blocks_cog3[1:3]), collapse = " + "), des)
b <- coef(fm$comb); V <- vcov(fm$comb)
a <- "cog3Dementia"; o <- "cog3MemoryNoDiagnosis"
est <- b[a] - b[o]; se <- sqrt(V[a, a] + V[o, o] - 2 * V[a, o]); q <- qt(0.975, min(fm$comb$df[c(a, o)]))
contrast <- data.frame(model = "M3", term = "cog3Dementia_vs_MemoryNoDiagnosis",
                       label = "Dementia diagnosis vs. memory difficulty without diagnosis",
                       PR = exp(est), CI_lower = exp(est - q * se), CI_upper = exp(est + q * se))
s7 <- add_label(cog3$entry[cog3$entry$model %in% "M3", ])
write_table(rbind(s7, contrast), "Table_S5")

# -----------------------------------------------------------------------------
# Table S8
# -----------------------------------------------------------------------------
write_table(rbind(primary$diag, swal$diag, ref$diag, selfrep$diag, cog3$diag), "Table_S8", 3)

writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
cat("Done. Results in", normalizePath(out_dir), "\n")
