# =============================================================================
# Figure 2. Factors associated with care use (forest plot)
# Reads the finished result files; nothing is re-estimated.
#   Draft final/Results/Figure_2.csv  prevalence ratios (entry model of each block)
#   Draft final/Results/Table_S7.csv  adjusted proportions with care use
# Output: Draft final/Tabellen und Abbildungen/Figure_2.png (300 dpi) and, on macOS, Figure_2.pdf
# Base R graphics only, no additional packages.
# =============================================================================
data_dir <- Sys.getenv("NHIS_DIR", "~/Desktop/NHAT")
res_dir  <- file.path(data_dir, "Draft final", "Results")
fig_dir  <- file.path(data_dir, "Draft final", "Tabellen und Abbildungen")
t2 <- read.csv(file.path(res_dir, "Figure_2.csv"))
s3 <- read.csv(file.path(res_dir, "Table_S7.csv"))

# term in Figure_2.csv -> label, and factor / category / reference in Table S7
m <- as.data.frame(rbind(
  c("year2025",              "Survey year 2025 (ref. 2022)",                      "year2025",      "yes",              "no"),
  c("age375-84",             "Age 75–84 years (ref. 65–74)",            "age3",          "75-84",            "65-74"),
  c("age385+",               "Age ≥85 years (ref. 65–74)",              "age3",          "85+",              "65-74"),
  c("female",                "Female (ref. male)",                                "female",        "yes",              "no"),
  c("educ3SomeCollege",      "Some college (ref. ≥ bachelor’s)",        "educ3",         "SomeCollege",      "Bachelor+"),
  c("educ3HighSchoolOrLess", "≤ High school (ref. ≥ bachelor’s)",  "educ3",         "HighSchoolOrLess", "Bachelor+"),
  c("inc_lt200",             "Family income <200% of FPL",                        "income",        "<200%",            ">=200%"),
  c("cost_barrier",          "Cost barrier to medical care",                      "cost_barrier",  "yes",              "no"),
  c("stroke",                "Stroke",                                            "stroke",        "yes",              "no"),
  c("hypertension",          "Hypertension",                                      "hypertension",  "yes",              "no"),
  c("diabetes",              "Diabetes",                                          "diabetes",      "yes",              "no"),
  c("dementia",              "Dementia",                                          "dementia",      "yes",              "no"),
  c("memory3Some",           "Memory/concentration: some difficulty",             "memory3",       "Some",             "None"),
  c("memory3ALotOrCannot",   "Memory/concentration: a lot/cannot do",             "memory3",       "ALotOrCannot",     "None"),
  c("seeing",                "Difficulty seeing",                                 "seeing",        "yes",              "no"),
  c("hearing",               "Difficulty hearing",                                "hearing",       "yes",              "no"),
  c("mobility",              "Difficulty walking or climbing steps",              "mobility",      "yes",              "no"),
  c("communication",         "Difficulty communicating",                          "communication", "yes",              "no"),
  c("severe_alot",           "Severity “a lot”",                        "severe_alot",   "yes",              "no"),
  c("duration7d",            "Problems on ≥7 days",                          "duration7d",    "yes",              "no")),
  stringsAsFactors = FALSE)
names(m) <- c("term", "label", "factor", "cat", "ref")
block_title <- c(M1 = "Sociodemographics and access", M2 = "Vascular factors", M3 = "Cognition",
                 M4 = "Functional limitations", M5 = "Severity and duration")

stopifnot(setequal(m$term, t2$term))
d <- merge(m, t2[, c("model", "term", "PR", "CI_lower", "CI_upper")], by = "term", sort = FALSE)
d <- d[match(m$term, d$term), ]
pct <- function(f, k) { x <- s3$adjusted_pct[s3$factor == f & s3$category == k]; stopifnot(length(x) == 1); x }
d$pct_cat <- mapply(pct, d$factor, d$cat)
d$pct_ref <- mapply(pct, d$factor, d$ref)
f2 <- function(x) formatC(x, format = "f", digits = 2)
f1 <- function(x) formatC(x, format = "f", digits = 1)
d$txt_pr  <- paste0(f2(d$PR), " [", f2(d$CI_lower), ", ", f2(d$CI_upper), "]")
d$txt_pct <- paste0(f1(d$pct_cat), " vs. ", f1(d$pct_ref))
# highlight = TRUE colours factors whose 95% CI excludes 1 and greys the others;
# highlight = FALSE (default) draws all factors alike.
highlight <- FALSE
d$clear   <- if (highlight) d$CI_lower > 1 | d$CI_upper < 1 else TRUE

# vertical positions: block headings and rows
y <- 0; rows <- list()
for (b in names(block_title)) {
  y <- y - 0.5
  rows[[length(rows) + 1]] <- data.frame(y = y, head = TRUE, i = NA, text = block_title[[b]])
  for (i in which(d$model == b)) { y <- y - 1; rows[[length(rows) + 1]] <- data.frame(y = y, head = FALSE, i = i, text = d$label[i]) }
  y <- y - 1
}
rows <- do.call(rbind, rows); y_min <- min(rows$y)

col_main <- "#0072B2"; col_grey <- "#6C7A84"; col_text <- "#1B2A38"; col_head <- "#4A5560"; col_grid <- "#E1E4E7"
x_lab <- 0.00; x_l <- 0.39; x_r <- 0.70; x_pct <- 0.80; x_pr <- 1.00
lim   <- log(c(0.35, 3.2)); ticks <- c(0.5, 1, 2, 3)
sx    <- function(pr) x_l + (log(pr) - lim[1]) / diff(lim) * (x_r - x_l)

draw <- function() {
  par(mar = c(0.3, 0.6, 0.3, 0.6), family = "sans", xpd = NA)
  plot.new(); plot.window(xlim = c(0, 1), ylim = c(y_min - 3.4, 2.5), xaxs = "i", yaxs = "i")
  text(x_lab, 1.5, "Factor", adj = 0, cex = 0.85, col = col_text)
  text(x_pct, 1.85, "Adjusted care use, %", adj = 0.5, cex = 0.85, col = col_text)
  text(x_pct, 1.20, "category vs. reference", adj = 0.5, cex = 0.72, col = col_text)
  text(x_pr,  1.5, "PR [95% CI]", adj = 1, cex = 0.85, col = col_text)
  segments(0, 0.8, 1, 0.8, col = col_head, lwd = 1.2)
  y_axis <- y_min - 1
  segments(sx(ticks), y_axis, sx(ticks), 0.2, col = col_grid, lwd = 1)
  segments(sx(1), y_axis, sx(1), 0.2, col = col_head, lty = 2, lwd = 1.2)
  segments(x_l, y_axis, x_r, y_axis, col = col_head, lwd = 1.2)
  text(sx(ticks), y_axis - 0.8, ticks, cex = 0.8, col = col_text)
  text((x_l + x_r) / 2, y_axis - 2.0, "Prevalence ratio (log scale)", cex = 0.85, col = col_text)
  for (r in seq_len(nrow(rows))) {
    if (rows$head[r]) { text(x_lab, rows$y[r], rows$text[r], adj = 0, font = 2, cex = 0.9, col = col_text); next }
    i <- rows$i[r]; yy <- rows$y[r]; cl <- if (d$clear[i]) col_main else col_grey
    text(x_lab + 0.02, yy, rows$text[r], adj = 0, cex = 0.82, col = col_text, font = if (highlight && d$clear[i]) 2 else 1)
    segments(sx(d$CI_lower[i]), yy, sx(d$CI_upper[i]), yy, col = cl, lwd = 2)
    segments(sx(c(d$CI_lower[i], d$CI_upper[i])), yy - 0.18, sx(c(d$CI_lower[i], d$CI_upper[i])), yy + 0.18, col = cl, lwd = 1.5)
    points(sx(d$PR[i]), yy, pch = 16, cex = 1.15, col = cl)
    text(x_pct, yy, d$txt_pct[i], adj = 0.5, cex = 0.82, col = col_text)
    text(x_pr,  yy, d$txt_pr[i],  adj = 1,   cex = 0.82, col = col_text)
  }
}

dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
png(file.path(fig_dir, "Figure_2.png"), width = 8.5, height = 8.2, units = "in", res = 300); draw(); dev.off()
# PDF copy through the macOS Quartz device (no Cairo/X11 needed); skipped elsewhere
if (capabilities("aqua")) try({
  quartz(file = file.path(fig_dir, "Figure_2.pdf"), type = "pdf", width = 8.5, height = 8.2); draw(); dev.off()
}, silent = TRUE)
cat("Figure 2 written to", normalizePath(fig_dir), "\n")
