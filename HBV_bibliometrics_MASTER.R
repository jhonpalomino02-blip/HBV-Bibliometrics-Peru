# #############################################################################
# HBV_bibliometrics_MASTER.R · SINGLE self-contained master script (PRUNED)
# Q1 bibliometric pipeline · Hepatitis B research in Peru, 1988-2023
# Ph.D.(c) Obert Marin Sanchez et al. · UNMSM · ORCID 0000-0003-2912-1191
# Consolidated: 2026-06-21
# -----------------------------------------------------------------------------
# ONE file = the entire curated pipeline. Reproducible from a clean R session.
# Every figure -> 600 dpi PNG + vector PDF. Only the curated set is exported
# (.FIG_ALLOWLIST); titles/captions stripped (.STRIP_TITLES, journal style).
# Fig 2 = co-word network (the Callon strategic map is NOT exported: it is
# degenerate for this corpus, but Section 03 still runs to build g_coword).
# USAGE:  Rscript HBV_bibliometrics_MASTER.R   (or: bash run_master.sh)
# #############################################################################

.MASTER_START <- Sys.time()
.STRIP_TITLES  <- TRUE
.FIG_ALLOWLIST <- c(
  "annual_production_landmarks",
  "lotka_law",
  "coword_network",
  "thematic_evolution_sankey",
  "world_collaboration_map",
  "institutional_network",
  "geographic_equity_peru",
  "PRISMA_flow",
  "all_keywords_treemap",
  "FigR_F_pub_vs_incidence"
)


#=============================================================================
# >>> SECTION 00 · GLOBAL SETUP
#=============================================================================

# =============================================================================
# 00_setup.R · Global setup for the Q1 bibliometric pipeline
# Study: Hepatitis B research in Peru (1988-2023)
# Author: Ph.D.(c) Obert Marín Sánchez · UNMSM · ORCID 0000-0003-2912-1191
# =============================================================================
#
# PURPOSE
# -------
# Load all dependencies, set the seed, define relative paths, initialise a
# publication-ready graphical theme (sans-serif Arial/Helvetica, theme_classic,
# colour-blind-friendly palettes) and a 600 dpi export helper in vector
# (PDF/EPS) + raster (TIFF/PNG) formats compatible with Q1 journals.

# -----------------------------------------------------------------------------
# 1. R environment configuration ---------------------------------------------
# -----------------------------------------------------------------------------

# NOTE: do NOT run rm(list = ls()) here. This script is source-d from others
# (run_all.R and each module). Clearing the environment would remove variables
# from the caller (e.g. the loop iterator `s` in run_all.R). To start clean,
# restart the R session (Session > Restart in RStudio; Rscript always starts
# with an empty environment).

# Universal encoding (avoids issues with accents in the Spanish database).
# try() because some systems (Windows) do not have this locale installed.
suppressWarnings(try(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"), silent = TRUE))

# Global seed — fixes the reproducible layout of all igraph/ggraph networks.
set.seed(2026)

# Silence deprecation warnings in dplyr summarise (cleaner logs).
options(dplyr.summarise.inform = FALSE)

# -----------------------------------------------------------------------------
# 2. Dependency management ----------------------------------------------------
# -----------------------------------------------------------------------------
# Helper that installs ONLY what is missing — avoids re-installs in CI/CD.

.install_if_missing <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

# Core pipeline packages — grouped by domain.
.pkgs_core <- c(
  # I/O and data manipulation
  "here", "readxl", "writexl", "openxlsx",
  # Tidyverse
  "dplyr", "tidyr", "stringr", "stringi", "purrr", "forcats", "lubridate",
  "tibble", "zoo",
  # Bibliometrics
  "bibliometrix",
  # Visualisation
  "ggplot2", "ggalluvial", "ggrepel", "ggtext", "patchwork", "scales",
  "viridis", "scico", "RColorBrewer", "treemapify",
  # Networks
  "igraph", "ggraph", "tidygraph",
  # Geospatial (collaboration world map)
  "sf", "rnaturalearth", "rnaturalearthdata", "geosphere",
  # Text / fuzzy matching for author and institution cleaning
  "stringdist",
  # Publication-ready tables
  "gt", "gtsummary", "flextable",
  # Q1 vector export
  "ragg", "Cairo", "svglite"
)

.install_if_missing(.pkgs_core)

# Silent loading.
suppressPackageStartupMessages({
  invisible(lapply(.pkgs_core, library, character.only = TRUE))
})

# -----------------------------------------------------------------------------
# 3. Relative paths (here::here) ---------------------------------------------
# -----------------------------------------------------------------------------
# Project structure:
#   HBV/
#   ├── R/                          ← modular scripts
#   ├── data/                       ← immutable .xlsx
#   ├── outputs/
#   │   ├── figures/
#   │   │   ├── main/               ← Figure_1..Figure_5 (manuscript body)
#   │   │   └── supplement/         ← Figure_S1..Figure_S7 (supplementary)
#   │   ├── tables/                 ← XLSX/DOCX
#   │   └── models/                 ← .rds (M object, networks, etc.)
#   └── logs/
.path_data        <- here("data")
.path_figs        <- here("outputs", "figures")
.path_figs_main   <- here("outputs", "figures", "main")
.path_figs_supp   <- here("outputs", "figures", "supplement")
.path_tables      <- here("outputs", "tables")
.path_models      <- here("outputs", "models")
.path_logs        <- here("logs")

# Create directories if they do not exist (idempotent).
invisible(lapply(c(.path_data, .path_figs_main, .path_figs_supp,
                   .path_tables, .path_models, .path_logs),
                 dir.create, showWarnings = FALSE, recursive = TRUE))

# -----------------------------------------------------------------------------
# 4. Publication-ready Q1 ggplot2 theme --------------------------------------
# -----------------------------------------------------------------------------
# Rationale for each choice:
#   - sans-serif (Helvetica/Arial): required by most Q1 biomedical journals.
#   - theme_classic base: removes the grey grids of theme_minimal/theme_grey.
#   - black axis.line 0.5: Lancet/Hepatology standard.
#   - facet without grey strip background: complies with Cell Press guidelines.
#   - title.position "plot": better visual hierarchy in composite figures.

theme_q1 <- function(base_size = 11, base_family = "Helvetica") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Text
      plot.title         = element_text(size = base_size + 2, face = "bold",
                                        hjust = 0, margin = margin(b = 6)),
      plot.subtitle      = element_text(size = base_size, color = "grey25",
                                        hjust = 0, margin = margin(b = 8)),
      plot.caption       = element_text(size = base_size - 2, color = "grey35",
                                        hjust = 1, face = "italic"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      # Axes
      axis.title         = element_text(size = base_size, face = "bold"),
      axis.text          = element_text(size = base_size - 1, color = "black"),
      axis.line          = element_line(color = "black", linewidth = 0.5),
      axis.ticks         = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length  = unit(0.15, "cm"),
      # Legend
      legend.position    = "bottom",
      legend.title       = element_text(size = base_size, face = "bold"),
      legend.text        = element_text(size = base_size - 1),
      legend.key         = element_blank(),
      legend.background  = element_blank(),
      # Facets
      strip.background   = element_blank(),
      strip.text         = element_text(size = base_size, face = "bold"),
      # Panel
      panel.background   = element_blank(),
      panel.grid         = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      plot.margin        = margin(8, 12, 8, 8)
    )
}

# Set as the project's default theme.
theme_set(theme_q1())

# -----------------------------------------------------------------------------
# 5. Colour-blind-friendly institutional palette -----------------------------
# -----------------------------------------------------------------------------
# Validated with cvdsim (deuteranopia/protanopia/tritanopia).
# Inspiration: scico::lajolla + Okabe-Ito.
palette_q1 <- c(
  primary    = "#005F73",  # navy — dominant themes
  secondary  = "#0A9396",  # teal — secondary themes
  accent     = "#EE9B00",  # amber — emerging/highlights
  warning    = "#BB3E03",  # terracotta red — declining
  neutral    = "#94A3A3",  # blue-grey — base category
  ink        = "#1F2937",  # soft black — text and borders
  paper      = "#FFFFFF"   # background
)

# Categorical palettes by number of levels.
palette_q1_n <- function(n) {
  if (n <= 2) unname(palette_q1[c("primary", "accent")])
  else if (n <= 5) scico::scico(n, palette = "batlow", end = 0.85)
  else viridis::viridis(n, option = "D", end = 0.9)
}

# Diverging palette (motor vs declining in the thematic map).
palette_q1_div <- scico::scico(11, palette = "vik")

# -----------------------------------------------------------------------------
# 6. Q1 export helper --------------------------------------------------------
# -----------------------------------------------------------------------------
# save_q1(): exports a figure in multiple formats to the correct subfolder.
#   - .tiff 600 dpi LZW (Q1 raster standard — Hepatology/Lancet/JoH)
#   - .pdf vector via cairo (Q1 vector standard)
#   - .png 600 dpi (high-resolution preview)
#   - .eps via cairo (Elsevier legacy)
#
# Note: 600 dpi is the standard requirement of Q1 biomedical journals for raster
# figures with embedded lines/text (Cell Press, Lancet, JAMA, NEJM, JoH).
#
# EDITORIAL CURATION: the `tier` argument decides which subfolder the figure
# goes to and prefixes the Q1 editorial naming convention:
#   tier = "main"        → outputs/figures/main/Figure_N_*  (Figure 1..N in paper)
#   tier = "supplement"  → outputs/figures/supplement/Figure_SN_*  (Suppl. material)
#
# Args:
#   plot      ggplot/patchwork object
#   name      descriptive identifier (e.g., "annual_production")
#   number    index (integer) — produces Figure_<number> or Figure_S<number>
#   tier      "main" or "supplement"; controls subdir + prefix
#   width     width in inches (Q1 single-column = 3.5; double-column = 7.2)
#   height    height in inches
#   formats   vector of formats to export

# Optional figure allow-list gate. If a script (e.g. a pruned master) defines
# .FIG_ALLOWLIST, only figures whose name matches an entry are exported; every
# other figure is silently skipped. When .FIG_ALLOWLIST is absent/NULL, all
# figures are exported (default behaviour for run_all.R).
.fig_allowed <- function(name) {
  if (!exists(".FIG_ALLOWLIST", inherits = TRUE) || is.null(.FIG_ALLOWLIST))
    return(TRUE)
  name %in% .FIG_ALLOWLIST || any(startsWith(name, .FIG_ALLOWLIST))
}

# Restore biomedical acronyms after str_to_sentence()/str_to_title() lower-cases
# them (e.g. "Hbv genotyping" -> "HBV genotyping", "...hiv..." -> "...HIV...",
# "(kap)" -> "(KAP)"). Case-insensitive, whole-word, anywhere in the string.
.fix_acronyms <- function(x) {
  for (a in c("HBV", "HDV", "HCV", "HIV", "KAP", "HAI", "IAAS", "MSM", "NAT")) {
    x <- stringr::str_replace_all(
      x, stringr::regex(paste0("\\b", a, "\\b"), ignore_case = TRUE), a)
  }
  x <- stringr::str_replace_all(x, stringr::regex("\\bstis\\b", ignore_case = TRUE), "STIs")
  x <- stringr::str_replace_all(x, stringr::regex("\\bsti\\b",  ignore_case = TRUE), "STI")
  x
}

save_q1 <- function(plot, name, number,
                    tier = c("main", "supplement"),
                    width = 7.2, height = 5.0,
                    formats = c("png", "pdf")) {   # 600 dpi PNG + vector PDF
                                                   # (add "tiff" if a journal
                                                   # requires 600 dpi LZW raster)

  if (!.fig_allowed(name)) {
    message("[save_q1] skipped (not in .FIG_ALLOWLIST): ", name)
    return(invisible(NULL))
  }
  # Optional: strip embedded title/subtitle/caption (journal submission style;
  # the caption belongs in the manuscript text). Axis labels are kept. Patchwork
  # composites strip their titles at source (see 02 Lotka, 07 geographic equity).
  if (isTRUE(get0(".STRIP_TITLES", ifnotfound = FALSE)) &&
      inherits(plot, "ggplot") && !inherits(plot, "patchwork")) {
    plot <- plot + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
  }
  tier <- match.arg(tier)
  stopifnot(inherits(plot, "ggplot") | inherits(plot, "patchwork"))
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.numeric(number), length(number) == 1)

  prefix <- if (tier == "main") sprintf("Figure_%d", number)
            else                 sprintf("Figure_S%d", number)
  full_name <- paste(prefix, name, sep = "_")
  dir <- if (tier == "main") .path_figs_main else .path_figs_supp

  for (fmt in formats) {
    out <- file.path(dir, paste0(full_name, ".", fmt))
    switch(fmt,
      tiff = ggsave(out, plot, width = width, height = height,
                    units = "in", dpi = 600, device = ragg::agg_tiff,
                    compression = "lzw", bg = "white"),
      png  = ggsave(out, plot, width = width, height = height,
                    units = "in", dpi = 600, device = ragg::agg_png,
                    bg = "white"),
      pdf  = ggsave(out, plot, width = width, height = height,
                    units = "in", device = cairo_pdf, bg = "white"),
      eps  = ggsave(out, plot, width = width, height = height,
                    units = "in", device = cairo_ps, bg = "white"),
      svg  = ggsave(out, plot, width = width, height = height,
                    units = "in", device = svglite::svglite, bg = "white"),
      stop("Unsupported format: ", fmt)
    )
    message("[save_q1] -> ", out)
  }
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# 7. Traceable logging helper ------------------------------------------------
# -----------------------------------------------------------------------------
# log_step(): records each cleaning step with a timestamp + row count.

.log_path <- file.path(.path_logs, paste0("pipeline_",
                                          format(Sys.time(), "%Y%m%d_%H%M%S"),
                                          ".log"))

log_step <- function(step, n_in, n_out, msg = "") {
  line <- sprintf("[%s] %-35s | rows in=%5d  out=%5d  delta=%+5d  %s",
                  format(Sys.time(), "%H:%M:%S"),
                  step, n_in, n_out, n_out - n_in, msg)
  message(line)
  cat(line, "\n", file = .log_path, append = TRUE)
}

# -----------------------------------------------------------------------------
# 8. Study constants ---------------------------------------------------------
# -----------------------------------------------------------------------------
# Periods for the Thematic Evolution Sankey (corpus range: 1988–2023).
PERIODS <- list(
  early = c(1988, 1992),  # P1: pre-universal-vaccine era
  mid   = c(1993, 2009),  # P2: EPI vaccine introduction + molecular biology
  late  = c(2010, 2023)   # P3: post-universal-vaccine era + NGS sequencing
)

PERIOD_LABELS <- c(
  early = "1988–1992",
  mid   = "1993–2009",
  late  = "2010–2023"
)

# Focal country of the study.
COUNTRY_FOCUS <- "Peru"

# Closing message.
message("\n[00_setup.R] Setup complete. Q1 theme active. Logs in: ", .log_path, "\n")


#=============================================================================
# >>> SECTION · 01 LOAD CLEAN   (from R/01_load_clean.R)
#=============================================================================

# =============================================================================
# 01_load_clean.R · Load, normalise and build the bibliometrix object
# =============================================================================
#
# PURPOSE
# -------
# (1) Load the `TOTAL` sheet of the .xlsx (the single curated source).
# (2) Normalise authors (separators, initials, accents, fuzzy match).
# (3) Normalise institutions (UNMSM, INS, Cayetano, Gorgas, etc.).
# (4) Infer country from institution (country -> international collaboration).
# (5) Build a `bibliometrix::M`-compatible object (data.frame class with ISI
#     fields: AU, TI, PY, SO, DE, ID, C1, RP, AB, DT, TC, CR).
# (6) Serialise M to outputs/models/M.rds for fast reuse.
#
# STATEMENT FOR Q1 REVIEWER (reportable in Methods/Supplement)
# ------------------------------------------------------------
#   - The database is not a raw Scopus/WoS export; it is a database curated
#     manually in Spanish by the research team.
#   - DE (Author Keywords) is built as the concatenation of
#     [Macro_topic; Micro_topic] — a defensible semantic proxy for co-word
#     analysis and thematic mapping per Cobo et al. (2011, JASIS) §3.2.
#   - ID (Keywords Plus) is left empty as there are no indexed abstracts.
#   - DT (Document Type) maps {Original article, Thesis, Review} to ISI codes
#     (ARTICLE, THESIS, REVIEW).
# -----------------------------------------------------------------------------

# [merged] 00_setup already sourced above

# -----------------------------------------------------------------------------
# 1. LOAD THE .XLSX ----------------------------------------------------------
# -----------------------------------------------------------------------------
# Robust to multiple locations of the raw file. Priority:
#   1. data/base_datos_hbv.xlsx (canonical)
#   2. HBV_XLSX environment variable if the user prefers to point manually
# If the file is found outside data/, it is physically copied there to keep the
# project self-contained.

xlsx_path <- "data/base_datos_hbv.xlsx"
raw <- readxl::read_excel(xlsx_path, sheet = "TOTAL", .name_repair = "minimal")

log_step("01_load · raw read", 0, nrow(raw),
         msg = sprintf("cols=%d", ncol(raw)))

# Normalise column names BEFORE mapping: trim + collapse internal spaces.
# This absorbs small inconsistencies in the xlsx (trailing spaces, double
# spaces, etc.) without breaking the pipeline.
names(raw) <- stringr::str_squish(names(raw))

# Rename to (internal) English snake_case for downstream consistency.
# Explicit, documented mapping (keys are the normalised xlsx headers — no
# leading/trailing spaces). These Spanish keys must match the data file.
col_map <- c(
  "ID"                          = "id",
  "Fuente de origen"            = "source_db",
  "Año"                         = "py",        # Publication Year (ISI)
  "Título"                      = "ti",
  "Tipo de documento"           = "dt_es",
  "Revista/Universidad"         = "so",
  "Autor principal"             = "rp_raw",
  "Todos los autores"           = "au_raw",
  "Instituciones"               = "c1_raw",
  "Red de colaboración"         = "collab_es",
  "Región"                      = "region_pe",
  "Alcance geográfico"          = "scope_es",
  "Micro tema"                  = "micro_topic",
  "Macro tema"                  = "macro_topic",
  "Tipo de investigación"       = "research_type_es",
  "DOI/Enlace directo (URL)"    = "doi_url"
)

# Guard against header changes in future versions of the xlsx.
missing_cols <- setdiff(names(col_map), names(raw))
if (length(missing_cols) > 0) {
  stop("Expected columns not found in sheet 'TOTAL':\n",
       "  Missing: ", paste(shQuote(missing_cols), collapse = ", "), "\n",
       "  Found: ", paste(shQuote(names(raw)), collapse = ", "), "\n",
       "Check the exact names in the .xlsx (case/accents/spaces).",
       call. = FALSE)
}

# dplyr::rename(any_of()) expects c("new_name" = "old_name"). Since our
# dictionary is c("old" = "new") (more human-readable), we invert it here.
.rename_map <- setNames(names(col_map), unname(col_map))
df <- raw |>
  dplyr::rename(dplyr::any_of(.rename_map)) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), stringr::str_squish))

# Post-rename sanity check: the snake_case columns must exist.
expected_cols <- unname(col_map)
missing_after <- setdiff(expected_cols, names(df))
if (length(missing_after) > 0) {
  stop("Rename failed for columns: ",
       paste(shQuote(missing_after), collapse = ", "), call. = FALSE)
}

# -----------------------------------------------------------------------------
# 2. DEDUPLICATION -----------------------------------------------------------
# -----------------------------------------------------------------------------
# Strategy: by normalised DOI first; if missing, by normalised title.

normalize_doi <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_extract("10\\.[0-9]{4,9}/[^\\s\"<>]+") |>
    stringr::str_remove_all("[\\.,;]+$")
}

normalize_title <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[[:punct:]]+", " ") |>
    stringr::str_squish()
}

n_pre <- nrow(df)
df <- df |>
  dplyr::mutate(
    doi_norm   = normalize_doi(doi_url),
    ti_norm    = normalize_title(ti),
    dedup_key  = dplyr::coalesce(doi_norm, ti_norm)
  ) |>
  dplyr::distinct(dedup_key, .keep_all = TRUE)
log_step("02_dedup · DOI/title", n_pre, nrow(df))

# -----------------------------------------------------------------------------
# 3. AUTHOR NORMALISATION ----------------------------------------------------
# -----------------------------------------------------------------------------
# Rules:
#  (a) split by `;`
#  (b) UPPER + Latin-ASCII transliteration (removes accents)
#  (c) standard ISI "SURNAME INITIAL" format (e.g., "MARIN-SANCHEZ O")
#  (d) fuzzy match (Jaro-Winkler) to collapse variants < 0.10 distance.

# Catalogue of known manual aliases (extensible as needed).
ALIAS_AUTHORS <- tibble::tribble(
  ~variant,                  ~canonical,
  "MARIN SANCHEZ O",         "MARIN-SANCHEZ O",
  "MARIN-SANCHEZ OBERT",     "MARIN-SANCHEZ O",
  "GOTUZZO E",               "GOTUZZO E",
  "GOTUZZO HERENCIA E",      "GOTUZZO E",
  "CABEZAS C",               "CABEZAS C",
  "CABEZAS SANCHEZ C",       "CABEZAS C"
)

clean_author_token <- function(tok) {
  tok |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("\\.", "") |>
    stringr::str_squish() |>
    stringr::str_to_upper() |>
    # "Surname, F." -> "SURNAME F"
    stringr::str_replace(",\\s*", " ") |>
    # Compress multiple initials: "GARCIA J A" -> "GARCIA JA"
    stringr::str_replace_all("(?<=[A-Z])\\s+(?=[A-Z]$)", "")
}

# Apply canonical aliases (exact match first).
canonize_alias <- function(x) {
  m <- match(x, ALIAS_AUTHORS$variant)
  ifelse(is.na(m), x, ALIAS_AUTHORS$canonical[m])
}

# Fuzzy collapsing (optional, conservative): only within the same surname
# and Jaro-Winkler distance < 0.10.
fuzzy_collapse_authors <- function(unique_authors, threshold = 0.10) {
  surn <- stringr::word(unique_authors, 1)
  result <- unique_authors
  for (s in unique(surn)) {
    grp <- which(surn == s)
    if (length(grp) <= 1) next
    cand <- unique_authors[grp]
    d <- stringdist::stringdistmatrix(cand, cand, method = "jw")
    # keep the longest as the canonical form of each cluster
    keep <- which.max(nchar(cand))
    canon <- cand[keep]
    close <- which(d[keep, ] < threshold)
    for (k in close) result[grp[k]] <- canon
  }
  result
}

# Author<->document table.
au_long <- df |>
  dplyr::select(id, au_raw) |>
  tidyr::separate_rows(au_raw, sep = ";") |>
  dplyr::mutate(au = clean_author_token(au_raw)) |>
  dplyr::filter(au != "") |>
  dplyr::mutate(au = canonize_alias(au))

# Fuzzy: apply on the unique vector and propagate.
au_unique     <- unique(au_long$au)
au_canonized  <- fuzzy_collapse_authors(au_unique)
au_map        <- tibble::tibble(au = au_unique, au_canon = au_canonized)
au_long <- au_long |>
  dplyr::left_join(au_map, by = "au") |>
  dplyr::mutate(au = au_canon) |>
  dplyr::select(id, au)

log_step("03_authors · normalize",
         length(au_unique), dplyr::n_distinct(au_long$au),
         msg = sprintf("unique authors collapsed from %d -> %d",
                       length(au_unique), dplyr::n_distinct(au_long$au)))

# Rebuild ISI AU field ("AU1;AU2;AU3") per document.
au_isi <- au_long |>
  dplyr::group_by(id) |>
  dplyr::summarise(AU = paste(unique(au), collapse = ";"), .groups = "drop")

# -----------------------------------------------------------------------------
# 4. INSTITUTION NORMALISATION + COUNTRY INFERENCE ---------------------------
# -----------------------------------------------------------------------------
# Dictionary of Peruvian institutions and country mapping.
# Heuristic: if an institution matches the PE catalogue -> Peru. The rest is
# inferred from geographic keywords / explicit country.

INST_CATALOG_PE <- c(
  "UNMSM" = "Universidad Nacional Mayor de San Marcos",
  "UPCH"  = "Universidad Peruana Cayetano Heredia",
  "UPC"   = "Universidad Peruana de Ciencias Aplicadas",
  "USMP"  = "Universidad de San Martin de Porres",
  "UNT"   = "Universidad Nacional de Trujillo",
  "INS"   = "Instituto Nacional de Salud",
  "INEN"  = "Instituto Nacional de Enfermedades Neoplasicas",
  "MINSA" = "Ministerio de Salud",
  "ESSALUD" = "Seguro Social de Salud del Peru",
  "HNERM" = "Hospital Nacional Edgardo Rebagliati Martins",
  "HNAL"  = "Hospital Nacional Arzobispo Loayza",
  "HNDM"  = "Hospital Nacional Dos de Mayo",
  "HNCH"  = "Hospital Nacional Cayetano Heredia",
  "NAMRU-6" = "U.S. Naval Medical Research Unit Six"
)

# Dictionary of variants -> canonical form (refine iteratively).
INST_ALIASES <- list(
  "Universidad Nacional Mayor de San Marcos" =
    c("UNMSM", "San Marcos University", "Univ. Nac. Mayor"),
  "Universidad Peruana Cayetano Heredia" =
    c("UPCH", "Cayetano Heredia University", "Universidad Cayetano Heredia"),
  "Instituto Nacional de Salud" =
    c("INS", "National Institute of Health Peru", "Instituto Nac. Salud"),
  "U.S. Naval Medical Research Unit Six" =
    c("NAMRU-6", "NMRCD", "Naval Medical Research Center Detachment", "NAMRID")
)

normalize_institution <- function(x) {
  x_norm <- x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_squish()
  for (canon in names(INST_ALIASES)) {
    pat <- paste0("(?i)", paste(INST_ALIASES[[canon]], collapse = "|"))
    x_norm <- ifelse(stringr::str_detect(x_norm, pat), canon, x_norm)
  }
  x_norm
}

# Country inferred from PE catalogue + country-suffix heuristic.
COUNTRY_KEYWORDS <- c(
  "USA"     = "United States|USA|U\\.S\\.A|Wisconsin|Maryland|Boston|Harvard",
  "Brazil"  = "Brazil|Brasil|Sao Paulo|Rio de Janeiro|Fiocruz",
  "Spain"   = "Spain|Espana|Barcelona|Madrid|Valencia",
  "France"  = "France|Paris|Pasteur(?! Lille)|INSERM",
  "UK"      = "United Kingdom|UK|London|Oxford|Cambridge|Edinburgh",
  "Germany" = "Germany|Deutschland|Berlin|Heidelberg",
  "Mexico"  = "Mexico|UNAM|Cuernavaca",
  "Argentina" = "Argentina|Buenos Aires|UBA",
  "Chile"   = "Chile|Santiago",
  "Colombia"= "Colombia|Bogota",
  "Japan"   = "Japan|Tokyo|Osaka",
  "China"   = "China|Beijing|Shanghai|Hong Kong",
  "Uruguay" = "Uruguay|Montevideo"
)

infer_country <- function(inst_norm) {
  is_pe <- inst_norm %in% names(INST_ALIASES) |
           stringr::str_detect(inst_norm, "(?i)Peru|Lima|Cusco|Arequipa|Iquitos")
  res <- ifelse(is_pe, "Peru", NA_character_)
  for (cn in names(COUNTRY_KEYWORDS)) {
    res <- ifelse(is.na(res) & stringr::str_detect(inst_norm,
                                                  paste0("(?i)", COUNTRY_KEYWORDS[[cn]])),
                  cn, res)
  }
  res
}

inst_long <- df |>
  dplyr::select(id, c1_raw) |>
  tidyr::separate_rows(c1_raw, sep = ";") |>
  dplyr::mutate(c1 = stringr::str_squish(c1_raw)) |>
  dplyr::filter(c1 != "") |>
  dplyr::mutate(c1 = normalize_institution(c1),
                country = infer_country(c1))

log_step("04_inst · normalize",
         dplyr::n_distinct(inst_long$c1_raw),
         dplyr::n_distinct(inst_long$c1))

# ISI C1 format: "[Author1] Inst1; [Author2] Inst2"
c1_isi <- inst_long |>
  dplyr::group_by(id) |>
  dplyr::summarise(C1 = paste(unique(c1), collapse = ";"),
                   AU_CO = paste(unique(country[!is.na(country)]),
                                 collapse = ";"),
                   .groups = "drop")

# -----------------------------------------------------------------------------
# 5. BIOMEDICAL TRANSLATION ES -> EN (categorical fields) --------------------
# -----------------------------------------------------------------------------
# Every figure is in English (Q1 target). We build curated dictionaries for the
# 5 macro topics, ~140 micro topics, 4 research types, 3 scopes and 3
# collaboration types. Applied BEFORE building M$DE.
# NOTE: the dictionary KEYS are the Spanish values stored in the data file and
# must NOT be translated; only the English values are the outputs.
#
# Editorial rationale (reportable in Methods/Supplement):
#   "Topic taxonomy was curated in Spanish during data collection and
#    professionally translated to English biomedical terminology
#    (e.g., MeSH-aligned where applicable) prior to thematic analysis."

# Macro topics (5 values).
MACRO_MAP <- c(
  "Ciencias básicas y virología"    = "Basic sciences & virology",
  "Clínica y tratamiento"           = "Clinical & therapeutic",
  "Epidemiología y salud pública"   = "Epidemiology & public health",
  "Gestión y políticas"             = "Management & policy",
  "Prevención y control"            = "Prevention & control"
)

# Micro topics (~140 unique values). Curated biomedical translation.
MICRO_MAP <- c(
  "Análisis de costo-beneficio / Economía de la salud"                                   = "Cost-benefit analysis / Health economics",
  "Biología molecular y genotipos"                                                       = "Molecular biology & genotypes",
  "Características clínico-epidemiológicas"                                              = "Clinical-epidemiological features",
  "Carcinoma hepatocelular y embarazo / Complicaciones"                                  = "Hepatocellular carcinoma & pregnancy / Complications",
  "Cobertura de inmunización"                                                            = "Immunization coverage",
  "Cobertura de vacunación / Factores asociados"                                         = "Vaccination coverage / Associated factors",
  "Cobertura de vacunación / Recién nacidos"                                             = "Vaccination coverage / Newborns",
  "Cobertura de vacunación / Transmisión vertical"                                       = "Vaccination coverage / Vertical transmission",
  "Coinfección VHB-VHD en poblaciones indígenas"                                         = "HBV-HDV coinfection in indigenous populations",
  "Coinfección VHB-VIH / Factores asociados"                                             = "HBV-HIV coinfection / Associated factors",
  "Coinfección VIH-VHB-VHC"                                                              = "HIV-HBV-HCV coinfection",
  "Conocimientos y Actitudes (CAP)"                                                      = "Knowledge & attitudes (KAP)",
  "Conocimientos y Conductas Preventivas (CAP)"                                          = "Knowledge & preventive behaviors (KAP)",
  "Conocimientos y Prácticas / Evaluación de intervención"                               = "Knowledge & practices / Intervention assessment",
  "Conocimientos y actitudes"                                                            = "Knowledge & attitudes",
  "Conocimientos y actitudes / Egresados de Estomatología"                               = "Knowledge & attitudes / Dentistry graduates",
  "Conocimientos y actitudes / Estudiantes de Estomatología"                             = "Knowledge & attitudes / Dentistry students",
  "Conocimientos y actitudes / Internos de medicina"                                     = "Knowledge & attitudes / Medical interns",
  "Conocimientos y actitudes en internos de medicina / Factores"                         = "Knowledge & attitudes among medical interns / Factors",
  "Conocimientos y cobertura de inmunización / Factores asociados"                       = "Knowledge & immunization coverage / Associated factors",
  "Conocimientos y estado de vacunación / Factores asociados"                            = "Knowledge & vaccination status / Associated factors",
  "Conocimientos y factores asociados en estudiantes"                                    = "Knowledge & associated factors among students",
  "Conocimientos, Actitudes y Prácticas"                                                 = "Knowledge, attitudes & practices",
  "Conocimientos, Actitudes y Prácticas (CAP)"                                           = "Knowledge, attitudes & practices (KAP)",
  "Conocimientos, Actitudes y Prácticas (CAP) / Percepción de riesgo"                    = "Knowledge, attitudes & practices (KAP) / Risk perception",
  "Conocimientos, Actitudes y Prácticas (CAP) / Prevenció"                               = "Knowledge, attitudes & practices (KAP) / Prevention",
  "Conocimientos, Actitudes y Prácticas (CAP) / Prevención"                              = "Knowledge, attitudes & practices (KAP) / Prevention",
  "Costo-efectividad de tratamiento / Antivirales"                                       = "Treatment cost-effectiveness / Antivirals",
  "Cumplimiento de vacunación / Internos de medicina"                                    = "Vaccination adherence / Medical interns",
  "Cumplimiento de vacunación / Población pediátrica"                                    = "Vaccination adherence / Pediatric population",
  "Diagnóstico"                                                                          = "Diagnosis",
  "Diagnóstico laboratorial"                                                             = "Laboratory diagnosis",
  "Efectividad de intervención educativa / Prevención"                                   = "Educational intervention effectiveness / Prevention",
  "Eficacia de la entrevista epidemiológica / Correlación con tamizaje"                  = "Epidemiological interview efficacy / Screening correlation",
  "Eficacia de métodos preventivos / Factores asociados"                                 = "Preventive methods efficacy / Associated factors",
  "Eficacia de pruebas NAT / Detección temprana"                                         = "NAT testing efficacy / Early detection",
  "Eficacia de vacuna"                                                                   = "Vaccine efficacy",
  "Eficacia de vacuna / Tratamientos alternativos"                                       = "Vaccine efficacy / Alternative treatments",
  "Epidemiología general"                                                                = "General epidemiology",
  "Epidemiología molecular"                                                              = "Molecular epidemiology",
  "Etiología / Complicaciones clínicas"                                                  = "Etiology / Clinical complications",
  "Evolución clínica"                                                                    = "Clinical evolution",
  "Evolución clínica / Complicaciones"                                                   = "Clinical evolution / Complications",
  "Evolución clínica / Pronóstico"                                                       = "Clinical evolution / Prognosis",
  "Factores de riesgo"                                                                   = "Risk factors",
  "Factores de riesgo / Complicaciones"                                                  = "Risk factors / Complications",
  "Factores de riesgo / Factores asociados"                                              = "Risk factors / Associated factors",
  "Factores de riesgo / Personal policial"                                               = "Risk factors / Police personnel",
  "Factores de riesgo / Población penitenciaria"                                         = "Risk factors / Prison population",
  "Factores de riesgo / Seroprevalencia en población de riesgo"                          = "Risk factors / Seroprevalence in at-risk populations",
  "Factores de riesgo en donantes de sangre / Anticuerpo anti-core"                      = "Risk factors in blood donors / Anti-core antibody",
  "Factores de riesgo en hemodiálisis"                                                   = "Risk factors in hemodialysis",
  "Factores de riesgo en personal de salud"                                              = "Risk factors in healthcare workers",
  "Genotipificación del VHB / Análisis filogenético"                                     = "HBV genotyping / Phylogenetic analysis",
  "Genotipificación del VHB / Biología molecular"                                        = "HBV genotyping / Molecular biology",
  "Impacto de la vacunación / Comunidades indígenas"                                     = "Vaccination impact / Indigenous communities",
  "Impacto de la vacunación / Prevalencia de VHB y VHD"                                  = "Vaccination impact / HBV & HDV prevalence",
  "Impacto de vacunación / Morbimortalidad"                                              = "Vaccination impact / Morbidity-mortality",
  "Impacto de vacunación / Mortalidad por hepatopatías"                                  = "Vaccination impact / Hepatopathy mortality",
  "Incidencia de Carcinoma Hepatocelular / Factores asociados"                           = "Hepatocellular carcinoma incidence / Associated factors",
  "Incidencia y reporte de casos / Perfil epidemiológico"                                = "Case incidence & reporting / Epidemiological profile",
  "Infecciones asociadas a la atención en salud (IAAS)"                                  = "Healthcare-associated infections (HAI)",
  "Infecciones asociadas a la atención en salud (IAAS) / Factores de riesgo"             = "Healthcare-associated infections (HAI) / Risk factors",
  "Inmunización en comunidades indígenas"                                                = "Immunization in indigenous communities",
  "Inmunización en personal de salud / Seroprotección"                                   = "Immunization in healthcare workers / Seroprotection",
  "Inmunogenicidad y seguridad / Vacunación"                                             = "Immunogenicity & safety / Vaccination",
  "Intervenciones educativas/promocionales"                                              = "Educational/promotional interventions",
  "Manejo de gestante con VHB / Prevención vertical"                                     = "Pregnant patient management with HBV / Vertical prevention",
  "Manifestaciones extrahepáticas / Síndrome paraneoplásico"                             = "Extrahepatic manifestations / Paraneoplastic syndrome",
  "Marcadores infecciosos en donantes / Prevalencia de seropositividad"                  = "Infectious markers in donors / Seropositivity prevalence",
  "Mecanismos de transmisión"                                                            = "Transmission mechanisms",
  "Mortalidad"                                                                           = "Mortality",
  "Mutaciones del VHB / Región Pre-Core"                                                 = "HBV mutations / Pre-Core region",
  "Nivel de conocimiento / Estudiantes de enfermería"                                    = "Knowledge level / Nursing students",
  "Perfil clínico y epidemiológico en gestantes"                                         = "Clinical & epidemiological profile in pregnant women",
  "Perfil epidemiológico / Factores de riesgo"                                           = "Epidemiological profile / Risk factors",
  "Perfil epidemiológico / Pacientes adultos"                                            = "Epidemiological profile / Adult patients",
  "Perfil epidemiológico y clínico"                                                      = "Epidemiological & clinical profile",
  "Políticas de salud / Interculturalidad"                                               = "Health policy / Interculturality",
  "Políticas públicas y orfandad / Impacto socioeconómico"                               = "Public policy & orphanhood / Socioeconomic impact",
  "Prevalencia en contactos intrafamiliares"                                             = "Prevalence in household contacts",
  "Prevalencia en donantes / Factores asociados"                                         = "Prevalence in donors / Associated factors",
  "Prevalencia en donantes de sangre"                                                    = "Prevalence in blood donors",
  "Prevalencia en gestantes / Marcadores serológicos"                                    = "Prevalence in pregnant women / Serological markers",
  "Prevalencia en gestantes / Tamizaje prenatal"                                         = "Prevalence in pregnant women / Prenatal screening",
  "Prevalencia en grupos ocupacionales"                                                  = "Prevalence in occupational groups",
  "Prevalencia en poblaciones clave (HSH y Mujeres Trans)"                               = "Prevalence in key populations (MSM & trans women)",
  "Prevalencia en poblaciones indígenas / Etnia Matsés"                                  = "Prevalence in indigenous populations / Matses ethnicity",
  "Prevalencia y factores asociados / Comunidades indígenas"                             = "Prevalence & associated factors / Indigenous communities",
  "Prevalencia y factores asociados / Discapacidad intelectual"                          = "Prevalence & associated factors / Intellectual disability",
  "Prevención de transmisión vertical / Cumplimiento de norma técnica"                   = "Vertical transmission prevention / Technical guideline adherence",
  "Resistencia al tratamiento / Rebrote virológico"                                      = "Treatment resistance / Viral rebound",
  "Respuesta a vacuna VHB en pacientes VIH / Factores asociados"                         = "HBV vaccine response in HIV patients / Associated factors",
  "Respuesta inmune a la vacuna / Pacientes en hemodiálisis"                             = "Vaccine immune response / Hemodialysis patients",
  "Respuesta inmunogénica / Factores predictores"                                        = "Immunogenic response / Predictive factors",
  "Respuesta inmunológica / Pacientes con VIH / Factores asociados"                      = "Immunological response / HIV patients / Associated factors",
  "Respuesta inmunológica postvacunación / Factores asociados"                           = "Post-vaccination immunological response / Associated factors",
  "Riesgo ocupacional"                                                                   = "Occupational risk",
  "Seguimiento pediátrico / Barreras de atención"                                        = "Pediatric follow-up / Care barriers",
  "Seroepidemiología de hepatitis virales / Prevalencia poblacional"                     = "Viral hepatitis seroepidemiology / Population prevalence",
  "Seroepidemiología del VHB y HDV / Comunidades indígenas"                              = "HBV & HDV seroepidemiology / Indigenous communities",
  "Seropositividad en donantes / Factores demográficos"                                  = "Seropositivity in donors / Demographic factors",
  "Seropositividad en donantes / Factores epidemiológicos"                               = "Seropositivity in donors / Epidemiological factors",
  "Seroprevalencia en banco de sangre"                                                   = "Seroprevalence in blood bank",
  "Seroprevalencia en donantes / Factores asociados"                                     = "Seroprevalence in donors / Associated factors",
  "Seroprevalencia en donantes / Marcador anti-HBc"                                      = "Seroprevalence in donors / Anti-HBc marker",
  "Seroprevalencia en donantes / Marcadores infecciosos"                                 = "Seroprevalence in donors / Infectious markers",
  "Seroprevalencia en donantes de sangre"                                                = "Seroprevalence in blood donors",
  "Seroprevalencia en donantes de sangre / Marcadores serológicos"                       = "Seroprevalence in blood donors / Serological markers",
  "Seroprevalencia en gestantes / Factores sociodemográficos"                            = "Seroprevalence in pregnant women / Sociodemographic factors",
  "Seroprevalencia en personal de salud"                                                 = "Seroprevalence in healthcare workers",
  "Seroprevalencia en población de riesgo"                                               = "Seroprevalence in at-risk populations",
  "Seroprevalencia en población de riesgo / Coinfección VIH"                             = "Seroprevalence in at-risk populations / HIV coinfection",
  "Seroprevalencia en población de riesgo / ITS"                                         = "Seroprevalence in at-risk populations / STIs",
  "Seroprevalencia en población de riesgo / Transmisión vertical"                        = "Seroprevalence in at-risk populations / Vertical transmission",
  "Seroprevalencia en población general"                                                 = "Seroprevalence in general population",
  "Seroprevalencia en población general / Factores asociados"                            = "Seroprevalence in general population / Associated factors",
  "Seroprevalencia en universitarios"                                                    = "Seroprevalence in university students",
  "Seroprevalencia poblacional / Coinfección"                                            = "Population seroprevalence / Coinfection",
  "Seroprevalencia y factores de riesgo / Comunidades nativas"                           = "Seroprevalence & risk factors / Native communities",
  "Seroprotección / Eficacia de vacuna"                                                  = "Seroprotection / Vaccine efficacy",
  "Serorreactividad en donantes de sangre / Patologías infecciosas"                      = "Seroreactivity in blood donors / Infectious pathologies",
  "Serorreactividad y factores asociados en donantes"                                    = "Seroreactivity & associated factors in donors",
  "Susceptibilidad postvacunación"                                                       = "Post-vaccination susceptibility",
  "Tamizaje en donantes"                                                                 = "Donor screening",
  "Tamizaje en donantes / Impacto económico"                                             = "Donor screening / Economic impact",
  "Tamizaje en donantes de banco de sangre"                                              = "Blood bank donor screening",
  "Tamizaje en donantes de sangre"                                                       = "Blood donor screening",
  "Tamizaje en donantes de sangre / Factores asociados"                                  = "Blood donor screening / Associated factors",
  "Tamizaje en donantes de sangre / Factores de riesgo"                                  = "Blood donor screening / Risk factors",
  "Tamizaje en donantes de sangre / Incidencia de marcadores"                            = "Blood donor screening / Marker incidence",
  "Tamizaje en donantes de sangre / Marcadores infecciosos"                              = "Blood donor screening / Infectious markers",
  "Tamizaje en donantes de sangre / Prevalencia de Anti-HBc"                             = "Blood donor screening / Anti-HBc prevalence",
  "Tamizaje en donantes de sangre / Prevalencia de marcadores"                           = "Blood donor screening / Marker prevalence",
  "Tamizaje en donantes de sangre / Seropositividad"                                     = "Blood donor screening / Seropositivity",
  "Tamizaje en donantes de sangre / Seroprevalencia de Hepatitis B y C"                  = "Blood donor screening / Hepatitis B & C seroprevalence",
  "Transmisión vertical / Población gestante"                                            = "Vertical transmission / Pregnant population",
  "Tratamiento y manejo clínico"                                                         = "Treatment & clinical management",
  "Tratamientos alternativos / Estudios in vitro"                                        = "Alternative treatments / In vitro studies",
  "Vacunación en recién nacidos / Factores institucionales"                              = "Vaccination in newborns / Institutional factors",
  "Vacunación neonatal / Intervención de enfermería"                                     = "Neonatal vaccination / Nursing intervention",
  "Validación de métodos diagnósticos / Precisión analítica"                             = "Diagnostic methods validation / Analytical precision"
)

# Research type (4 values).
RT_MAP <- c(
  "Observacional descriptivo" = "Descriptive observational",
  "Observacional analítico"   = "Analytical observational",
  "Experimental"              = "Experimental",
  "Revisión bibliográfica"    = "Literature review"
)

# Geographic scope (3 values).
SCOPE_MAP <- c(
  "Local"          = "Local",
  "Multiregional"  = "Multiregional",
  "Nacional"       = "National"
)

# Collaboration network (3 values).
COLLAB_MAP <- c(
  "Colaboración internacional" = "International collaboration",
  "Colaboración nacional"      = "National collaboration",
  "Sin colaboración"           = "No collaboration"
)

# Translation helper with explicit fallback and unmapped logging.
translate_with_map <- function(x, map, field_name) {
  out <- unname(map[x])
  unmapped <- unique(x[is.na(out) & !is.na(x) & nzchar(x)])
  if (length(unmapped) > 0) {
    log_step(paste0("translate · ", field_name, " UNMAPPED"),
             length(unmapped), 0L,
             msg = paste(shQuote(head(unmapped, 5)), collapse = " · "))
    # Fallback: keep the original value (better than a silent NA).
    out[is.na(out)] <- x[is.na(out)]
  }
  out
}

df <- df |>
  dplyr::mutate(
    macro_topic_en      = translate_with_map(macro_topic, MACRO_MAP, "macro_topic"),
    micro_topic_en      = translate_with_map(micro_topic, MICRO_MAP, "micro_topic"),
    research_type_en    = translate_with_map(research_type_es, RT_MAP, "research_type"),
    scope_en            = translate_with_map(scope_es, SCOPE_MAP, "scope"),
    collab_en           = translate_with_map(collab_es, COLLAB_MAP, "collab")
  )

log_step("05_translate · ES->EN", nrow(df), nrow(df),
         msg = sprintf("macro=%d uniq · micro=%d uniq · rt=%d uniq",
                       dplyr::n_distinct(df$macro_topic_en),
                       dplyr::n_distinct(df$micro_topic_en),
                       dplyr::n_distinct(df$research_type_en)))

# -----------------------------------------------------------------------------
# 6. BUILD THE M OBJECT (bibliometrix-compatible) ----------------------------
# -----------------------------------------------------------------------------
# Document Type mapping ES -> standard ISI.
DT_MAP <- c(
  "Artículo original"     = "ARTICLE",
  "Artículo de revisión"  = "REVIEW",
  "Tesis de pregrado"     = "THESIS",
  "Tesis de posgrado"     = "THESIS"
)

M <- df |>
  dplyr::left_join(au_isi, by = "id") |>
  dplyr::left_join(c1_isi, by = "id") |>
  dplyr::transmute(
    AU = AU,
    TI = stringr::str_to_upper(ti),
    PY = as.integer(py),
    SO = stringr::str_to_upper(so),
    # Synthetic Author Keywords IN ENGLISH (declared in Methods). We use the
    # translated terminology (macro_topic_en / micro_topic_en) so that every
    # downstream figure (thematic_map, sankey, coword) is in English.
    DE = paste(stringr::str_to_upper(macro_topic_en),
               stringr::str_to_upper(micro_topic_en),
               sep = ";"),
    ID = NA_character_,                    # Keywords Plus not available
    C1 = C1,
    RP = stringr::str_to_upper(rp_raw),    # Reprint author
    AB = NA_character_,                    # Abstract not available
    DT = unname(DT_MAP[dt_es]),
    DI = doi_norm,
    TC = NA_integer_,                      # Citation count not available
    CR = NA_character_,                    # Cited references not available
    SR = paste(stringr::word(AU, 1, sep = ";"), PY, SO, sep = ", "),
    AU_CO = AU_CO,
    AU_UN = C1,                            # Affiliations
    DB = "CURATED",
    # Project-custom fields in ENGLISH (kept for Peru-specific analyses such as
    # Figure 5 geographic_equity).
    macro_topic    = stringr::str_to_upper(macro_topic_en),
    micro_topic    = stringr::str_to_upper(micro_topic_en),
    research_type  = research_type_en,
    region_pe      = region_pe,                   # department names; translated
                                                  # at use time (Figure 5 uses
                                                  # str_to_title)
    scope          = scope_en,
    collab         = collab_en
  )

class(M) <- c("bibliometrixDB", "data.frame")
attr(M, "DB") <- "CURATED"

# Critical pre-analysis validations.
stopifnot(
  "M must have non-empty AU" = all(!is.na(M$AU) & nchar(M$AU) > 0),
  "M must have valid PY"     = all(!is.na(M$PY) & M$PY >= 1976),
  "M must have non-empty DE" = all(!is.na(M$DE))
)

log_step("05_M build · bibliometrix",
         0, nrow(M),
         msg = sprintf("unique AU=%d  countries=%d  PY=[%d,%d]",
                       dplyr::n_distinct(unlist(strsplit(M$AU, ";"))),
                       dplyr::n_distinct(unlist(strsplit(M$AU_CO, ";"))),
                       min(M$PY), max(M$PY)))

# -----------------------------------------------------------------------------
# 7. SERIALISATION FOR REUSE (pipeline cache) --------------------------------
# -----------------------------------------------------------------------------
saveRDS(M,         file.path(.path_models, "M.rds"))
saveRDS(au_long,   file.path(.path_models, "au_long.rds"))
saveRDS(inst_long, file.path(.path_models, "inst_long.rds"))

# Cleaning summary table for the supplement.
data_provenance <- tibble::tibble(
  metric = c("Records in raw .xlsx",
             "After dedup (DOI/title)",
             "Unique authors raw",
             "Unique authors after fuzzy collapse",
             "Unique institutions raw",
             "Unique institutions normalized",
             "Countries detected",
             "Period covered"),
  value  = c(nrow(raw),
             nrow(M),
             length(au_unique),
             dplyr::n_distinct(au_long$au),
             dplyr::n_distinct(inst_long$c1_raw),
             dplyr::n_distinct(inst_long$c1),
             dplyr::n_distinct(unlist(strsplit(M$AU_CO[M$AU_CO != ""], ";"))),
             paste0(min(M$PY), "–", max(M$PY)))
)
writexl::write_xlsx(data_provenance,
                    file.path(.path_tables, "Table_S6_data_provenance.xlsx"))

# Table S1 (Supplementary): ES->EN biomedical translation table for the topic
# taxonomy. Required by the manuscript (Methods §2.6) and by the reviewer to
# audit the professional translation declared.
topic_translation <- dplyr::bind_rows(
  tibble::tibble(level = "Macro topic",
                 spanish = names(MACRO_MAP),
                 english = unname(MACRO_MAP)),
  tibble::tibble(level = "Micro topic",
                 spanish = names(MICRO_MAP),
                 english = unname(MICRO_MAP))
)
writexl::write_xlsx(topic_translation,
                    file.path(.path_tables, "Table_S1_topic_translation.xlsx"))

message("\n[01_load_clean.R] OK · M with ", nrow(M),
        " docs · Table S1 (translation, n=",
        nrow(topic_translation), ") + Table S6 (provenance) exported.\n")


#=============================================================================
# >>> SECTION · 02 DESCRIPTIVE METRICS   (from R/02_descriptive_metrics.R)
#=============================================================================

# =============================================================================
# 02_descriptive_metrics.R · Descriptive metrics + Figure 1 (main) + S1–S5
# =============================================================================
#
# Q1 EDITORIAL CURATION
# ---------------------
#   Figure 1  (MAIN)        Annual scientific production with epidemiological landmarks
#   Figure S1 (Supplement)  Top 15 most productive authors
#   Figure S2 (Supplement)  Lotka's Law of Scientific Productivity
#   Figure S3 (Supplement)  Top 15 most relevant sources
#   Figure S4 (Supplement)  Document type distribution
#   Figure S5 (Supplement)  Country production with international collaboration ratio
#   Table 1   (MAIN)        Main bibliometric indicators
# -----------------------------------------------------------------------------

# [merged] 00_setup already sourced above
M <- readRDS(file.path(.path_models, "M.rds"))

# -----------------------------------------------------------------------------
# Helper: fill missing years with 0 to obtain a continuous series
# -----------------------------------------------------------------------------
fill_year_gaps <- function(df, year_col, fill = 0L) {
  rng <- range(df[[year_col]], na.rm = TRUE)
  full <- tibble::tibble(!!year_col := seq(rng[1], rng[2]))
  dplyr::full_join(full, df, by = year_col) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                ~ tidyr::replace_na(., fill)))
}

# =============================================================================
# FIGURE 1 (MAIN) · Annual scientific production with epidemiological landmarks
# =============================================================================
# Peruvian historical landmarks relevant to HBV (reportable in Methods):
#   1985 — First HBV genotype F characterization in Amerindian populations
#   1991 — Hepatitis B vaccine introduced in Peruvian Expanded Program on
#          Immunization (EPI) for high-risk Amazonian populations
#   2003 — Universal infant HBV vaccination policy adopted nationwide
#   2014 — Direct-acting antivirals (DAA) era begins; Peru protocols updated

annual <- M |>
  dplyr::count(PY, name = "n_pubs") |>
  fill_year_gaps("PY") |>
  dplyr::arrange(PY) |>
  dplyr::mutate(ma3 = zoo::rollmean(n_pubs, k = 3, fill = NA, align = "center"))

landmarks <- tibble::tribble(
  ~year, ~label_short,                             ~label_long,
  1985,  "Genotype F",                             "Genotype F characterization\n(Amerindian populations)",
  1991,  "EPI vaccine\n(Amazon focus)",            "HBV vaccine added to EPI\n(Amazonian high-risk regions)",
  2003,  "Universal infant\nvaccination",          "Nationwide universal infant\nHBV vaccination policy",
  2014,  "DAA era",                                "Direct-acting antivirals (DAA)\nera begins"
)

f1 <- ggplot(annual, aes(x = PY, y = n_pubs)) +
  # Landmarks: subtle vertical line + top label
  geom_vline(data = landmarks, aes(xintercept = year),
             linetype = "dotted", color = palette_q1["warning"],
             linewidth = 0.4, alpha = 0.7) +
  geom_text(data = landmarks,
            aes(x = year, y = max(annual$n_pubs, na.rm = TRUE) * 1.05,
                label = label_short),
            hjust = -0.05, vjust = 1, size = 2.6, lineheight = 0.85,
            color = palette_q1["warning"], fontface = "italic") +
  # Annual production + moving average
  geom_col(fill = palette_q1["primary"], alpha = 0.65, width = 0.85) +
  geom_line(aes(y = ma3), color = palette_q1["accent"],
            linewidth = 1.0, na.rm = TRUE) +
  # Peak year
  geom_point(data = annual |>
               dplyr::filter(n_pubs == max(n_pubs, na.rm = TRUE)),
             aes(x = PY, y = n_pubs),
             color = palette_q1["warning"], size = 3, shape = 21,
             fill = "white", stroke = 1.2) +
  scale_x_continuous(breaks = seq(1980, 2025, by = 5),
                     limits = c(min(annual$PY), max(annual$PY) + 1),
                     expand = expansion(mult = 0.01)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title    = "Hepatitis B research in Peru, 1988–2023",
    subtitle = "Bars: annual publications · Orange line: 3-year moving average · Dotted lines: epidemiological landmarks",
    x        = "Publication year",
    y        = "Number of publications",
    caption  = sprintf("n = %d documents · Source: curated bibliographic database",
                       nrow(M))
  )

save_q1(f1, "annual_production_landmarks", number = 1, tier = "main",
        width = 9.0, height = 5.0)

# =============================================================================
# FIGURE S1 · Top 15 most productive authors -------------------------------
# =============================================================================

au_long <- readRDS(file.path(.path_models, "au_long.rds"))

top_authors <- au_long |>
  dplyr::count(au, name = "n_pubs", sort = TRUE) |>
  dplyr::slice_head(n = 15) |>
  dplyr::mutate(au = forcats::fct_reorder(au, n_pubs))

fS1 <- ggplot(top_authors, aes(x = n_pubs, y = au)) +
  geom_segment(aes(x = 0, xend = n_pubs, yend = au),
               color = palette_q1["neutral"], linewidth = 0.6) +
  geom_point(color = palette_q1["primary"], size = 3.5) +
  geom_text(aes(label = n_pubs),
            hjust = -0.4, size = 3.2, color = palette_q1["ink"]) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Top 15 most productive authors on hepatitis B research in Peru",
    subtitle = "Ranked by total number of publications, 1988–2023",
    x        = "Number of publications",
    y        = NULL,
    caption  = "Authors deduplicated via Jaro-Winkler fuzzy matching"
  )

save_q1(fS1, "top_authors", number = 1, tier = "supplement",
        width = 7.2, height = 5.5)

# =============================================================================
# FIGURE S2 · Lotka's Law of Scientific Productivity ----------------------
# =============================================================================

lotka_data <- au_long |>
  dplyr::count(au, name = "n_pubs") |>
  dplyr::count(n_pubs, name = "n_authors")

fit_lotka <- lm(log10(n_authors) ~ log10(n_pubs), data = lotka_data)
n_hat   <- -coef(fit_lotka)[2]
r2_lotka <- summary(fit_lotka)$r.squared

# --- Pathogens revision (R1.6) · cluster-robust CI by DOCUMENT bootstrap
# The OLS CI via confint() assumes author independence, an assumption violated
# by co-authorship. It is replaced by a bootstrap resampling DOCUMENTS
# (preserving within-document co-authorship; B = 2000). The point estimate
# (n_hat) is kept; only the interval changes. Per the reviewer's option (a) we
# no longer display a CI in the figure (see Methods); the bootstrap is retained
# as a sensitivity check.
.M_lotka <- readRDS(file.path(.path_models, "M.rds"))
.lotka_exp <- function(au_vec) {
  ct <- table(unlist(lapply(strsplit(au_vec, ";"),
                            function(a) unique(toupper(trimws(a[a != ""]))))))
  fr <- as.data.frame(table(papers = as.integer(ct)))
  fr$papers <- as.integer(as.character(fr$papers))
  -coef(lm(log10(Freq) ~ log10(papers), data = fr))[2]
}
set.seed(2026)
.boot_lotka <- replicate(2000, {
  idx <- sample(nrow(.M_lotka), replace = TRUE)
  tryCatch(.lotka_exp(.M_lotka$AU[idx]), error = function(e) NA_real_)
})
# Order c(0.975, 0.025) so n_ci[2] = lower bound, n_ci[1] = upper bound.
n_ci <- unname(quantile(.boot_lotka, c(0.975, 0.025), na.rm = TRUE))

# --- Figure: author-productivity BAR chart with an EMBEDDED log–log inset -----
# Design restored per author request (bar distribution + inset Lotka fit).
# NO confidence interval is displayed: a conventional interval assumes author
# independence, which co-authorship clustering violates (see Methods). The
# document-bootstrap above is retained only as a sensitivity check.

# (i) Embedded inset: log–log scatter of the same distribution + OLS fit line.
lotka_inset <- ggplot(lotka_data, aes(x = n_pubs, y = n_authors)) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x,
              linetype = "dashed", linewidth = 0.6,
              color = palette_q1["ink"]) +
  geom_point(color = palette_q1["accent"], size = 1.8) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "log–log fit", x = "log(Articles)", y = "log(Authors)") +
  theme_q1(base_size = 8) +
  theme(
    plot.title       = element_text(size = 8, face = "bold"),
    axis.title       = element_text(size = 7),
    axis.text        = element_text(size = 6),
    plot.background  = element_rect(fill = "white", color = "grey80",
                                    linewidth = 0.3),
    plot.margin      = margin(4, 6, 4, 4)
  )

# (ii) Main panel: bar chart of the author-productivity distribution.
lotka_bar_labels <- lotka_data |> dplyr::filter(n_authors >= 5)
fS2_main <- ggplot(lotka_data, aes(x = n_pubs, y = n_authors)) +
  geom_col(fill = palette_q1["secondary"], width = 0.8) +
  geom_text(data = lotka_bar_labels, aes(label = n_authors),
            vjust = -0.5, size = 3, fontface = "bold",
            color = palette_q1["ink"]) +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(
    title    = "Lotka's law of scientific productivity",
    subtitle = sprintf("Author productivity distribution · fitted n = %.2f · R² = %.2f",
                       n_hat, r2_lotka),
    x        = "Articles per author",
    y        = "Number of authors",
    caption  = "No confidence interval is reported: a conventional interval assumes author independence, which co-authorship clustering violates (see Methods)."
  )

# (iii) Compose: inset over the empty top-right region of the bar chart.
if (isTRUE(get0(".STRIP_TITLES", ifnotfound = FALSE)))
  fS2_main <- fS2_main + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL)
fS2 <- fS2_main +
  patchwork::inset_element(lotka_inset,
                           left = 0.40, bottom = 0.40,
                           right = 0.99, top = 0.99,
                           align_to = "panel")

save_q1(fS2, "lotka_law", number = 2, tier = "supplement",
        width = 8.2, height = 5.2)

# =============================================================================
# FIGURE S3 · Top 15 most relevant sources --------------------------------
# =============================================================================

top_sources <- M |>
  dplyr::count(SO, name = "n_pubs", sort = TRUE) |>
  dplyr::slice_head(n = 15) |>
  dplyr::mutate(SO = stringr::str_to_title(SO),
                SO = forcats::fct_reorder(SO, n_pubs))

fS3 <- ggplot(top_sources, aes(x = n_pubs, y = SO)) +
  geom_col(fill = palette_q1["secondary"], alpha = 0.85, width = 0.7) +
  geom_text(aes(label = n_pubs),
            hjust = -0.3, size = 3.2, color = palette_q1["ink"]) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Top 15 most relevant publication sources",
    subtitle = "Journals and academic institutions, 1988–2023",
    x        = "Number of publications",
    y        = NULL
  ) +
  theme(axis.text.y = element_text(size = 9))

save_q1(fS3, "top_sources", number = 3, tier = "supplement",
        width = 7.2, height = 5.5)

# =============================================================================
# FIGURE S4 · Document type distribution -----------------------------------
# =============================================================================

DT_LABELS_EN <- c(
  ARTICLE = "Original article",
  REVIEW  = "Review article",
  THESIS  = "Graduate / undergraduate thesis"
)

dt_dist <- M |>
  dplyr::count(DT, name = "n_pubs") |>
  dplyr::mutate(
    label_en = DT_LABELS_EN[DT],
    pct      = n_pubs / sum(n_pubs) * 100,
    label    = sprintf("%d (%.1f%%)", n_pubs, pct)
  ) |>
  dplyr::arrange(dplyr::desc(n_pubs)) |>
  dplyr::mutate(label_en = forcats::fct_inorder(label_en))

fS4 <- ggplot(dt_dist, aes(x = n_pubs, y = forcats::fct_rev(label_en),
                           fill = label_en)) +
  geom_col(width = 0.6, color = palette_q1["ink"], linewidth = 0.3) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.4,
            fontface = "bold", color = palette_q1["ink"]) +
  scale_fill_manual(values = palette_q1_n(nrow(dt_dist))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  guides(fill = "none") +
  labs(
    title = "Distribution of document types in the corpus",
    x     = "Number of publications",
    y     = NULL
  )

save_q1(fS4, "document_types", number = 4, tier = "supplement",
        width = 7.2, height = 3.5)

# =============================================================================
# FIGURE S5 · Country production with international collaboration ratio ----
# =============================================================================

country_long <- M |>
  dplyr::transmute(doc_id = dplyr::row_number(), AU_CO) |>
  tidyr::separate_rows(AU_CO, sep = ";") |>
  dplyr::mutate(country = stringr::str_squish(AU_CO)) |>
  dplyr::filter(country != "" & !is.na(country))

doc_has_intl <- country_long |>
  dplyr::group_by(doc_id) |>
  dplyr::summarise(
    has_peru = any(country == "Peru"),
    is_intl  = any(country != "Peru") & any(country == "Peru"),
    .groups  = "drop"
  )

country_stats <- country_long |>
  dplyr::left_join(doc_has_intl, by = "doc_id") |>
  dplyr::group_by(country) |>
  dplyr::summarise(
    n_pubs = dplyr::n_distinct(doc_id),
    n_intl = sum(is_intl & country != "Peru" |
                   (country == "Peru" & is_intl), na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(country != "Peru") |>
  dplyr::arrange(dplyr::desc(n_pubs)) |>
  dplyr::slice_head(n = 12) |>
  dplyr::mutate(
    pct_intl = n_intl / n_pubs * 100,
    country  = forcats::fct_reorder(country, n_pubs)
  )

fS5 <- ggplot(country_stats, aes(x = n_pubs, y = country)) +
  geom_col(aes(fill = pct_intl), width = 0.7,
           color = palette_q1["ink"], linewidth = 0.3) +
  geom_text(aes(label = sprintf("%d (%.0f%% int'l)", n_pubs, pct_intl)),
            hjust = -0.1, size = 3, color = palette_q1["ink"]) +
  scico::scale_fill_scico(palette = "lajolla", end = 0.85,
                          name = "International\ncollaboration (%)") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "Country production with Peru and international collaboration ratio",
    subtitle = "Top 12 co-authoring countries, 1988–2023",
    x        = "Number of publications co-authored with Peru",
    y        = NULL
  ) +
  theme(legend.key.width = unit(0.8, "cm"))

save_q1(fS5, "country_collaboration", number = 5, tier = "supplement",
        width = 7.5, height = 4.5)

# =============================================================================
# TABLE 1 (MAIN) · Main bibliometric indicators ----------------------------
# =============================================================================

main_indicators <- tibble::tribble(
  ~Indicator,                              ~Value,
  "Time span",                             paste0(min(M$PY), "–", max(M$PY)),
  "Documents",                             format(nrow(M), big.mark = ","),
  "Sources (journals/universities)",       format(dplyr::n_distinct(M$SO),
                                                  big.mark = ","),
  "Authors (deduplicated)",                format(dplyr::n_distinct(au_long$au),
                                                  big.mark = ","),
  "Authors per document (mean)",           sprintf("%.2f",
                                                   nrow(au_long) / nrow(M)),
  "Single-authored documents",             format(sum(table(au_long$id) == 1),
                                                  big.mark = ","),
  "Co-authored documents",                 format(sum(table(au_long$id) >= 2),
                                                  big.mark = ","),
  "International collaboration (%)",       sprintf("%.1f%%",
                                                   mean(doc_has_intl$is_intl) * 100),
  "Lotka exponent (n)",                    sprintf("%.2f (R² = %.2f)",
                                                   n_hat, r2_lotka),
  "Annual growth rate (CAGR)",             {
    yrs <- max(M$PY) - min(M$PY)
    cagr <- (annual$n_pubs[which.max(annual$PY)] /
               max(annual$n_pubs[which.min(annual$PY) + 1], 1)) ^ (1/yrs) - 1
    sprintf("%.2f%%", cagr * 100)
  }
)

t1 <- main_indicators |>
  gt::gt() |>
  gt::tab_header(
    title    = gt::md("**Table 1.** Main bibliometric indicators"),
    subtitle = gt::md("Hepatitis B research in Peru, 1988–2023")
  ) |>
  gt::cols_label(Indicator = "Indicator", Value = "Value") |>
  gt::tab_options(table.font.names = "Helvetica",
                  table.font.size = 11)

gt::gtsave(t1, file.path(.path_tables, "Table_1_main_indicators.html"))
gt::gtsave(t1, file.path(.path_tables, "Table_1_main_indicators.docx"))

# Persist data for downstream figures.
saveRDS(annual,         file.path(.path_models, "annual.rds"))
saveRDS(country_stats,  file.path(.path_models, "country_stats.rds"))
saveRDS(country_long,   file.path(.path_models, "country_long.rds"))

message("\n[02_descriptive_metrics.R] OK · Figure 1 (main) + Figures S1–S5 (suppl) + Table 1 generated.\n")


#=============================================================================
# >>> SECTION · 03 THEMATIC MAP   (from R/03_thematic_map.R)
#=============================================================================

# =============================================================================
# 03_thematic_map.R · Thematic Map (Motor Themes / Cobo et al. 2011)
# =============================================================================
#
# PURPOSE
# -------
# Produce the four-quadrant Thematic Map:
#   - Motor themes                  (high density + high centrality)  ↗
#   - Niche themes                  (high density + low centrality)   ↖
#   - Emerging or declining themes  (low density + low centrality)    ↙
#   - Basic & transversal themes    (low density + high centrality)   ↘
#
# Density    = internal cohesion of the cluster (Callon density)
# Centrality = degree of interaction of the cluster with others (Callon centrality)
#
# Implementation:
#   1. Build a keyword co-occurrence network (DE = Macro;Micro topics).
#   2. Detect communities with cluster_louvain.
#   3. Compute Callon centrality and density per cluster.
#   4. Plot quadrants with ggplot2 (do NOT use bibliometrix::plot directly,
#      as its output is not publication-ready Q1).
#
# Reference:
#   Cobo MJ, López-Herrera AG, Herrera-Viedma E, Herrera F. (2011).
#   An approach for detecting, quantifying, and visualizing the evolution
#   of a research field: A practical application to the Fuzzy Sets Theory
#   field. JOI 5(1): 146–166.
# -----------------------------------------------------------------------------

# [merged] 00_setup already sourced above
M <- readRDS(file.path(.path_models, "M.rds"))

# -----------------------------------------------------------------------------
# 1. Build the keyword co-occurrence network ---------------------------------
# -----------------------------------------------------------------------------

# ATOMIC tokenisation (Pathogens revision · reviewer "questioned the grouping").
# The curated micro-topic labels are split on "/" into ATOMIC concepts, and the
# co-occurrence network is built from those. Rationale: the previous "MACRO;MICRO"
# tokenisation produced a bipartite star (every edge macro->micro), so Louvain
# trivially returned the 5 macro families — i.e. the grouping was the input
# taxonomy, not an emergent result. Splitting micro on "/" yields genuine
# micro-micro co-occurrence, so the thematic communities EMERGE from the data
# (modularity optimisation) and are auditable (see Supplementary keyword table).
# .fix_acronyms() is defined in 00_setup.R (shared, case-insensitive).
de_long <- M |>
  dplyr::mutate(doc_id = dplyr::row_number()) |>
  dplyr::select(doc_id, micro_topic) |>
  tidyr::separate_rows(micro_topic, sep = "\\s*/\\s*") |>
  dplyr::mutate(kw = .fix_acronyms(stringr::str_to_sentence(
                       stringr::str_squish(micro_topic)))) |>
  dplyr::filter(kw != "" & !is.na(kw) & nchar(kw) > 2)

# Minimum filter: atomic concept appears in >= MIN_OCC documents.
MIN_OCC <- 2L
kw_freq <- de_long |>
  dplyr::count(kw, name = "freq") |>
  dplyr::filter(freq >= MIN_OCC) |>
  dplyr::arrange(dplyr::desc(freq))

de_filt <- de_long |> dplyr::filter(kw %in% kw_freq$kw)

# Edges: pairs of keywords co-occurring in the same document, weighted.
edges <- de_filt |>
  dplyr::inner_join(de_filt, by = "doc_id",
                    relationship = "many-to-many") |>
  dplyr::filter(kw.x < kw.y) |>
  dplyr::count(kw.x, kw.y, name = "weight")

# Ensure at least 2 nodes to build the graph.
stopifnot(nrow(edges) > 0)

g <- igraph::graph_from_data_frame(
  d = edges |> dplyr::rename(from = kw.x, to = kw.y),
  vertices = kw_freq |> dplyr::rename(name = kw),
  directed = FALSE
)

# -----------------------------------------------------------------------------
# 2. Community detection (Louvain) -------------------------------------------
# -----------------------------------------------------------------------------
set.seed(2026)
comm <- igraph::cluster_louvain(g, weights = igraph::E(g)$weight)
igraph::V(g)$cluster <- igraph::membership(comm)
n_clusters <- max(igraph::V(g)$cluster)

# -----------------------------------------------------------------------------
# 3. Callon metrics per cluster ----------------------------------------------
# -----------------------------------------------------------------------------
# Centrality_c = sum of weights of edges connecting cluster c with OTHER clusters
# Density_c    = mean of weights of internal edges of cluster c

calc_callon <- function(g, comm) {
  membership <- igraph::membership(comm)
  edges_df <- igraph::as_data_frame(g, what = "edges") |>
    dplyr::mutate(c_from = membership[from],
                  c_to   = membership[to],
                  internal = c_from == c_to)

  intra <- edges_df |>
    dplyr::filter(internal) |>
    dplyr::group_by(cluster = c_from) |>
    dplyr::summarise(density = mean(weight) * 100, .groups = "drop")

  inter <- edges_df |>
    dplyr::filter(!internal) |>
    tidyr::pivot_longer(c(c_from, c_to), values_to = "cluster") |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(centrality = sum(weight) * 10, .groups = "drop")

  size <- tibble::tibble(
    cluster = seq_len(max(membership)),
    size    = as.integer(table(membership))
  )

  size |>
    dplyr::left_join(intra,  by = "cluster") |>
    dplyr::left_join(inter,  by = "cluster") |>
    dplyr::mutate(dplyr::across(c(density, centrality),
                                ~ tidyr::replace_na(., 0)))
}

callon <- calc_callon(g, comm)

# Cluster label: central keyword (highest degree within the cluster).
labels_clusters <- purrr::map_dfr(seq_len(n_clusters), \(c) {
  vs <- igraph::V(g)[igraph::V(g)$cluster == c]
  if (length(vs) == 0) return(tibble::tibble(cluster = c, label = NA))
  sub <- igraph::induced_subgraph(g, vs)
  top <- names(sort(igraph::degree(sub), decreasing = TRUE))[1]
  tibble::tibble(cluster = c, label = top)
})

callon <- callon |>
  dplyr::left_join(labels_clusters, by = "cluster") |>
  dplyr::filter(!is.na(label))

# -----------------------------------------------------------------------------
# 4. Quadrant classification --------------------------------------------------
# -----------------------------------------------------------------------------
med_x <- median(callon$centrality)
med_y <- median(callon$density)

callon <- callon |>
  dplyr::mutate(
    quadrant = dplyr::case_when(
      centrality >= med_x & density >= med_y ~ "Motor themes",
      centrality <  med_x & density >= med_y ~ "Niche themes",
      centrality <  med_x & density <  med_y ~ "Emerging or declining",
      centrality >= med_x & density <  med_y ~ "Basic & transversal"
    ),
    quadrant = factor(quadrant,
                      levels = c("Motor themes", "Niche themes",
                                 "Emerging or declining", "Basic & transversal"))
  )

# -----------------------------------------------------------------------------
# 5. Q1 plot -----------------------------------------------------------------
# -----------------------------------------------------------------------------

quad_colors <- c(
  "Motor themes"          = unname(palette_q1["primary"]),
  "Niche themes"          = unname(palette_q1["secondary"]),
  "Basic & transversal"   = unname(palette_q1["accent"]),
  "Emerging or declining" = unname(palette_q1["warning"])
)

f7 <- ggplot(callon, aes(x = centrality, y = density)) +
  # Quadrants
  geom_vline(xintercept = med_x, linetype = "dashed",
             color = palette_q1["neutral"], linewidth = 0.5) +
  geom_hline(yintercept = med_y, linetype = "dashed",
             color = palette_q1["neutral"], linewidth = 0.5) +
  # Bubbles
  geom_point(aes(size = size, fill = quadrant),
             shape = 21, color = palette_q1["ink"],
             alpha = 0.85, stroke = 0.5) +
  # Labels
  ggrepel::geom_text_repel(aes(label = label, color = quadrant),
                           size = 3.2, fontface = "bold",
                           max.overlaps = Inf,
                           box.padding = 0.5,
                           segment.size = 0.3,
                           min.segment.length = 0,
                           show.legend = FALSE) +
  # Quadrant annotations
  annotate("text", x = max(callon$centrality) * 1.02, y = max(callon$density) * 1.02,
           label = "Motor\nthemes", hjust = 1, vjust = 1,
           fontface = "italic", color = quad_colors["Motor themes"],
           size = 3.5, alpha = 0.7) +
  annotate("text", x = min(callon$centrality), y = max(callon$density) * 1.02,
           label = "Niche\nthemes", hjust = 0, vjust = 1,
           fontface = "italic", color = quad_colors["Niche themes"],
           size = 3.5, alpha = 0.7) +
  annotate("text", x = min(callon$centrality), y = min(callon$density),
           label = "Emerging or\ndeclining", hjust = 0, vjust = 0,
           fontface = "italic", color = quad_colors["Emerging or declining"],
           size = 3.5, alpha = 0.7) +
  annotate("text", x = max(callon$centrality) * 1.02, y = min(callon$density),
           label = "Basic &\ntransversal", hjust = 1, vjust = 0,
           fontface = "italic", color = quad_colors["Basic & transversal"],
           size = 3.5, alpha = 0.7) +
  # Scales
  scale_fill_manual(values = quad_colors, name = "Theme quadrant") +
  scale_color_manual(values = quad_colors, guide = "none") +
  scale_size_continuous(range = c(4, 16),
                        name = "Cluster size\n(n keywords)",
                        breaks = scales::pretty_breaks(4)) +
  scale_x_continuous(expand = expansion(mult = 0.08)) +
  scale_y_continuous(expand = expansion(mult = 0.08)) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  labs(
    title    = "Thematic map of hepatitis B research in Peru",
    subtitle = "Strategic diagram (Callon) · clusters detected by Louvain modularity",
    x        = "Centrality (degree of interaction with other themes)",
    y        = "Density (internal cohesion of the theme)",
    caption  = sprintf("n = %d keywords (occurrence ≥ %d) · %d thematic clusters",
                       igraph::vcount(g), MIN_OCC, n_clusters)
  )

save_q1(f7, "thematic_map", number = 2, tier = "main",
        width = 9.0, height = 6.5)

# Supplementary table.
writexl::write_xlsx(
  callon |> dplyr::select(cluster, label, size, centrality, density, quadrant),
  file.path(.path_tables, "Table_S2_thematic_clusters.xlsx")
)

saveRDS(g,      file.path(.path_models, "g_coword.rds"))
saveRDS(callon, file.path(.path_models, "callon.rds"))

message("\n[03_thematic_map.R] OK · ", n_clusters,
        " thematic clusters · Figure 2 (main) saved.\n")


#=============================================================================
# >>> SECTION · 04 THEMATIC EVOLUTION SANKEY   (from R/04_thematic_evolution_sankey.R)
#=============================================================================

# =============================================================================
# 04_thematic_evolution_sankey.R · Thematic Evolution (Sankey)
# =============================================================================
#
# PURPOSE
# -------
# Visualise the thematic evolution across three periods (defined in 00_setup.R):
#   P1: 1988–1992  · pre-universal-vaccine era
#   P2: 1993–2009  · EPI vaccination · emerging molecular biology
#   P3: 2010–2023  · universal vaccine · NGS sequencing era
#
# Implementation:
#   1. For each period: detect thematic clusters (Louvain).
#   2. Build flows (alluvia) between clusters of consecutive periods based on
#      shared keywords (Inclusion Index >= THRESHOLD).
#   3. Final Sankey with ggalluvial + Q1 palette + English labels.
#
# Reference:
#   Aria M, Cuccurullo C. (2017). bibliometrix: An R-tool for comprehensive
#   science mapping analysis. JOI 11(4): 959–975.
# -----------------------------------------------------------------------------

# [merged] 00_setup already sourced above
M <- readRDS(file.path(.path_models, "M.rds"))

# -----------------------------------------------------------------------------
# 1. Helper: detect clusters per period --------------------------------------
# -----------------------------------------------------------------------------

cluster_period <- function(M_period, min_occ = 2L, period_label) {

  if (nrow(M_period) < 2) {
    return(tibble::tibble(period = period_label,
                          cluster = NA, label = NA, kws = list(NA), size = NA))
  }

  de_long <- M_period |>
    dplyr::mutate(doc_id = dplyr::row_number()) |>
    dplyr::select(doc_id, DE) |>
    tidyr::separate_rows(DE, sep = ";") |>
    dplyr::mutate(kw = stringr::str_squish(DE)) |>
    dplyr::filter(kw != "" & !is.na(kw))

  freq <- de_long |>
    dplyr::count(kw, name = "freq") |>
    dplyr::filter(freq >= min_occ)

  if (nrow(freq) < 2) {
    return(tibble::tibble(period = period_label,
                          cluster = NA, label = NA, kws = list(NA), size = NA))
  }

  de_filt <- de_long |> dplyr::filter(kw %in% freq$kw)
  edges <- de_filt |>
    dplyr::inner_join(de_filt, by = "doc_id",
                      relationship = "many-to-many") |>
    dplyr::filter(kw.x < kw.y) |>
    dplyr::count(kw.x, kw.y, name = "weight")

  if (nrow(edges) == 0) {
    # Edge case: no co-occurrences -> each keyword is its own cluster.
    return(tibble::tibble(
      period  = period_label,
      cluster = seq_len(nrow(freq)),
      label   = freq$kw,
      kws     = lapply(freq$kw, identity),
      size    = freq$freq
    ))
  }

  g <- igraph::graph_from_data_frame(
    d = edges |> dplyr::rename(from = kw.x, to = kw.y),
    vertices = freq |> dplyr::rename(name = kw),
    directed = FALSE
  )

  set.seed(2026)
  comm <- igraph::cluster_louvain(g, weights = igraph::E(g)$weight)
  membership <- igraph::membership(comm)

  tibble::tibble(name = igraph::V(g)$name,
                 freq = igraph::V(g)$freq,
                 cluster = as.integer(membership)) |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(
      label = name[which.max(freq)],
      kws   = list(name),
      size  = sum(freq),
      .groups = "drop"
    ) |>
    dplyr::mutate(period = period_label, .before = 1)
}

# -----------------------------------------------------------------------------
# 2. Apply to the three periods ----------------------------------------------
# -----------------------------------------------------------------------------

clusters_by_period <- purrr::imap_dfr(PERIODS, \(rng, key) {
  Mp <- M |> dplyr::filter(PY >= rng[1] & PY <= rng[2])
  message(sprintf("Period %s [%d–%d]: %d documents",
                  key, rng[1], rng[2], nrow(Mp)))
  cluster_period(Mp, min_occ = 2L, period_label = PERIOD_LABELS[[key]])
}) |>
  dplyr::filter(!is.na(cluster))

# -----------------------------------------------------------------------------
# 3. Build flows (Inclusion Index between consecutive periods) ---------------
# -----------------------------------------------------------------------------
# Inclusion Index (Jaccard-like): |A ∩ B| / min(|A|, |B|)
# Only flows with II >= THRESHOLD are drawn.

INCL_THRESHOLD <- 0.20

inclusion_index <- function(a, b) {
  inter <- length(intersect(a, b))
  denom <- min(length(a), length(b))
  if (denom == 0) return(0)
  inter / denom
}

flows <- list()
period_seq <- unname(PERIOD_LABELS)
for (i in seq_len(length(period_seq) - 1)) {
  from <- clusters_by_period |> dplyr::filter(period == period_seq[i])
  to   <- clusters_by_period |> dplyr::filter(period == period_seq[i + 1])

  for (r in seq_len(nrow(from))) {
    for (s in seq_len(nrow(to))) {
      ii <- inclusion_index(from$kws[[r]], to$kws[[s]])
      if (ii >= INCL_THRESHOLD) {
        flows[[length(flows) + 1]] <- tibble::tibble(
          from_period  = from$period[r],
          from_label   = from$label[r],
          from_size    = from$size[r],
          to_period    = to$period[s],
          to_label     = to$label[s],
          to_size      = to$size[s],
          inclusion    = ii,
          flow_weight  = ii * sqrt(from$size[r] * to$size[s])
        )
      }
    }
  }
}
flow_df <- dplyr::bind_rows(flows)

# -----------------------------------------------------------------------------
# 4. Sankey plot with ggalluvial ---------------------------------------------
# -----------------------------------------------------------------------------

# Reshape for ggalluvial: each row = one flow; needs axis1, axis2, axis3.
# Convert the chain of flows into a long table with one stratum per axis.

# Assign unique IDs per (period, label).
clusters_by_period <- clusters_by_period |>
  dplyr::mutate(node_id = paste0(stringr::str_sub(period, 1, 4), "_",
                                 stringr::str_to_upper(label)))

# Build lodes-format rows for ggalluvial::geom_alluvium.
# For 3 periods: we need pairs (P1->P2) and (P2->P3) joined by P2 clusters that
# appear on both sides.
make_lodes <- function(flow_df) {
  # If a P2 cluster is connected to several P1 and/or P3 clusters, we generate
  # cartesian combinations to represent the alluvia correctly.

  flow_p1p2 <- flow_df |>
    dplyr::filter(from_period == period_seq[1] & to_period == period_seq[2]) |>
    dplyr::transmute(p1 = from_label, p2 = to_label, w12 = flow_weight)

  flow_p2p3 <- flow_df |>
    dplyr::filter(from_period == period_seq[2] & to_period == period_seq[3]) |>
    dplyr::transmute(p2 = from_label, p3 = to_label, w23 = flow_weight)

  combined <- flow_p1p2 |>
    dplyr::full_join(flow_p2p3, by = "p2",
                     relationship = "many-to-many") |>
    dplyr::mutate(
      p1 = dplyr::coalesce(p1, "(no precursor)"),
      p3 = dplyr::coalesce(p3, "(no descendant)"),
      flow_size = pmin(dplyr::coalesce(w12, w23, 1),
                       dplyr::coalesce(w23, w12, 1))
    ) |>
    dplyr::mutate(alluvium_id = dplyr::row_number())

  # lodes format for ggalluvial.
  tidyr::pivot_longer(combined,
                      cols = c(p1, p2, p3),
                      names_to = "axis_key",
                      values_to = "stratum") |>
    dplyr::mutate(
      axis_key = factor(axis_key, levels = c("p1", "p2", "p3"),
                        labels = period_seq),
      stratum  = stringr::str_to_title(stratum)
    )
}

lodes <- make_lodes(flow_df)

# Consistent palette per stratum (cluster label).
strata <- unique(lodes$stratum)
strata_colors <- setNames(palette_q1_n(length(strata)), strata)

f8 <- ggplot(lodes,
             aes(x = axis_key, y = flow_size,
                 stratum = stratum, alluvium = alluvium_id,
                 fill = stratum, label = stratum)) +
  ggalluvial::geom_alluvium(alpha = 0.55, width = 1/4, knot.pos = 0.4,
                            color = NA, decreasing = FALSE) +
  ggalluvial::geom_stratum(width = 1/4, color = palette_q1["ink"],
                           linewidth = 0.3, decreasing = FALSE) +
  ggrepel::geom_text_repel(
    stat = "stratum",
    aes(label = ggplot2::after_stat(stratum)),
    direction = "y",
    nudge_x   = 0.25,
    size = 3, fontface = "bold",
    segment.size = 0.2,
    segment.color = palette_q1["neutral"],
    min.segment.length = 0,
    box.padding = 0.2
  ) +
  scale_fill_manual(values = strata_colors, guide = "none") +
  scale_x_discrete(expand = expansion(mult = c(0.10, 0.30))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Thematic evolution of hepatitis B research in Peru, 1988–2023",
    subtitle = sprintf("Three periods · alluvial flows by Inclusion Index ≥ %.2f",
                       INCL_THRESHOLD),
    x        = NULL,
    y        = "Flow size (inclusion-weighted)",
    caption  = "Clusters detected per period via Louvain modularity on co-keyword network"
  ) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 11, face = "bold"))

save_q1(f8, "thematic_evolution_sankey", number = 3, tier = "main",
        width = 9.5, height = 6.5)

# Persist flows for the technical supplement.
writexl::write_xlsx(flow_df,
                    file.path(.path_tables, "Table_S3_thematic_flows.xlsx"))

message("\n[04_thematic_evolution_sankey.R] OK · ", nrow(flow_df),
        " flows detected (II ≥ ", INCL_THRESHOLD, ") · Figure 3 (main) saved.\n")


#=============================================================================
# >>> SECTION · 05 COLLABORATION WORLD MAP   (from R/05_collaboration_world_map.R)
#=============================================================================

# =============================================================================
# 05_collaboration_world_map.R · World map of Peru–International collaboration
# =============================================================================
#
# PURPOSE
# -------
# Visually demonstrate Peru's insertion into global HBV science via a world map
# (Robinson projection) that:
#   (a) Colours each country by co-authorship intensity with Peru.
#   (b) Draws geodesic arcs (great circles) Lima <-> co-authoring country's
#       capital, weighted by the number of joint publications.
#   (c) Visually highlights Peru with a distinct ring.
#
# Cartographic source: Natural Earth (rnaturalearth, CC0 public domain).
# CRS: EPSG:4326 for points, ESRI:54030 (Robinson) for visualisation.
# -----------------------------------------------------------------------------

# [merged] 00_setup already sourced above
M <- readRDS(file.path(.path_models, "M.rds"))
country_long <- readRDS(file.path(.path_models, "country_long.rds"))

# -----------------------------------------------------------------------------
# 1. Collaboration pairs Peru <-> other country ------------------------------
# -----------------------------------------------------------------------------

collab_pairs <- country_long |>
  dplyr::group_by(doc_id) |>
  dplyr::filter(any(country == "Peru")) |>
  dplyr::filter(country != "Peru") |>
  dplyr::ungroup() |>
  dplyr::count(country, name = "n_collab") |>
  dplyr::arrange(dplyr::desc(n_collab))

# -----------------------------------------------------------------------------
# 2. Load world shapefile (Natural Earth) ------------------------------------
# -----------------------------------------------------------------------------

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::select(name, iso_a3, geometry)

# Map study names -> Natural Earth names (case-sensitive).
NAME_MAP <- c(
  "United States" = "United States of America",
  "USA"           = "United States of America",
  "UK"            = "United Kingdom"
)

collab_pairs <- collab_pairs |>
  dplyr::mutate(country_ne = dplyr::recode(country, !!!NAME_MAP))

# Join: fill n_collab; the rest = NA (grey).
world_collab <- world |>
  dplyr::left_join(collab_pairs |> dplyr::select(country_ne, n_collab),
                   by = c("name" = "country_ne")) |>
  dplyr::mutate(n_collab = ifelse(is.na(n_collab), 0L, n_collab),
                is_peru  = name == "Peru")

# -----------------------------------------------------------------------------
# 3. Country centroids for arcs ----------------------------------------------
# -----------------------------------------------------------------------------

# Suppress st_centroid warnings on invalid geometries.
suppressWarnings({
  centroids <- world |>
    dplyr::filter(name %in% c("Peru", collab_pairs$country_ne)) |>
    dplyr::mutate(centroid = sf::st_centroid(geometry))
})

coords <- sf::st_coordinates(centroids$centroid)
centroids$lon <- coords[, 1]
centroids$lat <- coords[, 2]

peru_coord <- centroids |>
  sf::st_drop_geometry() |>
  dplyr::filter(name == "Peru") |>
  dplyr::select(lon, lat) |>
  unlist()

# -----------------------------------------------------------------------------
# 4. Generate geodesic arcs (great circles) ----------------------------------
# -----------------------------------------------------------------------------

make_arc <- function(lon1, lat1, lon2, lat2, n = 100) {
  arc <- geosphere::gcIntermediate(c(lon1, lat1), c(lon2, lat2),
                                   n = n, addStartEnd = TRUE,
                                   breakAtDateLine = TRUE)
  if (is.list(arc)) {
    # Line cut by the antimeridian -> return two segments.
    segs <- lapply(seq_along(arc), \(i) {
      tibble::as_tibble(arc[[i]]) |>
        dplyr::mutate(seg_id = i)
    })
    dplyr::bind_rows(segs)
  } else {
    tibble::as_tibble(arc) |> dplyr::mutate(seg_id = 1)
  }
}

arcs_df <- centroids |>
  sf::st_drop_geometry() |>
  dplyr::filter(name != "Peru") |>
  dplyr::left_join(collab_pairs |>
                     dplyr::transmute(country_ne, n_collab),
                   by = c("name" = "country_ne")) |>
  dplyr::filter(!is.na(n_collab) & n_collab > 0) |>
  dplyr::group_by(name) |>
  dplyr::group_modify(~ {
    arc <- make_arc(peru_coord["lon"], peru_coord["lat"],
                    .x$lon, .x$lat, n = 80)
    arc |> dplyr::mutate(n_collab = .x$n_collab)
  }) |>
  dplyr::ungroup() |>
  dplyr::rename(lon = lon, lat = lat) |>
  dplyr::mutate(group_id = paste(name, seg_id, sep = "_"))

# If column names change due to st_drop_geometry, normalise them:
if (!"lon" %in% names(arcs_df)) names(arcs_df)[names(arcs_df) == "V1"] <- "lon"
if (!"lat" %in% names(arcs_df)) names(arcs_df)[names(arcs_df) == "V2"] <- "lat"

# -----------------------------------------------------------------------------
# 5. Q1 plot -----------------------------------------------------------------
# -----------------------------------------------------------------------------

# Robinson projection for world maps (Q1 cartographic standard).
ROBINSON_CRS <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

world_robin <- sf::st_transform(world_collab, crs = ROBINSON_CRS)

# Convert arcs to sf and reproject.
arcs_sf <- arcs_df |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  dplyr::group_by(group_id, name, n_collab) |>
  dplyr::summarise(do_union = FALSE, .groups = "drop") |>
  sf::st_cast("LINESTRING") |>
  sf::st_transform(crs = ROBINSON_CRS)

# Reprojected Peru point (for the highlight ring).
peru_pt <- sf::st_sf(geometry = sf::st_sfc(
  sf::st_point(c(peru_coord["lon"], peru_coord["lat"])), crs = 4326)) |>
  sf::st_transform(crs = ROBINSON_CRS)

f9 <- ggplot() +
  # Countries: filled by n_collab (log-friendly scale).
  geom_sf(data = world_robin,
          aes(fill = n_collab), color = "white", linewidth = 0.15) +
  # Geodesic arcs.
  geom_sf(data = arcs_sf,
          aes(linewidth = n_collab),
          color = palette_q1["accent"], alpha = 0.7) +
  # Peru ring.
  geom_sf(data = peru_pt, shape = 21,
          fill = palette_q1["warning"], color = palette_q1["ink"],
          size = 4, stroke = 1.2) +
  # Peru label.
  geom_sf_text(data = peru_pt, label = "PERU",
               nudge_y = -1.5e6, fontface = "bold",
               color = palette_q1["ink"], size = 3.5) +
  # Scales
  scico::scale_fill_scico(
    palette = "lajolla", end = 0.9,
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(0, 1, 5, 10, 20, 40),
    name = "Co-authored\npublications",
    na.value = palette_q1["neutral"]
  ) +
  scale_linewidth_continuous(range = c(0.2, 1.6),
                             name = "Collaboration\nintensity",
                             breaks = scales::pretty_breaks(4)) +
  coord_sf(crs = ROBINSON_CRS, expand = FALSE) +
  guides(fill = guide_colorbar(barwidth = 0.6, barheight = 6,
                               title.position = "top",
                               order = 1),
         linewidth = guide_legend(title.position = "top", order = 2)) +
  labs(
    title    = "International scientific collaboration on hepatitis B research in Peru",
    subtitle = "Geodesic arcs link Peru with co-authoring countries (1988–2023)",
    caption  = "Projection: Robinson (ESRI:54030) · Cartography: Natural Earth (CC0)"
  ) +
  theme_q1(base_size = 11) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    panel.background = element_rect(fill = "#F5F8FA", color = NA)
  )

save_q1(f9, "world_collaboration_map", number = 4, tier = "main",
        width = 10, height = 6.0)

# Supplementary table.
writexl::write_xlsx(collab_pairs,
                    file.path(.path_tables, "Table_S4_collaboration_countries.xlsx"))

message("\n[05_collaboration_world_map.R] OK · ", nrow(collab_pairs),
        " co-authoring countries with Peru · Figure 4 (main) saved.\n")


#=============================================================================
# >>> SECTION · 06 COWORD NETWORK   (from R/06_coword_network.R)
#=============================================================================

# =============================================================================
# 06_coword_network.R · Co-word Network with Community Detection
# =============================================================================
#
# PURPOSE
# -------
# Identify the strongest sub-areas of Peruvian HBV research via:
#   (a) Keyword co-occurrence network (Macro;Micro topic).
#   (b) Community detection with cluster_louvain.
#   (c) Fruchterman-Reingold layout with cluster tracking.
#   (d) Annotation: top-5 keywords per cluster + cluster size.
#
# Outputs:
#   F10 · Main co-word network (publication view)
#   F11 · Institutional co-authorship network (bonus, same techniques)
#   T2  · Per-community summary (gt)
# -----------------------------------------------------------------------------

# [merged] 00_setup already sourced above
M <- readRDS(file.path(.path_models, "M.rds"))
g <- readRDS(file.path(.path_models, "g_coword.rds"))   # from 03_thematic_map
inst_long <- readRDS(file.path(.path_models, "inst_long.rds"))

# -----------------------------------------------------------------------------
# 1. F10 · CO-WORD NETWORK ---------------------------------------------------
# -----------------------------------------------------------------------------
# Reuses the keyword graph built in 03_thematic_map.R to ensure consistency
# across figures (same clusters -> traceability for the reviewer).

set.seed(2026)
comm <- igraph::cluster_louvain(g, weights = igraph::E(g)$weight)
igraph::V(g)$cluster   <- as.character(igraph::membership(comm))
igraph::V(g)$degree    <- igraph::degree(g, mode = "all")
igraph::V(g)$between   <- igraph::betweenness(g, normalized = TRUE)
igraph::V(g)$pagerank  <- igraph::page_rank(g, weights = igraph::E(g)$weight)$vector

n_clusters <- igraph::V(g)$cluster |> unique() |> length()
cluster_pal <- setNames(palette_q1_n(n_clusters),
                        sort(unique(igraph::V(g)$cluster)))

# Move to tidygraph to work elegantly with ggraph.
tg <- tidygraph::as_tbl_graph(g)

# Labels: only top-N by betweenness to avoid saturation.
TOP_N_LABELS <- 30L
tg <- tg |>
  tidygraph::activate("nodes") |>
  dplyr::mutate(
    show_label = between >= sort(between, decreasing = TRUE)[
      pmin(TOP_N_LABELS, dplyr::n())
    ],
    name_disp = stringr::str_to_title(name)
  )

# Layout WITHOUT edge weights: weighting pulls the densely co-occurring core
# into a single overlapping blob. An unweighted Fruchterman-Reingold spreads the
# nodes evenly so labels can be separated; edge weight is still shown via width.
set.seed(2026)                                   # reproducible node layout
f10 <- ggraph(tg, layout = "fr") +
  ggraph::geom_edge_link(aes(width = weight),
                         color = palette_q1["neutral"],
                         alpha = 0.3) +
  ggraph::geom_node_point(aes(size = pagerank, fill = cluster),
                          shape = 21, color = palette_q1["ink"],
                          stroke = 0.4, alpha = 0.92) +
  ggraph::geom_node_text(aes(filter = show_label, label = name_disp),
                         repel = TRUE, size = 2.6, fontface = "bold",
                         color = palette_q1["ink"],
                         box.padding = 0.9, point.padding = 0.3,
                         force = 4, max.iter = 5000,
                         segment.size = 0.2,
                         segment.color = palette_q1["neutral"],
                         min.segment.length = 0,
                         max.overlaps = Inf) +
  ggraph::scale_edge_width(range = c(0.15, 1.2),
                           guide = "none") +
  scale_fill_manual(values = cluster_pal,
                    name = "Thematic\ncommunity") +
  scale_size_continuous(range = c(2, 9),
                        name = "PageRank\ncentrality",
                        breaks = scales::pretty_breaks(4)) +
  guides(fill = guide_legend(override.aes = list(size = 5),
                             ncol = 2, order = 1),
         size = guide_legend(order = 2)) +
  labs(
    title    = "Co-occurrence network of research topics",
    subtitle = "Communities detected via Louvain · node size proportional to PageRank",
    caption  = sprintf("n = %d keywords · n = %d co-occurrence edges · %d communities",
                       igraph::vcount(g), igraph::ecount(g), n_clusters)
  ) +
  theme_void(base_family = "Helvetica", base_size = 11) +
  theme(
    plot.title    = element_text(size = 13, face = "bold", hjust = 0,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, color = "grey25", hjust = 0,
                                 margin = margin(b = 8)),
    plot.caption  = element_text(size = 9, hjust = 1, face = "italic",
                                 color = "grey35"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(8, 8, 8, 8)
  )

# Promoted to MAIN Figure 2 (replaces the Callon strategic map, which is
# degenerate for this corpus: most themes are isolated -> all collapse into one
# quadrant). The co-word network represents the thematic structure honestly.
save_q1(f10, "coword_network", number = 2, tier = "main",
        width = 12, height = 9)

# -----------------------------------------------------------------------------
# 2. T2 · COMMUNITY SUMMARY TABLE --------------------------------------------
# -----------------------------------------------------------------------------

community_summary <- tibble::tibble(
  name     = igraph::V(g)$name,
  cluster  = igraph::V(g)$cluster,
  freq     = igraph::V(g)$freq,
  pagerank = igraph::V(g)$pagerank
) |>
  dplyr::group_by(cluster) |>
  dplyr::summarise(
    size         = dplyr::n(),
    total_freq   = sum(freq, na.rm = TRUE),
    top_keywords = paste(stringr::str_to_title(
                            name[order(-pagerank)][1:pmin(5, dplyr::n())]),
                          collapse = " · "),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(size))

t2 <- community_summary |>
  gt::gt() |>
  gt::tab_header(
    title    = gt::md("**Table 2.** Co-word communities detected by Louvain modularity"),
    subtitle = gt::md("Top-5 keywords per community ranked by PageRank centrality")
  ) |>
  gt::cols_label(
    cluster      = "Community",
    size         = "Keywords (n)",
    total_freq   = "Total occurrences",
    top_keywords = "Representative keywords"
  ) |>
  gt::tab_options(table.font.names = "Helvetica",
                  table.font.size = 11)

gt::gtsave(t2, file.path(.path_tables, "Table_2_coword_communities.html"))
gt::gtsave(t2, file.path(.path_tables, "Table_2_coword_communities.docx"))

# -----------------------------------------------------------------------------
# 3. F11 · INSTITUTIONAL COLLABORATION NETWORK -------------------------------
# -----------------------------------------------------------------------------

# Institutions with >= 2 documents (noise filter).
inst_freq <- inst_long |>
  dplyr::count(c1, name = "freq") |>
  dplyr::filter(freq >= 2L)

inst_filt <- inst_long |> dplyr::filter(c1 %in% inst_freq$c1)

# Institutional edges.
inst_edges <- inst_filt |>
  dplyr::inner_join(inst_filt, by = "id",
                    relationship = "many-to-many") |>
  dplyr::filter(c1.x < c1.y) |>
  dplyr::count(c1.x, c1.y, name = "weight")

if (nrow(inst_edges) > 0) {

  g_inst <- igraph::graph_from_data_frame(
    d = inst_edges |> dplyr::rename(from = c1.x, to = c1.y),
    vertices = inst_freq |> dplyr::rename(name = c1),
    directed = FALSE
  )

  set.seed(2026)
  comm_inst <- igraph::cluster_louvain(g_inst,
                                       weights = igraph::E(g_inst)$weight)
  igraph::V(g_inst)$cluster <- as.character(igraph::membership(comm_inst))
  igraph::V(g_inst)$degree  <- igraph::degree(g_inst)

  n_inst_clusters <- igraph::V(g_inst)$cluster |> unique() |> length()
  inst_pal <- setNames(palette_q1_n(n_inst_clusters),
                       sort(unique(igraph::V(g_inst)$cluster)))

  tg_inst <- tidygraph::as_tbl_graph(g_inst) |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(name_disp = stringr::str_wrap(name, width = 24))

  f11 <- ggraph(tg_inst, layout = "stress") +
    ggraph::geom_edge_link(aes(width = weight),
                           color = palette_q1["neutral"], alpha = 0.35) +
    ggraph::geom_node_point(aes(size = degree, fill = cluster),
                            shape = 21, color = palette_q1["ink"],
                            stroke = 0.5, alpha = 0.92) +
    ggraph::geom_node_text(aes(label = name_disp),
                           repel = TRUE, size = 2.4, fontface = "bold",
                           color = palette_q1["ink"], lineheight = 0.85,
                           box.padding = 0.45,
                           segment.size = 0.2,
                           segment.color = palette_q1["neutral"],
                           min.segment.length = 0, max.overlaps = Inf) +
    ggraph::scale_edge_width(range = c(0.2, 1.5), guide = "none") +
    scale_fill_manual(values = inst_pal,
                      name = "Collaboration\ncluster") +
    scale_size_continuous(range = c(3, 12),
                          name = "Degree\ncentrality") +
    guides(fill = guide_legend(override.aes = list(size = 5),
                               ncol = 1, order = 1),
           size = guide_legend(order = 2)) +
    labs(
      title    = "Institutional collaboration network in hepatitis B research, Peru",
      subtitle = "Communities detected via Louvain · node size proportional to degree",
      caption  = sprintf("n = %d institutions (≥ 2 docs) · n = %d edges · %d clusters",
                         igraph::vcount(g_inst), igraph::ecount(g_inst),
                         n_inst_clusters)
    ) +
    theme_void(base_family = "Helvetica", base_size = 11) +
    theme(
      plot.title    = element_text(size = 13, face = "bold", hjust = 0,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11, color = "grey25", hjust = 0,
                                   margin = margin(b = 8)),
      plot.caption  = element_text(size = 9, hjust = 1, face = "italic",
                                   color = "grey35"),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(8, 8, 8, 8)
    )

  save_q1(f11, "institutional_network", number = 7, tier = "supplement",
          width = 11, height = 8)
}

message("\n[06_coword_network.R] OK · Figures S6 + S7 (suppl) + Table 2 saved.\n")


#=============================================================================
# >>> SECTION · 07 GEOGRAPHIC EQUITY   (from R/07_geographic_equity.R)
#=============================================================================

# =============================================================================
# 07_geographic_equity.R · Figure 5 (MAIN) · Geographic equity within Peru
# =============================================================================
#
# Q1 EDITORIAL CURATION — RATIONALE
# ---------------------------------
# A manuscript on hepatitis B research in Peru must show the INTRA-country
# dimension. Reviewers expect to see:
#   (a) which departments concentrate scientific production;
#   (b) which are empty despite their epidemiological burden.
#
# This figure answers the geographic-equity question a competent reviewer will
# raise: "does research reach the regions where HBV is hyperendemic (the Amazon,
# Loreto, Madre de Dios, Ucayali)?"
#
# COMPOSITION
# -----------
#   Panel A · Departmental choropleth of Peru with n_publications
#   Panel B · Horizontal lollipop of covered vs empty departments
#             (explicit gap analysis)
#
# Final layout: patchwork::wrap_plots(A, B, widths = c(1.4, 1))
#
# SOURCES
# -------
#   - Geometry: Natural Earth Admin-1 (CC0) via rnaturalearth.
#     Fallback: public GeoJSON from the juaneladio/peru-geojson repository.
# -----------------------------------------------------------------------------

# [merged] 00_setup already sourced above
M <- readRDS(file.path(.path_models, "M.rds"))

# -----------------------------------------------------------------------------
# 1. EXTRACT REGION PER DOCUMENT AND NORMALISE -------------------------------
# -----------------------------------------------------------------------------
# The `region_pe` column comes as ";"-separated strings with accents.
# Normalise to UPPER + Latin-ASCII to join with the shapefile.

normalize_region <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_upper() |>
    stringr::str_squish()
}

region_long <- M |>
  dplyr::mutate(doc_id = dplyr::row_number()) |>
  dplyr::select(doc_id, region_pe) |>
  tidyr::separate_rows(region_pe, sep = ";") |>
  dplyr::mutate(region = normalize_region(region_pe)) |>
  dplyr::filter(region != "" & !is.na(region))

# Publication count per department (a doc may cover several).
region_counts <- region_long |>
  dplyr::distinct(doc_id, region) |>
  dplyr::count(region, name = "n_pubs", sort = TRUE)

# -----------------------------------------------------------------------------
# 2. LOAD PERU DEPARTMENTAL SHAPEFILE ----------------------------------------
# -----------------------------------------------------------------------------
# We always use the public juaneladio/peru-geojson GeoJSON as the canonical
# source. Reason: rnaturalearth::ne_states() requires `rnaturalearthhires`
# (a non-CRAN package, installable only from GitHub via drat) and triggers an
# interactive menu that breaks non-interactive execution (Rscript). The GeoJSON
# is cached locally in data/ after the first download to avoid a network
# dependency on subsequent runs.

GEOJSON_LOCAL <- file.path(.path_data, "peru_departamental.geojson")
GEOJSON_URL   <- "https://raw.githubusercontent.com/juaneladio/peru-geojson/master/peru_departamental_simple.geojson"

if (!file.exists(GEOJSON_LOCAL)) {
  message("[07_geographic_equity] Downloading Peru departmental GeoJSON...")
  tryCatch(
    download.file(GEOJSON_URL, destfile = GEOJSON_LOCAL,
                  method = "libcurl", quiet = TRUE, mode = "wb"),
    error = function(e) {
      stop("[07_geographic_equity] Could not download GeoJSON: ", e$message,
           "\n  Suggestion: download it manually from\n  ",
           GEOJSON_URL,
           "\n  and save it to: ", GEOJSON_LOCAL, call. = FALSE)
    }
  )
  message("[07_geographic_equity] OK · GeoJSON cached in ", GEOJSON_LOCAL)
}

peru_dept <- sf::st_read(GEOJSON_LOCAL, quiet = TRUE) |>
  dplyr::transmute(
    region   = normalize_region(NOMBDEP),
    geometry
  )

# Official canonical catalogue of Peruvian departments (24 + Callao = 25).
DEPT_OFICIAL <- c("AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
                  "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO",
                  "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA",
                  "LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA",
                  "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI")

# Reconcile common aliases (Cuzco -> Cusco, etc.).
ALIAS_DEPT <- c(
  "CUZCO"            = "CUSCO",
  "APURIMAC"         = "APURIMAC",
  "MADRE DE DIOS"    = "MADRE DE DIOS",
  "LA LIBERTAD"      = "LA LIBERTAD"
)
region_counts <- region_counts |>
  dplyr::mutate(region = dplyr::recode(region, !!!ALIAS_DEPT))

# Join with shapefile.
peru_map <- peru_dept |>
  dplyr::left_join(region_counts, by = "region") |>
  dplyr::mutate(n_pubs = tidyr::replace_na(n_pubs, 0L),
                covered = n_pubs > 0)

# -----------------------------------------------------------------------------
# 3. PANEL A · CHOROPLETH ----------------------------------------------------
# -----------------------------------------------------------------------------
# Editorial bins for readability (not raw continuous).

peru_map <- peru_map |>
  dplyr::mutate(
    bin = dplyr::case_when(
      n_pubs == 0          ~ "0",
      n_pubs >= 1 & n_pubs <= 5   ~ "1–5",
      n_pubs >= 6 & n_pubs <= 15  ~ "6–15",
      n_pubs >= 16 & n_pubs <= 30 ~ "16–30",
      n_pubs > 30          ~ ">30"
    ),
    bin = factor(bin, levels = c("0", "1–5", "6–15", "16–30", ">30"))
  )

# Centroids to label the most productive departments.
suppressWarnings({
  centroids_top <- peru_map |>
    dplyr::filter(n_pubs >= 6) |>
    dplyr::mutate(centroid = sf::st_centroid(geometry))
})
coords_top <- sf::st_coordinates(centroids_top$centroid)
centroids_top$lon <- coords_top[, 1]
centroids_top$lat <- coords_top[, 2]
centroids_top$label <- stringr::str_to_title(centroids_top$region)

bin_pal <- c(
  "0"      = unname(palette_q1["neutral"]),
  "1–5"    = scico::scico(4, palette = "lajolla", end = 0.85)[1],
  "6–15"   = scico::scico(4, palette = "lajolla", end = 0.85)[2],
  "16–30"  = scico::scico(4, palette = "lajolla", end = 0.85)[3],
  ">30"    = scico::scico(4, palette = "lajolla", end = 0.85)[4]
)

panelA <- ggplot(peru_map) +
  geom_sf(aes(fill = bin),
          color = palette_q1["ink"], linewidth = 0.2) +
  ggrepel::geom_text_repel(
    data = sf::st_drop_geometry(centroids_top),
    aes(x = lon, y = lat, label = label),
    size = 2.6, fontface = "bold", color = palette_q1["ink"],
    bg.color = "white", bg.r = 0.12,
    box.padding = 0.35, segment.size = 0.25,
    segment.color = palette_q1["neutral"],
    min.segment.length = 0, max.overlaps = Inf
  ) +
  scale_fill_manual(values = bin_pal,
                    name = "Publications",
                    drop = FALSE) +
  coord_sf(xlim = c(-82, -68), ylim = c(-19, 0.5),
           expand = FALSE, crs = 4326) +
  labs(
    subtitle = "A · Spatial distribution of HBV research output by department"
  ) +
  theme_q1(base_size = 11) +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#F5F8FA", color = NA),
    legend.position = "right",
    legend.key.height = unit(0.5, "cm")
  )

# -----------------------------------------------------------------------------
# 4. PANEL B · GAP ANALYSIS (covered vs empty) -------------------------------
# -----------------------------------------------------------------------------
# Build the complete catalogue of the 25 departments and flag coverage.

coverage <- tibble::tibble(region = DEPT_OFICIAL) |>
  dplyr::left_join(region_counts, by = "region") |>
  dplyr::mutate(
    n_pubs   = tidyr::replace_na(n_pubs, 0L),
    covered  = n_pubs > 0,
    label    = stringr::str_to_title(region)
  )

n_covered  <- sum(coverage$covered)
n_total    <- nrow(coverage)
pct_cov    <- round(n_covered / n_total * 100, 1)

coverage <- coverage |>
  dplyr::arrange(dplyr::desc(n_pubs), label) |>
  dplyr::mutate(label = forcats::fct_inorder(label))

panelB <- ggplot(coverage,
                 aes(x = n_pubs, y = forcats::fct_rev(label))) +
  geom_segment(aes(x = 0, xend = n_pubs, yend = forcats::fct_rev(label),
                   color = covered),
               linewidth = 0.5) +
  geom_point(aes(color = covered, size = n_pubs)) +
  geom_text(data = coverage |> dplyr::filter(n_pubs > 0),
            aes(label = n_pubs), hjust = -0.5, size = 2.6,
            color = palette_q1["ink"]) +
  geom_text(data = coverage |> dplyr::filter(n_pubs == 0),
            aes(label = "no studies"), hjust = -0.1, size = 2.4,
            fontface = "italic", color = palette_q1["warning"]) +
  scale_color_manual(values = c(`TRUE`  = unname(palette_q1["primary"]),
                                `FALSE` = unname(palette_q1["warning"])),
                     labels = c(`TRUE`  = "Covered",
                                `FALSE` = "Research gap"),
                     name = NULL) +
  scale_size_continuous(range = c(0.8, 4.5), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.30))) +
  labs(
    subtitle = sprintf("B · Departmental coverage gap: %d / %d departments (%.0f%%)",
                       n_covered, n_total, pct_cov),
    x = "Number of publications",
    y = NULL
  ) +
  theme(
    axis.text.y     = element_text(size = 8),
    legend.position = "top"
  )

# -----------------------------------------------------------------------------
# 5. PATCHWORK COMPOSITION ---------------------------------------------------
# -----------------------------------------------------------------------------

f5 <- (panelA | panelB) +
  patchwork::plot_layout(widths = c(1.35, 1)) +
  patchwork::plot_annotation(
    title    = "Geographic equity of hepatitis B research within Peru",
    subtitle = "Production concentrates in Lima and Amazonian regions; multiple Andean and southern departments remain underrepresented",
    caption  = sprintf("Cartography: Natural Earth (CC0) · CRS: EPSG:4326 · n = %d documents · %d/%d departments covered",
                       nrow(M), n_covered, n_total),
    theme    = theme(
      plot.title    = element_text(size = 13, face = "bold",
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11, color = "grey25",
                                   margin = margin(b = 8)),
      plot.caption  = element_text(size = 9, color = "grey35",
                                   face = "italic", hjust = 1)
    )
  )

# Optional journal-submission style: drop the composite title/subtitle/caption.
if (isTRUE(get0(".STRIP_TITLES", ifnotfound = FALSE)))
  f5 <- f5 + patchwork::plot_annotation(title = NULL, subtitle = NULL,
                                        caption = NULL)

save_q1(f5, "geographic_equity_peru", number = 5, tier = "main",
        width = 11.5, height = 7.0)

# Supplementary table with full coverage.
writexl::write_xlsx(
  coverage |> dplyr::transmute(Department = label,
                               Publications = n_pubs,
                               Covered = covered),
  file.path(.path_tables, "Table_S5_departmental_coverage.xlsx")
)

message("\n[07_geographic_equity.R] OK · Figure 5 (main) · ",
        n_covered, "/", n_total, " departments covered (",
        pct_cov, "%).\n")


#=============================================================================
# >>> SECTION · 08 PRISMA FLOW   (from R/08_prisma_flow.R)
#=============================================================================

# =============================================================================
# 08_prisma_flow.R · Figure S1 (Supplement) · PRISMA 2020 flow diagram
# =============================================================================
#
# PURPOSE
# -------
# Build the official PRISMA 2020 flow diagram with the exact counts of the
# selection process documented in Methods §2.4:
#   1,409 records identified -> 232 included
#
# Implemented 100% in ggplot2 + grid (no external dependencies such as Graphviz,
# DiagrammeR or the PRISMA2020 package by Haddaway et al., to guarantee
# reproducibility without additional installation). PRISMA 2020 layout with two
# identification arms: (i) Databases & registers; (ii) Other methods (snowballing).
#
# Reference (declarative, no citation needed in the script):
#   Page MJ et al. PRISMA 2020 statement. BMJ 2021;372:n71.
# -----------------------------------------------------------------------------

# [merged] 00_setup already sourced above

# -----------------------------------------------------------------------------
# 1. PRISMA COUNTS (synchronised with Methods §2.4) --------------------------
# -----------------------------------------------------------------------------
# If the database numbers change, update them here.

PRISMA <- list(
  scopus      = 181,
  google      = 1000,
  renati      = 222,
  other       = 6,
  duplicates  = 218,
  screened_db = 1185,
  excl_geo    = 767,
  excl_topic  = 153,
  excl_doctype= 13,
  retrieve_db = 252,
  not_retr_db = 17,
  elig_db     = 235,
  excl_full_topic   = 2,
  excl_full_geo     = 3,
  excl_full_doctype = 1,
  excl_full_dup     = 1,
  included_db       = 228,
  retrieve_other  = 6,
  not_retr_other  = 2,
  elig_other      = 4,
  included_other  = 4,
  TOTAL_INCLUDED  = 232
)

stopifnot(PRISMA$scopus + PRISMA$google + PRISMA$renati == 1403L)
stopifnot(PRISMA$included_db + PRISMA$included_other == PRISMA$TOTAL_INCLUDED)

# -----------------------------------------------------------------------------
# 2. LAYOUT: boxes with (x, y, width, height, text, colour) ------------------
# -----------------------------------------------------------------------------
# Coordinates in a 0–100 system for maximum portability. Two columns:
# left (x ~ 25) = database identification; right (x ~ 75) = other methods.

# Helper to create a standardised PRISMA box.
.box <- function(id, x, y, w, h, label, fill = palette_q1["primary"],
                 text_color = "white") {
  list(id = id, x = x, y = y, w = w, h = h, label = label,
       fill = unname(fill), text_color = text_color)
}

# Build the full layout. Four aligned columns in a 0–108 x-system:
#   (i)   main database flow      x = 23  (w = 34)
#   (ii)  RED exclusion rail      x = 60  (w = 24, right-aligned, horiz. arrows)
#   (iii) other-methods arm       x = 84  (w = 16)
#   (iv)  other-methods exclusion x = 100 (w = 13, branches RIGHT of the arm at
#         the retrieval level, mirroring the database rail — no arrow crosses it)
# Text is centred (hjust = vjust = 0.5) in every box.
boxes <- list(
  # === LEFT COLUMN: database identification ===
  .box("id_db",   23, 95, 34, 6,
       sprintf("Records identified from databases (n = %d):\nScopus (n = %d) · Google Scholar (n = %d) · RENATI (n = %d)",
               PRISMA$scopus + PRISMA$google + PRISMA$renati,
               PRISMA$scopus, PRISMA$google, PRISMA$renati)),
  .box("dedup",   60, 95, 24, 4.5,
       sprintf("Records removed before screening:\nDuplicates (n = %d)",
               PRISMA$duplicates),
       fill = palette_q1["warning"]),
  .box("screen_db", 23, 84, 34, 4.5,
       sprintf("Records screened (n = %d)", PRISMA$screened_db)),
  .box("excl_screen", 60, 84, 24, 6.5,
       sprintf("Records excluded (n = %d):\nGeographic (n = %d)\nTopic (n = %d)\nDocument type (n = %d)",
               PRISMA$excl_geo + PRISMA$excl_topic + PRISMA$excl_doctype,
               PRISMA$excl_geo, PRISMA$excl_topic, PRISMA$excl_doctype),
       fill = palette_q1["warning"]),
  .box("retrieve_db", 23, 73, 34, 4.5,
       sprintf("Reports sought for retrieval (n = %d)", PRISMA$retrieve_db)),
  .box("notretr_db", 60, 73, 24, 4.5,
       sprintf("Reports not retrieved (n = %d)", PRISMA$not_retr_db),
       fill = palette_q1["warning"]),
  .box("elig_db", 23, 62, 34, 4.5,
       sprintf("Reports assessed for eligibility (n = %d)", PRISMA$elig_db)),
  .box("excl_full", 60, 62, 24, 8,
       sprintf("Reports excluded (n = %d):\nGeography (n = %d)\nTopic (n = %d)\nDocument type (n = %d)\nDuplicate (n = %d)",
               PRISMA$excl_full_geo + PRISMA$excl_full_topic +
                 PRISMA$excl_full_doctype + PRISMA$excl_full_dup,
               PRISMA$excl_full_geo, PRISMA$excl_full_topic,
               PRISMA$excl_full_doctype, PRISMA$excl_full_dup),
       fill = palette_q1["warning"]),
  .box("incl_db", 23, 49, 34, 4.5,
       sprintf("Studies included from databases (n = %d)", PRISMA$included_db),
       fill = palette_q1["secondary"]),

  # === RIGHT COLUMN: identification via other methods ===
  # Wider boxes (w = 18) with two-line labels so text never overflows the box.
  # NB: "Citation snowballing" is rendered as a label ABOVE this box (see plot).
  .box("id_other", 83, 95, 18, 6,
       sprintf("Records identified from\nother sources (n = %d)",
               PRISMA$other)),
  .box("retr_other", 83, 73, 18, 5,
       sprintf("Reports sought for\nretrieval (n = %d)", PRISMA$retrieve_other)),
  # Exclusion branches RIGHT of the arm, at the retrieval level (y = 73), so the
  # main vertical flow (retrieval -> eligibility) never crosses it.
  .box("notretr_other", 101, 73, 15, 5,
       sprintf("Reports not retrieved\n(n = %d)", PRISMA$not_retr_other),
       fill = palette_q1["warning"]),
  .box("elig_other", 83, 60, 18, 5,
       sprintf("Reports assessed for\neligibility (n = %d)", PRISMA$elig_other)),
  .box("incl_other", 83, 49, 18, 5.5,
       sprintf("Studies included from\nother sources (n = %d)",
               PRISMA$included_other),
       fill = palette_q1["secondary"]),

  # === FINAL BOX: convergence ===
  .box("included_total", 50, 28, 40, 6,
       sprintf("Total studies included in the bibliometric analysis (n = %d)",
               PRISMA$TOTAL_INCLUDED),
       fill = palette_q1["accent"], text_color = palette_q1["ink"])
)

box_df <- do.call(rbind.data.frame, lapply(boxes, as.data.frame))

# -----------------------------------------------------------------------------
# 3. EDGES: (from_id, to_id) -------------------------------------------------
# -----------------------------------------------------------------------------
edges <- tibble::tribble(
  ~from,         ~to,
  "id_db",       "screen_db",
  "id_db",       "dedup",         # lateral exclusion
  "screen_db",   "retrieve_db",
  "screen_db",   "excl_screen",
  "retrieve_db", "elig_db",
  "retrieve_db", "notretr_db",
  "elig_db",     "incl_db",
  "elig_db",     "excl_full",
  "incl_db",     "included_total",
  # right arm
  "id_other",    "retr_other",
  "retr_other",  "elig_other",
  "retr_other",  "notretr_other",
  "elig_other",  "incl_other",
  "incl_other",  "included_total"
)

# Compute the start and end points of each arrow (box centres).
edges <- edges |>
  dplyr::left_join(box_df |>
                     dplyr::select(id, x, y, w, h),
                   by = c("from" = "id")) |>
  dplyr::rename(x_from = x, y_from = y, w_from = w, h_from = h) |>
  dplyr::left_join(box_df |>
                     dplyr::select(id, x, y, w, h),
                   by = c("to" = "id")) |>
  dplyr::rename(x_to = x, y_to = y, w_to = w, h_to = h) |>
  dplyr::mutate(
    # If vertical (same x), start = bottom of "from", end = top of "to".
    # If horizontal (same y), start = right of "from", end = left of "to".
    same_x = abs(x_from - x_to) < 1,
    same_y = abs(y_from - y_to) < 1,
    x0 = dplyr::case_when(same_x ~ x_from, same_y & x_to > x_from ~ x_from + w_from/2,
                          TRUE ~ x_from),
    y0 = dplyr::case_when(same_x ~ y_from - h_from/2, same_y ~ y_from, TRUE ~ y_from - h_from/2),
    # Diagonal convergence arrows (e.g. the two "Studies included" boxes feeding
    # the final total) end at the TOP-CENTRE of the target box, not its left edge.
    x1 = dplyr::case_when(same_x ~ x_to, same_y & x_to > x_from ~ x_to - w_to/2,
                          TRUE ~ x_to),
    y1 = dplyr::case_when(same_x ~ y_to + h_to/2, same_y ~ y_to, TRUE ~ y_to + h_to/2)
  )

# -----------------------------------------------------------------------------
# 4. PLOT --------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Side bands with PRISMA phase labels (Identification, Screening, Included).
# Standard official PRISMA 2020 aesthetic.
phase_labels <- tibble::tibble(
  y = c(95, 78.5, 35.5),
  label = c("Identification", "Screening", "Included")
)

f_prisma <- ggplot() +
  # Side bands (light fill)
  geom_rect(data = phase_labels,
            aes(xmin = 0, xmax = 8, ymin = y - 8, ymax = y + 5),
            fill = palette_q1["neutral"], alpha = 0.18) +
  geom_text(data = phase_labels,
            aes(x = 4, y = y - 1.5, label = label),
            angle = 90, fontface = "bold", size = 4.5,
            color = palette_q1["ink"]) +
  # Edges (arrows)
  geom_segment(data = edges,
               aes(x = x0, y = y0, xend = x1, yend = y1),
               arrow = grid::arrow(length = unit(0.18, "cm"),
                                   type = "closed"),
               color = palette_q1["ink"], linewidth = 0.4) +
  # Boxes (rectangles)
  geom_rect(data = box_df,
            aes(xmin = x - w/2, xmax = x + w/2,
                ymin = y - h/2, ymax = y + h/2,
                fill = I(fill)),
            color = palette_q1["ink"], linewidth = 0.4) +
  # Text inside boxes (centred: hjust = vjust = 0.5 by default)
  geom_text(data = box_df,
            aes(x = x, y = y, label = label, color = I(text_color)),
            size = 2.7, lineheight = 0.95, fontface = "plain",
            hjust = 0.5, vjust = 0.5) +
  # "Citation snowballing" rendered ABOVE the other-sources box (not inside it)
  annotate("text", x = 83, y = 99.6, label = "Citation snowballing",
           fontface = "italic", size = 2.7, color = palette_q1["ink"]) +
  # Final layout
  coord_fixed(xlim = c(0, 110), ylim = c(20, 100), expand = FALSE) +
  labs(
    title    = "Figure S1. PRISMA 2020 flow diagram for the bibliometric search",
    subtitle = "Hepatitis B research in Peru, 1988–2023",
    caption  = "Reporting standard: Page MJ et al. The PRISMA 2020 statement. BMJ 2021;372:n71."
  ) +
  theme_void(base_family = "Helvetica") +
  theme(
    plot.title    = element_text(size = 13, face = "bold", hjust = 0,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, color = "grey25", hjust = 0,
                                 margin = margin(b = 8)),
    plot.caption  = element_text(size = 9, color = "grey35",
                                 face = "italic", hjust = 1),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  )

# Export as Figure S1 (legitimate supplement slot).
save_q1(f_prisma, "PRISMA_flow", number = 1, tier = "supplement",
        width = 12, height = 9)

# Optional supplementary table with the counts in an auditable format.
prisma_table <- tibble::tibble(
  Stage = c("Identification — Scopus",
            "Identification — Google Scholar",
            "Identification — RENATI",
            "Identification — Other methods (snowballing)",
            "Total identified",
            "Duplicates removed",
            "Records screened",
            "Records excluded (geography)",
            "Records excluded (topic)",
            "Records excluded (document type)",
            "Reports sought for retrieval (databases)",
            "Reports not retrieved (databases)",
            "Reports assessed for eligibility (databases)",
            "Reports excluded — full text (databases)",
            "Studies included from databases",
            "Studies included from other sources",
            "TOTAL INCLUDED IN BIBLIOMETRIC ANALYSIS"),
  n = c(PRISMA$scopus, PRISMA$google, PRISMA$renati, PRISMA$other,
        PRISMA$scopus + PRISMA$google + PRISMA$renati + PRISMA$other,
        PRISMA$duplicates, PRISMA$screened_db,
        PRISMA$excl_geo, PRISMA$excl_topic, PRISMA$excl_doctype,
        PRISMA$retrieve_db, PRISMA$not_retr_db, PRISMA$elig_db,
        PRISMA$excl_full_geo + PRISMA$excl_full_topic +
          PRISMA$excl_full_doctype + PRISMA$excl_full_dup,
        PRISMA$included_db, PRISMA$included_other, PRISMA$TOTAL_INCLUDED)
)
writexl::write_xlsx(prisma_table,
                    file.path(.path_tables, "Table_S7_PRISMA_counts.xlsx"))

message("\n[08_prisma_flow.R] OK · Figure S1 (PRISMA) + Table S7 (PRISMA counts) saved.\n")


#=============================================================================
# >>> SECTION · 15 ALIGNMENT BURDEN CORRELATION   (from R/15_alignment_burden_correlation.R)
#=============================================================================

#!/usr/bin/env Rscript
# =============================================================================
# 15_alignment_burden_correlation.R
# Spearman correlation: # publications per department vs HBV burden.
# Indicators: anti-HBc IgG (primary · cumulative exposure), HBsAg (secondary
# · post-vaccination floor effect) and incidence (if provided). Answers R4.5/R3.1/R2.
# Burden data: data/burden_by_department.csv (Cabezas et al. 2020, Table 2;
#              MINSA cases via Ango-Aguilar et al. 2025; INEI population).
# Output (publications) derived from data/base_datos_hbv.xlsx (authoritative).
# Outputs: 600 dpi figures (PNG+PDF) + .xlsx table in manuscript/03_submission/.
# Usage:  Rscript R/15_alignment_burden_correlation.R
# =============================================================================

need <- c("here","readxl","readr","writexl","dplyr","tidyr","stringr","stringi","ggplot2","ggrepel")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(need, require, character.only = TRUE))

root <- tryCatch(here::here(), error = function(e) getwd())
xlsx <- file.path(root, "data", "base_datos_hbv.xlsx")
csv  <- file.path(root, "data", "burden_by_department.csv")
stopifnot(file.exists(xlsx), file.exists(csv))
figdir <- file.path(root, "manuscript", "03_submission", "figures_revision")
tabdir <- file.path(root, "manuscript", "03_submission", "tables_revision")
dir.create(figdir, showWarnings = FALSE, recursive = TRUE)
dir.create(tabdir, showWarnings = FALSE, recursive = TRUE)

key <- function(x) str_squish(stri_trans_general(toupper(x), "Latin-ASCII"))

# --- 1. Output per department (from the authoritative master database) -------
raw <- readxl::read_excel(xlsx, sheet = "TOTAL"); names(raw) <- str_squish(names(raw))
prod <- raw |>
  dplyr::mutate(rid = dplyr::row_number()) |>
  dplyr::select(rid, region = `Región`) |>
  tidyr::separate_rows(region, sep = ";") |>
  dplyr::mutate(k = key(region)) |>
  dplyr::filter(k != "" & k != "TODO EL PERU") |>
  dplyr::distinct(rid, k) |>
  dplyr::count(k, name = "n_pub")

# --- 2. Burden (CSV) ---------------------------------------------------------
burden <- readr::read_csv(csv, show_col_types = FALSE) |> dplyr::mutate(k = key(department))
dat <- burden |> dplyr::left_join(prod, by = "k") |> dplyr::mutate(n_pub = dplyr::coalesce(n_pub, 0L))

if ("n_publications_ref" %in% names(dat)) {
  mm <- dat |> dplyr::filter(!is.na(n_publications_ref) & n_publications_ref != n_pub)
  if (nrow(mm)) { cat("[WARNING] Derived output != reference for:\n"); print(as.data.frame(mm[,c("department","n_publications_ref","n_pub")]), row.names = FALSE) }
}

# --- 3. Spearman + scatter per indicator ------------------------------------
spear <- function(d, xcol) {
  if (!xcol %in% names(d)) return(NULL)
  s <- d |> dplyr::filter(!is.na(.data[[xcol]]) & !is.na(n_pub))
  if (nrow(s) < 4) return(NULL)
  ct <- suppressWarnings(cor.test(s$n_pub, s[[xcol]], method = "spearman", exact = FALSE))
  list(rho = unname(ct$estimate), p = ct$p.value, n = nrow(s), data = s, xcol = xcol)
}
scatter <- function(res, xlab, fname, title) {
  if (is.null(res)) return(invisible())
  # Honour the optional figure allow-list
  if (exists(".fig_allowed", mode = "function") && !.fig_allowed(fname))
    return(invisible())
  
  s <- res$data
  
  # Calcular medianas de forma automatizada para los cuadrantes
  mediana_pub <- median(s$n_pub, na.rm = TRUE)
  mediana_ind <- median(s[[res$xcol]], na.rm = TRUE)
  
  # Tu diseño MDPI: Publicaciones en X, Indicador en Y
  p <- ggplot(s, aes(x = n_pub, y = .data[[res$xcol]])) +
    
    # Líneas de corte de la mediana (Punteadas) para cuadrantes
    geom_vline(xintercept = mediana_pub, linetype = "dotted", color = "black", linewidth = 0.5) +
    geom_hline(yintercept = mediana_ind, linetype = "dotted", color = "black", linewidth = 0.5) +
    
    # Puntos
    geom_point(color = "#2c7bb6", size = 2.5, alpha = 0.8) +
    
    # Curva de tendencia suavizada (LOESS) al estilo MDPI
    geom_smooth(method = "loess", color = "#b30000", fill = NA, linewidth = 0.8, linetype = "dashed") +
    
    # Etiquetas de texto con force alta para evitar solapamiento
    ggrepel::geom_text_repel(aes(label = department), 
                             size = 3, force = 40, max.overlaps = Inf, 
                             segment.color = "grey60", box.padding = 0.3) +
    
    # Textos en inglés dinámicos
    labs(title = title,
         subtitle = sprintf("Spearman correlation: rho = %+.2f, p = %.3f (n = %d)", res$rho, res$p, res$n),
         x = "Number of publications (by study setting)", 
         y = xlab) +
    
    # Tema clásico Q1
    theme_classic(base_size = 12) +
    theme(axis.title = element_text(face = "bold", size = 11),
          axis.text = element_text(size = 10))
  
  if (isTRUE(get0(".STRIP_TITLES", ifnotfound = FALSE)))
    p <- p + labs(title = NULL, subtitle = NULL)
  
  # Exportación automatizada: PNG, PDF y el requerido TIFF a 600 DPI (LZW)
  ggsave(file.path(figdir, paste0(fname, ".png")), p, width = 8, height = 6, dpi = 600)
  ggsave(file.path(figdir, paste0(fname, ".pdf")), p, width = 8, height = 6, device = cairo_pdf)
  ggsave(file.path(figdir, paste0(fname, ".tiff")), p, width = 8, height = 6, dpi = 600, device = "tiff", compression = "lzw")
  
  cat("[fig]", fname, "(PNG+PDF+TIFF 600dpi)\n")
}

specs <- list(
  list(col="anti_hbc_prevalence", lab="Anti-HBc IgG seroprevalence (%)", f="FigR_D_pub_vs_antiHBc",  t="Research output vs cumulative HBV exposure (anti-HBc) by department", name="Publications ~ anti-HBc IgG (%)"),
  list(col="hbsag_prevalence",    lab="HBsAg seroprevalence (%)",        f="FigR_E_pub_vs_HBsAg",   t="Research output vs current HBsAg prevalence by department",          name="Publications ~ HBsAg (%)"),
  list(col="incidence",           lab="HBV incidence",                   f="FigR_F_pub_vs_incidence", t="Research output vs HBV incidence by department",                     name="Publications ~ incidence")
)

cat("\n==================  OUTPUT ~ BURDEN ALIGNMENT  ==================\n")
summ <- list()
for (sp in specs) {
  res <- spear(dat, sp$col)
  if (is.null(res)) { cat(sprintf("· %-32s insufficient data\n", sp$name)); next }
  cat(sprintf("· %-32s Spearman rho = %+.3f | p = %.3f | n = %d\n", sp$name, res$rho, res$p, res$n))
  summ[[sp$name]] <- data.frame(comparison = sp$name, spearman_rho = round(res$rho,3),
                                p_value = round(res$p,4), n_departments = res$n)
  scatter(res, sp$lab, sp$f, sp$t)
}

# Per-capita sensitivity (if population is provided)
if ("population" %in% names(dat) && any(!is.na(dat$population))) {
  d2 <- dat |> dplyr::mutate(n_pub = ifelse(!is.na(population) & population>0, n_pub/population*1e5, NA_real_))
  rp <- spear(d2, "anti_hbc_prevalence")
  if (!is.null(rp)) cat(sprintf("· %-32s Spearman rho = %+.3f | p = %.3f | n = %d (per-capita sensitivity)\n",
                                "Pub/100k ~ anti-HBc IgG (%)", rp$rho, rp$p, rp$n))
}

summary_df <- if (length(summ)) do.call(rbind, summ) else data.frame(note = "No data")
writexl::write_xlsx(list(correlation_summary = summary_df, merged_data = dat),
                    file.path(tabdir, "S_alignment_burden_correlation.xlsx"))
cat("\n[OK] Table:", file.path(tabdir, "S_alignment_burden_correlation.xlsx"), "\n")


#=============================================================================
# >>> SECTION · 16 KEYWORD TREEMAP   (from R/16_keyword_treemap.R)
#=============================================================================

# =============================================================================
# 16_keyword_treemap.R · Figure S8 (Supplement) · ALL thematic keywords
# =============================================================================
#
# PURPOSE
# -------
# Reviewer request: a single figure that visualises EVERY keyword (not only the
# co-occurrence-network subset at occurrence >= 2). A hierarchical TREEMAP shows
# all micro-topics grouped by their macro-theme, with tile area proportional to
# the number of documents. This complements the strategic map / co-word network
# (which by construction display only recurrent terms) and makes the full
# thematic taxonomy auditable.
#
# Output:
#   Figure S8 · treemap of all micro-topics grouped by macro-theme
#   Table  S8 · every keyword with its exact document count (auditable)
#
# Source: M$macro_topic / M$micro_topic (already curated and English, from 01).
# -----------------------------------------------------------------------------

# [merged] 00_setup already sourced above
M <- readRDS(file.path(.path_models, "M.rds"))

# -----------------------------------------------------------------------------
# 1. Keyword frequency by macro / micro --------------------------------------
# -----------------------------------------------------------------------------
tm_data <- M |>
  dplyr::filter(!is.na(micro_topic), micro_topic != "",
                !is.na(macro_topic), macro_topic != "") |>
  dplyr::count(macro_topic, micro_topic, name = "n") |>
  dplyr::mutate(
    macro = stringr::str_to_sentence(macro_topic),
    micro = .fix_acronyms(stringr::str_to_sentence(micro_topic))  # HBV/HIV/KAP...
  ) |>
  dplyr::arrange(macro, dplyr::desc(n))

# -----------------------------------------------------------------------------
# 2. Supplementary table: EVERY keyword + exact count ------------------------
# -----------------------------------------------------------------------------
writexl::write_xlsx(
  tm_data |>
    dplyr::transmute(`Macro theme`  = macro,
                     `Micro keyword` = micro,
                     Documents       = n,
                     `% of corpus`   = round(n / sum(n) * 100, 2)),
  file.path(.path_tables, "Table_S8_all_keywords.xlsx")
)

# -----------------------------------------------------------------------------
# 3. Treemap ------------------------------------------------------------------
# -----------------------------------------------------------------------------
n_macro <- dplyr::n_distinct(tm_data$macro)

fS8 <- ggplot(tm_data,
              aes(area = n, fill = macro, label = micro, subgroup = macro)) +
  treemapify::geom_treemap(color = "white", size = 1, start = "topleft") +
  treemapify::geom_treemap_subgroup_border(color = "white", size = 4,
                                           start = "topleft") +
  treemapify::geom_treemap_subgroup_text(
    place = "bottomright", grow = FALSE, reflow = TRUE, alpha = 0.20,
    colour = "white", fontface = "bold", min.size = 0, start = "topleft") +
  treemapify::geom_treemap_text(
    colour = "white", place = "topleft", reflow = TRUE,
    grow = FALSE, size = 7, min.size = 4, start = "topleft") +
  scale_fill_manual(values = palette_q1_n(n_macro), name = "Macro-theme") +
  labs(
    title    = "All thematic keywords (micro-topics) in the HBV-Peru corpus",
    subtitle = sprintf("Grouped by macro-theme · tile area proportional to documents · %d keywords · n = %d documents · 1988–2023",
                       nrow(tm_data), nrow(M)),
    caption  = "Every micro-topic is displayed; exact counts for all keywords are listed in Supplementary Table S8."
  ) +
  theme_q1() +
  theme(
    legend.position = "bottom",
    axis.line  = element_blank(),
    axis.ticks = element_blank(),
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

save_q1(fS8, "all_keywords_treemap", number = 8, tier = "supplement",
        width = 12, height = 8.5)

message("\n[16_keyword_treemap.R] OK · Figure S8 (treemap, ", nrow(tm_data),
        " keywords) + Table S8 saved.\n")


#=============================================================================
# >>> PIPELINE COMPLETE
#=============================================================================

.MASTER_END <- Sys.time()
cat("\n", strrep("=",78), "\n  HBV MASTER PIPELINE COMPLETE\n", sep="")
cat(sprintf("  Runtime: %.1f min · figures: outputs/figures/ (PNG 600dpi + PDF)\n",
            as.numeric(difftime(.MASTER_END,.MASTER_START,units="mins"))))
cat(strrep("=",78), "\n", sep="")
