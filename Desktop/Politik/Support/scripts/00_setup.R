# ==================================================================
# 00_setup.R
# Packages, functions, data and period networks shared by all scripts.
# Run from the project root (open Support.Rproj).
# ==================================================================

# ------------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------------
packages <- c(
  "data.table", "readxl",                               # data
  "igraph", "ggraph", "graphlayouts", "ggforce",        # networks
  "ggplot2", "ggrepel", "scales", "dplyr",              # plotting (dplyr: cross_join in most_interventionist)
  "countrycode",                                        # ISO3 labels
  "clue"                                                # Hungarian alignment in smon_consensus
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing packages: ", paste(missing, collapse = ", "),
       "\nInstall with: install.packages(c(\"", paste(missing, collapse = "\", \""), "\"))")
}

invisible(lapply(packages, library, character.only = TRUE))


# ------------------------------------------------------------------
# 2. Functions: source everything under R/
# ------------------------------------------------------------------
for (f in list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)) {
  source(f)
}


# ------------------------------------------------------------------
# 3. Output folders (mirror the Overleaf structure)
# ------------------------------------------------------------------
for (d in c("Data", "Figures/ISN", "Figures/SAN", "Figures/Community_Detection")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}


# ------------------------------------------------------------------
# 4. Data
# ------------------------------------------------------------------
isn <- load_support_nocoalitions()                       # ISN: actor-dyad-year rows

san <- load_san(isn)                                     # yearly weighted SAN

disaggregated_san <- compute_dyadic_relationships(       # yearly multiplex SAN
  isn[!is.na(ext_name)]                                  # (one tie per shared conflict)
)


# ------------------------------------------------------------------
# 5. Periods and period SANs
# ------------------------------------------------------------------
epochs <- list(1975:1991, 1992:2001, 2002:2008, 2009:2017)
names(epochs) <- vapply(epochs, function(p) paste0(min(p), "-", max(p)), character(1))

san_periods <- lapply(epochs, function(p) build_period_san(isn, p))


# ------------------------------------------------------------------
# 6. Quick check
# ------------------------------------------------------------------
cat("Setup complete.\n",
    "ISN rows:", nrow(isn), "| yearly SAN rows:", nrow(san), "\n",
    "Period SANs (edges):",
    paste(names(san_periods), vapply(san_periods, nrow, integer(1)),
          sep = " = ", collapse = ", "), "\n")
