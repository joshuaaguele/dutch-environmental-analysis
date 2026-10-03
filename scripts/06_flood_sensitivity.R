# 06_flood_sensitivity.R
# Step 1: Read and audit the raw flood categories

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Install readxl first: install.packages('readxl')")
}

workbook_files <- list.files(
  "data_raw",
  pattern = "\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)

flood_workbook <- workbook_files[
  grepl("buurtdashboard", basename(workbook_files),
        ignore.case = TRUE)
]

if (length(flood_workbook) != 1L) {
  stop("Expected one Buurtdashboard workbook in data_raw.")
}

flood_raw <- readxl::read_excel(
  flood_workbook,
  sheet = "Data",
  na = c("", "NA", "NVT", "nvt"),
  guess_max = 20000
)

flood_columns <- c(
  "P_tot20cm",
  "P_20tot50cm",
  "P_50tot200cm",
  "P_200pluscm"
)

stopifnot(all(c(flood_columns, "AANT_INW") %in%
                names(flood_raw)))

flood_values <- as.data.frame(flood_raw[flood_columns])

all_flood_blank <- rowSums(is.na(flood_values)) == 4L

population <- as.numeric(flood_raw$AANT_INW)
valid_population <- is.finite(population) & population > 0

flood_audit <- data.frame(
  neighbourhoods = nrow(flood_raw),
  all_four_blank = sum(all_flood_blank),
  population_share_all_blank_pct =
    100 * sum(population[valid_population & all_flood_blank]) /
    sum(population[valid_population])
)

print(flood_audit)

# Step 2: Compare municipal flood calculations

stopifnot(all(vapply(flood_values, is.numeric, logical(1))))
stopifnot(all(c("gemeentecode", "gemeentenaam") %in%
                names(flood_raw)))

# Provider-confirmed baseline: blanks in these flood-depth categories mean no inundation.
flood_zero <- rowSums(
  flood_values[c("P_50tot200cm", "P_200pluscm")],
  na.rm = TRUE
)

# Conditional denominator sensitivity: exclude all-four-blank (zero-inundation) rows.
# NA here implements exclusion; it does not mean the provider data are missing.
# Historical names flood_missing_pct/flood_coverage_pct are retained for compatibility.
# flood_coverage_pct is the population-weighted share in retained neighbourhoods,
# not completeness or quality of source data. Both estimates summarize area percentages.
flood_missing <- flood_zero
flood_missing[all_flood_blank] <- NA_real_

flood_groups <- split(
  seq_len(nrow(flood_raw)),
  flood_raw$gemeentecode
)

flood_municipal <- do.call(
  rbind,
  lapply(flood_groups, function(rows) {
    populated <- rows[valid_population[rows]]
    covered <- populated[!is.na(flood_missing[populated])]
    
    total_population <- sum(population[populated])
    covered_population <- sum(population[covered])
    
    data.frame(
      municipality_code = flood_raw$gemeentecode[rows[1]],
      municipality_name = flood_raw$gemeentenaam[rows[1]],
      flood_zero_pct = if (total_population > 0) {
        weighted.mean(flood_zero[populated],
                      population[populated])
      } else NA_real_,
      flood_missing_pct = if (covered_population > 0) {
        weighted.mean(flood_missing[covered],
                      population[covered])
      } else NA_real_,
      flood_coverage_pct = if (total_population > 0) {
        100 * covered_population / total_population
      } else NA_real_
    )
  })
)

rownames(flood_municipal) <- NULL

flood_municipal$change_percentage_points <-
  flood_municipal$flood_missing_pct -
  flood_municipal$flood_zero_pct

print(data.frame(
  municipalities = nrow(flood_municipal),
  missing_alternative_flood =
    sum(is.na(flood_municipal$flood_missing_pct)),
  coverage_at_least_80_pct =
    sum(flood_municipal$flood_coverage_pct >= 80, na.rm = TRUE),
  coverage_at_least_90_pct =
    sum(flood_municipal$flood_coverage_pct >= 90, na.rm = TRUE)
))

print(summary(flood_municipal$flood_coverage_pct))

# Show the ten largest changes
largest_changes <- order(
  flood_municipal$change_percentage_points,
  decreasing = TRUE,
  na.last = NA
)

print(
  head(flood_municipal[largest_changes, ], 10),
  row.names = FALSE
)

# Step 3: Join the original data and verify the baseline

flood_original <- read.csv(
  "tables/municipality_environmental_indicators.csv",
  check.names = FALSE
)

flood_indicator_names <- c(
  "canopy_percent",
  "drought_depth_m",
  "heat_exposure_41plus_pct",
  "flood_exposure_over50cm_pct",
  "heavy_rain_exposure_20plus_pct"
)

stopifnot(all(c("municipality_code", flood_indicator_names) %in%
                names(flood_original)))
stopifnot(!anyDuplicated(flood_original$municipality_code))
stopifnot(!anyDuplicated(flood_municipal$municipality_code))

flood_comparison <- merge(
  flood_original,
  flood_municipal[
    c("municipality_code", "flood_zero_pct",
      "flood_missing_pct", "flood_coverage_pct")
  ],
  by = "municipality_code",
  all.x = TRUE,
  sort = FALSE
)

stopifnot(
  nrow(flood_comparison) == nrow(flood_original),
  all(is.finite(flood_comparison$flood_zero_pct))
)

baseline_difference <- max(
  abs(flood_comparison$flood_exposure_over50cm_pct -
        flood_comparison$flood_zero_pct),
  na.rm = TRUE
)

print(c(maximum_baseline_difference = baseline_difference))
stopifnot(baseline_difference < 1e-6)

flood_complete_original <- apply(
  flood_comparison[flood_indicator_names],
  1,
  function(x) all(is.finite(x))
)

coverage_thresholds <- c(0, 80, 90, 100)

flood_sample_counts <- data.frame(
  minimum_coverage_pct = coverage_thresholds,
  municipalities = vapply(
    coverage_thresholds,
    function(threshold) {
      sum(
        flood_complete_original &
          is.finite(flood_comparison$flood_missing_pct) &
          flood_comparison$flood_coverage_pct >= threshold - 1e-8,
        na.rm = TRUE
      )
    },
    integer(1)
  )
)

print(c(original_complete_municipalities =
          sum(flood_complete_original)))

print(flood_sample_counts, row.names = FALSE)

# Step 4: Compare PCA and clustering on identical samples

# Adjusted Rand index: compares partitions regardless of labels
flood_ari <- function(a, b) {
  membership <- table(a, b)
  total_pairs <- choose(sum(membership), 2)
  
  observed <- sum(choose(membership, 2))
  row_pairs <- sum(choose(rowSums(membership), 2))
  col_pairs <- sum(choose(colSums(membership), 2))
  
  expected <- row_pairs * col_pairs / total_pairs
  denominator <- (row_pairs + col_pairs) / 2 - expected
  
  if (abs(denominator) < 1e-12) {
    return(NA_real_)
  }
  
  (observed - expected) / denominator
}

flood_models <- list()
flood_results <- list()

for (threshold in coverage_thresholds) {
  
  keep <- flood_complete_original &
    is.finite(flood_comparison$flood_missing_pct) &
    is.finite(flood_comparison$flood_coverage_pct) &
    flood_comparison$flood_coverage_pct >= threshold - 1e-8
  
  sample_data <- flood_comparison[keep, ]
  sample_data <- sample_data[
    order(sample_data$municipality_code), ]
  
  # Same municipalities and five indicators in both versions
  x_zero <- as.matrix(sample_data[flood_indicator_names])
  x_missing <- x_zero
  
  x_missing[, "flood_exposure_over50cm_pct"] <-
    sample_data$flood_missing_pct
  
  stopifnot(
    nrow(x_zero) > 4L,
    all(is.finite(x_zero)),
    all(is.finite(x_missing)),
    all(apply(x_zero, 2, sd) > 0),
    all(apply(x_missing, 2, sd) > 0)
  )
  
  # Standardize separately, as in a full rerun of the analysis
  z_zero <- scale(x_zero)
  z_missing <- scale(x_missing)
  
  pca_zero <- prcomp(z_zero, center = FALSE, scale. = FALSE)
  pca_missing <- prcomp(z_missing, center = FALSE,
                        scale. = FALSE)
  
  set.seed(2026)
  clusters_zero <- kmeans(
    z_zero, centers = 4, nstart = 100, iter.max = 100
  )
  
  set.seed(2026)
  clusters_missing <- kmeans(
    z_missing, centers = 4, nstart = 100, iter.max = 100
  )
  
  key <- as.character(threshold)
  
  flood_models[[key]] <- list(
    data = sample_data,
    x_zero = x_zero,
    x_missing = x_missing,
    pca_zero = pca_zero,
    pca_missing = pca_missing,
    clusters_zero = clusters_zero,
    clusters_missing = clusters_missing
  )
  
  flood_results[[key]] <- data.frame(
    minimum_coverage_pct = threshold,
    municipalities = nrow(sample_data),
    adjusted_rand_index = flood_ari(
      clusters_zero$cluster,
      clusters_missing$cluster
    ),
    PC1_PC2_zero_pct =
      100 * sum(pca_zero$sdev[1:2]^2) /
      sum(pca_zero$sdev^2),
    PC1_PC2_missing_pct =
      100 * sum(pca_missing$sdev[1:2]^2) /
      sum(pca_missing$sdev^2)
  )
}

flood_cluster_summary <- do.call(rbind, flood_results)
rownames(flood_cluster_summary) <- NULL

print(
  round(flood_cluster_summary, 3),
  row.names = FALSE
)

# Control check: full-coverage samples should have equal inputs
stopifnot(
  max(abs(flood_models[["100"]]$x_zero -
            flood_models[["100"]]$x_missing)) < 1e-6
)

# Step 5: Save the flood sensitivity results

dir.create(
  "outputs/sensitivity",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  flood_municipal,
  "outputs/sensitivity/flood_municipal_comparison.csv",
  row.names = FALSE
)

write.csv(
  flood_sample_counts,
  "outputs/sensitivity/flood_sample_counts.csv",
  row.names = FALSE
)

write.csv(
  flood_cluster_summary,
  "outputs/sensitivity/flood_cluster_summary.csv",
  row.names = FALSE
)

saveRDS(
  flood_models,
  "outputs/sensitivity/flood_sensitivity_models.rds"
)

writeLines(
  capture.output(sessionInfo()),
  "outputs/sensitivity/flood_session_info.txt"
)

print(list.files(
  "outputs/sensitivity",
  pattern = "^flood_"
))
