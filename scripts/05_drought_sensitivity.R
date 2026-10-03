# Drought sensitivity check

sensitivity_data <- read.csv(
  "tables/municipality_environmental_indicators.csv"
)

indicator_names <- c(
  "canopy_percent",
  "drought_depth_m",
  "heat_exposure_41plus_pct",
  "flood_exposure_over50cm_pct",
  "heavy_rain_exposure_20plus_pct"
)

complete_rows <- complete.cases(
  sensitivity_data[, indicator_names]
)

sensitivity_complete <- sensitivity_data[complete_rows, ]
sensitivity_x <- sensitivity_complete[, indicator_names]

# Check the analysis sample and drought values
nrow(sensitivity_x)
summary(sensitivity_x$drought_depth_m)

# Identify the original clustering settings
original_code <- readLines(
  "scripts/04_full_environmental_analysis.R"
)

cat(
  original_code[
    grepl("set.seed|kmeans|nstart", original_code)
  ],
  sep = "\n"
)

# Reproduce the original results before testing sensitivity
print(nrow(sensitivity_x))
print(summary(sensitivity_x$drought_depth_m))

sensitivity_z <- scale(sensitivity_x)

baseline_pca <- prcomp(
  sensitivity_z,
  center = FALSE,
  scale. = FALSE
)

set.seed(2026)
baseline_clusters <- kmeans(
  sensitivity_z,
  centers = 4,
  nstart = 100,
  iter.max = 100
)

print(sort(baseline_clusters$size))

print(round(
  100 * baseline_clusters$betweenss /
    baseline_clusters$totss,
  2
))

print(round(baseline_pca$sdev^2, 3))

# Sensitivity analysis using log-transformed drought depth
stopifnot(all(sensitivity_x$drought_depth_m > 0))

log_x <- sensitivity_x
log_x$drought_depth_m <- log(log_x$drought_depth_m)
log_z <- scale(log_x)

log_pca <- prcomp(
  log_z,
  center = FALSE,
  scale. = FALSE
)

set.seed(2026)
log_clusters <- kmeans(
  log_z,
  centers = 4,
  nstart = 100,
  iter.max = 100
)

print(sort(log_clusters$size))
print(round(log_pca$sdev^2, 3))

# Compare membership; cluster numbers can change between runs
membership_table <- table(
  Original = baseline_clusters$cluster,
  Log_drought = log_clusters$cluster
)
print(membership_table)

# Adjusted Rand index: agreement independent of cluster numbering
pair_total <- choose(sum(membership_table), 2)
row_pairs <- sum(choose(rowSums(membership_table), 2))
col_pairs <- sum(choose(colSums(membership_table), 2))
observed_pairs <- sum(choose(membership_table, 2))
expected_pairs <- row_pairs * col_pairs / pair_total

log_ari <- (observed_pairs - expected_pairs) /
  ((row_pairs + col_pairs) / 2 - expected_pairs)

print(round(c(adjusted_rand_index = log_ari), 3))

# Show the details of the log-drought comparison
print(membership_table)
print(log_clusters$size)

print(round(
  100 * log_pca$sdev^2 / sum(log_pca$sdev^2),
  2
))

# Summarize the new clusters in the original measurement units
log_profiles <- aggregate(
  sensitivity_x,
  by = list(log_cluster = log_clusters$cluster),
  FUN = mean
)
print(log_profiles)

# Save the results
dir.create(
  "outputs/sensitivity",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  log_profiles,
  "outputs/sensitivity/log_drought_profiles.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(membership_table),
  "outputs/sensitivity/log_drought_membership.csv",
  row.names = FALSE
)

print(membership_table)
print(log_clusters$size)

print(round(
  100 * log_pca$sdev^2 / sum(log_pca$sdev^2),
  2
))

print(log_profiles, row.names = FALSE)

# Identify and exclude the highest-drought municipality
max_row <- which.max(sensitivity_x$drought_depth_m)

print(sensitivity_complete[
  max_row,
  c("municipality_code", "municipality_name", "drought_depth_m")
])

# Re-standardize the remaining observations
drop_x <- sensitivity_x[-max_row, ]
drop_z <- scale(drop_x)

drop_pca <- prcomp(
  drop_z, center = FALSE, scale. = FALSE
)

set.seed(2026)
drop_clusters <- kmeans(
  drop_z, centers = 4, nstart = 100, iter.max = 100
)

# Compare assignments for the same remaining municipalities
drop_table <- table(
  Original = baseline_clusters$cluster[-max_row],
  Excluding_max = drop_clusters$cluster
)

pair_total <- choose(sum(drop_table), 2)
row_pairs <- sum(choose(rowSums(drop_table), 2))
col_pairs <- sum(choose(colSums(drop_table), 2))
observed_pairs <- sum(choose(drop_table, 2))
expected_pairs <- row_pairs * col_pairs / pair_total

drop_ari <- (observed_pairs - expected_pairs) /
  ((row_pairs + col_pairs) / 2 - expected_pairs)

print(round(c(adjusted_rand_index = drop_ari), 3))
print(drop_table)
print(sort(drop_clusters$size))

# Save sensitivity comparison
sensitivity_summary <- data.frame(
  analysis = c("Log-transformed drought", "Highest drought excluded"),
  municipalities = c(nrow(log_x), nrow(drop_x)),
  adjusted_rand_index = c(log_ari, drop_ari),
  first_two_PC_percent = c(
    100 * sum(log_pca$sdev[1:2]^2) / sum(log_pca$sdev^2),
    100 * sum(drop_pca$sdev[1:2]^2) / sum(drop_pca$sdev^2)
  )
)

write.csv(
  sensitivity_summary,
  "outputs/sensitivity/drought_sensitivity_summary.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(drop_table),
  "outputs/sensitivity/excluding_max_membership.csv",
  row.names = FALSE
)

write.csv(
  sensitivity_complete[
    max_row,
    c("municipality_code", "municipality_name", "drought_depth_m")
  ],
  "outputs/sensitivity/excluded_municipality.csv",
  row.names = FALSE
)

print(sensitivity_summary)