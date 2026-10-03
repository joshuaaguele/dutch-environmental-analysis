# 04_buurtdashboard.R
# Purpose:
# Inspect and document neighbourhood-level environmental data from the
# Dutch Climate Impact Atlas Buurtdashboard workbook.
#
# Source: Klimaateffectatlas Buurtdashboard
# Raw workbook: Downloadbuurtdashboard_maa26-2.xlsx

# -------------------------------------------------------------------
# 1. Load packages
# -------------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "sf",
  "spdep",
  "spatialreg",
  "fpc"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(tidyverse)
library(readxl)
library(janitor)

dir.create("tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/data_audit", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/multivariate", recursive = TRUE, showWarnings = FALSE)
dir.create("data/spatial", recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------
# 2. Define and verify the raw-data file
# -------------------------------------------------------------------

dashboard_path <- file.path(
  "data_raw",
  "Downloadbuurtdashboard_maa26-2.xlsx"
)

stopifnot(file.exists(dashboard_path))

# -------------------------------------------------------------------
# 3. Inspect workbook structure
# -------------------------------------------------------------------

dashboard_sheets <- readxl::excel_sheets(dashboard_path)

print(dashboard_sheets)

# Expected worksheets:
# "Voorblad", "Metadata", "Data"

# -------------------------------------------------------------------
# 4. Import and clean the metadata
# -------------------------------------------------------------------

dashboard_metadata <- readxl::read_excel(
  dashboard_path,
  sheet = "Metadata"
) |>
  janitor::clean_names()

print(dim(dashboard_metadata))
print(names(dashboard_metadata))

# Expected dimensions for this workbook version:
# 87 rows and 9 columns

stopifnot(
  nrow(dashboard_metadata) == 87,
  ncol(dashboard_metadata) == 9
)

# Save a reproducible copy of the cleaned metadata
readr::write_csv(
  dashboard_metadata,
  "tables/buurtdashboard_metadata.csv"
)

# -------------------------------------------------------------------
# 5. Preview the main data sheet
# -------------------------------------------------------------------

# Read only five rows initially because the workbook is relatively large.

dashboard_preview <- readxl::read_excel(
  dashboard_path,
  sheet = "Data",
  n_max = 5
) |>
  janitor::clean_names()

print(dim(dashboard_preview))
print(names(dashboard_preview)[1:20])

# This workbook version contains 227 data columns.
stopifnot(ncol(dashboard_preview) == 227)

# -------------------------------------------------------------------
# 6. Broad search for climate-related metadata
# -------------------------------------------------------------------

climate_candidates <- dashboard_metadata |>
  dplyr::filter(
    dplyr::if_any(
      dplyr::everything(),
      ~ stringr::str_detect(
        stringr::str_to_lower(as.character(.x)),
        "boom|groen|hitte|warm|droog|waterdiepte|overstrom|neerslag|regen"
      )
    )
  ) |>
  dplyr::select(
    indicator,
    categorie,
    attribuutnaam,
    eenheid,
    omschrijving_kort
  )

print(climate_candidates, n = Inf)

# -------------------------------------------------------------------
# 7. Search specifically within official indicator names
# -------------------------------------------------------------------

climate_indicator_matches <- dashboard_metadata |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(indicator),
      "boom|groen|hitte|temperatuur|droog|waterdiepte|overstrom|neerslag|regen"
    )
  ) |>
  dplyr::select(
    indicator,
    categorie,
    attribuutnaam,
    eenheid
  ) |>
  dplyr::arrange(indicator, attribuutnaam)

print(climate_indicator_matches, n = Inf)

print(
  sort(unique(climate_indicator_matches$indicator))
)

# -------------------------------------------------------------------
# 8. Search for possible drought indicators
# -------------------------------------------------------------------

drought_candidates <- dashboard_metadata |>
  dplyr::filter(
    dplyr::if_any(
      dplyr::everything(),
      ~ stringr::str_detect(
        stringr::str_to_lower(as.character(.x)),
        "droog|grondwater|bodemvocht|verdroging|laagste grondwater"
      )
    )
  ) |>
  dplyr::select(
    indicator,
    categorie,
    attribuutnaam,
    eenheid,
    omschrijving_kort
  ) |>
  dplyr::distinct()

print(drought_candidates, n = Inf, width = Inf)

# Selected drought-exposure proxy:
# Extreem lage grondwaterstand, attribute LG3MaxHuidig.
#
# Interpretation:
# Current groundwater level during an extremely dry year, measured in
# metres below ground level. Larger values represent deeper groundwater
# and potentially greater drought exposure.
#
# Source identified in the metadata:
# Klimaateffectatlas / Nationaal Water Model (2019).
#
# Important limitation:
# This is a modelled, indicative groundwater statistic rather than a
# direct observation of drought damage.

drought_choice <- dashboard_metadata |>
  dplyr::filter(
    indicator == "Extreem lage grondwaterstand"
  )

print(
  t(as.data.frame(drought_choice)),
  quote = FALSE
)

# -------------------------------------------------------------------
# 9. Inspect the five proposed environmental dimensions
# -------------------------------------------------------------------

selected_indicator_details <- dashboard_metadata |>
  dplyr::filter(
    indicator %in% c(
      "Boomkroonoppervlakte",
      "Extreem lage grondwaterstand",
      "Gevoelstemperatuur - AHN4",
      "Maximale overstromingsdiepte",
      "Waterdiepte bij hevige bui"
    )
  ) |>
  dplyr::select(
    indicator,
    attribuutnaam,
    eenheid,
    omschrijving_kort
  ) |>
  dplyr::arrange(indicator, attribuutnaam)

print(
  selected_indicator_details,
  n = Inf,
  width = Inf
)

# Save the candidate-variable documentation
readr::write_csv(
  selected_indicator_details,
  "tables/buurtdashboard_selected_indicator_details.csv"
)

# -------------------------------------------------------------------
# 10. Display the available heat categories
# -------------------------------------------------------------------

heat_options <- selected_indicator_details |>
  dplyr::filter(
    indicator == "Gevoelstemperatuur - AHN4"
  ) |>
  dplyr::select(
    attribuutnaam,
    eenheid,
    omschrijving_kort
  )

print(
  heat_options,
  n = Inf,
  width = Inf
)
# -------------------------------------------------------------------
# 11. Define the proposed heat-exposure measure
# -------------------------------------------------------------------

# Heat exposure will be calculated as the combined percentage of area
# in the following apparent-temperature categories:
# 41–45°C, 46–50°C, and 51°C or higher.

heat_attributes <- c(
  "pet41tm45p",
  "pet46tm50p",
  "pet51plusp"
)

# -------------------------------------------------------------------
# 13. Inspect the definition and source of the heavy-rain measure
# -------------------------------------------------------------------

heavy_rain_definition <- dashboard_metadata |>
  dplyr::filter(
    indicator == "Waterdiepte bij hevige bui"
  ) |>
  dplyr::select(
    bron,
    omschrijving_lang
  ) |>
  dplyr::distinct()

print(
  t(as.data.frame(heavy_rain_definition)),
  quote = FALSE
)
# -------------------------------------------------------------------
# 14. Define and verify the final environmental columns
# -------------------------------------------------------------------

# Tree-canopy coverage: AHN4 version, whole neighbourhood
canopy_attribute <- "bk_bgraad_tot_perc_land_ahn4"

# Drought exposure: extreme low groundwater in the current climate
drought_attribute <- "lg3max_huidig"

# Heat exposure: apparent temperature of at least 41°C, AHN4
heat_attributes <- c(
  "pet41tm45p_ahn4",
  "pet46tm50p_ahn4",
  "pet51plusp_ahn4"
)

# Flood exposure: maximum depth greater than 50 cm during an
# extremely low-probability flood event
flood_attributes <- c(
  "p_50tot200cm",
  "p_200pluscm"
)

# Heavy-rain exposure: water depth of at least 20 cm
# Note: column names use "mm", but metadata descriptions use centimetres.
heavy_rain_attributes <- c(
  "perc20_30mm",
  "perc30mm_me"
)

selected_environmental_columns <- c(
  canopy_attribute,
  drought_attribute,
  heat_attributes,
  flood_attributes,
  heavy_rain_attributes
)

missing_environmental_columns <- setdiff(
  selected_environmental_columns,
  names(dashboard_preview)
)

print(selected_environmental_columns)
print(missing_environmental_columns)

stopifnot(length(missing_environmental_columns) == 0)
# -------------------------------------------------------------------
# 15. Find the actual environmental column names in the Data sheet
# -------------------------------------------------------------------

possible_environmental_names <- names(dashboard_preview)[
  stringr::str_detect(
    names(dashboard_preview),
    "bkb|boom|graad|pet|gevoel|perc.*20|perc.*30|lg3|p_50|p_200"
  )
]

print(possible_environmental_names)

# Display tree-canopy column names separately.

canopy_data_names <- names(dashboard_preview)[
  stringr::str_detect(
    names(dashboard_preview),
    "bkb|boom|graad"
  )
]

print(canopy_data_names)

# Display heavy-rain column names separately.

heavy_rain_data_names <- names(dashboard_preview)[
  stringr::str_detect(
    names(dashboard_preview),
    "^perc(5|10|15|20|30|nodata)"
  )
]

print(heavy_rain_data_names)
# -------------------------------------------------------------------
# 16. Inspect the complete tree-canopy metadata
# -------------------------------------------------------------------

canopy_definition <- dashboard_metadata |>
  dplyr::filter(
    indicator == "Boomkroonoppervlakte"
  )

print(
  t(as.data.frame(canopy_definition)),
  quote = FALSE
)
# -------------------------------------------------------------------
# 17. Import the complete Buurtdashboard data
# -------------------------------------------------------------------

dashboard_data <- readxl::read_excel(
  dashboard_path,
  sheet = "Data",
  na = c("", "NA", "NVT", "nvt"),
  guess_max = 10000
) |>
  janitor::clean_names()

print(dim(dashboard_data))
print(object.size(dashboard_data), units = "MB")
# -------------------------------------------------------------------
# 18. Create the neighbourhood-level environmental working dataset
# -------------------------------------------------------------------

identifier_columns <- c(
  "objectid",
  "buurtcode2024",
  "buurtnaam",
  "gemeentecode",
  "gemeentenaam",
  "aant_inw"
)

missing_identifier_columns <- setdiff(
  identifier_columns,
  names(dashboard_data)
)

print(missing_identifier_columns)

stopifnot(length(missing_identifier_columns) == 0)

environmental_neighbourhood <- dashboard_data |>
  dplyr::select(
    dplyr::all_of(identifier_columns),
    dplyr::all_of(selected_environmental_columns)
  )

environmental_structure <- environmental_neighbourhood |>
  dplyr::summarise(
    rows = dplyr::n(),
    neighbourhoods = dplyr::n_distinct(
      buurtcode2024,
      na.rm = TRUE
    ),
    municipalities = dplyr::n_distinct(
      gemeentecode,
      na.rm = TRUE
    ),
    missing_neighbourhood_codes = sum(is.na(buurtcode2024)),
    missing_municipality_codes = sum(is.na(gemeentecode))
  )

print(environmental_structure)
dplyr::glimpse(environmental_neighbourhood)
# -------------------------------------------------------------------
# 19. Diagnose negative missing-value codes
# -------------------------------------------------------------------

print(environmental_structure)

negative_value_summary <- environmental_neighbourhood |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(selected_environmental_columns),
      ~ sum(.x < 0, na.rm = TRUE)
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "negative_values"
  )

print(
  negative_value_summary,
  n = Inf
)
# -------------------------------------------------------------------
# 20. Compare tree-canopy coverage across AHN versions
# -------------------------------------------------------------------

canopy_version_columns <- c(
  "bk_bgraad_tot_perc_land_ahn3",
  "bk_bgraad_tot_perc_land_ahn4",
  "bk_bgraad_tot_perc_land_ahn5"
)

canopy_version_availability <- dashboard_data |>
  dplyr::select(
    dplyr::all_of(canopy_version_columns)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "canopy_version",
    values_to = "value"
  ) |>
  dplyr::group_by(canopy_version) |>
  dplyr::summarise(
    total_rows = dplyr::n(),
    valid_values = sum(!is.na(value) & value >= 0),
    negative_codes = sum(value < 0, na.rm = TRUE),
    ordinary_missing = sum(is.na(value)),
    coverage_percent = round(
      mean(!is.na(value) & value >= 0) * 100,
      1
    ),
    .groups = "drop"
  )

print(canopy_version_availability)
# -------------------------------------------------------------------
# 21. Replace negative missing-value codes with NA
# -------------------------------------------------------------------

environmental_neighbourhood_clean <- environmental_neighbourhood |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(selected_environmental_columns),
      ~ dplyr::if_else(.x < 0, NA_real_, .x)
    )
  )

remaining_negative_values <- environmental_neighbourhood_clean |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(selected_environmental_columns),
      ~ sum(.x < 0, na.rm = TRUE)
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "negative_values"
  )

print(
  remaining_negative_values,
  n = Inf
)
# -------------------------------------------------------------------
# 22. Clean and inspect the neighbourhood population variable
# -------------------------------------------------------------------

environmental_neighbourhood_clean <-
  environmental_neighbourhood_clean |>
  dplyr::mutate(
    population = as.numeric(aant_inw)
  )

population_check <- environmental_neighbourhood_clean |>
  dplyr::summarise(
    total_rows = dplyr::n(),
    original_missing = sum(is.na(aant_inw)),
    numeric_missing = sum(is.na(population)),
    zero_population = sum(population == 0, na.rm = TRUE),
    negative_population = sum(population < 0, na.rm = TRUE),
    minimum_population = min(population, na.rm = TRUE),
    maximum_population = max(population, na.rm = TRUE)
  )

print(population_check)
# -------------------------------------------------------------------
# 23. Create the five neighbourhood environmental indicators
# -------------------------------------------------------------------

environmental_indicators <- environmental_neighbourhood_clean |>
  dplyr::mutate(
    canopy_percent = bk_bgraad_tot_perc_land_ahn4,

    drought_depth_m = lg3max_huidig,

    heat_exposure_41plus = dplyr::if_else(
      dplyr::if_all(
        dplyr::all_of(c(
          "pet41tm45p_ahn4",
          "pet46tm50p_ahn4",
          "pet51plusp_ahn4"
        )),
        is.na
      ),
      NA_real_,
      rowSums(
        dplyr::pick(
          dplyr::all_of(c(
            "pet41tm45p_ahn4",
            "pet46tm50p_ahn4",
            "pet51plusp_ahn4"
          ))
        ),
        na.rm = TRUE
      )
    ),

    flood_exposure_over50cm = dplyr::if_else(
      dplyr::if_all(
        dplyr::all_of(c(
          "p_50tot200cm",
          "p_200pluscm"
        )),
        is.na
      ),
      NA_real_,
      rowSums(
        dplyr::pick(
          dplyr::all_of(c(
            "p_50tot200cm",
            "p_200pluscm"
          ))
        ),
        na.rm = TRUE
      )
    ),

    heavy_rain_exposure_20plus = dplyr::if_else(
      dplyr::if_all(
        dplyr::all_of(c(
          "perc20_30mm",
          "perc30mm_me"
        )),
        is.na
      ),
      NA_real_,
      rowSums(
        dplyr::pick(
          dplyr::all_of(c(
            "perc20_30mm",
            "perc30mm_me"
          ))
        ),
        na.rm = TRUE
      )
    )
  )

environmental_indicator_names <- c(
  "canopy_percent",
  "drought_depth_m",
  "heat_exposure_41plus",
  "flood_exposure_over50cm",
  "heavy_rain_exposure_20plus"
)

indicator_summary <- environmental_indicators |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(environmental_indicator_names),
      list(
        missing = ~ sum(is.na(.x)),
        minimum = ~ min(.x, na.rm = TRUE),
        maximum = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("indicator", ".value"),
    names_pattern = "^(.*)_(missing|minimum|maximum|mean)$"
  )

print(indicator_summary, n = Inf)
# -------------------------------------------------------------------
# 24. Inspect indicator distributions and possible scale differences
# -------------------------------------------------------------------

indicator_quantiles <- environmental_indicators |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(environmental_indicator_names),
      list(
        p00 = ~ quantile(.x, 0.00, na.rm = TRUE),
        p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
        p50 = ~ quantile(.x, 0.50, na.rm = TRUE),
        p75 = ~ quantile(.x, 0.75, na.rm = TRUE),
        p95 = ~ quantile(.x, 0.95, na.rm = TRUE),
        p99 = ~ quantile(.x, 0.99, na.rm = TRUE),
        p100 = ~ quantile(.x, 1.00, na.rm = TRUE)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("indicator", ".value"),
    names_pattern = "^(.*)_(p00|p25|p50|p75|p95|p99|p100)$"
  )

print(indicator_quantiles, n = Inf, width = Inf)

highest_drought_values <- environmental_indicators |>
  dplyr::select(
    buurtcode2024,
    buurtnaam,
    gemeentenaam,
    drought_depth_m
  ) |>
  dplyr::arrange(dplyr::desc(drought_depth_m)) |>
  dplyr::slice_head(n = 15)

print(highest_drought_values, n = 15, width = Inf)
# -------------------------------------------------------------------
# 25. Harmonise indicator measurement scales
# -------------------------------------------------------------------

environmental_indicators_scaled <- environmental_indicators |>
  dplyr::mutate(
    heat_exposure_41plus_pct =
      heat_exposure_41plus * 100,

    heavy_rain_exposure_20plus_pct =
      heavy_rain_exposure_20plus * 100,

    flood_exposure_over50cm_pct =
      flood_exposure_over50cm
  )

scaled_indicator_names <- c(
  "canopy_percent",
  "drought_depth_m",
  "heat_exposure_41plus_pct",
  "flood_exposure_over50cm_pct",
  "heavy_rain_exposure_20plus_pct"
)

scaled_indicator_summary <- environmental_indicators_scaled |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(scaled_indicator_names),
      list(
        missing = ~ sum(is.na(.x)),
        minimum = ~ min(.x, na.rm = TRUE),
        maximum = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("indicator", ".value"),
    names_pattern = "^(.*)_(missing|minimum|maximum|mean)$"
  )

print(
  scaled_indicator_summary,
  n = Inf,
  width = Inf
)
# -------------------------------------------------------------------
# 26. Aggregate environmental indicators to municipalities
# -------------------------------------------------------------------

safe_weighted_mean <- function(x, weights) {
  valid <- !is.na(x) & !is.na(weights) & weights > 0

  if (!any(valid)) {
    return(NA_real_)
  }

  stats::weighted.mean(
    x = x[valid],
    w = weights[valid]
  )
}

population_coverage <- function(x, weights) {
  valid_population <- !is.na(weights) & weights > 0
  available_population <- valid_population & !is.na(x)

  total_population <- sum(weights[valid_population])

  if (total_population == 0) {
    return(NA_real_)
  }

  100 * sum(weights[available_population]) / total_population
}

municipality_environmental <- environmental_indicators_scaled |>
  dplyr::group_by(
    gemeentecode,
    gemeentenaam
  ) |>
  dplyr::summarise(
    neighbourhoods = dplyr::n(),
    municipality_population = sum(population, na.rm = TRUE),

    dplyr::across(
      dplyr::all_of(scaled_indicator_names),
      list(
        weighted_mean = ~ safe_weighted_mean(.x, population),
        population_coverage_pct = ~ population_coverage(.x, population)
      )
    ),

    .groups = "drop"
  )

municipality_environmental_check <- municipality_environmental |>
  dplyr::summarise(
    municipalities = dplyr::n(),
    minimum_neighbourhoods = min(neighbourhoods),
    maximum_neighbourhoods = max(neighbourhoods),
    zero_population_municipalities =
      sum(municipality_population == 0)
  )

print(
  municipality_environmental_check,
  width = Inf
)

dplyr::glimpse(municipality_environmental)
# -------------------------------------------------------------------
# 27. Assess municipal indicator completeness and coverage
# -------------------------------------------------------------------

municipal_missing_summary <- municipality_environmental |>
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_weighted_mean"),
      ~ sum(is.na(.x))
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "indicator",
    values_to = "municipalities_missing"
  )

municipal_coverage_summary <- municipality_environmental |>
  dplyr::summarise(
    dplyr::across(
      dplyr::ends_with("_population_coverage_pct"),
      list(
        minimum = ~ min(.x, na.rm = TRUE),
        p25 = ~ quantile(.x, 0.25, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        p75 = ~ quantile(.x, 0.75, na.rm = TRUE),
        maximum = ~ max(.x, na.rm = TRUE),
        below_80 = ~ sum(.x < 80, na.rm = TRUE)
      )
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = c("indicator", ".value"),
    names_pattern =
      "^(.*)_(minimum|p25|median|p75|maximum|below_80)$"
  )

print(
  municipal_missing_summary,
  n = Inf,
  width = Inf
)

print(
  municipal_coverage_summary,
  n = Inf,
  width = Inf
)
# -------------------------------------------------------------------
# 28. Investigate municipal flood-data availability
# -------------------------------------------------------------------

flood_coverage_review <- municipality_environmental |>
  dplyr::transmute(
    gemeentecode,
    gemeentenaam,
    municipality_population,
    flood_exposure =
      flood_exposure_over50cm_pct_weighted_mean,
    flood_population_coverage =
      flood_exposure_over50cm_pct_population_coverage_pct,

    coverage_category = dplyr::case_when(
      is.na(flood_exposure) |
        flood_population_coverage == 0 ~ "No flood data",

      flood_population_coverage < 80 ~ "Below 80% coverage",

      TRUE ~ "At least 80% coverage"
    )
  )

flood_coverage_counts <- flood_coverage_review |>
  dplyr::count(
    coverage_category,
    name = "municipalities"
  )

municipalities_without_flood_data <- flood_coverage_review |>
  dplyr::filter(coverage_category == "No flood data") |>
  dplyr::arrange(gemeentenaam)

lowest_flood_coverage <- flood_coverage_review |>
  dplyr::filter(coverage_category != "No flood data") |>
  dplyr::arrange(flood_population_coverage) |>
  dplyr::slice_head(n = 20)

print(
  flood_coverage_counts,
  n = Inf,
  width = Inf
)

print(
  municipalities_without_flood_data,
  n = Inf,
  width = Inf
)

print(
  lowest_flood_coverage,
  n = 20,
  width = Inf
)

municipalities_without_flood_data |>
  dplyr::select(
    gemeentecode,
    gemeentenaam,
    municipality_population
  ) |>
  print(n = Inf)
# -------------------------------------------------------------------
# 29. Investigate the meaning of missing flood values
# -------------------------------------------------------------------

flood_definition <- dashboard_metadata |>
  dplyr::filter(
    indicator == "Maximale overstromingsdiepte"
  ) |>
  dplyr::select(
    indicator,
    categorie,
    attribuutnaam,
    eenheid,
    bron,
    omschrijving_kort,
    omschrijving_lang
  ) |>
  dplyr::distinct()

print(
  t(as.data.frame(flood_definition)),
  quote = FALSE
)

all_flood_columns <- c(
  "p_tot20cm",
  "p_20tot50cm",
  "p_50tot200cm",
  "p_200pluscm"
)

flood_missing_patterns <- dashboard_data |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(all_flood_columns),
      ~ sum(is.na(.x))
    ),
    all_four_missing = sum(
      dplyr::if_all(
        dplyr::all_of(all_flood_columns),
        is.na
      )
    )
  )

print(
  flood_missing_patterns,
  width = Inf
)

no_flood_data_examples <- dashboard_data |>
  dplyr::filter(
    dplyr::if_all(
      dplyr::all_of(all_flood_columns),
      is.na
    )
  ) |>
  dplyr::select(
    buurtcode2024,
    buurtnaam,
    gemeentenaam,
    aant_inw,
    dplyr::all_of(all_flood_columns)
  ) |>
  dplyr::slice_head(n = 20)

print(
  no_flood_data_examples,
  n = 20,
  width = Inf
)
# -------------------------------------------------------------------
# 30. Check whether flood-depth categories form a complete distribution
# -------------------------------------------------------------------

flood_category_check <- dashboard_data |>
  dplyr::mutate(
    flood_category_total = rowSums(
      dplyr::pick(
        dplyr::all_of(all_flood_columns)
      ),
      na.rm = TRUE
    )
  )

flood_total_summary <- flood_category_check |>
  dplyr::summarise(
    rows = dplyr::n(),
    minimum = min(flood_category_total),
    p25 = quantile(flood_category_total, 0.25),
    median = median(flood_category_total),
    p75 = quantile(flood_category_total, 0.75),
    maximum = max(flood_category_total),
    approximately_100 = sum(
      dplyr::between(flood_category_total, 99.99, 100.01)
    ),
    below_99_99 = sum(flood_category_total < 99.99),
    above_100_01 = sum(flood_category_total > 100.01)
  )

print(
  flood_total_summary,
  width = Inf
)

flood_total_examples <- flood_category_check |>
  dplyr::select(
    buurtcode2024,
    buurtnaam,
    gemeentenaam,
    dplyr::all_of(all_flood_columns),
    flood_category_total
  ) |>
  dplyr::arrange(flood_category_total) |>
  dplyr::slice_head(n = 20)

print(
  flood_total_examples,
  n = 20,
  width = Inf
)

# -------------------------------------------------------------------
# 31. Correct flood-category structural missing values
# -------------------------------------------------------------------

environmental_indicators_final <-
  environmental_indicators_scaled |>
  dplyr::mutate(
    flood_exposure_over50cm_pct =
      dplyr::coalesce(p_50tot200cm, 0) +
      dplyr::coalesce(p_200pluscm, 0)
  )

municipality_environmental_final <-
  environmental_indicators_final |>
  dplyr::group_by(
    gemeentecode,
    gemeentenaam
  ) |>
  dplyr::summarise(
    neighbourhoods = dplyr::n(),

    municipality_population =
      sum(population, na.rm = TRUE),

    dplyr::across(
      dplyr::all_of(scaled_indicator_names),
      list(
        weighted_mean =
          ~ safe_weighted_mean(.x, population),

        population_coverage_pct =
          ~ population_coverage(.x, population)
      )
    ),

    .groups = "drop"
  )

final_municipal_check <- municipality_environmental_final |>
  dplyr::summarise(
    municipalities = dplyr::n(),

    missing_canopy = sum(
      is.na(canopy_percent_weighted_mean)
    ),

    missing_drought = sum(
      is.na(drought_depth_m_weighted_mean)
    ),

    missing_heat = sum(
      is.na(heat_exposure_41plus_pct_weighted_mean)
    ),

    missing_flood = sum(
      is.na(flood_exposure_over50cm_pct_weighted_mean)
    ),

    missing_heavy_rain = sum(
      is.na(heavy_rain_exposure_20plus_pct_weighted_mean)
    ),

    minimum_flood_coverage = min(
      flood_exposure_over50cm_pct_population_coverage_pct,
      na.rm = TRUE
    )
  )

print(
  final_municipal_check,
  width = Inf
)
# -------------------------------------------------------------------
# 32. Save the final municipality environmental dataset
# -------------------------------------------------------------------

municipality_environmental_analysis <-
  municipality_environmental_final |>
  dplyr::transmute(
    municipality_code = gemeentecode,
    municipality_name = gemeentenaam,
    neighbourhoods,
    municipality_population,

    canopy_percent =
      canopy_percent_weighted_mean,

    drought_depth_m =
      drought_depth_m_weighted_mean,

    heat_exposure_41plus_pct =
      heat_exposure_41plus_pct_weighted_mean,

    flood_exposure_over50cm_pct =
      flood_exposure_over50cm_pct_weighted_mean,

    heavy_rain_exposure_20plus_pct =
      heavy_rain_exposure_20plus_pct_weighted_mean
  )

readr::write_csv(
  municipality_environmental_analysis,
  "tables/municipality_environmental_indicators.csv",
  na = ""
)

print(dim(municipality_environmental_analysis))
print(names(municipality_environmental_analysis))

# Identify municipality-related CBS objects currently available.
print(
  ls(
    pattern = "municip|gemeente",
    envir = .GlobalEnv
  )
)

# ============================================================
# 33. AUDIT THE FINAL DATASET BEFORE MULTIVARIATE ANALYSIS
# ============================================================

# ------------------------------------------------------------
# 33.1 Load the final municipality dataset
# ------------------------------------------------------------

data_file <- "tables/municipality_environmental_indicators.csv"

stopifnot(file.exists(data_file))

message("Dataset selected: ", data_file)

analysis_data <- read_csv(
  data_file,
  show_col_types = FALSE
) |>
  clean_names()

# ------------------------------------------------------------
# 33.2 Basic dataset dimensions
# ------------------------------------------------------------

dataset_summary <- tibble(
  dataset_file = data_file,
  number_of_rows = nrow(analysis_data),
  number_of_columns = ncol(analysis_data),
  duplicate_rows = sum(duplicated(analysis_data))
)

print(dataset_summary)

# ------------------------------------------------------------
# 33.3 Identify numeric variables suitable for analysis
# ------------------------------------------------------------

numeric_data <- analysis_data |>
  select(where(is.numeric)) |>
  select(
    -matches(
      "code|id|year|number|nummer",
      ignore.case = TRUE
    )
  )

message(
  "Number of candidate numeric indicators: ",
  ncol(numeric_data)
)

# ------------------------------------------------------------
# 33.4 Produce a variable-level quality table
# ------------------------------------------------------------

variable_audit <- purrr::map_dfr(
  names(numeric_data),
  function(variable_name) {

    x <- numeric_data[[variable_name]]
    sd_result <- sd(x, na.rm = TRUE)
    missing_result <- round(mean(is.na(x)) * 100, 2)
    unique_result <- n_distinct(x, na.rm = TRUE)

    problem_result <- case_when(
      missing_result > 20 ~ "High missingness",
      !is.na(sd_result) & sd_result == 0 ~ "No variation",
      unique_result < 3 ~ "Too few distinct values",
      TRUE ~ "No immediate problem"
    )

    tibble(
      variable = variable_name,
      observations = length(x),
      available_values = sum(!is.na(x)),
      missing_values = sum(is.na(x)),
      missing_percent = missing_result,
      unique_values = unique_result,
      mean = round(mean(x, na.rm = TRUE), 3),
      standard_deviation = round(sd_result, 3),
      minimum = round(min(x, na.rm = TRUE), 3),
      median = round(median(x, na.rm = TRUE), 3),
      maximum = round(max(x, na.rm = TRUE), 3),
      zero_variance = !is.na(sd_result) & sd_result == 0,
      possible_problem = problem_result
    )
  }
) |>
  arrange(desc(missing_percent))

print(variable_audit, n = Inf)

# ------------------------------------------------------------
# 33.5 Count complete observations
# ------------------------------------------------------------

complete_case_summary <- tibble(
  total_municipalities = nrow(numeric_data),
  complete_municipalities = sum(complete.cases(numeric_data)),
  incomplete_municipalities = sum(!complete.cases(numeric_data)),
  complete_case_percent = round(
    mean(complete.cases(numeric_data)) * 100,
    2
  )
)

print(complete_case_summary)

# ------------------------------------------------------------
# 33.6 Visualise missingness by variable
# ------------------------------------------------------------

missingness_plot <- variable_audit |>
  ggplot(
    aes(
      x = reorder(variable, missing_percent),
      y = missing_percent
    )
  ) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  labs(
    title = "Missing Data in Candidate Municipality Indicators",
    subtitle = "Screening before multivariate analysis",
    x = NULL,
    y = "Missing observations (%)",
    caption = "Source: Author's calculations from the assembled dataset"
  ) +
  theme_minimal(base_size = 12)

print(missingness_plot)

# ------------------------------------------------------------
# 33.7 Save reproducible audit outputs
# ------------------------------------------------------------

dir.create(
  "outputs/data_audit",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  dataset_summary,
  "outputs/data_audit/dataset_summary.csv"
)

write_csv(
  variable_audit,
  "outputs/data_audit/variable_quality_audit.csv"
)

write_csv(
  complete_case_summary,
  "outputs/data_audit/complete_case_summary.csv"
)

ggsave(
  filename = "outputs/data_audit/missingness_by_variable.png",
  plot = missingness_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# Confirm saved outputs
list.files(
  "outputs/data_audit",
  full.names = TRUE
)

# ============================================================
# 34. PREPARE DATA FOR MULTIVARIATE ANALYSIS
# ============================================================

# ------------------------------------------------------------
# 34.1 Select environmental vulnerability indicators
# ------------------------------------------------------------

indicator_names <- c(
  "drought_depth_m",
  "canopy_percent",
  "heat_exposure_41plus_pct",
  "flood_exposure_over50cm_pct",
  "heavy_rain_exposure_20plus_pct"
)

# Check that all required indicators exist
missing_indicators <- setdiff(
  indicator_names,
  names(analysis_data)
)

if (length(missing_indicators) > 0) {
  stop(
    "These indicators were not found: ",
    paste(missing_indicators, collapse = ", ")
  )
}

# Keep municipalities with complete information on all five indicators
complete_rows <- complete.cases(
  analysis_data[, indicator_names]
)

analysis_ready <- analysis_data |>
  filter(complete_rows)

indicator_data <- analysis_ready |>
  select(all_of(indicator_names))

analysis_n <- nrow(analysis_ready)
analysis_caption <- paste0(
  "Source: Author's calculations from ",
  analysis_n,
  " Dutch municipalities"
)

# Confirm the resulting dimensions
analysis_ready_summary <- tibble(
  original_municipalities = nrow(analysis_data),
  retained_municipalities = nrow(analysis_ready),
  excluded_municipalities = sum(!complete_rows),
  environmental_indicators = ncol(indicator_data)
)

print(analysis_ready_summary)
print(names(indicator_data))

# ------------------------------------------------------------
# 34.2 Examine Pearson correlations between indicators
# ------------------------------------------------------------

correlation_matrix <- cor(
  indicator_data,
  use = "complete.obs",
  method = "pearson"
)

correlation_matrix_rounded <- round(
  correlation_matrix,
  3
)

print(correlation_matrix_rounded)

# ------------------------------------------------------------
# 34.3 Create a readable pairwise correlation table
# ------------------------------------------------------------

correlation_pairs <- combn(
  names(indicator_data),
  2,
  simplify = FALSE
) |>
  purrr::map_dfr(
    function(indicator_pair) {
      tibble(
        indicator_1 = indicator_pair[1],
        indicator_2 = indicator_pair[2],
        correlation = round(
          cor(
            indicator_data[[indicator_pair[1]]],
            indicator_data[[indicator_pair[2]]],
            method = "pearson"
          ),
          3
        )
      )
    }
  ) |>
  arrange(desc(abs(correlation)))

print(correlation_pairs, n = Inf)

# ------------------------------------------------------------
# 34.4 Create and save the correlation heatmap
# ------------------------------------------------------------

correlation_long <- as.data.frame(
  as.table(correlation_matrix)
) |>
  as_tibble() |>
  rename(
    indicator_1 = Var1,
    indicator_2 = Var2,
    correlation = Freq
  ) |>
  mutate(
    indicator_1 = recode(
      as.character(indicator_1),
      drought_depth_m = "Drought depth",
      canopy_percent = "Canopy cover",
      heat_exposure_41plus_pct = "Heat exposure",
      flood_exposure_over50cm_pct = "Flood exposure",
      heavy_rain_exposure_20plus_pct = "Heavy-rain exposure"
    ),
    indicator_2 = recode(
      as.character(indicator_2),
      drought_depth_m = "Drought depth",
      canopy_percent = "Canopy cover",
      heat_exposure_41plus_pct = "Heat exposure",
      flood_exposure_over50cm_pct = "Flood exposure",
      heavy_rain_exposure_20plus_pct = "Heavy-rain exposure"
    )
  )

correlation_heatmap <- ggplot(
  correlation_long,
  aes(
    x = indicator_1,
    y = indicator_2,
    fill = correlation
  )
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = sprintf("%.2f", correlation)),
    size = 4
  ) +
  scale_fill_gradient2(
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson r"
  ) +
  coord_equal() +
  labs(
    title = "Correlations Between Municipal Environmental Indicators",
    subtitle = paste0(
      "Complete-case sample: ",
      analysis_n,
      " Dutch municipalities"
    ),
    x = NULL,
    y = NULL,
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

print(correlation_heatmap)

# Save correlation outputs
dir.create(
  "outputs/multivariate",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  correlation_pairs,
  "outputs/multivariate/pairwise_correlations.csv"
)

correlation_matrix_table <- correlation_matrix_rounded |>
  as.data.frame() |>
  rownames_to_column("indicator")

write_csv(
  correlation_matrix_table,
  "outputs/multivariate/correlation_matrix.csv"
)

ggsave(
  filename = "outputs/multivariate/correlation_heatmap.png",
  plot = correlation_heatmap,
  width = 10,
  height = 8,
  dpi = 300
)

# ============================================================
# 35. PRINCIPAL COMPONENT ANALYSIS
# ============================================================

# ------------------------------------------------------------
# 35.1 Run PCA using standardised indicators
# ------------------------------------------------------------

pca_model <- prcomp(
  indicator_data,
  center = TRUE,
  scale. = TRUE
)

saveRDS(
  pca_model,
  "outputs/multivariate/pca_model.rds"
)

# Calculate variance explained by each component
pca_eigenvalues <- pca_model$sdev^2

pca_results <- tibble(
  component = paste0(
    "PC",
    seq_along(pca_eigenvalues)
  ),
  eigenvalue = round(pca_eigenvalues, 3),
  variance_percent = round(
    pca_eigenvalues /
      sum(pca_eigenvalues) * 100,
    2
  ),
  cumulative_percent = round(
    cumsum(pca_eigenvalues) /
      sum(pca_eigenvalues) * 100,
    2
  )
)

print(pca_results)

# ------------------------------------------------------------
# 35.2 Create and save the PCA scree plot
# ------------------------------------------------------------

scree_plot <- ggplot(
  pca_results,
  aes(
    x = component,
    y = eigenvalue,
    group = 1
  )
) +
  geom_line(
    color = "#2C7FB8",
    linewidth = 1
  ) +
  geom_point(
    color = "#2C7FB8",
    size = 3
  ) +
  geom_text(
    aes(label = eigenvalue),
    vjust = -0.8,
    size = 3.5
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "#B2182B"
  ) +
  annotate(
    "text",
    x = 4.3,
    y = 1.08,
    label = "Eigenvalue = 1",
    color = "#B2182B",
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 2.3),
    breaks = seq(0, 2.0, 0.5)
  ) +
  labs(
    title = "PCA Scree Plot of Environmental Indicators",
    subtitle = "Components above the dashed line satisfy the Kaiser criterion",
    x = "Principal component",
    y = "Eigenvalue",
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12)

print(scree_plot)

# Save PCA outputs
write_csv(
  pca_results,
  "outputs/multivariate/pca_variance_explained.csv"
)

ggsave(
  filename = "outputs/multivariate/pca_scree_plot.png",
  plot = scree_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 35.3 Calculate and graph PCA loadings
# ------------------------------------------------------------

# Convert PCA coefficients into variable-component correlations
pca_loadings_matrix <- sweep(
  pca_model$rotation[, 1:2, drop = FALSE],
  2,
  pca_model$sdev[1:2],
  FUN = "*"
)

pca_loadings <- pca_loadings_matrix |>
  as.data.frame() |>
  tibble::rownames_to_column("indicator") |>
  tibble::as_tibble()

# Print the loadings table
pca_loadings |>
  mutate(
    across(
      c(PC1, PC2),
      ~ round(.x, 3)
    )
  ) |>
  print(n = Inf)

# Reshape for plotting
pca_loadings_long <- pca_loadings |>
  pivot_longer(
    cols = c(PC1, PC2),
    names_to = "component",
    values_to = "loading"
  ) |>
  mutate(
    indicator = recode(
      indicator,
      drought_depth_m = "Drought depth",
      canopy_percent = "Canopy cover",
      heat_exposure_41plus_pct = "Heat exposure",
      flood_exposure_over50cm_pct = "Flood exposure",
      heavy_rain_exposure_20plus_pct = "Heavy-rain exposure"
    )
  )

# Create loading plot
pca_loading_plot <- ggplot(
  pca_loadings_long,
  aes(
    x = indicator,
    y = loading,
    fill = component
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.4
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      PC1 = "#2C7FB8",
      PC2 = "#F28E2B"
    )
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.25)
  ) +
  labs(
    title = "Indicator Loadings on the First Two Components",
    subtitle = "Loadings represent correlations between indicators and components",
    x = NULL,
    y = "PCA loading",
    fill = "Component",
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12)

print(pca_loading_plot)

# Save table and graph
write_csv(
  pca_loadings,
  "outputs/multivariate/pca_loadings.csv"
)

ggsave(
  filename = "outputs/multivariate/pca_loading_plot.png",
  plot = pca_loading_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 35.4 Calculate and graph municipality PCA scores
# ------------------------------------------------------------

pca_scores <- pca_model$x[, 1:2, drop = FALSE] |>
  as.data.frame() |>
  tibble::as_tibble()

pca_scores_output <- dplyr::bind_cols(
  analysis_ready |>
    dplyr::select(-dplyr::all_of(indicator_names)),
  pca_scores
)

pca_score_plot <- ggplot(
  pca_scores,
  aes(
    x = PC1,
    y = PC2
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  geom_point(
    color = "#2C7FB8",
    alpha = 0.65,
    size = 2
  ) +
  labs(
    title = "Dutch Municipalities in PCA Component Space",
    subtitle = "Each point represents one municipality",
    x = paste0(
      "PC1 (",
      pca_results$variance_percent[1],
      "%)"
    ),
    y = paste0(
      "PC2 (",
      pca_results$variance_percent[2],
      "%)"
    ),
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12)

print(pca_score_plot)

write_csv(
  pca_scores_output,
  "outputs/multivariate/municipality_pca_scores.csv"
)

ggsave(
  filename = "outputs/multivariate/pca_municipality_scores.png",
  plot = pca_score_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ============================================================
# 36. CLUSTER ANALYSIS
# ============================================================

# ------------------------------------------------------------
# 36.1 Standardise indicators and calculate within-cluster variation
# ------------------------------------------------------------

scaled_indicators <- scale(indicator_data)

set.seed(2026)

wss_results <- purrr::map_dfr(
  1:10,
  function(k_value) {

    model <- kmeans(
      scaled_indicators,
      centers = k_value,
      nstart = 50,
      iter.max = 100
    )

    tibble(
      clusters = k_value,
      total_within_ss = model$tot.withinss
    )
  }
)

print(wss_results)

elbow_plot <- ggplot(
  wss_results,
  aes(
    x = clusters,
    y = total_within_ss
  )
) +
  geom_line(
    color = "#2C7FB8",
    linewidth = 1
  ) +
  geom_point(
    color = "#2C7FB8",
    size = 3
  ) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Elbow Method for Selecting the Number of Clusters",
    x = "Number of clusters",
    y = "Total within-cluster sum of squares",
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12)

print(elbow_plot)

write_csv(
  wss_results,
  "outputs/multivariate/cluster_elbow_results.csv"
)

ggsave(
  filename = "outputs/multivariate/cluster_elbow_plot.png",
  plot = elbow_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 36.2 Validate cluster numbers using silhouette width
# ------------------------------------------------------------

distance_matrix <- dist(
  scaled_indicators,
  method = "euclidean"
)

set.seed(2026)

silhouette_results <- purrr::map_dfr(
  2:10,
  function(k_value) {

    model <- kmeans(
      scaled_indicators,
      centers = k_value,
      nstart = 50,
      iter.max = 100
    )

    silhouette_values <- cluster::silhouette(
      model$cluster,
      distance_matrix
    )

    tibble(
      clusters = k_value,
      average_silhouette = mean(
        silhouette_values[, "sil_width"]
      )
    )
  }
)

print(silhouette_results)

silhouette_plot <- ggplot(
  silhouette_results,
  aes(
    x = clusters,
    y = average_silhouette
  )
) +
  geom_line(color = "#2C7FB8", linewidth = 1) +
  geom_point(color = "#2C7FB8", size = 3) +
  scale_x_continuous(breaks = 2:10) +
  labs(
    title = "Silhouette Analysis for Cluster Selection",
    subtitle = "Higher average silhouette width indicates better separation",
    x = "Number of clusters",
    y = "Average silhouette width",
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12)

print(silhouette_plot)

write_csv(
  silhouette_results,
  "outputs/multivariate/cluster_silhouette_results.csv"
)

ggsave(
  filename = "outputs/multivariate/cluster_silhouette_plot.png",
  plot = silhouette_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 36.3 Compare two-cluster and four-cluster solutions
# ------------------------------------------------------------

set.seed(2026)

cluster_model_2 <- kmeans(
  scaled_indicators,
  centers = 2,
  nstart = 100,
  iter.max = 100
)

set.seed(2026)

cluster_model_4 <- kmeans(
  scaled_indicators,
  centers = 4,
  nstart = 100,
  iter.max = 100
)

saveRDS(
  cluster_model_4,
  "outputs/multivariate/four_cluster_model.rds"
)

cluster_comparison <- tibble(
  solution = c("Two clusters", "Four clusters"),
  average_silhouette = c(
    mean(
      cluster::silhouette(
        cluster_model_2$cluster,
        distance_matrix
      )[, "sil_width"]
    ),
    mean(
      cluster::silhouette(
        cluster_model_4$cluster,
        distance_matrix
      )[, "sil_width"]
    )
  ),
  variance_explained_percent = c(
    cluster_model_2$betweenss /
      cluster_model_2$totss * 100,
    cluster_model_4$betweenss /
      cluster_model_4$totss * 100
  )
) |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

print(cluster_comparison)

cluster_sizes <- bind_rows(
  tibble(
    solution = "Two clusters",
    cluster = seq_along(cluster_model_2$size),
    municipalities = cluster_model_2$size
  ),
  tibble(
    solution = "Four clusters",
    cluster = seq_along(cluster_model_4$size),
    municipalities = cluster_model_4$size
  )
)

print(cluster_sizes, n = Inf)

write_csv(
  cluster_comparison,
  "outputs/multivariate/cluster_solution_comparison.csv"
)

write_csv(
  cluster_sizes,
  "outputs/multivariate/cluster_sizes_comparison.csv"
)

# ------------------------------------------------------------
# 36.4 Profile the four-cluster solution
# ------------------------------------------------------------

cluster_profiles <- cluster_model_4$centers |>
  as.data.frame() |>
  tibble::rownames_to_column("cluster") |>
  tibble::as_tibble() |>
  mutate(
    cluster = paste("Cluster", cluster)
  )

print(cluster_profiles)

cluster_profiles_long <- cluster_profiles |>
  pivot_longer(
    cols = -cluster,
    names_to = "indicator",
    values_to = "standardised_mean"
  ) |>
  mutate(
    indicator = recode(
      indicator,
      drought_depth_m = "Drought depth",
      canopy_percent = "Canopy cover",
      heat_exposure_41plus_pct = "Heat exposure",
      flood_exposure_over50cm_pct = "Flood exposure",
      heavy_rain_exposure_20plus_pct = "Heavy-rain exposure"
    )
  )

cluster_profile_plot <- ggplot(
  cluster_profiles_long,
  aes(
    x = indicator,
    y = standardised_mean,
    colour = cluster,
    group = cluster
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Environmental Profiles of Four Municipality Clusters",
    subtitle = "Values are standardised indicator means",
    x = NULL,
    y = "Standardised mean (z-score)",
    colour = "Cluster",
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    )
  )

print(cluster_profile_plot)

write_csv(
  cluster_profiles,
  "outputs/multivariate/four_cluster_profiles.csv"
)

ggsave(
  filename = "outputs/multivariate/four_cluster_profile_plot.png",
  plot = cluster_profile_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 36.5 Display final clusters in PCA space
# ------------------------------------------------------------

clustered_results <- pca_scores_output |>
  mutate(
    cluster = factor(cluster_model_4$cluster)
  )

cluster_centroids_pca <- clustered_results |>
  group_by(cluster) |>
  summarise(
    PC1 = mean(PC1),
    PC2 = mean(PC2),
    municipalities = n(),
    .groups = "drop"
  )

pca_cluster_plot <- ggplot(
  clustered_results,
  aes(
    x = PC1,
    y = PC2,
    colour = cluster
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey70"
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey70"
  ) +
  geom_point(
    alpha = 0.65,
    size = 2
  ) +
  geom_point(
    data = cluster_centroids_pca,
    shape = 4,
    size = 5,
    stroke = 1.5
  ) +
  geom_text(
    data = cluster_centroids_pca,
    aes(label = paste("Cluster", cluster)),
    vjust = -1,
    show.legend = FALSE
  ) +
  scale_colour_brewer(
    palette = "Dark2",
    name = "Cluster"
  ) +
  labs(
    title = "Four Municipality Clusters in PCA Space",
    subtitle = "Crosses indicate cluster centroids",
    x = paste0("PC1 (", pca_results$variance_percent[1], "%)"),
    y = paste0("PC2 (", pca_results$variance_percent[2], "%)"),
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12)

print(pca_cluster_plot)

write_csv(
  clustered_results,
  "outputs/multivariate/municipality_cluster_assignments.csv"
)

write_csv(
  cluster_centroids_pca,
  "outputs/multivariate/cluster_centroids_pca.csv"
)

ggsave(
  filename = "outputs/multivariate/pca_cluster_plot.png",
  plot = pca_cluster_plot,
  width = 9,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 36.6 Summarise clusters in original measurement units
# ------------------------------------------------------------

clustered_indicators <- indicator_data |>
  mutate(
    cluster = factor(cluster_model_4$cluster)
  )

cluster_summary_original <- clustered_indicators |>
  group_by(cluster) |>
  summarise(
    municipalities = n(),
    across(
      all_of(indicator_names),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE)
      )
    ),
    .groups = "drop"
  )

print(
  cluster_summary_original,
  n = Inf,
  width = Inf
)

write_csv(
  cluster_summary_original,
  "outputs/multivariate/cluster_summary_original_units.csv"
)

# ------------------------------------------------------------
# 36.7 Add descriptive cluster names
# ------------------------------------------------------------

cluster_names <- c(
  "1" = "High-canopy, low-heat profile",
  "2" = "Low flood/rain profile",
  "3" = "Flood-exposure profile",
  "4" = "Drought-heavy-rain extreme profile"
)

clustered_results <- clustered_results |>
  mutate(
    cluster_name = recode(
      as.character(cluster),
      !!!cluster_names
    )
  )

write_csv(
  clustered_results,
  "outputs/multivariate/municipality_cluster_assignments_named.csv"
)

print(
  clustered_results |>
    count(cluster, cluster_name)
)

# ============================================================
# 37. MULTIPLE LINEAR REGRESSION
# ============================================================

# ------------------------------------------------------------
# 37.1 Fit the baseline heat-exposure model
# ------------------------------------------------------------

regression_model <- lm(
  heat_exposure_41plus_pct ~
    canopy_percent +
    drought_depth_m +
    flood_exposure_over50cm_pct +
    heavy_rain_exposure_20plus_pct,
  data = analysis_ready
)

print(summary(regression_model))

# Create coefficient table
coefficient_matrix <- coef(
  summary(regression_model)
)

confidence_intervals <- confint(
  regression_model
)

regression_coefficients <- tibble(
  term = rownames(coefficient_matrix),
  estimate = coefficient_matrix[, 1],
  standard_error = coefficient_matrix[, 2],
  t_value = coefficient_matrix[, 3],
  p_value = coefficient_matrix[, 4],
  confidence_low = confidence_intervals[, 1],
  confidence_high = confidence_intervals[, 2]
)

print(
  regression_coefficients |>
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 4)
      )
    ),
  n = Inf
)

# Save results
write_csv(
  regression_coefficients,
  "outputs/multivariate/regression_coefficients.csv"
)

saveRDS(
  regression_model,
  "outputs/multivariate/regression_model.rds"
)

# ------------------------------------------------------------
# 37.2 Extract overall regression model fit
# ------------------------------------------------------------

model_summary <- summary(regression_model)
f_values <- model_summary$fstatistic

regression_model_fit <- tibble(
  observations = nobs(regression_model),
  r_squared = model_summary$r.squared,
  adjusted_r_squared = model_summary$adj.r.squared,
  residual_standard_error = model_summary$sigma,
  f_statistic = unname(f_values[1]),
  numerator_df = unname(f_values[2]),
  denominator_df = unname(f_values[3]),
  model_p_value = pf(
    f_values[1],
    f_values[2],
    f_values[3],
    lower.tail = FALSE
  ),
  aic = AIC(regression_model),
  bic = BIC(regression_model)
)

print(
  regression_model_fit |>
    mutate(
      across(
        where(is.numeric),
        ~ signif(.x, 5)
      )
    )
)

write_csv(
  regression_model_fit,
  "outputs/multivariate/regression_model_fit.csv"
)

# ------------------------------------------------------------
# 37.3 Residuals-versus-fitted diagnostic
# ------------------------------------------------------------

regression_diagnostics <- tibble(
  observation = seq_len(nobs(regression_model)),
  fitted_value = fitted(regression_model),
  residual = residuals(regression_model),
  standardised_residual = rstandard(regression_model),
  leverage = hatvalues(regression_model),
  cooks_distance = cooks.distance(regression_model)
)

residual_fitted_plot <- ggplot(
  regression_diagnostics,
  aes(
    x = fitted_value,
    y = residual
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_point(
    color = "#2C7FB8",
    alpha = 0.65
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    color = "#B2182B"
  ) +
  labs(
    title = "Residuals Versus Fitted Values",
    subtitle = "Multiple regression predicting municipal heat exposure",
    x = "Fitted heat-exposure percentage",
    y = "Residual",
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12)

print(residual_fitted_plot)

write_csv(
  regression_diagnostics,
  "outputs/multivariate/regression_diagnostics.csv"
)

ggsave(
  filename = "outputs/multivariate/regression_residuals_fitted.png",
  plot = residual_fitted_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 37.4 Normal Q-Q plot of regression residuals
# ------------------------------------------------------------

qq_plot <- ggplot(
  regression_diagnostics,
  aes(sample = standardised_residual)
) +
  stat_qq(
    color = "#2C7FB8",
    alpha = 0.65
  ) +
  stat_qq_line(
    color = "#B2182B",
    linewidth = 1
  ) +
  labs(
    title = "Normal Q-Q Plot of Regression Residuals",
    subtitle = "Points should approximately follow the reference line",
    x = "Theoretical quantiles",
    y = "Standardised residuals",
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12)

print(qq_plot)

ggsave(
  filename = "outputs/multivariate/regression_qq_plot.png",
  plot = qq_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 37.5 Formal regression diagnostic tests
# ------------------------------------------------------------

# Shapiro-Wilk normality test
shapiro_result <- shapiro.test(
  residuals(regression_model)
)

# Manual Breusch-Pagan test
bp_data <- analysis_ready |>
  mutate(
    squared_residual =
      residuals(regression_model)^2
  )

bp_auxiliary_model <- lm(
  squared_residual ~
    canopy_percent +
    drought_depth_m +
    flood_exposure_over50cm_pct +
    heavy_rain_exposure_20plus_pct,
  data = bp_data
)

bp_statistic <- nobs(regression_model) *
  summary(bp_auxiliary_model)$r.squared

bp_df <- length(coef(regression_model)) - 1

bp_p_value <- pchisq(
  bp_statistic,
  df = bp_df,
  lower.tail = FALSE
)

diagnostic_tests <- tibble(
  test = c(
    "Shapiro-Wilk normality test",
    "Breusch-Pagan heteroskedasticity test"
  ),
  statistic = c(
    unname(shapiro_result$statistic),
    bp_statistic
  ),
  degrees_of_freedom = c(
    NA,
    bp_df
  ),
  p_value = c(
    shapiro_result$p.value,
    bp_p_value
  )
)

print(diagnostic_tests)

# Calculate variance inflation factors
predictor_matrix <- model.matrix(
  regression_model
)[, -1, drop = FALSE]

vif_values <- purrr::map_dbl(
  seq_len(ncol(predictor_matrix)),
  function(column_number) {

    other_columns <- predictor_matrix[
      ,
      -column_number,
      drop = FALSE
    ]

    auxiliary_model <- lm(
      predictor_matrix[, column_number] ~
        other_columns
    )

    1 / (
      1 - summary(auxiliary_model)$r.squared
    )
  }
)

vif_results <- tibble(
  predictor = colnames(predictor_matrix),
  vif = round(vif_values, 3)
)

print(vif_results)

write_csv(
  diagnostic_tests,
  "outputs/multivariate/regression_diagnostic_tests.csv"
)

write_csv(
  vif_results,
  "outputs/multivariate/regression_vif.csv"
)

# ------------------------------------------------------------
# 37.6 Cook's distance influence diagnostic
# ------------------------------------------------------------

cooks_threshold <- 4 / nobs(regression_model)

influence_summary <- tibble(
  threshold = cooks_threshold,
  maximum_cooks_distance =
    max(regression_diagnostics$cooks_distance),
  observations_above_threshold =
    sum(
      regression_diagnostics$cooks_distance >
        cooks_threshold
    ),
  most_influential_observation =
    which.max(
      regression_diagnostics$cooks_distance
    )
)

print(influence_summary)

cooks_plot <- ggplot(
  regression_diagnostics,
  aes(
    x = observation,
    y = cooks_distance
  )
) +
  geom_col(
    fill = "#2C7FB8",
    width = 0.8
  ) +
  geom_hline(
    yintercept = cooks_threshold,
    linetype = "dashed",
    color = "#B2182B"
  ) +
  labs(
    title = "Cook's Distance for the Heat-Exposure Model",
    subtitle = "Dashed line represents the 4/n screening threshold",
    x = "Observation",
    y = "Cook's distance",
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12)

print(cooks_plot)

write_csv(
  influence_summary,
  "outputs/multivariate/regression_influence_summary.csv"
)

ggsave(
  filename = "outputs/multivariate/regression_cooks_distance.png",
  plot = cooks_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 37.7 Sensitivity analysis excluding influential cases
# ------------------------------------------------------------

influential_rows <- which(
  regression_diagnostics$cooks_distance >
    cooks_threshold
)

# Save the flagged municipalities and their values
influential_cases <- dplyr::bind_cols(
  analysis_ready,
  regression_diagnostics |>
    select(
      observation,
      fitted_value,
      residual,
      standardised_residual,
      leverage,
      cooks_distance
    )
) |>
  filter(
    observation %in% influential_rows
  ) |>
  arrange(desc(cooks_distance))

print(influential_cases, n = Inf)

# Fit sensitivity model without the flagged observations
sensitivity_data <- if (length(influential_rows) > 0) {
  analysis_ready[-influential_rows, , drop = FALSE]
} else {
  analysis_ready
}

sensitivity_model <- update(
  regression_model,
  data = sensitivity_data
)

extract_coefficients <- function(model, model_name) {

  coefficient_matrix <- coef(summary(model))

  tibble(
    model = model_name,
    term = rownames(coefficient_matrix),
    estimate = coefficient_matrix[, 1],
    standard_error = coefficient_matrix[, 2],
    p_value = coefficient_matrix[, 4]
  )
}

sensitivity_comparison <- bind_rows(
  extract_coefficients(
    regression_model,
    "Primary model"
  ),
  extract_coefficients(
    sensitivity_model,
    "Sensitivity model"
  )
)

print(
  sensitivity_comparison |>
    mutate(
      across(
        where(is.numeric),
        ~ round(.x, 4)
      )
    ),
  n = Inf
)

write_csv(
  influential_cases,
  "outputs/multivariate/regression_influential_cases.csv"
)

write_csv(
  sensitivity_comparison,
  "outputs/multivariate/regression_sensitivity_comparison.csv"
)

saveRDS(
  sensitivity_model,
  "outputs/multivariate/regression_sensitivity_model.rds"
)

# ------------------------------------------------------------
# 37.8 HC3 robust standard errors
# ------------------------------------------------------------

model_matrix <- model.matrix(regression_model)
model_residuals <- residuals(regression_model)
model_leverage <- hatvalues(regression_model)

bread_matrix <- solve(
  crossprod(model_matrix)
)

hc3_weights <- (
  model_residuals /
    (1 - model_leverage)
)^2

meat_matrix <- crossprod(
  model_matrix,
  model_matrix * hc3_weights
)

hc3_covariance <- bread_matrix %*%
  meat_matrix %*%
  bread_matrix

hc3_standard_errors <- sqrt(
  diag(hc3_covariance)
)

hc3_estimates <- coef(regression_model)

hc3_t_values <- hc3_estimates /
  hc3_standard_errors

hc3_p_values <- 2 * pt(
  -abs(hc3_t_values),
  df = df.residual(regression_model)
)

hc3_results <- tibble(
  term = names(hc3_estimates),
  estimate = hc3_estimates,
  robust_standard_error = hc3_standard_errors,
  t_value = hc3_t_values,
  p_value = hc3_p_values,
  confidence_low = hc3_estimates -
    qt(0.975, df.residual(regression_model)) *
    hc3_standard_errors,
  confidence_high = hc3_estimates +
    qt(0.975, df.residual(regression_model)) *
    hc3_standard_errors
)

print(hc3_results, n = Inf)

coefficient_plot_data <- hc3_results |>
  filter(term != "(Intercept)") |>
  mutate(
    term = recode(
      term,
      canopy_percent = "Canopy cover",
      drought_depth_m = "Drought depth",
      flood_exposure_over50cm_pct = "Flood exposure",
      heavy_rain_exposure_20plus_pct = "Heavy-rain exposure"
    )
  )

coefficient_plot <- ggplot(
  coefficient_plot_data,
  aes(
    x = estimate,
    y = reorder(term, estimate)
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_errorbar(
    aes(
      xmin = confidence_low,
      xmax = confidence_high
    ),
    orientation = "y",
    width = 0.2,
    color = "#2C7FB8"
  ) +
  geom_point(
    size = 3,
    color = "#2C7FB8"
  ) +
  labs(
    title = "Adjusted Associations With Municipal Heat Exposure",
    subtitle = "Coefficients with HC3 robust 95% confidence intervals",
    x = "Regression coefficient",
    y = NULL,
    caption = analysis_caption
  ) +
  theme_minimal(base_size = 12)

print(coefficient_plot)

write_csv(
  hc3_results,
  "outputs/multivariate/regression_hc3_results.csv"
)

ggsave(
  filename = "outputs/multivariate/regression_coefficient_plot.png",
  plot = coefficient_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# 37.9 Ramsey RESET test for model specification
# ------------------------------------------------------------

reset_data <- analysis_ready |>
  mutate(
    centred_fitted =
      fitted(regression_model) -
      mean(fitted(regression_model)),
    fitted_squared = centred_fitted^2,
    fitted_cubed = centred_fitted^3
  )

reset_model <- lm(
  heat_exposure_41plus_pct ~
    canopy_percent +
    drought_depth_m +
    flood_exposure_over50cm_pct +
    heavy_rain_exposure_20plus_pct +
    fitted_squared +
    fitted_cubed,
  data = reset_data
)

reset_comparison <- anova(
  regression_model,
  reset_model
)

reset_results <- tibble(
  test = "Ramsey RESET test",
  added_terms = 2,
  f_statistic = reset_comparison$F[2],
  numerator_df = reset_comparison$Df[2],
  denominator_df = reset_comparison$Res.Df[2],
  p_value = reset_comparison$`Pr(>F)`[2]
)

print(reset_results)

write_csv(
  reset_results,
  "outputs/multivariate/regression_reset_test.csv"
)

# ============================================================
# 38. SPATIAL REGRESSION AND SENSITIVITY ANALYSIS
# ============================================================

boundary_file <- "data/spatial/cbs_municipalities_2024.gpkg"
boundary_url <- paste0(
  "https://api.pdok.nl/cbs/gebiedsindelingen/ogc/v1/",
  "collections/gemeente_gegeneraliseerd/items",
  "?f=json&datetime=2024-01-01T00:00:00Z/2024-12-31T23:59:59Z&limit=1000"
)

# Reuse the cached boundary file; download it only when it is absent.
if (file.exists(boundary_file)) {
  municipality_boundaries <- sf::st_read(
    boundary_file,
    quiet = TRUE
  )
} else {
  municipality_boundaries <- sf::st_read(
    boundary_url,
    quiet = TRUE
  )

  sf::st_write(
    municipality_boundaries,
    boundary_file,
    delete_dsn = TRUE,
    quiet = TRUE
  )
}

print(dim(municipality_boundaries))
print(names(municipality_boundaries))

# Inspect municipality identifiers for joining
print(
  head(
    sf::st_drop_geometry(municipality_boundaries) |>
      dplyr::select(statcode, statnaam)
  )
)

print(
  head(
    municipality_environmental_analysis |>
      dplyr::select(municipality_code, municipality_name),
    10
  )
)

stopifnot(
  !anyDuplicated(
    municipality_environmental_analysis$municipality_code
  )
)

# Join environmental data to the municipality polygons
municipality_spatial <- municipality_boundaries |>
  dplyr::select(statcode, statnaam) |>
  dplyr::left_join(
    municipality_environmental_analysis,
    by = c("statcode" = "municipality_code")
  )

# Validate the spatial join
spatial_join_check <- tibble::tibble(
  boundary_polygons = nrow(municipality_spatial),
  matched_municipalities = sum(!is.na(municipality_spatial$municipality_name)),
  unmatched_municipalities = sum(is.na(municipality_spatial$municipality_name))
)

print(spatial_join_check)

stopifnot(spatial_join_check$unmatched_municipalities == 0)

# Prepare complete municipalities for spatial diagnostics
spatial_regression_data <- municipality_spatial |>
  dplyr::filter(
    stats::complete.cases(
      canopy_percent,
      drought_depth_m,
      heat_exposure_41plus_pct,
      flood_exposure_over50cm_pct,
      heavy_rain_exposure_20plus_pct
    )
  ) |>
  sf::st_transform(28992) |>
  sf::st_make_valid()

# Refit the same model while retaining municipality geometry
spatial_regression_model <- stats::lm(
  heat_exposure_41plus_pct ~
    canopy_percent +
    drought_depth_m +
    flood_exposure_over50cm_pct +
    heavy_rain_exposure_20plus_pct,
  data = sf::st_drop_geometry(spatial_regression_data)
)

spatial_regression_data$model_residual <-
  stats::residuals(spatial_regression_model)

print(nrow(spatial_regression_data))
print(stats::coef(spatial_regression_model))

# Define neighbouring municipalities using shared boundaries or vertices
queen_neighbours <- spdep::poly2nb(
  spatial_regression_data,
  queen = TRUE,
  row.names = spatial_regression_data$statcode
)

neighbour_counts <- spdep::card(queen_neighbours)

neighbour_summary <- tibble::tibble(
  municipalities = length(queen_neighbours),
  mean_neighbours = mean(neighbour_counts),
  minimum_neighbours = min(neighbour_counts),
  maximum_neighbours = max(neighbour_counts),
  municipalities_without_neighbours = sum(neighbour_counts == 0)
)

print(neighbour_summary)

# Create row-standardised spatial weights
queen_weights <- spdep::nb2listw(
  queen_neighbours,
  style = "W",
  zero.policy = TRUE
)

# Moran's I test for regression residuals
moran_result <- spdep::moran.test(
  spatial_regression_data$model_residual,
  queen_weights,
  alternative = "two.sided",
  zero.policy = TRUE
)

print(moran_result)

# Save Moran's I and neighbour summaries
moran_summary <- tibble::tibble(
  observations = nrow(spatial_regression_data),
  moran_i = unname(moran_result$estimate[1]),
  expected_i = unname(moran_result$estimate[2]),
  variance = unname(moran_result$estimate[3]),
  z_value = unname(moran_result$statistic),
  p_value = moran_result$p.value,
  weights = "Queen contiguity, row-standardised"
)

print(neighbour_summary)
print(moran_summary)

readr::write_csv(
  neighbour_summary,
  "outputs/multivariate/spatial_neighbour_summary.csv"
)

readr::write_csv(
  moran_summary,
  "outputs/multivariate/regression_morans_i.csv"
)

# Common regression formula and non-spatial data
spatial_formula <- heat_exposure_41plus_pct ~
  canopy_percent +
  drought_depth_m +
  flood_exposure_over50cm_pct +
  heavy_rain_exposure_20plus_pct

spatial_model_data <- sf::st_drop_geometry(spatial_regression_data)

# Spatial-lag model
spatial_lag_model <- spatialreg::lagsarlm(
  formula = spatial_formula,
  data = spatial_model_data,
  listw = queen_weights,
  method = "eigen",
  zero.policy = TRUE
)

# Spatial-error model
spatial_error_model <- spatialreg::errorsarlm(
  formula = spatial_formula,
  data = spatial_model_data,
  listw = queen_weights,
  method = "eigen",
  zero.policy = TRUE
)

# Initial model comparison
spatial_model_comparison <- tibble::tibble(
  model = c("OLS", "Spatial lag", "Spatial error"),
  log_likelihood = c(
    as.numeric(logLik(spatial_regression_model)),
    as.numeric(logLik(spatial_lag_model)),
    as.numeric(logLik(spatial_error_model))
  ),
  AIC = c(
    AIC(spatial_regression_model),
    AIC(spatial_lag_model),
    AIC(spatial_error_model)
  ),
  spatial_parameter = c(
    NA,
    spatial_lag_model$rho,
    spatial_error_model$lambda
  )
)

print(spatial_model_comparison)

# Test residual spatial autocorrelation after each spatial model
lag_residual_moran <- spdep::moran.test(
  residuals(spatial_lag_model),
  queen_weights,
  alternative = "two.sided",
  zero.policy = TRUE
)

error_residual_moran <- spdep::moran.test(
  residuals(spatial_error_model),
  queen_weights,
  alternative = "two.sided",
  zero.policy = TRUE
)

spatial_residual_comparison <- tibble::tibble(
  model = c("OLS", "Spatial lag", "Spatial error"),
  residual_moran_i = c(
    unname(moran_result$estimate[1]),
    unname(lag_residual_moran$estimate[1]),
    unname(error_residual_moran$estimate[1])
  ),
  z_value = c(
    unname(moran_result$statistic),
    unname(lag_residual_moran$statistic),
    unname(error_residual_moran$statistic)
  ),
  p_value = c(
    moran_result$p.value,
    lag_residual_moran$p.value,
    error_residual_moran$p.value
  )
)

print(spatial_residual_comparison)

readr::write_csv(
  spatial_model_comparison,
  "outputs/multivariate/spatial_model_comparison.csv"
)

readr::write_csv(
  spatial_residual_comparison,
  "outputs/multivariate/spatial_residual_morans_i.csv"
)

# Display the selected spatial-error model
spatial_error_summary <- summary(spatial_error_model)
print(spatial_error_summary)

# Save models for reproducibility
saveRDS(
  spatial_error_model,
  "outputs/multivariate/spatial_error_model.rds"
)

saveRDS(
  spatial_lag_model,
  "outputs/multivariate/spatial_lag_model.rds"
)

# Extract spatial-error regression coefficients
spatial_error_coefficients <-
  as.data.frame(spatial_error_summary$Coef) |>
  tibble::rownames_to_column("term")

names(spatial_error_coefficients) <- c(
  "term",
  "estimate",
  "standard_error",
  "z_value",
  "p_value"
)

spatial_error_coefficients <- spatial_error_coefficients |>
  dplyr::mutate(
    confidence_low = estimate - 1.96 * standard_error,
    confidence_high = estimate + 1.96 * standard_error
  )

# Add the spatial error parameter
lambda_row <- tibble::tibble(
  term = "Spatial error parameter (lambda)",
  estimate = spatial_error_model$lambda,
  standard_error = spatial_error_summary$lambda.se,
  z_value = estimate / standard_error,
  p_value = 2 * stats::pnorm(abs(z_value), lower.tail = FALSE),
  confidence_low = estimate - 1.96 * standard_error,
  confidence_high = estimate + 1.96 * standard_error
)

spatial_error_results <- dplyr::bind_rows(
  spatial_error_coefficients,
  lambda_row
)

# Convert to tibble for complete printing
spatial_error_results <- tibble::as_tibble(spatial_error_results)

print(spatial_error_results, n = Inf, width = Inf)

readr::write_csv(
  spatial_error_results,
  "outputs/multivariate/spatial_error_coefficients.csv"
)

# Prepare spatial-error coefficients for plotting
spatial_coefficient_plot_data <- spatial_error_results |>
  dplyr::filter(
    !term %in% c(
      "(Intercept)",
      "Spatial error parameter (lambda)"
    )
  ) |>
  dplyr::mutate(
    term_label = dplyr::recode(
      term,
      canopy_percent = "Canopy cover",
      drought_depth_m = "Drought depth",
      flood_exposure_over50cm_pct = "Flood exposure",
      heavy_rain_exposure_20plus_pct = "Heavy-rain exposure"
    )
  )

# Publication-quality coefficient plot
spatial_coefficient_plot <- ggplot2::ggplot(
  spatial_coefficient_plot_data,
  ggplot2::aes(
    x = estimate,
    y = forcats::fct_reorder(term_label, estimate)
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey50"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = confidence_low,
      xmax = confidence_high
    ),
    orientation = "y",
    width = 0.15
  ) +
  ggplot2::geom_point(size = 3, colour = "#0072B2") +
  ggplot2::labs(
    title = "Spatial Associations With Municipal Heat Exposure",
    subtitle = "Spatial-error coefficients with 95% confidence intervals",
    x = "Regression coefficient",
    y = NULL,
    caption = analysis_caption
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(spatial_coefficient_plot)

ggplot2::ggsave(
  filename = "outputs/multivariate/spatial_error_coefficient_plot.png",
  plot = spatial_coefficient_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# Five-nearest-neighbour spatial-weights sensitivity test
municipality_centroids <- sf::st_centroid(
  sf::st_geometry(spatial_regression_data)
)

centroid_coordinates <- sf::st_coordinates(municipality_centroids)

knn5_neighbours <- spdep::knn2nb(
  spdep::knearneigh(centroid_coordinates, k = 5),
  row.names = spatial_regression_data$statcode
)

knn5_weights <- spdep::nb2listw(
  knn5_neighbours,
  style = "W",
  zero.policy = TRUE
)

# Spatial-error model using five nearest neighbours
knn5_error_model <- spatialreg::errorsarlm(
  formula = spatial_formula,
  data = spatial_model_data,
  listw = knn5_weights,
  method = "eigen",
  zero.policy = TRUE
)

# Test its residual spatial autocorrelation
knn5_residual_moran <- spdep::moran.test(
  residuals(knn5_error_model),
  knn5_weights,
  alternative = "two.sided",
  zero.policy = TRUE
)

# Compare alternative spatial-weight definitions
spatial_weights_sensitivity <- tibble::tibble(
  weights = c("Queen contiguity", "Five nearest neighbours"),
  AIC = c(
    AIC(spatial_error_model),
    AIC(knn5_error_model)
  ),
  lambda = c(
    spatial_error_model$lambda,
    knn5_error_model$lambda
  ),
  residual_moran_i = c(
    unname(error_residual_moran$estimate[1]),
    unname(knn5_residual_moran$estimate[1])
  ),
  residual_moran_p = c(
    error_residual_moran$p.value,
    knn5_residual_moran$p.value
  )
)

print(spatial_weights_sensitivity)

readr::write_csv(
  spatial_weights_sensitivity,
  "outputs/multivariate/spatial_weights_sensitivity.csv"
)

saveRDS(
  knn5_error_model,
  "outputs/multivariate/spatial_error_model_knn5.rds"
)

# Extract coefficients from the five-nearest-neighbour model
knn5_summary <- summary(knn5_error_model)

knn5_coefficients <-
  as.data.frame(knn5_summary$Coef) |>
  tibble::rownames_to_column("term") |>
  tibble::as_tibble()

names(knn5_coefficients) <- c(
  "term",
  "estimate",
  "standard_error",
  "z_value",
  "p_value"
)

# Compare coefficient stability
spatial_coefficient_sensitivity <- dplyr::bind_rows(
  spatial_error_coefficients |>
    dplyr::transmute(
      weights = "Queen contiguity",
      term,
      estimate,
      standard_error,
      p_value
    ),
  knn5_coefficients |>
    dplyr::transmute(
      weights = "Five nearest neighbours",
      term,
      estimate,
      standard_error,
      p_value
    )
)

print(tibble::as_tibble(spatial_coefficient_sensitivity), n = Inf, width = Inf)
readr::write_csv(
  spatial_coefficient_sensitivity,
  "outputs/multivariate/spatial_coefficient_sensitivity.csv"
)

# Attach selected-model predictions and residuals to municipalities
spatial_regression_data <- spatial_regression_data |>
  dplyr::mutate(
    spatial_fitted = fitted(spatial_error_model),
    spatial_residual = residuals(spatial_error_model)
  )

# Map spatial-error residuals
spatial_residual_map <- ggplot2::ggplot(spatial_regression_data) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = spatial_residual),
    colour = "white",
    linewidth = 0.1
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#2166AC",
    midpoint = 0,
    mid = "white",
    high = "#B2182B",
    name = "Residual"
  ) +
  ggplot2::labs(
    title = "Spatial Distribution of Heat-Exposure Model Residuals",
    subtitle = "Residuals from the selected spatial-error model",
    caption = analysis_caption
  ) +
  ggplot2::theme_void(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "right"
  )

print(spatial_residual_map)

ggplot2::ggsave(
  filename = "outputs/multivariate/spatial_error_residual_map.png",
  plot = spatial_residual_map,
  width = 9,
  height = 7,
  dpi = 300
)

# Save mapped model results
sf::st_write(
  spatial_regression_data |>
    dplyr::select(
      statcode,
      statnaam,
      municipality_name,
      spatial_fitted,
      spatial_residual
    ),
  "outputs/multivariate/spatial_error_results.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# Prepare cluster assignments for spatial joining
cluster_assignments_for_map <- clustered_results |>
  dplyr::transmute(
    statcode = municipality_code,
    cluster,
    cluster_name
  )

# Join cluster assignments to municipality polygons
municipality_cluster_map <- municipality_boundaries |>
  dplyr::select(statcode, statnaam,) |>
  dplyr::left_join(
    cluster_assignments_for_map,
    by = "statcode"
  )

# Check cluster counts and missing municipalities
print(
  table(
    municipality_cluster_map$cluster_name,
    useNA = "ifany"
  )
)

# Create geographic cluster map
cluster_geographic_plot <- ggplot2::ggplot(municipality_cluster_map) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = cluster_name),
    colour = "white",
    linewidth = 0.1
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "High-canopy, low-heat profile" = "#009E73",
      "Low flood/rain profile" = "#E69F00",
      "Flood-exposure profile" = "#0072B2",
      "Drought-heavy-rain extreme profile" = "#CC79A7"
    ),
    na.value = "grey85",
    name = "Environmental profile"
  ) +
  ggplot2::labs(
    title = "Environmental Profiles of Dutch Municipalities",
    subtitle = "Four-cluster solution based on five standardised indicators",
    caption = paste(
      analysis_caption,
      "grey areas represent incomplete observations",
      sep = "; "
    )
  ) +
  ggplot2::theme_void(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "right"
  )

print(cluster_geographic_plot)

ggplot2::ggsave(
  filename = "outputs/multivariate/municipality_cluster_map.png",
  plot = cluster_geographic_plot,
  width = 11,
  height = 8,
  dpi = 300
)

sf::st_write(
  municipality_cluster_map,
  "outputs/multivariate/municipality_cluster_map.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# ============================================================
# 39. ADDITIONAL PCA AND CLUSTER ROBUSTNESS CHECKS
# ============================================================

# Parallel analysis for PCA component retention
set.seed(20260912)

parallel_repetitions <- 1000
indicator_matrix <- as.matrix(scaled_indicators)

parallel_eigenvalues <- replicate(
  parallel_repetitions,
  {
    permuted_matrix <- apply(
      indicator_matrix,
      2,
      sample,
      replace = FALSE
    )

    eigen(
      cor(permuted_matrix),
      symmetric = TRUE,
      only.values = TRUE
    )$values
  }
)

observed_eigenvalues <- eigen(
  cor(indicator_matrix),
  symmetric = TRUE,
  only.values = TRUE
)$values

parallel_analysis_results <- tibble::tibble(
  component = seq_along(observed_eigenvalues),
  observed_eigenvalue = observed_eigenvalues,
  random_mean = rowMeans(parallel_eigenvalues),
  random_95th_percentile = apply(
    parallel_eigenvalues,
    1,
    stats::quantile,
    probs = 0.95
  ),
  retained = observed_eigenvalue > random_95th_percentile
)

print(parallel_analysis_results)

readr::write_csv(
  parallel_analysis_results,
  "outputs/multivariate/pca_parallel_analysis.csv"
)

# Parallel-analysis graph
parallel_plot_data <- parallel_analysis_results |>
  tidyr::pivot_longer(
    cols = c(observed_eigenvalue, random_95th_percentile),
    names_to = "series",
    values_to = "eigenvalue"
  ) |>
  dplyr::mutate(
    series = dplyr::recode(
      series,
      observed_eigenvalue = "Observed eigenvalue",
      random_95th_percentile = "Random 95th percentile"
    )
  )

parallel_analysis_plot <- ggplot2::ggplot(
  parallel_plot_data,
  ggplot2::aes(
    x = component,
    y = eigenvalue,
    colour = series,
    group = series
  )
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_x_continuous(breaks = 1:5) +
  ggplot2::labs(
    title = "Parallel Analysis for PCA Component Retention",
    subtitle = paste0(
      "Observed eigenvalues compared with ",
      format(parallel_repetitions, big.mark = ","),
      " permuted datasets"
    ),
    x = "Principal component",
    y = "Eigenvalue",
    colour = NULL,
    caption = analysis_caption
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "bottom"
  )

print(parallel_analysis_plot)

ggplot2::ggsave(
  filename = "outputs/multivariate/pca_parallel_analysis.png",
  plot = parallel_analysis_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# Bootstrap stability of the four-cluster solution
cluster_stability <- fpc::clusterboot(
  data = as.matrix(scaled_indicators),
  B = 500,
  bootmethod = "boot",
  clustermethod = fpc::kmeansCBI,
  k = 4,
  runs = 50,
  seed = 20260912,
  multipleboot = FALSE,
  count = TRUE
)

# Save the complete bootstrap object
saveRDS(
  cluster_stability,
  "outputs/multivariate/cluster_bootstrap_stability.rds"
)

# Match clusterboot's arbitrary cluster numbers to the selected
# four-cluster solution by maximising membership overlap.
cluster_count <- length(cluster_model_4$size)

cluster_overlap <- table(
  reference = factor(
    cluster_model_4$cluster,
    levels = seq_len(cluster_count)
  ),
  bootstrap = factor(
    cluster_stability$partition,
    levels = seq_len(cluster_count)
  )
)

cluster_permutations <- as.matrix(
  expand.grid(
    rep(list(seq_len(cluster_count)), cluster_count)
  )
)

cluster_permutations <- cluster_permutations[
  apply(
    cluster_permutations,
    1,
    function(x) length(unique(x)) == cluster_count
  ),
  ,
  drop = FALSE
]

permutation_scores <- apply(
  cluster_permutations,
  1,
  function(permutation) {
    sum(
      cluster_overlap[
        cbind(seq_len(cluster_count), permutation)
      ]
    )
  }
)

matched_bootstrap_clusters <- as.integer(
  cluster_permutations[which.max(permutation_scores), ]
)

# Create a readable stability table
cluster_stability_results <- tibble::tibble(
  cluster_number = seq_len(cluster_count),
  cluster = paste0("Cluster ", cluster_number),
  cluster_name = unname(
    cluster_names[as.character(cluster_number)]
  ),
  bootstrap_cluster = matched_bootstrap_clusters,
  mean_jaccard = as.numeric(
    cluster_stability$bootmean[matched_bootstrap_clusters]
  ),
  dissolved_runs = as.integer(
    cluster_stability$bootbrd[matched_bootstrap_clusters]
  ),
  recovered_runs = as.integer(
    cluster_stability$bootrecover[matched_bootstrap_clusters]
  )
) |>
  dplyr::mutate(
    dissolved_percent = 100 * dissolved_runs / cluster_stability$B,
    recovered_percent = 100 * recovered_runs / cluster_stability$B,
    stability = dplyr::case_when(
      mean_jaccard >= 0.85 ~ "Highly stable",
      mean_jaccard >= 0.75 ~ "Stable",
      mean_jaccard >= 0.60 ~ "Weak pattern",
      TRUE ~ "Unstable"
    )
  )

print(cluster_stability_results)

readr::write_csv(
  cluster_stability_results,
  "outputs/multivariate/cluster_bootstrap_stability.csv"
)

# Plot bootstrap cluster stability
cluster_stability_plot <- ggplot2::ggplot(
  cluster_stability_results,
  ggplot2::aes(
    x = cluster,
    y = mean_jaccard,
    fill = stability
  )
) +
  ggplot2::geom_col(width = 0.65) +
  ggplot2::geom_hline(
    yintercept = 0.75,
    linetype = "dashed",
    colour = "red"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.3f", mean_jaccard)),
    vjust = -0.4,
    size = 4
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1)
  ) +
  ggplot2::labs(
    title = "Bootstrap Stability of the Four-Cluster Solution",
    subtitle = paste0(
      "Mean Jaccard similarity from ",
      cluster_stability$B,
      " bootstrap samples"
    ),
    x = NULL,
    y = "Mean Jaccard similarity",
    fill = "Stability",
    caption = "Dashed line indicates the 0.75 stability threshold"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "bottom"
  )

print(cluster_stability_plot)

ggplot2::ggsave(
  filename = "outputs/multivariate/cluster_bootstrap_stability.png",
  plot = cluster_stability_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# Record software information for reproducibility
writeLines(
  capture.output(sessionInfo()),
  "outputs/multivariate/session_info.txt"
)

# Create an inventory of all saved multivariate outputs
output_files <- list.files(
  "outputs/multivariate",
  recursive = TRUE,
  full.names = TRUE
)

output_inventory <- tibble::tibble(
  file = output_files,
  size_kb = round(file.info(output_files)$size / 1024, 2),
  modified = file.info(output_files)$mtime
)

print(output_inventory)

readr::write_csv(
  output_inventory,
  "outputs/multivariate/output_inventory.csv"
)
