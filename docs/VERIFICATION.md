# Verification record — 3 October 2026

Joshua Aguele ran scripts 04, 05 and 06 from the extracted project in RStudio on Windows, starting with an empty environment and the study workbook in data_raw. Console screenshots show each script returning to the prompt. The assistant inspected the supplied CSV outputs and session record; the assistant did not independently execute R or inspect the source workbook in this environment.

## Checks completed

- Script 04 screenshot: bootstrap Jaccard means 0.816, 0.936, 0.978, 0.879 match the submitted manuscript comparison targets.
- Drought CSV: log-transformed ARI 0.931921250556789 (337 municipalities); highest drought excluded ARI 0.997402332792262 (336). Both match reported values after rounding. Screenshot identifies excluded municipality as Landgraaf.
- Flood CSV: thresholds 0, 80, 90, 100 have sample sizes 314, 184, 166, 133 and ARIs 0.912469142058071, 0.944167902872065, 1, 1. Screenshot shows maximum baseline flood difference 0.
- Queen neighbour graph: 337 municipalities, components of 334 and 3, no zero-neighbour municipalities. The smaller group is Hulst (GM0677), Terneuzen (GM0715), Sluis (GM1714), identified in the user's output. This is consistent with their geographical separation in Zeeuws-Vlaanderen; the warning is retained and documented, not suppressed by altering adjacency.
- Spatial-error sensitivity: queen/five-nearest-neighbour canopy coefficients -0.817606/-0.848797; drought +0.159756/+0.129467. Canopy and drought remain significant at 5%; flood and heavy rain remain non-significant. Drought magnitude changes by about 19%, so do not claim identical estimates.
- Residual Moran p-values 0.089500 and 0.071486: neither rejects at 5%, which does not prove absence of residual dependence. Consult the script for the test alternative.
- Queen/five-nearest-neighbour AICs 1881.873968/1888.347901; lambda 0.615733/0.631591.

## Recorded environment

R 4.6.1, Windows 11 x64; tidyverse 2.0.0, readxl 1.5.0, janitor 2.2.1, sf 1.1-2, spdep 1.4-2, spatialreg 1.4-3, fpc 2.2-15. Full supplied session record is in verification_2026-10-03/. This records versions, not a dependency lockfile.

## Limits and release scope

These are selected checks from a user-executed run, not independent reproduction or verification of every manuscript table, figure, coefficient or claim. The municipal workbook and boundaries are not redistributed. Historical flood output names containing missing or coverage refer to the conditional denominator sensitivity, not proof of missing source observations. Source blanks in the four flood fields represent zero inundation under the provider clarification; the baseline retains zeros.

Before claiming complete manuscript reproduction, compare all primary manuscript outputs. Before public deposition, review author/citation metadata and data licensing. Do not claim a public preprint DOI or acceptance before confirmed.
