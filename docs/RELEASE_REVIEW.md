# Release review — 3 October 2026

Input snapshot: GitHub commit 489585d60508e122ad37f78050b2977ac68dae4d. ZIP entries are dated 26 September 2026.

## Verified by inspecting the supplied archive
- The two copies of 04_full_environmental_analysis.R have identical bytes. One canonical copy is retained under scripts/.
- The archive includes older exploration scripts, baseline outputs, municipal datasets, spatial files and a complete project report.
- The GitHub snapshot lacks the sensitivity scripts. They have now been added from the separate user uploads.
- No source workbook is supplied, so a full reproduction cannot be verified here.

## Changes in this draft
- Baseline analysis code retained unchanged.
- Duplicate root script and legacy/original backup scripts omitted.
- Older report, tables, figures, spatial files and model objects omitted from the draft; original ZIP remains the private archive.
- README states current submission status and run order for all three scripts.
- Script 05 is unchanged apart from its filename. Script 06 has corrected explanatory comments only; computations and historical output names are retained.
- R is unavailable in the assistant environment. The user has now executed all three scripts; selected supplied results have been checked, as documented in VERIFICATION.md.
- No DOI, acceptance, reproducibility success or data redistribution permission is invented.

## Required before final release
1. Sensitivity scripts added. Both read the derived municipal table; script 06 additionally reads the source workbook.
2. User run and R/package version record received; see VERIFICATION.md for evidence and limitations.
3. Compare generated manuscript results and resolve discrepancies rather than silently changing published claims.
4. Confirm provider terms and boundary licences before adding any data or maps.
5. Add actual public preprint DOI when assigned. Add repository citation metadata once final version is chosen.
6. Review full author name and contact details, then publish the clean release instead of exposing an unchecked private archive.
