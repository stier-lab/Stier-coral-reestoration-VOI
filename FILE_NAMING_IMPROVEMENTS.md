# File Naming and Organization Improvements

## Current File Structure Analysis

### Code Files (Current)
```
code/
├── 0_libraries.R                      # Good: Clear purpose
├── 1_coral_operating_model.R           # Good: Descriptive
├── 2_coral_parameters.R                # Good: Clear
├── 2b_coral_dangerzone.R               # OK: Could be more descriptive
├── 3_coral_mse_model.R                 # OK: But "IMPROVED" version is better
├── 3_coral_mse_model_IMPROVED.R        # Better version - should be primary
├── 4_coral_model_tests.R               # Good: Clear purpose
└── 5_compare_old_vs_improved_models.R  # Good: Descriptive

test_model.R                            # OK: Could be in code/ directory
```

## Recommended Changes

### 1. Rename Files for Clarity

**KEEP AS-IS (Good names):**
- `0_libraries.R` - Clear setup file
- `1_coral_operating_model.R` - Descriptive
- `2_coral_parameters.R` - Clear purpose

**IMPROVE:**
- `2b_coral_dangerzone.R` → `2b_risk_zone_functions.R`
  - More descriptive of what it does (calculates risk zones)

- `3_coral_mse_model.R` → `3a_mse_model_ORIGINAL.R`
  - Mark as original/deprecated version
  - Keep for comparison purposes only

- `3_coral_mse_model_IMPROVED.R` → `3_mse_model.R`
  - This is the BETTER version, should be the default
  - Remove "IMPROVED" since it should be the standard

- `test_model.R` → `quick_test.R` or move to `code/0_quick_test.R`
  - Better describes purpose
  - Optionally move into code/ directory for consistency

**ADD NEW FILES:**
- `6_generate_figures.R` - Dedicated figure generation script
- `7_run_simulations.R` - Run parameter sweep simulations
- `RUN_ALL_ANALYSES.sh` - Shell script to run complete analysis pipeline

### 2. Output File Naming Convention

All outputs should follow this pattern:
```
[type]_[description]_[details].[ext]

Examples:
- fig1_voi_by_monitoring_precision.pdf
- fig2_restoration_trajectories_by_starting_cover.pdf
- fig3_npv_heatmap_allee_vs_cv.pdf
- table1_model_parameters.csv
- table2_voi_summary_statistics.csv
- sim_results_monte_carlo_n1000.rds
```

### 3. Create Output Subdirectories

```
output/
├── figures/
│   ├── main/           # Publication-ready main figures
│   ├── supplement/     # Supplementary figures
│   └── diagnostic/     # Model diagnostic plots
├── tables/
│   ├── main/           # Main results tables
│   └── supplement/     # Supplementary tables
├── simulation/
│   ├── raw/            # Raw simulation .rds files
│   └── processed/      # Processed summary .csv files
└── README.md           # Describes all outputs
```

### 4. Script Organization Recommendations

**Current numbering is good!** Keep the 0-5 prefix system:
- `0_` = Setup/configuration
- `1_` = Core model components
- `2_` = Parameters and helper functions
- `3_` = Main MSE framework
- `4_` = Testing and validation
- `5_` = Comparison and analysis
- `6_` = Figure generation (NEW)
- `7_` = Simulations (NEW)

### 5. Add Script Headers

Every script should start with:
```r
# ==============================================================================
# SCRIPT NAME AND PURPOSE
# ==============================================================================
# Brief description of what this script does
#
# INPUTS:
#   - List input files or dependencies
#
# OUTPUTS:
#   - List specific output files generated
#
# AUTHOR: [Name]
# DATE: [Date]
# ==============================================================================
```

### 6. Figure Specifications

All figures should:
- Have clear, publication-ready titles
- Include axis labels with units
- Use consistent color schemes (defined in 0_libraries.R)
- Be saved in multiple formats (PDF for publication, PNG for viewing)
- Include figure captions in output/README.md

### 7. Table Specifications

All tables should:
- Have clear column names (no abbreviations unless defined)
- Include units in column headers where appropriate
- Be saved as CSV for compatibility
- Have corresponding descriptions in output/README.md

## Implementation Priority

### HIGH PRIORITY (Do immediately):
1. ✅ Rename `3_coral_mse_model_IMPROVED.R` → `3_mse_model.R`
2. ✅ Rename `3_coral_mse_model.R` → `3a_mse_model_ORIGINAL.R`
3. ✅ Rename `2b_coral_dangerzone.R` → `2b_risk_zone_functions.R`
4. ✅ Create output subdirectories (main/, supplement/, diagnostic/)
5. ✅ Create output/README.md describing all outputs
6. ✅ Update all source() calls in scripts after renaming

### MEDIUM PRIORITY (Do next):
1. Add comprehensive headers to all scripts
2. Create `6_generate_figures.R` with clear output naming
3. Create `7_run_simulations.R` for parameter sweeps
4. Add figure/table generation to existing scripts

### LOW PRIORITY (Nice to have):
1. Create RUN_ALL_ANALYSES.sh master script
2. Add progress bars to long-running simulations
3. Create automated testing script

## Expected Benefits

After implementation:
- **Clarity**: Anyone can understand what each file does
- **Reproducibility**: Clear workflow from data → results → figures
- **Publication-ready**: Outputs have descriptive names for manuscripts
- **Maintainability**: Easy to find and update specific components
- **Professionalism**: Well-organized research compendium

## Notes

- Keep old filenames in git history for reference
- Update all documentation (README.md, etc.) with new names
- Test all scripts after renaming to ensure no broken dependencies
