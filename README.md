---

# README: ACORN-HAI Interim Analysis Guide

## Introduction

The ACORN-HAI study is a prospective cohort initiated in September 2022 and scheduled to conclude in December 2024, with data for this interim analysis updated as of April 26, 2024. The study aims to establish a large-scale, multi-center patient-centered surveillance network focused on antimicrobial resistance in severe healthcare-associated infections. Additionally, it serves as a foundation for future interventional clinical trials targeting multidrug-resistant infections. This includes building the capacity of microbiology laboratories and developing robust data collection and sharing platforms.

For further details, please refer to the interim analysis report.

In this README, we guide you through the interim analysis of the ACORN-HAI cohort using R. The guide covers baseline characteristics, microbiology data, and antibiotic usage, alongside explanations, code examples, and the implications of each component.

---

## Step-by-Step Guide

### Preparation

#### Step 1: Install R and RStudio
Ensure that **R** and **RStudio** are installed on your computer:

- Download R from: [https://cran.r-project.org/](https://cran.r-project.org/)
- Download RStudio from: [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

#### Step 2: Download raw data
The raw data for this analysis is managed using **REDCap**. Download the required data files from REDCap. If you encounter any issues, please contact **Sujie** at jiesu@nus.edu.sg.

**Important:** Do not modify the names of the raw data files.

---

### Cleaning raw data

#### Step 1: Place raw data
Store the raw data files in the following directory: `data/raw_data/`. Import the data files (F01, F02, F03, F04, F07a, F07b, F07c, F07d, F07e, F07m) sequentially.

#### Step 2: Run the data cleaning script
Open the `clean_data.Rmd` file in RStudio, and click **Run All** to process and clean the raw data for analysis.

#### Step 3: Output files
After cleaning, the following nine Excel files will be available in the `data/clean_data_excel/` folder:

- `infection_types_index`: Contains infection types for each patient.
- `baseline_outcomes_index`: Baseline characteristics and outcomes.
- `ast_all`: AST results for all episodes.
- `ast_all_index`: AST results for the index episode.
- `anti_treat_index`: Antibiotic usage during the index episode.
- `all_vap_bsi`: Key variables for all episodes.
- `vap_bsi_index`: Key variables for the index episode.
- `df_ast` and `each_ast`: Prepared data for AST visualizations.

For details on specific variables, refer to the [data directory](https://docs.google.com/spreadsheets/d/1vjWEZeZuSjVBDFOE1_vwWC4YEVcdSkaDV3bWvFfXKLc/edit?gid=891995523#gid=891995523).

**Note:** The cleaned data files are ready for use in SPSS, STATA, R, or other statistical software.

---

### Demographic characteristics and antibiotic resistance profiles

#### Preparing data for visualization
To prepare the data for plotting, run the following scripts:
- `descriptive_analysis/data_for_plot_1.R`
- `descriptive_analysis/data_for_plot_2.R`

#### Baseline characteristics 
To analyze baseline characteristics, run the `descriptive_analysis/table_baseline.R` script.  
The output table will be saved in the `output/table/` folder.

#### Proportion of infection types
Run the `descriptive_analysis/proportion_infection_types.R` script to generate proportion of infection types across countries with total index episodes.

#### Stacked charts
Run the `descriptive_analysis/stacked_charts_ast.R` script to generate stacked charts showing the proportions of AST results by antibiotic class for the index episode.

#### Pie charts 
Run the `descriptive_analysis/pie_charts_ast.R` script to create pie charts displaying the proportions of AST results by antibiotics for the index episode.

#### Heatmap
Run the `descriptive_analysis/heatmap_ast.R` script to generate a heatmap of resistant organism proportions for the index episode.

#### Antibiotic resistance profiles
Run the `descriptive_analysis/amr_profiles.R` script to visualize antibiotic resistance profiles across different infection types.

#### Sankey 
Run the `descriptive_analysis/sankey.R` script to illustrate the transition from empirical to definitive antibiotic prescriptions.

**Note:** All figures will be saved in the `output/figure/` folder.

---

### Clinical Outcomes

#### All-Cause Mortality
Open the `all_cause_mortality` folder, run all R scripts, and tables will be generated in the `all_cause_mortality/table/` directory, with figures in the `all_cause_mortality/figure/` directory.

#### All-Cause Readmission
Open the `all_cause_readmission` folder, run all R scripts, and tables will be generated in the `all_cause_readmission/table/` directory, with figures in the `all_cause_readmission/figure/` directory.

#### Attributable Mortality
Open the `attributable_mortality` folder, which includes subfolders for **carbapenem-resistant *Acinetobacter* (car_aci)**, **third-generation cephalosporin-resistant *Enterobacterales* (thir_ent)**, and **carbapenem-resistant *Enterobacterales* (car_ent)**. Run all R scripts within each subfolder, and tables will be generated in the respective `table/` directories, with figures in the respective `figure/` directories.

#### Excess Length of Stay
Open the `excess_length_of_stay` folder, run all R scripts, and tables will be generated in the `excess_length_of_stay/table/` directory, with figures in the `excess_length_of_stay/figure/` directory.

---

### Troubleshooting
For any issues with code execution, please contact Xinxin at xx_hao@nus.edu.sg.

--- 
