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

#### Step 3: Output Files
After cleaning, the following nine Excel files will be available in the `data/clean_data_excel/` folder:

- `infection_types_index`: Contains infection types for each patient.
- `baseline_outcomes_index`: Baseline characteristics and outcomes.
- `ast_all`: AST results for all episodes.
- `ast_all_index`: AST results for the index episode.
- `anti_treat_index`: Antibiotic usage during the index episode.
- `all_vap_bsi`: Key variables for all episodes.
- `vap_bsi_index`: Key variables for the index episode.
- `df_ast` and `each_ast`: Prepared data for AST visualizations.

For details on specific variables, refer to the [data directory](https://docs.google.com/spreadsheets/d/1jz6uLEr5W_TLF88tHTcc-DNK-3iql8Ji5GziOl9Hflg/edit?gid=510416900#gid=510416900).

**Note:** The cleaned data files are ready for use in SPSS, STATA, R, or other statistical software.

---

### Baseline Characteristics 
To analyze baseline characteristics, run the `table_baseline.R` script.  
The output table will be saved in the `output/table/` folder.

---

### Data Visualization

#### Preparing Data
To prepare the data for plotting, run the following scripts:
- `data_for_plot_1.R`
- `data_for_plot_2.R`

#### Stacked Charts
Run the `stacked_charts_ast.R` script to generate stacked charts showing the proportions of AST results by antibiotic class for the index episode.

#### Pie Charts 
Run the `pie_charts_ast.R` script to create pie charts displaying the proportions of AST results by antibiotics for the index episode.

#### Heatmap
Run the `heatmap_ast.R` script to generate a heatmap of resistant organism proportions for the index episode.

#### Antibiotic Resistance Profiles
Run the `amr_profiles.R` script to visualize antibiotic resistance profiles across different infection types.

#### Sankey Diagram
Run the `sankey.R` script to illustrate the transition from empirical to definitive antibiotic prescriptions.

**Note:** All figures will be saved in the `output/figure/` folder.

---

### Troubleshooting
For any issues with code execution, please contact Xinxin at xx_hao@nus.edu.sg.

--- 
