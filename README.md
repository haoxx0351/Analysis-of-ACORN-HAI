---

# README: ACORN-HAI Interim Analysis Guide

## Introduction

ACORN-HAI study is a prospective cohort conducted from September 2022 to December 2024, with data for the interim analysis updated until April 26, 2024. The study aims to create a large-scale, multi-center, patient-centered surveillance network focused on antimicrobial resistance in severe healthcare-associated infections. It also lays the groundwork for future interventional clinical trials targeting multidrug-resistant infections by building microbiology laboratory capacity and developing robust data collection and sharing platforms.

For further details, please refer to the **interim analysis report**.

This README provides a step-by-step guide for conducting the interim analysis of the ACORN-HAI cohort using R. It covers key aspects such as baseline characteristics, antibiotic resistance, clinical outcomes, and antibiotic prescriptions, particularly highlighting **Carbapenem-resistant *Acinetobacter* (CRA)**, **Third-generation cephalosporin-resistant *Enterobacterales* (3GCRE)**, and **Carbapenem-resistant *Enterobacterales* (CRE)**. Throughout the guide, you will find explanations, code examples, and the implications of each component.

---

## Step-by-Step Guide

### Preparation

#### Step 1: Install R and RStudio
Make sure **R** and **RStudio** are installed on your computer:

- Download R from: [https://cran.r-project.org/](https://cran.r-project.org/)
- Download RStudio from: [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)

#### Step 2: Download the raw data
The raw data is managed via **REDCap**. To proceed:

- Download the required raw data files from REDCap.
- For any issues during the download process, please reach out to **Sujie** at jiesu@nus.edu.sg.

**Important:** Do not modify the names of the raw data files.

#### Step 3: Download and extract the code
Go to the `<> Code` section and download the ZIP folder containing the scripts. Once downloaded, unzip the folder to access the necessary code files.

---

### Cleaning raw data

#### Step 1: Place raw data
Store the raw data files in the following directory: `data/raw_data/`. Import the data files (F01, F02, F03, F04, F07a, F07b, F07c, F07d, F07e, F07m) sequentially.

#### Step 2: Run the data cleaning script
Open the `clean_data.Rmd` file in RStudio, and click **Run All** to process and clean the raw data for analysis.

#### Step 3: Output files
After cleaning, the following nine Excel files will be available in the `data/clean_data_excel/` folder:

- `infection_types_index`: Infection types for each patient.
- `baseline_outcomes_index`: Baseline and outcome-related variables.
- `ast_all`: Antimicrobial susceptibility test (AST) results for all episodes.
- `ast_all_index`: AST results for the index episodes.
- `anti_treat_index`: Antibiotic usage for the index episodes.
- `all_vap_bsi`: Relevant variables for all episodes.
- `vap_bsi_index`: Relevant variables for the index episodes.
- `df_ast`; `each_ast`: Prepare data for AST visualizations.

For details on specific variables, refer to the [data directory](https://docs.google.com/spreadsheets/d/1qLqACtCwm7IUfF0Fh_TJnrfE94kV-5Dq_Cn5IjIzS9c/edit?gid=766714505#gid=766714505).

**Note:** The cleaned data files are ready for analysis in SPSS, STATA, R, or other statistical software.

---

### Demographic characteristics and antibiotic resistance profiles

#### Preparing data for visualization
To prepare the data for plotting, run the following scripts:
- `descriptive_analysis/data_for_plot_1.R`
- `descriptive_analysis/data_for_plot_2.R`

#### Baseline characteristics 
Run `descriptive_analysis/table_baseline.R` script to generate a baseline characteristics table.

#### Proportion of infection types
Run the `descriptive_analysis/proportion_infection_types.R` script to generate proportion of infection types across countries with total index episodes.

#### Stacked charts
Run the `descriptive_analysis/stacked_charts_ast.R` script to generate stacked charts showing the proportions of  AST results by antibiotic class for the index episodes.

#### Pie charts 
Run the `descriptive_analysis/pie_charts_ast.R` script to create pie charts displaying the proportions of AST results by antibiotics for the index episodes.

#### Heatmap
Run the `descriptive_analysis/heatmap_ast.R` script to generate a heatmap of resistant organism proportions for the index episodes.

#### Antibiotic resistance profiles
Run the `descriptive_analysis/amr_profiles.R` script to visualize antibiotic resistance profiles across different infection types.

#### Prescriptions 
Run the `descriptive_analysis/sankey.R` script to illustrate the transition from empirical to definitive antibiotic prescriptions.

**Note:** Tables are saved in `output/table/`, and figures in `output/figure/`.

---

### Clinical outcomes

**Copy all `.RData` files from `data/clean_data_RData/` to:**

- `all_cause_mortality/data/clean data/`
- `all_cause_readmission/data/clean data/`
- `attributable_mortality/car_aci/data/clean data/`
- `attributable_mortality/thir_ent/data/clean data/`
- `attributable_mortality/car_ent/data/clean data/`
- `excess_length_of_stay/data/clean data/`

#### All-cause mortality
- Open the `all_cause_mortality` folder.
- Run the R scripts in numerical order.
- Tables and figures will be saved in `all_cause_mortality/table/` and `all_cause_mortality/figure/` directories.

#### All-cause readmission
- Open the `all_cause_readmission` folder.
- Run the R scripts in numerical order.
- Tables and figures will be saved in `all_cause_readmission/table/` and `all_cause_readmission/figure/` directories.

#### Attributable mortality
- Open the `attributable_mortality` folder, which contains subfolders for:
  - **CRA (car_aci)**
  - **3GCRE (thir_ent)**
  - **CRE (car_ent)**

- Run the numbered R scripts in order within each subfolder.
- Tables and figures will be saved in the respective `table/` and `figure/` directories.

#### Excess length of stay
- Open the `excess_length_of_stay` folder.
- Run the R scripts in numerical order.
- Tables and figures will be saved in `excess_length_of_stay/table/` and `excess_length_of_stay/figure/` directories.

---

### Troubleshooting
For any issues with code execution, please contact Xinxin at xx_hao@nus.edu.sg.

--- 
