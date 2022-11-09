# Investigating the epidemiology of MIS-C
Here we provide the steps for running Phase2.2 MISC R package. This package specifically requres [Phase2.2 data](https://github.com/covidclinical/PhaseX.2SqlDataExtraction/blob/main/Extras/4CE_PhaseX.2_CustomCohorts_MISCPatients_mssql.sql). 

## Install and run

### 1. Install the R pacakge
In R, install and load the latest version of
**Phase2.2MISCRPackage** from GitHub with remotes and run:

``` r
remotes::install_github('covidclinical/Phase2.2MISCRPackage',
                        subdir = 'FourCePhase2.2MISC',
                        upgrade = FALSE)

library(FourCePhase2.2MISC)
```

### 2. Run the analysis.
To run the analysis you will have to run the function 'FourCePhase2.2MISC::runAnalysis'. 
Before running it update the arguments accordingly to your site. 
- dir.input will be the path to the folder where you have the 2.2 MISC files. Note that the name of files should be the same than the regular 2.2 files
- dir.output will be the folder where the results will be saved. 
- obfuscation: if you do not need to apply obfuscation to your site, leave it as FALSE, if not change it to the numeric value (e.g., 3)
- raceAvailable: set it to FALSE if you do not have these information available.
- country: set it to your country (e.g., "us", "france", "spain", "uk")
- data_updated_date: change it to the date when your data was generated.
- dateFormat: this is set to the dateFormat used in 4CE, change it if you have your dates in another format. 
- verbose: leave it as TRUE to get the log file

``` r
FourCePhase2.2MISC::runAnalysis( dir.input = "/path_input_2.2MISCdata/",
                                 dir.output = "/path_to_folder_to_save_output/",
                                 obfuscation = FALSE,
                                 raceAvailable= TRUE,
                                 country = "yourCountry",
                                 data_update_date = "2022-06-01",
                                 dateFormat = "%Y-%m-%d",
                                 verbose = TRUE)
```
Note that if you run the function multiple times you will get a warning saying that the output directory already exists. You can ignore the warning since the output files generated are overwritten each time. 

#### Outputs
What outputs are generated?
- figures folder: contains visual summaries of the cohort counts
  - Age distribution
  - Hospitalization length distribution
  - Number of patients in ICU per variant
  - Patient counts per month
  
- QC folder: 
  - SITEID_ICDdiagnosisCodes.RData: contains the ICD codes available in the patient population
  - MISC_logs_QC.txt: contains the runtime logs and warnings from running the package

- SITEID_table1.txt: human readable Table 1 (demographics & comorbidities)
- SITEID_table2.txt: human readable Table 2 (lab values)
- SITEID_table3.txt: human readable Table 3 (diagnoses)
- SITEID_table1Categorical.RData: raw, obfuscated, aggregate counts for Table 1 categorical variables (demographics & comorbidities)
- SITEID_table1Continuous.RData: raw, obfuscated, aggregate counts for Table 1 continuous variables (demographics & comorbidities)
- SITEID_table2AtAdmission.RData: raw, obfuscated, aggregate counts for Table 2 laboratory values at admission
- SITEID_table2DuringAdmission.RData: raw, obfuscated, aggregate counts for Table 2 laboratory values during admission
- SITEID_table3.RData: raw, obfuscated, aggregate counts for Table 3 (diagnoses)

### 3. Submit
Please share the generated output folder with us (@Simran Makwana, @Alba Gutierrez ) via the \#pediatrics Slack channel.
If you run into any problem adapting this code to your data, let us know via Slack.

Thank you very much for your contribution!

