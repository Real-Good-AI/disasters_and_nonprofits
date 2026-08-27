# Natural Hazard/Disaster Damage and the Nonprofit Sector
**NOTE**: This project uses the harmonized/core/ (pre-tier layout) of the NCCS Core Files, which is now deprecated. 
This is because the files were downloaded in 2025 prior to the new tier system. 

## Data Cleaning and Pre-processing (General)
This section goes through the step-by-step workflow for reproducing the data cleaning and processing for this project using the scripts in this directory.
You will need to download the data. 
First, ensure that the working directory is set to `rgai_summer2025_nccs/disasters_project`.
Then, run the file `rgai_summer2025_nccs/disasters_project/download_data.R`.
After running the file, you should see the following in the `data/` directory:

* BMF_UNIFIED_V1.1.csv
* pc/
* pf/
* pz/

`pc/`, `pf/`, and `pz/` are subdirectories. The `pc/` directory should be empty. The other two should be populated with CORE files.

### Unified Business Master File (BMF)
Next, you will need to clean and process the raw Unified BMF file (`BMF_UNIFIED_V1.1.csv`) by running the file `cleanBMF.R`. 
This should produce the cleaned BMF file in the `data` folder with the file name `cleanBMF.rds`. 
This must be done **before running** `cleanPZ.R`, `cleanPF.R`, or `create_mega_df.R`.

### The CORE Files
At the time we began this project, the raw CORE files were indexed by year, tax-exampt class, and tax form scope.
For tax-exempt class, there were three divisions: private foundations (PF), non-PF 501c3 charities, and non-PF charities of all other 501c types. We took the data from all divisions.
For tax-form scope, PFs have their own tax form to fill out. The remaining non-PF charities fill out either a full 990 form or a 990-EZ form depending on a number of factors. For these, we used the PZ scope, which includes both tax forms.
We need to clean, process, and merge all the files together to get a "mega" dataset that includes the tax info from ALL charities, across ALL years 1991-2021 across ALL form types.

The files `cleanPZ.R` and `cleanPF.R` both take in raw CORE files, do some cleaning and pre-processing (including merging with the cleaned BMF file), and return the files that are used to create the final "mega" dataframe.
The outputs of `cleanPZ.R` and `cleanPF.R` are fed to `create_mega_df.R`, which does the final merge. **NOTE**: By default, in `cleanPZ.R`, `cleanPF.R`, and `create_mega_df.R` the lines where the files are saved are commented out. 
You will need to uncomment them before running the files. Within each file, look for the following text: "uncomment this line of code to save the file"

Running `cleanPF.R` will produce a file `pf_processed.rds` in the `data` directory. 
The `cleanPZ.R` file is used to produce TWO different files, but to do so you must run the file two separate times, changing certain variable names between the runs.
This is because the non-PF charities are split into two groups: the 501c3 charities and all other 501c types (denoted 501ce in the files/code). 
The processing is almost exactly the same for both, but some variable names need to be changed. In the file, search for the following phrase "SPECIFIC TO PZ TYPE" to know which lines to change.
In total, after two separate runs, the following files should be in your `data` directory:

* (501c3 run) `pz_merged.rds`, `pz_merged_bmf.rds`, `pz_processed.rds`
* (501ce run) `pz_ce_merged.rds`, `pz_ce_merged_bmf.rds`, `pz_ce_processed.rds`
* Note that `pz_processed.rds` and `pz_ce_processed.rds` are really the files that we need for the final product; the other files were produced as intermediate steps that were helpful in debugging stages. 
I keep them in just in case they are useful to a future researcher who is trying to reproduce these results.

Finally, to create the "mega" merged dataset, run the file `create_mega_df.R`. This file takes in `pf_processed.rds`, `pz_processed.rds`, and `pz_ce_processed.rds` and creates a file `mega.rds` in the `data` directory.

### Disaster Data
The raw disaster data is downloaded from SHELDUS (https://sheldus.org). You need a subscription (sometimes provided by your institution). 
Assuming you have a subscription, you would need to query the data following the format in the file `SHELDUS_query.png`. 
Once you save it in the `data` directory, you would need to change line 8 in `disaster_data.R` to match your saved file name. Then, running the script should produce `disasters.rds` in the `data` directory, which we use to build the county-year panel.

HOWEVER, in case you do not have access to the raw SHELDUS dataset, the directory `data/disasters` contains the `disasters.rds` that we used, which is the *aggregated* version of the data. 
Unfortunately I am not allowed to share the raw file, but can share the final version. 
Simply move the file to the main `data/` directory so that it works with the other files.

CITATION: CEMHS, 2026. Spatial Hazard Events and Losses Database for the United States, Version 24.0. [Online Database]. Phoenix, AZ: Center for Emergency Management and Homeland Security, Arizona State University. Available at https://sheldus.org.

### County-Year Panel Dataset
Finally, to build the county-year panel that aggregates nonprofit data at the county level, you will need to run `data_prep_county_long.R`.
You will need to uncomment the last line of code to save the result. This should produce the file `county_long.rds` in the `data/` directory.

## Two Distinct Approaches
The directories `causal_approach/` and `gp_approach/` each contain data processing and analysis files specific to the corresponding approach. While most of the data cleaning and processing was identical, each approach required certain small adjustments for different reasons. 
Thus, there are some minor additional data cleaning or processing steps within each folder. 

For `gp_approach/`, begin with the file `county_level_analysis.Rmd`. 
After finishing the county-level analysis, you may continue with `org_level_analysis.Rmd`.
Files produced in these scripts should be saved to a directory called `gp_data/`.
