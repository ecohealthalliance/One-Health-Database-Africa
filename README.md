# gheri-africom-data

The “Strategic Coordination to Strengthen AFRICOM One Health and Veterinary Programs for Global Health Engagement” is 
reviewing current capacity and programmatic status, gaps, and operations in each country and by sub-regions of Africa. 
The term “AFRICOM” refers to the 53 countries (exlcuding Egypt) that make up the continent. 

This repository combines datasets from multiple sources to produce a single One Health database containing all the selected indicators. 
 
# Data
Downloaded data files are stored in the data folder. 
The sources of each data file and their associated indicators are listed in the 'gheri_africom_indicators_metadata.csv' file. This file also contains links to the relevant webpages. 

The column entitled 'manual update needed' idenitifes those datasets for which an api call/direct download wasn't available and that will need manually updating as required. 

**Definitions for columns in metadata file**

Indicator - measure used to assess one health capacity
filename - name of file storing the data
source_url - website/api endpoint where data can be 
Notes - Additional context about a source
function_names - function used to ingest and process data
api_call_or_direct_download - **Need additional context**
Manual update needed - Is the data updated by hand, not via an automated data ingest
# Functions
Individual functions for extracting the data for individual indicators are available in the 'R_api' folders

'R_api' contains functions to retrieve data using direct downloads or api calls where available. For those where a direct download/api call is not possible, 
the functions use the data stored in the 'data' folder. 


# Compiling full dataset

'top_script_api' uses direct downloads or api calls where they are available. When not available it draws on the downloaded data in the 'data' folder. 

There are three outputs from each of these scripts:
* full data factor api - contains the data where outcome values are factors
* full data number api - contains the data where outcome values are numeric
* full data combined api - combined the numeric and factor datasets. Values are stored as characters and there is an additional column identifying whether the value is numeric or factor

Note - For RVF related indicators (RVFany, RVFHumanReport, RVFAnimalReport, RVFAnimalandHuman) the values are numeric (0 or 1) where 0 represents no reports and 1 represents some reports, but does not quantify how many.  

#Country reports

'country.report.Rmd' produces a report for a specified year and country which the user specifies in lines 21 and 23 of the script. 
Only variables for which data are available for the specified country and years will appear in the report. 
