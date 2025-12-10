These R scripts can be used to recreate the graphs used in CSC8008's final project.  The tableau ones use the dataset that are created by CombineAllTheDataFiles.R

Steps:

Download the data from:
https://vaers.hhs.gov/data/datasets.html
The complete dataset with all years:
https://vaers.hhs.gov/eSubDownload/index.jsp?fn=AllVAERSDataCSVS.zip

This will uncompress all the files. Put them in a subdirectory AllData where the R files are installed.

Then run CombineAllTheDataFiles.R, 

Set firstYear to the beginning year to be used
Set lastYear to the last year to be used


once completed,

Set firstYear and lastYear in FinalProjectChartScripts.R to match what was used in CombineAllTheDataFiles.R

