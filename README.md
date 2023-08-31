# viyajobs
In SAS Studio Flows clone this repository to a location on Explorer>Server files/home/user/viyajobs
Download the fuelefficiency.spk package to a local folder
Import /packages/fuelefficiency.spk in the SAS Environment Manager to a folder in SAS Content 
for example:/Users/myuserid/My Folder/ 
In SAS Studio Flows, open the folder /flowtoextractjobs folder and select the Extract job definitions.step and add this to a flow
Fill in the information about the job of which you wish to extract sas code and json information: 
sascontent:/Users/myuserid/My Folder/VJOB/Fuel Efficiency by Type and Origin
Fill in the location on the git repository where you wish to store the extracted information:
sasserver:/home/user/viyajobs/jobdefinitions
