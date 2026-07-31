#### Title: software release for Water Quality Improvements and Hydrogeomorphic Change Over 20 years Contributed to Macrophyte Community Recovery in the Upper Mississippi River 
#### Data and scripts for the manuscript,"Water Quality Improvements and Hydrogeomorphic Change Over 20 years Contributed to Macrophyte Community Recovery in the Upper Mississippi River"
#### Authors: Danelle M. Larson, Mirkka Jones, Benjamin Weigel, Brian Gray, and Otso Ovaskainen
#### Points of contact: Danelle M. Larson (dmlarson@usgs.gov) 
#### Repository Type:  _R_ script, datasets
#### Year of Origin:   2026 (original publication)
#### Year of Version:  2026


***

_Suggested Citation:_

Larson DM, Jones M, Weigel B, Gray B, and Ovaskainen O.
2026.
Mississippi River Macrophytes Recovery.
U.S. Geological Survey software release. Reston, Va. 
https://github.com/DanelleLarson/HMSC-Mississippi-River-Macrophytes

_Authors' [ORCID](https://orcid.org) nos.:_

- Danelle M. Larson, [0000-0001-6349-6267](https://orcid.org/0000-0001-6349-6267)
- Mirkka Jones, [0000-0002-8157-8730](https://orcid.org/0000-0002-8157-8730)
- Benjamin Weigel, [0000-0003-2302-5529](https://orcid.org/0000-0003-2302-5529)
- Brian Gray, [0000-0001-7682-9550](https://orcid.org/0000-0001-7682-9550)
- Otso Ovaskainen, [0000-0001-9750-4421](https://orcid.org/0000-0001-9750-4421)
***
***

This repository contains analysis codes to reproduce the key findings reported in the manuscript, "Upper Mississippi River re-oligotrophication and hydrogeomorphic changes over 20 years contributed to macrophyte community shifts and recovery" (https://doi.org/XXXXXXXXXXXX).

# Repository organization

The repository contains the following folders and files:


- `Data sets` folder holding all data sets for the HMSC macrophyte study, which included nearly 20,000 study plots. These are non-authorative copies of the data and are included to allow a reproducible workflow. The authorative copies will reside on the USGS data portal, ScienceBase:  https://doi.org/10.5066/P1JGFM76
  * `climate.csv` CSV file containing multiple climate summaries for precipitation and temperature for each study plot
  * `environmental covariates.csv` CSV file containing environmental habitat data for each study plot
  * `macrophyte community.csv` CSV file containing macrophyte species data observed for each study plot
  * `spatiotemporal context.csv` CSV file showing the hierarchical spatial nature of the Mississippi River for each study plot
  * `traits and phylogeny.csv` CSV file providing various macrophyte traits and phylogeny for each macrophyte species
  * `metadata.docx` Word file containing metadata for project context, data sources, and information for all datasets herein
  

- Files in the main-level of the repository.
  * `README.md`, which is this file.
  * `.gitignore` is a Git ignore files for the repository.
  * `LICENSE.md` is the Official USGS License. 
  * `code.json` is the meta-data about the code in a machine readable format following USGS Style requirements.
  * `workflow diagram.png` is a graphical image of the major steps in analysis.
  * `HMSC_XXXXX.R` are eight .R files for reproducing analyses and figures
    
# Subject area and programming background required

This code assumes the user is familiar with R.
No subject area expertise is required, although expertise in water quality and macrophytes will aid in interpretation of code and results. 

# Software version details

This code uses R, version 4.1.2 or higher R Core Team. (2024). R: A Language and Environment for Statistical Computing [Computer software]. R Foundation for Statistical Computing. 

# Code run time

Code times range ~30 seconds per random forest algorithm for spatial predictions.

# Acknowledgments 

The data and analysis were funded as part of the U.S. Army Corps of Engineers’ Upper Mississippi River Restoration Program. All data collection, author D.M.L, and the ‘2021 macrophyte restoration workshop’ was funded through the U.S. Army Corps of Engineers’ Upper Mississippi River Restoration Program. Author B.W. was supported by the Strategic Research Council of the Academy of Finland (Project 312650 BlueAdapt). Author O.O. was funded by Academy of Finland (grant no. 336212 and 345110), and the European Union: The European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation programme (grant agreement No 856506: ERC-synergy project LIFEPLAN). 

Any use of trade, firm, or product names is for descriptive purposes only and does not imply endorsement by the U.S. Government.
