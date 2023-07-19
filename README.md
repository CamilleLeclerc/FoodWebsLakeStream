# FoodWebsRiverLake
📦 Data and R code used in Bonnaffé *et al.* (in prep) to explore the individual and interactive effects of enrichment and temperature on food-web structure in lakes and rivers.
<br />

## Repository overview
This repository is structured as follow:

:file_folder: [**FoodWebsRiverLake/**](https://github.com/CamilleLeclerc/FoodWebsRiverLake) *(root of the compendium)*  
├── :file_folder: [**data/**](https://github.com/CamilleLeclerc/FoodWebsRiverLake/tree/master/data) *(contains data, arranged by type: raw and derived)*  
│ &nbsp;  &nbsp;  &nbsp; ├── :file_folder: [**derived-data/**](https://github.com/CamilleLeclerc/FoodWebsRiverLake/tree/master/data/derived-data) *(contains modified data derived from raw-data)*  
│ &nbsp;  &nbsp;  &nbsp; └── :file_folder: [**raw-data/**](https://github.com/CamilleLeclerc/FoodWebsRiverLake/tree/master/data/raw-data) *(read-only data and should never be changed)*       
├── :file_folder: [**doc/**](https://github.com/CamilleLeclerc/FoodWebsRiverLake/tree/main/doc) *(contains documents related to the project (e.g., pdf from an .Rmd file))*  
├── :file_folder: [**figs/**](https://github.com/CamilleLeclerc/FoodWebsRiverLake/tree/main/figs) *(contains figures related to the project)*   
├── :file_folder: [**outputs/**](https://github.com/CamilleLeclerc/FoodWebsRiverLake/tree/main/outputs) *(contains all intermediate and final results sorted by topic)*   
├── :file_folder: [**R/**](https://github.com/CamilleLeclerc/FoodWebsRiverLake/tree/main/R) *(contains the R functions developed for this project)*  
└── :file_folder: [**vignettes/**](https://github.com/CamilleLeclerc/FoodWebsRiverLake/tree/master/vignettes) *(contains Rmd files for running specific analysis)*  
<br />

## Running instructions
**Used softwares:**  
<br />
<img align="left" width="25" src="https://github.com/devicons/devicon/blob/master/icons/r/r-original.svg">*version 4.2.2 or greater*
<br />
<br />
<img align="left" width="25" src="https://github.com/devicons/devicon/blob/master/icons/rstudio/rstudio-original.svg">*version 2022.12.0 or greater* 
<br />
<br />
* Install (if necessary) and load all required packages
* Load all required functions
<br />

## Additional resources
* Bonnaffé, W., Danet, A., Legendre, S. & Edeline, E. (2021). Comparison of size-structured and species-level trophic networks reveals antagonistic effects of temperature on vertical trophic diversity at the population and species level. *Oikos*, 130, 1297–1309. [https://doi.org/10.1111/oik.08173](https://doi.org/10.1111/oik.08173).
* Danet, A., Mouchet, M., Bonnaffé, W., Thébault, E. & Fontaine, C. (2021). Species richness and food-web structure jointly drive community biomass and its temporal stability in fish communities. *Ecol. Lett.*, 24, 2364–2377. [https://doi.org/10.1111/ele.13857](https://doi.org/10.1111/ele.13857). Associated compendium: [fishcom](https://github.com/alaindanet/fishcom).
* [SizeTrophicInteractions](https://github.com/alaindanet/SizeTrophicInteractions) to compile and estimate (if necessary) of the individual body size of the fish.
