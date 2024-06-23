# FoodWebsLakeStream
📦 Scripts and data for reproducing the results obtained by Willem Bonnaffé <a itemprop="sameAs" content="https://orcid.org/0000-0002-5053-8891" href="https://orcid.org/0000-0002-5053-8891" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>, Alain Danet <a itemprop="sameAs" content="https://orcid.org/0000-0002-1592-9483" href="https://orcid.org/0000-0002-1592-9483" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>, Camille Leclerc <a itemprop="sameAs" content="https://orcid.org/0000-0001-5830-1787" href="https://orcid.org/0000-0001-5830-1787" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>, Victor Frossard <a itemprop="sameAs" content="https://orcid.org/0000-0003-1338-4739" href="https://orcid.org/0000-0003-1338-4739" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>, Eric Edeline <a itemprop="sameAs" content="https://orcid.org/0000-0003-4963-4453" href="https://orcid.org/0000-0003-4963-4453" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a>, and Arnaud Sentis <a itemprop="sameAs" content="https://orcid.org/0000-0003-4617-3620" href="https://orcid.org/0000-0003-4617-3620" target="orcid.widget" rel="noopener noreferrer" style="vertical-align:top;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width:1em;margin-right:.5em;" alt="ORCID iD icon"></a> in the paper "The interaction between warming and enrichment accelerates food-web simplification in freshwater systems".


<br />

## Repository overview
This repository is structured as follow:

:file_folder: [**FoodWebsRiverLake/**](https://github.com/CamilleLeclerc/FoodWebsLakeStream) *(root of the compendium)*  
├── :file_folder: [**data/**](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/master/data) *(contains data, arranged by type: raw and derived)*  
│ &nbsp;  &nbsp;  &nbsp; ├── :file_folder: [**derived-data/**](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/master/data/derived-data) *(contains modified data derived from raw-data)*  
│ &nbsp;  &nbsp;  &nbsp; └── :file_folder: [**raw-data/**](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/master/data/raw-data) *(read-only data and should never be changed)*       
├── :file_folder: [**doc/**](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/main/doc) *(contains documents related to the project (e.g., pdf from an .Rmd file))*  
├── :file_folder: [**figs/**](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/main/figs) *(contains figures related to the project)*   
├── :file_folder: [**statistical_analysis/**](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/main/statistical_analysis) *(contains all files related to statistical analysis)*   
├── :file_folder: [**R/**](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/main/R) *(contains the R functions)*  
└── :file_folder: [**vignettes/**](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/master/vignettes) *(contains Rmd files for running specific analysis (e.g. metaweb reconstruction))*  
<br />

Please note that the :file_folder: [**statistical_analysis**](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/main/statistical_analysis) includes a README.md file with specific instructions for analysis. Additionally, both the :file_folder: [**derived-data **](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/master/data/derived-data) and the :file_folder: [**raw-data **](https://github.com/CamilleLeclerc/FoodWebsLakeStream/tree/master/data/raw-data) contain their own README.md files, which provide details about the data files.
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
