# raw-data/

## Folder content
This folder contains the following files and folders:  
- `BassinVersantTopographique_Métropole2019_BDTopage` – a folder including spatial files of [topographic watersheds](https://www.sandre.eaufrance.fr/atlas/srv/fre/catalog.search#/metadata/6571f89a-0608-49a2-9ab5-f005d842e81f).  

- `BassinsHydrographiques_Métropole2019_BDTopage` – a folder including spatial files of [hydrographic basins](https://www.sandre.eaufrance.fr/atlas/srv/fre/catalog.search#/metadata/4714a793-dcd8-4528-bd6a-80bfdbee9728).  

- `environment` – a folder including raw data for environmental variables (BOD and temperature) measured in lakes and streams.  

- `ne_10m_lakes` – a folder including spatial files of [natural and artificial lakes from *Natural Earth*](https://www.naturalearthdata.com/downloads/10m-physical-vectors/).  

- `ne_10m_rivers_lake_centerlines` – a folder including spatial files of [rivers + lake centerlines from *Natural Earth*](https://www.naturalearthdata.com/downloads/10m-physical-vectors/).  

- `code_species_river_lake.txt` – (66 lines x 6 columns) – includes the species code (`sp_code`), as well as the Latin names of stream species (`sp_river`) and lake species (`sp_lake`) if present in the ecosystem.  

- `fish_diet_shift.csv` – (154 lines x 18 columns) – includes the species code (`sp_code`) and associated latin name (`Anguilla_anguilla`), range of body size considered (`size_min` and `size_max`),  ontogenetic stage (`stage`) and the resources on which the species feeds (`light`, `det`, `biof`, `phytob`,`macroph`, `phytopl`, `zoopl`,`zoob`, `fish`) as well as additional information about the source of the information (`ref_diet`, `ref_ods`, `hypoth_ods`, `remark`).  

- `ind_size_lake.csv` – (751,129 lines x 7 columns) – includes the code of the sampling station (`code_lac`), the sampling year (`camp_annee`), the sampling event ID (`id_campagne`), the fish sampling ID (`id_prelev_poisson`), the sampling point ID where the fish sampling was conducted (`id_point_prelev`), the species name (`species`) and associated body size (`fish`).  

- `ind_size_stream.csv` – (3,152,334 lines x 3 columns) – includes the code of the sampling station (`opcod`), the species code (`species`) and associated body size (`length`).  

- `lake_list.txt` – (3,152,334 lines x 3 columns) – includes the code of the sampling station (`code_lac`), the sampling year (`camp_annee`) and the sampling event ID (`id_campagne`).  

- `op_analysis.csv` – (5,905 lines × 10 columns) – includes the code of the sampling event ID (`opcod`), the sampling station ID (`station`), the sampling date (`date`), the sampling year (`year`), the type of protocol (`protocol_type`), the protocol used (`protocol`), the surface sampled (`surface`), the number of species (`nb_sp`) and individuals (`nb_ind`) and associated body size measured (`length_sourced`).  

- `plando_spatial_coordinates.csv` – (284 lines × 3 columns) – includes the sampling station ID (`cd_lac`), and associated latitude (`lat_plando`) and longitude (`long_plando`).  

- `pred_win.csv` – (61 lines × 9 columns) – includes the species code (`species`), information related to the predation window (`alpha_min`, `beta_min`, `alpha_max`, `beta_max`, `alpha_mean`, `beta_mean`) and additional information about the source (`source`, `remark`).    

- `resource_diet_shift.csv` – (7 lines x 18 columns) – similar file than the `fish_diet_shift.csv`, but this time for the 7 resource.   

- `sampling_protocol.csv` – (11,924 lines x 13 columns) – includes the code of the sampling station (`code_lac`), the sampling year (`camp_annee`), the sampling event ID (`id_campagne`), the fish sampling ID (`id_prelev_poisson`), the sampling point ID where the fish sampling was conducted (`id_point_prelev`), the deployment date (`date_pose`), the deployment time (`heure_pose`), the retrieval date (`date_releve`), the retrieval time (`heure_releve`), the minimum depth at deployment point (`prof_min_point_pose`), the maximum depth at deployment point (`prof_max_point_pose`), the fishing gear code (`cd_engin_peche`), the sampling stratum (`strate`).  

- `station_analysis.csv` – (422 lines x 4 columns) – includes the code of the sampling station (`id`), the name of the sampling station (`precise_location`), the municipality code (`com_code_insee`), and spatial information (`geometry`).  

- `weight_length_coef.csv` – (50 lines x 4 columns) – includes the species code (`weight_length_coef`) and associated length-weight relationship coefficients (`a` and `b`) and source (`source`).
