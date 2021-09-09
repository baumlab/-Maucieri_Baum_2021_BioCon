****

Data and R code accompanying:

<b>Impacts of heat stress on soft corals, an overlooked and highly vulnerable component
of coral reef ecosystems, at a central equatorial Pacific atoll</b>

Authors: [Dominique G. Maucieri](https://dominiquemaucieri.com), and [Julia K. Baum](https://www.juliakbaum.org)

****


[Scripts Folder](scripts/)

* [soft_coral_cover_models.R](data/soft_coral_cover_models.R) for the models used in the manuscript analysis

* [sample_sizes.R](data/soft_coral_cover_models.R) for the script to create Table S1

* [figures.R](data/figures.R) for the script to create the figures


[Data Folder](data/)

* [raw_soft_coral.RData](data/raw_soft_coral.RData) includes raw soft coral cover data from Kiritimati island

	* ```UniqueID```: Unique identification for each quadrat at each site in each year sampled
	* ```Year```: Year that the data was collected
	* ```Site```: Site number
	* ```FieldSeason```: Field season ID 
	* ```HD_Cat```: Estimate of local human disturbance at each site as a categorical variable
	* ```HD_Cont```: Estimate of local human disturbance at each site as a continuous variable
	* ```NPP```: Max net primary productivity at each site (mg C m^-2 day^-1)
	* ```WE```: If the sampling site is on the sheltered or windward side of the atoll
	* ```Region```: Region of the atoll
	* ```TimeBlock```: If the sampling season was before, during or after the El Niño event
	* ```WindEnergy```: Wave energy at each site (kW m^-1)
	* ```Lobophytum```: Coral cover (%) for the soft coral Lobophytum
	* ```Sinularia```: Coral cover (%) for the soft coral Sinularia
	* ```Sarcophyton```: Coral cover (%) for the soft coral Sarcophyton
	* ```Cladiella```: Coral cover (%) for the soft coral Cladiella
	


* [Literature_Review.xlsx](data/Literature_Review.xlsx) raw data collected for literature review of soft coral data from recent heatwaves

	*```README```: tab contains definishtions of all columns and colors used as well as summaries of data collected
