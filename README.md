# Climate Change and Leptospirosis Health Risks in Brazil
Understanding the dynamic environmental, climatic, and social variables that drive leptospirosis outbreaks is a primary goal of epidemiological modelling. However, this is a complex task due to the intricate interactions involved. For a continental-scale country like Brazil, which experiences diverse climate regimes across its territory, it is essential to account for these regional differences in risk mapping. 

The goal of this case study is to characterise leptospirosis incidence in Brazil from 2007 to 2024, examining its spatial distribution alongside its main environmental and climatic drivers. The ultimate objective is to identify the areas most vulnerable to flood events across the Brazilian territory.

## Description

Confirmed leptospirosis cases from 2007 to 2024 were obtained from the Brazilian Notifiable Diseases Information System (Sistema de Informação de Agravos de Notificação – SINAN) (DATASUS, 2024). Because reporting is mandatory for local health authorities in Brazil, only confirmed cases were included in the analysis. 

Cases were aggregated by municipality, year, and month based on notification date and patient place of residence. Annual population estimates were sourced from the Brazilian Institute of Geography and Statistics (IBGE). The monthly incidence rate was calculated as the number of cases divided by the municipal population, expressed per 100,000 inhabitants. Daily rainfall data were obtained from the Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS), and land surface temperature (LST) data were obtained from the ESA LST products.


## Getting Started

### Dependencies

* R version >= 4.6
* Rstudio

### Installing

* Install R and Rstudio.
* Download or clone this repository.

### Executing program

* The code is organised into data preparation and data analysis scripts.
* Data from SINAN and IBGE are available in the `data/` folder.
* Satellite data processing scripts will be available soon.
  

## Help
Under construction.

## Authors

Andréa de Lima Oliveira (andrea.liolive@gmail.com)

## Version History

* 0.1
  * Initial Release (underconstrution)

## License

This project is licensed under the MIT License of 2023 ESA Climate Change Initiative - see the LICENSE file for details.

## Acknowledgments

We gratefully acknowledge the following organizations and institutions for providing open access to the data used in this project:

DATASUS / SINAN: For providing epidemiologic data on reported leptospirosis cases.

IBGE: For providing municipal population estimates.

CHIRPS (Climate Hazards Center, UC Santa Barbara): For daily precipitation data.

European Space Agency (ESA): For Land Surface Temperature (LST) climate data products.

