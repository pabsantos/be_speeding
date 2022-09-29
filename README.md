# The Impact of Built Environment on Speeding Behavior in Curitiba - Brazil

## Introduction
This repository contains the methods of the master thesis entitled ["The Impact of Built Environment on Speeding Behavior in Curitiba - Brazil"](https://acervodigital.ufpr.br/bitstream/handle/1884/78426/R%20-%20D%20-%20PEDRO%20AUGUSTO%20BORGES%20DOS%20SANTOS.pdf?sequence=1&isAllowed=y) from the Post-Graduate Program in Urban Planning - PPU UPPR. The main objective is to apply a geographically weighted regression (GWR) to analyze the effect of the built environment on the occurrence of speeding in Curitiba, based on its 135 traffic analysis zones (TAZ) as area unit. The speeding data was gathered in the [Brazilian Naturalistic Driving Study (NDS-BR)](http://www.tecnologia.ufpr.br/portal/ceppur/estudo-naturalistico-de-direcao-brasileiro/). 

## Structure
| File/folder       | Description                                |
| ----------------- | ------------------------------------------ | 
| `R/`              | R scripts.                                 |
| `R/main.R`        | Main script.                               |
| `R/nds.R`         | Loads and processes the NDS-BR data.       |
| `R/taz.R`         | Calculates variables in TAZs.              |
| `R/eda.R`         | Establishes a EDA on the NDS-BR data.      |
| `R/gwr.R`         | Calculates GWR models on the variables.    |
| `R/clusters.R`    | Defines LISA clusters analysis on results. | 
| `R/misc.R`        | Analyzes land use and road safety.         |
| `data/`           | Data folder with inputs.                   |
| `plot/`           | All visual results (plots, maps).          |
| `table/`          | All numerical results, arranged in tables. |
NOTE: The `data/` folder can be downloaded in the following [link](https://drive.google.com/file/d/1vUMqP_QjeuGgOmwhLm3-VZjlEa7PPZX-/view?usp=sharing). 
