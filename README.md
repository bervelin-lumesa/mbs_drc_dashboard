# MBS-RDC : [MALARIA BEHAVIOR SURVEY](https://malariabehaviorsurvey.org/)

L'enquête comportementale liée paludisme est une étude transversale auprès des ménages sur les comportements liés au paludisme et les facteurs qui les motivent ou les inhibent. L'enquête utilise une méthodologie théorique et standardisée pour produire des données pour informer sur le paludisme social et comportemental
modifie les interventions.

Contient les codes R utilisés pour créer le tableau de bord pour l'enquête comportementale liée au paludisme en République Démocratique du Congo en 2021.
Ce tableau de bord servait à suivre l'évolution de la collecte de données dans les 15 provinces du pays où la collecte de données se déroulait.

Pour créer ce tableau de bord, le logiciel R et son IDE RStudio ont été utilisés, avec les packages :

library(flexdashboard)
library(haven)
library(foreign)
library(leaflet)
library(tidyverse)
library(kableExtra)
library(knitr)
