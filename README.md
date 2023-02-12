# Life Cycle Deficit (LCD) by age and gender

This repository contains the replication file for clustering Life Cycle Deficit (LCD) age profiles and visualizing the results using line plots and heatmaps for 77 countries, and for gender specific LCD age profiles for 39 countries.

### FIGURE 1: Life cycle deficit age profile clusters (N=77, 2002-2016)
![Image](https://user-images.githubusercontent.com/68189671/218335143-b0374c7c-f5ca-4f89-b024-d082f8c43f54.jpg)
### FIGURE 2: Life cycle deficit by age and clusters in 77 countries (2002-2016)
![Image](https://user-images.githubusercontent.com/68189671/218335529-b7b89352-2352-4b30-a787-9a2b78f4f5c4.jpg)

[Download FIGURE 1](https://github.com/LiliVargha/Public-Transfers_TG/blob/main/ClusterTG.jpg)
[Download FIGURE 2](https://github.com/LiliVargha/Public-Transfers_TG/blob/main/ClusterTGtiles.jpg)

The lifecycle deficit/surplus (LCD) is defined as age specific consumption (C) less labour income (YL). It is positive during the dependent years (shown in red colours) and negative during the working years (shown in blue colours). The values are averages calculated using  National Accounts, administrative and survey data in the different countries. The values at each age are normalized using the average labour income of age 30-49. Data is from 2002-2016, the most recent country estimations. Clustering is done using a data driven way: using Ward's clustering. For more details on this see the presentation on the typology of economic lifecycles [References below] and the replication files. For more details on the data see documentation of the data sources.

## Life Cycle Deficit by gender and age

### FIGURE 3: Life Cycle Deficit by gender and age in 39 countries ordered by the age of deficit/surplus transition of women

![Image](https://user-images.githubusercontent.com/68189671/218336215-a4aaf882-9e26-412e-b849-ff477f3f9505.jpg)

[Download FIGURE 3](https://github.com/LiliVargha/Public-Transfers_TG/blob/main/VizTG.jpg)

## Data source
1. [Global NTA results](https://www.ntaccounts.org/web/nta/show/Browse%20database) (Lee and Mason 2011)
2. [European AGENTA Project](http://dataexplorer.wittgensteincentre.org/nta/) (Istenič et al. 2019)
3. [Counting Women's Work](https://www.countingwomenswork.org/data) (Counting Women's Work 2022)
4. [World Population Prospects 2022](https://population.un.org/wpp/) (United Nations, DESA, Population Division: WPP 2022) and [wpp2022 R package](https://github.com/PPgp/wpp2022)

## Replication files
The files for replication is [cluster.R](https://github.com/LiliVargha/Labour-Income_YL/blob/main/cluster.R) for total labour income age profiles and [YL_byGender.R](https://github.com/LiliVargha/Labour-Income_YL/blob/main/YL_byGender.R) for gender specific labour income age profiles. The files contain explanations, different visualizations and types of clustering.

## References
Lili Vargha, Tanja Istenič: Towards a Typology of Economic Lifecycle Patterns. [Presentation at NTA14 Paris](https://ntaccounts.org/web/nta/show/Documents/Meetings/NTA14%20Abstracts), 15 February 2023

Lili Vargha, Bernhard Binder-Hammer, Gretchen Donehower, and Tanja Istenič: [Intergenerational transfers around the world: introducing a new visualization tool](https://www.ntaccounts.org/web/nta/show/Working%20Papers) NTA Working Papers, 2022. 

## Future versions will
- Include newest NTA data.
- Use different ordering (for example by continents).

## Also check the following repositories
- [Labour income by age in 77 countries and labour income by gender and by age in 39 countries](https://github.com/LiliVargha/Labour-Income_YL)
- [Public transfers by age in 50 countries](https://github.com/LiliVargha/Public-Transfers_TG)
