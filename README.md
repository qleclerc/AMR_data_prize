# Stronger together? Potential and limitations of combining industry datasets to fill in global AMR surveillance gaps

Quentin Leclerc $^{1,2,3}$ *, Eve Rahbe $^{1,2}$ , Aleksandra Kovacevic $^{1,2}$ , Lulla Opatowski $^{1,2}$ 

$^{1}$ Epidemiology and Modelling of Antimicrobial Resistance, Institut Pasteur, France  
$^{2}$ Conservatoire National des Arts et Métiers, France  
$^{3}$ Université de Versailles Saint-Quentin-en-Yvelines/INSERM, France  

*quentin.leclerc@pasteur.fr

<br/>

This repository contains the code for our project submitted to the Vivli AMR Data Challenge.

<br/>

**Note**: The raw industry surveillance datasets were obtained via Vivli and are not included in this repository. The `prepare_data.R` script requires these datasets to be present in a folder named "raw", placed in the "data" folder.

We have included in the "data" folder the example combined dataset `final_AMR_dataset.csv` for *E. coli* and *K. pneumoniae* resistance to ceftriaxone, ceftazidime, imipenem and meropenem, for the years 2018-19.

If you have the industry surveillance datasets, you can edit the `prepare_data.R` script to choose the combinations of years, bacteria and antibiotics to extract and combine from the datasets. This script will automatically warn you if your chosen combination is not present within one or more datasets. The resulting combined dataset will be saved in the "data" folder as `final_AMR_dataset.csv`.
