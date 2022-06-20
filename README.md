# MAMME - Assessment of air pollution health co-benefits of Net-zero climate policies
 
This is the code used for the analyses and figures of the MAMME - UPC master thesis **Assessment of air pollution health co-benefits of Net-zero climate policies**.

## Usage

The code is written mainly in R and divided in three big blocks to pre-process and perform the sensitivity and uncertainty analyses:
* Emissions
* Concentrations
* Mortality

To run the pre-processing script, run:

```R
source('R/E_data_preprocess.R') # for emissions
source('R/C_data_preprocess.R') # for concentrations
source('R/M_data_preprocess.R') # for mortality
```
To build the figures, run:
```R
source('R/E_main.R') # for emissions
source('R/C_main.R') # for concentrations
source('R/M_main.R') # for mortality
```
To obtain better legends on the sensitivity plots, run:
```python
python3 Python/CI_overlay_imp.py        # for parameter values' sensitivity
python3 Python/ZCF_overlay_imp.py       # for counterfactual values' sensitivity
python3 Python/ZCF_CI_overlay_imp.py    # for parameter and counterfactual values' sensitivity
```
## Description of the files
For emissions(E), concentrations(C), and mortality(M), the following R scripts are present: 
* `R/*_data_preprocess.R`: to handle the data before tests and plots.
* `R/*_main.R`: to run the tests and build the figures of the specified data.

In detail, the tests and figures are build through:
* `R/E_functions.R`: to call the tests and arrange the emission's figures.
* `R/C_functions.R`: to call the tests and arrange the concentration's figures.
* `R/E_C_functions.R`: to compute the tests and build the emission's and concentration's figures.
* `R/M_avoided_deaths.R`: to compute the avoided deaths table and map.
* `R/M_num_deaths.R`: to compute the premature deaths table.
* `R/M_distrib_cum.R`: to compute the probability distribution and cumulative frequency of mortalities and build the corresponding figures.
* `R/M_ks_test.R`: to compute the two-sample Kolmogorov-Smirnov test and build the corresponding figure.
* `R/M_sensitivity.R`: to build the figures to assess the sensitivity of both impact function's parameter and counterfactual percentile.
* `R/M_badtails.R`: to compute the exceedance probability by climate policy and build the corresponding figure.
* `R/IamsVsImpfun.R`: to build the figures to assess the uncertainty produced by IAMs and impact functions.


Moreover, `R/zzz.R` contains palettes and extra functions to rename factors. To do the methodology plot `R/plot_regionalize_methods.R` as well as `R/plot_uncert_methods.R` are considered.

Finally, to obtain a more descriptive legend of the sensitivity figures, `Python/CI_overlay_imp.py`, `Python/ZCF_overlay_imp.py`, and `Python/ZCF_CI_overlay_imp.py` could be run.


## Data source
Emissions data are obtained from the [ENGAGE project database](https://data.ene.iiasa.ac.at/engage/#/login?redirect=%2Fdownloads). To estimate concentrations [TM5-FASST(R)](https://acp.copernicus.org/articles/18/16173/2018) is used. Several impact functions described in the memory of the project are employed to compute mortality. 

For any questions, please ask klaudia.krb[at]gmail.com.
