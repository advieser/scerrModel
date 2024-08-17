# scerrModel - A Model for Scientific Errors in Data Analysis
This `R` package implements a formal model of scientific errors in data analysis. The model is based on the work of Kohrt et al. (2023) and Melinscak (2023).
This implementation allows the user to simulate a data set of studies while easily manipulating the model parameters and enabling simple analysis and comparison of its outsputs.

## Installation
Install this package from GitHub using the `remotes` package:
```r
install.packages("remotes")
remotes::install_github("https://github.com/advieser/scerrModel")
```

## References
- Kohrt, F., Melinščak, F., McElreath, R., & Schönbrodt, F. D. (2023). A theoretical model of analysis errors and their correction in scientific research (Version 1.0). Zenodo. https://doi.org/10.5281/ZENODO.10053574
- Melinščak, F. (2023). Error search decision-making model \[Personal communication\].
