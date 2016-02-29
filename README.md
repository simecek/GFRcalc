## GFRcalc

R/Shiny app to estimate Glomerular Filtration Rate with two-compartment model 

### How-To

To access R/Shiny app, go to 

[http://churchilldev.jax.org:49194/GFRcalc/](http://churchilldev.jax.org:49194/GFRcalc/)
(JAX network only)

To use the batch processing, collect all XLSX files into one folder and use `manual_mode.R` script (after modifying the first four lines)

### Files

* `ui.R`, `server.R` = Shiny app
* `manual_mode.R` = batch mode
* `helpers.R` = helper R functions used both by Shiny and batch mode

### Reference

Hall, J. E., Guyton, A. C., & Farr, B. M. (1977). A single-injection method for measuring glomerular filtration rate. American Journal of Physiology-Renal Physiology, 232(1), F72-F76.
