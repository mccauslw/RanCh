# RanCh

An R project providing tools and data for abstract discrete Random Choice analysis.

To install, update R to at least version 3.6.0, then at the R prompt:

1. Install devtools package (if not already installed) and load it
```R
install.packages("devtools")
library(devtools)
```
2. Install RanCh package (to do once)
```R
install_github("mccauslw/RanCh", build_vignettes=TRUE)
```
3. Load RanCh package (to do before using package)
```R
library(RanCh)
```
4. To get help for RanCh package:
```R
help(RanCh)
```
5. To get list of all functions with short descriptions
```R
library(help='RanCh')
```
6. To get help on a particular function
```R
help('regularity')
```
7. To read vignette:
```R
vignette('main_vignette', package='RanCh')
```
