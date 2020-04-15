# COVIDiStress-Shiny
A R Shiny app for COVIDiStress Survey

To launch the app, put all the files in a "COVIDiStress-Shiny" folder, the data [available here](https://osf.io/z39us/) renamed `covid_06042020_choice_values` in a folder above (`../`) and run the following code:

````
setwd("C:/Users/Loïs/Dropbox/RCoronavirus")
library(shiny)
runApp("COVIDiStress-Shiny")
````

To make the code run faster check line 15 of `server.R`