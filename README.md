# airthings_explorer

This repository contains R code that handles the csv output from the Airthings web dashboard. There are 2 main projects here: 

1. An R script which cleans up the .csv file that Airthings gives you and makes a simple time series plot with each of the metrics. You can download just this file and read in your own data file to make it work. 



2. A Shiny app: this currently does the same thing as the R script but lets you set a date range and look at one variable at a time. The graphs are also made using plotly, so you can hover over them for more information. 
