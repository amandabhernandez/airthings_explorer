# airthings_explorer

This repository contains R code that handles the csv output from the Airthings web dashboard. There are 2 main projects here: 

1. An R script which cleans up the .csv file that Airthings gives you and makes a simple time series plot with each of the metrics. You can download just this file and read in your own data file to make it work. (airthings_explorer.R)



2. A Shiny app: this currently works similarly to the R script but provides a user friendly data exploration interface. The graphs are made using plotly, so you can hover over them for more information. At present, the app relies on the Airthings API to fetch real time measurement data associated with the sensor. This means you'll need to edit the "client_id" and "client_secret" from your own airthings dashboard. I'm working on publishing a live version of site with my own data as a demo (airthings_explorer_app/...)
