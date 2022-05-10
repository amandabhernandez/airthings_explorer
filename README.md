# airthings_explorer

This repository contains R code that handles the csv output from the Airthings web dashboard. There are 2 main projects here: 

1. An R scripts which cleans up the .csv file that Airthings gives you, including changing the time zone to EST. You can download just this file and read in your own data file to make it work (airthings_explorer.R). Also the script created for an analysis of the impact of kitchen exhaust fans and natural ventilation on cooking emissions from gas stoves (airthings_final.R).




2. A Shiny app: this is a user friendly data exploration interface. The graphs are made using plotly, so you can hover over them for more information. The app was originally set up to use Airthings API to fetch real time measurement data associated with the sensor, but that feature was disabled since I no longer have the monitor. It now includes a page showing an intervention study looking at cooking emissions. Check out the live site: https://abhz.shinyapps.io/airthings_iaq_explorer/
