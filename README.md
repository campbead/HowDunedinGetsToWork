# How Dunedin Gets to Work
*How Dunedin Gets to Work* was built for the [There and back again](https://www.stats.govt.nz/2018-census/there-and-back-again-data-visualisation-competition) data visualisation competetion from [Stats NZ](https://www.stats.govt.nz/).

The purpose of this app to allow residents of Dunedin to visualise commuting paterns within thier city.  I wanted to encourage visitors to reflect on their own transportation choices, while considering their neighbors and wider community.  I hope this project is a starting point for considering why residents make the choices they make around commuting.
            
This app was built using [Shiny from RStudio](https://shiny.rstudio.com/). The interactive map is built using [Leaflet for R.](https://rstudio.github.io/leaflet) 
Data processing and development are done using [R](https://www.r-project.org/) and the [tidyverse packages.](https://www.tidyverse.org/)

This app is open source and available at [Github](https://github.com/campbead/HowDunedinGetsToWork)

If you like this, please consider reaching out to me on [Twitter](https://twitter.com/campbead) or check my personal website: [adam-campbell.com](https://www.adam-campbell.com/)

- `app.R` contains the Shiny app
- `preprocess_data.Rmd` processes census data into the scoring data in the Shiny app
- `submission_description.txt` is the Submission Description text supplied for the competition.
