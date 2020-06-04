# Metadataset
Metadataset (www.metadataset.com) is a collection of open data from scientific publications. These publications are about the management of agricultural and natural resources (e.g., crops, soil, water, and wildlife). The data are extracted from scientific publications, standardized, and summarized using dynamic meta-analysis. Metadataset is two separate web apps:

1. A Django app for data entry and browsing publications and data by intervention, outcome and country
2. A Shiny app for analysing the data

This README file is about the Shiny app. For information about the Django app, please see https://github.com/gormshackelford/metadataset.

# Metadataset-Shiny
Metadataset-Shiny is a Shiny app that parses the data from the Metadataset API and uses the data for dynamic meta-analysis.

# Downloading and running Metadataset-Shiny offline
This README file describes the steps you could take to download and run Metadataset-Shiny on your computer. This could be useful if:

- You want to edit the Shiny app, to do analyses that are not currently available.
- You want to debug the Shiny app, because you are getting errors in the online version.
- You want to use the Shiny app offline, because it is not currently available online.

## Downloading the app
If you are using `git` you can download the Shiny app and its dependencies with `git clone https://github.com/gormshackelford/metadataset-shiny.git`. If not, you can download them manually from https://github.com/gormshackelford/metadataset-shiny.

You will also need to download the R packages that are required for this app (you can see which packages are loaded at the top of the `app.R` file). You may be able to use `packrat` to install these packages by running `packrat::status()` in R or R Studio.

## Changing the settings
To run the app offline, without caching, you will need to change several settings in `app.R` by uncommenting several lines of code. Please see the heading in `app.R` for `Running this Shiny app offline, without caching`. You will need to uncomment these lines:

```
read_data_from_cache <- FALSE
save_data_to_cache <- FALSE
```

If you are working with an offline version of Metadataset (e.g., running on the Django development server), then you will need to specify the URL for your server. For example:

`local_host <- "http://127.0.0.1:8000/api/"`

And you will need to set the host for the API queries by uncommenting this line:

`host <- local_host`

## Selecting the subject, intervention, and outcome
You will also need to set the ID numbers for the subject, intervention, and outcome that you want to analyse. For example, for the example on cover crops and their effects on crop yield, you would need to set these IDs:

```
subject <- "6"        # Cover crops (subject)
intervention <- "91"  # Cover cropping (intervention)
outcome <- "4"        # Crop yield (outcome)
```

To find these ID numbers (e.g., for cassava, subject == "1") you could check the links from Metadataset. For example, by hovering over the link for the example of cover crops and crop yield, you would see this URL:

https://metadataset.shinyapps.io/meta-analysis/?subject=6&intervention=91&outcome=4

In this URL, the subject is 6, the intervention is 91, and the outcome is 4.

## Running the app
After you have changed these settings, you should be able to run the app by selecting and running all of the lines in `app.R`. In R Studio, there is also a button for `Run App`. The app should open in a browser on your computer.

# Copyright
Metadataset-Shiny is copyright (c) 2020 Gorm Shackelford, but it is Open Source and licensed under the MIT License.
