# This Shiny app is for the dynamic meta-analysis of data from Metadataset (www.metadataset.com).
# This code is provided under the MIT licence, without any warranties. It was developed by Gorm
# Shackelford <gorm.shackelford@gmail.com> in 2020. 

# To run this Shiny app online (e.g., on shinyapps.io), with caching, you need access to an Amazon 
# AWS S3 bucket with credentials that must be provided in a file named "config.yml" in the 
# same directory as this file (and you need set the name of "s3_bucket", below). This S3 bucket is 
# where the cached files are saved.

# To run this Shiny app offline (e.g., on your computer), without caching, you do not need access to
# an AWS S3 bucket, but you will need to set the options, below, under the heading for 
# "Running this Shiny app offline, without caching".

# This code was tested with R version version 3.6.3 and the following R packages.
library(aws.s3)    # Tested with version 0.3.21
library(config)    # Tested with version 0.3
library(digest)    # Tested with version 0.6.21
library(ggplot2)   # Tested with version 3.3.0
library(httr)      # Tested with version 1.4.1
library(jsonlite)  # Tested with version 1.6
library(nlme)      # Tested with version 3.1-147
library(plyr)      # Tested with version 1.8.6
library(showtext)  # Tested with version 2.0
library(svglite)   # Tested with version 1.2.3

library(shiny)     # Tested with version 1.4.0.2
library(shinycssloaders)  # Tested with version 0.3

library(metafor)   # Tested with version 2.4-0
library(MuMIn)     # Tested with version 1.43.17

# Evaluate the helper functions for using metafor models in MuMIn. These functions need to be
# assigned to the global environment in the Shiny app, or the dredge() function will not use the
# moderators that are specified in rma.mv() (e.g., mods = ~ mod_1 + mod_2).
eval(metafor:::.MuMIn)
assign("makeArgs.rma", makeArgs.rma, .GlobalEnv)
assign("coefTable.rma", coefTable.rma, .GlobalEnv)

options(warn = -1)     # Suppress warnings.
options(scipen = 999)  # Suppress scientific notation (e.g., 1e10).

# Use Google Fonts.
font_add_google(name = "Noto Serif", family = "Noto Serif", regular.wt = 400, bold.wt = 700)
showtext_auto()




#####################################################################################################
# UI
#####################################################################################################




ui <- function(request) { fluidPage(
  title="Metadataset",
  tags$head(includeCSS("stylesheet.css")),
  #conditionalPanel(
  #  condition="$('html').hasClass('shiny-busy')",
  #  HTML('
  #    <div id="loading_window">
  #      <div id="loading_progress"></div>
  #    </div>
  #  ')
  #),
  titlePanel(""),
  tabsetPanel(type = "tabs",
    tabPanel("Dynamic meta-analysis",
      sidebarLayout(position = "right",
        sidebarPanel(
          h1('Filters'),
          uiOutput('data_filters'),
          HTML('<br />'),
          h1('Settings'),
          sliderInput(
            "significant_p", 'Assumed p-value for comparisons reported as "significant" or "p < 0.05"',
            min = 0.0001, max = 0.05, value = 0.025, step = 0.001
          ),
          sliderInput(
            "non_significant_p", 'Assumed p-value for comparisons reported as "non-significant" or "p > 0.05"',
            min = 0.05, max = 0.9999, value = 0.525, step = 0.005
          ),
          checkboxInput(
            "v_from_p",
            "Approximate the variance of the log response ratio using its (assumed) p-value or z-value",
            value = TRUE
          ),
          checkboxInput(
            "v_outliers",
            "Exclude rows with exceptionally high variance (outliers)",
            value = TRUE
          ),
          sliderInput(
            "v_outliers_threshold",
            "Threshold for excluding outliers (in deviations from the median variance)",
            min = 1, max = 10, value = 10, step = 0.5
          ),
          checkboxInput(
            "impute_v",
            "Impute the variance for rows without variance (using the mean variance)",
            value = TRUE
          ),
          HTML('<br />'),
          h1('Random effects'),
          HTML('<br />'),
          uiOutput('column_names'),
          HTML('Rows with different values in the selected column(s) will be treated as independent
            studies within a publication. Specifically, the metafor model will use the formula
            "random = ~ 1 | publication/study". Please see the "Data" tab for the "study" column,
            which is a combination of "study_ID" and the column(s) you selected here.'),
          HTML('<br />'),
          HTML('<br />'),
          HTML('<br />'),
          HTML('<br />'),
          uiOutput('refresh_button')
          ),
        mainPanel(
          HTML('<div class="col-sm-6" id="column-left">'),
            HTML('<h1>Dynamic meta-analysis</h1>'),
            HTML('To analyse the effect of this intervention on this outcome, we suggest (1) subgroup
              analysis first (faster and simpler) and (2) meta-regression second (slower and more
              complicated, but potentially more powerful).'),
            HTML('<br />'),
            HTML('<br />'),
            uiOutput('intervention'),
            HTML('<br />'),
            uiOutput('outcome'),
            HTML('<br />'),
            HTML('<h1>Data</h1>'),
            HTML('You can also download the data and use it in your own analyses. If you have filtered
              the data, this will only download the data that you have filtered.'),
            HTML('<br />'),
            HTML('<br />'),
            downloadButton("downloadData", "Download CSV"),
            HTML('<br />'),
            HTML('<br />'),
          HTML('</div>'),
          HTML('<div class="col-sm-6" id="column-right">'),
            HTML('<h1>(1) Subgroup analysis</h1>'),
            HTML('Subgroup analysis uses only the data that you have filtered.'),
            HTML('<br />'),
            HTML('<br />'),
            actionButton("go", "Start your analysis"),
            actionButton("make_bookmark", "Bookmark your analysis"),
            HTML('<br />'),
            uiOutput('bookmark_link'),
            HTML('<br />'),
            imageOutput('subgroup_analysis_plot', inline = TRUE),
            HTML('<br />'),
            HTML('<br />'),
            uiOutput('paragraph'),
            HTML('<br />'),
            HTML('<h1>(2) Meta-regression</h1>'),
            HTML('Meta-regression uses all of the data, not only the data that you have filtered
              (unlike subgroup analysis). In meta-regression, the results for your data will differ
              from the overall results only if at least one of your filters has a statistically
              significant effect on the results. Meta-regression is potentially more powerful than
              subgroup analysis, but it may also be much slower. Please finish your subgroup analysis
              first, and then do your meta-regression second, using the same filters that you used
              for your subgroup analysis.'),
            HTML('<br />'),
            HTML('<br />'),
            actionButton("meta_regression_go", "Meta regression"),
            HTML('<br />'),
            HTML('<br />'),
            imageOutput('meta_regression_plot', inline = TRUE),
            HTML('<br />'),
            HTML('<br />'),
            uiOutput('meta_regression_paragraph'),
          HTML('</div>'),
          HTML('<br />'),
          HTML('<br />'),
          uiOutput('debug1'),
          HTML('<br />'),
          HTML('<br />'),
          uiOutput('debug2'),
          HTML('<br />'),
          HTML('<br />'),
          uiOutput('debug3'),
          HTML('<br />')
        )
        )
      ),
    tabPanel("Forest plot",
      mainPanel(
        h1('Forest plot'),
        HTML('<br />'),
        withSpinner(imageOutput('forest_plot'))
      )
    ),
    tabPanel("Funnel plot",
      mainPanel(
        h1('Funnel plot'),
        HTML('<br />'),
        withSpinner(imageOutput('funnel_plot'))
      )
    ),
    tabPanel("Model summaries",
        h1('Model summaries'),
        HTML('We use the <span class="bold">metafor</span> package in R to model the data. Please
          note that the units in these model summaries are <span class="bold">log response ratios</span>,
          which are back-transformed into <span class="bold">response ratios</span> on the
          &quot;Dynamic meta-analysis&quot; tab (in R, response_ratio = exp(log_response_ratio).'),
        HTML('<br />'),
        h1('(1) Subgroup analysis'),
        HTML('rma.mv(log_response_ratio, selected_v, random = ~ 1 | publication/study)'),
        HTML('<br />'),
        HTML('<br />'),
        withSpinner(verbatimTextOutput('subgroup_analysis_summary')),
        HTML('<br />'),
        h1('(2) Meta-regression'),
        HTML('We use the <span class="bold">MuMIn</span> package in R to fit <span class="bold">
          metafor</span> models with all combinations of the variables that you selected and their
          two-way interactions as moderators. We then select the &quot;best&quot; model (with the
          lowest AIC) and get the model predictions for the moderator levels that you selected.'),
        HTML('<br />'),
        HTML('<br />'),
        withSpinner(textOutput('meta_regression_formula')),
        HTML('<br />'),
        withSpinner(verbatimTextOutput('meta_regression_summary')),
        HTML('<br />'),
        withSpinner(textOutput('meta_regression_predict')),
        HTML('<br />'),
        withSpinner(verbatimTextOutput('meta_regression_prediction'))
      ),
    tabPanel("Study summaries and weights",
      mainPanel(
        h1('Study summaries'),
        HTML('<br />'),
        HTML('<table>'),
        uiOutput('summaries'),
        HTML('</table>')
      )
    ),
    tabPanel("Value judgements",
      mainPanel(
        h1('Value judgements'),
        HTML('<br />'),
        HTML('Is it better for an outcome to increase or decrease as a result of an intervention?
          The default setting is that "increase is better". You will need to adjust this setting
          for some outcomes. For example, a decrease in soil erosion is probably better than an
          increase. However, this value judgement will probably be more complicated for some
          outcomes.
          <br />
          <br />
          If you check the box for "decrease is better", then the meta-analysis will replace the
          response ratio (R) with the inverse of the response ratio (1 / R) for that outcome. Thus,
          a higher value (1 / R) will be better, and it will be meaningful to compare this outcome
          with outcomes for which "increase is better".'),
        HTML('<br />'),
        HTML('<br />'),
        h1('Settings'),
        HTML('<br />'),
        uiOutput('outcome_filters')
        )
      )
    )
  )
}  # End of UI





#####################################################################################################
# Server
#####################################################################################################




server <- function(input, output, session) {

  withProgress(message="Loading data...", value=0, {
    
    ########################
    # Get the URL parameters
    ########################

    # Get the url parameters that were used to launch the app.
    # For example, subject=6 for https://www.metadataset.com/api/data/?subject=6
    protocol <- isolate(session$clientData$url_protocol)
    hostname <- isolate(session$clientData$url_hostname)
    pathname <- isolate(session$clientData$url_pathname)
    port <- isolate(session$clientData$url_port)
    query <- parseQueryString(isolate(session$clientData$url_search))
    if (!is.null(query[['subject']])) {
      subject <- query[['subject']]
    } else {
      subject <- "6"  # Cover crops is the default subject, if no subject is specified.
    }
    if (!is.null(query[['intervention']])) {
      intervention <- query[['intervention']]
      intervention_pk <- as.numeric(intervention)
    } else {
      intervention <- ""
      intervention_pk <- NA
    }
    if (!is.null(query[['outcome']])) {
      outcome <- query[['outcome']]
      outcome_pk <- as.numeric(outcome)
    } else {
      outcome <- ""
      outcome_pk <- NA
    }
    if (!is.null(query[['publication']])) {
      publication <- query[['publication']]
    } else {
      publication <- ""
    }
    if (!is.null(query[['user']])) {
      user <- query[['user']]
    } else {
      user <- ""
    }
    if (!is.null(query[['bookmark']])) {
      bookmark <- query[['bookmark']]
    } else {
      bookmark <- ""
    }
    if (!is.null(query[['refresh']])) {
      read_data_from_cache <- FALSE
    } else {
      read_data_from_cache <- TRUE
    }
    save_data_to_cache <- TRUE

    
    #################################################
    # Running this Shiny app offline, without caching
    #################################################
    
    # To run the Shiny app offline (e.g., to debug it), you should disable data caching. There are
    # two parts to caching: (1) reading data from the cache (an Amazon S3 bucket), and (2) saving the
    # data to the cache (if not reading data from the cache). Reading data from the cache is the
    # default (read_data_from_cache == TRUE). However, if a user clicks the refresh button, 
    # read_data_from_cache == FALSE, and fresh data will be queried from the Metadataset API. The data 
    # will then be saved to the S3 bucket (overwriting the old cache, if it exists), if 
    # save_data_to_cache == TRUE. To run the Shiny app offline, without access to the S3 bucket
    # (which requires the S3 credentials to be saved in a separate file, named "config.yml") you 
    # should disable both parts of data caching by uncommenting these two lines:
    
    #read_data_from_cache <- FALSE
    #save_data_to_cache <- FALSE
    
    
    # To run the Shiny app offline (e.g., to debug it), you should also specify the ID number for the
    # subject and optionally the intervention and/or outcome and/or publication you want to analyse. 
    # To find these ID numbers (e.g., for cassava, subject == "1") you could check the links from 
    # Metadataset. For example, by hovering over the link for the example of cover crops and crop 
    # yield, you would see this URL: 
    # https://metadataset.shinyapps.io/meta-analysis/?subject=6&intervention=91&outcome=4. 
    # In this URL, the subject is 6, the intervention is 91, and the outcome is 4. Here are some 
    # example of how you should specify the ID numbers:
    
    # Examples for cover crops
    #subject <- "6"        # Cover crops (subject)
    #intervention <- "91"  # Cover cropping (intervention)
    #outcome <- "4"        # Crop yield (outcome)
    
    # Other examples of outcomes for cover crops
    #outcome <- "20"  # Soil
    #outcome <- "198"  # Soil organic matter
    #outcome <- "46"   # Soil microbial biomass
    #outcome <- "38"   # Soil nitrogen content
    #outcome <- "31"  # Soil water content
    #outcome <- "182"  # Soil nutrient leaching
    #outcome <- "71"  # Carbon dioxide
    #outcome <- "166"  # Crop damage
    #outcome <- "455"  # Weed abundance
    #outcome <- "456"  # Weed diversity
    
    # Examples for Japanese knotweed
    #subject = "7"
    #intervention = "774"  # Herbicides
    #intervention = "782"  # Cutting/chopping
    #publication = "26309"
    
    # Examples for cassava
    #subject = "1"
    #intervention = "46"  # Intercropping
      
        
    ##############
    # Get the data
    ##############
    
    # Connect to AWS S3 using credentials for the user, "metadataset-shiny".
    if (read_data_from_cache == TRUE | save_data_to_cache == TRUE) {
      s3_credentials <- config::get("s3")
      Sys.setenv(
        "AWS_ACCESS_KEY_ID" = s3_credentials$AWS_ACCESS_KEY_ID,
        "AWS_SECRET_ACCESS_KEY" = s3_credentials$AWS_SECRET_ACCESS_KEY,
        "AWS_DEFAULT_REGION" = s3_credentials$AWS_DEFAULT_REGION
      )
    }
    s3_bucket <- "metadataset-shiny-cache"
    
    # Get the data from the API. To get the data from the local host (e.g., if running the Metadatset
    # website offline, on your computer, for development), uncomment the line for 
    # "host <- local_host" and specify the path to the API on your computer (this is the path for
    # the API for the Django development server ["python manage.py runserver"]).
    local_host <- "http://127.0.0.1:8000/api/"
    remote_host <- "https://www.metadataset.com/api/"
    host <- remote_host
    #host <- local_host

    # Data
    api_query_string <- paste("subject=", subject, "&intervention=", intervention, "&outcome=", outcome, "&publication=", publication, "&user=", user, sep="")
    endpoint <- paste(host, "data/?", api_query_string, sep="")

    get_data_from_api <- function(endpoint) {
      i <- 0  # Counter for each page of results
      pages <- list()  # An empty list for storing pages of results
      response <- fromJSON(endpoint, flatten = TRUE)  # The first page of results
      repeat {
        i <- i + 1
        pages[[i]] <- response$results  # Store page i of the results in the list
        # If this is not the last page of results, then get the data from the next page
        if (!is.null(response$'next')) {
          response <- fromJSON(response$'next', flatten = TRUE)  # The URL of the next page
        } else {
          break
        }
      }
      results <- rbind_pages(pages)  # Bind the results from all pages
      return(results)
    }

    # The filenames for the cached data are based on hashes of the api_query_string.
    cache <- paste(digest(api_query_string), "/", sep = "")
    cached_data <- paste(cache, "data.rds", sep = "")
    cached_intervention <- paste(cache, "intervention.rds", sep = "")
    cached_outcome <- paste(cache, "outcome.rds", sep = "")
    cached_attributes <- paste(cache, "attributes.rds", sep = "")

    if (read_data_from_cache == TRUE & head_object(cached_data, s3_bucket, check_region=FALSE)) {
      # Get the cached data from the S3 bucket.
      df <- s3readRDS(cached_data, s3_bucket, check_region=FALSE)
      attributes_df <- s3readRDS(cached_attributes, s3_bucket, check_region=FALSE)
      intervention <- s3readRDS(cached_intervention, s3_bucket, check_region=FALSE)
      output$intervention <- renderUI(HTML(paste("<h1>This intervention</h1>", intervention)))
      outcome <- s3readRDS(cached_outcome, s3_bucket, check_region=FALSE)
      output$outcome <- renderUI(HTML(paste("<h1>This outcome</h1>", outcome)))
    } else {
      read_data_from_cache <- FALSE
      # Get the non-cached data from the API.
      df <- get_data_from_api(endpoint)
    }




    incProgress(0.10)




    if (read_data_from_cache == FALSE) {

      # Intervention
      if (intervention != "") {
        query <- paste("interventions/", intervention, "/", sep="")
        endpoint <- paste(host, query, sep="")
        response <- fromJSON(endpoint, flatten = TRUE)
        intervention <- response$intervention
      } else {
        intervention <- "All interventions"
      }
      if (save_data_to_cache == TRUE) {
        s3saveRDS(intervention, object = cached_intervention, s3_bucket, check_region=FALSE)
      }
      output$intervention <- renderUI(HTML(paste("<h1>This intervention</h1>", intervention)))

      # Outcome
      if (outcome != "") {
        query <- paste("outcomes/", outcome, "/", sep="")
        endpoint <- paste(host, query, sep="")
        response <- fromJSON(endpoint, flatten = TRUE)
        outcome <- response$outcome
      } else {
        outcome <- "All outcomes"
      }
      if (save_data_to_cache == TRUE) {
        s3saveRDS(outcome, object = cached_outcome, s3_bucket, check_region=FALSE)
      }
      output$outcome <- renderUI(HTML(paste("<h1>This outcome</h1>", outcome)))

      # Publication-level metadata
      names(df)[names(df) == "publication.EAV_publication"] <- "EAV_publication"
      names(df)[names(df) == "publication.xcountry_publication"] <- "publication_country"
      # Intervention-level metadata
      names(df)[names(df) == "experiment.intervention"] <- "intervention"
      names(df)[names(df) == "experiment.EAV_experiment"] <- "EAV_intervention"
      names(df)[names(df) == "experiment.xcountry_experiment"] <- "intervention_country"
      names(df)[names(df) == "experiment.study_experiment"] <- "intervention_study"
      # Population-level metadata
      names(df)[names(df) == "experiment_population.population"] <- "population"
      names(df)[names(df) == "experiment_population.EAV_population"] <- "EAV_population"
      names(df)[names(df) == "experiment_population.xcountry_population"] <- "population_country"
      names(df)[names(df) == "experiment_population.study_population"] <- "population_study"
      # Outcome-level metadata
      names(df)[names(df) == "experiment_population_outcome.outcome"] <- "outcome"
      names(df)[names(df) == "experiment_population_outcome.EAV_outcome"] <- "EAV_outcome"
      names(df)[names(df) == "experiment_population_outcome.xcountry_outcome"] <- "outcome_country"
      names(df)[names(df) == "experiment_population_outcome.study_outcome"] <- "outcome_study"

      # Country
      df$publication_country <- lapply(df$publication_country, unlist)
      df$publication_country <- cbind(lapply(df$publication_country, function(x) if(is.null(x)) NA else x))
      df$intervention_country <- lapply(df$intervention_country, unlist)
      df$intervention_country <- cbind(lapply(df$intervention_country, function(x) if(is.null(x)) NA else x))
      df$population_country <- lapply(df$population_country, unlist)
      df$population_country <- cbind(lapply(df$population_country, function(x) if(is.null(x)) NA else x))
      df$outcome_country <- lapply(df$outcome_country, unlist)
      df$outcome_country <- cbind(lapply(df$outcome_country, function(x) if(is.null(x)) NA else x))
      # Select the metadata at the lowest level (intervention > population > outcome)
      df$Country <- df$publication_country
      df$Country[!is.na(df$intervention_country)] <- df$intervention_country[!is.na(df$intervention_country)]
      df$Country[!is.na(df$population_country)] <- df$population_country[!is.na(df$population_country)]
      df$Country[!is.na(df$outcome_country)] <- df$outcome_country[!is.na(df$outcome_country)]
      df$Country <- lapply(df$Country, unlist)

      # Study
      df$intervention_study_id <- lapply(df$intervention_study, function(x) if(is.null(x[["study_id"]])) NA else x[["study_id"]])
      df$intervention_study_name <- lapply(df$intervention_study, function(x) if(is.null(x[["study_name"]])) NA else x[["study_name"]])
      df$population_study_id <- lapply(df$population_study, function(x) if(is.null(x[["study_id"]])) NA else x[["study_id"]])
      df$population_study_name <- lapply(df$population_study, function(x) if(is.null(x[["study_name"]])) NA else x[["study_name"]])
      df$outcome_study_id <- lapply(df$outcome_study, function(x) if(is.null(x[["study_id"]])) NA else x[["study_id"]])
      df$outcome_study_name <- lapply(df$outcome_study, function(x) if(is.null(x[["study_name"]])) NA else x[["study_name"]])
      # Select the metadata at the lowest level (intervention > population > outcome)
      df$study_id <- df$intervention_study_id
      df$study_id[!is.na(df$population_study_id)] <- df$population_study_id[!is.na(df$population_study_id)]
      df$study_id[!is.na(df$outcome_study_id)] <- df$outcome_study_id[!is.na(df$outcome_study_id)]
      df$study_name <- df$intervention_study_name
      df$study_name[!is.na(df$population_study_name)] <- df$population_study_name[!is.na(df$population_study_name)]
      df$study_name[!is.na(df$outcome_study_name)] <- df$outcome_study_name[!is.na(df$outcome_study_name)]

      # Location
      names(df)[names(df) == "experiment.location"] <- "Location"
      df$Location <- lapply(df$Location, unlist)

      # Experimental design
      names(df)[names(df) == "experiment.experimentdesign_set"] <- "Design"
      df$Design <- lapply(df$Design, unlist)

      # Methods
      names(df)[names(df) == "experiment.methods"] <- "Methods"
      df$Methods <- lapply(df$Methods, unlist)

      # Citation
      names(df)[names(df) == "publication.title"] <- "publication"
      names(df)[names(df) == "publication.citation"] <- "citation"

      # Get the attributes for this subject
      endpoint <- paste(host, "subjects/?subject=", subject, sep="")
      this_subject <- fromJSON(endpoint, flatten=FALSE)
      this_subject_attribute <- this_subject$results$attribute
      endpoint <- paste(host, "attributes/?parent=", this_subject_attribute, sep="")
      attributes <- fromJSON(endpoint, flatten=FALSE)
      attribute_types <- attributes$results$type
      attribute_units <- attributes$results$unit
      attributes <- attributes$results$attribute
      attribute_types <- attribute_types[order(attributes)]  # Sort in the same order that we will sort attributes
      attribute_units <- attribute_units[order(attributes)]  # Sort in the same order that we will sort attributes
      attributes <- attributes[order(attributes)]            # Sort attributes alphabetically




      incProgress(0.20)




      if (!is.null(attributes)) {

        # Parse the EAVs from the API query. EAVs (Entity Attribute Values) are user-defined, 
        # subject-specific attributes, whereas other attributes (such as "Country" and "Design") are not 
        # user-defined (they are the same for all subjects). EAVs can be specified at four hierarchical levels:
        # publication > intervention > population > outcome. For each of these levels, there is a column in df
        # (e.g., df$EAV_publication). Each row in this column is itself a data frame, containing all of the 
        # EAVs for one data point (i.e. all of the EAVs for one row in df). This function takes one of these 
        # columns (e.g., df$EAV_publication) and returns a list with one list item for each row in this column. 
        # Each list item is itself a list that contains at least one value (which may be NA) for each 
        # attribute in this subject.
        get_EAV_list <- function(x) {
          # If there are any values, these will be either "value_as_factor" or "value_as_number" (and the other
          # one will be NA). First, select the non-NA values. Second, aggregate the values by attribute (as one 
          # list of values per attribute). This allows us to have multiple values for each attribute, like
          # having multiple values for "Design" (e.g., "replicated" and "controlled") (but note that these are 
          # subject-specific attributes, which are dynamically defined, whereas "Design" is not).
          if(length(x) > 0) {
            # Select "value_as_factor" or "value_as_number" for each row (the one that is not NA).
            x$value <- ifelse(!is.na(x$value_as_factor), x$value_as_factor, x$value_as_number)
            # Aggregate the values by attribute.
            x <- aggregate(x$value, list(x$attribute), paste)
            colnames(x) <- c("attribute", "value")
            # Add these values to a data frame with one row for each attribute (not only the attributes
            # for which we have values). Attributes for which we do not have values will be set to NA.
            EAV_df <- data.frame(attribute=attributes)
            x <- base::merge(EAV_df, x, by="attribute", all=TRUE)
            # If there are not any values, set NA for all attributes.
          } else {
            x <- data.frame(attribute=attributes)
            x$value <- NA
          }
          # These values are in the same order as the attributes (which are in alphabetical order).
          return(x$value)
        }
        
        get_EAV_df <- function(x) {
          # Parse the EAVs from the API query. This returns a list with one item for each row in df.
          x <- lapply(x, get_EAV_list)
          # Convert each item in the list to a column in a matrix.
          x <- do.call(cbind, x)
          # Transpose the matrix, convert it to a data frame, and add column names (the names of all 
          # attributes for this subject).
          x <- t(x)
          x <- as.data.frame(x, stringsAsFactors = FALSE)
          colnames(x) <- attributes
          return(x)
        }
        
        # Get publication-level EAVs.
        EAV_publication_df <- get_EAV_df(df$EAV_publication)
        # Get intervention-level EAVs.
        EAV_intervention_df <- get_EAV_df(df$EAV_intervention)
        # Get population-level EAVs.
        EAV_population_df <- get_EAV_df(df$EAV_population)
        # Get outcome-level EAVs.
        EAV_outcome_df <- get_EAV_df(df$EAV_outcome)

        # Select the metadata at the lowest level (publication > intervention > population > outcome)
        EAV_df <- EAV_publication_df
        for (attribute in attributes) {
          EAV_df[[attribute]][!is.na(EAV_intervention_df[[attribute]])] <- EAV_intervention_df[[attribute]][!is.na(EAV_intervention_df[[attribute]])]
          EAV_df[[attribute]][!is.na(EAV_population_df[[attribute]])] <- EAV_population_df[[attribute]][!is.na(EAV_population_df[[attribute]])]
          EAV_df[[attribute]][!is.na(EAV_outcome_df[[attribute]])] <- EAV_outcome_df[[attribute]][!is.na(EAV_outcome_df[[attribute]])]
        }

      }  # End of if (!is.null(attributes))

      # Dataset for display
      df <- df[c("citation", "publication", "intervention", "population", "outcome", "comparison", "study_id", "study_name", "note", "treatment_mean", "treatment_sd", "treatment_n", "treatment_se", "control_mean", "control_sd", "control_n", "control_se", "treatment_mean_before", "treatment_sd_before", "treatment_n_before", "treatment_se_before", "control_mean_before", "control_sd_before", "control_n_before", "control_se_before", "n", "unit", "lsd", "is_significant", "approximate_p_value", "p_value", "z_value", "correlation_coefficient", "effect_size", "effect_size_unit", "other_effect_size_unit", "lower_limit", "upper_limit", "confidence", "se", "variance", "Methods", "Location", "Country", "Design")]
      if (!is.null(attributes)) {
        df <- cbind(df, EAV_df)
      }
      publications_n <- length(unique(df$publication))
      df$citation[df$citation == ""] <- "[AUTHOR], [YEAR]"            # The format from Metadataset if both author and year are NA
      df$citation[df$citation == "[AUTHOR], [YEAR]"] <- "[CITATION NA]"  # Convert to this new format.
      citations <- unique(df$citation)
      n_citations <- length(citations)
      # Differentiate publications with the same citation (append "a", "b", etc.).
      for (i in 1:n_citations) {
        this_citation <- citations[i]
        df_for_this_citation <- subset(df, citation == citations[i], select = c("publication", "citation"))
        publications_with_this_citation <- unique(df_for_this_citation$publication)
        if (length(publications_with_this_citation) > 1) {
          for (j in 1:length(publications_with_this_citation)) {
            this_publication <- publications_with_this_citation[j]
            df$citation[df$publication == this_publication] <- paste(this_citation, letters[j], sep = "")
          }
        }
      }
      df <- df[order(df$citation),]
      # Differentiate interventions within the same publication (append "[Study 1]", "[Study 2]", etc.).
      citations <- unique(df$citation)
      n_citations <- length(citations)
      for (i in 1:n_citations) {
        this_citation <- citations[i]
        df_for_this_citation <- subset(df, citation == citations[i], select = c("Methods", "citation"))
        studies <- unique(unlist(df_for_this_citation$Methods))
        if (length(studies) > 1) {
          for (j in 1:length(studies)) {
            this_study <- studies[j]
            df$citation[df$citation == this_citation & df$Methods == this_study] <- paste(this_citation, " [Study ", j, "]", sep = "")
          }
        }
      }
      row.names(df) <- NULL

      # SD from SE
      df$treatment_sd_from_se <- df$treatment_se * (sqrt(df$treatment_n))
      df$treatment_sd[is.na(df$treatment_sd)] <- df$treatment_sd_from_se[is.na(df$treatment_sd)]
      df$control_sd_from_se <- df$control_se * (sqrt(df$control_n))
      df$control_sd[is.na(df$control_sd)] <- df$control_sd_from_se[is.na(df$control_sd)]

      # Is it better if treatment_mean is lower than control_mean?
      df$lower_is_better <- FALSE

      # Response ratio, using formulas from Hedges, L.V., Gurevitch, J., and Curtis, P.S. (1999)
      # THE META-ANALYSIS OF RESPONSE RATIOS IN EXPERIMENTAL ECOLOGY. Ecology 80(4):1150-1156.
      # https://doi.org/10.2307/177062
      get_response_ratio <- function(x) {
        if(x$treatment_mean > 0 & x$control_mean > 0) {           # If both values are positive
          x$treatment_mean / x$control_mean                       # we use the response ratio.
        } else if(x$treatment_mean < 0 & x$control_mean < 0) {    # If both values are negative                                        # If both values are positive
          x$control_mean / x$treatment_mean                       # we use the reciprocal of the response ratio.
        } else if(x$treatment_mean == 0 & x$control_mean == 0) {  # If both values are 0
          1                                                       # we use 1.
        } else {                                                  # If only one value is negative
          NA                                                      # the response ratio is undefined.
        }
      }
      df$response_ratio <- apply(df, 1, get_response_ratio)
      df$log_response_ratio <- log(df$response_ratio)
      df$v_from_sd_and_n <- (df$treatment_sd ^ 2 / (df$treatment_n * df$treatment_mean ^ 2)) + (df$control_sd ^ 2 / (df$control_n * df$control_mean ^ 2))

      # Significance from LSD
      df$mean_difference <- df$treatment_mean - df$control_mean
      df$significance_from_lsd <- abs(df$mean_difference) - df$lsd
      df$significance_from_lsd <- lapply(df$significance_from_lsd, function(x) if(is.na(x)) NA else if(x < 0) "Non-significant" else "Significant")

      # Selected significance
      df$is_significant <- lapply(df$is_significant, function(x) if(is.na(x)) NA else if(x == "FALSE") "Non-significant" else "Significant")
      df$selected_significance <- df$is_significant
      df$selected_significance[!is.na(df$significance_from_lsd)] <- df$significance_from_lsd[!is.na(df$significance_from_lsd)]




      incProgress(0.20)




      # Countries
      countries <- sort(unique(unlist(df$Country)))

      # Designs
      designs <- sort(unique(unlist(df$Design)))

      # Generic attributes
      attributes_df <- data.frame(attribute = c("Country", "Design"))
      attributes_df$options <- c(list(countries), list(designs))
      attributes_df$min <- c(NA, NA)
      attributes_df$max <- c(NA, NA)
      attributes_df$type <- c("factor", "factor")
      attributes_df$unit <- c("", "")

      # Subject-specific attributes
      if (!is.null(attributes)) {
        attributes_df <- rbind(attributes_df, data.frame(attribute = attributes, options = NA, min = NA, max = NA, type = attribute_types, unit = attribute_units))
        for (attribute in attributes) {
          options <- unique(unlist(df[attribute]))
          options <- list(sort(options[!is.na(options)]))
          attributes_df$options[attributes_df$attribute == attribute] <- options
          if (attributes_df$type[attributes_df$attribute == attribute] == "number") {
            min <- min(as.numeric(unlist(df[attribute])), na.rm = TRUE)
            max <- max(as.numeric(unlist(df[attribute])), na.rm = TRUE)
            attributes_df$min[attributes_df$attribute == attribute] <- min
            attributes_df$max[attributes_df$attribute == attribute] <- max
          }
        }
      }

      if (save_data_to_cache == TRUE) {
        s3saveRDS(df, object = cached_data, s3_bucket, check_region=FALSE)
        s3saveRDS(attributes_df, object = cached_attributes, s3_bucket, check_region=FALSE)
      }

    }  # End of if (read_data_from_cache == FALSE)




    incProgress(0.20)




    # Selectors for attributes
    attributes_df$encoded_attribute <- gsub("[^[:alnum:]._]", ".", attributes_df$attribute)  # Delete non-alphanumeric characters (except periods and underscores)
    output$data_filters <- renderUI({
      lapply(1:length(attributes_df$attribute), function(i) {
        if(attributes_df$type[i] == "factor") {
          options <- sort(unique(unlist(df[[paste(attributes_df$attribute[i])]])))
          tagList(
            selectInput(paste(attributes_df$encoded_attribute[i]), paste(attributes_df$attribute[i]), options, multiple=TRUE)
          )
        } else {
          min <- min(as.numeric(unlist(df[[paste(attributes_df$attribute[i])]])), na.rm = TRUE)
          max <- max(as.numeric(unlist(df[[paste(attributes_df$attribute[i])]])), na.rm = TRUE)
          attributes_df$min[i] <- min
          attributes_df$max[i] <- max
          tagList(
            sliderInput(paste(attributes_df$encoded_attribute[i]), paste(attributes_df$attribute[i], " (", attributes_df$unit[i], ")", sep = ""), min = min, max = max, value = c(min, max))
          )
        }
      })
    })

    # Outcomes
    outcomes <- sort(unique(unlist(df$outcome)))
    encoded_outcomes <- gsub("[^[:alnum:]_]", "", outcomes)  # Delete non-alphanumeric characters (except underscores)

    # Selectors for outcomes
    output$outcome_filters <- renderUI({
      lapply(1:length(outcomes), function(i) {
        tagList(
          HTML(if (i == 1) '<table class="table"><tr><th>Outcome</th><th>Decrease is better</th></tr>'),
          HTML('<tr><td>'),
          HTML(paste(outcomes[i])),
          HTML('</td><td>'),
          checkboxInput(paste(encoded_outcomes[i]), "", value = FALSE),
          HTML('</td></tr>'),
          HTML(if (i == length(outcomes)) '<table>')
        )
      })
    })
    outputOptions(output, "outcome_filters", suspendWhenHidden = FALSE)

    # Selectors for column names
    output$column_names <- renderUI({
      selectInput("column_names", "Columns", unique(attributes_df$attribute), multiple=TRUE)
    })
    outputOptions(output, "column_names", suspendWhenHidden = FALSE)

  })  # End of withProgress() for data loading




  # Save a copy of this df, so that we can revert to it later.
  original_df <- df




  # Use bookmarked settings, if they exist
  if (bookmark != "") {
    bookmarked_settings <- paste("bookmarks_for_", cache, "settings_", bookmark, ".rds", sep = "")
    bookmarked_settings <- s3readRDS(bookmarked_settings, s3_bucket, check_region=FALSE)
    updateSelectInput(session, "column_names", selected = bookmarked_settings[["column_names"]])
    updateSliderInput(session, "significant_p", value = bookmarked_settings[["significant_p"]])
    updateSliderInput(session, "non_significant_p", value = bookmarked_settings[["non_significant_p"]])
    updateCheckboxInput(session, "v_from_p", value = bookmarked_settings[["v_from_p"]])
    updateCheckboxInput(session, "v_outliers", value = bookmarked_settings[["v_outliers"]])
    updateSliderInput(session, "v_outliers_threshold", value = bookmarked_settings[["v_outliers_threshold"]])
    updateCheckboxInput(session, "impute_v", value = bookmarked_settings[["impute_v"]])
    for (this_attribute in attributes_df$encoded_attribute) {
      updateSelectInput(session, this_attribute, selected = bookmarked_settings[[this_attribute]])
    }
    for (this_outcome in encoded_outcomes) {
      updateCheckboxInput(session, this_outcome, value = bookmarked_settings[[this_outcome]])
    }
  }




  ###################################################################################################
  # Reactive components
  ###################################################################################################




  # A list of user inputs, which is used to identify caches and bookmarks.
  settings <- reactive({
    all_inputs <- reactiveValuesToList(input)
    if("go" %in% names(all_inputs)) all_inputs$go <- NULL
    if("meta_regression_go" %in% names(all_inputs)) all_inputs$meta_regression_go <- NULL
    if("make_bookmark" %in% names(all_inputs)) all_inputs$make_bookmark <- NULL
    all_inputs <- all_inputs[order(names(all_inputs))]
    return(all_inputs)
  })




  # Reactive values
  rv <- reactiveValues()
  rv[["analysis_button"]] <- "subgroup_analysis"
  rv[["bookmark_link"]] <- ""
  rv[["read_data_from_cache"]] <- read_data_from_cache




  # Update the actions buttons when they are clicked (and also update rv[["analysis_button"]], which
  # is used in the get_results() function to switch between subgroup analysis and meta-regression.
  observeEvent(input$go, {
    updateActionButton(session, "go", label = "Update your analysis", icon = NULL)
  }, autoDestroy=TRUE)
  observeEvent(input$go, {
    rv[["analysis_button"]] <- "subgroup_analysis"
  })
  observeEvent(input$meta_regression_go, {
    rv[["analysis_button"]] <- "meta_regression"
  })




  get_df <- reactive({

    # Revert to the original df.
    df <- original_df

    # Invert the response ratio, if lower values are "better" (based on user input)
    for (i in 1:length(outcomes)) {
      if (!is.null(input[[paste(encoded_outcomes[i])]])) {
        if (input[[paste(encoded_outcomes[i])]] == TRUE) {
          df$lower_is_better[df$outcome == outcomes[i]] <- TRUE
        }
      }
    }
    df$response_ratio[df$lower_is_better == TRUE] <- 1 / df$response_ratio[df$lower_is_better == TRUE]
    df$log_response_ratio[df$lower_is_better == TRUE] <- log(df$response_ratio[df$lower_is_better == TRUE])

    # P-value from significance (using assumed p-values for comparisons reported as "significant" or "non-significant")
    df$p_from_significance <- lapply(df$selected_significance, function(x) if(is.na(x)) NA else if(x == "Significant") input$significant_p else input$non_significant_p)

    # Selected p-value
    df$selected_p <- df$p_from_significance
    df$selected_p[!is.na(df$p_value)] <- df$p_value[!is.na(df$p_value)]

    # Z-value from selected p-value
    df$z_from_p <- lapply(df$selected_p, function(x) if(is.na(x)) NA else qnorm(1 - (x/2)))

    # Selected z-value
    df$selected_z <- df$z_from_p
    df$selected_z[!is.na(df$z_value)] <- df$z_value[!is.na(df$z_value)]

    # Variance from z-value and log response ratio
    get_v_from_z <- function(x) {
      if(!is.na(x$selected_z) & !is.na(x$log_response_ratio) & x$log_response_ratio != 0) {
        (x$log_response_ratio / x$selected_z) ^ 2
      } else {
        NA
      }
    }
    df$v_from_z <- apply(df, 1, get_v_from_z)

    # Selected variance
    if (input[["v_from_p"]]) {
      df$selected_v <- df$v_from_z
      df$selected_v[!is.na(df$v_from_sd_and_n)] <- df$v_from_sd_and_n[!is.na(df$v_from_sd_and_n)]
    } else {
      df$selected_v <- df$v_from_sd_and_n
    }
    df$selected_v[is.infinite(df$selected_v)] <- NA
    df$selected_v[df$selected_v == 0] <- NA

    # Define independent data points "studies" within publications, based on user input.
    df$study <- paste("Study ID ", df$study_id, sep = "")
    for (column in input[["column_names"]]) {
      df$study <- paste(df$study, df[[column]], sep = "-")
    }
    df$study <- factor(df$study)

    # Rows with log_response_ratio and selected_variance
    df$es_and_v <- !is.na(df$log_response_ratio) & !is.na(df$selected_v)

    # Remove outliers, based on the Median Absolute Deviation (MAD)
    # Leys et al. (2013) Detecting outliers: Do not use standard deviation around the mean,
    # use absolute deviation around the median. Journal of Experimental Social Psychology 49(4):
    # 764-766. https://doi.org/10.1016/j.jesp.2013.03.013
    if (input[["v_outliers"]]) {
      selected_v_mad <- mad(df$selected_v, na.rm=TRUE)
      df$selected_v_mads <- (abs(df$selected_v - median(df$selected_v, na.rm=TRUE))) / selected_v_mad
      df$es_and_v[df$selected_v_mads > input$v_outliers_threshold] <- FALSE  # Here we define outliers as being more than [[input]] deviations from the median
    }

    # Imputed variance
    if (input[["impute_v"]]) {
      df_with_v <- df[!is.na(df$selected_v), ]
      if(length(df_with_v$selected_v) > 1) {
        imputation <- lme(selected_v ~ 1, data = df_with_v, random = ~ 1 | publication/study)
        imputed_v <- imputation$fitted[1]
        df$selected_v[is.na(df$selected_v)] <- imputed_v
        df$es_and_v <- !is.na(df$log_response_ratio) & !is.na(df$selected_v)
        if (input[["v_outliers"]]) {
          selected_v_mad <- mad(df$selected_v, na.rm=TRUE)
          if (selected_v_mad > 0) {
            df$selected_v_mads <- (abs(df$selected_v - median(df$selected_v, na.rm=TRUE))) / selected_v_mad
            df$es_and_v[df$selected_v_mads > input$v_outliers_threshold] <- FALSE  # Here we define outliers as being more than [[input]] deviations from the median
          }
        }
      }
    }

    # Study weights
    citations <- unique(df$citation)
    n_citations <- length(citations)
    encoded_citations <- gsub("[^[:alnum:]_]", "", citations)  # Delete non-alphanumeric characters (except underscores)
    for (i in 1:n_citations) {
      encoded_citation <- encoded_citations[i]
      if (encoded_citation %in% names(settings())) {
        relevance_weight <- input[[encoded_citation]]
      } else {
        relevance_weight <- 1
      }
      df$relevance_weight[df$citation == citations[i]] <- relevance_weight
    }

    # Round log_response_ratio and selected_v (for reproducible examples comparisons between this
    # analysis and analyses that use the data in the downloaded CSV file, which otherwise get rounded
    # to different numbers of digits).
    df$log_response_ratio <- round(df$log_response_ratio, 8)
    df$selected_v <- round(df$selected_v, 8)

    return(df)

  })  # End of get_df




  # This function deletes non-alphanumeric characters from a string ("x") and returns
  # a string ("pattern"), with the OR operator ("|") between terms
  # (for comparisons using grepl)
  get_filter <- function(input, variable) {
    # Delete non-alphanumeric characters from the input (e.g., replace "Barley and hairy vetch" with "Barleyandhairyvetch").
    pattern <- gsub("[^[:alnum:]]", "", input)
    # Add word boundaries ("\\b") and insert the "OR" operator ("|") between inputs. This stops the input "Barley" from matching "Barleyandhairyvetch".
    pattern <- paste("\\b", pattern, "\\b", collapse = "|", sep = "")
    pattern <- paste(pattern, collapse = "|", sep = "")
    # Delete non-alphanumeric characters from the data (e.g., replace "Barley and hairy vetch" with "Barleyandhairyvetch").
    x <- lapply(variable, function(x) gsub("[^[:alnum:]]", "", x))
    # Return a list of rows where the pattern is matched.
    return(list(pattern = pattern, x = x))
  }




  # After filtering the data, update the filters (remove choices that are not in the data).
  update_filters <- function(df) {
    for (i in 1:length(attributes_df$attribute)) {
      if(attributes_df$type[i] == "factor") {
        selected <- input[[paste(attributes_df$encoded_attribute[i])]]
        choices <- sort(unique(unlist(df[[paste(attributes_df$attribute[i])]])))
        updateSelectInput(session, paste(attributes_df$encoded_attribute[i]), selected = selected, choices = choices)
      } else {
        #selected_min <- min(as.numeric(unlist(df[[paste(attributes_df$attribute[i])]])), na.rm = TRUE)
        #selected_max <- max(as.numeric(unlist(df[[paste(attributes_df$attribute[i])]])), na.rm = TRUE)
        #value <- c(selected_min, selected_max)
        #updateSliderInput(session, paste(attributes_df$encoded_attribute[i]), value = value)
      }
    }
  }




  # This function gets a subset of the data, based on the filters that the user has selected.
  get_data <- reactive({
    df <- get_df()
    for (i in 1:length(attributes_df$attribute)) {
      # Subset by attribute
      if (!is.null(input[[paste(attributes_df$encoded_attribute[i])]])) {
        if (attributes_df$type[i] == "factor") {
          filter <- get_filter(input[[paste(attributes_df$encoded_attribute[i])]], df[[paste(attributes_df$attribute[i])]])
          df <- subset(df, grepl(filter$pattern, filter$x))
        } else {  # attributes_df$type[i] == "number"
          min <- min(as.numeric(unlist(attributes_df$options[i])), na.rm = TRUE)
          max <- max(as.numeric(unlist(attributes_df$options[i])), na.rm = TRUE)
          selected_min <- input[[paste(attributes_df$encoded_attribute[i])]][1]
          selected_max <- input[[paste(attributes_df$encoded_attribute[i])]][2]
          if (selected_min != min | selected_max != max) {
            df <- df[!is.na(df[[paste(attributes_df$attribute[i])]]), ]
            df <- df[as.numeric(df[[paste(attributes_df$attribute[i])]]) >= selected_min, ]
            df <- df[as.numeric(df[[paste(attributes_df$attribute[i])]]) <= selected_max, ]
          }
        }
      }
    }
    update_filters(df)
    return(df)
  })




  # In the df, some columns are lists, which are useful in some parts of this code, but not in others.
  # Here we flatten these lists. We also encode the column names and factor levels using gsub(), for
  # use in the meta-regression model and in write.csv().
  get_encoded_df <- function(df) {
    encoded_df <- df

    # Delete spaces and other non-alphanumeric characters from column names (so that they can be
    # used as moderators in the meta-regression model). These must match the encoded_attributes.
    encoded_column_names <- gsub("[^[:alnum:]._]", ".", colnames(encoded_df))
    colnames(encoded_df) <- encoded_column_names

    # Replace NAs with empty strings (so that NAs will be included as an additional
    # factor level for categorical variables, rather than being excluded from the model).
    encoded_df[is.na(encoded_df)] <- ""

    # Encode the categorical variables.
    factors <- attributes_df$encoded_attribute[attributes_df$type == "factor"]
    for (i in 1:length(factors)) {
      this_factor <- factors[i]
      # Delete the "&" symbol from list items. We will use the "&" symbol when flattening lists.
      encoded_df[[this_factor]] <- lapply(encoded_df[[this_factor]], function(x) gsub("&", "and", x))
      # Sort the records alphabetically.
      encoded_df[[this_factor]] <- lapply(encoded_df[[this_factor]], sort)
      # Paste and unlist, with records separated by " & ".
      encoded_df[[this_factor]] <- lapply(encoded_df[[this_factor]], function(x) paste(unlist(x), collapse = " & "))
      encoded_df[[this_factor]] <- unlist(encoded_df[[this_factor]])
      # Refactor.
      encoded_df[[this_factor]] <- as.factor(encoded_df[[this_factor]])
    }
    # Encode publication and study (the random effects) as factors.
    encoded_df$publication <- as.factor(encoded_df$publication)
    encoded_df$study <- as.factor(encoded_df$study)

    # Encode the continuous variables as numeric (so that they are not treated as categorical
    # variables, and so that NAs are excluded from the model for numeric moderators).
    numbers <- attributes_df$encoded_attribute[attributes_df$type == "number"]
    for (i in 1:length(numbers)) {
      this_number <- numbers[i]
      encoded_df[[this_number]][encoded_df[[this_number]] == ""] <- NA
      encoded_df[[this_number]] <- as.numeric(encoded_df[[this_number]])
    }

    return(encoded_df)
  }




  # This function gets the results of a meta-analysis (of the subset of data rows with values for
  # both the effect size ("es") and its variance ("v").
  get_results <- eventReactive(c(input$go, input$meta_regression_go), ignoreInit = T, {
    withProgress(message="Analysing data...", value=0.5, {
      data <- get_data()
      d <- subset(data, es_and_v == TRUE)
      n_rows <- length(d$es_and_v)
      n_publications <- length(unique(d$publication))
      n_citations <- length(unique(d$citation))
      read_data_from_cache <- rv[["read_data_from_cache"]]
      cached_results <- paste(cache, "results_", digest(c(settings(), rv[["analysis_button"]])), ".rds", sep = "")
      if (read_data_from_cache == TRUE & head_object(cached_results, s3_bucket, check_region=FALSE)) {
        results <- s3readRDS(cached_results, s3_bucket, check_region=FALSE)
      } else {
        if (n_rows > 0) {

          ###############
          # Meta-analysis
          ###############

          # We will compare the results of the subgroup analysis with an analysis of the full dataset.
          df <- get_df()
          df <- subset(df, es_and_v == TRUE)
          df_n_rows <- length(df$es_and_v)
          df_n_publications <- length(unique(df$publication))
          df_n_citations <- length(unique(df$citation))
          if (df_n_rows > 1) {
            meta_analysis <- rma.mv(yi = log_response_ratio, V = selected_v, random = ~ 1 | publication/study, data = df)
            supergroup_results <- list(
              effect_size = as.numeric(round(exp(meta_analysis$b), 2)),  # Effect size = response ratio
              ci.lb = round(exp(meta_analysis$ci.lb), 2),  # Lower bound of the confidence interval
              ci.ub = round(exp(meta_analysis$ci.ub), 2)   # Upper bound of the confidence interval
            )
          } else {
            supergroup_results <- list(
              effect_size = as.numeric(round(exp(df$log_response_ratio[1]), 2)),  # Effect size = response ratio
              ci.lb = round(exp(df$log_response_ratio[1] - (1.96 * sqrt(df$selected_v[1]))), 2),
              ci.ub = round(exp(df$log_response_ratio[1] + (1.96 * sqrt(df$selected_v[1]))), 2)
            )
          }

          ###################
          # Subgroup analysis
          ###################

          # If there are at least two data points in the subgroup, we do the analysis.
          if (n_rows > 1) {

            # We fit the default model (with inverse-variance weights, without relevance weights).
            subgroup_analysis <- rma.mv(yi = log_response_ratio, V = selected_v, random = ~ 1 | publication/study, data = d)

            ###################
            # Relevance weights
            ###################

            # M is the variance-covariance matrix from the default model. The inverse of M is the
            # default weight matrix: weights(subgroup_analysis, type = "matrix").
            M <- subgroup_analysis$M
            # C is a diagonal matrix of user-defined relevance weights.
            C <- diag(d$relevance_weight)
            # Here we modify the default weight matrix (solve(M)) by multiplying it by C.
            W <- sqrt(C) %*% solve(M) %*% sqrt(C)
            # Then we fit a new model with this modified weight matrix.
            subgroup_analysis <- rma.mv(yi = log_response_ratio, V = selected_v, W = W, random = ~ 1 | publication/study, data = d)
            subgroup_analysis_summary <- summary(subgroup_analysis)

            # Results of the subgroup analysis
            log_response_ratio <- subgroup_analysis$b
            log_response_ratio_se <- subgroup_analysis$se
            effect_size <- as.numeric(round(exp(subgroup_analysis$b), 2))  # Effect size = response ratio
            ci.lb <- round(exp(subgroup_analysis$ci.lb), 2)                # Lower bound of the confidence interval
            ci.ub <- round(exp(subgroup_analysis$ci.ub), 2)                # Upper bound of the confidence interval
            pval <- subgroup_analysis$pval
            pval <- round(pval, 4)
            QE <- round(subgroup_analysis$QE)
            QEp <- round(subgroup_analysis$QEp, 2)
            if (QEp == 0) QEp <- 0.0001
          } else if (n_rows == 1) {
            subgroup_analysis_summary <- NULL
            log_response_ratio <- d$log_response_ratio[1]
            log_response_ratio_se <- sqrt(d$selected_v[1])
            effect_size <- as.numeric(round(exp(d$log_response_ratio[1]), 2))  # Effect size = response ratio
            ci.lb <- exp(d$log_response_ratio[1] - (1.96 * sqrt(d$selected_v[1])))
            ci.lb <- round(ci.lb, 2)
            ci.ub <- exp(d$log_response_ratio[1] + (1.96 * sqrt(d$selected_v[1])))
            ci.ub <- round(ci.ub, 2)
            zval <- abs(d$log_response_ratio[1] / sqrt(d$selected_v[1]))
            pval <- 2 * (1 - pnorm(zval))
            pval <- round(pval, 4)
            QE <- NA
            QEp <- NA
          }
          # Text for the paragraph
          if (effect_size > 1) {
            direction <- "positive"
            percent <- paste(round(abs(1 - effect_size) * 100), "% higher", sep = "")
          } else if (effect_size < 1) {
            direction <- "negative"
            percent <- paste(round(abs(1 - effect_size) * 100), "% lower", sep = "")
          } else {
            direction <- "neutral"
            percent <- "0% different"
          }
          if (ci.lb < 1) {
            lower_percent <- paste(round(abs(1 - ci.lb) * 100), "% lower", sep ="")
          } else if (ci.lb > 1) {
            lower_percent <- paste(round(abs(1 - ci.lb) * 100), "% higher", sep ="")
          } else {
            lower_percent <- "0% different"
          }
          if (ci.ub < 1) {
            upper_percent <- paste(round(abs(1 - ci.ub) * 100), "% lower", sep ="")
          } else if (ci.ub > 1) {
            upper_percent <- paste(round(abs(1 - ci.ub) * 100), "% higher", sep ="")
          } else {
            upper_percent <- "0% different"
          }
          if (n_rows > 1) {
            results_df <- data.frame(
              citation = d$citation,
              effect_size = exp(subgroup_analysis$yi),  # Effect size = response ratio
              ci.lb = exp(subgroup_analysis$yi - 1.96 * sqrt(subgroup_analysis$vi)),
              ci.ub = exp(subgroup_analysis$yi + 1.96 * sqrt(subgroup_analysis$vi)),
              log_response_ratio = subgroup_analysis$yi,  # The log response ratio (not the response ratio) for the funnel plot
              log_response_ratio_se = sqrt(subgroup_analysis$vi)  # Standard error of the log response ratio (not the response ratio) for the funnel plot
            )
          } else if (n_rows == 1) {
            results_df <- data.frame(
              citation = d$citation,
              effect_size = exp(d$log_response_ratio[1]),  # Effect size = response ratio
              ci.lb = exp(d$log_response_ratio[1] - (1.96 * sqrt(d$selected_v[1]))),
              ci.ub = exp(d$log_response_ratio[1] + (1.96 * sqrt(d$selected_v[1]))),
              log_response_ratio = d$log_response_ratio[1],
              log_response_ratio_se = sqrt(d$selected_v[1])  # Standard error of the log response ratio (not the response ratio) for the funnel plot
            )
          }
          results <- list(subgroup_analysis_summary = subgroup_analysis_summary, effect_size = effect_size, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, QE = QE, QEp = QEp, direction = direction, percent = percent, lower_percent = lower_percent, upper_percent = upper_percent, log_response_ratio = log_response_ratio, log_response_ratio_se = log_response_ratio_se, supergroup_results = supergroup_results, results_df = results_df, d = d, n_publications = n_publications, n_citations = n_citations, n_rows = n_rows)

          if (rv[["analysis_button"]] == "meta_regression") {
            if (df_n_rows > 1) {

              #################
              # Meta-regression
              #################

              # We fit a meta-regression model using the full dataset, not the filtered dataset, using
              # the selected filters (if any) as moderators.

              # First, we get the full data set (not the subset, which is used for subgroup analysis,
              # whereas the full data set is used for meta-regression) and we prepare it for use in
              # meta-regression.
              encoded_df <- get_encoded_df(df)

              # Second, we get the moderators (if the user has selected any filters).
              moderators_df <- attributes_df
              moderators_df$moderator_value <- 0
              moderators_df$moderator_level <- NA
              for (i in 1:length(moderators_df$attribute)) {
                if (!is.null(input[[paste(moderators_df$encoded_attribute[i])]])) {
                  this_input <- input[[paste(moderators_df$encoded_attribute[i])]]
                  this_attribute <- moderators_df$encoded_attribute[i]
                  if (moderators_df$type[i] == "factor") {
                    this_level <- paste(unlist(this_input), collapse = "OR")
                    moderators_df$moderator_level[i] <- paste(this_attribute, this_level, sep = "")
                    moderators_df$moderator_value[i] <- 1
                    # Use get_filter() to identify rows where this_input is in this_attribute. Note
                    # that this uses the lists in df, not the flattened lists in encoded_df.
                    filter <- get_filter(this_input, df[[paste(moderators_df$attribute[i])]])
                    encoded_df[[this_attribute]] <- as.character(encoded_df[[this_attribute]])
                    encoded_df[[this_attribute]][grepl(filter$pattern, filter$x)] <- this_level
                    encoded_df[[this_attribute]] <- as.factor(encoded_df[[this_attribute]])
                  } else {  # if (moderators_df$type[i] == "number")
                    # Check to see if the user has moved the sliders from their min or max values.
                    # Only use this attribute as a moderator if they have moved the sliders.
                    low_value <- input[[paste(moderators_df$encoded_attribute[i])]][1]
                    high_value <- input[[paste(moderators_df$encoded_attribute[i])]][2]
                    if (low_value != moderators_df$min[i] | high_value != moderators_df$max[i]) {
                      this_value <- (low_value + high_value) / 2  # Use the mean value for model predictions.
                      moderators_df$moderator_value[i] <- this_value
                      moderators_df$moderator_level[i] <- this_attribute
                    }
                  }
                }
              }
              moderators_df <- subset(moderators_df, moderator_value > 0)
              moderators <- moderators_df$encoded_attribute

              # Third, if there are any moderators, we create a model formula from these moderators.
              n_moderators <- length(moderators)
              if (n_moderators > 0) {
                # If there is more than one moderator, we get all pairwise interactions.
                if (n_moderators > 1) {
                  moderator_combinations <- combn(moderators, 2)
                  for (i in 1:dim(moderator_combinations)[2]) {
                    this_interaction <- paste(moderator_combinations[1,i], moderator_combinations[2,i], sep = ":")
                    if (i == 1) {
                      moderator_interactions <- this_interaction
                    } else {
                      moderator_interactions <- c(paste(this_interaction), moderator_interactions)
                    }
                  }
                  # If there is more than one moderator, we try to fit a model with all pairwise
                  # interactions between these moderators.
                  mods <- paste(paste(moderators, collapse="+"), "+", paste(moderator_interactions, collapse="+"))
                } else {
                  # If there is only one moderator, we try to fit a model with no interactions.
                  mods <- paste(moderators, collapse="+")
                }
                print(mods)

                start_time <- Sys.time()

                # Fourth, we fit the meta-regression model, using MuMIn for automated model selection.
                meta_regression_dredge <- dredge(
                  rma.mv(log_response_ratio, selected_v, method = "ML",  # ML is needed for log-likelihood comparisons, but we will refit with REML below.
                    mods = as.formula(paste(" ~ ", mods)),
                    random = ~ 1 | publication/study,
                    data = encoded_df
                  ),
                  trace = 2  # Show progress bar in R console.
                )
                print(subset(meta_regression_dredge, delta <= 2, recalc.weights=FALSE))
                # Select the "best" model.
                meta_regression <- get.models(meta_regression_dredge, subset = 1, method = "REML")[[1]]
                meta_regression_summary <- summary(meta_regression)

                # Fifth, we get the model predictions for the moderator levels that the user selected.
                # The names of the moderators (including dummy variables for categorical moderators,
                # which are formatted as "ModeratorALevelX" (e.g., "CovercroptypeLegume") and also
                # including interaction terms, which are formatted as
                # "ModeratorAFilterX:ModeratorBFilterY"
                # (e.g., "CovercroptypeLegume:NitrogenfertilizedcashcropNo")).
                names <- names(coef(meta_regression))
                names <- names[-1]  # Delete the intercept, which will automatically be added by the predict function.

                # If there is at least one moderator in the selected model
                if (length(names) > 0) {
                  # Set newmods to zero (i.e. the intercept of the model). We will modify this below.
                  newmods <- c(rep(0, length(names)))
                  print(newmods)

                  for (i in 1:length(moderators_df$moderator_level)) {  # For each filter that the user has selected
                    this_filter <- moderators_df$moderator_level[i]
                    pattern_1 <- paste(":", this_filter, "$", sep = "")
                    pattern_2 <- paste("^", this_filter, ":", sep = "")
                    for (j in 1:length(names)) {  # For each name
                      this_name <- names[j]
                      # If this filter is equal to this name
                      if (this_filter == this_name) {
                        # Set the value for use in predict(meta_regression, newmods = newmods)
                        this_new_mod <- moderators_df$moderator_value[i]
                        newmods[j] <- this_new_mod
                      }
                      # Else if this filter is in this name (but not equal to it, because it is an interaction)
                      else if (grepl(pattern_1, names[j]) | grepl(pattern_2, names[j])) {
                        other_moderators_df <- moderators_df[-i,]  # moderators_df minus the row for this filter
                        for (k in 1:length(other_moderators_df$moderator_level)) {  # For each other filter
                          this_other_filter <- other_moderators_df$moderator_level[k]
                          pattern_3 <- paste(":", this_other_filter, "$", sep = "")
                          pattern_4 <- paste("^", this_other_filter, ":", sep = "")
                          # If one other filter is also in this name (an interaction with two of our filters)
                          if (grepl(pattern_3, names[j]) | grepl(pattern_4, names[j])) {
                            # Set the value for use in predict(meta_regression, newmods = newmods)
                            this_new_mod <- moderators_df$moderator_value[i]
                            this_other_new_mod <- other_moderators_df$moderator_value[k]
                            newmods[j] <- this_new_mod * this_other_new_mod  # Interaction = new_mod * new_mod
                          }
                        }
                      }
                    }
                  }
                  print(newmods)
                  # Meta-regression results with moderators
                  meta_regression_results <- predict(meta_regression, newmods = newmods)
                # Else if there is not at least one moderator in the selected model
                } else {
                  # Meta-regression results without moderators
                  meta_regression_results <- predict(meta_regression)
                  newmods <- ""
                }
                print(meta_regression_results)

                # Meta-regression results
                log_response_ratio <- meta_regression_results$pred
                log_response_ratio_se <- meta_regression_results$se
                zval <- abs(log_response_ratio / log_response_ratio_se)
                pval <- 2 * (1 - pnorm(zval))
                pval <- round(pval, 4)
                effect_size <- round(exp(meta_regression_results$pred), 2)
                ci.lb <- round(exp(meta_regression_results$ci.lb), 2)
                ci.ub <- round(exp(meta_regression_results$ci.ub), 2)
                QE <- round(meta_regression$QE)
                QEp <- round(meta_regression$QEp, 2)
                if (QEp == 0) QEp <- 0.0001
                # Text for the paragraph
                if (effect_size > 1) {
                  direction <- "positive"
                  percent <- paste(round(abs(1 - effect_size) * 100), "% higher", sep = "")
                } else if (effect_size < 1) {
                  direction <- "negative"
                  percent <- paste(round(abs(1 - effect_size) * 100), "% lower", sep = "")
                } else {
                  direction <- "neutral"
                  percent <- "0% different"
                }
                if (ci.lb < 1) {
                  lower_percent <- paste(round(abs(1 - ci.lb) * 100), "% lower", sep ="")
                } else if (ci.lb > 1) {
                  lower_percent <- paste(round(abs(1 - ci.lb) * 100), "% higher", sep ="")
                } else {
                  lower_percent <- "0% different"
                }
                if (ci.ub < 1) {
                  upper_percent <- paste(round(abs(1 - ci.ub) * 100), "% lower", sep ="")
                } else if (ci.ub > 1) {
                  upper_percent <- paste(round(abs(1 - ci.ub) * 100), "% higher", sep ="")
                } else {
                  upper_percent <- "0% different"
                }

                # Which moderators were included in the model?
                moderators_df$mod <- NA
                meta_regression_formula <- as.character(meta_regression$formula.mods)[2]
                for(i in 1:length(moderators)) {
                  pattern <- paste("\\b", moderators[i], "\\b", sep = "")
                  moderators_df$mod[i] <- grepl(pattern, meta_regression_formula)
                }

                # Create text for output to the "Model summaries" tab.
                # If any moderators were included
                if (TRUE %in% moderators_df$mod) {
                  meta_regression_formula <- paste('rma.mv(log_response_ratio, selected_v, mods = ~ ', meta_regression_formula, ', random = ~ 1 | publication/study)', sep = "")
                  meta_regression_predict <- paste('predict(model, newmods = c(', paste(newmods, collapse = ","), '))', sep = "")
                  # Else if no moderators were included in the model
                } else {
                  meta_regression_formula <- paste('rma.mv(log_response_ratio, selected_v, random = ~ 1 | publication/study)', sep = "")
                  meta_regression_predict <- paste('predict(model)', sep = "")
                }

                # Save the results.
                meta_regression_results <- list(meta_regression_summary = meta_regression_summary, meta_regression_formula = meta_regression_formula, meta_regression_results = meta_regression_results, meta_regression_predict = meta_regression_predict, moderators_df = moderators_df, effect_size = effect_size, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, QE = QE, QEp = QEp, direction = direction, percent = percent, lower_percent = lower_percent, upper_percent = upper_percent, n_publications = df_n_publications, n_citations = df_n_citations, n_rows = df_n_rows)
                results$meta_regression_results <- meta_regression_results

                finish_time <- Sys.time()
                print(finish_time - start_time)

              } else {  # if (n_moderators == 0)
                # No filters selected. Please select one or more filters.
              }
            } else {  # if (n_rows <= 1)
              # Not enough data. Please use fewer filters.
            }
          }  # End of meta-regression
          # Save the results to the cache.
          if (save_data_to_cache == TRUE) {
            s3saveRDS(results, object = cached_results, s3_bucket, check_region=FALSE)
          }
        } else {  # if (n_rows == 0)
          results <- NA
        }
      }
    })  # End of withProgress
    return(results)
  })




  get_results_by_study <- reactive({
    results <- get_results()
    if (!is.na(results)) {
      d <- results$d
      n_citations <- results$n_citations
      citations <- unique(d$citation)
      results_by_study_df <- data.frame(matrix(nrow = n_citations, ncol = 8))
      colnames(results_by_study_df) <- c("citation", "effect_size", "ci.lb", "ci.ub", "log_response_ratio", "log_response_ratio_se", "relevance_weight", "paragraph")
      for (i in 1:n_citations) {
        this_citation <- citations[i]
        di <- subset(d, citation == this_citation)
        n_rows <- length(di$es_and_v)
        if (n_rows > 0) {
          results_by_study_df$citation[i] <- this_citation
          results_by_study_df$relevance_weight[i] <- mean(di$relevance_weight)

          get_countries <- function() {
            if (length(countries) > 1) {
              countries <- sort(countries)
              result <- "multiple countries ("
              for (i in 1:length(countries)) {
                if (i == 1) {
                  result = paste(result, countries[i], sep = "")
                } else if (i == length(countries)) {
                  result = paste(result, countries[i], sep = " and ")
                  result = paste(result, ")", sep = "")
                } else {
                  result = paste(result, countries[i], sep = ", ")
                }
              }
            } else {
              plural_countries <- c("Gambia", "Netherlands", "United Kingdom of Great Britain and Northern Ireland", "United States of America")
              if (countries[1] %in% plural_countries) {
                result = paste("the", countries[1], sep = " ")
              } else {
                result = countries[1]
              }
            }
            return(result)
          }
          countries <- unique(unlist(di$Country))
          country <- if(!is.null(countries)) { get_countries() }

          get_designs <- function() {
            if (length(designs) > 1) {
              for (i in 1:length(designs)) {
                if (i == 1) {
                  result = designs[i]
                } else {
                  result = paste(result, designs[i], sep = ", ")
                }
              }
            } else {
              result = designs[1]
            }
            return(tolower(result))
          }
          designs <- unique(unlist(di$Design))
          design <- if(!is.null(designs)) { get_designs() }

          location <- unique(di$Location)
          methods_text <- unique(unlist(di$Methods))
          if (n_rows > 1) {
            subgroup_analysis <- rma.mv(yi = log_response_ratio, V = selected_v, random = ~ 1 | study, data = di)
            effect_size <- as.numeric(round(exp(subgroup_analysis$b), 2))  # Response ratio
            ci.lb <- round(exp(subgroup_analysis$ci.lb), 2)                # Lower bound of the confidence interval
            ci.ub <- round(exp(subgroup_analysis$ci.ub), 2)                # Upper bound of the confidence interval
            log_response_ratio <- round(subgroup_analysis$b, 2)            # The log response ratio (not the response ratio) for the funnel plot
            log_response_ratio_se <- round(subgroup_analysis$se, 2)        # Standard error of the log response ratio (not the response ratio) for the funnel plot
          } else if (n_rows == 1) {
            effect_size <- as.numeric(round(exp(di$log_response_ratio[1]), 2))
            ci.lb <- round(exp(di$log_response_ratio[1] - (1.96 * sqrt(di$selected_v[1]))), 2)
            ci.ub <- round(exp(di$log_response_ratio[1] + (1.96 * sqrt(di$selected_v[1]))), 2)
            log_response_ratio <- round(di$log_response_ratio[1], 2)
            log_response_ratio_se <- round(sqrt(di$selected_v[1]), 2)
          }
          results_by_study_df$effect_size[i] <- effect_size
          results_by_study_df$ci.lb[i] <- ci.lb
          results_by_study_df$ci.ub[i] <- ci.ub
          results_by_study_df$log_response_ratio[i] <- log_response_ratio
          results_by_study_df$log_response_ratio_se[i] <- log_response_ratio_se
          if (effect_size > 1) {
            direction <- "positive"
            percent <- paste(round(abs(1 - effect_size) * 100), "% higher", sep = "")
          } else if (effect_size < 1) {
            direction <- "negative"
            percent <- paste(round(abs(1 - effect_size) * 100), "% lower", sep = "")
          } else {
            direction <- "neutral"
            percent <- "0% different"
          }
          if (ci.lb < 1) {
            lower_percent <- paste(round(abs(1 - ci.lb) * 100), "% lower", sep ="")
          } else if (ci.lb > 1) {
            lower_percent <- paste(round(abs(1 - ci.lb) * 100), "% higher", sep ="")
          } else {
            lower_percent <- "0% different"
          }
          if (ci.ub < 1) {
            upper_percent <- paste(round(abs(1 - ci.ub) * 100), "% lower", sep ="")
          } else if (ci.ub > 1) {
            upper_percent <- paste(round(abs(1 - ci.ub) * 100), "% higher", sep ="")
          } else {
            upper_percent <- "0% different"
          }
          this_paragraph_header <- paste(
            "<span class='bold'>", this_citation, "</span><br /><br />",
            "This intervention: <span class='italic'>", di$intervention[1], "</span><br /><br />",
            "This outcome: <span class='italic'>", di$outcome[1], "</span><br /><br />",
            sep = ""
          )
          this_paragraph <- paste(
            "Based on <span class='bold'>", n_rows, " data point", if (n_rows > 1) "s", "</span>
            from a ", if(!is.null(design)) paste("<span class='bold'>", design, "</span>",
            sep = ""), " study ", if (!is.na(location)) location else if(!is.null(country))
            paste("in <span class='bold'>", country, "</span>"), " (", if (this_citation != "")
            this_citation else "[citation not available]", ") this outcome was <span
            class='bold'>", percent, " with this intervention</span> than it was without it (between ",
            lower_percent, " and ", upper_percent, ", based on the 95% confidence interval). <span
            class='bold'> Methods: </span>", if (methods_text != "") methods_text else
            paste("[METHODS NA]"),
            sep = ""
          )
          this_paragraph <- paste(this_paragraph_header, this_paragraph, "<br /><br /><hr /><br />", sep = "")
          results_by_study_df$paragraph[i] <- this_paragraph
        }
      }
      results_by_study_df$encoded_citation <- gsub("[^[:alnum:]_]", "", results_by_study_df$citation)  # Delete non-alphanumeric characters (except underscores)
      return(results_by_study_df)
    } else {  # if (is.na(results))
      return(NA)
    }
  })








  output$paragraph <- renderUI({
    results <- get_results()
    if (!is.na(results)) {
      n_rows <- results$n_rows
      n_citations <- results$n_citations
      n_publications <- results$n_publications
      effect_size <- results$effect_size
      ci.lb <- results$ci.lb
      ci.ub <- results$ci.ub
      pval <- results$pval
      QE <- results$QE
      QEp <- results$QEp
      direction <- results$direction
      percent <- results$percent
      lower_percent <- results$lower_percent
      upper_percent <- results$upper_percent
      HTML(paste(
        "For your data, this outcome was <span class='bold'>", percent, " with this intervention
        </span> than it was without it (response ratio = ", effect_size, "). This effect was ",
        if (pval >= 0.05) "<span class='red bold'>not " else "<span class='bold'>", "statistically
        significant (P = ", format(pval), ")</span>. This outcome could have been <span class='bold'>
        between ", lower_percent, " and ", upper_percent, "</span> with this intervention than it
        would have been without it (", ci.lb, " &#8804; response ratio &#8804; ", ci.ub, " in the 95%
        confidence interval). Please also see the &quot;Value judgements&quot; tab.
        <br /><br />
        This subgroup analysis included <span class='bold'>", n_rows, " data point", if (n_rows > 1)
        "s", " from ", n_citations, if (n_citations > 1) " studies" else " study", " in ",
        n_publications, " publication", if (n_publications > 1) "s", ". </span>",
        if (n_rows > 1) {
          paste(
            "There was ", if (QEp >= 0.05) "not ", "significant heterogeneity between these data
            points (Q = ", QE, ", P = ", format(QEp), "). ", sep = ""
          )
        },
        "<span class='hidden' id='effect_size'>", effect_size, "</span>",
        "<span class='hidden' id='pval'>", pval, "</span>",
        "<span class='hidden' id='lb'>", ci.lb, "</span>",
        "<span class='hidden' id='ub'>", ci.ub, "</span>",
        "<span class='hidden' id='intervention_pk'>", intervention_pk, "</span>",
        "<span class='hidden' id='outcome_pk'>", outcome_pk, "</span>",
        "<span class='hidden' id='api_query_string'>", api_query_string, "</span>",
        "<span class='hidden' id='user_settings'>", digest(settings()), "</span>",
        sep = ""
      ))
    } else {  # if (is.na(results))
      HTML("<span class='red'>No data. Please use fewer filters.</span>")
    }
  })



  output$meta_regression_paragraph <- renderUI({
    results <- get_results()
    if (!is.na(results)) {
      if (!is.null(results$meta_regression_results)) {
        results <- results$meta_regression_results
        n_rows <- results$n_rows
        n_citations <- results$n_citations
        n_publications <- results$n_publications
        effect_size <- results$effect_size
        ci.lb <- results$ci.lb
        ci.ub <- results$ci.ub
        pval <- results$pval
        QE <- results$QE
        QEp <- results$QEp
        direction <- results$direction
        percent <- results$percent
        lower_percent <- results$lower_percent
        upper_percent <- results$upper_percent
        moderators_df <- results$moderators_df
        moderators_text <- "<br />"
        for (i in 1:length(moderators_df$mod)) {
          if (moderators_df$mod[i] != TRUE) {
            this_text <- paste("<span class='bold red'>Warning: &quot;", moderators_df$attribute[i],
              "&quot; did not have a significant effect on this outcome. You could consider removing
              this filter from your subgroup analysis.</span><br /><br />", sep = "")
            moderators_text <- paste(moderators_text, this_text)
          }
        }
        HTML(paste(
          "For your data, this outcome was <span class='bold'>", percent, " with this intervention
          </span> than it was without it (response ratio = ", effect_size, "). This effect was ",
          if (pval >= 0.05) "<span class='red bold'>not " else "<span class='bold'>", "statistically
          significant (P = ", format(pval), ")</span>. This outcome could have been <span class='bold'>
          between ", lower_percent, " and ", upper_percent, "</span> with this intervention than it
          would have been without it (", ci.lb, " &#8804; response ratio &#8804; ", ci.ub, " in the 95%
          confidence interval).
          <br /><br />
          This meta-regression included <span class='bold'>", n_rows, " data point", if (n_rows > 1)
            "s", " from ", n_citations, if (n_citations > 1) " studies" else " study", " in ",
          n_publications, " publication", if (n_publications > 1) "s", ". </span>",
          if (n_rows > 1) {
            paste(
              "There was ", if (QEp >= 0.05) "not ", "significant heterogeneity between these data
              points (Q = ", QE, ", P = ", format(QEp), "). ", sep = ""
            )
          },
          paste("<br /><br />"),
          moderators_text,
          sep = ""
        ))
      }
    }
  })




  output$forest_plot <- renderImage({
    results <- get_results()
    results_by_study <- get_results_by_study()
    if (!is.na(results)) {
      results_df <- data.frame(
        citation = "Mean effect size",
        effect_size = results$effect_size,
        ci.lb = results$ci.lb,
        ci.ub = results$ci.ub,
        log_response_ratio = results$log_response_ratio,
        log_response_ratio_se = results$log_response_ratio_se,
        relevance_weight = "",
        paragraph = "",
        encoded_citation = "",
        shape = 18,  # Diamond
        size = 12
      )
      if (!is.na(results_by_study)) {
        results_by_study_df <- results_by_study
        results_by_study_df$shape <- 16  # Circle
        results_by_study_df$size <- 4
        results_by_study_df <- rbind(results_by_study_df, results_df)
        results_by_study_df$citation <- reorder(results_by_study_df$citation, c(length(results_by_study_df$citation):1))
        n_rows <- length(results_by_study_df$effect_size)
        p <- ggplot(data=results_by_study_df, aes(x=citation, y=effect_size, ymin=ci.lb, ymax=ci.ub)) +
          geom_pointrange(shape=results_by_study_df$shape, fatten=results_by_study_df$size) +
          geom_hline(yintercept=1, lty=2) +
          coord_flip() +
          xlab("Study") + ylab("Response ratio") +
          theme_bw(base_size = 10) +
          theme(
            plot.background = element_rect(fill = "#f5f5f5", color = "#e3e3e3", size = 0.5),
            text = element_text(family = "Noto Serif")
          )
        outfile <- tempfile(fileext='.svg')
        height <- (length(results_by_study_df$citation) + 2) / 4
        width <- 6
        ggsave(p, filename = outfile, height = height, width = width)
        list(src = normalizePath(outfile), contentType = 'image/svg+xml')
      }
    }
  }, deleteFile = TRUE)




  output$subgroup_analysis_plot <- renderImage({
    results <- get_results()
    if (!is.na(results)) {
      subgroup_df <- data.frame(
        citation = "Your data",
        effect_size = results$effect_size,
        ci.lb = results$ci.lb,
        ci.ub = results$ci.ub,
        shape = 18,  # Diamond
        size = 12
      )
      supergroup_df <- data.frame(
        citation = "All data",
        effect_size = results$supergroup_results$effect_size,
        ci.lb = results$supergroup_results$ci.lb,
        ci.ub = results$supergroup_results$ci.ub,
        shape = 18,  # Diamond
        size = 12
      )
      results_df <- rbind(subgroup_df, supergroup_df)
      p <- ggplot(data=results_df, aes(x=citation, y=effect_size, ymin=ci.lb, ymax=ci.ub)) +
        geom_pointrange(shape=results_df$shape, fatten=results_df$size) +
        geom_hline(yintercept=1, lty=2) +
        ylab("Response ratio") +
        coord_flip() +
        theme_bw(base_size = 9) +
        theme(
          plot.background = element_rect(fill = "#f5f5f5", color = "#e3e3e3", size = 0.5),
          axis.title.x=element_text(size = 9),
          axis.title.y=element_blank(),
          axis.text=element_text(size = 9),
          text = element_text(family = "Noto Serif"),
          plot.margin = unit(c(0.125,0.125,0.125,0.125), "in")
        )
      outfile <- tempfile(fileext='.svg')
      ggsave(p, filename = outfile, width = 3, height = 2)
      list(src = normalizePath(outfile), contentType = 'image/svg+xml')
    } else {
      outfile <- tempfile(fileext='.svg')
      list(src = normalizePath(outfile), contentType = 'image/svg+xml')
    }
  }, deleteFile = TRUE)




  output$meta_regression_plot <- renderImage({
    results <- get_results()
    if (!is.na(results)) {
      if (!is.null(results$meta_regression_results)) {
        subgroup_analysis_df <- data.frame(
          citation = "Subgroup analysis",
          effect_size = results$effect_size,
          ci.lb = results$ci.lb,
          ci.ub = results$ci.ub,
          shape = 18,  # Diamond
          size = 12
        )
        meta_regression_df <- data.frame(
          citation = "Meta-regression",
          effect_size = results$meta_regression_results$effect_size,
          ci.lb = results$meta_regression_results$ci.lb,
          ci.ub = results$meta_regression_results$ci.ub,
          shape = 18,  # Diamond
          size = 12
        )
        results_df <- rbind(meta_regression_df, subgroup_analysis_df)
        p <- ggplot(data=results_df, aes(x=citation, y=effect_size, ymin=ci.lb, ymax=ci.ub)) +
          geom_pointrange(shape=results_df$shape, fatten=results_df$size) +
          geom_hline(yintercept=1, lty=2) +
          ylab("Response ratio") +
          coord_flip() +
          theme_bw(base_size = 9) +
          theme(
            plot.background = element_rect(fill = "#f5f5f5", color = "#e3e3e3", size = 0.5),
            axis.title.x=element_text(size = 9),
            axis.title.y=element_blank(),
            axis.text=element_text(size = 9),
            text = element_text(family = "Noto Serif"),
            plot.margin = unit(c(0.125,0.125,0.125,0.125), "in")
          )
        outfile <- tempfile(fileext='.svg')
        ggsave(p, filename = outfile, width = 3, height = 2)
        list(src = normalizePath(outfile), contentType = 'image/svg+xml')
      } else {
        outfile <- tempfile(fileext='.svg')
        list(src = normalizePath(outfile), contentType = 'image/svg+xml')
      }
    } else {
      outfile <- tempfile(fileext='.svg')
      list(src = normalizePath(outfile), contentType = 'image/svg+xml')
    }
  }, deleteFile = TRUE)




  output$funnel_plot <- renderImage({
    results <- get_results()
    if (!is.na(results)) {
      results_df <- results$results_df
      log_response_ratio <- results$log_response_ratio
      log_response_ratio_se <- results$log_response_ratio_se
      se_seq <- seq(0, max(results_df$log_response_ratio_se), 0.001)
      lb <- log_response_ratio - (1.96 * se_seq)
      ub <- log_response_ratio + (1.96 * se_seq)
      mean_lb = log_response_ratio - (1.96 * log_response_ratio_se)
      mean_ub = log_response_ratio + (1.96 * log_response_ratio_se)
      ci_df = data.frame(lb, ub, se_seq, log_response_ratio, mean_lb, mean_ub)
      p <- ggplot(data=results_df, aes(x=log_response_ratio_se, y=log_response_ratio)) +
        geom_point(shape=16) +
        xlab("Standard error") +
        ylab("Log response ratio") +
        geom_line(aes(x = se_seq, y = lb), lty=2, data = ci_df) +
        geom_line(aes(x = se_seq, y = ub), lty=2, data = ci_df) +
        geom_line(aes(x = se_seq, y = log_response_ratio), lty=2, data = ci_df) +
        scale_x_reverse() +
        coord_flip() +
        theme_bw(base_size = 10) +
        theme(
          plot.background = element_rect(fill = "#f5f5f5", color = "#e3e3e3", size = 0.5),
          text = element_text(family = "Noto Serif")
        )
      outfile <- tempfile(fileext='.svg')
      ggsave(p, filename = outfile, height = 4, width = 6)
      list(src = normalizePath(outfile), contentType = 'image/svg+xml')
    }
  }, deleteFile = TRUE)




  output$summaries <- renderUI({
    results_by_study <- get_results_by_study()
    if (!is.na(results_by_study)) {
      df <- results_by_study
      lapply(1:length(df$paragraph), function(i) {
        tagList(
          HTML("<tr><td>"),
          HTML(paste(df$paragraph[i])),
          HTML("</td><td class='padding-left'>"),
          sliderInput(paste(df$encoded_citation[i]), "Relevance weight for this study", value = df$relevance_weight[i], min = 0.0000001, max = 1, step = 0.1),
          HTML("</td></tr>")
        )
      })
    }
  })
  outputOptions(output, "summaries", suspendWhenHidden = FALSE)




  output$subgroup_analysis_summary <- renderPrint({
    results <- get_results()
    if (!is.na(results)) {
      results$subgroup_analysis_summary
    } else {
    }
  })
  outputOptions(output, "subgroup_analysis_summary", suspendWhenHidden = FALSE)




  output$meta_regression_formula <- renderPrint({
    results <- get_results()
    if (!is.na(results)) {
      if (!is.null(results$meta_regression_results)) {
        cat(results$meta_regression_results$meta_regression_formula)
      } else {
        cat()  # Return no output.
      }
    } else {
      cat()  # Return no output.
    }
  })
  outputOptions(output, "meta_regression_formula", suspendWhenHidden = FALSE)

  output$meta_regression_summary <- renderPrint({
    results <- get_results()
    if (!is.na(results)) {
      if (!is.null(results$meta_regression_results)) {
        results$meta_regression_results$meta_regression_summary
      } else {
        cat()
      }
    } else {
      cat()
    }
  })
  outputOptions(output, "meta_regression_summary", suspendWhenHidden = FALSE)

  output$meta_regression_predict <- renderPrint({
    results <- get_results()
    if (!is.na(results)) {
      if (!is.null(results$meta_regression_results)) {
        cat(results$meta_regression_results$meta_regression_predict)
      } else {
        cat()  # Return no output.
      }
    } else {
      cat()  # Return no output.
    }
  })
  outputOptions(output, "meta_regression_predict", suspendWhenHidden = FALSE)

  output$meta_regression_prediction <- renderPrint({
    results <- get_results()
    if (!is.na(results)) {
      if (!is.null(results$meta_regression_results)) {
        results$meta_regression_results$meta_regression_results
      } else {
        cat()  # Return no output.
      }
    } else {
      cat()  # Return no output.
    }
  })
  outputOptions(output, "meta_regression_prediction", suspendWhenHidden = FALSE)




  output$refresh_button <- renderUI({
    HTML(paste('<a href="', session$clientData$url_search, if (session$clientData$url_search == "") '?' else '&', 'refresh">Refresh data</a>', sep = ""))
  })




  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Metadataset.csv")
    },
    content = function(file) {
      df <- get_data()
      # Flatten lists and encode variable names for use in meta-regression.
      encoded_df <- get_encoded_df(df)
      # Flatten all other lists (lists cannot be written by write.csv()).
      encoded_df <- apply(encoded_df, 2, function(x) {
        if (is.list(x)) {
          sapply(x, function(y) paste(unlist(y), collapse = ", "))
        }
      })
      # But use the unencoded column names.
      colnames(encoded_df) <- colnames(df)
      write.csv(encoded_df, file)
    }
  )




  observeEvent(input$make_bookmark, {
    bookmark_object <- paste("bookmarks_for_", cache, "settings_", digest(settings()), ".rds", sep = "")
    if (save_data_to_cache == TRUE) {
      s3saveRDS(settings(), object = bookmark_object, s3_bucket, check_region=FALSE)
    }
    rv[["bookmark_url"]] <- paste(protocol, "//", hostname, if (port != "") ":", port, pathname, "?bookmark=", digest(settings()), "&", api_query_string, sep="")
    output$bookmark_link <- renderUI(HTML(paste(
      "<br />",
      "Save this link to reload your analysis later: <a id='shiny_bookmark' href='",
      rv[["bookmark_url"]],
      "'>",
      rv[["bookmark_url"]],
      "</a>",
      "<br />"
    )))
  })




  output$debug1 <- renderUI("")
  output$debug2 <- renderUI("")
  output$debug3 <- renderUI("")

  #output$debug1 <- renderPrint(digest(api_query_string))     # Hash for data folder on S3
  #output$debug2 <- renderPrint(digest(settings()))  # Hash for results and settings on S3
  #output$debug3 <- renderPrint(settings())  # Settings (to be hashed)

  #output$debug1 <- renderPrint(df$Country)
  #inp <- lapply(df$Country, function(x) gsub("[^[:alnum:]]", "", x))
  #output$debug2 <- renderPrint(inp)
  #pat <- gsub("[^[:alnum:]]", "", "American Samoa")
  #pat <- paste("\\b", pat, "\\b", collapse = "|", sep = "")
  #output$debug3 <- renderPrint(grepl(pat, inp))

  #output$debug1 <- renderPrint(attribute_units)  # Encoded attributes
  #output$debug2 <- renderPrint(attributes_df)  # Encoded attributes
  #output$debug1 <- renderPrint(get_filter(input[[paste(attributes_df$attribute[4])]], df[[paste(attributes_df$attribute[4])]]))
  #output$debug1 <- renderPrint(bookmarked_settings)
  #output$debug2 <- renderPrint(settings())
  #output$debug2 <- renderPrint(settings())
  #output$debug2 <- renderPrint(rv[["read_data_from_cache"]])
  #output$debug2 <- renderPrint(settings()[1][[1]])





}  # End of server




# Run the app ----
shinyApp(ui = ui, server = server)
