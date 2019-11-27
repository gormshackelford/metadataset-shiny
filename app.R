rm(list = ls())

library(shiny)
library(shinycssloaders)
library(config)
library(digest)
library(aws.s3)
library(httr)
library(jsonlite)
library(nlme)
library(DT)
library(ggplot2)
library(metafor)

options(scipen = 999)




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
    tabPanel("Meta-analysis",
      sidebarLayout(position = "left",
        sidebarPanel(
          h1('Filters'),
          HTML('<br />'),
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
          HTML('<br />'),
          h1('Meta-analysis'),
          HTML('<br />'),
          uiOutput('intervention'),
          HTML('<br />'),
          uiOutput('outcome'),
          HTML('<br />'),
          actionButton("go", "Start your analysis"),
          actionButton("make_bookmark", "Bookmark your analysis"),
          HTML('<br />'),
          HTML('<br />'),
          uiOutput('bookmark_link'),
          HTML('<br />'),
          HTML('<br />'),
          uiOutput('paragraph'),
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
    tabPanel("Data",
      mainPanel(
        HTML('<br />'),
        h1('Data'),
        HTML('<br />'),
        HTML('<br />'),
        DT::dataTableOutput('data_table')
      )
    ),
    tabPanel("Forest plot",
      mainPanel(
        HTML('<br />'),
        h1('Forest plot'),
        HTML('<br />'),
        withSpinner(plotOutput('forest_plot'))
      )
    ),
    tabPanel("Funnel plot",
      mainPanel(
        HTML('<br />'),
        h1('Funnel plot'),
        HTML('<br />'),
        withSpinner(plotOutput('funnel_plot'))
      )
    ),
    tabPanel("Value judgements",
      mainPanel(
        HTML('<br />'),
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
      ),
    tabPanel("Study summaries and weights",
      mainPanel(
        HTML('<br />'),
        h1('Study summaries'),
        HTML('<br />'),
        HTML('<table>'),
        uiOutput('summaries'),
        HTML('</table>')
      )
    )
    )
  )}  # End of UI




server <- function(input, output, session) {
  
  withProgress(message="Loading data...", value=0, {
    
    # Get the url parameters that were used to launch the app
    # For example, subject=6 for https://www.metadataset.com/api/data/?subject=6
    protocol <- isolate(session$clientData$url_protocol)
    hostname <- isolate(session$clientData$url_hostname)
    pathname <- isolate(session$clientData$url_pathname)
    port <- isolate(session$clientData$url_port)
    query <- parseQueryString(isolate(session$clientData$url_search))
    if (!is.null(query[['subject']])) {
      subject <- query[['subject']]
    } else {
      subject <- "6"  # Cover crops
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
      use_cached_data <- FALSE
    } else {
      use_cached_data <- TRUE
    }
    
    # Uncomment the following for testing the data on all agricultural subjects.
    #subject = 2
    
    # Uncomment the following for testing the data on Japanese knotweed.
    #subject = 7
    #intervention = 774  # Herbicides
    #intervention = 782  # Cutting/chopping

    # Uncomment the following for testing the data on Himalayan balsam.
    #subject = 8
    #intervention = 774  # Herbicides
    #intervention = 782  # Cutting/chopping
    #outcome = 348  # Plants (weeds and invasive species)
    
    # Uncomment the following for testing the data on Giant hogweed.
    #subject = "10"
    #intervention = "774"  # Herbicides
    #intervention = "782"  # Cutting/chopping
    #publication = "26083"

    # Uncomment one of the following outcomes for testing the data on cover crops.
    #outcome <- "4"  # Crop yield
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

    # Connect to AWS S3 using credentials for the user, "metadataset-shiny".
    s3_credentials <- config::get("s3")
    Sys.setenv(
      "AWS_ACCESS_KEY_ID" = s3_credentials$AWS_ACCESS_KEY_ID,
      "AWS_SECRET_ACCESS_KEY" = s3_credentials$AWS_SECRET_ACCESS_KEY,
      "AWS_DEFAULT_REGION" = s3_credentials$AWS_DEFAULT_REGION
    )
    s3_bucket <- "metadataset-shiny-cache"
    
    # Get the data from the API
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
    
    cache <- paste(digest(api_query_string), "/", sep = "")
    cached_data <- paste(cache, "data.rds", sep = "")
    cached_intervention <- paste(cache, "intervention.rds", sep = "")
    cached_outcome <- paste(cache, "outcome.rds", sep = "")
    cached_attributes <- paste(cache, "attributes.rds", sep = "")
    
    if (use_cached_data == TRUE & head_object(cached_data, s3_bucket, check_region=FALSE)) {
      df <- s3readRDS(cached_data, s3_bucket, check_region=FALSE)
      attributes_df <- s3readRDS(cached_attributes, s3_bucket, check_region=FALSE)
      intervention <- s3readRDS(cached_intervention, s3_bucket, check_region=FALSE)
      output$intervention <- renderUI(HTML(paste("<span class='bold'>This intervention:</span><span class='italic'>", intervention, "</span>")))
      outcome <- s3readRDS(cached_outcome, s3_bucket, check_region=FALSE)
      output$outcome <- renderUI(HTML(paste("<span class='bold'>This outcome:</span><span class='italic'>", outcome, "</span>")))
    } else {
      use_cached_data <- FALSE
      # Delete the cached data, if it exists.
      s3_objects <- get_bucket(s3_bucket, prefix = digest(api_query_string))
      if (length(s3_objects) > 0) {
        for (i in 1:length(s3_objects)) {
          key <- unlist(s3_objects[i])[1]
          delete_object(key, s3_bucket)
        }
      }
      # Get the non-cached data from the API.
      results <- get_data_from_api(endpoint)
      df <- results
    }
    
    
    
    
    incProgress(0.10)
    
    
    
    
    if (use_cached_data == FALSE) {
      
      # Intervention
      if (intervention != "") {
        query <- paste("interventions/", intervention, "/", sep="")
        endpoint <- paste(host, query, sep="")
        response <- fromJSON(endpoint, flatten = TRUE)
        intervention <- response$intervention
      } else {
        intervention <- "All interventions"
      }
      s3saveRDS(intervention, object = cached_intervention, s3_bucket, check_region=FALSE)
      output$intervention <- renderUI(HTML(paste("This intervention:<span class='italic'>", intervention, "</span>")))
      
      # Outcome
      if (outcome != "") {
        query <- paste("outcomes/", outcome, "/", sep="")
        endpoint <- paste(host, query, sep="")
        response <- fromJSON(endpoint, flatten = TRUE)
        outcome <- response$outcome
      } else {
        outcome <- "All outcomes"
      }
      s3saveRDS(outcome, object = cached_outcome, s3_bucket, check_region=FALSE)
      output$outcome <- renderUI(HTML(paste("This outcome:<span class='italic'>", outcome, "</span>")))
      
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
      df$intervention_country <- lapply(df$intervention_country, unlist)
      df$intervention_country <- cbind(lapply(df$intervention_country, function(x) if(is.null(x)) NA else x))
      df$population_country <- lapply(df$population_country, unlist)
      df$population_country <- cbind(lapply(df$population_country, function(x) if(is.null(x)) NA else x))
      df$outcome_country <- lapply(df$outcome_country, unlist)
      df$outcome_country <- cbind(lapply(df$outcome_country, function(x) if(is.null(x)) NA else x))
      # Select the metadata at the lowest level (intervention > population > outcome)
      df$Country <- df$intervention_country
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
      attributes <- attributes$results$attribute
      attribute_types <- attribute_types[order(attributes)]  # Sort attribute types in the same order that we will sort attributes
      attributes <- attributes[order(attributes)]            # Sort attributes alphabetically
      
      
      
      
      incProgress(0.20)
      
      
      
      
      if (!is.null(attributes)) {
        
        # Get the values (EAVs) for these attributes
        # EAVs (Entity Attribute Values) are user-defined, subject-specific attributes
        get_EAV_df <- function(EAVs) {
          EAV_df <- data.frame(attribute=attributes[order(attributes)])
          # Each EAV is NA for either value_as_factor or value_as_number. Here we delete the NAs.
          for (i in 1:length(EAVs)) {  # For each row of results
            this_df <- EAVs[[i]]  # This row of results
            if (length(this_df) > 0) {  # If there are results in this row
              values <- c(na.omit(c(t(this_df[, -1]))))  # Delete the NAs.
              this_df <- cbind(this_df[1], value=values)
            } else {  # If there are no results in this row
              this_df <- data.frame(attribute=attributes, value=NA)
            }
            # If there are multiple rows for one attribute, combine them into one row.
            if (length(this_df$attribute) > length(unique(this_df$attribute))) {
              this_df$timevar = row.names(this_df)
              this_df <- reshape(this_df, idvar = "attribute", timevar = "timevar", direction = "wide")
              this_df$value <- apply(this_df[-1], 1, function(x) list(x[!is.na(x)]))
              this_df <- cbind(this_df["attribute"], this_df["value"])
            }      
            EAV_df <- base::merge(EAV_df, this_df, by="attribute", all=TRUE, no.dups=FALSE)
          }
          names <- EAV_df[,1]
          if (dim(EAV_df)[2] > 2) {  # If there is more than one record in the data set
            EAV_df <- setNames(data.frame(t(EAV_df[,-1])), names)
          } else {  # If there is only one record in the data set, the dataframe cannot be transposed as above.
            attributes_df <- data.frame(matrix(ncol=length(attributes), nrow=1))
            colnames(attributes_df) <- attributes
            for (attribute in attributes) {
              attributes_df[attribute] <- EAV_df["value"][EAV_df["attribute"]==attribute]
            }
            EAV_df <- attributes_df
          }
          return(EAV_df)
        }
        
        # Intervention-level EAVs
        EAV_intervention_df <- get_EAV_df(df$EAV_intervention)
        incProgress(0.10)
        
        # Population-level EAVs
        EAV_population_df <- get_EAV_df(df$EAV_population)
        incProgress(0.10)
        
        # Outcome-level EAVs
        EAV_outcome_df <- get_EAV_df(df$EAV_outcome)
        incProgress(0.10)
        
        # Select the metadata at the lowest level (intervention > population > outcome)
        EAV_df <- EAV_intervention_df
        for (attribute in attributes) {
          EAV_df[[attribute]] <- as.character(EAV_df[[attribute]])
          EAV_population_df[[attribute]] <- as.character(EAV_population_df[[attribute]])
          EAV_outcome_df[[attribute]] <- as.character(EAV_outcome_df[[attribute]])
          EAV_df[[attribute]][!is.na(EAV_population_df[[attribute]])] <- EAV_population_df[[attribute]][!is.na(EAV_population_df[[attribute]])]
          EAV_df[[attribute]][!is.na(EAV_outcome_df[[attribute]])] <- EAV_outcome_df[[attribute]][!is.na(EAV_outcome_df[[attribute]])]
        }
        
      }  # End of if (!is.null(attributes))
      
      # Dataset for display
      df <- df[c("citation", "publication", "intervention", "population", "outcome", "comparison", "study_id", "study_name", "note", "treatment_mean", "treatment_sd", "treatment_n", "treatment_se", "control_mean", "control_sd", "control_n", "control_se", "n", "unit", "lsd", "is_significant", "approximate_p_value", "p_value", "z_value", "correlation_coefficient", "effect_size", "effect_size_unit", "other_effect_size_unit", "lower_limit", "upper_limit", "confidence", "se", "variance", "Methods", "Location", "Country", "Design")]
      if (!is.null(attributes)) {
        df <- cbind(df, EAV_df)
      }
      publications_n <- length(unique(df$publication))
      df$citation[df$citation == ""] <- "[AUTHOR], [YEAR]"            # The format from Metadataset if both author and year are NA
      df$citation[df$citation == "[AUTHOR], [YEAR]"] <- "[CITATION]"  # Convert to this new format.
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
      attributes_df$type <- c("factor", "factor")
      
      # Subject-specific attributes
      if (!is.null(attributes)) {
        attributes_df <- rbind(attributes_df, data.frame(attribute = attributes, options = NA, type = attribute_types))
        for (attribute in attributes) {
          options <- unique(unlist(df[attribute]))
          options <- list(sort(options[!is.na(options)]))
          attributes_df$options[attributes_df$attribute == attribute] <- options
        }
      }
      
      s3saveRDS(df, object = cached_data, s3_bucket, check_region=FALSE)
      s3saveRDS(attributes_df, object = cached_attributes, s3_bucket, check_region=FALSE)
      
    }  # End of if (use_cached_data == FALSE)
    
    
    
    
    incProgress(0.20)
    
    
    
    
    # Selectors for attributes
    attributes_df$encoded_attribute <- gsub("[^[:alnum:]_]", "", attributes_df$attribute)  # Delete non-alphanumeric characters (except underscores)
    output$data_filters <- renderUI({
      lapply(1:length(attributes_df$attribute), function(i) {
        if(attributes_df$type[i] == "factor") {
          options <- sort(unique(unlist(df[[paste(attributes_df$attribute[i])]])))
          tagList(
            selectInput(paste(attributes_df$encoded_attribute[i]), paste(attributes_df$attribute[i], ":", sep = ""), options, multiple=TRUE)
          )
        } else {
          min <- min(as.numeric(unlist(df[[paste(attributes_df$attribute[i])]])), na.rm = TRUE)
          max <- max(as.numeric(unlist(df[[paste(attributes_df$attribute[i])]])), na.rm = TRUE)
          tagList(
            sliderInput(paste(attributes_df$encoded_attribute[i]), paste(attributes_df$attribute[i], ":", sep = ""), min = min, max = max, value = c(min, max))
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
      selectInput("column_names", "Columns:", unique(attributes_df$attribute), multiple=TRUE)
    })
    outputOptions(output, "column_names", suspendWhenHidden = FALSE)
    
  })  # End of withProgress() for data loading
  
  
  
  
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
  
  
  
  
  #####################
  # Reactive components
  #####################
  
  
  
  
  settings <- reactive({
    all_inputs <- reactiveValuesToList(input)
    if("go" %in% names(all_inputs)) all_inputs$go <- NULL
    if("make_bookmark" %in% names(all_inputs)) all_inputs$make_bookmark <- NULL
    if("data_table_state" %in% names(all_inputs)) all_inputs$data_table_state <- NULL
    if("data_table_rows_all" %in% names(all_inputs)) all_inputs$data_table_rows_all <- NULL
    if("data_table_rows_current" %in% names(all_inputs)) all_inputs$data_table_rows_current <- NULL
    if("data_table_rows_selected" %in% names(all_inputs)) all_inputs$data_table_rows_selected <- NULL
    if("data_table_cell_clicked" %in% names(all_inputs)) all_inputs$data_table_cell_clicked <- NULL
    if("data_table_search" %in% names(all_inputs)) all_inputs$data_table_search <- NULL
    all_inputs <- all_inputs[order(names(all_inputs))]
    return(all_inputs)
  })

  
  
  
  # Reactive values
  rv <- reactiveValues()
  rv[["use_cached_data"]] <- use_cached_data
  rv[["bookmark_link"]] <- ""
  rv[["n_rows"]] <- 0
  
  
  
  
  get_df <- reactive({
    
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
    
    # Weighted variance
    citations <- unique(df$citation)
    n_citations <- length(citations)
    encoded_citations <- gsub("[^[:alnum:]_]", "", citations)  # Delete non-alphanumeric characters (except underscores)
    for (i in 1:n_citations) {
      encoded_citation <- encoded_citations[i]
      if (encoded_citation %in% names(settings())) {
        weight <- input[[encoded_citation]]
      } else {
        weight <- 1
      }
      df$weight[df$citation == citations[i]] <- weight
    }
    df$weighted_v <- df$selected_v * (1 / df$weight)

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
    output$data_table <- DT::renderDataTable(withProgress(message="Please wait...", value=0.5, 
      expr = { DT::datatable(
        df,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip', 
          buttons = c('copy', 'csv'),
          pageLength = 100
        )
      )}),
      server = TRUE
    )
    update_filters(df)
    return(df)
  })
  
  
  
  
  # This function gets the results of a meta-analysis (of the subset of data rows with values for 
  # both the effect size ("es") and its variance ("v").
  get_results <- eventReactive(input$go, {
    withProgress(message="Analyzing data...", value=0.5, {
      data <- get_data()
      d <- subset(data, es_and_v == TRUE)
      n_rows <- length(d$es_and_v)
      n_publications <- length(unique(d$publication))
      n_citations <- length(unique(d$citation))
      use_cached_data <- rv[["use_cached_data"]]
      cached_results <- paste(cache, "results_", digest(settings()), ".rds", sep = "")
      if (use_cached_data == TRUE & head_object(cached_results, s3_bucket, check_region=FALSE)) {
        results <- s3readRDS(cached_results, s3_bucket, check_region=FALSE)
      } else {
        if (n_rows > 0) {
          if (n_rows > 1) {
            model <- rma.mv(yi = log_response_ratio, V = weighted_v, random = ~ 1 | publication/study, data = d)
            log_response_ratio <- model$b
            log_response_ratio_se <- model$se
            effect_size <- as.numeric(round(exp(model$b), 2))  # Effect size = response ratio
            ci.lb <- round(exp(model$ci.lb), 2)                # Lower bound of the confidence interval
            ci.ub <- round(exp(model$ci.ub), 2)                # Upper bound of the confidence interval
            pval <- round(model$pval, 4)
            QE <- round(model$QE)
            QEp <- round(model$QEp, 2)
            if (QEp == 0) QEp <- 0.0001
          } else if (n_rows == 1) {
            log_response_ratio <- d$log_response_ratio[1]
            log_response_ratio_se <- sqrt(d$weighted_v[1])
            effect_size <- as.numeric(round(exp(log_response_ratio)), 2)  # Effect size = response ratio
            ci.lb <- exp(d$log_response_ratio[1] - (1.96 * sqrt(d$weighted_v[1])))
            ci.lb <- round(ci.lb, 2) 
            ci.ub <- exp(d$log_response_ratio[1] + (1.96 * sqrt(d$weighted_v[1])))
            ci.ub <- round(ci.ub, 2)
            zval <- abs(d$log_response_ratio[1] / sqrt(d$weighted_v[i]))
            pval <- 2 * (1 - pnorm(zval))
            QE <- NA
            QEp <- NA
          }
          if (pval == 0) pval <- 0.0001
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
              effect_size = exp(model$yi),  # Effect size = response ratio
              ci.lb = exp(model$yi - 1.96 * sqrt(model$vi)), 
              ci.ub = exp(model$yi + 1.96 * sqrt(model$vi)),
              log_response_ratio = model$yi,  # The log response ratio (not the response ratio) for the funnel plot
              log_response_ratio_se = sqrt(model$vi)  # Standard error of the log response ratio (not the response ratio) for the funnel plot
            )
          } else if (n_rows == 1) {
            results_df <- data.frame(
              citation = d$citation,
              effect_size = exp(d$log_response_ratio[1]),  # Effect size = response ratio
              ci.lb = exp(d$log_response_ratio[1] - (1.96 * sqrt(d$weighted_v[1]))),
              ci.ub = exp(d$log_response_ratio[1] + (1.96 * sqrt(d$weighted_v[1]))),
              response_ratio = d$log_response_ratio[1],
              log_response_ratio_se = sqrt(d$weighted_v[1])  # Standard error of the log response ratio (not the response ratio) for the funnel plot
            )
          }
          results <- list(effect_size = effect_size, ci.lb = ci.lb, ci.ub = ci.ub, pval = pval, QE = QE, QEp = QEp, direction = direction, percent = percent, lower_percent = lower_percent, upper_percent = upper_percent, log_response_ratio = log_response_ratio, log_response_ratio_se = log_response_ratio_se, results_df = results_df, d = d, n_publications = n_publications, n_citations = n_citations, n_rows = n_rows)
          s3saveRDS(results, object = cached_results, s3_bucket, check_region=FALSE)
          rv[["use_cached_data"]] <- TRUE
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
      colnames(results_by_study_df) <- c("citation", "effect_size", "ci.lb", "ci.ub", "log_response_ratio", "log_response_ratio_se", "weight", "paragraph")
      for (i in 1:n_citations) {
        this_citation <- citations[i]
        di <- subset(d, citation == this_citation)
        n_rows <- length(di$es_and_v)
        if (n_rows > 0) {
          results_by_study_df$citation[i] <- this_citation
          results_by_study_df$weight[i] <- mean(di$weight)
          
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
            model <- rma.mv(yi = log_response_ratio, V = weighted_v, random = ~ 1 | study, data = di)
            effect_size <- as.numeric(round(exp(model$b), 2))  # Response ratio
            ci.lb <- round(exp(model$ci.lb), 2)                # Lower bound of the confidence interval
            ci.ub <- round(exp(model$ci.ub), 2)                # Upper bound of the confidence interval
            log_response_ratio <- round(model$b, 2)            # The log response ratio (not the response ratio) for the funnel plot
            log_response_ratio_se <- round(model$se, 2)        # Standard error of the log response ratio (not the response ratio) for the funnel plot
          } else if (n_rows == 1) {
            effect_size <- as.numeric(round(exp(di$log_response_ratio[1]), 2))
            ci.lb <- round(exp(di$log_response_ratio[1] - (1.96 * sqrt(di$weighted_v[1]))), 2)
            ci.ub <- round(exp(di$log_response_ratio[1] + (1.96 * sqrt(di$weighted_v[1]))), 2)
            log_response_ratio <- round(di$log_response_ratio[1], 2)
            log_response_ratio_se <- round(sqrt(di$weighted_v[1]), 2)
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
            "Intervention: <span class='italic'>", di$intervention[1], "</span><br /><br />",
            "Outcome: <span class='italic'>", di$outcome[1], "</span><br /><br />",
            sep = ""
          )
          this_paragraph <- paste(
            "Based on <span class='bold'>", n_rows, " data point", if (n_rows > 1) "s", "</span> from a ", 
            if(!is.null(design)) paste("<span class='bold'>", design, "</span>", sep = ""), " study ", 
            if (!is.na(location)) location else if(!is.null(country)) paste("in <span 
            class='bold'>", country, "</span>"), " (", if (this_citation != "") this_citation else 
            "[CITATION NA]", ") this outcome was <span class='bold'>", percent, "</span> with 
            this intervention than it was without this intervention (between ",lower_percent, " 
            and ", upper_percent, ", based on the 95% confidence interval). <span class='bold'> 
            Methods: </span>", if (methods_text != "") methods_text else paste("[METHODS NA]"), 
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
        "This intervention had a <span class='red'>", direction, " effect</span> on this outcome. 
        This outcome was <span class='red'>", percent, "</span> with this intervention than it was 
        without this intervention (response ratio = ", effect_size, "), based on the selected 
        data. A &quot;", direction, "&quot; effect could be either &quot;good&quot; or 
        &quot;bad&quot; (please see the &quot;Value judgements&quot; tab).
        <br /><br />
        This effect was <span class='red'>", if (pval >= 0.05) "not ", "statistically significant 
        (P = ", format(pval), ")</span>. Based on the 95% confidence interval for this effect, 
        this outcome could have been <span class='red'>between ", lower_percent, " and ", 
        upper_percent, "</span> with this intervention than it would have been without it (", 
        ci.lb, " &#8804; response ratio &#8804; ", ci.ub, ").
        <br />
        <br /> 
        This analysis included <span class='red'>", n_rows, " data points from ", n_citations, 
        " studies in ", n_publications, " publications</span> (please see the &quot;Data&quot; 
        tab). ", 
        if (n_rows > 1) { 
          paste(
            "There was ", if (QEp >= 0.05) "not ", "significant heterogeneity between these data 
            points (Q = ", QE, ", P = ", format(QEp), ").", sep = ""
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
      HTML("<span class='red'>No data</span>")
    }
  })
  
  
  
  
  get_height <- function() {
    reactive({
      n_rows <- rv[["n_rows"]]
      h <- (n_rows + 4) * 36
      return(h)
    })
  }
  
  
  
  
  output$forest_plot <- renderPlot({
    results <- get_results()
    results_by_study <- get_results_by_study()
    if (!is.na(results) & !is.na(results_by_study)) {
      results_df <- data.frame(
        citation = "Mean effect size",
        effect_size = results$effect_size,
        ci.lb = results$ci.lb,
        ci.ub = results$ci.ub,
        log_response_ratio = results$log_response_ratio,
        log_response_ratio_se = results$log_response_ratio_se,
        weight = "",
        paragraph = "",
        encoded_citation = "",
        shape = 18,  # Diamond
        size = 24
      )
      results_by_study_df <- results_by_study
      results_by_study_df$shape <- 16  # Circle
      results_by_study_df$size <- 8
      results_by_study_df <- rbind(results_by_study_df, results_df)
      results_by_study_df$citation <- reorder(results_by_study_df$citation, c(length(results_by_study_df$citation):1))
      n_rows <- length(results_by_study_df$effect_size)
      rv[["n_rows"]] <- n_rows  # For get_height()
      p <- ggplot(data=results_by_study_df, aes(x=citation, y=effect_size, ymin=ci.lb, ymax=ci.ub)) +
        geom_pointrange(shape=results_by_study_df$shape, fatten=results_by_study_df$size) + 
        geom_hline(yintercept=1, lty=2) +
        coord_flip() +
        xlab("Study") + ylab("Response ratio") +
        theme_bw(base_size=16)
      p
    }
  }, height=get_height())

  
  
  
  output$funnel_plot <- renderPlot({
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
        theme_bw(base_size=16)
      p
    }
  })

  
  
  
  output$summaries <- renderUI({
    results_by_study <- get_results_by_study()
    if (!is.na(results_by_study)) {
      df <- results_by_study
      lapply(1:length(df$paragraph), function(i) {
        tagList(
          HTML("<tr><td>"),
          HTML(paste(df$paragraph[i])),
          HTML("</td><td class='padding-left'>"),
          sliderInput(paste(df$encoded_citation[i]), "Weight for this study", value = df$weight[i], min = 0.01, max = 1, step = 0.01),
          HTML("</td></tr>")
        )
      })
    }
  })
  outputOptions(output, "summaries", suspendWhenHidden = FALSE)




  output$refresh_button <- renderUI({
    HTML(paste('<a href="', session$clientData$url_search, if (session$clientData$url_search == "") '?' else '&', 'refresh">Refresh data</a>', sep = ""))
  })
  
  
  
  
  observeEvent(input$go, {
    updateActionButton(session, "go", label = "Update your analysis", icon = NULL)
  }, autoDestroy=TRUE)
  
  
  
  
  observeEvent(input$make_bookmark, {
    bookmark_object <- paste("bookmarks_for_", cache, "settings_", digest(settings()), ".rds", sep = "")
    s3saveRDS(settings(), object = bookmark_object, s3_bucket, check_region=FALSE)
    rv[["bookmark_url"]] <- paste(protocol, "//", hostname, if (port != "") ":", port, pathname, "?bookmark=", digest(settings()), "&", api_query_string, sep="")
    output$bookmark_link <- renderUI(HTML(paste(
      "Save this link to reload your analysis later: <a id='shiny_bookmark' href='", 
      rv[["bookmark_url"]], 
      "'>",
      rv[["bookmark_url"]], 
      "</a>"
    )))
  })
  
  
  
  
  output$debug1 <- renderUI("")
  output$debug2 <- renderUI("")
  output$debug3 <- renderUI("")

  #output$debug1 <- renderPrint(digest(api_query_string))     # Hash for data folder on S3
  #output$debug2 <- renderPrint(digest(settings()))  # Hash for results and settings on S3
  
  #output$debug1 <- renderPrint(df$Country)
  #inp <- lapply(df$Country, function(x) gsub("[^[:alnum:]]", "", x)) 
  #output$debug2 <- renderPrint(inp)
  #pat <- gsub("[^[:alnum:]]", "", "American Samoa")
  #pat <- paste("\\b", pat, "\\b", collapse = "|", sep = "")
  #output$debug3 <- renderPrint(grepl(pat, inp))
  
  #output$debug1 <- renderPrint(attributes_df[3])  # Encoded attributes
  #output$debug1 <- renderPrint(get_filter(input[[paste(attributes_df$attribute[4])]], df[[paste(attributes_df$attribute[4])]]))
  #output$debug1 <- renderPrint(bookmarked_settings)
  #output$debug2 <- renderPrint(settings())
  #output$debug2 <- renderPrint(settings())
  #output$debug2 <- renderPrint(rv[["use_cached_data"]])
  #output$debug2 <- renderPrint(settings()[1][[1]])
  
  
  
  
  
}  # End of server




# Run the app ----
shinyApp(ui = ui, server = server)