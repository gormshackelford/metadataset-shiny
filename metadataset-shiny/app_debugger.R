rm(list = ls())

library(shiny)
library(DT)
library(httr)
library(jsonlite)
library(metafor)

options(scipen = 999)

subject <- 6  # Cover crops
subject <- 7  # Japanese knotweed
intervention <- ""
outcome <- ""
#outcome <- "4"  # Crop yield
#outcome <- "20"  # Soil
publication <- ""
user <- ""


#subject <- 7
#user <- "10"  # Phil
#publication <- "22526"

subject <- "8"  # Himalayan balsam
#publication <- "24184"
#user <- "1"


# Get the data from the API
local_host <- "http://127.0.0.1:8000/api/"
remote_host <- "https://www.metadataset.com/api/"
host <- remote_host
#host <- local_host








get_data <- function(endpoint) {
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

# Data
query <- paste("data/?subject=", subject, "&intervention=", intervention, "&outcome=", outcome, "&publication=", publication, "&user=", user, sep="")
query
endpoint <- paste(host, query, sep="")
endpoint
results <- get_data(endpoint)
df <- results




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

# Experimental design    
names(df)[names(df) == "experiment.experimentdesign_set"] <- "Design"
df$Design <- lapply(df$Design, unlist)

# Get the attributes for this subject
endpoint <- paste(host, "subjects/?subject=", subject, sep="")
this_subject <- fromJSON(endpoint, flatten=FALSE)
this_subject_attribute <- this_subject$results$attribute
endpoint <- paste(host, "attributes/?parent=", this_subject_attribute, sep="")
attributes <- fromJSON(endpoint, flatten=FALSE)
attributes <- attributes$results$attribute
attributes <- sort(attributes)
attributes

is.null(attributes)

is.null(df$EAV_intervention)



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
    EAV_df <- merge(EAV_df, this_df, by="attribute", all=TRUE, no.dups=FALSE)
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
EAV_intervention_df

# Population-level EAVs
EAV_population_df <- get_EAV_df(df$EAV_population)

# Outcome-level EAVs
EAV_outcome_df <- get_EAV_df(df$EAV_outcome)
















# Select the metadata at the lowest level (intervention > population > outcome)
EAV_df <- EAV_intervention_df
for (attribute in attributes) {
  EAV_df[[attribute]] <- as.character(EAV_df[[attribute]])
  EAV_population_df[[attribute]] <- as.character(EAV_population_df[[attribute]])
  EAV_outcome_df[[attribute]] <- as.character(EAV_outcome_df[[attribute]])
  EAV_df[[attribute]][!is.na(EAV_population_df[[attribute]])] <- EAV_population_df[[attribute]][!is.na(EAV_population_df[[attribute]])]
  EAV_df[[attribute]][!is.na(EAV_outcome_df[[attribute]])] <- EAV_outcome_df[[attribute]][!is.na(EAV_outcome_df[[attribute]])]
}

df <- cbind(df, EAV_df)
row.names(df) <- NULL












# SD from SE
df$treatment_sd_from_se <- df$treatment_se * (sqrt(df$treatment_n))
df$treatment_sd[is.na(df$treatment_sd)] <- df$treatment_sd_from_se[is.na(df$treatment_sd)]
df$control_sd_from_se <- df$control_se * (sqrt(df$control_n))
df$control_sd[is.na(df$control_sd)] <- df$control_sd_from_se[is.na(df$control_sd)]

# Response ratio, using formulas from Hedges, L.V., Gurevitch, J., and Curtis, P.S. (1999)
# THE META-ANALYSIS OF RESPONSE RATIOS IN EXPERIMENTAL ECOLOGY. Ecology 80(4):1150-1156.
# https://doi.org/10.2307/177062
get_response_ratio <- function(x) {
  if(x$treatment_mean > 0 & x$control_mean > 0) {         # If both values are positive
    x$treatment_mean / x$control_mean                     # We use the response ratio.
  } else if(x$treatment_mean < 0 & x$control_mean < 0) {  # If both values are negative                                        # If both values are positive
    x$control_mean / x$treatment_mean                     # We use the reciprocal of the response ratio.   
  } else {                                                # If only one value is negative
    NA                                                    # The response ratio is undefined.
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


















# Countries
countries <- sort(unique(unlist(df$Country)))
df$Country <- lapply(df$Country, paste, collapse = ", ")

# Designs
designs <- sort(unique(unlist(df$Design)))
df$Design <- lapply(df$Design, paste, collapse = ", ")

# Generic attributes
attributes_df <- data.frame(attribute = c("Country", "Design"))
attributes_df$options <- c(list(countries), list(designs))


# Subject-specific attributes
attributes_df <- rbind(attributes_df, data.frame(attribute = attributes, options = NA))
for (attribute in attributes) {
  options <- unique(unlist(df[attribute]))
  options <- list(sort(options[!is.na(options)]))
  attributes_df$options[attributes_df$attribute == attribute] <- options
}
attributes_df
attributes_df$options

lapply(1:length(attributes_df$attribute), function(i) {
  unlist(attributes_df$options[i])
})
