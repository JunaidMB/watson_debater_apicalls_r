# Watson Debater Services: This code is a wrapper around the curl commands related to the Watson Debater API. For these functions to work, you will need an apikey for Watson Debater.
# The services covered in this script are:
# 1. Claim Detection
# 2. Claim Boundaries
# 3. Evidence Detection
# 4. Pro/ Con
# 5. Term Wikifier
# 6. Term Relater
# 7. Clustering
# 8. Theme Extraction

# Load Packages
library(tidyverse)
library(glue)
library(jsonlite)
library(httr)
library(stringi)


# Claim Detection
## Function
debater_claimdet_score <- function(apikey, topic, sentence) {
  headers = c(
    `apiKey` = glue('{apikey}'),
    `Content-Type` = 'application/json'
  )
  
  data <- str_c('{ "pairs": [ ', '"', glue('{sentence}'), '^^^', glue('{topic}'), ' " ] }')
  
  response <- httr::POST(url = 'https://claim-sentence.debater.res.ibm.com/score/',
                         httr::add_headers(.headers=headers),
                         body = data)
  
  response_text <- content(response, type = "text")
  response_data <- fromJSON(response_text)
  
  return(response_data)
  
}

## Example
debater_claimdet_score(apikey = apikey,
                       topic = "We should legalize cannabis",
                       sentence = "A recent federal study indicates that cannabis is dangerous")
# Claim Boundaries
## Function
debater_claimbound_score <- function(apikey, sentence) {
  headers = c(
    `apiKey` = glue('{apikey}'),
    `Content-Type` = 'application/json'
  )
  data <- str_c('{ "sentences": [ ', '"', glue('{sentence}'), '" ] }')
  response <- httr::POST(url = 'https://claim-boundaries.debater.res.ibm.com/score/',
                         httr::add_headers(.headers=headers),
                         body = data)
  response_text <- content(response, type = "text")
  response_data <- fromJSON(response_text)
  return(response_data)
}

## Example
debater_claimbound_score(apikey = apikey, sentence = "A recent federal study indicates that cannabis is dangerous")

# Evidence Detection
## Function
debater_evidet_score <- function(apikey, topic, sentence) {
  headers = c(
    `apiKey` = glue('{apikey}'),
    `Content-Type` = 'application/json'
  )
  data <- str_c('{ "pairs": [ ', '"', glue('{sentence}'), '^^^', glue('{topic}'), ' " ] }')
  response <- httr::POST(url = 'https://motion-evidence.debater.res.ibm.com/score/',
                         httr::add_headers(.headers=headers),
                         body = data)
  response_text <- content(response, type = "text")
  response_data <- fromJSON(response_text)
  return(response_data)
}

## Example
debater_evidet_score(apikey = apikey,
                     topic = "We should legalize cannabis",
                     sentence = "A recent federal study indicates that cannabis is dangerous")

# Argument Quality
## Function
debater_argqual_score <- function(apikey, topic, sentence) {
  headers = c(
    `apiKey` = glue('{apikey}'),
    `Content-Type` = 'application/json'
  )
  
  data <- str_c('{ "pairs": [ [', '"', glue('{sentence}'), '\", \"', glue('{topic}'), ' "] ] }') 
  
  response <- httr::POST(url = 'https://arg-quality.debater.res.ibm.com/score/',
                         httr::add_headers(.headers=headers),
                         body = data)
  
  response_text <- content(response, type = "text")
  response_data <- fromJSON(response_text)
  
  return(response_data)
}

# Example
debater_argqual_score(apikey = apikey,
                      topic = "We should further explore the development of autonomous vehicles",
                      sentence = "Cars should only provide assisted driving, not complete autonomy")

# Procon 
## Function
debater_procon_score <- function(apikey, topic, sentence) {
  data <- glue('{{\n
               "discussion_topic": "{topic}" ,\n
               "arguments": [\n
               "{sentence}"]\n
}}')
  
  response <- POST(url = 'https://pro-con.debater.res.ibm.com/score/'
                   , body = data
                   , add_headers(.headers = 
                                   c('Content-Type' = "application/json",
                                     'apiKey' = apikey)))
  
  response_text <- content(response, type = "text")
  response_data <- fromJSON(response_text)
  
  return(response_data)
  
  }

## Example
debater_procon_score(apikey = apikey,
                     topic = "Social media is harmful",
                     sentence = "Social media disproportionally promotes fake news")

# Term Wikifier
## Function
debater_termwikifier <- function(apikey, sentence) {
  
  headers = c(
    `apiKey` = glue('{apikey}'),
    `Content-Type` = 'application/json'
  )
  
  data = str_c('{ "config": "", "contextTitles": [], "contextText": "", "textsToAnnotate": [ {"text": "', glue('{sentence}'), '", ', '"contextTitles": []} ] }')
  
  
  response <- httr::POST(url = 'https://tw.debater.res.ibm.com/TermWikifier/v2/annotate/', httr::add_headers(.headers=headers), body = data)
  response_text <- content(response, type = "text")
  response_data <- fromJSON(response_text)
  
  return(response_data)
}

## Example

### Printed output
response_df <- data.frame(debater_termwikifier(apikey = apikey, sentence = 'Cars should only assist drivers since self driving cars are too dangerous.')$annotations) %>% 
  mutate(span = str_c("(", spanStart, ", ", spanEnd, ")"),
         inlinks_trim = stri_replace_all_regex(inlinks, "\\b0*(\\d+)\\b", "$1")) %>% 
  filter(titleOfDisambiguationPage != "<NA>")
output_sentences <- c()

for (i in seq_along(response_df$cleanText) ) {
  
  cleanText <- response_df$cleanText[i]
  span <- response_df$span[i]
  title <- response_df$title[i]
  inlinks_trim <- response_df$inlinks_trim[i]
  
  sentence <- glue('Text "{cleanText}", within "{span}", is wikified to titles "{title}", whose inlinks = {inlinks_trim}. \n')
  
  output_sentences[i] <- sentence
  
}

cat(glue('For sentence: "{argument}", identified annotations are:'), "[", output_sentences, "]", sep = "\n")

### Raw output

debater_termwikifier(apikey = apikey, sentence = 'Cars should only assist drivers since self driving cars are too dangerous.')

# Term Relater
## Function
debater_termrel_score <- function(apikey, term1, term2) {
  headers = c(
    `apiKey` = glue('{apikey}'),
    `Content-Type` = 'application/json'
  )
  
  data <- str_c('[ {"first": "', glue('{term1}'), '", "second": "', glue('{term2}'), '" } ]')
  
  response <- httr::POST(url = 'https://tr.debater.res.ibm.com/api/scores/TermRelater/pairs/', httr::add_headers(.headers=headers), body = data)
  response_text <- content(response, type = "text")
  response_data <- fromJSON(response_text)
  
  return(response_data)
}

## Example
debater_termrel_score(apikey = apikey, term1 = 'Dog', term2 = 'Rottweiler')

# Clustering
## Function
debater_clustering <- function(apikey, clusters = 2, sentences) {
  headers = c(
    `apiKey` = glue('{apikey}'),
    `Content-Type` = 'application/json'
  )
  
  data <- str_c('{ "text_preprocessing": ["stemming"], "embedding_method": "tf", "clustering_method": "sib", "num_of_clusters": ',
                glue('{clusters}'),
                ', "arguments": ', 
                str_c('[ "', paste(sentences, collapse = '", "'), '" ]'),
                " }")
  
  response <- httr::POST(url = 'https://clustering.debater.res.ibm.com/api/public/clustering', httr::add_headers(.headers=headers), body = data)
  response_text <- content(response, type = "text")
  response_data <- fromJSON(response_text)
  
  cluster_output <- response_data$arguments_id_and_distance_per_cluster$argumentInfoContainers
  
  
  # Assign Cluster ID to each Data Frame
  for (i in seq_along(cluster_output)) {
    cluster_output[[i]] <- cluster_output[[i]] %>% 
      mutate(cluster_id = i)
  }
  
  # Format return dataframe
  cluster_output_df <- bind_rows(cluster_output)
  
  cluster_output_df <- cluster_output_df %>% 
    mutate(sentence_id = (argument_id + 1),
           cluster_id = (cluster_id - 1))
  
  # Take input sentences and format as dataframe
  sentences_df <- data.frame(sentences) %>% 
    mutate(sentence_id = row_number())
  
  clustered_sentences <- sentences_df %>% 
    left_join(cluster_output_df[, c('cluster_id', 'sentence_id')], by = c('sentence_id' = 'sentence_id')) %>% 
    arrange(cluster_id)
  
  return(clustered_sentences)

}

## Example
sentences <- c("The cat (Felis catus) is a small carnivorous mammal",
               "The origin of the domestic dog includes the dogs evolutionary divergence from the wolf",
               "As of 2017, the domestic cat was the second-most popular pet in the U.S.",
               "Domestic dogs have been selectively bred for millennia for various behaviors, sensory capabilities, and physical attributes.",
               "Cats are similar in anatomy to the other felid species",
               "Dogs are highly variable in height and weight.")

debater_clustering(apikey = apikey, clusters = 2, sentences = sentences)

# Theme Extraction
## Function
debater_theme_extract <- function(apikey, dominantConcept, topic, cluster, return_valid_only = TRUE) {
  
  headers = c(
    `apiKey` = glue('{apikey}'),
    `Content-Type` = 'application/json'
  )
  
  # Helper function to make Json of the correct format
  wrap_str_json <- function(x) {
    return(str_c('[ "', paste(x, collapse = '", "'), '" ]'))
  }
  
  cluster_of_sentences_wrapped <- lapply(cluster, wrap_str_json)
  cluster_flattened <- str_c('[ ', paste(cluster_of_sentences_wrapped, collapse = ', '), ' ]')
  
  # Format curl request message body
  data <- str_c('{ "dominantConcept": "',
                glue('{dominantConcept}'),
                '", ',
                '"topic": "', 
                glue('{topic}'),
                '", ',
                '"elements": ',
                cluster_flattened,
                ' }')
  
  response <- httr::POST(url = 'https://theme-extraction.debater.res.ibm.com/api/v1/theme/extract/', httr::add_headers(.headers=headers), body = data)
  response_text <- content(response, type = "text")
  response_data <- fromJSON(response_text)
  
  # Extract the Theme information with P-values from response object
  themes_list <- data.frame(response_data)$elements.themes
  
  # Assign Cluster ID to each Data Frame
  for (i in seq_along(themes_list)) {
    themes_list[[i]] <- themes_list[[i]] %>% 
      mutate(cluster_id = i)
  }
  
  # Format return dataframe
  themes_df <- bind_rows(themes_list)
  
  if (return_valid_only == TRUE) {
    
    themes_df_return <- themes_df %>% 
      filter(is_valid == TRUE)
    
    return(themes_df_return)
    
  } else {
    
    themes_df_return <- themes_df
    
    return(themes_df_return)
    
  }
  
}

## Example

dominantConcept <- "Animal"
topic <- "We should love Animals"

sentences_1 <- c("The cat (Felis catus) is a small carnivorous mammal",
                 "As of 2017, the domestic cat was the second-most popular pet in the U.S.",
                 "Cats are similar in anatomy to the other felid species")
sentences_2 <- c("The origin of the domestic dog includes the dogs evolutionary divergence from the wolf",
                 "Domestic dogs have been selectively bred for millennia for various behaviors, sensory capabilities, and physical attributes.",
                 "Dogs are highly variable in height and weight")

cluster_of_sentences <- list(sentences_1, sentences_2)

debater_theme_extract(apikey = apikey,
                      dominantConcept = "Animal",
                      topic = "We should love animals",
                      cluster = cluster_of_sentences,
                      return_valid_only = TRUE)