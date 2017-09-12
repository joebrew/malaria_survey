library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(RColorBrewer)
# library(Hmisc)
# library(Rmisc)
# install.packages(c("gender", "genderdata"),
#                  repos = "http://packages.ropensci.org",
#                  type = "source")
library(gender)
library(wru)
library(sampleSelection)
library(stargazer)

# Get publication theme for charts
source('theme_publication.R')

# Send emails
source('code/recruit.R', chdir = TRUE)

# Get number of citations of each article (sept 2017)
citations_df <- data_frame(id = as.character(sort(unique(final_authors$id))),
                           n_citations = NA)
if(!'citations.RData' %in% dir('data')){
  for (i in 1:nrow(citations_df)){
    message(paste0(i, ' of ', nrow(citations_df)))
    try({
      res <- EUtilsSummary(paste0(citations_df$id[i],
                                  '[uid]'))
      citations_df$n_citations[i] <- Cited(res)
    })
  }
  
  citations_df <- 
    citations_df %>%
    arrange(desc(n_citations))
  save(citations_df, 
       file = 'data/citations.RData')
} else {
  load('data/citations.RData')
}


# Get average and total number of citations per author
citations_metrics <-
  final_authors %>%
  left_join(citations_df) %>%
  group_by(author) %>%
  summarise(n_citations_total = sum(n_citations, na.rm = TRUE),
            n_citations_average = mean(n_citations, na.rm = TRUE))

# Join that data to our author data
final_authors_aggregated <-
  left_join(final_authors_aggregated,
            citations_metrics,
            by = 'author')

# Load survey data
if(exists('params')){
  get_new_data <- params$get_new_data
} else {
  get_new_data <- FALSE
}

if(get_new_data){
  file_name <- paste0('snapshot_', Sys.Date(), '.RData')
} else {
  snaps <- dir('snapshots')
  dates <- as.Date(substr(snaps, 10, 19))
  file_name <- snaps[which.max(dates)]
}
if(!file_name %in% dir('snapshots')){
  # Fetch data
  library(gsheet)
  url_responses<- 'https://docs.google.com/spreadsheets/d/1yeQ4TR73mdnDABPZ-ZEg3DAFu2Bko5NJ_nkMcNKr2lo/edit?usp=sharing'
  df <- gsheet2tbl(url_responses)
  
  # Manually rename variables
  names(df) <- tolower(gsub('.', '_', names(df), fixed = TRUE))
  names(df) <- gsub(' ', '_', names(df))
  names(df)[4:17] <-
    c('area',
      'years_10',
      'years_20',
      'years_30',
      'years_40',
      'years_50',
      'years',
      'governance',
      'health_workforce',
      'health_financing',
      'health_technologies',
      'health_information',
      'service_delivery',
      'comments')
  
  # Save a snapshot
  save(df, 
       file = paste0('snapshots/',
                     file_name))
} else {
  load(paste0('snapshots/',
              file_name))
}

# Clean up survey responses
# Make NA all empty responses
df$comments <- ifelse(df$comments == '', 
                      NA,
                      df$comments)
df$email_address <- ifelse(df$email_address == '',
                           NA,
                           df$email_address)
# Flag duplicates
df$duplicate <- duplicated(df$email_address)
# Remove duplicates
df <- df[is.na(df$email_address) |
           !df$duplicate,]
# Create area dummies
df$area <- tolower(df$area)
df$area <- gsub('/', ', ', df$area)
df$area <- gsub('(', '', df$area, fixed = TRUE)
df$area <- gsub(')', '', df$area, fixed = TRUE)
# Perform some replacements
df$area[df$area == 'biochemistry'] <- 'chemistry'
df$area[df$area == 'medical entomology'] <- 'entomology'
df$area[df$area == 'vector biology'] <- 'biology'

areas <- tolower(paste0(sort(unique(df$area)), collapse = ', '))
areas <- sort(unique(unlist(strsplit(x = areas, split = ','))))
areas <- trimws(areas, which = 'both')
old_areas <- areas

# Remove those areas for which there are fewer than 5 of a kind
areas_df <- data.frame(area = areas)
areas_df$count <- NA
for (i in 1:nrow(areas_df)){
  areas_df$count[i] <-
    length(which(grepl(areas_df$area[i], 
                       df$area)))
}
areas_df <- areas_df %>%
  filter(area != '',
         count >= 5)
areas <- areas[areas %in% areas_df$area]

for (j in 1:length(areas)){
  df[,paste0(
    'area_',
    areas[j]
  )] <- NA
}
for (j in 1:length(areas)){
  df[,paste0(
    'area_',
    areas[j]
  )] <- grepl(areas[j], df$area)
}

# Get number of areas
# Average number of areas
area_counter <- function(x){
  result <- unlist(gregexpr(',', x))
  if(is.na(result)){
    return(0)
  } else if(result[1] == -1){
    return(1)
  } else {
    return(length(result) + 1)
  }
}
df$n_areas <- sapply(df$area, area_counter)

# Make years numeric
df$years <- tolower(df$years)
# more needs to be done here

# Make comments standardized
df$comments <- tolower(df$comments)

# Clean up names
names(df) <- tolower(gsub('.', '_', names(df), fixed = TRUE))
names(df) <- gsub(' ', '_', names(df))
names(df) <- gsub('-', '_', names(df))
names(df) <- gsub('&', 'and', names(df), fixed = TRUE)
names(df) <- gsub('__', '_', names(df))

df <- data.frame(df)

# Clean up missing responses
for (i in seq(10, 50, 10)){
  df[,paste0('years_', i)][df[,paste0('years_', i)] == ''] <- NA
  df[,paste0('years_', i)] <-
    factor(df[,paste0('years_', i)],
           levels = c('0%',
                      '1-20%',
                      '21-40%',
                      '41-60%',
                      '61-80%',
                      '81-99%',
                      '100%'))
}

# Clean up years
df$years[df$years %in% c('', '10%',
                         'cannot say',
                         'depends on approach and commitment',
                         "human species may be eradicated but zoonotic species may come up",
                         "i don't know - too many unknown variables",
                         "it depends...hard to predict.",
                         "i think we have to be humble. malaria eradication was promised a long time ago...",
                         "it is depend on global stability and climate change",
                         "l don't know but it will remain big health problem",
                         "not sure - depends on when the world begins to take the right steps towards eradication.",
                         'not year but want to')] <- NA
df$years[grepl('never|nevet|ever', df$years)] <- 50
df$years <- gsub(' years| yrs|years|yrs| yeras|about ', '', df$years)
df$years[df$years %in% c('100+',
                         '≥100 to a global zero',
                         '> 50',
                         '50-100',
                         '50 ish',
                         '50yrs',
                         '60 (one more generation)',
                         '75a',
                         'at least 400',
                         "cannot be fully erdicated-some local eradication can be achieved within 50 ",
                         'it will not be eradicated',
                         'more than 50',
                         'unlikely',
                         'i doubt it can be eradicated at all')] <- 50
df$years[df$years == '10-15'] <- 12.5
df$years[df$years == '20-30'] <- 25
df$years[df$years == '40-50'] <- 45
df$years <- as.numeric(as.character(df$years))
df$years[df$years > 50] <- 50

# Clean up the rankings
bb <- c('governance', 'health_workforce',
        'health_financing', 'health_technologies',
        'health_information', 'service_delivery')
for (i in 1:nrow(df)){
  sub_df <- df[i,bb]
  new_values <- sub_df / sum(sub_df, na.rm = TRUE) * 21
  new_values[new_values > 6] <- 6
  df[i,bb] <- new_values
}

# Get the non-respondents into the same data
df$responded <- TRUE
non_respondents <- email_df %>%
  mutate(surname = `first _author_last_name`,
         first_name = `first _author_first_name`,
         name = author) %>%
  dplyr::select(name, surname, first_name, email) %>%
  filter(!email %in% df$email)

# Add to one dataframe
df <- bind_rows(df, non_respondents)

# Also get citations metrics, etc. into the final dataset
df <-
  df %>%
  left_join(final_authors_aggregated %>%
              rename(name = author) %>%
              dplyr::select(name,
                            n_citations_total,
                            n_citations_average)) %>%
  mutate(n_citations_total = ifelse(is.na(n_citations_total), 0, n_citations_total),
         n_citations_average = ifelse(is.na(n_citations_average), 0, 
                                      n_citations_average))

# Predict the gender based on first name ####

# Extract first/last names
df$first_name <- tolower(unlist(lapply(strsplit(df$name, ' '), function(x){x[1]})))
df$last_name <- tolower(unlist(lapply(strsplit(df$name, ' '), function(x){x[length(x)]})))
df$surname <- df$last_name

if(get_new_data){
  # Get gender based on name
  names_df <- df %>%
    group_by(first_name) %>%
    tally %>%
    ungroup %>%
    dplyr::select(-n)
  # Get ethnicity based on last name
  surnames_df <- df %>%
    group_by(surname) %>% 
    tally %>%
    ungroup %>%
    dplyr::select(-n)
  gender_scores <- gender(names = names_df$first_name,
                          years = c(1940, 1990),
                          method = 'ssa') %>%
    rename(first_name = name) %>%
    dplyr::select(first_name,
                  proportion_male,
                  proportion_female,
                  gender)
  surname_scores <- 
    predict_race(voter.file = surnames_df,
                 surname.only = TRUE)
  save(surname_scores,
       gender_scores,
       file = 'data/name_scores.RData')
} else {
  load('data/name_scores.RData')
}

# Get most likely ethnicity
ethnicities <- c('white', 'black', 'hispanic',
                 'asian', 'other')
surname_scores$ethnicity <- ethnicities[apply(surname_scores[,2:6], 1, function(x){which.max(x)[1]})]

# Join surname scores to df
df <-
  left_join(x = df,
            y = surname_scores,
            by = 'surname')
df <-
  left_join(x = df,
            y = gender_scores,
            by = 'first_name')

# Flag non respondents as such
df$responded[is.na(df$responded)] <- FALSE

# sentiment score of contents
library(databrew)
df$sentiment <- databrew::score_sentiment(df$comments)

# Define function for getting number of publications from pubmed
# and their pubmed ids
get_publications <-
  function(first_name, last_name){
    require(jsonlite)
    
    url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=",
                  first_name,
                  "+",
                  last_name,
                  "[author]&retmax=1000&retmode=json")
    
    # Read json
    info <- fromJSON(txt=url)
    
    # Get number of publications
    n <- info$esearchresult$count
    
    # Get pids of those publications
    pids <- info$esearchresult$idlist
    
    # Create an object for return
    out <- data_frame(first_name,
                      last_name,
                      n_publications = n,
                      pids)
    
    # return the dataframe
    return(out)
  }


# # get number of publications
# if('n_publications.RData' %in% dir('../data')){
#   load('../data/n_publications.RData')
# } else {
#   # get the number of publications for each researcher
#   n_publications <- data_frame(first_name = df$first_name,
#                                last_name = df$last_name,
#                                n_publications = NA)
#   publications_list <- list()
#   for (i in 1:nrow(df)){
#     message(paste0(i, ' of ', nrow(df)))
#     try({
#           # Get publications for this researcher
#           this_researcher <- get_publications(first_name = df$first_name[i],
#                                               last_name = df$last_name[i])
#           # Insert number of publications into the n_publications dataset
#           n_publications$n_publications[i] <- this_researcher$n_publications[1]
#           # Save the publications to a separate dataset
#           publications_list[[i]] <- this_researcher
#           Sys.sleep(0.5)
#     })
# 
#   }
#   # Make a publications dataset
#   publications <- bind_rows(publications_list)
#   # Save for later
#   save(publications, 
#        n_publications,
#        file = '../data/n_publications.RData')
# }

if('n_publications.RData' %in% dir('data')){
  load('data/n_publications.RData')
} else {
  n_publications <- final_authors %>%
    filter(!is.na(email)) %>%
    group_by(email) %>%
    summarise(author = dplyr::first(author),
              n_publications = n(),
              n_titles = length(unique(title)),
              language = paste0(sort(unique(language)), collapse = '; '),
              countries = paste0(sort(unique(country)), collapse = '; '),
              ids = paste0(sort(unique(id)), collapse = '; '),
              titles = paste0(sort(unique(title)), collapse = '; '),
              affiliations = paste0(sort(unique(affiliation)), collapse = '; '))
  save(n_publications,
       file = 'data/n_publications.RData')
}


# Join the n_publications dataset to df
df <- left_join(x = df,
                y = n_publications,
                by = 'email')

# Get crossref data from 
# http://brdb.warrington.ufl.edu/sjdb.html
# (not doing for now)

# Define a sex variable
df$sex <- ifelse(df$proportion_female >= 0.5,
                 'female',
                 ifelse(df$proportion_female < 0.5,
                        'male', 
                        NA))

# Bin the number of citations variable
df$bin_citations <- 
  cut(x = df$n_citations_total,
      breaks = quantile(df$n_citations_total,
               na.rm = TRUE),
      include.lowest = TRUE)

# Remove the 1 "other" ethnicity since it screws up all our matrices
df <- df %>%
  filter(ethnicity != 'other')

# Heckman selection
# We're estimating years as a function of sex and ethnicity
ols1 = lm(years ~ sentiment  + 
            ethnicity +
            sex +
            # area_anthropology +
            bin_citations, data = df[df$responded,])

# Estimate likelihood of response
# it is very desirable to have at least one variable in the selection equation that acts like an instrument:
# (https://rpubs.com/wsundstrom/t_selection)
# for us, this is ethnicity - it should affect response (due to language)
# but not likely to affect optimism

# Heckman 2 step
# first equation is likelihood of response
# second equation is impact on response
formula_response <- as.formula("responded ~ sex + ethnicity + bin_citations")
formula_years <- as.formula("years ~ #sentiment  + 
                  ethnicity +
                  sex +
                  area_clinical_medicine +
                  area_biology + 
                  area_epidemiology +
                  area_it +
                  bin_citations")
heck1 <- heckit(formula_response,
                formula_years,
                data = df)

# # See which dummies to use
# dummies <- names(df)[grepl('area_', names(df))]
# for (i in 1:length(dummies)){
#   cat(paste0(dummies[i], '##############\n'))
#   print(table(df[,dummies[i]]))
# }
# Maximum likelihood (like heckman 2 step, but slightly different)
# ML estimation of selection model
ml1 = selection(formula_response,
                formula_years,
                data = df)

# # Look at results
# stargazer(ols1, heck1, ml1,
#           # se=list(cse(ols1),NULL,NULL),
#           title="Regression on years until eradication", type="latex",
#           df=FALSE, digits=4)

