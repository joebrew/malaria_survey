library(RISmed)
library(dplyr)
library(ggplot2)
library(tidyr)
# http://amunategui.github.io/pubmed-query/

# Define function for getting results from pubmed
pubmed <- function(start_year = 2016,
                   end_year = 2016,
                   search_topic = 'malaria',
                   counts_only = false,
                   n_records = 5000){
  
  # Define a year range
  years <- start_year:end_year
  
  
  # If counts only, simply get the counts by year
  if(counts_only){
    # JUST COUNTING RESULTS
    
    # Create a placeholder for results
    return_object <- data.frame(year = years,
                                n = NA)
    
    for (year in 1:length(years)){
      message(paste0('Fetching records for year ', years[year]))
      
      # Perform search
      search_query <- try({ 
        EUtilsSummary(search_topic, 
                      db="pubmed", 
                      mindate=years[year], 
                      maxdate=years[year],
                      retmax = n_records)})
      if(inherits(search_query, 'try-error')){
        n <- 0
      } else {
        n <- QueryCount(search_query)
      }
      
      # Populate results dataframe
      return_object$n[year] <- n
    }
    
  } else {
    # NOT JUST COUNTS. ACTUALLY RETRIEVE RESULTS
    
    # Create an empty list for sticking results
    results_list <- list()
    # Create another empty list for sticking abstracts
    abstracts_list <- list()
    
    # Loop through each year to get results
    for (year in 1:length(years)){
      message(paste0('Fetching records for year ', years[year]))
      
      try({
        # Perform search
        search_query <- 
          EUtilsSummary(search_topic, 
                        db="pubmed", 
                        mindate=years[year], 
                        maxdate=years[year],
                        retmax = n_records)
        
        # See results summary
        # summary(search_query)
        
        # See number of results
        # QueryCount(search_query)
        
        # Get IDs of the articles returned in our query
        # Qids <- queryId(search_query)
        
        # Actually fetch data
        records <- EUtilsGet(search_query)
        
        # Turn into a data.frame
        pubmed_data <- 
          data.frame('title' = ArticleTitle(records),
                     # 'Abstract' = AbstractText(records),
                     'language' = Language(records),
                     'country' = Country(records),
                     'id' = ArticleId(records),
                     'year' = years[year],
                     'month' = MonthPubmed(records),
                     'day' = DayPubmed(records),
                     'affiliation' = Affiliation(records),
                     'cited' = Cited(records))
        # Cited(records)
        # RefSource(records)
        
        # Create separate dataframe for abstracts
        abstracts <- data.frame('id' = ArticleId(records),
                                'abstract' = AbstractText(records))
        
        # Add authors separately
        temp <- Author(records)
        first_authors <- lapply(temp,
                                function(x){
                                  x[1,]
                                })
        last_authors <- lapply(temp,
                               function(x){
                                 x[nrow(x),]
                               })
        for (i in c('first', 'last')){
          # Last name
          pubmed_data[,paste(i, '_author_last_name')] <- 
            unlist(lapply(get(paste0(i, '_authors')), function(x){x['LastName']}))
          
          # First name
          pubmed_data[,paste(i, '_author_first_name')] <- 
            unlist(lapply(get(paste0(i, '_authors')), function(x){x['ForeName']}))
          
          # Initials
          pubmed_data[,paste(i, '_author_initials')] <- 
            unlist(lapply(get(paste0(i, '_authors')), function(x){x['Initials']}))
        }
        
        # All authors
        pubmed_data$all_authors <- 
          unlist(lapply(Author(records), function(x){
            paste0(x$ForeName, 
                   ' ', 
                   x$LastName, 
                   collapse = ', ')}))
        
        # Add the year
        pubmed_data$year <- years[year]
        
        # Add results to results_list
        results_list[[year]] <- pubmed_data
        
        # Add abstract to abstracts list
        abstracts_list[[year]] <- abstracts
        
        # Remove unecessary objects
        rm(pubmed_data, abstracts)
        Sys.sleep(0.3)
      })
      
    }
    
    # Bind together the results
    results <- do.call('rbind', results_list)
    
    # Bind together the abstracts
    abstracts <- do.call('rbind', abstracts_list)
    
    # Put into list
    return_object <- 
      list(results = results,
           abstracts = abstracts)
  }
  # Return
  return(return_object)
}
if('pubmed_results.RData' %in% dir('../data')){
  load('../data/pubmed_results.RData')
} else {
  # Run on December 20, 2016, at 9:00 CAT
  x <- pubmed(start_year = 2000,
              end_year = 2016,
              search_topic = paste0('(malaria[Title/Abstract])'),
              counts_only = FALSE,
              n_records = 99999999)
  save(x, 
       file = '../data/pubmed_results.RData')
}

if('final_authors.RData' %in% dir('../data')){
  load('../data/final_authors.RData')
} else {
  # Extract results
  results <- x$results
  
  # Define function for expanding into a one row per author dataset
  expand_rows <- function(df){
    # Extract authors
    authors <- df$all_authors
    authors <- strsplit(x = authors, split = ', ')
    authors <- unlist(authors)
    
    # Create a dataframe
    left <- data_frame(author = authors,
                       dummy = 1)
    
    # Join to full data
    right <- df %>% dplyr::select(-all_authors) %>%
      mutate(dummy = 1)
    result <- left_join(left,
                        right,
                        by = 'dummy') %>%
      dplyr::select(-dummy)
    return(result)
  }
  
  # Go through each row, expand, and join results
  results_list <- list()
  for (i in 1:nrow(results)){
    these_rows <- expand_rows(df = results[i,])
    results_list[[i]] <- these_rows
  }
  final_authors <- bind_rows(results_list)
  save(final_authors,
       file = '../data/final_authors.RData')
}

# Get a count
final_authors_aggregated <-
  final_authors %>%
  group_by(author) %>%
  summarise(n = n(),
            titles = paste0(title, collapse = ', '),
            affiliations = paste0(affiliation, collapse = ', '),
            cited = sum(cited)) %>% 
  arrange(desc(n))
