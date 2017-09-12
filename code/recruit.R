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
        
        message('Cleaning up records')
        
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
                     # 'cited' = Cited(records),
                     'affiliation' = Affiliation(records))
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
  x <- pubmed(start_year = 2010,
              end_year = 2016,
              search_topic = 'malaria',
              # search_topic = paste0('(malaria[Title/Abstract])'),
              counts_only = FALSE,
              n_records = 99999)
  save(x, 
       file = '../data/pubmed_results.RData')
}

# Save the pubmed raw results for later
pubmed_raw <- x$results

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
    message(i)
    these_rows <- expand_rows(df = results[i,])
    results_list[[i]] <- these_rows
  }
  final_authors <- bind_rows(results_list)
  save(final_authors,
       file = '../data/final_authors.RData')
}

# Get email address
if('email_addresses.RData' %in% dir('../data')){
  load('../data/email_addresses.RData')
} else {
  
  # Get a count
  final_authors_aggregated <-
    final_authors %>%
    group_by(author) %>%
    summarise(n = n(),
              titles = paste0(title, collapse = ', '),
              # cited = sum(cited),
              affiliations = paste0(affiliation, collapse = ', ')) %>% 
    arrange(desc(n))
  
  # Get email addresses
  get_email_address <- function(affiliation,
                                return_only_first = TRUE){
    require(stringr)
    the_pattern <- '[[:alnum:].-_]+@[[:alnum:].-]+$'
    # matched <- str_match(affiliation, the_pattern)
    matched <- 
      unlist(regmatches(affiliation, gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", affiliation)))
    if(length(matched) == 0){
      matched <- NA
    }
    if(return_only_first){
      matched <- matched[1]
    } 
    return(matched)
  }
  
  final_authors$email <- NA
  for (i in 1:nrow(final_authors)){
    message(i)
    final_authors$email[i] <-
      get_email_address(final_authors$affiliation[i])
  }
  
  # # Get ALL email addresses
  # all_email_addresses <- c()
  # for (i in 1:nrow(final_authors)){
  #   message(i)
  #   all_email_addresses <-
  #     c(all_email_addresses,
  #       get_email_address(final_authors$affiliation[i],
  #                         return_only_first = FALSE))
  # }
  # 
  # # Keep only unique
  # unique_email_addresses <- sort(unique(all_email_addresses))
  
  # Get a one-row per email address dataframe
  email_df <-
    final_authors %>% 
    filter(!duplicated(email)) %>%
    filter(!is.na(email))
  
  # Get last name
  email_df$last_name <- unlist(lapply(strsplit(as.character(email_df$author), ' '), function(x){x[length(x)]}))
  
  # Save for later
  save(email_df,
       final_authors,
       final_authors_aggregated,
       file = '../data/email_addresses.RData')
}


# Send emails
library(mailR)

connection <- file('../credentials/password.txt')
connection2 <- file('../credentials/password2.txt')
password <- readLines(connection)
password2 <- readLines(connection2)
close(connection)
close(connection2)
# Define function for sending emails
sendify <- function(df){
  
  # Define body
  body <- paste0(
    'Dear Dr. ', 
    df$last_name,
    '\n\n',
    'I found your name and email through your article "',
    df$title,
    '", published in ',
    df$year,
    '.\n\n',
    'For my PhD research on the economics of malaria, I am conducting a ',
    'survey of researchers. The aim is to use a "wisdom of crowds" ',
    'approach to gauge the likelihood and timeframe of eradication. Given your research/publication history, ',
    'I was hoping you would have two minutes or so ',
    'to answer a few questions. The survey is at ',
    'https://goo.gl/forms/IroAEooDuJ6KM5Ho2 .\n\n',
    'Thank you very much for your time. If you have any questions, please do not ',
    'hesitate to contact me.\n\n',
    'Best,\n\n',
    'Joe Brew\n',
    'Barcelona Institute for Global Health (www.isglobal.org)\n\n',
    '(P.S. If you want more details on the study I am doing, visit ',
    'https://github.com/joebrew/malaria_survey#can-we-do-it-a-survey-of-research-professionals-on-the-timeline-and-obstacles-to-eliminating-malaria .)'
    
    
  )
  
  # Define subject
  the_subject <- paste0(
    'Hi Dr. ',
    df$last_name,
    ' - questions about malaria eradication'
  )
  
  # Define sender
  if(i %% 2 == 1){
    sender <- 'joebrew@gmail.com'
  } else {
    sender <- 'joe.brew@isglobal.org'
  }
  
  # Define passowrd
  if(sender == 'joebrew@gmail.com'){
    password <- password
  } else {
    password <- password2
  }
  send.mail(from = sender,
            to = as.character(df$email),
            subject = the_subject,
            body = body,
            smtp = list(host.name = "smtp.gmail.com", 
                        port = 465, 
                        # user.name="joebrew@gmail.com", 
                        user.name = sender,
                        passwd=password, 
                        ssl=TRUE),
            authenticate = TRUE,
            # attach.files = '../in_kind_proposal.pdf',
            send = TRUE)
}
# 
# start_at <- read.csv('~/Desktop/number.csv')$number
# for (i in start_at:nrow(email_df)){
#   message(paste0(i, ' of ', nrow(email_df)))
#   sendify(df = email_df[i,])
#   z <- data.frame(number = i)
#   write.csv(z, '~/Desktop/number.csv')
#   sleeper <- abs(sample(rnorm(mean = 5, n = 1000, sd = 0), 1))
#   Sys.sleep(sleeper)
# }
