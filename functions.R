
#############
#' FUNCTIONS #
#############


#### GOOGLE ANALYTICS ####

#' get page title and content type by web scraping page URLs
getPageData <- function(df){
  
  for(i in 1:nrow(df)){
    
    url <- as.character(df[i, 'pageURL'])
    
    #' get page titles
     tryCatch( { 
      url_tb <- url %>%
        read_html() %>%
        html_nodes('head > title') %>%
        html_text() %>%
        as.data.frame() %>%
        rename(title = 1)
      
       df[i, 'pageTitle'] <- url_tb[1, 'title']
       
     }, error = function(e){
       df[i, 'pageTitle'] <- NA
     })
    
    #' get content type from page metadata
    # if(df[i, 'site'] == 'rmi.org'){
      tryCatch( { 
        url_tb <- url %>%
          read_html() %>% 
          html_nodes('script') %>% 
          html_text() %>% 
          as.data.frame() %>%
          rename(node = 1) %>% 
          filter(grepl('schema.org', node)) %>% 
          mutate(program = str_extract(node, 'articleSection\\":\\"([^"]+)\\"'),
                 program = gsub('articleSection":"',"",program),
                 program = gsub('"', "", program)) %>%
          mutate(keywords = sub('.*keywords\\"\\:\\[', "", node),
                 keywords = gsub('\\].*', "", keywords))
        
        df[i, 'metadata'] <- url_tb[2, 'keywords']
        df[i, 'program'] <- url_tb[2, 'program']
        
      }, error = function(e){
        df[i, 'metadata'] <- NA
      })
    #}# else {
      #' categorize as 'New Website' if no metadata is detected
 #     df[i, 'metadata'] <- NA
  #    df[i, 'pageType'] <- 'New Website'
  #  }
    
  }
  
  #' categorize as 'Article' or 'Report' if these terms are detected in the metadata
  df <- df %>% 
    mutate(pageType = ifelse(grepl('article', tolower(metadata)), 'Article',
                             ifelse(grepl('report', tolower(metadata)), 'Report', pageType)),
           icon = ifelse(grepl('article', tolower(metadata)), 4,
                         ifelse(grepl('report', tolower(metadata)), 1, 5))) %>% 
    distinct(pageTitle, .keep_all = TRUE)
  
}

#' get web traffic metrics for all pages
getPageMetrics <- function(propertyID, pages){
  campaignPages <- ga_data(
    propertyID,
    metrics = c('screenPageViews', "totalUsers", "userEngagementDuration"),
    dimensions = c("pageTitle", "date"),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    #' calculate average engagement duration from userEngagementDuration and convert seconds to mm:ss format
    mutate(engagementDuration = userEngagementDuration / totalUsers,
           sec = round(engagementDuration %% 60, 0),
           min = (engagementDuration / 60) |> floor(),
           avgEngagementDuration = paste0(min, ':', ifelse(nchar(sec) == 1, paste0('0', sec), sec))) %>% 
    select(pageTitle, date, screenPageViews, totalUsers, engagementDuration, avgEngagementDuration) %>% 
    left_join(select(pageData, c(pageTitle, pageType, icon)), by = c('pageTitle')) %>% 
    #' remove " - RMI" from end of page titles
    mutate(pageTitle = gsub(' - RMI', '', pageTitle))
  
  return(campaignPages)
}

#' correct GA acquisition attribution for social media and email channels 
correctTraffic <- function(df, type){
  if(type == 'session'){
    df <- df %>% 
      rename(medium = sessionMedium,
             source = sessionSource,
             defaultChannelGroup = sessionDefaultChannelGroup)
  } 
  
  df <- df %>% 
    mutate(pageTitle = gsub(' - RMI', '', pageTitle)) %>% 
    mutate(medium = ifelse(grepl('mail.google.com', source)|grepl('web-email|sf|outlook', medium), 'email', medium),
           source = ifelse(grepl('linkedin|lnkd.in', source), 'linkedin', source),
           source = ifelse(grepl('facebook', source), 'facebook', source),
           source = ifelse(grepl('dlvr.it|twitter', source)|source == 't.co', 'twitter', source),
           medium = ifelse(grepl('linkedin|lnkd.in|facebook|twitter|instagram', source)|grepl('twitter|fbdvby', medium), 'social', medium),
           medium = ifelse(grepl('/t.co/', pageReferrer), 'social', medium),
           source = ifelse(grepl('/t.co/', pageReferrer), 'twitter', source),
           source = ifelse(grepl('instagram', source), 'instagram', source),
           defaultChannelGroup = ifelse(medium == 'social', 'Organic Social', 
                                        ifelse(medium == 'email', 'Email', defaultChannelGroup))) 
  
  return(df)
}

#' get page traffic (#' sessions) driven by social media channels
getTrafficSocial <- function(propertyID, pages, site = 'rmi.org'){
  
  aquisitionSocial <- ga_data(
    propertyID,
    metrics = c("sessions", "screenPageViews"),
    dimensions = c("pageTitle","date", "sessionSource", "sessionMedium", 'pageReferrer', 'sessionDefaultChannelGroup'),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    arrange(pageTitle) 
  
  aquisitionSocial <- correctTraffic(aquisitionSocial, type = 'session') %>% 
    filter(medium == 'social') %>% 
    dplyr::group_by(pageTitle, date, source) %>% 
    dplyr::summarize(Sessions = sum(sessions),
                     PageViews = sum(screenPageViews)) %>% 
    mutate(site = site)
  
  return(aquisitionSocial)
}

#' get page views broken down by country and region
getTrafficGeography <- function(propertyID, pages, site = 'rmi.org'){
  
  trafficByRegion <- ga_data(
    propertyID,
    metrics = c('screenPageViews'),
    dimensions = c("pageTitle","date", 'region', 'country'),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    #' filter out regions where page views < 5
    filter(screenPageViews > 4) %>% 
    arrange(pageTitle) %>% 
    dplyr::rename('Region Page Views' = screenPageViews) %>% 
    mutate(pageTitle = gsub(' - RMI', '', pageTitle),
           site = site)
  
  return(trafficByRegion)
}

#' get sessions and conversions attributions for acquisition channels (organic, email, social, paid ads, etc.)
#' sessions and conversions use different dimensions so make separate calls for each then bind rows
getAcquisition <- function(propertyID, pages, site = 'rmi.org'){
  
  #' 1) get sessions
  aquisitionSessions <- ga_data(
    propertyID,
    metrics = c("sessions"),
    dimensions = c("pageTitle","date", "sessionSource", "sessionMedium", "pageReferrer", 'sessionDefaultChannelGroup'),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    arrange(pageTitle) 
  
  acquisition <- correctTraffic(aquisitionSessions, 'session') %>% 
    group_by(pageTitle, date, defaultChannelGroup) %>% 
    summarize(Sessions = sum(sessions))
  
  if(site == 'rmi.org'){
    
    #' 2) get conversions
    aquisitionConversions <- ga_data(
      propertyID,
      metrics = c('conversions:emailFormSubmit', 'conversions:downloadThankYou'),
      dimensions = c("pageTitle","date", "source", "medium", 'pageReferrer', 'defaultChannelGroup'),
      date_range = dateRangeGA,
      dim_filters = ga_data_filter("pageTitle" == pages),
      limit = -1
    ) %>% 
      select(pageTitle,date, source, medium, pageReferrer, defaultChannelGroup, 
             form_submit = 'conversions:emailFormSubmit', download = 'conversions:downloadThankYou') %>% 
      arrange(pageTitle)
    
    aquisitionConversions <- correctTraffic(aquisitionConversions, 'conversion') %>% 
      group_by(pageTitle, date, defaultChannelGroup) %>% 
      summarize('Downloads' = sum(download),
                       'Form Submissions' = sum(form_submit)) 
    
    #' 3) bind sessions + conversions 
    acquisition <- acquisition %>% 
      left_join(aquisitionConversions, by = c('pageTitle', 'date', 'defaultChannelGroup')) 
    
  }
  
  acquisition <- acquisition %>% 
    mutate(site = site)
  
  return(acquisition)
  
}

#' get page traffic (#sessions) driven by referral sources that have been identified as “Media” 
#' these sources are defined in the referralSites file
getReferrals <- function(propertyID, pages, site = 'rmi.org'){
  
  referrals <- ga_data(
    propertyID,
    metrics = c("sessions"),
    dimensions = c("pageTitle","date", "sessionSource", "sessionMedium", "pageReferrer"),
    date_range = dateRangeGA,
    dim_filters = ga_data_filter("pageTitle" == pages),
    limit = -1
  ) %>% 
    arrange(pageTitle) %>% 
    group_by(pageTitle, sessionSource) %>% 
    filter(sessionMedium == 'referral') %>% 
    inner_join(select(referralSites, c(media, sessionSource, mediaType, mediaSubtype)), by = 'sessionSource') %>% 
    mutate(referrer = sub('(.*)https://', '', pageReferrer),
           referrer = sub('/(.*)', '', referrer)) %>% 
    filter(referrer != 'rmi.org') %>% 
    group_by(pageTitle, date, sessionSource, media, mediaType, mediaSubtype) %>% 
    summarise(sessions = sum(sessions)) %>% 
    filter(sessions > 2) %>% 
    mutate(site = site,
           pageTitle = gsub(' - RMI', '', pageTitle))
  
  return(referrals)
  
}



