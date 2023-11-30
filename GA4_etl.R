
setwd("C:/Users/ghoffman/OneDrive - RMI/01. Projects/Consultation/Influence_Subley")
### get packages and functions
source("packages.R")  
source("functions.R")  

load_dot_env('Renviron.env')

### get referral sites
referralSites <- read.xlsx('Referral Site Categories.xlsx', sheet = 'All Referral Sites')

currentDate <- paste(Sys.Date())
dayBefore <- paste(as.Date(currentDate) - days(1))
weekBefore <- paste(as.Date(currentDate) - days(7))
oneYearAgo <- ymd(currentDate) - years(1)


twoBefore <- paste(as.Date(currentDate) - days(2))
## GA Authentication
ga_auth(email = "ghoffman@rmi.org")


#' set GA variables and property ID
rmiPropertyID <- 354053620
metadataGA4 <- ga_meta(version = "data", rmiPropertyID)
dateRangeGA <- c(paste(weekBefore), paste(dayBefore))

#dateRangeGA <- c(paste(weekBefore), paste(dayBefore))

## set export
ss <- 'ParsleyReplaceDataset.xlsx'


## Dbase connection
# library(RMariaDB)
# 
con <- dbConnect(
        RMariaDB::MariaDB(),
        dbname = 'rmi_GA4',
        username = Sys.getenv('DBASE_USER'),
        password = Sys.getenv("DBASE_PWD"),
        host = Sys.getenv('DBASE_IP'),
        port = '3306',
        ssl.ca = normalizePath("C:\\Users\\ghoffman\\OneDrive - RMI\\01. Projects\\DigiCertGlobalRootCA.crt.pem")
      )

# dbWriteTable(con, 'geographyTraffic', geographyTraffic)

 a <- dbListTables(con)
# 
# b <- as.character("allTraffic" , "campaignNewsletters" ,"campaignPosts"  ,  "contentSummary" ,   "donations", 
#           "geographyTraffic"  ,  "mediaReferrals"  ,    "SFcampaigns"   ,      "socialTraffic"    ,   "targetCampaign")
# 
# 
# readxl::excel_sheets('OCI+ Dashboard Dataset.xlsx')
# 
# for(i in readxl::excel_sheets('OCI+ Dashboard Dataset.xlsx')){
#   df <- read.xlsx('OCI+ Dashboard Dataset.xlsx', sheet = i, detectDates = TRUE)
#   dbWriteTable(con, i, df, append = TRUE)
# 
# }

#' SUMMARY
#' 1. Pulls list of pages from campaign key file
#' 2. Gets page title and metadata by webscraping provided URLs
#' 3. For all pages
#'      Get key metrics - page views, users, engagement duration
#'      Get acquisition data - sessions + conversions - broken down by channel
#'      Get social media acquisition data - sessions
#'      Get page views broken down by country and region
#'      Get page traffic - sessions - driven by referral sources that have been identified as “Media” 
#'        - These sources are defined in the referralSites file



# Sitewide metrics

# □ Traffic source (acquisition)
# ® Take a look at the referral categories
# ® May want to parse out more granular to differentiate different news outlets vs. other RMI pages vs. social traffic
# □ Geography
# ® Country level (need)
# ® State level (nice to have)
# □ Engaged page views
# ® Number
# ® Rate
# □ Time spent on page
# □ Users
# ® Returning
# ® New
# □ Conversions
# ® Report downloads
# ® Online donations (via GA4 Goal)
# ® Event reg (Goal)
# Email sign up (Goal)


# Get all pages on rmi.org to use as reference table.
pages <- ga_data(
  rmiPropertyID,
  metrics = c('screenPageViews'),
  dimensions = c("pageTitle",'fullPageUrl'),
  date_range = dateRangeGA,
  limit = -1)


# Filter included pages to those with rmi.org in URL, populated page title, and at least 100 page views
pages_filter <- pages %>%
  filter(grepl('rmi.org', fullPageUrl)) %>%
  filter(pageTitle != 'Page not found - RMI') %>%
  filter(pageTitle != '(not set)') %>%
  filter(pageTitle != '') %>%
  filter(!grepl('rmi.org/people', fullPageUrl)) %>%
  filter(screenPageViews > 99) %>%
  mutate(pageURL = paste('http://', fullPageUrl, sep = ''), pageTitle = gsub(' - RMI', '', pageTitle),
         site = 'rmi.org', metadata = '', pageType = '')


# people_pages <- pages %>%
#   filter(grepl('rmi.org/people', fullPageUrl))
         
############# Working section for testing/exploration

# test_date <- ga_data(
#   rmiPropertyID,
#   metrics = c('screenPageViews', "totalUsers", "userEngagementDuration", 'newUsers'),
#  # dimensions = c("pageTitle", "country","city","region", 'sessionSource'),
#   dimensions = c("pageTitle", "country", "yearMonth"),
#   date_range = dateRangeGA,
#   dim_filters = ga_data_filter("pageTitle" == pages),
#   limit = -1)
# 
# test_date %>%
#   group_by(yearMonth) %>%
#   summarise(views = sum(screenPageViews))
# 
# # df for testing, top 5 rows of pages
# test_page <- pages_filter %>%
#   head()
# 
# url <- as.character(test_page[3, 'pageURL'])
# 
# url_tb <- url %>%
#   read_html() %>% 
#   html_nodes('script') %>% 
#   html_text() %>% 
#   as.data.frame()
#   
# pageData <- getPageData(test_page)
# 
# for(i in 1:nrow(test_page)){
#   url <- as.character(test_page[i, 'pageURL'])
#   
# url_tb <- url %>%
#   read_html() %>% 
#   html_nodes('script') %>% 
#   html_text() %>% 
#   as.data.frame() %>% 
#   rename(node = 1) %>% 
#   filter(grepl('schema.org', node)) %>% 
#   mutate(keywords = sub('.*keywords\\"\\:\\[', "", node),
#          keywords = gsub('\\].*', "", keywords))
# }
# 
# campaignPages <- ga_data(
#   rmiPropertyID,
#   metrics = c('screenPageViews', "totalUsers", "userEngagementDuration"),
#   dimensions = c("pageTitle", "date"),
#   date_range = dateRangeGA,
#   dim_filters = ga_data_filter("pageTitle" == pages),
#   limit = -1
# )

###################################################333
########################################################33

data <- getPageData(pages_filter)

pageData <- data

pages <- pageData[['pageTitle']]


#' get page metrics
pageMetrics <- getPageMetrics(rmiPropertyID, pages) 

#' get acquisition
acquisition <- getAcquisition(rmiPropertyID, pages) 

#' get social traffic
socialTraffic <- getTrafficSocial(rmiPropertyID, pages) 

#' get geographic segments
geographyTraffic <- getTrafficGeography(rmiPropertyID, pages) 

#' get referrals
mediaReferrals <- getReferrals(rmiPropertyID, pages)


pageMetrics <- pageMetrics %>%
  filter(!is.na(date)) %>%
  mutate(uid = paste(pageTitle, date, sep = "_"))
  
  
  #' bind page metrics and pivot table so that sessions/conversions are stored in one column
  #' this is to make a Power BI table column that changes based on an applied filter
allTraffic <- pageData %>% 
    select(site, pageTitle) %>% 
    distinct() %>% 
    plyr::rbind.fill(acquisition) %>% 
    pivot_longer(cols = c(Sessions:'Form Submissions'), names_to = "type", values_to = "count") %>% 
    filter(!is.na(count), count > 0, !is.na(date)) %>%
    mutate(count = round(count, 1), uid = paste(pageTitle, date, sep = "_")) %>%
    # left_join(select(pageMetrics, c(pageTitle, screenPageViews:avgEngagementDuration, pageType, icon)), by = c('pageTitle','date')) %>% 
    left_join(select(pageMetrics, c(uid, screenPageViews:avgEngagementDuration, pageType, icon)), by = c('uid')) %>% 
     mutate(count = as.numeric(ifelse(is.na(count), 0, count))) %>% 
    filter(defaultChannelGroup != 'Unassigned' & !is.na(defaultChannelGroup))
  
numChannels <- allTraffic %>% group_by(uid, type) %>% summarize(numChannels = n())
  
allTraffic <- allTraffic %>% 
    left_join(numChannels) %>% 
    mutate('page_views' = round(screenPageViews/numChannels, 2)) %>% 
    select(-c(screenPageViews, numChannels))

#########################################################

  
# page titles and IDs from database
query <- dbSendQuery(con, "SELECT Id, pageTitle FROM rmi_pages")
db_pages <- dbFetch(query)

# Filter to new pages
rmi_pages <- data %>%
  select("pageTitle","pageURL", "pageType" , "icon" ) %>%
  mutate(pageTitle = gsub(' - RMI', '', pageTitle)) %>%
  left_join(db_pages, by = c('pageTitle')) %>%
  filter(is.na(Id))
  
# import new pages to database, if new pages are present
if(length(rmi_pages >1)){
  dbWriteTable(con, 'rmi_pages', rmi_pages, append = T)
}

# Get updated complete list of page titles
query <- dbSendQuery(con, "SELECT Id, pageTitle FROM rmi_pages")
# page titles and IDs from database
db_pages <- dbFetch(query)

traffic_all <- allTraffic %>%
  left_join(db_pages, by=c('pageTitle')) %>%
  rename('pageID' = 'Id','pageViews'='page_views') %>%
  select(pageID, date, defaultChannelGroup, type, count, totalUsers,
         engagementDuration, pageViews) %>%
  filter(!is.na(pageID)) %>%
  as.data.frame()

traffic_social <- socialTraffic %>%
  ungroup()%>%
  left_join(db_pages, by=c('pageTitle')) %>%
  rename('pageID' = 'Id', 'sessions' = 'Sessions', 'pageViews'='PageViews') %>%
  select(pageID, date, source, sessions, pageViews) %>%
  filter(!is.na(pageID)) %>%
  as.data.frame()

traffic_geography <- geographyTraffic  %>%
  left_join(db_pages, by=c('pageTitle')) %>%
  rename('pageID'='Id', 'regionPageViews'='Region Page Views') %>%
  select(pageID, date, region, country, regionPageViews) %>%
  filter(!is.na(pageID)) %>%
  as.data.frame()

referrals_media <- mediaReferrals %>%
  ungroup()%>%
  left_join(db_pages, by=c('pageTitle')) %>%
  rename('pageID'='Id', 'source'='sessionSource') %>%
  select(pageID, date, source, media, mediaType, mediaSubtype, sessions) %>%
  filter(!is.na(pageID)) %>%
  as.data.frame()


dbDisconnect(con)

con <- dbConnect(
  RMariaDB::MariaDB(),
  dbname = 'rmi_ga4',
  username = Sys.getenv('DBASE_USER'),
  password = Sys.getenv("DBASE_PWD"),
  host = Sys.getenv('DBASE_IP'),
  port = '3306',
  ssl.ca = normalizePath("C:\\Users\\ghoffman\\OneDrive - RMI\\01. Projects\\DigiCertGlobalRootCA.crt.pem")
)


dfs <- list("traffic_all" = traffic_all, "traffic_social" = traffic_social, 
            "traffic_geography" = traffic_geography,"referrals_media" = referrals_media)


# Should be able to loop import
for(i in dfs){
  dbAppendTable(con, name =  i, value = i)
}

# If not, individual calls
dbWriteTable(con, "referrals_media", referrals_media, append = T)
dbWriteTable(con, "traffic_social", traffic_social, append = T)
dbWriteTable(con, "traffic_geography", traffic_geography, append = T)
dbAppendTable(con, "traffic_all", traffic_all)


write_xlsx(dfs, path = ss)



# Used for importing large historical data
# traffic_all_total <- traffic_all
# traffic_all <- traffic_all_total[135001:nrow(traffic_all_total),]
# 







