
# GA4 ETL Script Documentation

This repo is used to pipeline GA4 and website metadata to MySQL via R.

## Contents

- GA4_etl: Main data processing script, leveraging functions from functions.r
- functions.r: Includes all custom functions for GA4 data extraction and transformation
- packages.r: Packages used for project. Could be updated to a requirements file, but functions as is.

## Target Metrics
This script extracts the following metrics from GA4:
- Traffic source (acquisition)
    - Note: Take a look at the referral categories (May want to parse out more granular to differentiate different news outlets vs. other RMI pages vs. social traffic)
- Geography
    - State level is the ultimate target
- Engaged page views
- Number
- Rate
- Time spent on page
- Users
    - Returning
    - New
- Conversions
- Report downloads (Custom GA4 Goal)
- Online donations (Custom GA4 Goal)
- Event reg (Custom GA4 Goal)
- Email sign up (Custom GA4 Goal)


## Prerequisites

Before running this script, make sure you have the following:

- R and RStudio installed on your machine.
- Required packages and functions sourced from `packages.R` and `functions.R`.
- A `.env` file containing the necessary environment variables for database connection:
    - Monday_Token
    - SproutSocial_Token
    - Pardot_TokenV4
    - Pardot_TokenV5 
    - Pardot_Business_ID
    - sf_password
    - sf_user
    - DBASE_USER
    - DBASE_PWD
    - DBASE_IP

## Setup

1. Set the working directory to the appropriate folder using `setwd()` function.
2. Source the required packages and functions using `source()` function.
3. Load the environment variables from the `.env` file using `load_dot_env()` function.

## Database Connection

Establish a connection to the MySQL database using the `dbConnect()` function from the `RMariaDB` package. Provide the necessary credentials and SSL certificate path.

## Get Referral Sites

Read the referral site categories from the `Referral Site Categories.xlsx` file using the `read.xlsx()` function. Store the data in the `referralSites` variable.

## Get Most Recent Date Added

Retrieve the most recent `date_added` value from the `traffic_all` table in the MySQL database using a SQL query. Store the result in the `mostRecent` variable.

## Date Range

Set the date range for the GA4 data retrieval. Use the `paste()` function to create the necessary date strings.

## GA Authentication

Authenticate with the GA API using the `ga_auth()` function. Provide the email associated with the GA account.

## Set GA Variables and Property ID

Set the GA variables and property ID for the GA4 data retrieval. Use the `ga_meta()` function to retrieve the metadata for the specified property ID.

## Export Configuration

Set the export configuration for the data retrieval. Specify the file name and path for the exported data.

## Database Connection (Again)

Re-establish the database connection to the MySQL database.

## Data Retrieval and Processing

Retrieve and process the necessary data from GA4 using various functions like `ga_data()`, `getPageData()`, `getPageMetrics()`, `getAcquisition()`, `getTrafficSocial()`, `getTrafficGeography()`, and `getReferrals()`.

## Database Operations

Perform database operations like importing new pages, updating page titles, and storing the retrieved data in the appropriate database tables using the `dbWriteTable()` and `dbAppendTable()` functions.

## Export Data

Export the retrieved and processed data to an Excel file using the `write_xlsx()` function.

## Cleanup

Disconnect from the MySQL database using the `dbDisconnect()` function.

Remember to customize the script according to your specific requirements and database configuration.

