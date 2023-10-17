/* Create Schema */
CREATE SCHEMA rmi_influence_campaign;

/* Select your new schema */
USE rmi_influence_campaign;

/* Create blank table in schema */

CREATE TABLE `allTraffic` ( 
  `uid` int NOT NULL AUTO_INCREMENT, 
  `all` varchar(200), 
  `pageTitle` varchar(200),
  `defaultChannelGroup` varchar(200),
  `type` varchar(200),
  `count` int,
  `totalUsers` varchar(200),
  `engagementDuration` float,
  `avgEngagementDuration` varchar(200),
  `pageType` varchar(200),
  `icon` int,
  `page_views` float,
  `date_added` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  `date_updated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  PRIMARY KEY (`uid`) /*Identifies the primary key field for the table */
) ENGINE=InnoDB AUTO_INCREMENT=22585 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='Useful comment about the table';


CREATE TABLE `socialTraffic` ( /*Name your table */
  `uid` int NOT NULL AUTO_INCREMENT, /*Create your fields with the format 'field name' fieldtype(type length) default settings */
  `pageTitle` varchar(200),
  `source` varchar(200), 
  `Sessions` int,
  `PageViews` int,
  `site` varchar(200),
  `dashboardCampaign` varchar(200),
  `date_added` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  `date_updated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  PRIMARY KEY (`uid`) /*Identifies the primary key field for the table */
) ENGINE=InnoDB AUTO_INCREMENT=22585 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='Useful comment about the table';
					

CREATE TABLE `geographyTraffic` ( /*Name your table */
  `uid` int NOT NULL AUTO_INCREMENT, /*Create your fields with the format 'field name' fieldtype(type length) default settings */
  `pageTitle` varchar(200), 
  `region` varchar(200), 
  `country` varchar(200), 
  `regionPageViews` int,
  `site` varchar(200), 
  `dashboardCampaign` varchar(200), 
  `date_added` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  `date_updated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  PRIMARY KEY (`uid`) /*Identifies the primary key field for the table */
) ENGINE=InnoDB AUTO_INCREMENT=22585 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='Useful comment about the table';


CREATE TABLE `mediaReferrals` ( 
  `uid` int NOT NULL AUTO_INCREMENT, 
  `pageTitle` varchar(200), 
  `sessionSource` varchar(200), 
  `media` varchar(200), 
  `mediaType` varchar(200), 
  `mediaSubtype` varchar(200), 
  `sessions` int,
  `site` varchar(200), 
  `date_added` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  `date_updated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  PRIMARY KEY (`uid`) /*Identifies the primary key field for the table */
) ENGINE=InnoDB AUTO_INCREMENT=22585 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='Useful comment about the table';


CREATE TABLE `campaignNewsletters` ( /*Name your table */
  `uid` int NOT NULL AUTO_INCREMENT, /*Create your fields with the format 'field name' fieldtype(type length) default settings */
  `id` varchar(200), 
  `name` varchar(200),
  `date` date,
  `delivered_` int,
  `unique_opens` int,
  `open_rate` float,
  `unique_clicks` int,
  `unique_CTR` float,
  `UCTRvsAvg` float,
  `story_url` varchar(200),
  `story_title` varchar(200),
  `story_clicks` int,
  `story_COR` float,
  `date_added` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  `date_updated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  PRIMARY KEY (`uid`) 
) ENGINE=InnoDB AUTO_INCREMENT=22585 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='Useful comment about the table';

								
CREATE TABLE `SFcampaigns` (
  `uid` int NOT NULL AUTO_INCREMENT, /*Create your fields with the format 'field name' fieldtype(type length) default settings */
  `CampaignName` varchar(200), 
  `EngagementType` varchar(200), 
  `Icon` int, 
  `Id` varchar(200), 
  `Status` varchar(200), 
  `EngagementDate` Date,
  `Domain` varchar(200), 
  `Email` varchar(200), 
  `DonorType` varchar(200), 
  `AttributtedDonationValue` float,
  `AccountId` varchar(200), 
  `Account` varchar(200),
  `AccountType` varchar(200), 
  `Audience1` varchar(200), 
  `Audience2` varchar(200), 
  `Industry` varchar(200), 
  `Pardot_URL` varchar(200), 
  `Pardot_ID` varchar(200), 
  `GivingCircleTF` int, 
  `SolutionsCouncilTF` int, 
  `InnovatorsCircleTF` int, 
  `OpenOppTF` int, 
  `DonorTF` int, 
  `LapsedDonorsTF` int, 
  `DownloadTF` int, 
  `EventTF` int, 
  `EmailClickTF` int, 
  `Engagements` int, 
  `date_added` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  `date_updated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  PRIMARY KEY (`uid`) /*Identifies the primary key field for the table */
) ENGINE=InnoDB AUTO_INCREMENT=22585 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='Useful comment about the table';

CREATE TABLE `donations` (
  `uid` int NOT NULL AUTO_INCREMENT, /*Create your fields with the format 'field name' fieldtype(type length) default settings */
  `DonationId` varchar(200), 
  `DonationDate` Date,
  `DonationStatus` varchar(200), 
  `EngagementDate` Date,
  `Pardot_ID` varchar(200),
  `Id` varchar(200),
  `EngagementType` varchar(200),
  `CampaignName` varchar(200),
  `Pardot_Score` float,
  `DonorType` varchar(200),
  `TimeDifference` int,
  `DonationValue` float,
  `AttributtedValue` float,
  `count` int,
  `date_added` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  `date_updated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  PRIMARY KEY (`uid`) /*Identifies the primary key field for the table */
) ENGINE=InnoDB AUTO_INCREMENT=22585 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='Useful comment about the table';

								
CREATE TABLE `campaignPosts` ( /*Name your table */
  `uid` int NOT NULL AUTO_INCREMENT, /*Create your fields with the format 'field name' fieldtype(type length) default settings */
  `created_time` Date,
  `account` varchar(200), 
  `post_type` varchar(200), 
  `icon` varchar(200), 
  `tag_id` varchar(200), 
  `tag_name` varchar(200), 
  `impressions` varchar(200), 
  `impressionsVmedian` varchar(200), 
  `impressionsVmean` varchar(200), 
  `engagements` varchar(200), 
  `engagementsVmedian` varchar(200), 
  `engagementsVmean` varchar(200), 
  `engagementRate` varchar(200),
  `engrtVmedian` varchar(200),
  `engrtVmean` varchar(200),
  `postClicks` varchar(200),
  `shares` varchar(200),
  `brand` varchar(200),
  `program` varchar(200),
  `accountType` varchar(200),
  `post` varchar(200),
  `perma_link` varchar(200),
  `text` varchar(200),
  `date_added` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  `date_updated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  PRIMARY KEY (`uid`) /*Identifies the primary key field for the table */
) ENGINE=InnoDB AUTO_INCREMENT=22585 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='Useful comment about the table';

CREATE TABLE `targetCampaign` ( /*Name your table */
  `uid` int NOT NULL AUTO_INCREMENT, /*Create your fields with the format 'field name' fieldtype(type length) default settings */
  `CAMPAIGN_ID` varchar(200), 
  `ID` varchar(200),
  `audiences` varchar(200),
  `promotionTactics` varchar(200),
  `date_added` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  `date_updated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  PRIMARY KEY (`uid`) /*Identifies the primary key field for the table */
) ENGINE=InnoDB AUTO_INCREMENT=22585 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='Useful comment about the table';

CREATE TABLE `contentSummary` ( /*Name your table */
  `uid` int NOT NULL AUTO_INCREMENT, /*Create your fields with the format 'field name' fieldtype(type length) default settings */
  `type` varchar(200), 
  `name` varchar(200),
  `date_added` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  `date_updated` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, /* Useful standard field to include for all tables */
  PRIMARY KEY (`uid`) /*Identifies the primary key field for the table */
) ENGINE=InnoDB AUTO_INCREMENT=22585 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='Useful comment about the table';
