# Data Dictionary: Ad Lib

File: `AdLibAll.csv`  
Type: CSV
Description: Data fetched from FB Graph API (node: `ads_archive`). It contains information on political and social ads run in the region of AT during monitoring period (08.09.-29.09.2019), resp. from 01.08.2019 until election day if no delivery stop date was provided.  
Collection date: 08.10.2019  
Collections script: `src/data/scrape_ad_lib.py`  

## Schema

| Field                    | Description                                   | Possible Values                                  |
|--------------------------|-----------------------------------------------|--------------------------------------------------|
| ad_creation_time         | UTC datetime when ad was created              | 2019-09-11T13:21:19+0000                         | 
| ad_creative_body         | Text of ad                                    | Hello, ad. OR _blank_                            |
| ad_creative_link_caption | If link in ad, text in it                     | Hello, ad in link OR _blank_                     |
| ad_creative_link_description | If link in ad, text description of it | Hello, description of ad OR _blank_              | 
| ad_creative_link_title   | If link in ad, title                          | Hello, title. OR _blank_                         |
| ad_delivery_start_time   | When delivery should be started               | 2019-09-11T13:21:19+0000                         |
| ad_delivery_stop_time    | When delivery should be stopped (optional!)   | 2019-09-11T13:21:19+0000 OR _blank_              |
| ad_snapshot_url          | URL to persisted version of ad                | https://www.facebook.com/ads/archive/render_ad/?id=1234 |
| currency                 | Currency with which ad was payed              | EUR                                              |
| demographic_distribution | Demographics (age, gender) of people reached  | `[{'percentage': '0.1', 'age': '65+', 'gender': 'female'}]` |
| funding_entity           | Name of person, comp, entity which funded ad  | XPÖ                                              |
| impressions              | Number of times ad created an impression      | Ranges: <1000, 1K-5K, 5K-10K, 10K-50K, 50K-100K, 100K-200K, 200K-500K, >1M |
| page_id                  | Numeric ID of ad                              | 12345678910                                      |
| page_name                | Fan page name                                 | XPÖ Wien                                         |
| region_distribution      | Regional distribution of peoples reached      | `[{'percentage': '1', 'region': 'Upper Austria'}\]` |
| spend                    | Amount of money spent for running ad          | Ranges; <100, 100-499, 500-999, 1K-5K, 5K-10K, 10K- 50K, 50K-100K, 100K-200K, 200K-500K, >1M |

Reference: [API Docs](https://developers.facebook.com/docs/marketing-api
/reference/archived-ad/)
