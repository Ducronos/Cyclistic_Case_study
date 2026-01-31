# **CYCLIST CASE STUDY ANALYSIS REPORT**

Author: Minh Duc

Publish: 8 Jan 2026

Full process at [Process Report](https://ducronos.github.io/Cyclistic_Case_study/)

##

![BI Dashboard](/BI%20Dashboard-1.png)

## Introduction

**Cyclistic:**
A fictional company which provide bike-shared service. The Marketing Diretor define the company's success is tied along with the number of annual memberships. Therefore, I and my team want to understand how casual riders and annual memberships use company's service differently. From these insights, our team will design a new marketing strategy to convert casual riders to annual members.

**Required Tasks:**

* Examine the key differences between these two types of customers, what differences might lead them to sign up for annual passes, base on data in most recent year, which will be possible for the marketing team to set up campaigns.

* Seeing the differences between casual riders and annual members might reveal the reason between customer's decisions, or the changes in their mind. So that the company can plan for campaigns which will bring more customers with the same conditions or encourage currently customers and transform them to potential customers who are ready to buy memberships.

**Data:**

* Name: Cyclistic Bike-Share
* Provider: Cyclistic Co.ltd
* Description: The dataset contain records of individual trips made my customers in Quarter 1, 2019 and Quarter 1, 2020. Each record contains information about identities of related objects, information of the trip such as trip duration, station's name and side information of the customers

* Time period: 2 tables with each table contains the records of 1 year Quarter in 2019 and 2020
* Key variables:
  * trip_id, ride_id: unique key for each record
  * bike_id, from_station_id, to_station_id: unique identity for bikes and stations
  * start_station_name, end_station_name: station where each trip starts and ends
  * tripduration: duration of the trips in seconds
* Limitation and note:
  * The dataset mostly includes data of the trip rather than demographic data of the customers.
  * Minor of the trips have too short or too long duration, or invalid start time and end time, the age of some of the customers are too great
  * The data credibility is rely on the accuracy of the sensors and available stored information of the users.

## Analysis

**Member riders primarily use the service during commute hours (early morning and late afternoon) and are more likely to take round trips than casual riders.**

* The question implied that there are difference in the distribution of usage between two types of riders during the day.

* To answer this, we compare the distribution of trips between two types of riders through week days. Measure portion of riders in peak riders to see the differences in behavior. And validate the pre-conclusion with Median and IQR.

* Higher portion of member ride in peak hour in early morning and late afternoon compare to casual riders. The IQR measure have noticed tails in the data, however these tails represent another stable behavior and does not affect the conclusion. Through comparing the distribution of user types.
  * We have also noticed that usage behaviors between these user are also different in weekend. Casual riders portion is higher than member riders.

* This suggests that Member usage is more routine-based, whereas Casual usage includes many irregular or leisure trips.

**How do trip durations differ between member riders and casual riders?**

* Base on the understand about user type, the question hope to see that member's trips has shorter duration and more consistent in behavior.

* To answer the question, we compare the shape of the duration distribution between rider types through Boxplot graph, median and IQR.

* The comparision shows shorter median duration of member's ride, with narrower interquartile range and also narrower and lower range in duration.

* Shorter and more stable trip durations among members suggest a routine-driven usage pattern, likely associated with commuting or regular daily activities rather than exploratory or leisure trips.

**Members tend to repeatedly use a limited number of familiar routes, while casual riders explore a wider variety of routes.**

* To assess route concentration differences between member and casual riders, trips were grouped by origin–destination route and ranked in descending order based on trip frequency for each user type. A cumulative trip percentage curve was constructed to examine how total trips accumulate across ranked routes. In addition, two summary metrics were computed: (1) the number of distinct routes required to account for 50% of total trips, and (2) the proportion of trips contributed by the top 10 most frequently used routes.

* The cumulative trip distribution for casual riders increases more steeply than that for member riders, indicating that a smaller number of routes accounts for a larger share of total trips. Consistent with this pattern, casuals require fewer distinct routes to reach 50% of total trips. Additionally, the top 10 routes contribute a higher proportion of trips among casuals compared to member riders, whose trips are distributed more evenly across a wider set of routes.

* These findings do not support H6. Instead, the results suggest that casual riders concentrate their trips on a smaller subset of routes, while members distribute their rides more broadly across the network. This pattern indicates that casual usage may be driven by popular, well-known routes, whereas members engage in more diverse and flexible route choices.

**Membership adoption tends to increase with age but levels off among middle-aged and older users**

* We compare portion of membership between age group. Then the trip duration distribution between them, compare the IQR and median duration to validate the statement.

* Portion of membership increase along with the age group, the distributions of the duration are also tied along with them, the older the age group is, the more stable their rides's duration are.

* Older users are more likely to become members and use the service in a more predictable, utilitarian way, while younger users show more variable and exploratory usage patterns.

## Conclusion

Overall, the analysis reveals clear and consistent behavioral differences between member and casual riders. Member usage is characterized by shorter and more stable trip durations, stronger weekday peak-hour patterns, and broader route diversity, indicating a routine- and utility-driven usage pattern. In contrast, casual riders exhibit longer and more variable trips, higher weekend activity, and greater concentration on a limited set of popular routes, suggesting more leisure- and exploration-oriented behavior.

These findings highlight that membership status is strongly associated with predictable, habitual travel behavior, while casual usage reflects more discretionary and situational riding patterns.

## Appendix

### Cumulative Trip Percentage

The cumulative trip percentage shows how quickly total trips are accumulated as routes are ranked by usage frequency.

### Trip duration IQR by Age group (statistic in seconds)

![IQR Info](/IQR%20Duration%20by%20Age%20Group%20.png)
