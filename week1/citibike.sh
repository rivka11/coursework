#!/bin/bash
#
# add your solution after each of the 10 comments below
#
# Rivka Schuster
# count the number of unique stations
 cut -d, -f5 201402-citibike-tripdata.csv | sort | uniq | wc -l
330
# count the number of unique bikes
cut -d, -f12 201402-citibike-tripdata.csv | sort | uniq | wc -l
5700
# count the number of trips per day
cut -d, -f2 201402-citibike-tripdata.csv | cut -c 10-11 | sort | uniq -c

# find the day with the most rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -c 10-11 | sort | uniq -c | sort -nr | head -n1
# find the day with the fewest rides
cut -d, -f2 201402-citibike-tripdata.csv | cut -c 10-11 | sort | uniq -c | sort -nr | tail -n2 | head -n1
# find the id of the bike with the most rides
cut -d, -f12 201402-citibike-tripdata.csv | sort | uniq -c | sort -nr | head -n1
# count the number of rides by gender and birth year
 cut -d, -f15,14 201402-citibike-tripdata.csv | sort | uniq -c
# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
cut -d, -f5 201402-citibike-tripdata.csv | grep '[0-9].*[^&].*[0-9].*' | wc -l
 awk -F, '$5 ~ /[0-9].*&.*[0-9]/' 201402-citibike-tripdata.csv | wc -l
# compute the average trip duration
cut -d, -f1 201402-citibike-tripdata.csv | tr -d '"' | awk -F, 'NR>1 {total += $1; n++ } END { if (n > 0) print total / n; }'
#need to subtract 1 from above answer counts header row. Also have 