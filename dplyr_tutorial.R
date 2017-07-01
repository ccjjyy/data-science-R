library(tidyverse)
library(nycflights13)

flights
View(flights)

#--------------------------
# filter rows with filter()
#--------------------------

filter(flights, month==1, day==1)

filter(flights, month==11 | month==12)
filter(flights, month %in% c(11, 12))

filter(flights, !(arr_delay > 120 | dep_delay >120))
filter(flights, arr_delay <=120, dep_delay <= 120)

#----------------------------
# arrange rows with arrange()
#----------------------------

arrange(flights, year, month, day)

arrange(flights, desc(arr_delay))

#-----------------------------
# select columns with select()
#-----------------------------

select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

select(flights, starts_with("y"))
select(flights, ends_with("r"))
select(flights, contains("y"))
select(flights, matches("(.)\\1"))
select(flights, num_range("x", 1:3))
rename(flights, YEAR=year)
select(flights, time_hour, air_time, everything())

#--------------------------------
# add new variables with mutate()
#--------------------------------

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time)
mutate(flights_sml, 
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

transmute(flights_sml,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

transmute(flights_sml,
          air_time,
          hour = air_time %/% 60,
          minite = air_time %% 60)

#^^^^^^^^^^^^^^^^^^^^^^^^^^
# useful creation functions
#^^^^^^^^^^^^^^^^^^^^^^^^^^

lead(1:10, 2)
lag(1:10, 2)

cumsum(1:10)
cummax(1:10)
cummean(1:10)

x <- c(5, 1, 3, 2, 2, NA)
row_number(x)
min_rank(x)
dense_rank(x)
percent_rank(x)
cume_dist(x)
ntile(x, 2)

#-----------------------------------
# grouped summaries with summarize()
#-----------------------------------

summarize(flights, delay = mean(dep_delay, na.rm=TRUE))

by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm=TRUE))

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# combining multiple operations with the pipe
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

by_dest <- group_by(flights, dest)
delay_data <- summarize(by_dest, 
                   count = n(),
                   dist = mean(distance, na.rm=TRUE),
                   delay = mean(arr_delay, na.rm=TRUE))
delay_data <- filter(delay, count>20, dest != "HNL")
ggplot(data=delay_data, mapping=aes(x=dist, y=delay)) +
  geom_point(aes(size=count), alpha=1/3)+
  geom_smooth(se=TRUE)

#^^^^^^^^^^^^^^^
# missing values
#^^^^^^^^^^^^^^^

delay_data2 <- flights %>% 
  group_by(dest) %>%
  summarize(count=n(),
            dist=mean(distance, na.rm=TRUE), 
            delay = mean(arr_delay, na.rm=TRUE)) %>%
  filter(count>20, dest != "HNL")
ggplot(data=delay_data2, mapping=aes(x=dist, y=delay)) +
  geom_point(aes(size=count), alpha=1/3)+
  geom_smooth(se=TRUE)

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(mean = mean(dep_delay))

#^^^^^^^
# counts
#^^^^^^^

delay_data3 <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay))
ggplot(data=delay_data3, mapping=aes(x=delay)) +
  geom_freqpoly(binwidth=10)

delay_data4 <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay),
            count = n())
ggplot(data = delay_data4, mapping=aes(x=delay, y=count)) + 
  geom_point(alpha=0.1)

delay_data4 %>% 
  filter(count>25) %>%
  ggplot(mapping=aes(x=delay, y=count)) + 
    geom_point(alpha=0.1)

batting <- as_tibble(Lahman::Batting)
batters <- batting %>%
  group_by(playerID) %>%
  summarize(level = sum(H, na.rm=TRUE) / sum(AB, na.rm=TRUE),
            try = sum(AB, na.rm=TRUE))
batters %>%
  filter(try > 100) %>%
  ggplot(mapping=aes(x=try, y=level)) +
    geom_point() + 
    geom_smooth(se=TRUE)

batters %>% arrange(desc(level))

#^^^^^^^^^^^^^^^^^^^^^^^^^
# useful summary functions
#^^^^^^^^^^^^^^^^^^^^^^^^^

not_cancelled %>%
  group_by(dest) %>%
  summarize(carriers = n_distinct(carrier)) %>%
  arrange(desc(carriers))
  
not_cancelled %>%
  count(dest) %>%
  arrange(desc(n))

not_cancelled %>%
  count(tailnum, wt=distance) %>%
  arrange(desc(n))

not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(hour_perc = mean(arr_delay>60)) %>%
  arrange(desc(hour_perc))

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# grouping by multiple variables
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

daily <- group_by(flights, year, month, day)
per_day <- summarize(daily, flights=n())
per_day
per_month <- summarize(per_day, flights=sum(flights))
per_month
per_year <- summarize(per_month, flights=sum(flights))
per_year

daily %>%
  ungroup() %>%
  summarize(flights=n())

#----------------------------
# grouped mutates and filters
#----------------------------

flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay))<10)

popular_dests <- flights %>%
  group_by(dest) %>%
  filter(n()>365) %>%
  select(dest)
