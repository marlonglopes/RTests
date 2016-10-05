



# hflights is in the workspace as a tbl, with translated carrier names

# Make an ordered per-carrier summary of hflights
hflights %>%
   group_by(UniqueCarrier) %>%
   summarise(p_canc = mean(Cancelled == 1) * 100, 
             avg_delay = mean(ArrDelay, na.rm = TRUE)) %>%
   arrange(avg_delay, p_canc)




# dplyr is loaded, hflights is loaded with translated carrier names

# hflights is in the workspace as a tbl, with translated carrier names

hflights %>%
   filter(!is.na(ArrDelay) & ArrDelay >0) %>%
   group_by(UniqueCarrier) %>%
   summarise(avg = mean(ArrDelay)) %>%
   mutate(rank = rank(avg)) %>%
   arrange(rank)


# Ordered overview of average arrival delays per carrier

# dplyr and hflights (with translated carrier names) are pre-loaded

# How many airplanes only flew to one destination from Houston? adv1
hflights %>%
  group_by(TailNum) %>%
  summarise(ndest = n_distinct(Dest)) %>%
  filter(ndest == 1) %>%
  summarise(nplanes = n())

# Find the most visited destination for each carrier: adv2
hflights %>% 
  group_by(UniqueCarrier, Dest) %>%
  summarise(n = n()) %>%
  mutate(rank = rank(desc(n))) %>%
  filter(rank == 1)


  # Set up a connection to the mysql database
my_db <- src_mysql(dbname = "dplyr", 
                   host = "dplyr.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                   port = 3306, 
                   user = "dplyr",
                   password = "dplyr")

# Reference a table within that source: nycflights
nycflights <- tbl(my_db, "dplyr")

# glimpse at nycflights

glimpse(nycflights)

# Ordered, grouped summary of nycflights
  
nycflights %>%
group_by(carrier) %>%
summarise(n_flights = n(), avg_delay = mean(arr_delay)) %>%
arrange(avg_delay)