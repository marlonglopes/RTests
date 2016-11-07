#XTS
#eXtensible Time Series

#XTS = matrix * index
x = matrix(1:4, ncol = 2, nrow = 2)
idx = as.Date(c("2015-01-01", "2015-02-01"))

idx

X = xts(x, order.by = idx)

coredata(X, fmt=FALSE)
index(X)


# Load xts

library(xts)

# View the structure of ex_matrix

str(ex_matrix)

# Extract the 3rd observation of the 2nd column of ex_matrix
ex_matrix[3,2]

# Extract the 3rd observation of the 2nd column of core 
core[3,2]

 xts = matrix + times


 # Create the object data using 5 random numbers

data = rnorm(5)

# Create dates as a Date class object starting from 2016-01-01
dates <- seq(as.Date("2016-01-01"), length = 5, by = "days")

# Use xts() to create smith
smith <- xts(x = data, order.by = dates)

# Create bday (1899-05-08) using a POSIXct date class object
bday <- as.POSIXct("1899-05-08")

# Create hayek and add a new attribute called born
hayek <- xts(x = data, order.by = dates, born = bday)


# Extract the core data of hayek

hayek_core = coredata(hayek, fmt=FALSE)

# View the class of hayek_core

class(hayek_core)

# Extract the index of hayek

hayek_index = index(hayek)

# View the class of hayek_index

class(hayek_index)


a <- xts(x = 1:2, as.Date("2012-01-01") + 0:1)
a[index(a)]



# Create dates
dates <- as.Date("2016-01-01") + 0:4

# Create ts_a
ts_a <- xts(x = 1:5, order.by = dates)

# Create ts_b
ts_b <- xts(x = 1:5, order.by = as.POSIXct(dates))

# Extract the rows of ts_a using the index of ts_b
ts_a[index(ts_b)]

# Extract the rows of ts_b using the index of ts_a
ts_b[index(ts_a)]


data(sunspots)
class(sunspots)

sunspots_xts = as.xts(sunspots)
class(sunspots)

head(sunspots_xts)


read.zoo("file")
write.zoo(x, "file")

saveRDS(x, "file")

# Convert austres to an xts object called au
au <- as.xts(austres )

# Convert your xts object (au) into a matrix am
am <- as.matrix(au)

# Convert the original austres into a matrix am2


am2 <- as.matrix(austres)



# Create dat by reading tmp_file

dat = read.csv(tmp_file)

# Convert dat into xts
xts(dat, order.by = as.Date(rownames(dat), "%m/%d/%Y"))

# Read tmp_file using read.zoo
dat_zoo <- read.zoo(tmp_file, index.column = 0, sep = ",", format = "%m/%d/%Y")

# Convert dat_zoo to xts
dat_xts <- as.xts(dat_zoo)




# Convert sunspots to xts using as.xts(). Save this as 

sunspots_xts = as.xts(sunspots)


# Get the temporary file name
tmp <- tempfile()

# Write the xts object using zoo to tmp 
write.zoo(sunspots_xts, sep = ",", file = tmp)

# Read the tmp file. FUN = as.yearmon converts strings such as Jan 1749 into a proper time class
sun <- read.zoo(tmp, sep = ",", FUN = as.yearmon)

# Convert sun into xts. Save this as 
sun_xts = as.xts(sun)


#time series queries

data(edhec, package = "PerformanceAnalytics")

head(edhec["2007-01", 1])


A["201601"]         ## Jan 2016
A["20160125"]       ## Jan 25, 2016
A["201203/201212"]  ## Feb to Dec 2012


# Select all of 2016 from x
x_2016 <- x["2016"]

# Select January 2016 to March 22, 2016
jan_march <- x["201601/20160322"]


#Extracting recurring intraday intervals


# Extract all data from irreg between 8AM and 10AM
morn_2010 <- irreg["T08:00/T10:00"]

# Extract the observations in morn_2010 for January 13th, 2010
morn_2010["2010-01-13"]


x[c(1,2,3,4),]
x[index(x) > "2016-08-20"]


dates = as.POSIXct(c("date1", "date2"))
x[index(dates)]

index = x["date/date", which.i = TRUE]
index

# Subset x using the vector dates

x[dates]

# Subset x using dates as POSIXct

x[as.POSIXct(dates)]


# Replace all values in x on dates with NA
x[dates] <- NA

# Replace dates from 2016-06-09 and on with 0

x[index(x) >= "2016-06-09"] = 0


# Create lastweek using the last 1 week of temps
lastweek <- last(temps, "1 week")

# Print the last 2 observations in lastweek

lastweek

# Extract all but the last two days of lastweek

last(lastweek, "2 days")



# Last 3 days of first week
last(first(Temps, '1 week'),'3 days') 


# Extract the first three days of the second week of temps
first(last(first(temps, "2 weeks"), "1 week"), "3 days")


# Add a and b

a+b

# Add a with the numeric value of b


a + as.numeric(b)


#Math with non-overlapping indexes

merge(b, index(a))


# Add a to b, and fill all missing rows of b wil 0
a + merge(b, index(a), fill = 0)

# Add a to b and fill NAs with the last observation
a + merge(b, index(a), fill = na.locf)


merge(... , fill = NA, join = "outer")


# Basic argument use
merge(a, b, join = "right", fill = 9999)


# Perform an inner join of a and b
merge(a, b, join = "inner")

# Perform of a left-join of a and b, fill mising values with 0
merge(a, b, join = "left", fill =0)



# Row bind temps_june30 to temps, assign this to temps2
temps2 <- rbind(temps_june30, temps)

# Row bind temps_july17 and temps_july18 to temps2, call this temps3
temps3 <- rbind(temps_july17, temps_july18, temps2)


#Handling missingness in your data

Fill NAs with last observation

l.o.c.f - last observation carried forward - last non NA value

#from Zoo

na.locf(object,
		na.rm = FALSE,
		fromLast = FALSE,
		maxgap = Inf
		)

#Replace NAs
na.fill(object,
		fill,
		...

	) 
#Remove NAs

na.trim(object,
		...
	)

na.omit()

#Interpolate values
na.approx(object, ...)


# Fill missing values in temps using the last observation

temps_last = na.locf(temps) 

# Fill missing values in temps using the next observation
temps_next = na.locf(temps, fromLast = TRUE) 


#Lag operators and difference operations

lag(x, k=1, na.pad = TRUE, ...) # k=1 or k=-1

diff(x,
		lag = 1,
		differences = 1,
		arithmetic = TRUE,
		log = FALSE,
		na.pad = TRUE, 
		...
	)


# Your final object
cbind(lead_x, x, lag_x)

lag(x, k = 1) will shift future values one step back in time. 

# Create a leading object called lead_x
lead_x <- lag(x, k = -1)

# Create a lagging object called 

lag_x <- lag(x, k = 1)


# Merge your three series together and assign to z

z = cbind(lead_x, x, lag_x)


# Create a leading object called lead_x
lead_x <- lag(x, k = -1)

# Create a lagging object called 

lag_x <- lag(x, k = 1)


# Merge your three series together and assign to z

z = merge(lead_x, x, lag_x)

# These are the same
diff(x, differences = 2)
diff(diff(x))


difference is to see it as x(t) - x(t-k) where k is the number of lags to go back.

# calculate the first difference of AirPass using lag and subtraction
AirPass - lag(AirPass)

# calculate the first order 12 month difference if AirPass
diff(AirPass, lag = 12, differences = 1)


#What is the key difference in lag between xts and zoo

The k argument in zoo uses positive values for shifting past observations forward.


# Apply and aggregate by time

Apply by Time

period.apply()

split()

endpoints(x, on="years")

edhec_4yr = edhec["1997/2001"]
ep = endpoints(edhec_4yr, "years")

period.apply(edhec_4yr , INDEX = ep, FUN = mean)

apply.montly
apply.daily
apply.quarterly

split.xts


#S# methods for class xts

split(x, f = "months")


edhec.qtrs = split(edhec[, 1], f = "quarters")


endpoints(AirPass, on = "years")
[1] 0 12 24 36 48 60 72 84 96 108 120 132 144

# Locate the weeks
endpoints(temps , on = "weeks")

# Locate every two weeks
endpoints(temps, on = "weeks", k = 2)


For example, the code below locates the last observation of each year for the AirPass dataset.

endpoints(AirPass, on = "years")
[1] 0 12 24 36 48 60 72 84 96 108 120 132 144

# Calculate the weekly endpoints
ep <- endpoints(temps, on = "weeks")

# Now calculate the weekly mean and display the results
period.apply(temps[, "Temp.Mean"], INDEX = ep, FUN = mean)




# Split temps by week
temps_weekly <- split(temps, f = "weeks")

# Create a list of weekly means, temps_avg, and print this list
temps_avg <- lapply(X = temps_weekly, FUN = mean)
temps_avg

Excellent! As you can see, period.apply() is similar to using a combination of split() and lapply().

#Selection by endpoints vs. split-lapply-rbind


# use the proper combination of split, lapply and rbind
temps_1 <- do.call(rbind, lapply(split(temps, "weeks"), function(w) last(w, n = "1 day")))

# create last_day_of_weeks using endpoints()
last_day_of_weeks <- endpoints(temps, on = "weeks")

# subset temps using last_day_of_weeks 
temps_2 <- temps[last_day_of_weeks]


#Converting periodicity

Time series aggregation

convert univariate to a range bars

OHLC
Open , hight, Low, Close

to.period(
		period = "months",
		k = 1,
		indexAt,
		name = NULL,
		OHLC = TRUE
	)

to.period(
		edhec["199701/2001", 1],
		"years",
		name = "EDGEC",
	)

to.period(
		edhec["199701/2001", 1],
		"years",
		name = "EDGEC",
		indexAt = "firstof"
	)

to.period(
		edhec[, 1],
		"years",
		name = "EDGEC",
		indexAt = "firstof",
		OHLC = TRUE
	)

#Convert univariate series to OHLC data

to.period(x,
          period = "months", 
          k = 1, 
          indexAt, 
          name=NULL,
          OHLC = TRUE,
          ...)


 # Convert usd_eur to weekly
usd_eur_weekly <- to.period(usd_eur, period = "weeks", OHLC = TRUE)

usd_eur_weekly

# Convert usd_eur_weekly to monthly
usd_eur_monthly <- to.period(usd_eur_weekly, period = "months")

usd_eur_monthly

# Convert usd_eur_monthly to yearly univariate
usd_eur_yearly <- to.period(usd_eur_monthly , period = "years", OHLC = FALSE)

usd_eur_yearly


edhec[endpoints(edhec, "years"),1]


# Convert eq_mkt to quarterly OHLC
mkt_quarterly <- to.period(eq_mkt, period = "quarters")

# Convert eq_mkt to quarterly using shortcut function
mkt_quarterly2 <- to.quarterly(eq_mkt, name = "edhec_equity", indexAt = "firstof")


#Rolling Windows

#Discrete

lapply
split

#Continuous

rollapply



edhec.yrs =  split(edhec[,1], f = "years")

edhec.yrs =  lapply(edhec.yrs, cumsum)

edhec.ytd =  do.call(rbind, edhec.yrs)

cbind(edhec.ytd , edhec[,1])


rollapply(	data, width, FUN, ...,
			by = 1, by.column = TRUE,
			fill = if (na.pad) NA,
			na.pad = TRUE, partial = TRUE,
			align = c("right", "center", "left")
			)

rollapply(	edhec["200701/08", 1] , 3, mean )



# Split edhec into years
edhec_years <- split(edhec , f = "years")

# Use lapply to calculate the cumsum for each year in edhec_years
edhec_ytd <- lapply(edhec_years, FUN = cumsum)

# Use do.call to rbind the results
edhec_xts <- do.call(rbind, edhec_ytd)

Great job! The split-lapply-rbind syntax may seem complicated, but it is a powerful way to manipulate your time series data.


# Use rollapply to calculate the rolling 3 period sd of eq_mkt
eq_sd <- rollapply(eq_mkt, 3, FUN = sd)


#Index, Attributes, and Timezones

# Get the index class of temps

indexClass(temps)

# Get the timezone of temps

indexTZ(temps)

# Change the format of the time display
indexFormat(temps) <- "MM/DD/YYYY"

# Extract the new index

index(temps)

tzone(x) <- "Time_Zone"


# Construct times_xts with tzone set to America/Chicago
times_xts <- xts(1:10, order.by = times, tzone = "America/Chicago")

# Change the time zone of times_xts to Asia/Hong_Kong
tzone(times_xts) <- "Asia/Hong_Kong"
  
# Extract the current time zone of times_xts

indexTZ(times_xts)


#Periods, Periodicity and Timestamps

#Periodicity

periodicity(edhec)
periodicity(to.yearly(edhec)

#Modifying Timestamps

align.time(x, n=60) # n is in seconds

make.index.unique(x, eps = , drop = FALSE, fromLast = )

# Calculate the periodicity of temps
periodicity(temps)

# Calculate the periodicity of \edhec\


periodicity(edhec)

# Convert edhec to yearly
edhec_yearly <- to.yearly(edhec)

# Calculate the periodicity of edhec_yearlyp

periodicity(edhec_yearly)


Well done! The periodicity() command combined with the to.period() set of commands gives you a simple way to manipulate your time series data.

# Count the months

nmonths(edhec)

# Count the quarters

nquarters(edhec)

# Count the years

nyears(edhec)




# Explore underlying units of temps in two commands: .index() and .indexwday()


.index(temps)
.indexwday(temps)

# Create an index of weekend days using which()
index <- which(.indexwday(temps) == 0 | .indexwday(temps) == 6)

# Select the index
temps[index]

In this exercise, you'll take a look at the weekend weather in our temps data using the .indexwday() command. Note that the values range from 0-6, with Sunday equal to 0. Recall that you can use a logical vector to extract elements of an xts object.

#Modifying timestamps

make.index.unique(x, eps = 1e-4)  # Perturb
make.index.unique(x, drop = TRUE) # Drop duplicates
align.time(x, n = 60) # Round to the minute


# Make z have unique timestamps
z_unique <- make.index.unique(z, eps = 1e-4)

# Remove duplicate times in z
z_dup <- make.index.unique(z, drop = TRUE)

# Round observations in z to the next time
z_round <- align.time(z, n =60)


Great job! These final commands should round out your xts knowledge and give you the complete tools to manipulate time series data in R.

