# Data Wrangling in R
# Spring 2020
# UVa Library - Research Data Services
# Clay Ford


# load packages
library(tidyverse)
library(lubridate)



# Download Data for Workshop ----------------------------------------------

# The data we'll use in this workshop is available in a 4 MB ZIP file on my UVa
# people page. The following code will download and unzip the file. Before
# running the following code you may want to set your working directory to your
# Desktop or a new folder with a name like "DWR_workshop_01".

# To set working directory: 
# Session...Set Working Directory...Choose Directory...

URL <- "http://people.virginia.edu/~jcf2d/data/DWR_data.zip"
d <- basename(URL)
download.file(url = URL, destfile = d)
unzip(d)
setwd("DWR_data")
rm(URL, d)


# Binding -----------------------------------------------------------------

# Let's create some fake data to demonstrate. The tibble function creates data
# frames called "tbl_df", or "tibbles" for short. The main difference between a
# tibble and a data frame is the way they are printed to the console.

dat01 <- tibble(x = 1:5, y = 5:1)
dat01
dat02 <- tibble(x = 10:16, y = x/2)
dat02
dat03 <- tibble(z = runif(5)) # 5 random numbers from interval (0,1)
dat03

# row binding
# ie, stack data frames

# dplyr's bind_rows() works on data frames or a list of data frames. Columns are
# matched by name, and any missing columns will be filled with NA.

# stack dat01 and dat02
bind_rows(dat01, dat02)

# save the new stacked data frame
dat04 <- bind_rows(dat01, dat02)
dat04

# we can use the same data frames multiple times
bind_rows(dat01, dat02, dat01)

# Example of binding data frames with no matching columns.
bind_rows(dat01, dat03)

# We can use the optional ".id" argument to create a new column that contains an
# identifier for the original data.
bind_rows(dat01, dat02, .id = "id")

# This might be useful if you were row binding multiple data sets for, say,
# different classrooms and you wanted a classroom-level identifier.

# Naming the data frames that we're binding provides a useful label in the id 
# column.
bind_rows("dat01" = dat01, "dat02" = dat02, .id = "id")

# bind_rows() also works on lists of data frames
list01 <- list("dat01" = dat01, "dat02" = dat02)
list01
bind_rows(list01)
bind_rows(list01, .id = "source")

# The extended example below demonstrates how this can be very handy.

# column binding
# ie, set data frames side-by-side

# dplyr's bind_cols() works on data frames or a list of data frames. Rows are
# matched by position, so all data frames must have the same number of rows.

# pace dat01 and dat03 side-byside
bind_cols(dat01, dat03)

# The following throws an error since the data frames do not share the same
# number of rows.
bind_cols(dat01, dat02)


# Extended example: reading multiple data files ---------------------------


setwd("stocks")
# get file names
stocks <- list.files()  
# apply read_csv to each file name; return a list
stocks_ls <- lapply(stocks, read_csv)  

# lapply(stocks, read_csv) essentially does the following:
# stocks_ls <- list(read_csv("bbby.csv"), 
#      read_csv("flws.csv"), 
#      read_csv("foxa.csv"), 
#      read_csv("ftd.csv"), 
#      read_csv("tfm.csv"), 
#      read_csv("twx.csv"), 
#      read_csv("viab.csv"))

# stocks_ls is a list of 7 data frames
length(stocks_ls)

# The seven list elements have no name
names(stocks_ls)

# Before row binding, let's name each list element so we can use the .id
# argument to identify the stock in the final data frame.

# name each list element (replace ".csv" with nothing)
names(stocks_ls) <- str_remove(stocks, ".csv")

# Use bind_rows to combine all data frames in list to one data frame.
# Use the .id argument to add a column indicating the stock
stocks_df <- bind_rows(stocks_ls, .id = "stock")

# first six records
head(stocks_df)

# Let's work with the dates

# Convert Date to actual date value using lubridate's dmy() function
stocks_df$Date <- dmy(stocks_df$Date)

# Extract day of week and save into new column called "Day"
stocks_df$Day <- wday(stocks_df$Date, label = TRUE)

# Extract month of year and save into new column called "Month"
stocks_df$Month <- month(stocks_df$Date, label = TRUE)

# Calculate change from opening to closing price
stocks_df$Change <- stocks_df$Open - stocks_df$Close

# first six records
head(stocks_df)

# plot closing price over time for all stocks
ggplot(stocks_df, aes(x = Date, y = Close, color = stock)) +
  geom_line() 

# boxplots of change by day of week for each stock
ggplot(stocks_df, aes(x = Day, y = Change)) +
  geom_boxplot() +
  facet_wrap(~stock)


# YOUR TURN #1 ------------------------------------------------------------

# VA DOE: Public high school graduate and completer data for 2006 - 2016.
# http://www.doe.virginia.gov/statistics_reports/research_data/index.shtml

# Read in all CSV files and bind into one data frame called "grads_df".

# TIP: look at one of the CSV files before starting. Do you need to use the .id
# argument?
setwd("doe")
grads <- list.files()
grads_ls <- lapply(grads, read_csv)
grads_df <- bind_rows(grads_ls)


# Merging/Joining ---------------------------------------------------------

# Let's create some fake data to demonstrate merging/joining data:
left <- data.frame(id=c(2:5),
                   x=c(90, 93, 99, 89))
left
right <- data.frame(id=rep(1:4,each=2),
                    y=c("a", "d", "e", "c", "e", "d", "a", "b"))
right


#### left join

# If we want to retain everything in the left data frame and merge only what 
# has a matching id in the right data frame, we do a LEFT JOIN.
left_join(left, right, by = "id")

# Notice rows from the left data frame are recycled to match up with multiple id
# matches in the right data frame. Also notice all rows from left are retained 
# and NAs are created in the grp column where the left had no matching id. This is
# why it's called a "left join".

#### right join

# If we want to retain everything in the right data frame and merge only what 
# has a matching id in the left data frame, we do a RIGHT JOIN.
right_join(left, right, by = "id")

# Notice rows from the left data frame are recycled to match up with multiple id
# matches in the right data frame. Also notice all rows from right are retained.
# This is why it's called a "right join".

#### inner join

# If we want to retain only those rows with matching ids in both data sets, we
# do an INNER JOIN.
inner_join(left, right, by = "id")

# Notice y from the left data frame is recycled to match up with multiple id 
# matches in the right data frame. Also notice only those records with matching
# "by" variables are joined. 

#### full join

# If we wanted to merge all rows regardless of match, we do a FULL JOIN.
full_join(left, right, by = "id")

# Notice all rows from both data frames are retained and NAs are created in 
# columns where rows did not have matching ids in the other data set. 


#### merging with multiple keys

# Sometimes we have more than one key in each data frame that we want to merge
# on. In that case we give the by argument a vector of keys
dat11 <- tibble(REGION = c(1,1,2,2), 
                STATE = c(1,2,1,2), 
                x = runif(4))
dat11

dat12 <- tibble(REGION = c(1,2,3), 
                STATE = c(2,1,1), 
                y = c("a","b","c"))
dat12

# left join
left_join(dat11, dat12, by = c("REGION", "STATE"))
left_join(dat11, dat12, by = "REGION")



#### merging with multiple keys and keys with different names

# The examples above were clean: The keys had the same names. It's rarely that
# simple in real life. Below is an example of how to merge data using multiple
# keys with different names.

dat11 <- tibble(id1 = c(1,1,2,2), 
                id2 = c(1,2,1,2), 
                x = runif(4))
dat11

dat12 <- tibble(REGION = c(1,2,3), 
                STATE = c(2,1,1), 
                y = c("a","b","c"))
dat12

# Let's say columns "id1" and "id2" in dat11 correspond to columns "REGION" and 
# "STATE" in dat12. To perform a left join with these data frames using dplyr
# functions we use a named vector.

left_join(dat11, dat12, by = c("id1" = "REGION", "id2" = "STATE"))

# The same modifications will work for right joins, inner joins, and full joins.



# dplyr also provides functions for performing "filtering joins" which is a way
# to check if rows in one data frame have (or do not have) membership in another
# data frame.

# Once again let's create some fake data to demonstrate. 


ex01 <- tibble(id = 1:5, 
               name = c("Rick", "Morty", "Jerry", "Beth", "Summer"),
               age = c(67, 15, 42, 39, 17))
ex01
ex02 <- tibble(ID = 1:3, 
               GRP = c(1, 1, 2))
ex02

#### semi join

# all rows in ex01 that have a match in ex02 
semi_join(ex01, ex02, by = c("id" = "ID"))

#### anti join

# all rows in ex01 that do NOT have a match in ex02
anti_join(ex01, ex02, by = c("id" = "ID"))



# Extended example: merging Apple stock data with Google Trends data

# set working directory up one level
setwd("..")

# read in historical stock data for Apple (obtained from Yahoo Finance)
aapl <- read_csv("AAPL.csv")
summary(aapl)
head(aapl$Date)

# format date
aapl$Date <- mdy(aapl$Date)
aapl$Day <- wday(aapl$Date, label = TRUE)
summary(aapl)


# read in Google trends data for "new macbook pro 2018"
gt <- read_csv("mac_book_pro_trends.csv",
               skip = 3, 
               col_names = c("Date","Interest"))
summary(gt)

# add Day column
gt$Day <- wday(gt$Date, label = TRUE)

# Let's merge the stock data with the Google Trends data. It appears we can
# merge on Date.
names(aapl)
names(gt)

inner_join(aapl, gt, by = "Date")

# However all stock data is Monday - Friday while Google trends data is weekly
# on Sunday. No records in common by Date so inner_join returns an empty data
# frame.

table(aapl$Day)
table(gt$Day)

# Change google trends date to Monday by adding 1
gt$Date <- gt$Date + 1
gt$Day <- wday(gt$Date, label = TRUE)
table(gt$Day)


# Perform inner join to merge records from both data frames with matching dates,
# and save as a new data frame.
aapl_gt <- inner_join(aapl, gt, by = c("Date","Day"))
aapl_gt


# Is there any association between google trends and closing price? Plot Closing
# Price vs Interest and add a smooth trend line
ggplot(aapl_gt, aes(x = Interest, y = Close)) + 
  geom_point() +
  geom_smooth(se = F)


# YOUR TURN #2 ------------------------------------------------------------

# Read in school population totals for all Virginia schools for the 2016-2017
# year
va_schools  <- read_csv("va_schools_2016-2017.csv")
names(va_schools)

# (1) remove non-alpha-numeric characters from column names
names(va_schools) <- str_remove_all(names(va_schools), pattern = "[^[:alnum:]]")
names(va_schools) <- str_remove_all(names(va_schools), pattern = "[[:punct:][:space:]]")


# (2) add leading 0s to DivNo and SchoolNo. DinNo should be width 3 (eg "001");
# SchoolNo should be width 4 (eg, "0001").
va_schools$DivNo <- str_pad(va_schools$DivNo, width = 3, side = "left", pad = "0")
va_schools$SchoolNo <- str_pad(va_schools$SchoolNo, width = 4, side = "left", pad = "0")

# (3) subset grads_df to only include rows where SCHOOL_YEAR == "2016-2017" and
# name the new data frame "grads_df_2016_2017"
grads_df_2016_2017 <- filter(grads_df, SCHOOL_YEAR == "2016-2017")


# (4) Merge the grads_df_2016_2017 and va_schools data frames based on Division
# and School Number such that all rows from grads_df_2016_2017 are retained.
# Save the new data frame as va2016_2017
names(grads_df_2016_2017)[c(3,5)]
names(va_schools)[c(1,3)]



va2016_2017 <- left_join(grads_df_2016_2017, va_schools, by = c("DIV_NUM" = "DivNo", 
                                                                "SCH_NUM" = "SchoolNo"))


# With this combined data, we can look at, say, schools with the highest rate of
# economically disadvantaged completers.

# First calculate percentage of completers
va2016_2017$pctComplete <- va2016_2017$HS_COMPLETER_CNT/va2016_2017$Grade12

# Next subset data to eliminate redundant and unnecessary rows
va_completers <- subset(va2016_2017, !is.na(FEDERAL_RACE_CODE) & !is.na(GENDER) & 
                          !is.na(DISABILITY_FLAG) & !is.na(LEP_FLAG) & 
                          !is.na(DISADVANTAGED_FLAG) & DISADVANTAGED_FLAG == "Y",
                        select = c(SCH_NAME, GENDER, HS_COMPLETER_CNT, DISADVANTAGED_FLAG, 
                                   LEP_FLAG, DISABILITY_FLAG, FEDERAL_RACE_CODE, Grade12, pctComplete)) 

# sort in descending order by pctComplete
# arrange is a dplyr function
va_completers <- arrange(va_completers, desc(pctComplete))

# look at top 20
head(va_completers, n = 20)

# Using the tidyverse dplyr method with "pipes":
va2016_2017 %>% 
  mutate(pctComplete = HS_COMPLETER_CNT/Grade12) %>% 
  filter(!is.na(FEDERAL_RACE_CODE) & !is.na(GENDER) & 
           !is.na(DISABILITY_FLAG) & !is.na(LEP_FLAG) & 
           !is.na(DISADVANTAGED_FLAG)) %>% 
  filter(DISADVANTAGED_FLAG == "Y") %>% 
  arrange(desc(pctComplete)) %>% 
  select(SCH_NAME, GENDER, HS_COMPLETER_CNT, Grade12, pctComplete) %>% 
  head(n=20)



# Reshaping ---------------------------------------------------------------

# It's often helpful to think of data as "wide" or "long". 

# When there are multiple occurrences of values for a single observation in one
# row, the data is said to be wide. 

# When there are multiple occurrences of values for a single observation in
# multiple rows, the data is said to be long.

# Example of a wide data frame. Notice each person has multiple test scores
# that span columns.
wide <- data.frame(name=c("Clay","Garrett","Addison"), 
                   test1=c(78, 93, 90), 
                   test2=c(87, 91, 97),
                   test3=c(88, 99, 91))
wide

# Example of a long data frame. This is the same data as above, but in long
# format. We have one row per person per test.
long <- data.frame(name=rep(c("Clay","Garrett","Addison"),each=3),
                   test=rep(1:3, 3),
                   score=c(78, 87, 88, 93, 91, 99, 90, 97, 91))
long

# The long format is actually preferable for many scenarios in R. Hadley Wickham
# coined a term called "tidy data" to describe it. In tidy data, each variable 
# is a column and each observation is a row. Here we have 3 variables: name,
# test, and score. Each row represents a single obervation on a student. 

# With data in this format we can easily summarize and plot the data. For example:

# mean score per student
aggregate(score ~ name, data = long, mean)
aggregate(score ~ test, data = long, mean)

# line plot of scores over test, grouped by name
ggplot(long, aes(x = factor(test), y = score, color = name, group = name)) +
  geom_point() +
  geom_line() +
  xlab("Test")


# R for Data Science has a chapter called Tidy Data that goes into further detail:
# http://r4ds.had.co.nz/tidy-data.html


#### reshape wide to long

# The tidyverse package tidyr provides functions for reshaping data. To reshape
# wide data into long format we use the gather() function.

# Syntax: gather(data, key, value, columns to gather) where...

# - data is your data frame
# - key is the name of the new key column
# - value is the name of the new value column
# - and the last part is names or numeric indices of columns to collapse (or to
#   exclude from collapsing).

# The key argument will be the column that contains what were previously column 
# headers. 

# The value argument will be the column that contains the values that were
# previously under the column headers that are now under the key column.

# The remaining arguments are the columns we want to "gather" (or not gather)

# Hopefully a demonstration will make this clear. Let's make our example wide
# data frame long.

gather(wide, key = test, value = score, test1, test2, test3)

# Notice the "test" column contains what were previously the column headers and 
# that the score column contains the values that were previously under the wide 
# column headers (test1, test2, and test3). We list "test1, test2, test3" in the
# gather functions because those are the columns we wish to gather.

# Other ways to accomplish the same thing:

# specify range of columns to gather using ":"
gather(wide, key = test, value = score, test1:test3)

# specify to gather all columns except "name" using a minus sign
gather(wide, key = test, value = score, -name)

#### reshape long to wide 

# This is less common. For this we use the tidyr function spread().

# Basic syntax: spread(data, key, value) where...

# - data is a data frame, 
# - key is name of the column with the (unique) values you want turned into
#   column headers
# - value is the name of the column that has the values you want placed under
#   your new column headers.

# Let's reshape our example long data frame wide.
long
# values in test column become column headers, values in score column go under
# the new column headers.
spread(long, key = test, value = score)

# Using the sep argument allows us to create column headers of the form
# "<key_name><sep><key_value>"
spread(long, key = test, value = score, sep = "")
spread(long, key = test, value = score, sep = "-")

# For more on tidyr, see the tutorial I wrote in 2016:
# http://data.library.virginia.edu/a-tidyr-tutorial/


# Extended example: reshape stocks_df to be "long"

# If we examine the column headers of stocks_df we can see that we really have
# five variables instead of seven: (1) Stock, (2) Date, (3) price type, (4)
# price, and (5) volume. Below we use gather() to make the data "tidy".

stocks_df
stocks_df_long <- gather(stocks_df, key = price_type, value = price, Open:Close)
head(stocks_df_long)

# With our data in "long" format we can create plots like this:
ggplot(subset(stocks_df_long, price_type %in% c("High","Low")), 
       aes(x = Date, y = price, color = price_type)) +
  geom_line() +
  facet_wrap(~stock, scales = "free") 


# YOUR TURN #3 ------------------------------------------------------------


# The file "mhp.csv" contains data on the number of patients in mental hospitals
# (in thousands), by type of hospital, from 1904-1970. Each year's data was
# estimated in July.

mhp <- read.csv("mhp.csv")
head(mhp)

# Reshape mhp to have three columns: Year, HospitalType, and NumberPatients.
# Save the new data frame as "mhp_long". The first few rows of mhp_long should
# look like this:

#   Year  HospitalType  NumberPatients
# 1 1923       Federal              29
# 2 1931       Federal              12
# 3 1933       Federal              19





# If reshaped correctly, the following code should produce a plot with
# State hospitals showing a steady increase through about 1955 and then a steep
# decline, while Federal and Private numbers remained flat. 
ggplot(mhp_long, aes(x=Year, y=NumberPatients, 
                     color=HospitalType, group=HospitalType)) + 
  geom_line() + 
  labs(y="Number of Patients (Thousands)")



# About the data ----------------------------------------------------------

# Stock data from Yahoo and Google Finance:
# https://finance.yahoo.com/
# https://www.google.com/finance

# Google Trends: https://trends.google.com/trends/?geo=US

# Virginia Department of Education:
# http://www.doe.virginia.gov/statistics_reports/research_data/index.shtml

# Steckel, Richard H. , "Patients in mental hospitals, by type of hospital:
# 1904-1970 ." Table Bd212-216 in Historical Statistics of the United States,
# Earliest Times to the Present: Millennial Edition, edited by Susan B. Carter,
# Scott Sigmund Gartner, Michael R. Haines, Alan L. Olmstead, Richard Sutch, and
# Gavin Wright. New York: Cambridge University Press, 2006.
# http://dx.doi.org/10.1017/ISBN-9780511132971.Bd63-24010.1017/ISBN-9780511132971.Bd63-240



# Base R and the tidyverse ------------------------------------------------

# Due to R's power, flexibility, and maturity, there are numerous ways to 
# accomplish data wrangling tasks in R. One way is to use "Base R" functions, 
# which are the functions that come with R. No need to install extra packages. 
# Just open R (or RStudio) and they're ready to go. If you learned R prior to
# about 2012, this is most likely how you learned to work with data.

# Another increasingly popular way to work with data in R is using what are 
# called the "tidyverse" packages. I'll quote their web site: "The tidyverse is 
# a collection of R packages that share common philosophies and are designed to 
# work together." The tidyverse packages provide an alternative to the "Base R
# way" of working with data, particularly (but not exclusively) data frames.

# This post at Revolution Analytics nicely lays out the tidyverse philosophy: 
# http://blog.revolutionanalytics.com/2016/09/tidyverse.html

# As it says: "tidyverse puts a complete suite of modern data-handling tools 
# into your R session, and provides an essential toolbox for any data scientist 
# using R."

# You can also learn more at http://tidyverse.org/ and by reading R for Data
# Science at http://r4ds.had.co.nz/


# The verbs of data manipulation ------------------------------------------

# Data manipulation, or data wrangling, usually involves performing the same
# routine actions on a data set you've imported. You can think of the following
# as data wrangling verbs, but they are also dplyr functions:

# - select: selecting (or not selecting) columns based on their names 
#           (eg: select columns Q1 through Q25)

# - slice: selecting (or not selecting) rows based on their position 
#           (eg: select rows 1:10)

# - mutate: add or derive new columns (or variables) based on existing columns 
#           (eg: create a new column that expresses measurement in cm based on 
#           existing measure in inches)

# - rename: rename variables or change column names 
#           (eg: change "GraduationRate100" to "grad100")

# - filter: selecting rows based on a condition 
#           (eg: all rows where gender = Male)

# - arrange: ordering rows based on variable(s) numeric or alphabetical order
#            (eg: sort in descending order of Income)

# - sample: take random samples of data 
#           (eg: sample 80% of data to create a "training" set)

# Two other functions of note:

# - summarize: condense or aggregate multiple values into single summary values
#              (eg: calculate median income by age group)

# - group_by: convert a tbl into a grouped tbl so that operations are performed 
#            "by group"; allows us to summarize data or apply verbs to data by 
#            groups (eg, by gender or treatment)


# Last but not least, the pipe: %>% 
# Use Ctrl + Shift + M (Win) or Cmd + Shift + M (Mac) to enter in RStudio

# The pipe takes the output of a function and "pipes" into the first argument of
# the next function.

# Example: Instead of this...
round(mean(stocks_df$Open))
# with pipes we can do this...
mean(stocks_df$Open) %>% round()

# Read a pipe as "then"

# Some dplyr examples:

# (1) Take the stocks data frame, then group by stock, and then calculate the
# mean closing price:

stocks_df %>% 
  group_by(stock) %>% 
  summarize(mean_open = mean(Open))

# With summarize we can create many summaries
stocks_df %>% 
  group_by(stock) %>% 
  summarize(mean_open = mean(Open),
            mean_close = mean(Close),
            median_open = median(Open),
            median_close = median(Close))

# (2) Take the stocks data frame, then sort by closing price in descending
# order, then only show date, stock and closing price columns, and then show the
# top 10 results
stocks_df %>%
  arrange(desc(Close)) %>% 
  select(Date, stock, Close) %>% 
  top_n(n = 10)

# (3) Take the stocks data frame, then group by stock, then sort by closing
# price in descending order, then only show date, stock and closing price
# columns, and then show the top 10 results for EACH STOCK
stocks_df %>%
  group_by(stock) %>% 
  arrange(desc(Close)) %>% 
  select(Date, stock, Close) %>% 
  top_n(n = 10)

# The above works, but only shows the first few rows. That's how tibbles print.
# Append as.data.frame() to see all rows in the console
stocks_df %>%
  group_by(stock) %>% 
  arrange(desc(Close)) %>% 
  select(Date, stock, Close) %>% 
  top_n(n = 10) %>% 
  as.data.frame()

# (4) take the stocks data frame, then rename stock to Stock, Open to Opening,
# and Close to Closing. Notice we have to reassign the result to make the
# changes permanent.
stocks_df <- stocks_df %>% 
  rename(Stock = stock,
         Opening = Open,
         Closing = Close)    # New name = Old name


# (5) take the stocks data frame, then subset for Fridays, then create an
# indicator that is 1 when change is greater than 0 and 0 otherwise, then group
# by stock, and then find the proportion of times a stock closed higher than it
# opened on Fridays.

stocks_df %>% 
  filter(Day == "Fri") %>% 
  mutate(Higher = if_else(Change > 0, 1, 0)) %>% 
  group_by(Stock) %>% 
  summarise(mean_higher = mean(Higher))


# The free R for Data Science book at http://r4ds.had.co.nz/ has much more!