### Set directory for data set
myDir <- '~/MobilyWeeklyReport/Data'


### Read in data set(s)
colread_redemp <- list(col_datetime(), col_character(), col_character(), col_character(), 
                col_character(), col_character(),    col_integer(), 
                col_factor(levels = c('Mobily', 'Partner')), col_integer(), 
                col_character(), col_factor(levels = c(0,1)), col_factor(levels = c(1,2)))

df1 <- read_csv(file = paste(myDir,'weekly_redemptions.csv', sep = '/'), 
                col_types = colread_redemp)

colread_cust <- list(col_date(format = '%Y%m%d'), col_factor(levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12)),
		col_factor(levels = c(1,2)), col_integer(), col_integer(), col_double())

dfc1 <- read_csv(file = paste(myDir, 'weekly_customers.csv', sep = '/'),
		col_types = colread_cust)

pckg_info <- read_csv(file = paste(myDir, 'Package_Classification_Finance.csv', sep = '/'))


### Clean and Merge data set(s)
## Clean redemptions data set
names(df1) <- tolower(names(df1))
names(dfc1) <- tolower(names(dfc1))
## Remove BOM characters that may creep in
names(df1)[1] <- gsub('\\W|?|ï', '', names(df1)[1]) 
names(dfc1)[1] <- gsub('\\W|?|ï', '', names(dfc1)[1]) 
df1 <- tbl_df(as.data.frame(rapply(df1, as.factor, classes = 'character', how = 'replace')))
df1$pay_type_code <- factor(df1$pay_type_code, levels = c(1,2), labels = c('Prepaid', 'Postpaid'))
dfc1$pay_type_code <- factor(dfc1$pay_type_code, levels = c(1,2), labels = c('Prepaid', 'Postpaid'))
#order data set by date
df1 <- arrange(df1, redemption_date)
dfc1 <- arrange(dfc1, extractdate)
#Partitioning weeks to add a new variable for weekending dates
dts1 <- df1$redemption_date
dts2 <- wday(dts1)
df1$redemption_we <- dts1 + days(7 - dts2)
df1$redemption_we <- factor(df1$redemption_we, ordered = TRUE)
dfc1$extractdate <- factor(dfc1$extractdate, ordered = TRUE)
rm(dts1, dts2)

## Clean package info data set
names(pckg_info) <- tolower(names(pckg_info))


## Merge redemptions and package info data sets
df1 <- left_join(df1, pckg_info, by = c('package_description' = 'package'))


### Generate themes to use in graphs
myTheme <- theme_bw() + theme(plot.title = element_text(family = 'Garamond', face = 'bold'), axis.title = element_text(family = 'Garamond'), axis.text = element_text(family = 'Garamond', colour = 'black'))
singleBarGeom <- geom_bar(stat = 'identity', fill = 'lightblue', colour = 'black', size = 1)
histogramGeom <- geom_bar(fill = 'lightblue', colour = 'black', size = 1) 


### Formulating a function wrapper for tidying large numbers
tidyNum <- function(x){
x_rounded <- round(x)
x_neat <- format(x_rounded, big.mark = ',')
return(x_neat)
}


### Extracting report date range
covering_period <- paste(format(min(df1$redemption_date), '%d-%b-%Y'), format(max(df1$redemption_date), '%d-%b-%Y'), sep = ' to ')
