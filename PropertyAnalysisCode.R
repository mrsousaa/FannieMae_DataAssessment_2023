## Home Analysis for REI

## Read in files

firingRate = function(df)
  {
  vec <- apply(df,2,function(x){sum(!is.na(x))/length(x)*100})
  return(vec)
  }


### Fair Market Rent Data 2021
fmr = read.csv('/Users/luis/Downloads/fy2021-safmrs.csv', colClasses=c('character'), na='')

length(unique(fmr$ZIP.Code)) == nrow(fmr)
fmr = fmr %>% dplyr::rename(ZIP = ZIP.Code)
names(fmr)
fmr$SAFMR.0BR = as.numeric(gsub("[$,]","",fmr$SAFMR.0BR))
fmr$SAFMR.1BR = as.numeric(gsub("[$,]","",fmr$SAFMR.1BR))
fmr$SAFMR.2BR = as.numeric(gsub("[$,]","",fmr$SAFMR.2BR))
fmr$SAFMR.3BR = as.numeric(gsub("[$,]","",fmr$SAFMR.3BR))
fmr$SAFMR.4BR = as.numeric(gsub("[$,]","",fmr$SAFMR.4BR))
fmr$Rent_Index = (fmr$SAFMR.0BR+fmr$SAFMR.1BR+fmr$SAFMR.2BR+fmr$SAFMR.3BR+fmr$SAFMR.4BR)/4

length(unique(fmr$ZIP))
fmr = fmr %>% dplyr::group_by(ZIP) %>%
  dplyr::summarise(Rent_Index = median(Rent_Index))


## Group fmr data by ZIP.Code





## Median Multifamily Price By Zip
mfh_zip = read.csv('/Users/luis/Downloads/Zip_zhvi_bdrmcnt_5_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv', colClasses=c('character'), na='') 
mfh_zip = mfh_zip %>% dplyr::rename(ZIP = RegionName) %>%
  dplyr::group_by(ZIP) %>%
  dplyr::mutate(Median_MFH_Price_2020 = median(c(as.numeric(X2020.01.31),
                                             as.numeric(X2020.02.29),
                                             as.numeric(X2020.03.31),
                                             as.numeric(X2020.04.30),
                                             as.numeric(X2020.05.31),
                                             as.numeric(X2020.06.30),
                                             as.numeric(X2020.07.31),
                                             as.numeric(X2020.08.31),
                                             as.numeric(X2020.09.30)), na.rm=TRUE))
firingRate(mfh_zip)
colnames(mfh_zip)
mfh_zip_2020 = mfh_zip[,c(1:9,307)]              


## Graphical Statistics

histogram(as.numeric(mfh_zip_2020$Median_MFH_Price_2020))
summary(as.numeric(mfh_zip_2020$Median_MFH_Price_2020))

## Median 2020 MFH Price by State

mfh_state_2020 = mfh_zip_2020 %>% dplyr::group_by(State) %>%
  dplyr::summarise(median_mfh_2020 = median(Median_MFH_Price_2020))

ny = mfh_zip_2020 %>% dplyr::filter(State == 'NY')



## Fix Zip
table(nchar(mfh_zip$ZIP), useNA='ifany')



## Look at realtor Data

rdc = read.csv('/Users/luis/Downloads/RDC_Inventory_Core_Metrics_Zip.csv', colClasses=c('character'), na='')
nrow(rdc) == length(unique(rdc$postal_code))

firingRate(rdc)

## ZIP code is Unique here
## Fix ZIP

table(nchar(rdc$postal_code))
rdc = rdc %>% dplyr::mutate(ZIP = if_else(nchar(postal_code)==4, paste0('0',postal_code),
                                          if_else(nchar(postal_code)==3, paste0('00', postal_code), postal_code)))


### Let's inner join realtor listings by FMR value 

final = rdc %>% dplyr::inner_join(fmr, by='ZIP')


### Map Creation

library(usmap)
library(ggplot2)

plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))


## Try below

library(usmap)
library(ggplot2)

plot_usmap(data = statepop, values = "pop_2015", color = "black") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = statepop, values = "pop_2015", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Population (2015)", label = scales::comma
  ) + theme(legend.position = "right")


## County Level

usmap::plot_usmap("counties", fill = "yellow", alpha = 0.25,
                  # 06065 = Riverside County, CA
                  include = c(.south_region, "IA", "06065"),
                  # 12 = FL, 48141 = El Paso County, TX
                  exclude = c(.east_south_central, "12", "48141"))


plot_usmap(
  data = countypop, values = "pop_2015", include = c("NY"), color = "black"
) + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Population (2015)", label = scales::comma
  ) + 
  labs(title = "New York", subtitle = "New York County Level Map.") +
  theme(legend.position = "right")

## reference: https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html

View(usmap::citypop)

temp = mfh_state_2020 %>% dplyr::mutate(abbr = State) %>% dplyr::inner_join(statepop, by='abbr')


plot_usmap(data = temp, values = "median_mfh_2020", color = "black", labels=TRUE) + 
  scale_fill_continuous(name = "Median 2020 Home Value by State", label = scales::comma, low = 'white', high='blue') + 
  theme(legend.position = "right")



## fips to zip

fips2zip = read.csv('/Users/luis/Downloads/ZIP_COUNTY_092020.csv', colClasses=c('character'), na='') 
fips2zip = fips2zip %>% dplyr::arrange(ZIP, desc(RES_RATIO)) %>% dplyr::distinct(ZIP,.keep_all=TRUE)

temp = fips2zip %>% dplyr::inner_join(mfh_zip_2020, by='ZIP') %>% dplyr::rename(fips = COUNTY) 


## filter values from $100,000 to $1,000,000 (this would remove any outliers)
temp = temp %>% dplyr::filter(Median_MFH_Price_2020 > 100000 &
                                Median_MFH_Price_2020 < 1000000)

temp = temp %>% dplyr::group_by(fips) %>%
  dplyr::mutate(medianCounty_household = median(Median_MFH_Price_2020))



plot_usmap(data = temp, values = "medianCounty_household", color = "black") + 
  scale_fill_continuous(name = "Zillow 2020 Home Value by County", label = scales::comma, low = 'white', high='red') + 
  theme(legend.position = "right")


plot_usmap(data = temp, values = "medianCounty_household", color = "black", include = 'NY', labels = TRUE) + 
  scale_fill_continuous(name = "Zillow 2020 Home Value by County", label = scales::comma, low = 'white', high='red') + 
  theme(legend.position = "right")


plot_usmap(data = temp, values = "medianCounty_household", color = "black", include = 'IL', labels = TRUE) + 
  scale_fill_continuous(name = "Zillow 2020 Home Value by County", label = scales::comma, low = 'white', high='red') + 
  theme(legend.position = "right")

summary(temp$Median_MFH_Price_2020)


### Let's look at Realtor Listing Prices for September 2020 in NY Region

fips2zip = read.csv('/Users/luis/Downloads/ZIP_COUNTY_092020.csv', colClasses=c('character'), na='')
fips2zip = fips2zip %>% dplyr::arrange(ZIP, desc(RES_RATIO)) %>% dplyr::distinct(ZIP,.keep_all=TRUE)

temp = fips2zip %>% dplyr::inner_join(final, by='ZIP') %>% dplyr::rename(fips = COUNTY) %>% dplyr::mutate(median_listing_price = as.numeric(median_listing_price))
temp = temp %>% dplyr::filter(median_listing_price > 100000 &
                                median_listing_price < 1000000)
temp = temp %>% dplyr::group_by(fips) %>%
  dplyr::mutate(median_listing_price = median(median_listing_price))


plot_usmap(data = temp, values = "median_listing_price", color = "black", include = 'NY') + 
  scale_fill_continuous(name = "Median Listing Price from Realtor.com", label = scales::comma, low = 'white', high='navyblue') + 
  theme(legend.position = "right")


plot_usmap(data = temp, values = "median_listing_price", color = "black") + 
  scale_fill_continuous(name = "Median Listing Price from Realtor.com", label = scales::comma, low = 'white', high='navyblue') + 
  theme(legend.position = "right")

### Rent:Purchase Price Index

final = rdc %>% dplyr::inner_join(fmr, by='ZIP')

fips2zip = read.csv('/Users/luis/Downloads/ZIP_COUNTY_092020.csv', colClasses=c('character'), na='')
fips2zip = fips2zip %>% dplyr::arrange(ZIP, desc(RES_RATIO)) %>% dplyr::distinct(ZIP,.keep_all=TRUE)


temp = fips2zip %>% dplyr::inner_join(final, by='ZIP') %>% dplyr::rename(fips = COUNTY) %>% dplyr::mutate(median_listing_price = as.numeric(median_listing_price))


temp = temp %>% dplyr::group_by(fips) %>%
  dplyr::mutate(median_listing_price = median(median_listing_price))

temp$Rent_Purchase_Price_Index = (as.numeric(temp$Rent_Index)/
                                     as.numeric(temp$median_listing_price))*1000
histogram(temp$Rent_Purchase_Price_Index, nint=100)
summary(temp$Rent_Purchase_Price_Index)


temp = temp %>% dplyr::filter(Rent_Purchase_Price_Index> 0 &
                                    Rent_Purchase_Price_Index < 17)



plot_usmap(data = temp, values = "Rent_Purchase_Price_Index", color = "black", include='NY') + 
  scale_fill_continuous(name = "Ratio (FMR_of_County:Listing_Price) NY State", label = scales::comma, low = 'white', high='darkgreen') + 
  theme(legend.position = "right")

plot_usmap(data = temp, values = "Rent_Purchase_Price_Index", color = "black") + 
  scale_fill_continuous(name = "Ratio (FMR_of_County:Listing_Price) US Map", label = scales::comma, low = 'white', high='darkgreen') + 
  theme(legend.position = "right")

plot_usmap(data = temp, values = "Rent_Purchase_Price_Index", color = "black", include='CA') + 
  scale_fill_continuous(name = "Ratio (FMR_of_County:Listing_Price) NY State", label = scales::comma, low = 'white', high='darkgreen') + 
  theme(legend.position = "right")

plot_usmap(data = temp, values = "Rent_Purchase_Price_Index", color = "black", include=.northeast_region) + 
  scale_fill_continuous(name = "Ratio (FMR_of_County:Listing_Price) Northeast", label = scales::comma, low = 'white', high='darkgreen') + 
  theme(legend.position = "right")

write.csv(temp, '/Users/luis/Output_Table_Zip_County_RentPurchasePrice_Index.csv', row.names=FALSE,na='')

