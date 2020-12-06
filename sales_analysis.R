# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library("tidyverse")
library("readxl")
#library("ggplot2")

# 2.0 Importing Files ----
DATA_PATH = "C:/Users/corne/OneDrive/Dokumente/Uni/Master/WS2021/BuisnessDataScience/DS_101/00_data/01_bike_sales/01_raw_data/"
raw_data_bikes = read_excel(paste0(DATA_PATH,"bikes.xlsx"))
raw_data_shops = read_excel(paste0(DATA_PATH,"bikeshops.xlsx"))
raw_data_orderlines = read_excel(paste0(DATA_PATH,"orderlines.xlsx"))

# 3.0 Examining Data ----
raw_data_bikes %>% head(n = 5)
raw_data_shops
glimpse(raw_data_orderlines)

# 4.0 Joining Data ----
data_all = left_join(raw_data_orderlines, raw_data_bikes, by = c("product.id" = "bike.id"))
data_all = left_join(data_all, raw_data_shops, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----
data_wrangled = separate(data_all, col = location, into = c("city","state"), sep=",")

data_wrangled = data_wrangled %>% mutate(total.price = price*quantity) # new column with mutate
sales_by_states = data_wrangled %>% select(state, total.price) %>% group_by(state) %>% summarize(sales = sum(total.price)) # create a new dataframe with columns we need


p1 <- ggplot(sales_by_states,aes(state, sales)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_bar(stat = "identity") +  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                                                   decimal.mark = ",", 
                                                                                   prefix = "", 
                                                                                   suffix = " €")) +
  labs(
    title    = "Revenue by state",
    x = "state", # Override defaults for x and y
    y = "Revenue [€]"
  )

# 6.0 Business Insights ----

# 6.1 Sales by Year ----
sales_by_year_tbl <- data_wrangled %>% separate(order.date,into=c('year','month','day'),sep='-') %>% select(year, state, total.price) %>% group_by(year, state) %>% summarise(sales = sum(total.price))  %>% ungroup()


  # Formatting


  # labs(
  #   title = "Revenue by year and main category",
  #   subtitle = "Each product category has an upward trend",
  #   fill = "Main category" # Changes the legend name
  # )


# Step 1 - Manipulate

# Step 2 - Visualize


# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

# Step 2 - Visualize
p2 <- sales_by_year_tbl %>% ggplot(aes(x = year, y = sales)) +

  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot

  # Facet
  facet_wrap(~ state) +  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                                           decimal.mark = ",",
                                                                           prefix = "",
                                                                           suffix = " €")) +
  labs(
    title    = "Revenue by year and state",
    x = "year", # Override defaults for x and y
    y = "Revenue [€]"
  )
plot(p2)


# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----
