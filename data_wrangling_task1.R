library("tidyverse")
library("tidyr")
library("vroom")

# ignore columns with col_skip
col_types <- list(
  location_id = col_skip(),
  name_first = col_skip(),
  name_last = col_skip(),
  id = col_character(),
  type = col_character(),
  number = col_character(),
  country = col_character(),
  date = col_date("%Y-%m-%d"),
  abstract = col_character(),
  title = col_character(),
  kind = col_character(),
  num_claims = col_double(),
  filename = col_character(),
  withdrawn = col_double()
)

# read tables
patent_assignee_tbl <- vroom(file = "C:/Users/corne/OneDrive/Dokumente/Uni/Master/WS2021/BuisnessDataScience/DS_101/00_data/03_patent/patent_assignee.tsv",
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

assignee_tbl <- vroom(file = "C:/Users/corne/OneDrive/Dokumente/Uni/Master/WS2021/BuisnessDataScience/DS_101/00_data/03_patent/assignee.tsv",
                             delim      = "\t", 
                             col_types  = col_types,
                             na         = c("", "NA", "NULL")
)

# consider only US companies/cooperations
assignee_tbl <- assignee_tbl  %>% filter(type == 2)

# join the tables
joined <- left_join(patent_assignee_tbl,assignee_tbl, by =c("assignee_id"="id"))
joined <- drop_na(joined)

# count the number of patents of each organization
joined <- joined %>% group_by(organization) %>% summarize(count=n())

# sort in decreasing order
joined <- joined[order(joined$count, decreasing = TRUE),]
joined <- drop_na(joined)

# list the top 10 companies
companies <- list()
for (i in 1:10){
  companies[[i]] <- joined$organization[i]
}
print(companies)

