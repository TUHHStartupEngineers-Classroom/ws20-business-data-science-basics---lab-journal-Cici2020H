library("tidyverse")
library("tidyr")
library("vroom")

# ignore columns with col_skip
col_types <- list(
  number = col_skip(),
  country = col_skip(),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_skip(),
  filename = col_skip(),
  withdrawn = col_skip(),
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

patent_tbl <- vroom(file = "C:/Users/corne/OneDrive/Dokumente/Uni/Master/WS2021/BuisnessDataScience/DS_101/00_data/03_patent/patent.tsv",
                      delim      = "\t", 
                      col_types  = col_types,
                      na         = c("", "NA", "NULL")
)

# delete all patents, which are not assigned in 2019
patent_tbl <- patent_tbl %>% filter(date >= "2019-01-01" & date < "2020-01-01")
# keep only the columns "id" and "date" (we don't need colum type here)
patent_tbl <- patent_tbl[c("id","date")]

# consider only US companies/cooperations
assignee_tbl <- assignee_tbl  %>% filter(type == 2)

# join the three tables
joined <- left_join(patent_assignee_tbl, patent_tbl, by = c("patent_id"="id"))
joined <- drop_na(joined)
joined <- left_join(joined, assignee_tbl, by = c("assignee_id" = "id"))

# count the number of patents from each organization
joined <- joined %>% group_by(organization) %>% summarize(count=n())

# sort in decreasing order
joined <- joined[order(joined$count, decreasing = TRUE),]
joined <- drop_na(joined)

# write the top ten companies in a list
res_list <- list()
for (i in 1:10){
  res_list[[i]] <- joined[order(joined$count, decreasing = TRUE),]$organization[i]
}
print(res_list)


