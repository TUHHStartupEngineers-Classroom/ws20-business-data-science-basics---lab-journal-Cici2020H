library("tidyverse")
library("tidyr")
library("vroom")

# ignore columns with col_skip
col_types <- list(
  uuid = col_skip(), 
  subclass_id = col_skip(),
  sequence = col_skip(),
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

uspc_tbl <- vroom(file = "C:/Users/corne/OneDrive/Dokumente/Uni/Master/WS2021/BuisnessDataScience/DS_101/00_data/03_patent/uspc.tsv",
                    delim      = "\t", 
                    col_types  = col_types,
                    na         = c("", "NA", "NULL")
)

# list the top 10 wordwide companies!
assignee_tbl <- assignee_tbl  %>% filter(type == 2 | type == 3 ) # consider US or foreign companies
assignee_tbl <- drop_na(assignee_tbl)
joined <- left_join(patent_assignee_tbl, assignee_tbl, by =c("assignee_id"="id")) %>% drop_na()
comp_tbl <- joined %>% group_by(organization) %>% summarize(count=n())
comp_tbl <- comp_tbl[order(comp_tbl$count, decreasing = TRUE),]

top_ten_comp <- list()
for (i in 1:10) {
  top_ten_comp[[i]] = comp_tbl$organization[i]
  
}

# delete all entries where the company is not in the top 10 company list
joined <- joined[joined$organization %in% top_ten_comp,]

# join with uspc table via patent_id
test <- left_join(joined, uspc_tbl, by = c("patent_id"="patent_id"))
test <- test[!duplicated(test$patent_id), ]
test <-drop_na(test)

# group by main class and count
test <- test %>% group_by(mainclass_id) %>% summarise(count=n())

# order indecreasing order
test <- test[order(test$count,decreasing = TRUE),]

# determine the top 5main classe
main_class_list = list()
for (i in 1:5){
  main_class_list[[i]] = test$mainclass_id[i]
}

print(main_class_list)
