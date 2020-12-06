# task one

library("httr")
library("jsonlite")
library("tidyverse")


# resp <- GET("https://swapi.dev/api/")
# resp_as_list <- resp %>% .$content %>% rawToChar() %>% fromJSON()
# resp_as_list2 <- content(resp, as = "parsed")
# names_of_list = names(resp_as_list)
# #rawToChar(resp$content)

read <- function(url = "https://swapi.dev/api/"){
  resp <- GET(url)
  #resp_as_list <- content(resp)
  resp_as_list <- resp %>% .$content %>% rawToChar() %>% fromJSON()
  return(resp_as_list)
}

res_as_list = read()
names_of_list = names(res_as_list)
df_list = list()
for (names in 1:length(names_of_list)){
  urls = res_as_list[names_of_list[names]]  
  content = read(urls[[1]])
  counts = content$count
  
  observation = list()
  for (nr in 1:counts){
    #print(nr)
    observation[[nr]] <- pivot_wider(enframe(read(paste0(urls[[1]],nr,"/"))))
    #observation[[nr]] <- read(paste0(urls[[1]],nr,"/"))
      }
  df <- bind_rows(observation)
  df_list[[names]] = df
}
df5 = df_list[[5]]
glimpse(df5)
#test <- df1 %>% filter(!map_lgl(species, list()))



  