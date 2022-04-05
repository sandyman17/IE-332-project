library(rvest)
library(dplyr)
library(stringr)

movie_list = data.frame()

for (page_result in seq(from = 1, to = 101, by = 1)){
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&year=2020-01-01,2020-12-31&start=", page_result , "&ref_=adv_nxt")
  page = read_html(link)
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>% html_attr("href") %>% paste("https://www.imdb.com", ., sep = "")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  year = gsub("\\(", "", year)
  year = gsub("\\)", "", year)
  year = gsub("I", "", year)
  #img_link = page %>% html_nodes(".lister-item-image float-left") %>% html_text()
  directors_data  <-  page %>% 
    html_nodes('.text-muted+ p') %>% 
    html_text()%>%
    gsub("[|]","",.) %>%
    gsub(".*Directors:","",.)  %>%  
    gsub(".*Director:","",.) %>%
    gsub("Stars:.*","",.)%>% 
    gsub("[\n]", "", .) %>%  
    gsub("^\\s+|\\s+$", "", .)
  runtime = page %>% html_nodes(".runtime") %>% html_text()
  runtime = gsub(" min", "", runtime)
  genre = page %>% html_nodes(".genre") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  #votes = page %>% html_nodes(".sort-num_votes-visible span:nth-child(2)") %>% html_text()
  cast = page %>% html_nodes('.text-muted+ p') %>% html_text()
  cast = gsub(".*Stars:", "", cast)
  cast = gsub("\n", "", cast)
  cast = gsub("    ", "", cast)
  movie_list = rbind(movie_list, data.frame(name, year, movie_links, runtime, genre, synopsis, rating, cast, directors_data))
  print(paste("Page : ", page_result))
}
#img_link = page %>% html_nodes(".lister-item-image float-left a") %>% html_attr("href")

