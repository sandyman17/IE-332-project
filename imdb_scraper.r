library(rvest)
library(dplyr)
library(stringr)
library(fastDummies)
library(tidyr)
library(stringi)

movie_list = data.frame()


for (page_result in seq(from = 1, to = 51, by = 1)){
  #link = paste0("https://www.imdb.com/search/title/?title_type=feature&year=2020-01-01,2020-12-31&start=", page_result , "&ref_=adv_nxt")
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&release_date=2000-01-01,2022-12-31&user_rating=1.0,10.0&certificates=US%3AG,US%3APG,US%3APG-13,US%3AR,US%3ANC-17&runtime=1,1000&count=250&start=", page_result , "&ref_=adv_nxt")
  page = read_html(link)
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>% html_attr("href") %>% paste("https://www.imdb.com", ., sep = "")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  year = gsub("\\(", "", year)
  year = gsub("\\)", "", year)
  year = gsub("I", "", year)
  img_link = page %>% html_nodes(".loadlate") %>% html_attr("src")
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
  tv_rating = page %>% html_nodes(".certificate") %>% html_text()
  genre = page %>% html_nodes(".genre") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  cast = page %>% html_nodes('.text-muted+ p') %>% html_text()
  cast = gsub(".*Stars:", "", cast)
  cast = gsub("\n", "", cast)
  cast = gsub("    ", "", cast)
  movie_list = rbind(movie_list, data.frame(name, year, movie_links, img_link, runtime, tv_rating, genre, synopsis, rating, cast, directors_data))
  print(paste("Page : ", page_result))
}

movie_list <- movie_list %>% mutate_if(is.character, str_trim)

split_genres <- str_split_fixed(movie_list$genre, ", ", 3)

split_directors <- str_split_fixed(movie_list$directors_data, ", ", 3)

split_actors <- str_split_fixed(movie_list$cast, ", ", 4)

genre_dummies <- dummy_columns(split_genres, select_columns = c("V1", "V2", "V3"))

director_dummies <- dummy_columns(split_directors, select_columns = c("V1", "V2", "V3"))

actor_dummies <- dummy_columns(split_actors, select_columns = c("V1", "V2", "V3", "V4"))

tv_ratings = movie_list$tv_rating

tv_rating_dummies <- dummy_columns(tv_ratings)

tv_rating_dummies$.data = NULL

combine_dummies <- function(dummy_list, split_list){
  combined_df = data.frame(matrix(data = 0, nrow = nrow(dummy_list), ncol = 1))
  split_vec = c(t(split_list))
  unique_values <- unique(split_vec)
  unique_values = stri_remove_empty(unique_values)
  i = 0
  while(i < length(unique_values)){
    item = unique_values[i]
    selected_cols = dummy_list %>% select(contains(item))
    sum_col = rowSums(selected_cols)
    combined_df = cbind(combined_df, sum_col)
    i = i + 1
  }
  colnames(combined_df) <- unique_values
  return(combined_df)
}

combined_genre_dummies = combine_dummies(genre_dummies, split_genres)

combined_actor_dummies = combine_dummies(actor_dummies, split_actors)

combined_director_dummies = combine_dummies(director_dummies, split_directors)

final_movie_database = data.frame(movie_list$name, movie_list$year, movie_list$movie_links, movie_list$img_link, movie_list$runtime, movie_list$synopsis, tv_rating_dummies, combined_genre_dummies, combined_actor_dummies, combined_director_dummies)

write.csv(final_movie_database, "E:\\IE332_project_github\\IE-332-project\\movies_data.csv")
