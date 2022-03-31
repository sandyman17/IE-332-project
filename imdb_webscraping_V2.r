#install Packages
install.packages("rvest") #https://cran.r-project.org/web/packages/rvest/rvest.pdf
install.packages("ggplot2")

#Loading the rvest package
library('rvest')
library('ggplot2')

scrape_page <- function(webpage){
  #Title
  title_data_html <- html_nodes(webpage,'.lister-item-header a')
  title_data <- html_text(title_data_html)
  head(title_data)
  tail(title_data)

  #---------------------------------------------------------------------------------------------------------------
  
  #Description
  description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')
  description_data <- html_text(description_data_html)
  head(description_data)
  tail(description_data)
  
  #Data-Preprocessing: removing '\n'
  description_data<-gsub("\n","",description_data)
  
  #Let's have another look at the description data 
  head(description_data)
  
  #---------------------------------------------------------------------------------------------------------------
  
  #Runtime
  
  runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')
  runtime_data <- html_text(runtime_data_html)
  head(runtime_data)
  tail(runtime_data)
  
  #Data-Preprocessing: removing mins and converting it to numerical
  runtime_data<-gsub(" min","",runtime_data)
  runtime_data<-as.numeric(runtime_data)
  
  #Let's have another look at the runtime data
  head(runtime_data)
  tail(runtime_data)
  
  #---------------------------------------------------------------------------------------------------------------
  
  #Genre
  
  genre_data_html <- html_nodes(webpage,'.genre')
  genre_data <- html_text(genre_data_html)
  head(genre_data)
  tail(genre_data)
  
  #Data-Preprocessing: removing \n
  genre_data<-gsub("\n","",genre_data)
  
  #Data-Preprocessing: removing excess spaces
  genre_data<-gsub(" ","",genre_data)
  
  #taking only the first genre of each movie
  genre_data<-gsub(",.*","",genre_data)
  
  #Convering each genre from text to factor
  genre_data<-as.factor(genre_data)
  
  #Let's have another look at the genre data
  head(genre_data)
  tail(genre_data)
  
  #---------------------------------------------------------------------------------------------------------------
  
  #Rating
  
  rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')
  rating_data <- html_text(rating_data_html)
  head(rating_data)
  tail(rating_data)
  
  #Data-Preprocessing: converting ratings to numerical
  rating_data<-as.numeric(rating_data)
  
  #Let's have another look at the ratings data
  head(rating_data)
  tail(rating_data)
  
  #---------------------------------------------------------------------------------------------------------------
  
  #Director
  
  directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')
  directors_data <- html_text(directors_data_html)
  head(directors_data)
  tail(directors_data)
  
  #Data-Preprocessing: converting directors data into factors
  directors_data<-as.factor(directors_data)
  
  #Let's have another look at the directors data
  head(directors_data)
  tail(directors_data)
  
  #---------------------------------------------------------------------------------------------------------------
  
  #Actors
  
  actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')
  actors_data <- html_text(actors_data_html)
  head(actors_data)
  tail(actors_data)
  
  #Data-Preprocessing: converting actors data into factors
  actors_data<-as.factor(actors_data)
  
  #Let's have another look at the Actors data
  head(actors_data)
  tail(actors_data)
  
  #---------------------------------------------------------------------------------------------------------------

  #Step Final: Frame all in one
  
  #Combining all the lists to form a data frame
  page_df<-data.frame(Title = title_data,
                        Description = description_data, 
                        Runtime = runtime_data,
                        Genre = genre_data, 
                        Rating = rating_data,
                        Director = directors_data, 
                        Actor = actors_data)
  return(page_df)
}

#----------------------------------------------------------------------------------------------------------------
#Specifying the url for desired website to be scrapped
url_list <- c('https://www.imdb.com/search/title/?title_type=feature&release_date=2000-12-31,2022-01-01&user_rating=1.0,10.0&count=250', 'https://www.imdb.com/search/title/?title_type=feature&release_date=2000-12-31,2022-01-01&user_rating=1.0,10.0&count=250&start=251&ref_=adv_nxt', 'https://www.imdb.com/search/title/?title_type=feature&release_date=2000-12-31,2022-01-01&user_rating=1.0,10.0&count=250&start=501&ref_=adv_nxt', 'https://www.imdb.com/search/title/?title_type=feature&release_date=2000-12-31,2022-01-01&user_rating=1.0,10.0&count=250&start=751&ref_=adv_nxt')

i = 1
movies_df <- data.frame()
while (i < 5){
  site = read_html(url_list[i])
  page_df = scrape_page(site)
  movies_df = rbind(movies_df, page_df)
  i = i + 1
}

