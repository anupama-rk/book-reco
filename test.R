library(data.table)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
library(e1071)





path<- "C:/RProjects/bookcrossing"
setwd(path)
getwd()


#Read data from csv

users<-read.csv("users.csv",sep=";",quote="",stringsAsFactors = FALSE)
ratings<-read.csv("ratings.csv",sep=";",quote="",stringsAsFactors = FALSE)
books<-read.csv("books.csv",sep=";",quote="",stringsAsFactors = FALSE)



#Rename columns

names(users)=c("userId","location","age")
names(ratings)=c("userId","isbn","rating")
names(books)=c("isbn","title","author","yearPubished","publisher","imageUrlS","imageUrlM","imageUrlL")


#Delete columns from books

books<-books[-c(6,7,8)]


#delete quotes from dataframe

del_users <- colwise(function(users) str_replace_all(users, '\"', ""))
users_loc_age<-del_users(users)


del_books <- colwise(function(books) str_replace_all(books, '\"', ""))
books_auth_year<-del_books(books)


del_ratings <- colwise(function(ratings) str_replace_all(ratings, '\"', ""))
ratings_user<-del_ratings(ratings)


#numeric conversion 

users_loc_age$userId<-as.numeric(users_loc_age$userId)
users_loc_age$age<-as.numeric(users_loc_age$age)
books_auth_year$yearPubished<-as.numeric(books_auth_year$yearPubished)
ratings_user$userId<-as.numeric(ratings_user$userId)
ratings_user$rating<-as.numeric(ratings_user$rating)






#Replace NULL values with NA

users_df <-users_loc_age %>% replace(.=="NULL", NA)
books_df <-books_auth_year %>% replace(.=="NULL", NA)
ratings_df<-ratings_user %>% replace(.=="NULL", NA)




#Remove rows with NA

users_df<-users_df %>% drop_na(userId)
users_df<-users_df %>% drop_na(location)
books_df<-na.omit(books_df)




#Filter rows containing isbn like http
books_df<-books_df[!grepl("http", books_df$isbn),]





#Replace age less than 0 and greater than 100

users_df$age[users_df$age > 100] <- NA
users_df$age[users_df$age < 10] <- NA


#Replace missing age with mean

ageMean = trunc(mean(users_df$age, na.rm = TRUE))
users_df$age[is.na(users_df$age)] <- ageMean


#Replace year =0 and NA  with mean

yearMean = trunc(mean(books_df$yearPubished, na.rm = TRUE))
books_df$yearPubished[books_df$yearPubished ==0] <- NA
books_df$yearPubished[is.na(books_df$yearPubished)] <- yearMean


#Merge rating and book

ratings_books<-left_join(ratings_df,books_df,by="isbn")

#Merge ratings_books and users

ratings_books_users<-left_join(ratings_books,users_df,by="userId")


#Remove NA from master

ratings_books_users<-na.omit(ratings_books_users)


#Subset dataframe to EXplicit and Implicit


ratings_explicit<-subset(ratings_books_users,ratings_books_users$rating>0)
ratings_implicit<-subset(ratings_books_users,ratings_books_users$rating==0)





#Most Read books

most_read_books<-ratings_books_users%>%
  group_by(isbn,title,author,publisher)%>%
  dplyr::summarize(num_isbn=n())%>%
  filter(num_isbn>200)%>%
  arrange(-num_isbn)%>%
  select(isbn,title,author,publisher,num_isbn)%>%
  head(10)


bp<- ggplot(most_read_books, aes(x="Popular Books", y=num_isbn, fill=title))+
  geom_bar(width = 1, stat = "identity")
bp + scale_fill_brewer(palette="Dark2")+theme_minimal()



#Top Rated 10 books

top_rated_books<-ratings_explicit%>%
  group_by(isbn,title,author,publisher)%>%
  filter(rating>7)%>%
  dplyr::summarize(num_isbn=n())%>%
  filter(num_isbn>50)%>%
  arrange(-num_isbn)%>%
  select(isbn,title,author,publisher,num_isbn)%>%
  head(n=10)

pie<- ggplot(top_rated_books, aes(x="Top rated books", y=num_isbn, fill=title))+
  geom_bar(width = 1, stat = "identity")

pie + coord_polar("y", start=0)


#Rating distribution

summary(ratings_explicit)
range(ratings_explicit$rating)


hist(ratings_explicit$rating, 
     breaks = 'Sturges',
     xlab = 'Rating',
     main = '',
     col = 3)

rating_dist = ratings_explicit$rating
plotNormalHistogram(rating_dist)


# 
# hist(ratings_explicit$rating, 
#      breaks = 'Sturges',
#      xlab = 'Rating',
#      main = '',
#      col = 3)

library(rcompanion)

plotNormalHistogram(T_sqrt)


# theme_set(theme_light())
# ggplot(ratings_explicit, aes(x=rating)) +
#   geom_histogram()


# count_gt50<-ratings_explicit%>%
#   group_by(isbn,title,author,publisher,rating)%>%
#   dplyr::summarize(num_isbn=n())%>%
#   filter(num_isbn>50)%>%
#   arrange(-num_isbn)%>%
#   select(isbn,title,author,publisher,num_isbn,rating)
# View(count_gt50)
# 
# summary(count_gt50)
# 
# 
# 
# hist(count_gt50$rating, 
#      breaks = 'Sturges',
#      xlab = 'Rating',
#      main = '',
#      col = 3)
# 



# ratings_explicit$rating <- scale(ratings_explicit$rating,center= TRUE, scale=TRUE)


#Skewness and Resolving outliers ,normalising with log transformation

library(e1071)
print(skewness(ratings_explicit$rating))# Left skewed by -0.6613279


library(outliers)
outlier(ratings_explicit$rating)


ratings_norm<-subset(ratings_explicit,ratings_explicit$rating>3)



#ratings_norm$rating <- scale(ratings_norm$rating,center= TRUE, scale=TRUE)


print(skewness(ratings_norm$rating)) # skewness after -0.3522745


rating_log_norm = ratings_norm$rating
ratings_norm$rating<-  log(ratings_norm$rating)
plotNormalHistogram(rating_log_norm)


# 
# hist(ratings_norm$rating, 
#      breaks = 'Sturges',
#      xlab = 'Rating',
#      main = '',
#      col = 3)
# 
# 







# matrix with users rated more than 50 

rating_matrix_long<-ratings_norm%>%
  add_count(userId)%>%
  dplyr::filter(n>50)%>%
  select(isbn,userId,rating)%>%
  arrange(userId)



# matrix with books rated more than 50 

rating_matrix_long<-rating_matrix_long%>%
  add_count(isbn)%>%
  dplyr::filter(n>50)%>%
  select(isbn,userId,rating)%>%
  arrange(isbn)


dim(rating_matrix_long)


booklist<-rating_matrix_long%>%
  left_join(books_df,by="isbn")%>%
  select(title)%>%
  unique()



#User-Book Rating matrix

rating_matrix<-rating_matrix_long%>%pivot_wider(names_from = isbn,values_from=rating)




#removing userid from rating matrix

rating_matrix = as.matrix(rating_matrix[,-1])



#Real matrix
object.size(rating_matrix)
rating_matrix = as(rating_matrix, "realRatingMatrix")
object.size(rating_matrix)



#Normalize the ratings matrix
#rating_matrix = normalize(rating_matrix) 

#Recommendation package
library(recommenderlab)

ratingmodel = Recommender(rating_matrix, method = "UBCF", param=list(method="Cosine",nn=10)) 


#Predictions


Top_5_pred = predict(ratingmodel, rating_matrix[1], n=5)
Top_5_List = as(Top_5_pred, "list")


#Recommended predictions for a user based on previous ratings


recomdf=data.frame(Top_5_List)
colnames(recomdf)="isbn"
bookrecomdf<-left_join(recomdf,books_df,by="isbn")%>%
  select(title)%>%
  print()
