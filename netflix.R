library(tidyverse)
netflix <- read_csv("Downloads/netflix_titles.csv")

#movie and tv show distribution
d<-netflix%>%
  group_by(type)%>%
  count()%>%
  ungroup()%>%
  mutate(percent=round(n/sum(n),1)*100)%>%
  arrange(percent)

d%>%
  ggplot(aes(x="",y=percent,fill=type))+
  geom_bar(stat="identity",width=1)+
  coord_polar("x",start=0)+
  geom_text(aes(label=percent),position=position_stack(vjust=0.5))+
  coord_polar(theta="y")+
  labs(fill="Show Type")+
  ggtitle(label = "Show Type on",subtitle ="Netflix" )+
  theme(plot.title = element_text(hjust=0.5))+  
  theme(plot.subtitle = element_text(hjust=0.5))+
  xlab('')+ylab('')

netflix%>%
  subset(release_year>2000)%>%
  ggplot(aes(x=release_year,fill=type))+
  geom_bar()+
  facet_wrap(.~type)+
  labs(fill="Show Type")+
  ggtitle(label="Releases over the year",subtitle="Netflix")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(plot.subtitle=element_text(hjust=0.5))+
  xlab('Year')+ylab('Content Released')

month<-netflix
month <- month %>%
  separate(date_added, c('Month', 'Date'))
month$Month<-factor(month$Month,levels=c("December",
                                         "November","October","September",
                                         "August","July","June","May",
                                         "April","March","February","January"))
month<-month[!is.na(month$Month),]
  
month%>%
  ggplot(aes(y=Month,fill=type))+
  geom_bar()+
  facet_wrap(.~type)+
  labs(fill="Show Type")+
  ggtitle(label="Releases over the Month",subtitle="Netflix")+
  theme(plot.title=element_text(hjust=0.5))+ 
  theme(plot.subtitle=element_text(hjust=0.5))+
  xlab('Releases over the year')+ylab('Month')

library(lubridate)
wday(netflix$date_added, label=TRUE)


netflix2 <- read_csv("Downloads/netflix1 - netflix1 (1).csv")
rm(netflix1_netflix1)


netflix1<-netflix1[!is.na(netflix1$date_added),]

netflix1$week<-factor(netflix1$week,levels = c("Sun","Sat","Fri","Thur","Wed","Tue","Mon"))
  
  
netflix1%>%
  ggplot(aes(y=week,fill=type))+
  geom_bar()+
  facet_grid(.~type)+
  labs(fill="Show Type")+
  ggtitle(label="Releases over the Week",subtitle="Netflix")+
  theme(plot.title=element_text(hjust=0.5))+ 
  theme(plot.subtitle=element_text(hjust=0.5))+
  xlab('Count of Releases')+ylab('Day of the Week')


netflix2<-netflix2[!is.na(netflix1$rating),]

netflix2%>%
  ggplot(aes(y=rating,fill=type))+
  geom_bar()



d<-netflix2%>%
  group_by(rating)%>%
  count()%>%
  ungroup()%>%
  mutate(percent=(n/sum(n)*100))%>%
  arrange(percent)


d%>%
  ggplot(aes(x=n,y=rating,fill=type))+
  geom_col(position = "dodge") +
  labs(fill="Show Type")+
  ggtitle(label="Ratings of Shows",subtitle="Netflix")+
  theme(plot.title=element_text(hjust=0.5))+ 
  theme(plot.subtitle=element_text(hjust=0.5))+
  xlab('Count of Releases')+ylab('Rating')

genre<-netflix%>%
  select(title,listed_in)


genre1<-data.frame(countries = unlist(strsplit(as.character(genre$listed_in), ",")))

top%>%
  arrange(desc(perc))%>%
  ggplot(aes(y=countries,x=perc))+
  geom_col()

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

top<-genre3%>%
  group_by(type,Genre)%>%
  count()%>%
  ungroup()%>%
  mutate(perc=n/sum(n))%>%
  arrange(perc)%>%
  mutate(percent=scales::percent(perc))

top<-trim(top$countries)


genre2 <- as.data.frame(apply(genre1,2,function(x)gsub('\\s+', '',x)))



top<-str_trim(top$countries, side = c("both", "left", "right"))



top1<-slice(top,c(1,10))


top1%>%
  ggplot(aes(x=perc*100,y=reorder( Genre,perc),fill=type))+
  geom_col(position = "dodge") + 
  ggtitle(label="Most watched Genres on",subtitle="Netflix")+
  labs(fill="Show Type")+
  theme(plot.title=element_text(hjust=0.5))+ 
  theme(plot.subtitle=element_text(hjust=0.5))+
  xlab('Percentage of Releases')+ylab('Genre')


top1 <- top %>%
  group_by(type)%>%           # Top N highest values by group
  arrange(desc(perc)) %>% 
  slice(1:12)

rm(top3)
genre3 <- read_csv("Downloads/netflix1 - genr (5).csv")



install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)

install.packages("tm")
library(tm)

movie<-genre3%>%
  subset(type=='Movie')

tv<-genre3%>%
  subset(type=='TV Show')

text <- tv$Genre
# Create a corpus  
docs <- Corpus(VectorSource(text))



dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

df<-df[-c(1), ] 

wordcloud(words = df$word, freq = df$freq, min.freq = 1,   
          max.words=200, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(7, "Dark2"))


data<- merge(x = rating, y = genre3, by = "title", all.x = TRUE)

data<-data[!is.na(data$Genre),]

genre_rating<-data%>%
  group_by(Genre,type)%>%
  count()%>%
  ungroup()%>%
  mutate(percent=n)%>%
  arrange(percent)
  
genre_rating1<-data%>%
  group_by(Genre,type) %>% 
  summarise(Frequency = sum(rating))
  
data1<- merge(x = genre_rating, y = genre_rating1, by = "Genre", all.x = TRUE)

genre_rating2<-data1%>%
  mutate(avg_rat=Frequency/n)%>%
  arrange(avg_rat)


data <- mutate(data, rating=as.numeric(rating))


top1 <- genre_rating2[with(genre_rating2,order(-Frequency)),]
  top1<-top1[1:15,]
  
  top1<-top1[-c(6), ] 
  

top1%>%
  ggplot(aes(y=reorder( Genre,avg_rat),x=avg_rat,fill=type.x))+
  geom_col(position = "dodge") + 
  labs(fill="Show Type")+
  ggtitle(label="IMDb Ratings",subtitle="Netflix")+
  theme(plot.title=element_text(hjust=0.5))+ 
  theme(plot.subtitle=element_text(hjust=0.5))+
  xlab('Average IMDb rating')+ylab('Genre')


netflix1<-netflix%>%
  subset(type=='Movie')



data3<- merge(x = netflix1, y = rating, by = "title", all.x = TRUE)

data3<-data3[!is.na(data3$rating.y),]


data5<-data3%>%
  select(rating.x,rating.y)



genre_rating<-data4%>%
  group_by(rating.x) %>% 
  summarise(Frequency = sum(rating.y))
genre_rating<-genre_rating[-c(14,15), ] 


genre_rating1<-data4%>%
  group_by(rating.x)%>%
  count()%>%
  ungroup()%>%
  mutate(percent=n)%>%
  arrange(percent)

genre_rating1<-genre_rating1[-c(1,2), ] 



data5<- merge(x = genre_rating1, y = genre_rating, by = "rating.x", all.x = TRUE)
data5<-data5%>%
  mutate(avg_rat=Frequency/n)%>%
  arrange(avg_rat)


data5%>%
  ggplot(aes(y=reorder( rating.x,avg_rat),x=avg_rat,fill=avg_rat))+
  geom_bar(stat="identity")+
  labs(fill="rating")+
  ggtitle(label="IMDb vs Movie Ratings",subtitle="Netflix")+
  theme(plot.title=element_text(hjust=0.5))+ 
  theme(plot.subtitle=element_text(hjust=0.5))+
  xlab('Average IMDb rating')+ylab('Movie Ratings')


data5<-data5[-c(13), ] 

