#It allows us to compose general HTTP requests and provides convenient functions to fetch data.
install.packages("RCurl")
#It allows us to converts JSON object into R objects and vice-versa.
install.packages("rjson")
#A Mining Package for text mining applications within R. It offers a number of transformations that ease the tedium of cleaning data.
install.packages("tm")

#Call packages
library(RCurl)
library(rjson)
library(tm)

# dumping post url into variable url 
url <- "https://graph.facebook.com/v2.8/1327123630658509/comments?after=MTExNTE1&pretty=0&limit=500&access_token=EAACEdEose0cBADRfZAx5WwjTYrL78Y5eBpjjvfO2FsesdBOMnioVdCNZBVQ33GyaFDWlBegcVFmySON8Dp1EkcgPXmnXsOK1BdQhJ9GJuLwIBfqZCqQwQFRp8Onxv9LB16JbZAQVMdp9ZA85eYKWKiZAYrmqpwAMN2arprCX5a2hzQmJ0WAU4pDLymp8N81DIZD"
# accesing url through variable d and getURL() function
d<- getURL(url)

#getting JSON data from the url
j <- fromJSON(d)

#extracting comments from the data
comments <- sapply(j$data, function(j) {list(comment=j$message)})

# viewing comments
View(comments)


#Creating corpus & removing extra spaces, special characters & other unwanted things
cleandComments <- sapply(comments, function(x) iconv(enc2utf8(x), sub = "byte"))

my_corpus <- Corpus(VectorSource(cleandComments))
my_function <- content_transformer(function(x , pattern) gsub(pattern, " ", x))
my_cleaned_corpus <- tm_map(my_corpus,my_function, "/")
my_cleaned_corpus <- tm_map(my_cleaned_corpus, my_function, "@")
my_cleaned_corpus <- tm_map(my_cleaned_corpus, my_function, "\\|")

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
my_cleaned_corpus <- tm_map(my_cleaned_corpus, removeSpecialChars)
my_cleaned_corpus <- tm_map(my_cleaned_corpus, content_transformer(tolower))
my_cleaned_corpus <- tm_map(my_cleaned_corpus, removeWords, c(stopwords("english"),"united","airline","airlines"))
my_cleaned_corpus <- tm_map(my_cleaned_corpus, removePunctuation)
my_cleaned_corpus <- tm_map(my_cleaned_corpus, stripWhitespace)

1327128317324707

#Creating Term Document Matrix
my_tdm <- TermDocumentMatrix(my_cleaned_corpus)
m <- as.matrix(my_tdm)
View(m)

words <- sort(rowSums(m), decreasing = TRUE)
my_data <- data.frame(word = names(words), freq=words)
View(my_data)

#Creating wordcloud

install.packages("wordcloud")
library(wordcloud)

wordcloud(words = my_data$word, freq = my_data$freq, min.freq = 2,
          max.words = 100, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))



