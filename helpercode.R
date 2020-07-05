#--------------------------------------------------------------------------------------
#helpercode.R
#--------------------------------------------------------------------------------------
# Sentiment Text Mining
#--------------------------------------------------------------------------------------

if(!require(proxy)) 
  install.packages("proxy")
if(!require(caTools)) 
  install.packages("caTools")
if(!require(dplyr)) 
  install.packages("dplyr")
if(!require(randomForest)) 
  install.packages("randomForest")
if(!require(SnowballC)) 
  install.packages("SnowballC")
if(!require(stopwords)) 
  install.packages("stopwords")
if(!require(tm)) 
  install.packages("tm")

#--------------------------------------------------------------------------------------
############### Loading Data
#--------------------------------------------------------------------------------------
tweets <- read.csv("Tweets.csv", header=TRUE)

#--------------------------------------------------------------------------------------
# Function: text_to_document
#   Convert a text vector to a Corpus document object
#--------------------------------------------------------------------------------------
text_to_Corpus_document <- function(text_data)
{
  corpus_docs <- Corpus(VectorSource(text_data))
  
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  corpus_docs <- tm_map(corpus_docs, toSpace, "/")
  corpus_docs <- tm_map(corpus_docs, toSpace, "@")
  corpus_docs <- tm_map(corpus_docs, toSpace, "\\|")
  
  airline_stopwords <- c("flight","usairway","americanair","southwestair",
                         "jetblu","virginamerica","united","delta")
  
  corpus_docs <- tm_map(corpus_docs, content_transformer(tolower))
  corpus_docs <- tm_map(corpus_docs, removePunctuation)
  corpus_docs <- tm_map(corpus_docs, removeNumbers)
  corpus_docs <- tm_map(corpus_docs, removeWords, stopwords("english"))
  corpus_docs <- tm_map(corpus_docs, removeWords, airline_stopwords)
  
  corpus_docs <- tm_map(corpus_docs, stripWhitespace)
  corpus_docs <- tm_map(corpus_docs, stemDocument)
  
  return(corpus_docs)
}

#--------------------------------------------------------------------------------------
############### Data Preparation for Model
#--------------------------------------------------------------------------------------

# Get text messages and sentiment classification from tweets dataframe
text_data <- tweets$text
sentiment <- tweets$airline_sentiment

# Convert text to document
docs <- text_to_Corpus_document(text_data)

# Convert to Document-Term Matrix
# Remove Sparse Terms
dtm = DocumentTermMatrix(docs)
dtm = removeSparseTerms(dtm,sparse = 0.99)

# Convert matrix to dataframe
model_data = as.data.frame(as.matrix(dtm))

# Convert sentiment classification to factor
sentiment <- as.character(sentiment)
sentiment <- as.factor(sentiment)

# Add sentiment classification to dataframe
model_data$sentiment = sentiment

#--------------------------------------------------------------------------------------
############### Create a Random Forest model
#--------------------------------------------------------------------------------------

# Use 80/20 split for train/test sets
set.seed(123)
split = sample.split(sentiment,SplitRatio = 0.80)
train = subset(model_data,split = TRUE)
test  = subset(model_data,split = FALSE)

# Create a Random Forest model with ntree = 10
predict_rf = randomForest(x = train[,-length(train)], y = train$sentiment, ntree=10)
#summary(predict_rf)

# Prediction on test set
y_pred = predict(predict_rf,newdata = test[,-length(test)])

# Confusion matrix
confusion_matrix = table(test$sentiment, y_pred)
#confusion_matrix

# Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

#--------------------------------------------------------------------------------------
# Function: sentiment_prediction()
#   Input:  A Sentiment Comment Text Message
#   Output: Predicted Sentiment
#--------------------------------------------------------------------------------------
sentiment_prediction <- function(input)
{
  # Get user input text message
  row_num <- which(tweets$text == input)

  # Prepare text message data
  new_model_data <- model_data[row_num,]
  new_data <- new_model_data[,-length(new_model_data)]

  # Get Sentiment Prediction
  predict_result  = predict(predict_rf, newdata = new_data)

  return(predict_result)
}

#######################################################################################