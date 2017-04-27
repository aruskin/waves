source('1_preprocess_text.R') # make dataset
library(data.table)

to_freq_matrix <- function(word.matrix, authors){
  word.matrix <- as.data.table(word.matrix)
  for (j in seq_len(ncol(word.matrix)))
    set(word.matrix, which(is.na(word.matrix[[j]])),j,0)
  word.matrix[, total_words := rowSums(.SD)]
  apply(word.matrix, 2, function(x) x/word.matrix$total_words) %>%
    as.data.table %>%
    select(-total_words) %>%
    mutate(SPEAKER = authors)
}

freq.matrix <- to_freq_matrix(select(waves.text.words, -ID, -SPEAKER), 
                              waves.text.words$SPEAKER) %>%
  as.data.table

words <- colnames(freq.matrix)[1:(ncol(freq.matrix) - 1)]
# what proportion of the texts does each word appear in?
word.density <- sapply(words, 
                       function(x) 
                         sum(freq.matrix[,x, with=FALSE] > 0)/nrow(freq.matrix))

# keep only words that appear in at least 1% of the excerpts
# we should probably wrap all of this in a function
commonish.words <- word.density[which(word.density > .01)]
freq.matrix <- select(freq.matrix, one_of(names(commonish.words)), SPEAKER)
freq.matrix$SPEAKER <- factor(freq.matrix$SPEAKER)

# divide into training and test sets
set.seed(123)
train.rows <- sample(1:nrow(freq.matrix), size = .7*nrow(freq.matrix))

train.data <- freq.matrix[train.rows,]
test.data <- freq.matrix[-train.rows,]

train.label <- train.data$SPEAKER
test.label <- test.data$SPEAKER

########### XGBoost ########################
library(xgboost)

train.binary <- xgb.DMatrix(select(train.data, -SPEAKER) %>% as.matrix,
                            label=as.numeric(train.label)-1)
test.binary <- xgb.DMatrix(select(test.data, -SPEAKER) %>% as.matrix,
                           label=as.numeric(test.label)-1)

searchParams <- expand.grid(max_depth=c(5, 7),
                            lambda=c(10, 100, 1000),
                            gamma=c(0.25, 0.5),
                            colsample_bytree=c(.5, .75, 1))
ntrees <- 25
# For reference https://datascience.stackexchange.com/questions/9364/hypertuning-xgboost-parameters/9368
paramsOut <- apply(searchParams, 1, function(params) {
  xgbCV <- xgb.cv(data=train.binary, nrounds=ntrees, nfold=5, showsd=TRUE,
                  "eval_metric" = "mlogloss", "objective" = "multi:softprob",
                  "num_class" = 7, "eta" = 10/ntrees, 
                  "max_depth" = params[["max_depth"]],
                  "lambda" = params[["lambda"]],
                  "gamma" = params[["gamma"]],
                  "colsample_bytree" = params[["colsample_bytree"]])
  xval.error <- xgbCV$evaluation_log$test_mlogloss_mean
  perf_last <- xval.error[ntrees]
  perf_best <- xval.error[which.min(xval.error)]
  rm(xgbCV)
  gc()
  return(c(perf_last, perf_best, params[["max_depth"]], params[["lambda"]],
           params[["gamma"]],  params[["colsample_bytree"]]))
})

#get parameters with lowest cv error
xgb.params <- paramsOut[, which.min(paramsOut[2,])]
# [1]  1.543841  1.541482  5.000000 10.000000  0.500000  1.000000


init.model <- xgb.train(data=train.binary, nrounds=150, 
                        "eval_metric" = "mlogloss", "objective" = "multi:softprob",
                        "num_class" = 7, "eta" = .1, 
                        "subsample" = 0.5,
                        "max_depth" = xgb.params[3],
                        "lambda" = xgb.params[4],
                        "gamma" = xgb.params[5],
                        "colsample_bytree" = xgb.params[6],
                  watchlist=list(test=test.binary, train=train.binary),
                  verbose = TRUE)
# see test error over n iterations
plot(1:150, init.model$evaluation_log$test_mlogloss, type="l")
imp <- xgb.importance(feature_names = colnames(train.data), 
                      model=init.model)

library(caret)
xgb.confusion <- function(model, binary){
  preds <- predict(model, binary, reshape=TRUE) %>%
    as.data.table
  preds$pred_label <- apply(preds, 1, which.max) - 1
  preds$actual_label <- getinfo(binary, 'label')
  caret::confusionMatrix(data=preds$pred_label,
                         reference=preds$actual_label)
}

xgb.confusion(init.model, train.binary)
xgb.confusion(init.model, test.binary)
# so this is a kind of shit model, eh?
# turns out we're way, way overfitting -- 98.2% accuracy on training data vs
# 40.72% accuracy on test data; may want to revisit parameters