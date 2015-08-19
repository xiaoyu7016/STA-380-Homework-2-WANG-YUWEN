library(tm)

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)),id=fname,language='en') 
}

toDenseDTM = function(aList,anInt){
  file_list = NULL
  labels = NULL
  for(author in aList) {
    author_name = substring(author, first=anInt)
    files_to_add = Sys.glob(paste0(author, '/*.txt'))
    file_list = append(file_list, files_to_add)
    labels = append(labels, rep(author_name, length(files_to_add)))
  }
  
  all_docs = lapply(file_list, readerPlain) 
  names(all_docs) = file_list
  names(all_docs) = sub('.txt', '', names(all_docs))
  
  my_corpus = Corpus(VectorSource(all_docs))
  names(my_corpus) = file_list
  
  # Preprocessing
  my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
  my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
  my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
  my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
  my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))
  
  DTM = DocumentTermMatrix(my_corpus)
  DTM = removeSparseTerms(DTM, 0.95)
  
}

author_dirs = NULL
left_strip = NULL

Initialize_train = function(aFlag){
  if (aFlag == T){
    author_dirs = Sys.glob('ReutersC50/C50train/*')
    left_strip = 21 
  } else {
    author_dirs = Sys.glob('ReutersC50/C50test/*')
    left_strip = 20  
  }
  
  as.matrix(toDenseDTM(author_dirs,left_strip))
  
}

train = Initialize_train(T)
test = Initialize_train(F)

## eval = F
author_dirs = Sys.glob('ReutersC50/C50train/*')
left_strip = 21 
labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=left_strip)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  labels = append(labels, rep(author_name, length(files_to_add)))
}

# Smooth Factor
smooth_count = 1/nrow(train)

score_df = data.frame(labels)

for (i in seq(50,2500, by=50)){
  same_author = train[(i-50+1):i,]
  w_smoothed = colSums(same_author + smooth_count)
  w_same_author = w_smoothed/sum(w_smoothed)
  
  scores = NULL
  for (j in 1:2500){
    mask <- intersect(colnames(test),names(w_same_author))
    NB_score = sum(test[j,mask]*log(w_same_author[mask]))
    scores = append(scores,NB_score)
  }
  
  score_df[,labels[i]] <- scores
  
}
score_df.t <-t(score_df)
colnames(score_df.t) <-score_df.t[1,]
score_df.t <- score_df.t[-1,]

pred = NULL
for (i in 1:2500){
  if(names(which.max(score_df.t[,i]))==colnames(score_df.t)[i]){
    pred = append(pred,1)
  } else {
    pred = append(pred,0)
  }
}

sum(pred)/length(pred)


accuracy_by_author <- NULL
for(i in seq(50,2500,by=50)){
  pct <- sum(pred[(i-50+1):i])/length(pred[(i-50+1):i])
 
  accuracy_by_author <- append(b, pct)
}

accuracy_by_author[accuracy_by_author>0.75]
accuracy_by_author[accuracy_by_author<0.25]




