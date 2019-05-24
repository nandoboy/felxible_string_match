setwd('C:/Users/Nanda/useful/code/string_similarity')
#amazon <- data.frame(read.csv('Amazon.csv',stringsAsFactors = F))
#google <- data.frame(read.csv('GoogleProducts.csv',stringsAsFactors = F))
sample_file <- data.frame(read.csv('sample_file.csv',stringsAsFactors = F))

#calculating inverse document frequency
sample_file$Google_names <- gsub('( )+-( )+',' ',sample_file$Google_names)
sample_file$Google_names <- gsub('( ){2,}',' ',sample_file$Google_names)
sample_file$Amazon <- gsub('( )+-( )+',' ',sample_file$Amazon)
sample_file$Amazon <- gsub('( ){2,}',' ',sample_file$Amazon)


ggl_vec <- lapply(X = sample_file$Google_names,
                  function(x) strsplit(x,split = " "))
amz_vec <- lapply(X = sample_file$Amazon,
                  function(x) strsplit(x,split = " "))

#new_vec <-
#  c(unlist(lapply(X = amazon$title,function(x) strsplit(x,split = " "))),
#  unlist(lapply(X = google$name,function(x) strsplit(x,split = " "))))



nv1 <- c(unlist(ggl_vec),unlist(amz_vec))
  
#computing inverse document frequency vector
#head(sort(table(nv1),decreasing = T))
idf <- data.frame(sort(table(nv1),decreasing = T))
colnames(idf) <- c('word','count')
idf$score <- round(log(sum(idf$count)/idf$count),6)

#the matching part

#match-frame for storing scores
match_frame <- data.frame(google_prod <- c(),amazon_prod <- c(),
                          match_score <- c())

for(i in sample_file$Google_names){
  for(j in sample_file$Amazon){
#string split, compare and score calculator function here
    score <- mscore_calc(i,j,idf)
#insert into match_frame if non-zero scores
    if(score > 0){
      print(c("Score :",score))
      match_frame <- rbind(match_frame,data.frame(i,j,score))
    }
  }
}

mscore_calc <- function(i,j,idf){
#print('Inside func call')
tot_score <- 0

#possible improvements
# - do not run loop beyond first equality (pending further analysis)
# - do not run loop for one-count tokens (done)

#split strings by spaces
# i <- gsub(pattern = '-',replacement = ' ',i)
# i <- gsub(pattern = '( ){2,}',' ',i)
# j <- gsub(pattern = '-',replacement = ' ',j)
# j <- gsub(pattern = '( ){2,}',' ',j)

# print(i)
# print(j)

vec_1 <- unlist(strsplit(i,' '))
vec_2 <- unlist(strsplit(j,' '))

#print(vec_1)
#print(vec_2)
  
#compare for the love of god!!
  for(a in vec_1){
#    print(a)
    if(idf$count[tolower(idf$word) == tolower(a)] != 1){
      for(b in vec_2){
        if(idf$count[tolower(idf$word) == tolower(a)] != 1){
          if(tolower(a) == tolower(b)){
            tot_score <- tot_score + idf$score[tolower(idf$word) == tolower(a)]
          }
        }
      }
    }
  }
#  print(tot_score)
  return(tot_score)
}
