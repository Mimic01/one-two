#download.file("https://snap.stanford.edu/data/finefoods.txt.gz", "finefoods.txt.gz")
library(readr)
library(stringr)
library(coreNLP)
#reviews<-read_lines("finefoods.txt.gz") 
#reviews<-reviews[str_sub(reviews, 1, 12) == "review/text:"]
#reviews<-str_sub(reviews, start = 14)
#reviews<-iconv(reviews, to = "UTF-8")
#head(reviews, 2)
# update to newest version of the package
devtools::install_github("statsmaths/coreNLP")
# download base library (mandatory):
coreNLP::downloadCoreNLP()
# download desired language library:
coreNLP::downloadCoreNLP(type="spanish")
# attach package
library(coreNLP)
# run initCoreNLP specifying your language of choice
initCoreNLP(type="spanish")

setwd("C:/Users/Alex/Documents/Sentiment_Analysis/Corpus")
filename<-"skipgram_text_input.txt"
skipo<-paste(scan(filename,what="character",sep=NULL),collapse=" ")
Encoding(skipo)<-"UTF-8"
skipo<-gsub(pattern="NA",replacement="",skipo,fixed=TRUE)


library(keras)
tokenizer<-text_tokenizer(num_words=200)
tokenizer%>%fit_text_tokenizer(skipo)

library(reticulate)
library(purrr)
skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  function() {
    next_value <- generator_next(gen)
    if(is.null(next_value)){ #if there isn't new text from the generator
      gen <<- texts_to_sequences_generator(tokenizer, sample(text)) # remake the generator
      next_value <- generator_next(gen)
    }
    skip <- next_value %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}
embedding_size<-20  # Dimension of the embedding vector.
skip_window<-5       # How many words to consider left and right.
num_sampled<-0       # Number of negative examples to sample for each word.

input_target<-layer_input(shape=1)
input_context<-layer_input(shape=1)
#Embedding matrix
embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1, 
  output_dim = embedding_size, 
  input_length = 1, 
  name = "embedding"
)

target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten()

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()

dot_product<-layer_dot(list(target_vector, context_vector), axes = 1)
output<-layer_dense(dot_product, units = 1, activation = "sigmoid")

model<-keras_model(list(input_target, input_context), output)
model%>%compile(loss="binary_crossentropy",optimizer="adam")
summary(model)

model %>%
  fit_generator(
    skipgrams_generator(skipo, tokenizer, skip_window, negative_samples), 
    steps_per_epoch=1000,epochs=1
  )

library(dplyr)

embedding_matrix <- get_weights(model)[[1]]

words <- data_frame(
  word = names(tokenizer$word_index), 
  id = as.integer(unlist(tokenizer$word_index))
)

words <- words %>%
  filter(id <= tokenizer$num_words) %>%
  arrange(id)

row.names(embedding_matrix) <- c("UNK", words$word)

library(text2vec)

find_similar_words <- function(word, embedding_matrix, n = 5) {
  similarities <- embedding_matrix[word, , drop = FALSE] %>%
    sim2(embedding_matrix, y = ., method = "cosine")
  
  similarities[,1] %>% sort(decreasing = TRUE) %>% head(n)
}

find_similar_words("2", embedding_matrix)
find_similar_words("evo", embedding_matrix)
find_similar_words("chile", embedding_matrix)
find_similar_words("presidente", embedding_matrix)
##################################################################### Entregra resultados##############################################


library(Rtsne)
library(ggplot2)
library(plotly)

tsne <- Rtsne(embedding_matrix[2:500,], perplexity = 50, pca = FALSE)

tsne_plot <- tsne$Y %>%
  as.data.frame() %>%
  mutate(word = row.names(embedding_matrix)[2:500]) %>%
  ggplot(aes(x = V1, y = V2, label = word)) + 
  geom_text(size = 3)
tsne_plot