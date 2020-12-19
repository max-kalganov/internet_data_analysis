library("stringr")
library ("RCurl")
library ("XML")

new_results <- '/government/announcements?keywords=&announcement_filter_option=press-releases&topics[]=all&departments[]=all&world_locations[]=all& from_date=&to_date=01%2F07%2F2018'
signatures = system.file("CurlSSL", cainfo = "cacert.pem",
                         package = "RCurl")
all_links <- character()

while(length(new_results) > 0){
  new_results <- str_c("https://www.gov.uk/", new_results)
  results <- getURL(new_results, cainfo = signatures)
  results_tree <- htmlParse(results)
  all_links <- c(all_links, xpathSApply(results_tree,
                                        "//li[@id]//a", xmlGetAttr, "href"))
  new_results <- xpathSApply(results_tree,
                             "//nav[@id='show-more-documents']//li[@class='next']//a",
                             xmlGetAttr, "href")
}

for(i in 1:length(all_links)){
  url <- str_c("https://www.gov.uk", all_links[i])
  tmp <- getURL(url, cainfo = signatures)
  write(tmp, str_c("Press_Releases/", i, ".html"))
}

tmp <- readLines("Press_Releases/1.html")
tmp <- str_c(tmp, collapse = "")
tmp <- htmlParse(tmp)

release <- xpathSApply(tmp, "//div[@class='block-4']", xmlValue)
organisation <- xpathSApply(tmp, "//a[@class='organisation-link']", xmlValue)
publication <- xpathSApply(tmp, "//div[@class='block-5']//time[@class='date']", xmlValue)

library(tm)
release_corpus <- Corpus(VectorSource(release))

meta(release_corpus[[1]], "organisation") <- organisation[1]
meta(release_corpus[[1]], "publication") <- publication
meta(release_corpus[[1]])

n <- 1
for(i in 2:length(list.files("Press_Releases/"))){
  tmp <- readLines(str_c("Press_Releases/", i, ".html"))
  tmp <- str_c(tmp, collapse = "")
  tmp <- htmlParse(tmp)
  release <- xpathSApply(tmp,"//div[@class='block-4']", xmlValue)
  organisation <- xpathSApply(tmp, "//a[@class='organisation-link']", xmlValue)
  publication <- xpathSApply(tmp,  "//div[@class='block-5']//time[@class='date']",  xmlValue)
  if (length(release)!=0 & 
      (organisation == 'Department for Business, Innovation & Skills' |
       organisation == 'Ministry of Defence' |
       organisation == 'Foreign & Commonwealth Office')) {
    n <- n + 1
    tmp_corpus <- Corpus(VectorSource(release))
    release_corpus <- c(release_corpus, tmp_corpus)
    meta(release_corpus[[n]], "organisation") <- organisation[1]
    cat("n=",n)
  }
}

meta_data<- data.frame()
for (i in 1:NROW(release_corpus))
{
  meta_data [i, "organisation"] <- meta(release_corpus[[i]], "organisation") 
  meta_data [i, "num"] <- i 
  
}

table(as.character(meta_data[, "organisation"]))

release_corpus <- tm_map(release_corpus, content_transformer(removeNumbers))

release_corpus <- tm_map(release_corpus,   
                         content_transformer(str_replace_all), 
                         pattern = "[[:punct:]]", replacement = " ")

release_corpus[[1]]$content

release_corpus <- tm_map(release_corpus, content_transformer(removeWords), words =  stopwords("en"))
release_corpus <- tm_map(release_corpus, content_transformer(tolower))
release_corpus <- tm_map(release_corpus, stemDocument, language = "english")
tdm <- TermDocumentMatrix(release_corpus)

dtm <- DocumentTermMatrix(release_corpus)
dtm <- removeSparseTerms(dtm, 1-(10/length(release_corpus)))

library(RTextTools)

org_labels<-meta_data[, "organisation"]

N <- length(org_labels)

container <- create_container(
  dtm,
  labels = org_labels, 
  trainSize = 1:350,
  testSize = 351:N,
  virgin = FALSE
)

svm_model <- train_model(container, "SVM")
tree_model <- train_model(container, "TREE")
maxent_model <- train_model(container, "MAXENT")
  
svm_out <- classify_model(container, svm_model)
tree_out <- classify_model(container, tree_model)
maxent_out <- classify_model(container, maxent_model)

labels_out <- data.frame(
  correct_label = org_labels[351:N],
  svm = as.character(svm_out[,1]),
  tree = as.character(tree_out[,1]),
  maxent = as.character(maxent_out[,1]),
  stringsAsFactors = F)

table(labels_out[,1] == labels_out[,2])
table(labels_out[,1] == labels_out[,3])
table(labels_out[,1] == labels_out[,4])







