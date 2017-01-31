install.packages("RISmed")
library(RISmed)
# Define the search topic
search_topic <- "schizophrenia[Title/Abstract] AND incidence[Title/Abstract]"
# Create a search query object, this one will be used to download the records
search_query <- EUtilsSummary(search_topic, retmax=20000)
str(search_query)
# Download the records
records <- EUtilsGet(search_query)
getSlots("Medline")
# Number of records
length(PMID(records))
# Histogram for records per year
par(lwd=1.5, mar=c(6,6,4,4))
hist(YearPubmed(records), breaks=seq(1950,2017,1),
     xlim=c(1950, 2017),
     ylim=c(0,150),
     main=NA,
     xlab="Año de publicación",
     ylab="Número de artículos",
     xaxt="n",
     xaxs="i",
     yaxt="n",
     yaxs="i",
     col="#838B83",
     border="white")
axis(1, at=(seq(1950, 2015, 5)), las=3)
axis(2, at=(seq(0, 150, 25)))
abline(h=25, lty=3)
# Plot the map for the countries were the studies were performed
library(rworldmap)
CountryPub <- as.data.frame(matrix(nrow=56, ncol=2))
colnames(CountryPub) <- c("Country", "Number_of_publications")
CountryPub$Country <-names(table(Country(records)))
CountryPub$Number_of_publications <- table(Country(records))
Quantiles_country_pub <- quantile(table(Country(records)))
CountryPub$Quantile  <- NA
for(row in seq_along(CountryPub[,1])){
  if(CountryPub$Number_of_publications[row] <= Quantiles_country_pub[2]){
    CountryPub$Quantile[row] <- "25%"
  }
  if(CountryPub$Number_of_publications[row] > Quantiles_country_pub[2] & CountryPub$Number_of_publications[row] <= Quantiles_country_pub[3] ){
    CountryPub$Quantile[row] <- "50%"
  }
  if(CountryPub$Number_of_publications[row] > Quantiles_country_pub[3] & CountryPub$Number_of_publications[row] <= Quantiles_country_pub[4]){
    CountryPub$Quantile[row] <- "75%"
  }
  if(CountryPub$Number_of_publications[row] > Quantiles_country_pub[4] & CountryPub$Number_of_publications[row] <= Quantiles_country_pub[5]){
    CountryPub$Quantile[row] <- "100%"
  }
}
spdf <- joinCountryData2Map(CountryPub, joinCode="NAME", nameJoinColumn="Country")
par(mar=c(5,5,5,5))
mapCountryData(spdf, nameColumnToPlot="Number_of_publications",
               borderCol = "black",
               catMethod = "categorical",
               numCats = 1,
               addLegend = T,
               lwd =0.5)
# World cloud for the abstracts
library(tm)
library(SnowballC)
library(wordcloud)
library(sp)
abstractCorpus <- Corpus(VectorSource(AbstractText(records)))
abstractCorpus <- tm_map(abstractCorpus, PlainTextDocument, mc.cores = 1)
abstractCorpus <- tm_map(abstractCorpus, removePunctuation, mc.cores = 1)
abstractCorpus <- tm_map(abstractCorpus, removeNumbers, mc.cores = 1)
#abstractCorpus <- tm_map(abstractCorpus, tolower)
abstractCorpus <- tm_map(abstractCorpus, removeWords, c(stopwords('english'), "the", "this", "there"), mc.cores = 1)
#abstractCorpus <- tm_map(abstractCorpus, stemDocument,mc.cores = 1)
#abstractCorpus <- tm_map(abstractCorpus, stemCompletion,dicitionary=abstractCorpus, type="prevalence",mc.cores = 1)
wordcloud(abstractCorpus, max.words = 100, random.order = FALSE)
# World cloud for the Mesh terms associated to each paper
MeshRecords <- Mesh(records)
Mesh_nonNA <- MeshRecords[sapply(MeshRecords, is.data.frame)]
Mesh_Headings <- lapply(Mesh_nonNA, function(x) x$Heading)
MeshCorpus <- Corpus(VectorSource(Mesh_Headings))
MeshCorpus <- tm_map(MeshCorpus, PlainTextDocument, mc.cores = 1)
MeshCorpus <- tm_map(MeshCorpus, removePunctuation, mc.cores = 1)
MeshCorpus <- tm_map(MeshCorpus, removeNumbers, mc.cores = 1)
MeshCorpus <- tm_map(MeshCorpus, removeWords, c(stopwords('english'), "the", "this") , mc.cores = 1)
wordcloud(MeshCorpus, max.words = 100, random.order = FALSE)
# World cloud for the titles
TitleCorpus <- Corpus(VectorSource(ArticleTitle(records)))
TitleCorpus <- tm_map(TitleCorpus, PlainTextDocument, mc.cores = 1)
TitleCorpus <- tm_map(TitleCorpus, removePunctuation, mc.cores = 1)
TitleCorpus <- tm_map(TitleCorpus, removeNumbers, mc.cores = 1)
TitleCorpus <- tm_map(TitleCorpus, removeWords, c("the", "this", stopwords('english')), mc.cores = 1)
#TitleCorpus <- tm_map(TitleCorpus, stemDocument,mc.cores = 1)
wordcloud(TitleCorpus, max.words = 100, random.order = FALSE)




length(unique(ArticleTitle(records)))
table(Language(records))
ArticleTitle(records)[Language(records) == "chi"]
AbstractText(records)[Language(records) == "eng"]
