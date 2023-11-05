library(tm)
library(wordcloud)
library(memoise)

# The list of valid books

getTermMatrix <- memoise(function(x) {
  
  x = x |> collect() |> select(brief_title) |> as.character()
  myCorpus = Corpus(VectorSource(x))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})


terms <- reactive({
  # Change when the "update" button is pressed...
  input$update
  # ...but not for anything else
  isolate({
    withProgress({
      setProgress(message = "Processing corpus...")
      getTermMatrix(get_studies())
    })
  })
})


output$wordcloud_plot <- renderPlot({
  v <- terms()
  wordcloud(names(v), v, scale=c(4,0.5),
            min.freq = input$freq, max.words=input$max,
            colors=brewer.pal(8, "Dark2"))
})

library(sf)
library(ggplot2)
library(rnaturalearth)

world <- ne_countries(returnclass = "sf")

# Example data frame
data <- data.frame(
  ISO_A3 = c("CAN", "USA", "MEX", "BRA", "ARG"),
  Value = c(10, 15, 5, 20, 12)
)
world_data <- merge(world, data, by.x = "ISO_A3", by.y = "ISO_A3", all.x = TRUE)
ggplot() +
  geom_sf(data = world_data, aes(fill = Value)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "World Map with Colored Countries", fill = "Value") +
  theme_minimal()


library(rworldmap)

#create a map-shaped window

#join to a coarse resolution map
ddf = read.table(text="
country value
USA 10
UK 30
Sweden 50
Japan 70
China 90
Germany 100
France 80
Italy 60
Nepal 40
Nigeria 100
", header=T)
spdf <- joinCountryData2Map(ddf, joinCode="NAME", nameJoinColumn="country")

mapCountryData(spdf, nameColumnToPlot="value", catMethod="fixedWidth", mapTitle = "Number of Studies Worldwide")

