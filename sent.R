# sentiment analysis of a topic/subject
sentiment_plot <- function(){
  
source('extras.R', local = T)
  

library(jsonlite)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(jsonlite)
library(gganimate)

# read files from where the tweets on a topic is being saved
all_files <- list.files('target/subject/')

df <- data.frame(created_at = as.POSIXct(character()),tweets = character(), stringsAsFactors = F)

for (i in 1:length(all_files)){
  # read individual tweets from json outputs created by Ni-Fi 
  tt <- jsonlite::read_json(paste0('target/subject/',all_files[i]))
  
  df[i, 'tweets'] <- tt$t.tweet
  df[i, 'created_at'] <- as.POSIXct(strptime(tt$t.creationTime, 
                                             format = "%a %b %d %H:%M:%S %z %Y", 
                                             tz = "GMT"))

}

# we will get a lot of duplicate tweets frimn Twitter
unique_df <- unique(df)

# it's meaningleass to show per second summary for my purpose
# let's aggregate on per minute summary basis
unique_df$timemarker <- paste0(as.character(format(unique_df$created_at, "%Y-%m-%d")), 
                               " ",
                               as.character(format(unique_df$created_at, "%H")), 
                               ":",
                               as.character(format(unique_df$created_at, "%M")))


# tokenize the words. At this moment, we are using unigrams.
# Even though this will not produce the best sentiment analysis
# on tweets, this is fine for demo (at least for the lack of a readily available n-gram dictionary)
d2 <- unique_df %>% unnest_tokens(word, tweets)


# load the dictionary dataset
nrc <- get_sentiments("nrc")

# get the sentiment summary per minute
d3 <- d2 %>%
  inner_join(nrc) %>%
  group_by(sentiment, timemarker) %>%
  summarise(count = n()) %>%
  arrange(desc(count))




# capitalize the sentiment words, just beautification
d3$sentiment <- sapply(d3$sentiment, simpleCap)

# order by dominant sentiment
d3 <- d3[order(-d3$count),]


# scale the sentiment counts to create a coherent plot, also works 
# as probability distribution
d3scaled <- as.data.frame(d3)
d3scaled$count <- d3scaled$count/max(d3scaled$count) 

# do a spider/radar plot with a frame argument for animation
p <- ggplot(d3scaled, aes(x = sentiment, y = count, frame = timemarker)) +
      geom_point(aes(color = sentiment, size = count)) + 
      theme_minimal() +
      theme(strip.text.x = element_text(size = rel(0.8)),
            axis.text.x = element_text(size = rel(0.8), face = "bold"),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(size = 1),
            axis.text = element_text(size = 12),
            legend.position = "none") +
      xlab("") + ylab("") +
      guides(color = guide_legend(ncol=2)) +
      scale_color_manual(values = manual_colors) +
      coord_radar() 


# do a bar plot to clearly show the distribution on the aggregated dataset
p_bar <- ggplot(d3 %>% group_by(sentiment) %>% summarise(n = sum(count)) %>% mutate(freq = n / sum(n)), 
                aes(x = sentiment, y = freq, fill = sentiment)) +
          geom_col() +
          theme_minimal(base_size=9) +
          theme(legend.position = "none",
                axis.text = element_text(size = 9, colour = "black", face = "bold"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                axis.line.x = element_blank()
                ) +
        labs(x = "", y = "")  +
        scale_fill_manual(values = manual_colors) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.3))

    
# save the results in a list and return
r <- list()
r$plot <- p
r$bar <- p_bar

return(r)

}





