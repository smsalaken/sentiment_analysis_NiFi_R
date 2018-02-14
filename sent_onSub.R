# sentiment analysis of another topic 
# redundantly defined due to lack of time
# see sent.R for proper comments
sentiment_plot_onSub <- function(){
  
  source('extras.R', local = T)
  
  library(jsonlite)
  library(tidytext)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(jsonlite)
  library(gganimate)
  
  
  
  
  all_files <- list.files('target/aboutSubject/')
  
  df <- data.frame(created_at = as.POSIXct(character()),tweets = character(), stringsAsFactors = F)

  for (i in 1:length(all_files)){
    tt <- jsonlite::read_json(paste0('target/aboutSubject/',all_files[i]))
    df[i, 'tweets'] <- tt$t.tweet
    df[i, 'created_at'] <- as.POSIXct(strptime(tt$t.creationTime, format = "%a %b %d %H:%M:%S %z %Y", tz = "GMT"))
    
  }
  
  
  unique_df <- unique(df)
  unique_df$timemarker <- paste0(as.character(format(unique_df$created_at, "%Y-%m-%d")), " ",as.character(format(unique_df$created_at, "%H")), ":",as.character(format(unique_df$created_at, "%M")))
  
  
  d2 <- unique_df %>% unnest_tokens(word, tweets)
  
  
  nrc <- get_sentiments("nrc")
  
  d3 <- d2 %>%
    inner_join(nrc) %>%
    group_by(sentiment, timemarker) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  
  
  colorvalues <- c("Negative" = "#CC3B3B", "Anger" = "#EE841B", "Fear"= "#656571", "Trust" = "#068923", "Disgust" = "#A75067",
                   "Positive" = "#025816","Sadness" = "#EACF27", "Anticipation" = "#09B0A1", "Surprise" = "#4A7BAF", "Joy" = "#76EA8F")
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  
  
  d3$sentiment <- sapply(d3$sentiment, simpleCap)
  
  d3 <- d3[order(-d3$count),]
  
  
  # p <- ggplot(d3, aes(x = sentiment, y = count/sum(count), label = sentiment, size = count, colour = sentiment)) +
  #   geom_point(aes(size = count)) +
  #   geom_line(group = 1) +
  #   theme_minimal() +
  #   theme(legend.position = "none",
  #         axis.text.x = element_blank(),
  #         axis.text.y = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.ticks.y = element_blank(),
  #         panel.grid = element_blank()
  #   )+
  #   labs(x = "", y = "") +
  #   scale_color_manual(values = colorvalues)+
  #   geom_text(color = "black", size = 6)
  # 
  # r <- list()
  # r$plot <- p
  
  
  
  coord_radar <- function (theta = "x", start = 0, direction = 1) 
  {
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") 
      "y"
    else "x"
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
            direction = sign(direction),
            is_linear = function(coord) TRUE)
  }
  manual_colors <- c("#0c0887", "#4b03a1", "#7d03a8", "#a82296", "#cb4679", "#e56b5d", "#f89441",
                     "#fdc328", "#f0f921", "#aadc32", "#21908d")
  
  
  d3scaled <- as.data.frame(d3)
  d3scaled$count <- d3scaled$count/max(d3scaled$count) 
  # d3scaled$model <- rownames(mtcars)
  
  p <- ggplot(d3scaled, aes(x = sentiment, y = count, frame = timemarker)) +
    #geom_polygon(aes(group = sentiment, color = sentiment), fill = NA, size = 2, show.legend = FALSE) +
    #geom_line(aes(group = sentiment, color = sentiment), size = 2, group = 1) +
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
  
  #gganimate(p, "anim_ONsubject.gif")
  
  
  manual_colors <- c("#0c0887", "#4b03a1", "#7d03a8", "#a82296", "#cb4679", "#e56b5d", "#f89441",
                     "#fdc328", "#f0f921", "#aadc32", "#21908d")
  
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
  
  
  
  r <- list()
  r$plot <- p
  r$bar <- p_bar
  
  return(r)
  
}





