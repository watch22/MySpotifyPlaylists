#My year in music: Spotify Listening History
#load packages ----
# devtools::install_github("lchiffon/wordcloud2")
# install.packages("paletteer")
# install.packages("cartography")
# devtools::install_github("EmilHvitfeldt/paletteer")

pacman::p_load(tidyverse, lubridate, knitr, ggjoy, genius, tidytext, kableExtra, extrafont, ggthemes, ggrepel, magick, webshot, jsonlite, httr, zoo, plotly, wordcloud, RColorBrewer, cowplot, wordcloud2, paletteer)

#aesthetics ----
plotfont <- "Open Sans Condensed"
spotify_green <- "#1DB954"
wordcloud_palette <- c("#ffffff","#e9fcef","#d3f8e0","#bdf5d0","#a7f1c1","#91eeb1","#7beaa2","#65e792","#4fe383","#38e073","#22dd64","#1fc75a","#1db954","#1cb050","#189a46")



#import clean data set ----
song_df <- read_csv(file = here::here("data", "spotify_history_2108229_220831_clean_finaler.csv"))

#clean ----
colnames(song_df)

song_df_clean <- song_df %>% select(-c(key,mode,artist_uri,album_uri,track_uri,time_signature))
  
#get weekdays from date
song_df_clean <- song_df_clean %>% 
  filter(date > as.Date('2021-08-31')) %>% 
  complete(date = seq(as.Date("2021-09-01"), as.Date("2022-08-31"), by = "day")) %>% 
  mutate(day = day(date),
         wkday = fct_relevel(wday(date, label = TRUE, week_start = 1),c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
         ,
         wk = as.numeric(format(date,"%W")),
         month = as.factor(format(date, "%b-%y")),
         genres = as.factor(genres))

song_df_clean$month <- factor(song_df_clean$month,levels(song_df_clean$month)[c(2,6,7,9,1,8,4,5,3,10,11,12)])
# song_df_clean$month <- factor(song_df_clean$month,levels(song_df_clean$month)[c(12:1)])

#OUTPUTS ----
#1. Heat maps  -----
#1a. Weekdays ------
listen_heat_weekdays <- song_df_clean %>% 
  filter(!(is.na(msPlayed))) %>% 
  group_by(date, wkday, month = floor_date(date, "month")) %>% 
  summarise(Listening_Time = signif(sum(msPlayed)/60000,2)) %>% 
  mutate(Listening_Time = zoo::na.fill(Listening_Time,0)) %>% 
  ggplot(aes(x = month, y = fct_rev(wkday), fill = Listening_Time, 
             text = paste("Avg listening time", round(Listening_Time,0),"mins"))) +
  geom_tile(linejoin = "round")+
  scale_x_date(date_labels = "%b '%y",
               date_breaks = "month",
               expand = c(0,0)) + 
  labs(x = '', y = '') + 
  scale_fill_paletteer_c("ggthemes::Classic Area Green", name = "Listening Time (mins)") +
  theme_minimal(base_family = plotfont) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12, colour = wordcloud_palette[2]),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, colour = wordcloud_palette[2]),
        panel.grid = element_blank(),
        aspect.ratio = (0.25),
        plot.background = element_rect(fill = "black"),
        legend.position = "none",
        plot.margin = margin(10,10,10,10),
        title = element_text(size = 15, colour = wordcloud_palette[2])) + 
  labs(title= "listening time (weekly)")

listen_heat_weekdays

plotly::ggplotly(listen_heat, tooltip = "text", originalData = TRUE)

#1b. months ----
listen_time_months <- song_df_clean %>% 
  filter(!(is.na(msPlayed))) %>% 
  group_by(month = floor_date(date, "month")) %>% 
  summarise(Listening_Time = signif(sum(msPlayed)/60000,2))

  
listen_heat_months <- ggplot(data = listen_time_months, aes(x = month, y = 100, fill = Listening_Time, label = Listening_Time)) + 
  geom_tile(linejoin = "round") + 
  #geom_text(family = plotfont, angle = 90, colour = wordcloud_palette[1], size = 6) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_paletteer_c("ggthemes::Classic Area Green", name = "listening time (mins)") +
  scale_x_date(date_labels = "%b '%y",
               date_breaks = "month",
               expand = c(0,0)) + 
  theme_minimal(base_family = plotfont) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, colour = wordcloud_palette[2]),
        aspect.ratio = c(0.15),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        legend.text = element_blank(),
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.key.size = unit(0.75, "cm"),
        legend.title = element_text(size = 12, colour = wordcloud_palette[2]),
        legend.title.align = 0.5,
        legend.position = "bottom",
        plot.margin = margin(10,10,10,10),
        title = element_text(size = 15, colour = wordcloud_palette[2])) + 
  labs(title= "listening time (monthly)")

listen_heat_months

#combine plots
p <- plot_grid(listen_heat_weekdays + theme(legend.position = "none"), 
               listen_heat_months + theme(legend.position = "none"),
               align = "v", ncol = 1)
p
#extract legend to add later
heatmap_legend <- get_legend(
  listen_heat_months + theme(legend.box.margin = margin(0, 0, 0, 10),
                             legend.justification = "center",
                             legend.direction = "horizontal")
)
#add legend
p_legend <- plot_grid(p ,heatmap_legend,ncol = 1, rel_heights = c(1, .1))
#add background
p_final <- cowplot::ggdraw(p_legend) + theme(
  plot.background = element_rect(fill = "black", color = NA)
)
p_final

#2a. Genres per period ----
#spotify genres
genres_spotify_time <- song_df_clean %>% 
  filter(!is.na(genres_spotify)) %>% 
  group_by(month = floor_date(date, "month"), genres_spotify) %>% 
  summarise(genre_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(genre_mins == max(genre_mins))

#grouped genres
song_df_clean %>% 
  group_by(month, genres) %>% 
  summarise(genre_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(genre_mins == max(genre_mins))

#2b. Genres overall ----
#spotify genres
top_genres_spotify <- song_df_clean %>% 
  filter(!is.na(genres_spotify)) %>% 
  group_by(genres_spotify) %>% 
  summarise(genre_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins)) %>% 
  head(20) %>% pull(genres_spotify)

#column chart
song_df_clean %>% 
  filter(!is.na(genres_spotify)) %>% 
  group_by(genres_spotify) %>%  
  summarise(genre_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins)) %>% 
  head(20) %>% 
  ggplot(aes(x = genres_spotify, y = genre_mins, fill = genre_mins)) +
  geom_col() + 
  scale_x_discrete(limits = rev(top_genres_spotify[1:20])) + 
  coord_flip() + 
  scale_fill_distiller(palette = 5, direction = 1) + 
  labs(x = "", y = "") + 
  theme_minimal(base_family = plotfont)

#word cloud
genres_spotify_wc <- song_df_clean %>% 
  filter(!is.na(genres_spotify)) %>% 
  group_by(genres_spotify) %>%  
  summarise(genre_mins = round(sum(msPlayed/60000, na.rm = T),0)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins))

# wordcloud(words = genres_spotify_wc$genres_spotify, freq = genres_spotify_wc$genre_mins, 
#           min.freq = 1,max.words = 100, 
#           random.order = F, 
#           rot.per = 0.5, 
#           colors = brewer.pal(5, "Accent"), random.color = T)

wordcloud2(genres_spotify_wc,
           color = paletteer_c("ggthemes::Classic Green", n = 20),
           backgroundColor = "black",
           fontFamily = plotfont,
           minSize = 5,
           size = 2.5,
           maxRotation = pi/2,
           minRotation = pi/2,
           rotateRatio = 0.25)


#grouped genres
top_genres <- song_df_clean %>% 
  group_by(genres) %>% 
  summarise(genre_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins)) %>% 
  head(20) %>% pull(genres)

song_df_clean %>% 
  group_by(genres) %>% 
  summarise(genre_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins)) %>% 
  head(10) %>% 
  ggplot(aes(x = genres, y = genre_mins, fill = genre_mins)) +
  geom_col() + 
  scale_x_discrete(limits = rev(top_genres[1:10])) + 
  coord_flip() + 
  scale_fill_distiller(palette = 'PuBu', direction = 1) + 
  labs(x = "", y = "") + 
  theme_minimal() + 
  theme(panel.background = element_rect(fill = "black"))

#word cloud
genres_wc <- song_df_clean %>% 
  group_by(genres) %>%  
  summarise(genre_mins = round(sum(msPlayed/60000, na.rm = T),0)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins))


wordcloud2(genres_wc,
           color = rep_len(wordcloud_palette, nrow(genres_wc)),
           backgroundColor = "black",
           fontFamily = plotfont,
           maxRotation = pi/2,
           minRotation = pi/2,
           rotateRatio = 0.25,
           minSize = 1)

 
#3. Artists Stats -----
#3a. Artists by month
artist_month <- song_df_clean %>% 
  group_by(month = floor_date(date, "month"), artist) %>% 
  summarise(artist_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(artist_mins == max(artist_mins))

#3b. Artists Overall
top_artists <- song_df_clean %>% 
  group_by(artist) %>% 
  summarise(artist_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(artist_mins)) %>% 
  head(20) %>% pull(artist)

song_df_clean %>% 
  group_by(artist) %>% 
  summarise(artist_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(artist_mins)) %>% 
  head(15) %>% 
  ggplot(aes(x = artist, y = artist_mins, fill = artist_mins)) +
  geom_col() + 
  scale_x_discrete(limits = rev(top_artists[1:15])) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_gradientn(colours = wordcloud_palette, guide = "none") + 
  coord_flip() + 
  labs(x = "", y = "Minutes") + 
  theme_bw(base_family = plotfont, base_size = 14) + 
    theme(plot.background = element_rect(colour = "black", fill = "black"),
          panel.background = element_rect(colour = "#a6a6a6", fill = "#a6a6a6"),
          axis.text = element_text(colour = wordcloud_palette[2]),
          panel.grid.major.y = element_blank())


#4. albums per period -----
song_df_clean %>% 
  group_by(month, artist, album) %>% 
  summarise(album_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(album_mins == max(album_mins))

#5. songs per period -----
track_month <- song_df_clean %>% 
  filter(!is.na(trackName)) %>% 
  group_by(month = floor_date(date, "month"), artist, trackName) %>% 
  summarise(track_plays = n()) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(track_plays == max(track_plays))
  slice_head(n = 1)

song_df_clean %>% 
  count(trackName) %>% 
  arrange(desc(n))
  
  
  
  
#6. Audio Features-----
song_df_clean %>% 
  summarise(valence = mean(valence, na.rm = T),
            energy = mean(energy, na.rm = T),
            danceability = mean(danceability, na.rm = T),
            instrumentalness = mean(instrumentalness, na.rm = T),
            speechiness = mean(speechiness, na.rm = T)) %>% 
  pivot_longer(cols = c(1:5)) %>% 
  ggplot(aes(x = name, y = value)) + 
  geom_col() + 
  ylim(c(0,1))


song_df_clean %>% 
  group_by(month = floor_date(date, "month")) %>% 
  summarise(audio_features = mean(energy, na.rm = T) + mean(valence,na.rm = T) + mean(danceability, na.rm = T)) %>% # + 
  left_join(track_month, by = "month") %>% 
  ggplot(aes(x = month, y = audio_features)) + 
  geom_smooth(se = FALSE, colour = wordcloud_palette[10],
              method = 'loess', span = 0.5) + 
  labs(x = '',y = '') + 
  geom_label_repel(aes(label = stringr::str_wrap(paste(artist,"-",trackName),25)),
            family = plotfont,
             size = 3,
             min.segment.length = 1) +
  geom_text(data = genres_spotify_time, 
                  aes(x = month, y = 1.4, label = genres_spotify_time$genres_spotify),
                  angle = 90,
                  size = 3,
                  family = plotfont) + 
  theme_solarized(base_family = plotfont) + 
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month") + 
  theme(panel.grid.major.x = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())
