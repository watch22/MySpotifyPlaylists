#My year in music: Spotify Listening History
#load packages ----
# devtools::install_github("lchiffon/wordcloud2")
# install.packages("paletteer")
# install.packages("cartography")
# devtools::install_github("EmilHvitfeldt/paletteer")

pacman::p_load(tidyverse, lubridate, knitr, ggjoy, genius, tidytext, kableExtra, extrafont, ggthemes, ggrepel, magick, webshot, jsonlite, httr, zoo, plotly, wordcloud, RColorBrewer, cowplot, wordcloud2, paletteer, ggimage)

#aesthetics ----
plotfont <- "Open Sans Condensed"
spotify_green <- "#1DB954"
# wordcloud_palette <- c("#ffffff","#e9fcef","#d3f8e0","#bdf5d0","#a7f1c1","#91eeb1","#7beaa2","#65e792","#4fe383","#38e073","#22dd64","#1fc75a","#1db954","#1cb050","#189a46")
wordcloud_palette <- c("#ffffff","#f0f4f2","#eef7f1","#ebf9f0","#e9fcef","#d3f8e0","#bdf5d0","#a6f2bf","#a7f1c1","#90eeb0","#91eeb1","#7beaa2","#65e792","#4ee480","#4fe383","#37e170","#38e073","#21de60","#22dd64","#1ec856","#1fc75a","#1db954","#1cb050","#189a46","#15843a","#116f30")



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
  #filter(!(is.na(msPlayed))) %>% 
  group_by(date, wkday, month = floor_date(date, "month")) %>% 
  summarise(Listening_Time = signif(sum(msPlayed, na.rm = T)/60000,2)) %>% 
  #mutate(Listening_Time = zoo::na.fill(Listening_Time,0)) %>% 
  ggplot(aes(x = month, y = fct_rev(wkday), fill = Listening_Time, 
             text = paste("Avg listening time", round(Listening_Time,0),"mins"))) +
  geom_tile(linejoin = "round")+
  scale_x_date(date_labels = "%b '%y",
               date_breaks = "month",
               expand = c(0,0)) + 
  labs(x = '', y = '') + 
  #scale_fill_paletteer_c("ggthemes::Classic Area Green", name = "Listening Time (mins)") +
  scale_fill_gradientn(colours = wordcloud_palette[c(5:length(wordcloud_palette))], name = "Listening Time (mins)", na.value = "black") + 
  theme_minimal(base_family = plotfont) +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12, colour = wordcloud_palette[2]),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12, colour = wordcloud_palette[2]),
        panel.grid = element_blank(),
        aspect.ratio = (0.25),
        plot.background = element_rect(fill = "black"),
        legend.position = "bottom",
        plot.margin = margin(10,10,10,10),
        title = element_text(size = 15, colour = wordcloud_palette[2])) + 
  labs(title= "listening time (weekly)")

listen_heat_weekdays

#1b. months ----
listen_time_months <- song_df_clean %>% 
  filter(!(is.na(msPlayed))) %>% 
  group_by(month = floor_date(date, "month")) %>% 
  summarise(Listening_Time = signif(sum(msPlayed)/60000,2))

  
listen_heat_months <- ggplot(data = listen_time_months, aes(x = month, y = 100, fill = Listening_Time, label = Listening_Time)) + 
  geom_tile(linejoin = "round") + 
  #geom_text(family = plotfont, angle = 90, colour = wordcloud_palette[1], size = 6) + 
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_paletteer_c("ggthemes::Classic Area Green", name = "Listening Time (mins)") +
  scale_fill_gradientn(colours = wordcloud_palette, name = "Listening Time (mins)") + 
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
p_legend <- plot_grid(p ,heatmap_legend,ncol = 1, rel_heights = c(1, .15))
p_legend
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
spotify_genre_col <- song_df_clean %>% 
  filter(!is.na(genres_spotify)) %>% 
  group_by(genres_spotify) %>%  
  summarise(genre_mins = round(sum(msPlayed/60000, na.rm = T),0)) %>% 
  ungroup() %>% 
  mutate(colourtext = case_when(genre_mins > 1000 ~ wordcloud_palette[2], 
                                TRUE ~ wordcloud_palette[20])) %>% 
  arrange(desc(genre_mins)) %>% 
  head(15) %>% 
  ggplot(aes(x = genres_spotify, y = genre_mins, fill = genre_mins, label = genre_mins)) +
  geom_col() + 
  geom_text(aes(x = genres_spotify, y = case_when(genre_mins > 1000 ~ genre_mins - 120,
                                          TRUE ~ genre_mins - 100),
                colour = colourtext),
            family = plotfont, 
            fontface = "bold.italic",
            size = 5) +
  scale_x_discrete(limits = rev(top_genres_spotify[1:15])) +
  coord_flip() + 
  labs(x = "", 
       y = "minutes") + 
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_paletteer_c("ggthemes::Classic Area Green", guide = "none") +
  scale_fill_gradientn(colours = wordcloud_palette, guide = "none")+
  scale_color_identity() + 
  theme_minimal(base_family = plotfont) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, colour = wordcloud_palette[2]),
        axis.text.y = element_text(size = 15, colour = wordcloud_palette[2]),
        aspect.ratio = c(0.75),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        plot.margin = margin(10,10,10,10),
        title = element_text(size = 15, colour = wordcloud_palette[2])) + 
  labs(title= "listening time per genre (all)")

#word cloud (spotify genres) -----
genres_spotify_wc <- song_df_clean %>% 
  filter(!is.na(genres_spotify)) %>% 
  group_by(genres_spotify) %>%  
  summarise(genre_mins = round(sum(msPlayed/60000, na.rm = T),0)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins))

wordcloud2(genres_spotify_wc,
           # color = paletteer_c("ggthemes::Classic Green", n = 50),
           color = rep_len(wordcloud_palette,nrow(genres_spotify_wc)),
           backgroundColor = "black",
           fontFamily = plotfont,
           minSize = 5,
           size = 1.5,
           maxRotation = pi/2,
           minRotation = pi/2,
           rotateRatio = 0.15)


#grouped genres ----
top_genres <- song_df_clean %>% 
  group_by(genres) %>% 
  summarise(genre_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins)) %>% 
  head(20) %>% pull(genres)

grouped_genre_col <- song_df_clean %>% 
  group_by(genres) %>% 
  summarise(genre_mins = round(sum(msPlayed/60000, na.rm = T),0)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins)) %>% 
  mutate(colourtext = case_when(genre_mins > 1000 ~ wordcloud_palette[2], 
                                TRUE ~ wordcloud_palette[20])) %>% 
  head(15) %>% 
  ggplot(aes(x = genres, y = genre_mins, fill = genre_mins, label = genre_mins)) +
  geom_col() + 
  geom_text(aes(x = genres, y = case_when(genre_mins > 1000 ~ genre_mins - 120,
                                          TRUE ~ genre_mins - 100),
                colour = colourtext),
            family = plotfont, 
            fontface = "bold.italic",
            size = 5) +
  scale_x_discrete(limits = rev(top_genres[1:15])) + 
  coord_flip() + 
  labs(x = "", 
       y = "minutes") + 
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_paletteer_c("ggthemes::Classic Area Green", guide = "none") +
  scale_fill_gradientn(colours = wordcloud_palette, guide = "none")+
  scale_color_identity() + 
  theme_minimal(base_family = plotfont) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, colour = wordcloud_palette[2]),
        axis.text.y = element_text(size = 15, colour = wordcloud_palette[2]),
        aspect.ratio = c(0.75),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        plot.margin = margin(10,10,10,10),
        title = element_text(size = 15, colour = wordcloud_palette[2])) + 
  labs(title= "listening time per genre (grouped)")

grouped_genre_col
#combine plots
genre_plots <- plot_grid(spotify_genre_col,
                         grouped_genre_col,
               align = "v", ncol = 1)
genre_plots


#word cloud (grouped) ----
genres_wc <- song_df_clean %>% 
  group_by(genres) %>%  
  summarise(genre_mins = round(sum(msPlayed/60000, na.rm = T),0)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins))


wordcloud2(genres_wc,
           # color = paletteer_c("ggthemes::Classic Green", n = 50),
           color = rep_len(wordcloud_palette,nrow(genres_wc)),
           backgroundColor = "black",
           fontFamily = plotfont,
           minSize = 7.5,
           size = 1.5,
           maxRotation = pi/2,
           minRotation = pi/2,
           rotateRatio = 0.15)

 
#3. Artists Stats -----
#3a. Artists by month
artist_month <- song_df_clean %>% 
  group_by(month = floor_date(date, "month"), artist) %>% 
  summarise(artist_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(artist_mins == max(artist_mins))

song_df_clean %>% 
  group_by(month = floor_date(date, "month"), artist) %>% 
  summarise(artist_plays = n()) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(artist_plays == max(artist_plays))


artist_month <- artist_month %>% ungroup() %>% mutate(artist_photo_url = NA)

artist_month <- song_df %>% 
select(artist, artist_uri) %>% 
  filter(artist %in% unique(artist_month$artist)) %>% unique() %>% head(8) %>% 
  left_join(artist_month, by = "artist")

#get spotify artist images
for(j in 1:nrow(artist_month)){
  artist_url <- paste0('https://api.spotify.com/v1/artists/',artist_month$artist_uri[j])
  artist_response <- GET(url = artist_url, add_headers(Authorization = HeaderValue))
  artist_content <- content(artist_response)
  artist_month$artist_photo_url[j] <- artist_content$images[[1]]$url
}

artist_month <- artist_month %>% arrange(month)


ggplot(artist_month) +
  geom_image(aes(y = 1, image = artist_photo_url, x = month), size = 0.1, asp = 1.5)

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
#by month ----
album_month <- song_df_clean %>% 
  group_by(month = floor_date(date, "month"), artist, album, label) %>% 
  summarise(album_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(album_mins == max(album_mins))

album_month_artwork <- song_df %>% 
  select(artist, album, album_uri) %>% 
  filter(album %in% unique(album_month$album) & artist %in% album_month$artist) %>% unique() %>% 
  left_join(album_month, by = c("artist","album")) %>% 
  arrange(month) %>% 
  mutate(album_art = NA) %>% 
  slice(1:5,7:11,13:14)

album_month_artwork

for(j in 1:nrow(album_month_artwork)){
  album_url <- paste0('https://api.spotify.com/v1/albums/',album_month_artwork$album_uri[j])
  album_response <- GET(url = album_url, add_headers(Authorization = HeaderValue))
  album_content <- content(album_response)
  album_month_artwork$album_art[j] <- album_content$images[[1]]$url
}

#top albums table ----
album_table_headers <-data.frame(name = c("month","","artist","album","label"),
                    pos = c(0.1,0.75,3,6,10),
                    hjust= c(0,0,0,0,0))

album_month_artwork <- album_month_artwork %>% 
  mutate(index = (13 - row_number()) * 2)

album_month_plot <- ggplot(album_month_artwork)+
  annotate(geom = "rect", xmin = -Inf, xmax = Inf , ymin = 23 , ymax = 25, alpha = 0.75, fill = spotify_green) + 
  geom_image(aes(y = index, image = album_art, x = 2), size=0.05, asp = 1.5) +
  geom_text(data = album_table_headers, aes(label = name, x = pos, y = 26), hjust = 0, fontface = "bold", family = plotfont, size = 8, color=wordcloud_palette[2]) + 
  geom_text(aes(y = index, x = album_table_headers$pos[1] , label = format(month, "%b '%y")), hjust=0, family = plotfont, size = 8, color=wordcloud_palette[2]) + 
  geom_text(aes(y = index, x = album_table_headers$pos[3]  , label = str_wrap(artist,25)), hjust=0, family = plotfont, size = 8, color=wordcloud_palette[2]) + 
  geom_text(aes(y = index, x = album_table_headers$pos[4]  , label = str_wrap(album,25)), hjust=0, family = plotfont, size = 8, color=wordcloud_palette[2]) +
  geom_text(aes(y = index, x = album_table_headers$pos[5]  , label = label), hjust=0, family = plotfont, size = 8, color=wordcloud_palette[2]) + 
  scale_y_continuous(breaks = seq(1:26),
                     labels = seq(1:26))+
  geom_hline(data = track_plot_lines, mapping = aes(yintercept=lines), color=wordcloud_palette[2], size=0.3) + 
  scale_x_continuous(limits = c(0,14),
                     expand = c(0,0))+
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        title = element_text(size = 35, colour = wordcloud_palette[2], family = plotfont),
        plot.margin = margin(10,20,10,10),
        plot.caption = element_text(size = 24, colour = wordcloud_palette[2], family = plotfont)
  ) + 
  labs(title = "top albums by month",
       caption = "albums ranked by minutes played\nall data sourced from spotify // visuals by watch22")

album_month_plot

ggsave(plot = album_month_plot,
       filename = here::here("outputs","album_month_plot.png"),
       scale = 4, width = 8, height = 10, units = "cm")

#5. songs per period -----
track_month <- song_df_clean %>% 
  filter(!is.na(trackName)) %>% 
  group_by(month = floor_date(date, "month"), artist, trackName, album) %>% 
  summarise(track_plays = n()) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(track_plays == max(track_plays))

track_month
  
track_month <- track_month %>% 
  filter(trackName %notin% c("shutters", "Antidawn", "Pluck", "Antifate", "American Crucifixion", "Inter-City 125","Pogo","Dust" )) %>% 
  filter(album %notin% c("Maida Vale Sessions", "Have You Felt Lately?"))

#get album artwork
track_month_artwork <- song_df %>% 
  select(artist, album, album_uri) %>% 
  filter(album %in% unique(track_month$album) & artist %in% track_month$artist) %>% unique() %>% 
  left_join(track_month, by = c("artist","album")) %>% 
  arrange(month) %>% 
  slice(c(1:5,7:11,13,14)) %>% 
  mutate(album_art = NA)


for(j in 1:nrow(track_month_artwork)){
  album_url <- paste0('https://api.spotify.com/v1/albums/',track_month_artwork$album_uri[j])
  album_response <- GET(url = album_url, add_headers(Authorization = HeaderValue))
  album_content <- content(album_response)
  track_month_artwork$album_art[j] <- album_content$images[[1]]$url
}


#top tracks table ----
track_table_headers <- data.frame(name = c("month","","artist","track","album"),
                    pos = c(0.1,0.75,3,6,9),
                    hjust= c(0,0,0,0,0))

track_month_artwork <- track_month_artwork %>% 
  mutate(index = (13 - row_number()) * 2)

track_plot_lines <- data.frame(lines = seq(1,25, by = 2))


track_month_plot <- ggplot(track_month_artwork)+
  annotate(geom = "rect", xmin = -Inf, xmax = Inf , ymin = 23 , ymax = 25, alpha = 0.75, fill = spotify_green) + 
geom_image(aes(y = index, image = album_art, x = 2), size=0.05, asp = 1.5) +
  geom_text(data = track_table_headers, aes(label = name, x = pos, y = 26), hjust = 0, fontface = "bold", family = plotfont, size = 8, color=wordcloud_palette[2]) + 
  geom_text(aes(y = index, x = track_table_headers$pos[1] , label = format(month, "%b '%y")), hjust=0, family = plotfont, size = 8, color=wordcloud_palette[2]) + 
  geom_text(aes(y = index, x = track_table_headers$pos[3]  , label = str_wrap(artist,25)), hjust=0, family = plotfont, size = 8, color=wordcloud_palette[2]) + 
  geom_text(aes(y = index, x = track_table_headers$pos[4]  , label = str_wrap(trackName,25)), hjust=0, family = plotfont, size = 8, color=wordcloud_palette[2]) +
  geom_text(aes(y = index, x = track_table_headers$pos[5]  , label = album), hjust=0, family = plotfont, size = 8, color=wordcloud_palette[2]) + 
  scale_y_continuous(breaks = seq(1:26),
                     labels = seq(1:26))+
  geom_hline(data = track_plot_lines, mapping = aes(yintercept=lines), color=wordcloud_palette[2], size=0.3) + 
  scale_x_continuous(limits = c(0,14),
                     expand = c(0,0))+
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        title = element_text(size = 35, colour = wordcloud_palette[2], family = plotfont),
        plot.margin = margin(10,20,10,10),
        plot.caption = element_text(size = 24, colour = wordcloud_palette[2], family = plotfont)
  ) +
  labs(title = "top tracks by month",
       caption = "tracks ranked by number of plays\nall data sourced from spotify // visuals by watch22")
  
track_month_plot

ggsave(plot = track_month_plot,
       filename = here::here("outputs","track_month_plot.png"),
       scale = 4, width = 8, height = 10, units = "cm")


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
  # geom_label_repel(aes(label = stringr::str_wrap(paste(artist,"-",trackName),25)),
  #           family = plotfont,
  #            size = 3,
  #            min.segment.length = 1) +
  # geom_text(data = genres_spotify_time, 
  #                 aes(x = month, y = 1.4, label = genres_spotify_time$genres_spotify),
  #                 angle = 90,
  #                 size = 3,
  #                 family = plotfont) + 
  theme_solarized(base_family = plotfont) + 
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month") + 
  theme(panel.grid.major.x = element_line(colour = "lightgrey"),
        panel.grid.minor = element_blank())



#audio features area plots ----
#create index for plot
x <- data.frame(sequence=c(12:1)*2, times = 3)
plot_index <- data.frame(lapply(x, rep, x$times)) %>% pull(sequence)
horizontal_plot_lines <- data.frame(lines = c(1,(seq(1,12, by = 1)*2)+1))

plot_tracks <- track_month_artwork %>% 
  select(artist,trackName) %>% 
  mutate(top_tracks = paste(artist,"-",trackName),
         index = seq(24,2,-2))
  
features_sep_tracks <- song_df_clean %>% 
  filter(!(is.na(valence))) %>% 
  group_by(month = floor_date(date, "month")) %>% 
  summarise(valence = mean(valence),
            energy = mean(energy),
            danceability = mean(danceability)) %>% 
  select(month, valence, energy, danceability) %>% 
  pivot_longer(cols = -c(month)) %>% 
  cbind(plot_index) %>% 
  arrange(month) %>% 
  mutate(name = factor(name, levels = c("valence", "danceability", "energy"))) %>% 
  ggplot(aes(x = plot_index, y = value)) + 
  geom_vline(data = horizontal_plot_lines, mapping = aes(xintercept = lines), color=wordcloud_palette[2], size=0.3) +
  # annotate("rect", xmin = 2, xmax = 24, ymin = 0.1, ymax = 0.15, fill = "black", colour = "black") + 
  geom_smooth(aes(colour = name),se = F, span = 0.25) +
  geom_text(aes(x = plot_index, y = 0.125 - 0.4, label = format(month,"%b '%y")), colour = wordcloud_palette[2], size = 6, family = plotfont, fontface = "plain", hjust = 0) + 
  annotate(geom = "text", x = 26, y = 0.125 - 0.4, label = "month", hjust = 0, size = 6, family = plotfont, colour = wordcloud_palette[12]) + 
  geom_text(data = plot_tracks, aes(x = plot_index, y = 0.65 - 0.75, label = top_tracks), colour = wordcloud_palette[2], size = 6, family = plotfont, fontface = "plain", hjust = 0) + 
  annotate(geom = "text", x = 26, y = 0.65 - 0.75, label = "most played track", hjust = 0, size = 6, family = plotfont, colour = wordcloud_palette[12]) + 
  scale_y_continuous(limits = c(0.1 - 0.4,0.7),
                     breaks = seq(0.2,0.6,0.1),
                     labels = seq(0.2,0.6,0.1),
                     expand = c(0,0)) + 
  scale_colour_manual(values = c(wordcloud_palette[8],wordcloud_palette[16],wordcloud_palette[24])) + 
coord_flip() +
  theme_minimal(base_family = plotfont) + 
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = wordcloud_palette[12], linetype = "dotted"),
        axis.text.x = element_text(size = 15, colour = wordcloud_palette[2]),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        legend.direction = "horizontal",
        legend.justification = "right",
        legend.key.size = unit(1, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, colour = wordcloud_palette[2]),
        legend.position = "top",
        plot.margin = margin(10,10,10,10),
        title = element_text(size = 24, colour = wordcloud_palette[2]))+
  labs(title = "audio feature summary")

features_sep


#audio features sum plots (DISCARD) ----
features_sum <- song_df_clean %>% 
  filter(!(is.na(valence))) %>% 
  group_by(month = floor_date(date,"month")) %>% 
  summarise(valence = mean(valence),
            energy = mean(energy),
            danceability = mean(danceability)) %>% 
  select(month, valence, energy, danceability) %>% 
  mutate(value = valence + energy + danceability) %>% 
  select(month,value) %>% 
  arrange(month) %>% 
  mutate(plot_index = seq(24,2,-2)) %>% 
  left_join(track_month_artwork %>% select(month, artist, trackName), by = "month") %>% 
  mutate(top_track = paste(artist,"-",trackName)) %>% 
  ggplot(aes(x = plot_index, y = value)) + 
  geom_vline(data = horizontal_plot_lines, mapping = aes(xintercept = lines), color=wordcloud_palette[2], size=0.3) +
  geom_smooth(colour = wordcloud_palette[12],se = F, span = 0.25) +
  geom_text(aes(x = plot_index, y = 0.905, label = top_track), colour = wordcloud_palette[2], size = 5, family = plotfont, fontface = "plain", hjust = 0) + 
  scale_y_continuous(limits = c(0.9,1.4),
                     breaks = seq(1,1.4,0.1),
                     labels = seq(1,1.4,0.1),
                     expand = c(0,0)) +
  coord_flip() +
  theme_minimal(base_family = plotfont) + 
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = wordcloud_palette[12], linetype = "dotted"),
        axis.text.x = element_text(size = 12, colour = wordcloud_palette[2]),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.key.size = unit(0.75, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = wordcloud_palette[2]),
        legend.position = "top",
        plot.margin = margin(10,10,10,0),
        title = element_text(size = 15, colour = wordcloud_palette[2]))+
  labs(title = "feature sum")

features_sum

plot_grid(features_sep + theme(legend.position = "none"), 
          features_sum, 
          nrow = 1, align = "v")


#RIDGE PLOTS (DISCARD) ------
#top tracks features ---
year_top_20_tracks <- song_df_clean %>% 
  filter(!is.na(artist)) %>% 
  group_by(artist, trackName, album) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate(concat = paste0(artist,trackName,album)) %>% 
  filter(album != "The Noise Made By People") %>% 
  head(20)
  
#top 20 albums
  year_top_20_albums <- song_df_clean %>% 
    filter(!is.na(artist)) %>% 
    group_by(artist, album) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    mutate(artist_album = paste(artist,"-",album)) %>% 
    head(20)

top_20_albums <- year_top_20_albums %>% pull(artist_album) %>% as.factor()

top_20_albums <- factor(top_20_albums,levels(top_20_albums)[c(14,2,20,18,16,9,15,17,7,5,19,4,1,6,12,8,10,13,3,11)])

song_df_clean %>% 
  mutate(artist_album = paste(artist,"-",album)) %>% 
  filter(artist_album %in% top_20_albums) %>% 
  arrange(artist,trackName) %>% 
    unique() %>% 
  ggplot(aes(x = valence, y = factor(artist_album, rev(levels(top_20_albums))))) +
  geom_density_ridges(aes(fill = artist_album), colour = "white") + 
  scale_fill_manual(values = wordcloud_palette[6:36], guide = "none") + 
  scale_x_continuous(expand = c(0,0)) + 
  labs(x = "", y = "",
       title = "20 most listened to albums",
       subtitle = "valence") + 
  theme_minimal(base_family = plotfont) + 
  theme(axis.title = element_blank(),
        #axis.text.y = element_blank(),
        axis.text = element_text(size = 12, colour = wordcloud_palette[2]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.key.size = unit(0.75, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = wordcloud_palette[2]),
        legend.position = "top",
        plot.margin = margin(10,10,10,10),
        title = element_text(size = 15, colour = wordcloud_palette[2]))
  
song_df_clean %>% 
  mutate(artist_album = paste(artist,"-",album)) %>% 
  filter(artist_album %in% top_20_albums) %>% 
  arrange(artist,trackName) %>% 
  unique() %>% 
  ggplot(aes(x = danceability, y = factor(artist_album, rev(levels(top_20_albums))))) +
  geom_density_ridges(aes(fill = artist_album), colour = "white") + 
  scale_fill_manual(values = wordcloud_palette[26:6], guide = "none") + 
  scale_x_continuous(expand = c(0,0)) + 
  labs(x = "", y = "",
       title = "20 most listened to albums",
       subtitle = "danceability") + 
  theme_minimal(base_family = plotfont) + 
  theme(axis.title = element_blank(),
        #axis.text.y = element_blank(),
        axis.text = element_text(size = 12, colour = wordcloud_palette[2]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.key.size = unit(0.75, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = wordcloud_palette[2]),
        legend.position = "top",
        plot.margin = margin(10,10,10,10),
        title = element_text(size = 15, colour = wordcloud_palette[2]))

song_df_clean %>% 
  mutate(artist_album = paste(artist,"-",album)) %>% 
  filter(artist_album %in% top_20_albums) %>% 
  arrange(artist,trackName) %>% 
  unique() %>% 
  ggplot(aes(x = energy, y = factor(artist_album, rev(levels(top_20_albums))), fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 3) +
  #scale_fill_gradientn(colours = wordcloud_palette, guide = "none") +
  scale_fill_paletteer_c("ggthemes::Classic Red-White-Black", guide = "none", direction = -1) +
  scale_x_continuous(expand = c(0,0)) + 
  labs(x = "", y = "",
       title = "20 most listened to albums",
       subtitle = "energy") + 
  theme_minimal(base_family = plotfont) + 
  theme(axis.title = element_blank(),
        #axis.text.y = element_blank(),
        axis.text = element_text(size = 12, colour = wordcloud_palette[2]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.key.size = unit(0.75, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = wordcloud_palette[2]),
        legend.position = "top",
        plot.margin = margin(10,10,10,10),
        title = element_text(size = 15, colour = wordcloud_palette[2]))

song_df_clean %>% 
  mutate(artist_album = paste(artist,"-",album)) %>% 
  filter(artist_album %in% top_20_albums) %>% 
  arrange(artist,trackName) %>% 
  unique() %>% 
  ggplot(aes(x = energy + danceability + valence, y = factor(artist_album, rev(levels(top_20_albums))))) +
  geom_density_ridges(aes(fill = artist_album), colour = "white") + 
  scale_fill_manual(values = wordcloud_palette[26:6], guide = "none") + 
  scale_x_continuous(expand = c(0,0)) + 
  labs(x = "", y = "",
       title = "20 most listened to albums",
       subtitle = "energy") + 
  theme_minimal(base_family = plotfont) + 
  theme(axis.title = element_blank(),
        #axis.text.y = element_blank(),
        axis.text = element_text(size = 12, colour = wordcloud_palette[2]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.key.size = unit(0.75, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = wordcloud_palette[2]),
        legend.position = "top",
        plot.margin = margin(10,10,10,10),
        title = element_text(size = 15, colour = wordcloud_palette[2]))


