#My Year in Music: Spotify Listening History ------
pacman::p_load(tidyverse, lubridate, knitr, ggjoy, genius, tidytext, kableExtra, extrafont, ggthemes, ggrepel, magick, webshot, jsonlite, httr, zoo, plotly, wordcloud, RColorBrewer, cowplot, wordcloud2, paletteer, ggimage, patchwork)

#Setup Spotify Authentication ----
clientID = 'aaf0705a9d774da582794d3c0922a6ca'
secret = '9aa26a13b2654325844b33cb7573c901'
response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)
mytoken = content(response)$access_token
HeaderValue = paste0('Bearer ', mytoken)

#read files ----

library <- read_json(here::here("data","YourLibrary.json"))
library_music <- library[[1]]
library_pods <- library[[3]]
library_music <- data.frame(do.call(rbind,library_music), stringsAsFactors = F)
library_pods <- data.frame(do.call(rbind,library_pods), stringsAsFactors = F)
pods <- library_pods$name %>% unlist()
stream_history <- fromJSON(here::here("data","StreamingHistory0.json")) %>% as.data.frame()
playlists <- read_json(here::here("data", "Playlist1.json"))

#remove podcasts and article artists
stream_music <- stream_history %>% 
  filter(artistName %notin% pods & artistName %notin% c("Metallica","The Luke and Pete Show","We Have Ways of Making You Talk","Serial","S-Town","The Rest is History","1619","Veleno"))

stream_music <- stream_music %>% 
  mutate(endTime = as.Date(endTime),
         unique = paste0(artistName,":",trackName))

library_music <- library_music %>% 
  mutate(unique = paste0(artist,":",track),
         track_uri = sub("spotify:track:","",uri))

mi_tracks <- left_join(stream_music,library_music,by = "unique")
#Get missing track Uri's -----
missing <- mi_tracks %>% filter(is.na(track_uri))
missing <- missing %>% 
  mutate(track_search = paste0(str_replace_all(trackName," ","%20"),"%20"),
         artist_search = paste0(str_replace_all(artistName," ","%20"),"%20"))

for(i in 1:nrow(missing)){
  Sys.sleep(0.10)
  search_url = paste0("https://api.spotify.com/v1/search?q=track:",missing$track_search[i],"artist:",missing$artist_search[i],"&type=track")
  search_response = GET(url = search_url, add_headers(Authorization = HeaderValue))
  search_content = content(search_response)
  missing$track_uri[i] <- ifelse(length(search_content) > 0, ifelse("tracks" %in% names(search_content) & length(search_content$tracks$items) > 0,search_content$tracks$items[[1]]$id,NA),NA)
}

#Combine missing values back with library_music ----
missing_filter <- missing %>% 
  filter(!is.na(track_uri)) %>% 
  select(-c(23:24))
mi_tracks_filter <- mi_tracks %>% 
  filter(!is.na(track_uri))

#combine all iterations to get file with all track Uri's ----
song_df <- rbind(mi_tracks_filter,missing_filter,missing_na_filter)
song_df$album <- as.character(song_df$album)

#Get genres. missing album names, artist uri ----
for(j in 1:nrow(song_df)){
  temp_url <- paste0('https://api.spotify.com/v1/tracks/',   
                     song_df$track_uri[j])
  temp_response <- GET(url = temp_url, add_headers(Authorization = HeaderValue))
  temp_content <- content(temp_response)
  song_df_full$album[j] <- temp_content$album$name
  song_df$album_uri[j] <- temp_content$album$id
  song_df$artist_uri[j] <- temp_content$artists[[1]]$id
  artist_url <- paste0('https://api.spotify.com/v1/artists/',song_df$artist_uri[j])
  artist_response <- GET(url = artist_url, add_headers(Authorization = HeaderValue))
  artist_content <- content(artist_response)
  song_df$genres[j] <- ifelse(length(artist_content$genres)==0,NA,artist_content$genres[[1]])
  album_url <- paste0('https://api.spotify.com/v1/albums/',song_df$album_uri[j])
  album_response <- GET(url = album_url, add_headers(Authorization = HeaderValue))
  album_content <- content(album_response)
  song_df$label[j] <- album_content$label
}


# Get Audio Features ----
for(i in 1:nrow(song_df_full)){
  Sys.sleep(0.10)
  track_URI2 = paste0('https://api.spotify.com/v1/audio-features/',   
                      song_df_full$track_uri[i])
  track_response2 = GET(url = track_URI2, 
                        add_headers(Authorization = HeaderValue))
  tracks2 = content(track_response2)
  song_df_full$key[i] <- tracks2$key
  song_df_full$mode[i] <- tracks2$mode
  song_df_full$time_signature[i] <- tracks2$time_signature
  song_df_full$acousticness[i] <- tracks2$acousticness
  song_df_full$danceability[i] <- tracks2$danceability
  song_df_full$energy[i] <- tracks2$energy
  song_df_full$instrumentalness[i] <- tracks2$instrumentalness
  song_df_full$liveliness[i] <- tracks2$liveness
  song_df_full$loudness[i] <- tracks2$loudness
  song_df_full$speechiness[i] <- tracks2$speechiness
  song_df_full$valence[i] <- tracks2$valence
  song_df_full$tempo[i] <- tracks2$tempo
}



song_df <- song_df %>% 
  select(artist,trackName,album,msPlayed,label,genres,8:19,artist_uri,album_uri,track_uri,date)

#Write CV to keep as back up and import into google sheets to fix genres
write_csv(song_df,file = here::here("data", "spotify_history_2108229_220831_clean_final.csv"))



#Import back into R
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

#set aesthetics ----
plotfont <- "Open Sans Condensed"
spotify_green <- "#1DB954"
spotify_pink <- "#F56CBC"
spotify_blue <- "#3B00E3"
# wordcloud_palette <- c("#ffffff","#e9fcef","#d3f8e0","#bdf5d0","#a7f1c1","#91eeb1","#7beaa2","#65e792","#4fe383","#38e073","#22dd64","#1fc75a","#1db954","#1cb050","#189a46")
wordcloud_palette <- c("#ffffff","#f0f4f2","#eef7f1","#ebf9f0","#e9fcef","#d3f8e0","#bdf5d0","#a6f2bf","#a7f1c1","#90eeb0","#91eeb1","#7beaa2","#65e792","#4ee480","#4fe383","#37e170","#38e073","#21de60","#22dd64","#1ec856","#1fc75a","#1db954","#1cb050","#189a46","#15843a","#116f30")
wordcloud_palette_pink <- c("#ffffff","#fde7f4","#fccfe9","#fab7de","#f8a0d3","#f788c8","#f570be","#f56cbc","#f358b3","#f240a8", "#f0289d", "#ee1192")
wordcloud_palette_blue <- c("#ffffff", "#e9fcf8", "#d3f8f0", "#bdf5e9", "#a7f1e1", "#90eeda", "#7bead2", "#65e7cb", "#4fe3c3", "#38e0bc", "#22ddb4", "#1fc7a2")

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
        legend.position = "none",
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
        legend.justification = "center",
        legend.key.size = unit(0.75, "cm"),
        legend.title = element_text(size = 12, colour = wordcloud_palette[2]),
        legend.title.align = 0.5,
        legend.position = "bottom",
        plot.margin = margin(10,10,0,10),
        title = element_text(size = 15, colour = wordcloud_palette[2])) + 
  labs(title= "listening time (monthly)")

listen_heat_months

#combine plots
listen_heat_comb <- listen_heat_weekdays / listen_heat_months

ggsave(plot = listen_heat_comb,
       filename = here::here("outputs","tlisten_time.png"),
       scale = 2, width = 8, height = 8, units = "cm")

listen_heat_comb

#2. Top Tracks by Month -----
track_month <- song_df_clean %>% 
  filter(!is.na(trackName)) %>% 
  group_by(month = floor_date(date, "month"), artist, trackName, album) %>% 
  summarise(track_plays = n()) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(track_plays == max(track_plays))

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

#plot table with artwork
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
       caption = "tracks ranked by number of plays\nall data sourced from spotify // visuals and analysis by watch22")

track_month_plot

ggsave(plot = track_month_plot,
       filename = here::here("outputs","track_month_plot.png"),
       scale = 3.5, width = 10, height = 10, units = "cm")

#3. Top Albums by Month ----
album_month <- song_df_clean %>% 
  group_by(month = floor_date(date, "month"), artist, album, label) %>% 
  summarise(album_mins = sum(msPlayed/60000, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  filter(album_mins == max(album_mins))

#get album artwork
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

#plot table
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
       caption = "albums ranked by playtime\nall data sourced from spotify // visuals and analysis by watch22")

album_month_plot

ggsave(plot = album_month_plot,
       filename = here::here("outputs","album_month_plot.png"),
       scale = 4, width = 10, height = 8, units = "cm")

#4. Tracks vs Audio Features ----
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
        title = element_text(size = 24, colour = wordcloud_palette[2]),
        plot.caption = element_text(family = plotfont, size = 16, colour = wordcloud_palette[2])) +
  labs(title = "audio feature summary",
       caption = "valence reflects mood, higher values indicate more positive feelings\nenergy reflects intensity & activity\nall data sourced from spotify // visuals and analysis by watch22")

features_sep_tracks

ggsave(plot = features_sep_tracks,
       filename = here::here("outputs","features_by_tracks_small.png"),
       scale = 3.5, width = 8, height = 8, units = "cm")

#5. Top 20 Albums vs. Audio Features -----
#5a. Valence ----
top20_valence <- song_df_clean %>% 
  mutate(artist_album = paste(artist,"-",album)) %>% 
  filter(artist_album %in% top_20_albums) %>% 
  arrange(artist,trackName) %>% 
  unique() %>% 
  ggplot(aes(x = valence, y = factor(artist_album, rev(levels(top_20_albums))), fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 2, colour = wordcloud_palette[26]) +
  scale_fill_gradientn(colours = wordcloud_palette, guide = "none") +
  scale_x_continuous(expand = c(0,0)) + 
  labs(x = "", y = "",
       title = "20 most listened to albums",
       subtitle = "valence") + 
  theme_minimal(base_family = plotfont) + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 15, colour = wordcloud_palette[2]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        plot.margin = margin(10,0,10,0),
        title = element_blank(),
        plot.subtitle = element_text(family = plotfont, size = 24, colour = wordcloud_palette[12], hjust = 0.5))

top20_valence

#5b. Danceability ----- 
top20_dance <- song_df_clean %>% 
  mutate(artist_album = paste(artist,"-",album)) %>% 
  filter(artist_album %in% top_20_albums) %>% 
  arrange(artist,trackName) %>% 
  unique() %>% 
  ggplot(aes(x = danceability, y = factor(artist_album, rev(levels(top_20_albums))), fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 1.3, colour = wordcloud_palette_pink[length(wordcloud_palette_pink)]) +
  scale_fill_gradientn(colours = wordcloud_palette_pink, guide = "none") +
  scale_x_continuous(expand = c(0,0)) + 
  labs(x = "", y = "",
       title = "20 most listened to albums",
       subtitle = "danceability") + 
  theme_minimal(base_family = plotfont) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        # axis.text.x = element_text(size = 15, colour = wordcloud_palette[2]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        plot.margin = margin(10,0,10,0),
        title = element_blank(),
        plot.subtitle = element_text(family = plotfont, size = 24, colour = wordcloud_palette_pink[length(wordcloud_palette_pink)], hjust = 0.5))
top20_dance

#5c. Energy -----
top20_energy <- song_df_clean %>% 
  mutate(artist_album = paste(artist,"-",album)) %>% 
  filter(artist_album %in% top_20_albums) %>% 
  arrange(artist,trackName) %>% 
  unique() %>% 
  ggplot(aes(x = energy, y = factor(artist_album, rev(levels(top_20_albums))), fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 2, colour = wordcloud_palette_blue[length(wordcloud_palette_blue)]) +
  scale_fill_gradientn(colours = wordcloud_palette_blue, guide = "none") +
  scale_x_continuous(expand = c(0,0)) + 
  labs(x = "", y = "",
       title = "20 most listened to albums",
       subtitle = "energy") + 
  theme_minimal(base_family = plotfont) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        # axis.text = element_text(size = 15, colour = wordcloud_palette[2]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        plot.margin = margin(10,0,10,0),
        title = element_blank(),
        plot.subtitle = element_text(family = plotfont, size = 24, colour = wordcloud_palette_blue[length(wordcloud_palette_blue)], hjust = 0.5))

top20_energy

top20_feature_plots <- top20_valence + top20_dance + top20_energy +
  plot_layout(nrow = 1, widths = c(1, 1, 1)) &
  plot_annotation(theme = theme(plot.background = element_rect(fill ="black"),
                                plot.caption = element_text(family = plotfont, size = 16, colour = wordcloud_palette[2])),
                  caption = "valence reflects mood, higher values indicate more positive feelings\nenergy reflects intensity & activity\nalbums ranked by playtime\nall data sourced from spotify // visuals and analysis by watch22")

ggsave(plot = top20_feature_plots,
       filename = here::here("outputs","top20_feature_plots_small.png"),
       scale = 4, width = 6, height = 4, units = "cm")

#6. Top 20 Artists ----
top20_artists <- song_df_clean %>% 
  group_by(artist) %>% 
  summarise(artist_mins = round(sum(msPlayed/60000, na.rm = T),0)) %>% 
  ungroup() %>% 
  arrange(desc(artist_mins)) %>% 
  mutate(colourtext = case_when(artist_mins > 1000 ~ wordcloud_palette[2], 
                                TRUE ~ wordcloud_palette[24])) %>%
  head(15) %>% 
  ggplot(aes(x = artist, y = artist_mins, fill = artist_mins, label = artist_mins)) +
  geom_col() + 
  scale_x_discrete(limits = rev(top_artists[1:15])) +
  geom_text(aes(x = artist, y = case_when(artist_mins > 1000 ~ artist_mins - 100,
                                          TRUE ~ artist_mins - 50), colour = colourtext),
            family = plotfont, 
            fontface = "bold.italic",
            size = 5) +
  coord_flip() + 
  labs(x = "", 
       y = "minutes") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colours = wordcloud_palette[6:24], guide = "none")+
  scale_color_identity() + 
  theme_minimal(base_family = plotfont) +
  theme(axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, colour = wordcloud_palette[2]),
        axis.text.y = element_text(size = 15, colour = wordcloud_palette[2]),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black"),
        plot.margin = margin(10,10,10,10),
        plot.title = element_text(size = 24, colour = wordcloud_palette[2], vjust = 1),
        plot.title.position = "plot") +
  labs(title = "top 20 artists",
       caption = "all data sourced from spotify // visuals and analysis by watch22")

top20_artists

ggsave(plot = top20_artists,
       filename = here::here("outputs","top20_artists_small.png"),
       scale = 3, width = 8, height = 8, units = "cm")

#7. Genre Overview & Word clouds ----
#7a. Spotify Genres
genres_spotify_wc <- song_df_clean %>% 
  filter(!is.na(genres_spotify)) %>% 
  group_by(genres_spotify) %>%  
  summarise(genre_mins = round(sum(msPlayed/60000, na.rm = T),0)) %>% 
  ungroup() %>% 
  arrange(desc(genre_mins))

wordcloud2(genres_spotify_wc,
           color = rep_len(wordcloud_palette,nrow(genres_spotify_wc)),
           backgroundColor = "black",
           fontFamily = plotfont,
           minSize = 5,
           size = 1.5,
           maxRotation = pi/2,
           minRotation = pi/2,
           rotateRatio = 0.15)

#7b. Grouped Genres
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
