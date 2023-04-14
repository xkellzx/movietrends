# DSCI304 Final Project Code
###############################################################################
# Read in dataset
setwd("C://Users//Kelly//Downloads//2023Spring//DSCI304//Final Project")
imdb <- read.csv("imdb_movie_data.csv")
head(imdb)
str(imdb)
View(imdb)

# sourced from https://flixpatrol.com/market/box-office-revenues/
library(readxl)
box <- read_excel("markets.xlsx")
head(box)
str(box)
View(box)
###############################################################################

# Clean data

# Backup imdb df
imdb_backup <- imdb
imdb <- imdb_backup

# Delete rows with NA ('-)
remove <- imdb[(imdb$genre.2 == "'-" | imdb$genre.3 == "'-" | imdb$metascore == "'-" | imdb$gross_earning == "'-", ]
imdb <- imdb[!(as.numeric(rownames(imdb)) %in% as.numeric(rownames(remove))), ]

# Remove certificate column
drop <- c("certificate")
imdb <- imdb[ ,!(names(imdb) %in% drop)]

# Change year to numeric (remove parenthesis)
library(stringr)
imdb$year <- str_replace(imdb$year, "\\(I+\\) ", "")
imdb$year <- substr(imdb$year, 2, 5)
imdb$year <- as.numeric(imdb$year)

# Change metascore to numeric
imdb$metascore <- as.numeric(imdb$metascore)

# Change time_minute to numeric (remove min)
imdb$time_minute <- str_replace(imdb$time_minute, " min", "")
imdb$time_minute <- as.numeric(imdb$time_minute)

# Change gross_earning to numeric (remove $, ., M, and #)
imdb$gross_earning <- str_replace(imdb$gross_earning, "#.*", "")
imdb$gross_earning <- str_replace(imdb$gross_earning, "M", "")
imdb$gross_earning <- str_replace(imdb$gross_earning, "\\$", "")
imdb$gross_earning <- as.numeric(imdb$gross_earning)
imdb <- na.omit(imdb)

imdb$genre.1 <- as.factor(imdb$genre.1)

View(imdb)

# Backup box df
box_backup <- box
box <- box_backup

# Remove NA values
box <- na.omit(box)

# Change colnames
colnames(box) <- c("Country", "GDP_per_capita", "Population", "Box_Office_Admission", "Box_Office_Revenues")

View(box)
###############################################################################
# Creating yearly data 

imdb_1925 <- imdb[imdb$year %in% seq(1925,1929,1),]
imdb_1930 <- imdb[imdb$year %in% seq(1930,1934,1),]
imdb_1935 <- imdb[imdb$year %in% seq(1935,1939,1),]
imdb_1940 <- imdb[imdb$year %in% seq(1940,1944,1),]
imdb_1945 <- imdb[imdb$year %in% seq(1945,1949,1),]
imdb_1950 <- imdb[imdb$year %in% seq(1950,1954,1),]
imdb_1955 <- imdb[imdb$year %in% seq(1955,1959,1),]
imdb_1960 <- imdb[imdb$year %in% seq(1960,1964,1),]
imdb_1965 <- imdb[imdb$year %in% seq(1965,1969,1),]
imdb_1970 <- imdb[imdb$year %in% seq(1970,1974,1),]
imdb_1975 <- imdb[imdb$year %in% seq(1975,1979,1),]
imdb_1980 <- imdb[imdb$year %in% seq(1980,1984,1),]
imdb_1985 <- imdb[imdb$year %in% seq(1985,1989,1),]
imdb_1990 <- imdb[imdb$year %in% seq(1990,1994,1),]
imdb_1995 <- imdb[imdb$year %in% seq(1995,1999,1),]
imdb_2000 <- imdb[imdb$year %in% seq(2000,2004,1),]
imdb_2005 <- imdb[imdb$year %in% seq(2005,2009,1),]
imdb_2010 <- imdb[imdb$year %in% seq(2010,2014,1),]
imdb_2015 <- imdb[imdb$year %in% seq(2015,2019,1),]
imdb_2020 <- imdb[imdb$year %in% seq(2020,2024,1),]

imdbs <- list(imdb_1925, imdb_1930, imdb_1935, imdb_1940, imdb_1945, imdb_1950, imdb_1955, imdb_1960, imdb_1965, imdb_1970, imdb_1975, imdb_1980, imdb_1985, imdb_1990, imdb_1995, imdb_2000, imdb_2005, imdb_2010, imdb_2015, imdb_2020)

###############################################################################
# Creating plots
library(ggplot2)
library(dplyr)
library(ggthemes)
library(gridExtra)
library(paletteer)

# Plot 1: Most Popular Movie Genres (bar plot)
years <- c("1925-1929", "1930-1934", "1935-1939", "1940-1944", "1945-1949", "1950-1954", "1955-1959", "1960-1964", "1965-1969", "1970-1974", "1975-1979", "1980-1984", "1985-1989", "1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2024")

library(imager)
library(ggimage)
library(magick)
library(tidyverse)
library(gganimate)
library(png)
library(gapminder)
library(gifski)
library(reshape2)
library(Rcpp)

genre_counts <- as.data.frame(imdb) %>%
  group_by(genre.1, year) %>%
  summarise(count = n())
genre_counts <- as.data.frame(genre_counts)

plot <- ggplot(data = genre_counts,aes(x = genre.1, y = count, fill = genre.1))+
  geom_bar(stat = 'identity')+
  labs(title = "Number of Movies Produced Per Genre", x="Movie Genre", y="Number of Movies")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
plot
plot_ani <- plot + transition_states(year)
animate(plot_ani, renderer=gifski_renderer(), fps=5)
anim_save("moviegenresovertime.gif",animation=plot_ani, renderer=gifski_renderer(), fps=5)

# Plot 2: Box Office Revenue Per Capita (map)
library(ggmap)
library(maps)
library(mapdata)
library(sp)
library(spData)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
View(world)
colnames(world)[9] <- "Country" 
world$Country[world$Country == "United States of America"] <- "United States"
world$Country[world$Country == "Hong Kong S.A.R."] <- "Hong-Kong"
world_box <- left_join(world, box, by = "Country")
View(world_box) 

world_box$Box_Office_Revenues <- (world_box$Box_Office_Revenues / world_box$Population)

map.theme <- theme(axis.line = element_blank(), axis.text.x = element_blank(),
                 axis.text.y = element_blank(), axis.ticks = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 panel.background = element_blank(), panel.border=element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), plot.background=element_blank())

ggplot(data = world_box) +
  geom_sf(aes(fill = Box_Office_Revenues)) + map.theme +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") + theme(legend.position="bottom", plot.title = element_text(hjust = 0.5)) +
  labs(title = "Box Office Revenue Per Capita By Country", fill = "Revenue Per Capita") 

# https://www.ravereviews.org/entertainment/top-rated-movie-produced-in-every-country/ 
# We can see that countries with higher earnings do not necessarily have the highest rated movies.
# Of the top countries in our plot, only the United States had a more highly rated movies.

# Plot 3: (linear model, coefficient table, interaction plot)

lm <- lm(imdb_rating ~ gross_earning + time_minute + metascore*vote, data = imdb)
summary(lm)

stargazer(lm, type="text")

library(interactions)
interact_plot(lm, pred = vote, modx = metascore, interval = TRUE)

par(mfrow=c(4,1))
cplot(lm, "vote", main="Effect of Votes on IMDB Rating", 
      xlab="Number of Votes", ylab="IMDB Rating", se.fill="red", lwd=1)

cplot(lm, "metascore", main="Effect of Metascore on IMDB Rating", 
      xlab="Metascore", ylab="IMDB Rating", se.fill="red", lwd=1)

cplot(lm, "time_minute", main="Effect of Length of Movie on IMDB Rating", 
      xlab="Movie Length (in minutes)", ylab="IMDB Rating", se.fill="red", lwd=1)

cplot(lm, "gross_earning", main="Effect of Gross Earning of a Movie on IMDB Rating", 
      xlab="Gross Earning", ylab="IMDB Rating", se.fill="red", lwd=1)

# Plot 4: Movie Trends Over Time (line plot)
avg_imdb_rating <- as.data.frame(imdb_1925) %>%
  group_by(genre.1) %>%
  summarise_at(vars(imdb_rating), list(name = mean))
avg_imdb_rating <- as.data.frame(avg_imdb_rating)
df <- avg_imdb_rating
df$year <- "1925-1929"

for (i in 2:length(imdbs)){
  avg_imdb_rating <- as.data.frame(imdbs[i]) %>%
    group_by(genre.1) %>%
    summarise_at(vars(imdb_rating), list(name = mean))
  avg_imdb_rating <- as.data.frame(avg_imdb_rating)
  avg_imdb_rating$year <- years[i]
  df <- rbind(df, avg_imdb_rating)
}

avg_imdb_rating1 <- df %>%
  group_by(year) %>%
  summarise_at(vars(name), list(name = mean))
avg_imdb_rating1 <- as.data.frame(avg_imdb_rating1)

ggplot(data = avg_imdb_rating1, aes(x = year, y = name, group = 1)) + geom_line(size=.5)  + geom_point(size=2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme_minimal() +
  labs(title = "IMDB Ratings Have Decreased Over Time", subtitle = "Average IMDB Rating Per Year") + xlab("Year") + ylab("Average IMDB Rating")


# Plot 5: Movie Trends Over Time (dot plot)

ggplot(data = df, aes(x = year, y = name, color = genre.1))  + geom_point(size=3) + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Action, Mystery, Crime, and Animation are the Most Highly Rated Movie Genres" , subtitle = "Most Highly Rated Movie Genre Per Year", color = "Movie Genre") + xlab("Year") + ylab("Average IMDB Rating")

# Plot 6: Top 10 Highly Rated Movies
library(ggrepel)

imdb %>%
  arrange(desc(imdb_rating)) %>%
  top_n(15, imdb_rating) %>%
  ggplot(aes(x = gross_earning, y = imdb_rating)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label = movie)) +
  labs(x = "Gross Earning", y = "IMDB Rating", title = "Top 10 Highly Rated Movies") +
  theme(plot.title = element_text(hjust = 0.5))
###############################################################################