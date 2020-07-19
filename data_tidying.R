
# Final Project Data Cleaning ---------------------------------------------

# Data downloaded on 11/26/19

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)


# Tidying Game Release by Year Datasets -----------------------------------

# Importing 2004 - 2019 game release datasets
Y2004 <- read_csv("data/unprocessed/games/2004.csv")
Y2005 <- read_csv("data/unprocessed/games/2005.csv")
Y2006 <- read_csv("data/unprocessed/games/2006.csv")
Y2007 <- read_csv("data/unprocessed/games/2007.csv")
Y2008 <- read_csv("data/unprocessed/games/2008.csv")
Y2009 <- read_csv("data/unprocessed/games/2009.csv")
Y2010 <- read_csv("data/unprocessed/games/2010.csv")
Y2011 <- read_csv("data/unprocessed/games/2011.csv")
Y2012 <- read_csv("data/unprocessed/games/2012.csv")
Y2013 <- read_csv("data/unprocessed/games/2013.csv")
Y2014 <- read_csv("data/unprocessed/games/2014.csv")
Y2015 <- read_csv("data/unprocessed/games/2015.csv")
Y2016 <- read_csv("data/unprocessed/games/2016.csv")
Y2017 <- read_csv("data/unprocessed/games/2017.csv")
Y2018 <- read_csv("data/unprocessed/games/2018.csv")
Y2019 <- read_csv("data/unprocessed/games/2019.csv")

# Joining data together
Steam_Games_2004_2019 <- rbind(Y2004, Y2005, Y2006, Y2007, Y2008, Y2009, Y2010, Y2011, Y2012, Y2013, Y2014, Y2015, Y2016, Y2017, Y2018, Y2019)

# Exporting combined data as .csv
write_csv(Steam_Games_2004_2019, "data/unprocessed/Steam_Games_2004_2018_raw.csv")

# Tidying columns

df <- Steam_Games_2004_2019 

# Rename # and Game column
df <- df %>% 
  rename(number = `#`) 

df <- df %>% 
  rename(game = `Game`) 

# Convert Release data column to date
df$`Release date` = mdy(df$`Release date`)

df <- df %>% 
  rename(release_date = `Release date`) 

# Convert Prices to numeric format
df <- df %>% 
  rename(price = `Price`) 

df$price[df$price == "Free"] <- 0.00

df$price <- as.numeric(gsub('[$,]', '', df$price))


# Separating Score Rank from (Userscore / Metascore) -> Keeping only Metascore (Most to all of Userscore and Score Rank are N/As - not useful for EDA in my opinion)
df <- df %>%
  separate(`Score rank(Userscore / Metascore)`, into = c("score_rank", "Metascore"), sep = " ")

# Separating Userscore from Metascore and converting to numeric format
df$Metascore = str_sub(df$`Metascore`,-4,-3)

df$Metascore <- as.numeric(df$Metascore)

df <- df %>% 
  rename(metascore = `Metascore`) 

# Renaming Owners column
df <- df %>% 
  rename(owners = `Owners`) 

# Separating Average Playtime from Median Playtime
df <- df %>%
  separate(`Playtime (Median)`, into = c("avg_playtime", "median_playtime"), sep = " ")

df$median_playtime = str_sub(df$median_playtime,-6,-2)

# Convert avg_playtime and median_playtime to time

df <- df %>%
  mutate(avg_playtime = 60 * as.numeric(str_sub(avg_playtime, 1, 2)) + as.numeric(str_sub(avg_playtime, 4, 5)),
         median_playtime = 60 * as.numeric(str_sub(median_playtime, 1, 2)) + as.numeric(str_sub(median_playtime, 4, 5))
         )

# Renaming Developer(s) and Publisher(s) columns
df <- df %>% 
  rename(developers = `Developer(s)`) 

df <- df %>% 
  rename(publishers = `Publisher(s)`) 

# Removing unnecessary columns 
df <- df %>%
  select(-score_rank, -number)

# Importing "Top 100 Games in the Last 2 Weeks by Total Time Spent" Dataset
top_100 <- read_csv("data/unprocessed/top_100.csv")

top_100

# Tidying top_100
top_100 <- top_100 %>%
  # Removing unnecessary columns 
  select(Game) %>%
  rename(game = `Game`) %>%
  mutate(trending = c("Yes"))

top_100
  
# Joining with df_new dataset
df_tidy <- df %>%
  full_join(top_100, df, by = "game")

df_tidy


# Exporting tidied data as .cvs
write_csv(df_tidy, "data/processed/games_tidied.csv")


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Tidying Genre Datasets --------------------------------------------------

# Importing genre datasets (9 genres)

action <- read_csv("data/unprocessed/genre/action.csv")
adventure <- read_csv("data/unprocessed/genre/adventure.csv")
early_access <- read_csv("data/unprocessed/genre/early_access.csv")
indie <- read_csv("data/unprocessed/genre/indie.csv")
mmo <- read_csv("data/unprocessed/genre/mmo.csv")
rpg <- read_csv("data/unprocessed/genre/rpg.csv")
simulation <- read_csv("data/unprocessed/genre/simulation.csv")
sports <- read_csv("data/unprocessed/genre/sports.csv")
strategy <- read_csv("data/unprocessed/genre/strategy.csv")

# Adding genre identification column to each dataset

action <- action %>%
  mutate(genre = c("action")) %>%
  select(genre, Game)

adventure <- action %>%
  mutate(genre = c("adventure")) %>%
  select(genre, Game)

early_access <- early_access %>%
  mutate(genre = c("early_access")) %>%
  select(genre, Game)

indie <- indie %>%
  mutate(genre = c("indie")) %>%
  select(genre, Game)

mmo <- mmo %>%
  mutate(genre = c("mmo")) %>%
  select(genre, Game)

rpg <- rpg %>%
  mutate(genre = c("rpg")) %>%
  select(genre, Game)

simulation <- simulation %>%
  mutate(genre = c("simulation")) %>%
  select(genre, Game)

sports <- sports %>%
  mutate(genre = c("sports")) %>%
  select(genre, Game)

strategy <- strategy %>%
  mutate(genre = c("strategy")) %>%
  select(genre, Game)

action
adventure
early_access
indie
mmo
rpg
simulation
sports
strategy

# Joining data together
game_genres <- rbind(action, adventure, early_access, indie, mmo, rpg, simulation, sports, strategy)

game_genres

# Renaming Games columns to match format of Game Release by Year Dataset
game_genres <- game_genres %>% rename(game = `Game`) 

game_genres

# Exporting combined data as .csv
write_csv(game_genres, "data/processed/game_genres.csv")


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------







