#-------------------------------------------------- 
# Merging data frames
#--------------------------------------------------
library(tidyverse)

artist_DF <- data.frame(
  artist_name = c("Justin Bieber",
                  "Drake",
                  "The Smiths"),
  artist_ID = 1:3
)

album_DF <- data.frame(
  album_name = c("Thank Me Later", "Take Care",
                 "Nothing Was The Same", "Views",
                 "Scorpion", 
                 "Strangeways Here We Come",
                 "Thank U Next"),
  album_ID = 1:7,
  artist_ID = c(rep(2,5),3,4)
)

# inner join
merged_DF <- inner_join(x = artist_DF,
                        y = album_DF,
                        by = "artist_ID")

merged_DF

# full join
merged_DF <- 
  full_join(x = artist_DF,
            y = album_DF,
            by = "artist_ID")

merged_DF

# left join
merged_DF <- 
  left_join(x = artist_DF,
            y = album_DF,
            by = "artist_ID")

merged_DF

# right join
merged_DF <- 
  right_join(x = artist_DF,
             y = album_DF,
             by = "artist_ID")

merged_DF

#-------------------------------------------------- 
# Factor Levels - Wine Labels Example
#--------------------------------------------------
# see more on forcats https://forcats.tidyverse.org/
# recoding factor labels by hand
library('tidyverse')
WineDF <- read_csv(here::here("datasets","WineExample.csv"))
glimpse(WineDF)
summary(WineDF)

# View(WineDF)
table(WineDF$Variety, WineDF$Color)


# relabel by hand but only have to relabel changes
library('forcats')
WineDF <- WineDF %>%  
  mutate(Variety_lbl1 = fct_recode(Variety,  
                                   "OtherRed" = "V3",
                                   "OtherRed" = "V4",
                                   "OtherWhite" = "V7",
                                   "OtherWhite" = "V8"))

table(WineDF$Variety, WineDF$Variety_lbl1)


# reorder factors by frequency of values
WineDF <- WineDF %>% 
  mutate(Variety_lbl2 = fct_infreq(Variety))

table(WineDF$Variety, WineDF$Variety_lbl2)

levels(WineDF$Variety_lbl2)

# lump into top 5 factors
WineDF <- WineDF %>% 
  mutate(Variety_lbl3 = fct_lump(Variety, 5))

table(WineDF$Variety, WineDF$Variety_lbl3)


#-------------------------------------------------------------------------------
# Unstructured Strings -> Dummy varaibles
#-------------------------------------------------------------------------------
listings <- read.csv(here::here("datasets","listings_clean.csv"))

View(listings)
library('tokenizers')
library('magrittr')
tokenize_words(listings$amenities) # gives an error because we are trying to tokenize a factor

# need to convert from factors to strings
library('magrittr')
listings %<>% mutate(amenities = as.character(amenities))

# from strings to tokens
head(tokenize_words(listings$amenities))

head(listings$amenities)

# create a variable to hold the separated amenities
listings %<>%
  mutate(amenities_separated = NA) %>%
  select(id,amenities, amenities_separated, everything() )


# clean each of the rows of listings
for (i in 1:nrow(listings)){
  amenities <- unlist(str_split(listings$amenities[i], pattern = ","))
  amenities_vec <- str_replace_all(amenities,pattern = "\"", replacement = "")
  amenities_vec2 <- str_replace_all(amenities_vec,pattern = "\\(|\\)", replacement = "")
  listings$amenities_separated[i] <- paste(amenities_vec2, collapse = ", ")
}

head(listings$amenities_separated)

# create column of unique listings of amenities
total_amenities = unique(unlist(str_split(listings$amenities_separated, pattern = ",") ))
total_amenities = str_replace_all(total_amenities, pattern = "^ ", replacement="")
total_amenities = str_replace_all(total_amenities, pattern = "\\(|\\)", replacement="")


listings_wide = listings

# loop through all amenities in DF and flag = 1 if amenities match the listed one  
for (i in 1:length(total_amenities)){
  listings_wide[,total_amenities[i]] <- 
    str_count(listings_wide$amenities_separated, pattern = total_amenities[i])
}


listings_wide %<>%
  select(id, amenities, amenities_separated, total_amenities, everything())

# special thanks to Selina Carter for help with this one. https://twitter.com/selina_carter_?lang=en


#-------------------------------------------------------------------------------
# Text Sentiment
#-------------------------------------------------------------------------------
library('devtools')
devtools::install_github("trinker/sentimentr")


# average sentiment score
library('sentimentr')
library('tidyverse')

sentiment_by("I am very scared of the dark")

sentiment('I was very scared of the dark. 
          Now I glow at night and life is amazing')


"I am very scared of the dark" %>% 
  extract_sentiment_terms()

"Now I glow at night and life is amazing" %>% 
  extract_sentiment_terms()

"and I was like baby, baby, baby oh" %>% 
  extract_sentiment_terms()


"Ooh whoa, ooh whoa, ooh whoa.
You know you love me, I know you care.
Just shout whenever and I'll be there.
You are my love, you are my heart.
And we will never, ever, ever be apart.
Are we an item? Girl quit playin'.
We're just friends, what are you sayin'.
Said there's another, look right in my eyes.
My first love, broke my heart for the first time.
And I was like baby, baby, baby oh.
Like baby, baby, baby no.
Like baby, baby, baby oh.
I thought you'd always be mine (mine).
Baby, baby, baby oh.
Like baby, baby, baby no.
Like baby, baby, baby ooh.
I thought you'd always be mine.
Oh for you, I would have done whatever.
And I just can't believe we ain't together.
And I wanna play it cool.
But I'm losin' you.
I'll buy you anything.
I'll buy you any ring.
And I'm in pieces, baby fix me.
And just shake me, 'til you wake me from this bad dream.
I'm goin' down, down, down, down.
And I can't just believe my first love won't be around.
And I'm like baby, baby, baby oh.
Like baby, baby, baby no.
Like baby, baby, baby oh.
I thought you'd always be mine (mine).
Baby, baby, baby oh.
Like baby, baby, baby no.
Like baby, baby, baby ooh.
I thought you'd always be mine.
Luda, when I was thirteen, I had my first love.
There was nobody that compared to my baby.
And nobody came between us no one could ever come above.
She had me goin' crazy.
Oh, I was starstruck.
She woke me up daily.
Don't need no Starbucks.
She made my heart pound.
And skip a beat when I see her in the street and.
At school on the playground.
But I really wanna see her on the weekend.
She know she got me dazin'.
'Cause she was so amazin'.
And now my heart is breakin'.
But I just keep on sayin'.
Baby, baby, baby oh.
Like baby, baby, baby no.
Like baby, baby, baby oh.
I thought you'd always be mine (mine).
Baby, baby, baby oh.
Like baby, baby, baby no.
Like baby, baby, baby ooh.
I thought you'd always be mine.
Now I'm gone.
Yeah, yeah, yeah.
Yeah, yeah, yeah (now I'm all gone).
Yeah, yeah, yeah.
Yeah, yeah, yeah (now I'm all gone).
Yeah, yeah, yeah.
Yeah, yeah, yeah (now I'm all gone).
Gone, gone, gone, I'm gone." %>% 
  sentiment_by(by = NULL) %>% highlight()

##############################################
#  Unsupervised Learning
##############################################


# set seed for randomizer
rm(list =ls())
set.seed(1861)
options(scipen=15)

# set working directory to your directory 
setwd("")


##############################################
# k-means clustering
##############################################
library('ISLR')
library('factoextra')
library('cluster')
library('tidyverse')
data("USArrests")
kmeans3 <- kmeans(USArrests, centers = 3, nstart = 25)
str(kmeans3)
kmeans3

fviz_cluster(kmeans3, data = USArrests, main = "K=3 means clustering")

kmeans5 <- kmeans(USArrests, centers = 5, nstart = 25)
fviz_cluster(kmeans5, data = USArrests, main = "K=5 means clustering")

# how many clusters? Elbow method
wss <- function(k){
  kmeans(USArrests, k, nstart = 25)$tot.withinss
}
wss_values <- map_dbl(1:15, wss)
plot(1:15, wss_values, 
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-cluster sum of squares")

##############################################
# PCA
##############################################

rm(list=ls())

require('ISLR')
library('ggfortify')
dimnames(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2,var)

pca.out <- prcomp(USArrests, scale=TRUE)
pca.out

screeplot(pca.out)

# boring biplot plot
biplot(pca.out)

# ggplot biplot 
library('ggfortify')
autoplot(prcomp(USArrests), data = USArrests, 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 5, 
         label = TRUE) + theme_bw() 


