# For this file, we're going to clean our raw data
# this is not necessary but I wanted to show the process

# load libraries
library(tidyverse)

## get list of albums 
# (notice I didn't do all albums -- but you could!)
albums_t <- c("TaylorSwift", "Fearless", "SpeakNow",  "Red", 
              "1989", "Reputation", "Lover", "Folklore", 
              "Evermore", "Midnights_3amEdition_")

files <- vector() # need this so we can loop through albums
for (album in albums_t){
  loc <- paste0("data/swift_data_raw/Albums/", album) # tell it where to go
  new <- dir(path = loc, pattern = "txt$", full.names = T) # get lsit of files
  files <- c(files, new) # add the files to ongoing list
}

files  #yay, we did it!

## Now we need to loop through our list of files and pull out the text for each song
# easiest to use a function for this

get_lyrics <- function(path) {
  #import the data, skipping the first row
  Title <- str_match(path, "([a-zA-Z0-9\ _]+)\\.txt")[2]  %>% # get name and add spaces!
    str_replace_all( pattern = "([:lower:]|[:upper:])([:upper:])", replacement = "\\1 \\2")  %>%
    str_replace_all( pattern ="_", replacement = " ")
  Album <- str_match(path, "Albums/([a-zA-Z0-9\ _]+)\\/")[2] %>%
    str_replace_all( pattern = "([:lower:]|[:upper:])([:upper:])", replacement = "\\1 \\2")  %>%
    str_replace_all( pattern ="_", replacement = " ")
  
  #clean the text: first line and labels e.g. '[verse]'
  data <- read_lines(path, skip = 1, skip_empty_rows = TRUE) %>%
    str_subset( "[\\]$]", negate = TRUE) %>% 
    paste(collapse = " ")# gather it back in 
                        # (could have probably done with read_file but liked skipping the first line)
  
  # put it all together!
  data_comb <- as.data.frame(cbind(Album, Title, data )) %>%
    rename(Lyrics = `data`)
  #return the cleaned data
  return(data_comb)
}

# now, we use our function to get our lyrics!
expanded_swift_lyrics <- map_df(files, get_lyrics)

## one last little bit of cleaning:
write_csv(expanded_swift_lyrics, file ="data/expanded_swift_lyrics.csv")

