library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)

u_url <- "https://docs.google.com/spreadsheets/d/1O-ZoqV8PX7YsgXppnbf2r8-k6pV2JovA9-h5Wo6adMQ/edit#gid=257587008"


ss <- gs_title("Coal relationships")
gs_data <- gs_read(ss, ws="CCM summary", skip=2)
  
gs_data <- gs_data %>% 
  separate(`Time frame`, into = c("tf_start", "tf_end"), sep = " - ", remove = FALSE, convert = TRUE)
  

# By Journal
tmp <- gs_data %>% 
  group_by(Journal) %>% 
  summarize(count=n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  filter(count != 1)
tmp$Journal <- reorder(tmp$Journal, tmp$count)

p = ggplot(tmp) +
  geom_bar(aes(x=Journal, y=count), stat="identity") +
  theme_bw() +
  coord_flip() +
  xlab("") + ylab("")
print(p)

# Time frame
p = ggplot(gs_data %>% unite(AU_PY, Author, PY, sep=" - ")) +
  geom_segment(aes(x=AU_PY, xend=AU_PY, y=tf_start, yend=tf_end)) +
  theme_bw() +
  coord_flip() +
  xlab("") + ylab("")
print(p)
