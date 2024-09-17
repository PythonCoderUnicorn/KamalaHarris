library(tidyverse)
library(tidytext)
library(readtext)
library(showtext)
library(paletteer)
library(ggtext)

font_add_google('Ubuntu','Ubuntu')
showtext_auto()

harris = readtext::readtext(file = "harristrumpdebate.txt")

harris_df = tibble(text= harris)

harris_df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  filter( n > 10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(
    aes(x= n , y= word, fill= n)
  ) +
  geom_col( show.legend = F) +
  scale_x_continuous(breaks = scales::pretty_breaks()  ) +
  coord_cartesian(expand = F) +
  # `"ggthemes::Classic Area Red"`
  scale_fill_paletteer_c(`"ggthemes::Red-Blue Diverging"`, direction = -1) +
  labs(
    y=NULL,
    title = "\nVice President Kamala Harris Debate Word Count",
    subtitle = "transcript of Harris v Trump debate (Sept 11, 2024)",
    x= "Word Count",
    caption = "@unicornCoder | Source: ABC.com \n"
  ) +
  theme(
    text = element_text(family = "Ubuntu", color = 'grey90'),
    
    plot.title = element_text(size = 15, face = 'bold',hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, face = 'italic'),
    
    axis.text.x = element_text(size = 12, color = 'grey70'),
    axis.text.y = element_text(size = 13, color = 'grey80'),
    
    plot.caption = element_text(hjust = 0, size = 11, color = 'grey70'),
    
    panel.background = element_rect(fill = '#003d99'),
    plot.background = element_rect(fill = '#003d99'),
    panel.grid.major = element_line(colour = "#0047b3"),
    panel.grid.minor = element_line(colour = "#0047b3"),
    
    plot.margin = margin(t = 0,r = 1,b = 0,l = 1,unit = 'cm')
  )
