library(dplyr)
library(ggplot2)

start <- "2021-04-01 11:53:17 -04"

making <- "2021-04-01 12:03:37 -04"

styling <- Sys.time()

datos <- data.frame(time = c(Making = as.numeric(difftime(making, start, units = "mins")),
                             Styling = as.numeric(difftime(styling, making, units = "mins")))) %>% 
  tibble::rownames_to_column("stage") %>% 
  arrange(desc(stage))%>%
  mutate(
    prop = time/sum(time)*100,
    ypos = cumsum(prop)-0.5*prop,
    ypos2 = ypos+5
  )

ggplot(datos, aes(x="", y = prop, fill = stage)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = stage), color = "white", size = 6) +
  geom_text(aes(y = ypos2, label = paste0(round(prop,1), "%")), color = "white", size = 6) +
  ggtitle("Time spent making the chart \n vs \n time spent styling the chart") +
  labs(subtitle = "#30DayChartChallenge \n\n Day 1: part-to-whole", caption = "@drdsdaniel") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, margin = margin(20), size = 24),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(20), size = 16),
    plot.caption = element_text(margin = margin(10)),
    plot.background = element_rect(fill = "grey75", color = NA)
  ) +
  ggsave(glue::glue("1-part-to-whole/image_{as.numeric(Sys.time())}.png"))

images <- paste0("1-part-to-whole/", list.files("1-part-to-whole/", pattern = "image"))

m <- magick::image_read(images[1])
for (i in 2:length(images)){
  m <- c(m, magick::image_read(images[i]))
}

m <- magick::image_animate(m, fps = 1, loop = 1, dispose = "previous")

magick::image_write(m, "1-part-to-whole/part-to-whole.gif")
