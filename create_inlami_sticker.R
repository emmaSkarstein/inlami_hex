# Creating hex-sticker

#install.packages("hexSticker")

library(hexSticker)
library(ggplot2)
library(showtext)
library(ggimage)
library(ggforce)
library(dplyr)

font_add_google("Courier Prime", "courier")
## Automatically use showtext to render text for future devices
showtext_auto()
showtext_opts(dpi = 400)

col_salami <- "#CC4D40"
col_dark_salami <- "#9C4944"
col_spots <- "#FCF5E0"
col_backgr <- "#F2F2F2"
col_bite <- col_backgr

set.seed(4)

n_spots <- 4*10

spots <- data.frame(r = sqrt(runif(n_spots)), theta = runif(n_spots)*2*pi,
                          size = rep(c(0, 1, 2, 3), n_spots/4)) %>%
  mutate(x = r*cos(theta), y = r*sin(theta))

p <- ggplot() +
  # Salami circle
  geom_circle(aes(x0 = 0, y0 = 0, r = 1.1),
              #linewidth = 5,
              fill = col_salami,
              color = col_salami) +
  # Fat spots
  geom_point(data = spots,
             aes(x = x, y = y, size = size),
             position = position_jitter(),
             color = col_spots) +
  scale_size_continuous(range = c(0.5, 2)) +
  # Bite circle
  geom_circle(aes(x0 = -0.9, y0 = 1.2, r = 0.8),
              fill = col_backgr, color = col_backgr) +
  geom_circle(aes(x0 = -0.85, y0 = 0.45, r = 0.25),
              fill = col_bite, color = col_bite) +
  geom_circle(aes(x0 = -0.5, y0 = 0.48, r = 0.25),
              fill = col_bite, color = col_bite) +
  geom_circle(aes(x0 = -0.2, y0 = 0.65, r = 0.25),
              fill = col_bite, color = col_bite) +
  geom_circle(aes(x0 = 0, y0 = 0.99, r = 0.25),
              fill = col_bite, color = col_bite) +
  # Package title
  geom_text(aes(x = -0.45, y = 0.75), angle = 30, size = 5,
            label = "inlami", family = "courier") +
  # Other stuff
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1)) +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none")
p

sticker(p,
        package="",
        s_x = 1,
        s_y = 1,
        s_width = 1.7,
        s_height = 1.7,
        # Package title
        #p_size=5,
        #p_color = "col_salami",
        #p_family = "courier",
        h_fill = col_backgr,
        h_color = "#9C4944",
        white_around_sticker = TRUE,
        filename="figures/inlami_white.png",
        dpi = 400)

# Make background transparent
library(magick)
button_white <- image_read("figures/inlami_white.png")
button <- button_white %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 4, point = "+1+1") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 4, point = "+517+1") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 4, point = "+1+599") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = 4, point = paste0("+", image_info(button_white)$width-10, "+", image_info(button_white)$height-10))
image_write(image = button, path = "figures/inlami_transparent.png")

