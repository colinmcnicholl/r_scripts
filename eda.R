library(tidyr)
library(tidyselect)
library(dplyr) # for glimpse()
library(ggplot2)
library(ggrepel)
library(stringr)

#english_education <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')
#save(english_education, file = "english_education.RDA")
load("english_education.RDA")

# Filter out 2 observations: 'Other Small BUAs', and 'Not BUA'
english_education <- english_education %>%  filter(!size_flag %in% c("Other Small BUAs", "Not BUA"))
# Save 2 observations
#english_education_london <- english_education %>% subset(size_flag %in% c("Outer london BUA", "Inner London BUA"))

# Add a new variable 'size_flag3' renaming 'Outer london BUA' and 'Inner Lonon BUA'
# to 'London'
english_education$size_flag3 <- english_education$size_flag %>% str_replace("Outer london BUA", "London")
english_education$size_flag3 <- english_education$size_flag3 %>% str_replace("Inner London BUA", "London")

# Add indicator variable 'size_flag2' to give a logical vector for subsetting data
english_education$size_flag2 <- english_education$size_flag %in% c("Outer london BUA", "Inner London BUA")

#english_education <- english_education %>% group_by(size_flag2)

# english_education_london <- english_education %>% subset(size_flag3 == "London")

p <- ggplot(english_education, aes(education_score, size_flag3)) +
    geom_jitter(color = "skyblue") +
    stat_summary(fun = mean, geom = "crossbar", color = "black", width = .8, size = .3) +
    scale_x_continuous(sec.axis = dup_axis()) +
    labs(
        x = "",
        y = "",
        subtitle = "<----- low attainment                                         high attainment ----->"
    ) +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "grey75")
    )

p <- p +  geom_text_repel(mapping = aes(
        label = str_replace(size_flag, c("London BUA", "london BUA"), "")
        ),
        data = subset(english_education, size_flag2),
        nudge_y = 0.1
                     )

df <- data.frame(x1 = 3, x2 = 0.3, y1 = "Small Towns", y2 = "Small Towns")

p <- p + geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2), data = df,
               curvature = -0.3,
               size = 1,
               color = "black",
               arrow = arrow(length = unit(0.1, "cm")))

p +
    geom_text(label = "average\nattainment",
              color = "black",
              x = 4, y = "Small Towns")
