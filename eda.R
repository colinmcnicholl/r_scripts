library(tidyr)
library(tidyselect)
library(dplyr) # for glimpse()
library(ggplot2)
library(ggrepel)
library(stringr)

english_education <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-23/english_education.csv')
save(english_education, file = "english_education.RDA")

# Filter out 2 observations: 'Other Small BUAs', and 'Not BUA'
english_education <- english_education |> filter(!size_flag %in% c("Other Small BUAs", "Not BUA"))
# Save 2 observations
english_education_london <- english_education |> subset(size_flag %in% c("Outer london BUA", "Inner London BUA"))

# Change 'Outer london BUA', and 'Inner London BUA' to 'London'
english_education$size_flag <- english_education$size_flag |> str_replace("Outer london BUA", "London")
english_education$size_flag <- english_education$size_flag |> str_replace("Inner London BUA", "London")

english_education_london2 <- english_education |> subset(size_flag == "London")



p <- ggplot(english_education, aes(education_score, size_flag)) +
  geom_jitter(color = "skyblue") +
  stat_summary(fun = mean, geom = "crossbar", color = "black", width = .8, size = .3) +
  scale_x_continuous(sec.axis = dup_axis()) +
  labs(
    x = "attainment",
    y = "",
    title = "Educational attainment score, by town, size, England"
    ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linewidth = .1, colour = "grey75")
  )

p +  geom_text_repel(data = english_education_london2,
                      label = c("Inner London", "Outer London"),
                     arrow = arrow(length = unit(0.010, "npc")),
                     nudge_x = 0.15,
                     nudge_y = 0.5)

