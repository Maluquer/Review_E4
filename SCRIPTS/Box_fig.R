# Figure literature review Ecography E4 -------------------------------
## Xavier Serra Maluquer

# generate a bar chart showing how the number of studies
# for each ecological process and methodology
# varies over the years

# libraries
library(tidyverse)
library(patchwork)
library(readr)

# read data ---------------------------------------------------------------
# data where we have classified the papers from the literature review
# according to the ecoligcal process and methodology
setwd("~/OTROS_SIDE_PROJECTS/Ecography E4/E4/")
papers <- read_delim("review_papers.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)


papers |>
  count(response_variable, name = "n_studies")

papers |>
  count(data, name = "n_studies")


# prepare data for plotting -----------------------------------------------
# remove papers without statistical parameters
names(papers)
table(papers$statistical_parameter, useNA = "always")

papers_param <- papers |> 
  filter(
    statistical_parameter != "NO"
  ) |> 
  select(
    publication_year, response_variable,
    data
  )

# group data
group_data <- function(grp_var) {
  papers_param |>
    mutate(publication_year = as.factor(publication_year)) |> 
    group_by(publication_year, {{ grp_var }}) |>
    summarise(n_studies = n()) |> 
    drop_na()
}

# ecological processes
data_process <- group_data(response_variable)

# methodologies
data_methods <- group_data(data)


# figures -----------------------------------------------------------------
plot_data <- function(data, var_fill) {
  ggplot(data,
         aes(x = publication_year,
             y = n_studies,
             fill =  .data[[var_fill]])) +
    geom_bar(stat = "identity") +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      y = "Number of studies",
      x = "Publication year",
    ) +
    theme(
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.5),
      panel.background = element_blank(),
      axis.text = element_text(size = 15, colour = "grey20"),
      axis.title = element_text(size = 15, colour = "grey20"),
      axis.line = element_line(color = "grey20"),
      axis.ticks = element_line(color = "grey20"),
      legend.position = "right",
      legend.text = element_text(size = 11.5, colour = "grey20"),
      legend.title = element_text(size = 12, colour = "grey20")
    ) 
}

my_colors_1 <- c('#808080', '#DDCC77', '#AA4499', '#44AA99')

my_colors_2 <- c('#88CCEE', '#332288',
                 '#117733', '#999933',
                 '#CC6677', '#882255')

# ecological processes
p1 <- plot_data(data_process, "response_variable") +
  scale_fill_manual(
    values = my_colors_1,
    name = "Forest responses"
  )

# methodologies
p2 <- plot_data(data_methods, "data") +
  scale_fill_manual(
    values = my_colors_2,
    name = "Methodology"
  )


plots_review <- p1 / p2

ggsave(
  plot = plots_review,
  "plots_review.png",
  width = 8, height = 11,
  dpi = 600
)
