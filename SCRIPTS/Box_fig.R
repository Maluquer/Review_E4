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
setwd("E4/")
papers <- read_delim("papers_review_revisio1.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

papers |>
  count(FOREST_RESPONSE_1, name = "n_studies")

papers |>
  count(METHODOLOGY_1, name = "n_studies")

papers |>
  count(PARAMETER, name = "n_studies")

# prepare data for plotting -----------------------------------------------
# remove papers without statistical parameters
names(papers)
table(papers$ESTADISTICO_SI_NO, useNA = "always")

papers_param <- papers |> 
  filter(
    ESTADISTICO_SI_NO != "NO"
  ) |> 
  select(
    Year, FOREST_RESPONSE_1, METHODOLOGY_1,
    PARAMETER
  )

# group data
group_data <- function(grp_var) {
  papers_param |>
    mutate(Year = as.factor(Year)) |> 
    group_by(Year, {{ grp_var }}) |>
    summarise(n_studies = n()) |> 
    drop_na()
}

# ecological processes
data_process <- group_data(FOREST_RESPONSE_1)

# methodologies
data_methods <- group_data(METHODOLOGY_1)

# tests
data_test <- group_data(PARAMETER)

# figures -----------------------------------------------------------------
plot_data <- function(data, var_fill) {
  ggplot(data,
         aes(x = Year,
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
        axis.text = element_text(size = 11.5, colour = "grey20"),
        axis.title = element_text(size = 11.5, colour = "grey20"),
        axis.line = element_line(color = "grey20"),
        axis.ticks = element_line(color = "grey20"),
        legend.position = "right",
        legend.text = element_text(size = 11.5, colour = "grey20"),
        legend.title = element_text(size = 12, colour = "grey20")
      ) 
  }

my_colors_1 <- c('#808080', '#DDCC77', "#006633", '#AA4499', '#44AA99', "#3399FF","#000333")

my_colors_2 <- c('#999933', '#CC6677',
                 '#117733', '#88CCEE', '#332288',
                 '#882255')

my_colors_3 <- c('#006666',
                 '#333300', 
                 '#CC9966',
                 '#808080')

# ecological processes
p1 <- plot_data(data_process, "FOREST_RESPONSE_1") +
  scale_fill_manual(
  values = my_colors_1,
  name = "Forest responses",
  breaks=c('Physiology', 'Phenology', 'Tree growth', 'Demography', 'Biomass stocks and fluxes', 'Vegetation activity')
) 

# methodologies
p2 <- plot_data(data_methods, "METHODOLOGY_1") +
  scale_fill_manual(
    values = my_colors_2,
    name = "Data type",
    breaks=c("Field study", "Experimental", "Dendrochronology", "Eddy covariance", "Remote sensing",  "Other" )
  )

# methodologies
p3 <- plot_data(data_test, "PARAMETER") +
  scale_fill_manual(
    values = my_colors_3,
    name = "Methodology",
    breaks=c("Periods-Years-Interaction",  "Observed-predicted",  "Moving correlation", "Other" )
  )

plots_review <- p1 / p2 /p3 + plot_annotation(tag_levels = 'A')

ggsave(
  plot = plots_review,
  "plots_review_r1.png",
  width = 10, height = 14,
  dpi = 600
)
