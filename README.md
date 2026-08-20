This is the R script for the many_peguins data set from git hub on 94 penguins and 15 variables, 3 were categorical, and 12 were continuous.

library(tidyverse)
library (gtsummary)
library(janitor)
library(ggplot2)
*these are the packages necessary for my code 

I used Here::here at the beginning to read in the data 

Then a descriptive table was used using gtsummary. The variables were re-labeled to make more sense to the audience and the table was stratified based on peguin species and there were a total of 18 species
#| echo: false
#| label: tbl-one
#| tbl-cap: "Table summary stratified by peguin species"

tbl_one <- tbl_summary(
	many_penguins,
	by = species, 
	include = c(
		beak.length_culmen, sex, genus, beak.length_nares, beak.width, 
		beak.depth, tarsus.length, wing.length, kipps.distance, 
		secondary1, `hand-wing.index`, tail.length
	),
	
	label = list(
		beak.length_culmen ~ "Length from the tip of the beak to the base of the skull (mm)",
		beak.length_nares ~ "Length from the anterior edge of the nostrils to the tip of the beak (mm)",
		beak.width ~ "Width of beak (mm)",
		beak.depth ~ "Depth of beak (mm)", # Extra comma removed here
		tarsus.length ~ "Length of a bird’s lower leg bone (mm)",
		wing.length ~ "Length of the wing (mm)",
		kipps.distance ~ "Length from the tip of the first secondary feather to the tip of the longest primary (mm)",
		secondary1 ~ "Length from the bend of the wing to the tip of the first secondary (mm)",
		`hand-wing.index` ~ "Kipp’s distance corrected for wing size (mm)", # Backticks added around hand-wing.index
		tail.length ~ "Length of tail (mm)"
	),
	missing_text = "Missing"
)

tbl_one

Then a regression table was used 
#| echo: false
#| label: tbl-two
#| tbl-cap: "Univariate regression table"
many_penguins1 <- many_penguins |> 
	mutate(
		sex = case_when(
			sex %in% c(1, "1", "Male", "M") ~ "Male",
			sex %in% c(2, "2", "Female", "F") ~ "Female",
			sex %in% c("U", "Unknown", "U") ~ NA_character_,
			TRUE ~ NA_character_
		),
		sex = factor(sex, levels = c("Male", "Female"))
	) |> 
	type_convert()
***This is where i used AI because it was very difficult for me to understand how to mutate the sex variable to just be male and female. There were many different labels for it such as F,M, and U for unknown so I asked AI 
to explain how to make a code to re-label the sex variable so i could make a regression table. 

sex_penguin_table <- tbl_uvregression(
	many_penguins1,
	x = sex,
	include = c(
		beak.width,
		beak.depth,
		wing.length,
		tail.length
	), 
	method=lm
)
sex_penguin_table

In the study population, the mean beak length across all penguins was `r round(mean(many_penguins$beak.length_culmen, na.rm = TRUE), 1)` mm and and the mean tail length was `r round(mean(many_penguins$tail.length, na.rm = TRUE), 1)` mm.

Univariate linear regressions highlighting whether certain physical measurements of pequins (beak width, beak length, wing length, and tail length) differ between Female and Male penguins with Male as the reference group


The results presented in `@tbl-two` draw directly from the baseline data compiled in `@tbl-one`

G-Plots
* Here I made a ploy-hist function, and this I found in other notes avialable in the slides 

```{r}
#| echo: false


plot_hist <- function(many_penguins1, beak.depth) {
	ggplot(many_penguins1, aes(x = {{ beak.depth }})) +
		geom_histogram(bins = 30) +
		theme_minimal()
}

```

```{r}
#| echo: false
#| warning: false
#| label: fig-1
#| fig-cap: "G-plot of beak depth"
plot_hist(many_penguins1,beak.depth )
```

```{r}
#| echo: false
#| warning: false
#| label: fig-2
#| fig-cap: "G-plot of beak length"
Figure_2 <- plot_hist(many_penguins1,tail.length )
Figure_2

`@fig-1` is a G-plot generated using information from `@tbl-one` .

Function: a function that can get the average ratio of each continuous variable

```{r}
#| echo: false

Ratio_beak<- function (x,y) { 
		ratio <- x/y
		mean_ratio <- (mean(ratio, na.rm = TRUE))
		return(mean_ratio) 
}

Ratio_beak(
	x = many_penguins1$beak.width, 
	y = many_penguins1$beak.depth
) 

Ratio_beak(	x = many_penguins1$wing.length, 
						y = many_penguins1$tail.length
)
	Here is my function to get the average ratio of any numerical variable compared to another numerical variable 

