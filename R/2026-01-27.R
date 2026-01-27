library(ggplot2)
library(reshape2)

var_num <- 1
var_df <- data.frame(
	n = seq(10, 100, by = 1)
)
var_df$var_sample <- var_num / (var_df$n - 1)
var_df$var_pop <- var_num / (var_df$n)

var_df |>
	melt(id.var = 'n') |>
	ggplot(aes(x= n, y = value, color = variable)) +
	geom_path() +
	xlab('n') + ylab('variance')

data(legosets, package = 'brickset')
hist(legosets$pieces)
qplot(legosets$pieces)


ggplot(legosets, aes(x=pieces, y=US_retailPrice)) +
	geom_point() +
	theme_minimal()

ggplot(legosets, aes(x=pieces, y=US_retailPrice, size=minifigs, color=availability)) +
	geom_point(alpha = 0.1)


ggplot(legosets, aes(x = US_retailPrice)) + geom_histogram()
ggplot(legosets, aes(x = US_retailPrice)) + geom_histogram(binwidth = 20)
ggplot(legosets, aes(x = US_retailPrice)) + geom_histogram(bins = 50)

legosets |>
	dplyr::mutate(log_USD = log10(US_retailPrice)) |>
	ggplot(aes(x = log_USD)) +
	geom_histogram(bins = 20)


legosets |>
	dplyr::mutate(log_USD = log10(US_retailPrice)) |>
	ggplot(aes(x = log_USD)) +
	geom_histogram(bins = 20, fill = 'purple', color = 'white') +
	theme_minimal()

legosets |>
	ggplot(aes(x = US_retailPrice)) +
	geom_density()

ggplot(legosets, aes(x= availability, fill = themeGroup)) +
	geom_bar()

ggplot(legosets, aes(x= availability)) +
	geom_bar(fill = 'steelblue')
