my_inner_fun <- function(name = 'No name') {
	thename <- name
	print(thename)
}

my_fun <- function(...) {
	my_inner_fun(...)
}

if(FALSE) { # For testing
	my_inner_fun()
	my_inner_fun(name = 'Jason')
	my_fun()
	my_fun(name = 'Jason')


	data(mtcars)
	lm(mpg ~ wt, data = mtcars) |> summary()
	glm(mpg ~ wt, data = mtcars, family = gaussian()) |> summary()

	library(VisualStats)
	data("hand_washing")
	aov(Bacterial_Counts ~ Method, data = hand_washing) |> summary()
	table(hand_washing$Method)
	lm(Bacterial_Counts ~ Method, data = hand_washing) |> summary()
}

