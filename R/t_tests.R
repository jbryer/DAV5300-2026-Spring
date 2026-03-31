library(DATA606)
library(ggplot2)
data(sat)

sat <- sat[complete.cases(sat),]
sat <- sat |> dplyr::filter(Sex != '') |>
	dplyr::mutate(Verbal.SAT = as.numeric(Verbal.SAT),
		   Math.SAT = as.numeric(Math.SAT))

tab <- psych::describeBy(sat$Verbal.SAT, group = sat$Sex, mat = TRUE, skew = FALSE)
tab

mean_diff <- diff(tab$mean)

se <- sqrt( (tab[1,]$sd^2 / tab[1,]$n) + (tab[2,]$sd^2 / tab[2,]$n) )

c(mean_diff - 1.96 * se,
  mean_diff + 1.96 * se)

qt(0.025, df = 10)
qt(0.025, df = 30)

2 * (1 - pt(2, df=10))

cv <- tibble::tibble(df = 1:50,
				 cv = qt(0.025, df))
ggplot(cv, aes(x = df, y = cv)) +
	geom_hline(yintercept = -1.96) +
	geom_path() +
	geom_point(size = 0.5)

t.test(Verbal.SAT ~ Sex, data = sat)
t.test(Math.SAT ~ Sex, data = sat)

t.test(sat$Verbal.SAT, sat$Math.SAT, paired = TRUE)


library(ggplot2)
x <- seq(1, 500)
df <- data.frame(x = x, y = log(x))
ggplot(df, aes(x = x, y = y)) + geom_point()
