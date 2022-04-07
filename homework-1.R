pacman::p_load(tidyverse, ggplot)

# utils
gaussian <- function(x, mu, sigma) {
    1 / (sigma * sqrt(2 * pi)) * exp(-(x - mu)^2 / (2 * sigma^2))
}

# Answers to Kay, Fundamentals of Statistical Computing, Homework 1


# ## 2.2.1

# ## 1.1.1
# Round trip delay N(tau, tau_sigma) where tau is true value.
# Propose estimator R and find its PDF.
c <- 3 * 10^8
tau <- 2 * R / c
estimator_r <- function(tau) {
    tau * c
}

tau <- 0.5
rnorm(tau, size = 100)
a + rnorm(100)


# ## 1.1.4

estimator_a1 <- function(x) {
    (1 / length(x)) * sum(x)
}
estimator_a2 <- function(x) {
    (1 / (length(x) + 2)) * (2 * x[1] + sum(x) + 2 * x[length(x)])
}

experiment <- function(n, a_range) {
    df <- data.frame()
    for (a in a_range) {
        x <- rnorm(n, a, 1)
        y <- estimator_a1(x)
        z <- estimator_a2(x)
        df <- rbind(df, data.frame(a = a, x = x, y = y, z = z))
    }
    df <- df %>%
        group_by(a) %>%
        summarise(
            x_mean = mean(x),
            x_sd = sd(x),
            y_mean = mean(y),
            y_sd = sd(y),
            z_mean = mean(z),
            z_sd = sd(z)
        ) %>%
        mutate(
            y_mean_err = abs(y_mean - x_mean),
            y_sd_err = abs(y_sd - x_sd),
            z_mean_err = abs(z_mean - x_mean),
            z_sd_err = abs(z_sd - x_sd)
        ) %>%
        pivot_longer(
            c("y_mean_err", "z_mean_err"),
            names_to = "mean_err_names",
            values_to = "mean_err"
        ) %>%
        pivot_longer(
            c("y_sd_err", "z_sd_err"),
            names_to = "sd_err_names",
            values_to = "sd_err"
        )
    return(df)
}

experiment(100, -100:100) %>%
    ggplot() +
    aes(x = a) +
    geom_line(aes(y = mean_err, colour = mean_err_names)) +
    geom_line(aes(y = sd_err, colour = sd_err_names)) +
    labs(x = "a", y = "Error") +
    theme_bw() +
    theme(
        # legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white")
    )



# ## 2.1.1



# ## 1.1.2
# Unknown parameter A influences the distribution
# of the random variable X. The PDF is given by:
#
# $$
# f(x) = \frac{1}{\sqrt{2\pi}} e^{-\frac{(x-A)^2}{2}}
# $$

a <- 100
x <- rnorm(1000, 100, 1)
mean_x <- mean(x)
range_x <- range(x)

paste(
    "[1.1.2] Sampling a 1000 times with an A of", a,
    "gives a mean of", mean_x, "with range",
    range_x, "which indicates that the", "investigator is correct."
)