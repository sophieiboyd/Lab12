Lab 12 - Smoking during pregnancy
================
Sophie Boyd
4-10-26

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
```

### Exercise 1

The numeric variables are fage, mage, weeks, visits, gained, and weight.

``` r
ncbirths %>%
  select(fage, mage, weeks, visits, gained, weight) %>%
  summarize(across(everything(), list(mean = mean, sd = sd), na.rm = TRUE))
```

    ## # A tibble: 1 × 12
    ##   fage_mean fage_sd mage_mean mage_sd weeks_mean weeks_sd visits_mean visits_sd
    ##       <dbl>   <dbl>     <dbl>   <dbl>      <dbl>    <dbl>       <dbl>     <dbl>
    ## 1      30.3    6.76        27    6.21       38.3     2.93        12.1      3.95
    ## # ℹ 4 more variables: gained_mean <dbl>, gained_sd <dbl>, weight_mean <dbl>,
    ## #   weight_sd <dbl>

``` r
ncbirths_long <- ncbirths %>%
  pivot_longer(
    cols = c("fage", "mage", "weeks", "visits", "gained", "weight"),
    names_to = "numeric_label",
    values_to = "numeric_value")

ncbirths_long %>%
  ggplot(aes(x = numeric_value)) +
  geom_boxplot() +
  facet_wrap(~numeric_label, nrow = 6) + 
  labs(x = NULL) 
```

![](lab-12_files/figure-gfm/numeric-summaries-1.png)<!-- -->

Distributions are mostly balanced, with some high outliers on the amount
of weight gained during pregnancy and low outliers on the number of
weeks of pregnancy (which I assume are labeled as premature births with
the “premie” variable).

### Exercise 2

``` r
ncbirths_white <- ncbirths %>%
  filter(whitemom %in% c("white"))

mean(ncbirths_white$weight)
```

    ## [1] 7.250462

### Exercise 3

- I believe that the observations in the dataset are independent of one
  another, assuming that each birth corresponds to a different mother.
  If multiple births from a mother with multiple children were featured
  in the dataset, this would violate the assumption of independence. I
  would need more information to verify.

- ncbirths_white has 714 observations, which I believe would be a
  reasonably large sample size for bootstrapping.

- In the boxplots from Exercise 1, I do not see any extreme skew or
  clustering that would cause problems. (\*consider returning to this
  one and removing low weight outliers)

### Exercise 4a

``` r
set.seed(123)

boot_df <- ncbirths_white %>%
  specify(response = weight) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
```

### Exercise 4b

``` r
mean(boot_df$stat)
```

    ## [1] 7.251568

``` r
boot_df <- boot_df %>%
  mutate(stat_cent1 = stat - 7.25) %>%
  mutate(stat_cent = stat_cent1 + 7.43)
```

### Exercise 4c

``` r
boot_df %>%
  ggplot(aes(x = stat_cent)) +
  geom_histogram() +
  geom_vline(aes(xintercept = 7.25),
             color = "blue",
             linetype = "dashed",
             linewidth = 1)
```

![](lab-12_files/figure-gfm/sim-histogram-1.png)<!-- -->

### Exercise 4d

``` r
boot_df %>%
  summarize(lower = quantile(stat_cent, 0.025),
            upper = quantile(stat_cent, 0.975))
```

    ## # A tibble: 1 × 2
    ##   lower upper
    ##   <dbl> <dbl>
    ## 1  7.34  7.54

The mean of the simulated means fell outside of the 95% confidence
interval, meaning that the simulated mean (7.25) was significantly lower
than the mean weight set as the population value (7.43)

### Exercise 4e

Based on my results, it seems that birth weight has decreased
significantly since 2004.
