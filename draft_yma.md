draft\_yma
================
Anna Ma
12/12/2021

## Data exploration

**converted variables to per person and docs, beds, etc.**

``` r
cdi = read_csv("cdi.csv") %>%
  janitor::clean_names() %>%
  mutate(crm_1000 = 1000*(crimes/pop),
         pdocs_1000 = 1000*(docs/pop),
         pbeds_1000 = 1000*(beds/pop),
         density_pop = pop/area,
         # convert region to factors and recoded them accordingly 
         region = factor(region, levels = 1:4,
                    labels = c("northeast", "north central", "south", "west"))) %>% select(-c(docs,beds)) %>% view()
```

### Descriptive statistics of all varaibles

``` r
cdi_descriptive = cdi %>% select(-c(id,cty,state,region))

# Global
skimr::skim(cdi_descriptive) %>% 
  select(-c("skim_type","complete_rate")) %>% 
    mutate(skim_variable = 
             recode(skim_variable, pcincome = "pcincome (in dollars)", totalinc = "totalinc (in million of dollars)"
  )) %>% 
  knitr::kable(
    col.names = c("variable", "n_missing", "mean","sd","min","Q25","median","Q75","max","histogram"),
    caption = "Global Summary", digits = 4)
```

| variable                         | n\_missing |        mean |          sd |         min |         Q25 |      median |         Q75 |          max | histogram |
|:---------------------------------|-----------:|------------:|------------:|------------:|------------:|------------:|------------:|-------------:|:----------|
| area                             |          0 |   1041.4114 |   1549.9221 |     15.0000 |    451.2500 |    656.5000 |    946.7500 |   20062.0000 | ▇▁▁▁▁     |
| pop                              |          0 | 393010.9205 | 601987.0165 | 100043.0000 | 139027.2500 | 217280.5000 | 436064.5000 | 8863164.0000 | ▇▁▁▁▁     |
| pop18                            |          0 |     28.5684 |      4.1911 |     16.4000 |     26.2000 |     28.1000 |     30.0250 |      49.7000 | ▁▇▃▁▁     |
| pop65                            |          0 |     12.1698 |      3.9927 |      3.0000 |      9.8750 |     11.7500 |     13.6250 |      33.8000 | ▂▇▁▁▁     |
| crimes                           |          0 |  27111.6182 |  58237.5064 |    563.0000 |   6219.5000 |  11820.5000 |  26279.5000 |  688936.0000 | ▇▁▁▁▁     |
| hsgrad                           |          0 |     77.5607 |      7.0152 |     46.6000 |     73.8750 |     77.7000 |     82.4000 |      92.9000 | ▁▁▃▇▃     |
| bagrad                           |          0 |     21.0811 |      7.6545 |      8.1000 |     15.2750 |     19.7000 |     25.3250 |      52.3000 | ▆▇▃▁▁     |
| poverty                          |          0 |      8.7207 |      4.6567 |      1.4000 |      5.3000 |      7.9000 |     10.9000 |      36.3000 | ▇▆▁▁▁     |
| unemp                            |          0 |      6.5966 |      2.3379 |      2.2000 |      5.1000 |      6.2000 |      7.5000 |      21.3000 | ▇▇▁▁▁     |
| pcincome (in dollars)            |          0 |  18561.4818 |   4059.1920 |   8899.0000 |  16118.2500 |  17759.0000 |  20270.0000 |   37541.0000 | ▁▇▂▁▁     |
| totalinc (in million of dollars) |          0 |   7869.2727 |  12884.3215 |   1141.0000 |   2311.0000 |   3857.0000 |   8654.2500 |  184230.0000 | ▇▁▁▁▁     |
| crm\_1000                        |          0 |     57.2864 |     27.3277 |      4.6014 |     38.1019 |     52.4286 |     72.5969 |     295.9867 | ▇▅▁▁▁     |
| pdocs\_1000                      |          0 |      2.1230 |      1.5329 |      0.3559 |      1.2127 |      1.7509 |      2.4915 |      17.0377 | ▇▁▁▁▁     |
| pbeds\_1000                      |          0 |      3.6493 |      2.0011 |      0.1649 |      2.1972 |      3.3287 |      4.5649 |      19.6982 | ▇▃▁▁▁     |
| density\_pop                     |          0 |    888.4388 |   2194.7231 |     13.2587 |    192.3449 |    335.9081 |    756.5516 |   32403.7183 | ▇▁▁▁▁     |

Global Summary

Q: 1. do we need to group them by state or county? group by gounty gives
5000+ rows though… 2. do we need box plot still? 3. again, transfer some
variables to “per pop” / “per 1000 pop” ?

### Descriptive analysis

We can use the box plot/ or histogram to check for normality. But I
forgot when do we need normality…isn’t it for residual??

``` r
par(mfrow = c(3,5))

boxplot(cdi$density_pop,  main = "density_pop")
boxplot(cdi$area, main = "area")

boxplot(cdi$pop, main = "pop")
boxplot(cdi$pop18, main = "pop18")
boxplot(cdi$pop65, main = "pop65")

boxplot(cdi$pdocs_1000, main = "pdocs_1000")
boxplot(cdi$pbeds_1000, main = "pbeds_1000")

boxplot(cdi$crm_1000,  main = "crm_1000")

boxplot(cdi$hsgrad, main = "hsgrad")
boxplot(cdi$bagrad, main = "bagrad")
boxplot(cdi$poverty, main = "poverty")
boxplot(cdi$unemp,main = "unemp")
boxplot(cdi$pcincome, main = "pcincome")
boxplot(cdi$totalinc, main = "totalinc")
```

![](draft_yma_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Correlation

#### Pairwise relationship

-   This gives us an idea of the correlation between each variable, but
    my old project build whole model first then assessed the
    correlation. Need Discussion!

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
cor(cdi_descriptive) %>% knitr::kable()
```

|              |       area |        pop |      pop18 |      pop65 |     crimes |     hsgrad |     bagrad |    poverty |      unemp |   pcincome |   totalinc |  crm\_1000 | pdocs\_1000 | pbeds\_1000 | density\_pop |
|:-------------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|------------:|------------:|-------------:|
| area         |  1.0000000 |  0.1730834 | -0.0548781 |  0.0057709 |  0.1294754 | -0.0985981 | -0.1372377 |  0.1713433 |  0.1992093 | -0.1877151 |  0.1270743 |  0.0429484 |  -0.1163860 |  -0.1412335 |   -0.1568156 |
| pop          |  0.1730834 |  1.0000000 |  0.0783721 | -0.0290374 |  0.8863318 | -0.0174269 |  0.1468138 |  0.0380195 |  0.0053517 |  0.2356102 |  0.9867476 |  0.2800992 |   0.1668595 |   0.0203012 |    0.3220266 |
| pop18        | -0.0548781 |  0.0783721 |  1.0000000 | -0.6163096 |  0.0899406 |  0.2505843 |  0.4560970 |  0.0339755 | -0.2785271 | -0.0316484 |  0.0711615 |  0.1905688 |   0.2370280 |   0.0295244 |    0.1254644 |
| pop65        |  0.0057709 | -0.0290374 | -0.6163096 |  1.0000000 | -0.0352903 | -0.2682518 | -0.3392288 |  0.0065785 |  0.2363094 |  0.0185907 | -0.0227332 | -0.0665333 |   0.0186087 |   0.2471479 |    0.0291845 |
| crimes       |  0.1294754 |  0.8863318 |  0.0899406 | -0.0352903 |  1.0000000 | -0.1063284 |  0.0770765 |  0.1644057 |  0.0435568 |  0.1175391 |  0.8430980 |  0.5300430 |   0.1577103 |   0.0778907 |    0.5609842 |
| hsgrad       | -0.0985981 | -0.0174269 |  0.2505843 | -0.2682518 | -0.1063284 |  1.0000000 |  0.7077867 | -0.6917505 | -0.5935958 |  0.5229961 |  0.0433557 | -0.2264129 |   0.1427765 |  -0.2111625 |   -0.1040070 |
| bagrad       | -0.1372377 |  0.1468138 |  0.4560970 | -0.3392288 |  0.0770765 |  0.7077867 |  1.0000000 | -0.4084238 | -0.5409069 |  0.6953619 |  0.2222301 |  0.0383046 |   0.4410463 |  -0.0454183 |    0.1556063 |
| poverty      |  0.1713433 |  0.0380195 |  0.0339755 |  0.0065785 |  0.1644057 | -0.6917505 | -0.4084238 |  1.0000000 |  0.4369472 | -0.6017250 | -0.0387393 |  0.4718442 |   0.0637048 |   0.3713989 |    0.1265079 |
| unemp        |  0.1992093 |  0.0053517 | -0.2785271 |  0.2363094 |  0.0435568 | -0.5935958 | -0.5409069 |  0.4369472 |  1.0000000 | -0.3221444 | -0.0338763 |  0.0418466 |  -0.2478866 |  -0.0624878 |    0.0227179 |
| pcincome     | -0.1877151 |  0.2356102 | -0.0316484 |  0.0185907 |  0.1175391 |  0.5229961 |  0.6953619 | -0.6017250 | -0.3221444 |  1.0000000 |  0.3476816 | -0.0802442 |   0.3600458 |  -0.0535500 |    0.2332260 |
| totalinc     |  0.1270743 |  0.9867476 |  0.0711615 | -0.0227332 |  0.8430980 |  0.0433557 |  0.2222301 | -0.0387393 | -0.0338763 |  0.3476816 |  1.0000000 |  0.2281557 |   0.1991038 |   0.0063239 |    0.3162048 |
| crm\_1000    |  0.0429484 |  0.2800992 |  0.1905688 | -0.0665333 |  0.5300430 | -0.2264129 |  0.0383046 |  0.4718442 |  0.0418466 | -0.0802442 |  0.2281557 |  1.0000000 |   0.3070831 |   0.3644505 |    0.4804285 |
| pdocs\_1000  | -0.1163860 |  0.1668595 |  0.2370280 |  0.0186087 |  0.1577103 |  0.1427765 |  0.4410463 |  0.0637048 | -0.2478866 |  0.3600458 |  0.1991038 |  0.3070831 |   1.0000000 |   0.6666947 |    0.3180424 |
| pbeds\_1000  | -0.1412335 |  0.0203012 |  0.0295244 |  0.2471479 |  0.0778907 | -0.2111625 | -0.0454183 |  0.3713989 | -0.0624878 | -0.0535500 |  0.0063239 |  0.3644505 |   0.6666947 |   1.0000000 |    0.2064177 |
| density\_pop | -0.1568156 |  0.3220266 |  0.1254644 |  0.0291845 |  0.5609842 | -0.1040070 |  0.1556063 |  0.1265079 |  0.0227179 |  0.2332260 |  0.3162048 |  0.4804285 |   0.3180424 |   0.2064177 |    1.0000000 |

``` r
library(ggcorrplot)
library(ggstatsplot)
```

    ## You can cite this package as:
    ##      Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot' approach.
    ##      Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

``` r
ggstatsplot::ggcorrmat(
  data = cdi,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)
```

![](draft_yma_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

-   Here is the correlation with pairs function. More specific than the
    heat map above. We can observe the correlation between all the terms
    here

``` r
pairs(crm_1000 ~.,data=cdi_descriptive, panel = panel.smooth, upper.panel = NULL, main = "Scatterplot Matrix")
```

![](draft_yma_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

#### Marginal distribution ?

``` r
library(ggplot2)
library(ggExtra)
```

``` r
marg_den = cdi %>% ggplot(aes(x = density_pop, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
ggMarginal(marg_den, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
marg_area = cdi %>% ggplot(aes(x = area, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
ggMarginal(marg_area, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
marg_pop = cdi %>% ggplot(aes(x = pop, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
ggMarginal(marg_pop, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
marg_pop18 = cdi %>% ggplot(aes(x = pop18, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
# positive correlation
ggMarginal(marg_pop18, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
marg_pop65 = cdi %>% ggplot(aes(x = pop65, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
ggMarginal(marg_pop65, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
marg_pdocs_1000 = cdi %>% ggplot(aes(x = pdocs_1000, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
ggMarginal(marg_pdocs_1000, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
marg_pbeds_1000 = cdi %>% ggplot(aes(x = pbeds_1000, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
ggMarginal(marg_pbeds_1000, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
marg_hsgrad = cdi %>% ggplot(aes(x = hsgrad, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red') #negative correlation
ggMarginal(marg_hsgrad, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
marg_bagrad = cdi %>% ggplot(aes(x = bagrad, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
ggMarginal(marg_bagrad, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
marg_poverty = cdi %>% ggplot(aes(x = poverty, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red') # positive correlation
ggMarginal(marg_poverty, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
marg_unemp = cdi %>% ggplot(aes(x = unemp, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
ggMarginal(marg_unemp, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
marg_pcincome = cdi %>% ggplot(aes(x = pcincome, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
ggMarginal(marg_pcincome, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
marg_totalinc = cdi %>% ggplot(aes(x = totalinc, y = crm_1000)) + geom_point(alpha = 0.3) + geom_smooth(method = 'lm', se = TRUE, color = 'red')
ggMarginal(marg_totalinc, type = "histogram", fill="transparent")
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](draft_yma_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### Distribution of outcome

``` r
cdi %>% 
  ggplot(aes(x = crm_1000)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](draft_yma_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
# log transfor the outcome
cdi %>% 
  ggplot(aes(x = log(crm_1000))) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](draft_yma_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

do we look at the distribution of outcome like this and transform them
here? check again

### States with unusual rates

group by state

``` r
cdi_by_state = cdi %>% group_by(state) %>% 
  summarise(state_pop = sum(pop),
            state_crimes = sum(crimes)) %>% 
  mutate(state_CRM_1000 = 1000*state_crimes/state_pop)
```

Not sure what counts as out lier, this is what the other group did.

``` r
upper = quantile(cdi$crm_1000, 0.75)
lower = quantile(cdi$crm_1000, 0.25)
IQR = upper - lower
cdi %>% 
  filter(crm_1000 > upper + 1.5*IQR,
         crm_1000 > lower - 1.5*IQR) %>% 
  dplyr::select(cty, crm_1000) %>%
  knitr::kable(digits = 2)
```

| cty       | crm\_1000 |
|:----------|----------:|
| Kings     |    295.99 |
| Dade      |    126.34 |
| Fulton    |    143.35 |
| St.\_Loui |    161.60 |

This shows no outlier on the state level tho…

``` r
upper = quantile(cdi_by_state$state_CRM_1000, 0.75)
lower = quantile(cdi_by_state$state_CRM_1000, 0.25)
IQR = upper - lower
cdi_by_state %>% 
  filter(state_CRM_1000 > upper + 1.5*IQR,
         state_CRM_1000 > lower - 1.5*IQR) %>% 
  dplyr::select(state, state_CRM_1000) %>%
  knitr::kable(digits = 2)
```

| state | state\_CRM\_1000 |
|:------|-----------------:|

``` r
boxplot(cdi_by_state$state_CRM_1000, main = 'State Crime Rate per 1000 people')
```

![](draft_yma_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

**we probably need to plot region as well? not sure**

## Model!

**Q: do we need more descriptive analysis, visualization? rate of crime
for each state?** **there’s a few duplicated cty, but different area,
maybe diff place then? not sure** they can’t be the same… otherwise we
have to recalculate some stuff… duplicated(cdi\[,2:3\]) cdi %&gt;%
filter(cty == “Baltimor”) cdi %&gt;% filter(cty == “St.\_Loui”)

#### Full model predictors ok?

included pop in addition to pop18, pop65. Do we need pop tho? Would
pop18 and pop65 be enough already?

did not include cty, state, area, crimes, totalinc, totalinc (included
pcincome), do we need to include any of those? for example, both
totalinc and pcincome, area and pop\_den

this model used `northeast` as the reference level for region

``` r
cdi_model = cdi %>% select(-c(id,cty,state,area,crimes,totalinc))

# use 
full_fit = lm(crm_1000 ~ ., data = cdi_model)
summary(full_fit)
```

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ ., data = cdi_model)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -47.786 -11.422  -0.934  10.200  75.180 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         -6.922e+01  2.739e+01  -2.528 0.011849 *  
    ## pop                  5.486e-06  1.579e-06   3.474 0.000566 ***
    ## pop18                6.947e-01  3.305e-01   2.102 0.036150 *  
    ## pop65               -1.998e-01  3.055e-01  -0.654 0.513410    
    ## hsgrad               6.143e-01  2.690e-01   2.284 0.022864 *  
    ## bagrad              -4.835e-01  2.971e-01  -1.628 0.104327    
    ## poverty              1.856e+00  3.864e-01   4.803 2.17e-06 ***
    ## unemp                6.111e-01  5.314e-01   1.150 0.250812    
    ## pcincome             1.039e-03  4.734e-04   2.195 0.028670 *  
    ## regionnorth central  8.978e+00  2.732e+00   3.286 0.001100 ** 
    ## regionsouth          2.779e+01  2.659e+00  10.453  < 2e-16 ***
    ## regionwest           2.118e+01  3.125e+00   6.778 4.09e-11 ***
    ## pdocs_1000          -6.634e-01  1.019e+00  -0.651 0.515556    
    ## pbeds_1000           3.157e+00  7.939e-01   3.977 8.21e-05 ***
    ## density_pop          4.901e-03  4.537e-04  10.802  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.81 on 425 degrees of freedom
    ## Multiple R-squared:  0.589,  Adjusted R-squared:  0.5755 
    ## F-statistic: 43.51 on 14 and 425 DF,  p-value: < 2.2e-16

#### Backward

``` r
fit_back = step(full_fit, direction='backward')
```

    ## Start:  AIC=2548.68
    ## crm_1000 ~ pop + pop18 + pop65 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + region + pdocs_1000 + pbeds_1000 + density_pop
    ## 
    ##               Df Sum of Sq    RSS    AIC
    ## - pdocs_1000   1       134 134867 2547.1
    ## - pop65        1       136 134869 2547.1
    ## - unemp        1       419 135152 2548.1
    ## <none>                     134733 2548.7
    ## - bagrad       1       840 135573 2549.4
    ## - pop18        1      1401 136134 2551.2
    ## - pcincome     1      1528 136261 2551.6
    ## - hsgrad       1      1654 136387 2552.1
    ## - pop          1      3825 138558 2559.0
    ## - pbeds_1000   1      5013 139747 2562.8
    ## - poverty      1      7313 142046 2569.9
    ## - density_pop  1     36989 171723 2653.4
    ## - region       3     39099 173832 2654.8
    ## 
    ## Step:  AIC=2547.12
    ## crm_1000 ~ pop + pop18 + pop65 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + region + pbeds_1000 + density_pop
    ## 
    ##               Df Sum of Sq    RSS    AIC
    ## - pop65        1       130 134998 2545.6
    ## - unemp        1       410 135278 2546.5
    ## <none>                     134867 2547.1
    ## - bagrad       1      1080 135948 2548.6
    ## - pop18        1      1338 136206 2549.5
    ## - pcincome     1      1438 136305 2549.8
    ## - hsgrad       1      1744 136611 2550.8
    ## - pop          1      3789 138656 2557.3
    ## - poverty      1      7414 142281 2568.7
    ## - pbeds_1000   1      7715 142583 2569.6
    ## - density_pop  1     36918 171785 2651.6
    ## - region       3     38974 173841 2652.8
    ## 
    ## Step:  AIC=2545.55
    ## crm_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + unemp + 
    ##     pcincome + region + pbeds_1000 + density_pop
    ## 
    ##               Df Sum of Sq    RSS    AIC
    ## - unemp        1       360 135358 2544.7
    ## <none>                     134998 2545.6
    ## - bagrad       1      1079 136077 2547.1
    ## - pcincome     1      1505 136503 2548.4
    ## - hsgrad       1      1806 136804 2549.4
    ## - pop18        1      2397 137395 2551.3
    ## - pop          1      3746 138744 2555.6
    ## - poverty      1      8138 143136 2569.3
    ## - pbeds_1000   1      8151 143149 2569.3
    ## - density_pop  1     36790 171787 2649.6
    ## - region       3     39013 174011 2651.2
    ## 
    ## Step:  AIC=2544.72
    ## crm_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + pcincome + 
    ##     region + pbeds_1000 + density_pop
    ## 
    ##               Df Sum of Sq    RSS    AIC
    ## <none>                     135358 2544.7
    ## - bagrad       1      1402 136760 2547.2
    ## - hsgrad       1      1571 136929 2547.8
    ## - pcincome     1      1834 137192 2548.6
    ## - pop18        1      2389 137748 2550.4
    ## - pop          1      3652 139010 2554.4
    ## - pbeds_1000   1      7843 143201 2567.5
    ## - poverty      1     10154 145512 2574.6
    ## - density_pop  1     36580 171938 2648.0
    ## - region       3     40192 175550 2653.1

``` r
fit_back
```

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + 
    ##     pcincome + region + pbeds_1000 + density_pop, data = cdi_model)
    ## 
    ## Coefficients:
    ##         (Intercept)                  pop                pop18  
    ##          -6.700e+01            5.350e-06            7.817e-01  
    ##              hsgrad               bagrad              poverty  
    ##           5.852e-01           -5.929e-01            2.039e+00  
    ##            pcincome  regionnorth central          regionsouth  
    ##           1.109e-03            9.017e+00            2.703e+01  
    ##          regionwest           pbeds_1000          density_pop  
    ##           2.083e+01            2.497e+00            4.834e-03

crm\_1000 \~ pop + pop18 + hsgrad + bagrad + poverty + pcincome + region
+ pbeds\_1000 + density\_pop, data = cdi\_model

#### forward

``` r
fit_forward = step(full_fit, direction='forward')
```

    ## Start:  AIC=2548.68
    ## crm_1000 ~ pop + pop18 + pop65 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + region + pdocs_1000 + pbeds_1000 + density_pop

``` r
fit_forward
```

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop + pop18 + pop65 + hsgrad + bagrad + 
    ##     poverty + unemp + pcincome + region + pdocs_1000 + pbeds_1000 + 
    ##     density_pop, data = cdi_model)
    ## 
    ## Coefficients:
    ##         (Intercept)                  pop                pop18  
    ##          -6.922e+01            5.486e-06            6.947e-01  
    ##               pop65               hsgrad               bagrad  
    ##          -1.998e-01            6.143e-01           -4.835e-01  
    ##             poverty                unemp             pcincome  
    ##           1.856e+00            6.111e-01            1.039e-03  
    ## regionnorth central          regionsouth           regionwest  
    ##           8.978e+00            2.779e+01            2.118e+01  
    ##          pdocs_1000           pbeds_1000          density_pop  
    ##          -6.634e-01            3.157e+00            4.901e-03

crm\_1000 \~ pop + pop18 + pop65 + hsgrad + bagrad + poverty + unemp +
pcincome + region + pdocs\_1000 + pbeds\_1000 + density\_pop, data =
cdi\_model

#### both

step-wise?

``` r
fit_both = step(full_fit, direction='both')
```

    ## Start:  AIC=2548.68
    ## crm_1000 ~ pop + pop18 + pop65 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + region + pdocs_1000 + pbeds_1000 + density_pop
    ## 
    ##               Df Sum of Sq    RSS    AIC
    ## - pdocs_1000   1       134 134867 2547.1
    ## - pop65        1       136 134869 2547.1
    ## - unemp        1       419 135152 2548.1
    ## <none>                     134733 2548.7
    ## - bagrad       1       840 135573 2549.4
    ## - pop18        1      1401 136134 2551.2
    ## - pcincome     1      1528 136261 2551.6
    ## - hsgrad       1      1654 136387 2552.1
    ## - pop          1      3825 138558 2559.0
    ## - pbeds_1000   1      5013 139747 2562.8
    ## - poverty      1      7313 142046 2569.9
    ## - density_pop  1     36989 171723 2653.4
    ## - region       3     39099 173832 2654.8
    ## 
    ## Step:  AIC=2547.12
    ## crm_1000 ~ pop + pop18 + pop65 + hsgrad + bagrad + poverty + 
    ##     unemp + pcincome + region + pbeds_1000 + density_pop
    ## 
    ##               Df Sum of Sq    RSS    AIC
    ## - pop65        1       130 134998 2545.6
    ## - unemp        1       410 135278 2546.5
    ## <none>                     134867 2547.1
    ## - bagrad       1      1080 135948 2548.6
    ## + pdocs_1000   1       134 134733 2548.7
    ## - pop18        1      1338 136206 2549.5
    ## - pcincome     1      1438 136305 2549.8
    ## - hsgrad       1      1744 136611 2550.8
    ## - pop          1      3789 138656 2557.3
    ## - poverty      1      7414 142281 2568.7
    ## - pbeds_1000   1      7715 142583 2569.6
    ## - density_pop  1     36918 171785 2651.6
    ## - region       3     38974 173841 2652.8
    ## 
    ## Step:  AIC=2545.55
    ## crm_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + unemp + 
    ##     pcincome + region + pbeds_1000 + density_pop
    ## 
    ##               Df Sum of Sq    RSS    AIC
    ## - unemp        1       360 135358 2544.7
    ## <none>                     134998 2545.6
    ## - bagrad       1      1079 136077 2547.1
    ## + pop65        1       130 134867 2547.1
    ## + pdocs_1000   1       129 134869 2547.1
    ## - pcincome     1      1505 136503 2548.4
    ## - hsgrad       1      1806 136804 2549.4
    ## - pop18        1      2397 137395 2551.3
    ## - pop          1      3746 138744 2555.6
    ## - poverty      1      8138 143136 2569.3
    ## - pbeds_1000   1      8151 143149 2569.3
    ## - density_pop  1     36790 171787 2649.6
    ## - region       3     39013 174011 2651.2
    ## 
    ## Step:  AIC=2544.72
    ## crm_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + pcincome + 
    ##     region + pbeds_1000 + density_pop
    ## 
    ##               Df Sum of Sq    RSS    AIC
    ## <none>                     135358 2544.7
    ## + unemp        1       360 134998 2545.6
    ## + pdocs_1000   1       122 135236 2546.3
    ## + pop65        1        80 135278 2546.5
    ## - bagrad       1      1402 136760 2547.2
    ## - hsgrad       1      1571 136929 2547.8
    ## - pcincome     1      1834 137192 2548.6
    ## - pop18        1      2389 137748 2550.4
    ## - pop          1      3652 139010 2554.4
    ## - pbeds_1000   1      7843 143201 2567.5
    ## - poverty      1     10154 145512 2574.6
    ## - density_pop  1     36580 171938 2648.0
    ## - region       3     40192 175550 2653.1

``` r
fit_both
```

    ## 
    ## Call:
    ## lm(formula = crm_1000 ~ pop + pop18 + hsgrad + bagrad + poverty + 
    ##     pcincome + region + pbeds_1000 + density_pop, data = cdi_model)
    ## 
    ## Coefficients:
    ##         (Intercept)                  pop                pop18  
    ##          -6.700e+01            5.350e-06            7.817e-01  
    ##              hsgrad               bagrad              poverty  
    ##           5.852e-01           -5.929e-01            2.039e+00  
    ##            pcincome  regionnorth central          regionsouth  
    ##           1.109e-03            9.017e+00            2.703e+01  
    ##          regionwest           pbeds_1000          density_pop  
    ##           2.083e+01            2.497e+00            4.834e-03

crm\_1000 \~ pop + pop18 + hsgrad + bagrad + poverty + pcincome + region
+ pbeds\_1000 + density\_pop, data = cdi\_model This gives back the same
model as back

Some group forward again based on their first time step wise…not sure if
we need to do that tho

### Test based procedures

#### Cp

I didn’t include the full model here

**backward & both**

``` r
# printing the 2 best models of each size. For example, the first two lines: print the best 2 models that have 2 variables (including intercept)
library(leaps)
Cp_b = leaps(x = model.matrix(fit_back)[,-1],
      y = cdi_model$crm_1000,
      nbest = 2,
      method = "Cp")
Cp_b
```

    ## $which
    ##        1     2     3     4     5     6     7     8     9     A     B
    ## 1  FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
    ## 1  FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE
    ## 2  FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE
    ## 3  FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE
    ## 3  FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE
    ## 4  FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 4  FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE
    ## 5  FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 5  FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE
    ## 6  FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 6   TRUE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 7   TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 7   TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 8   TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 8   TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 9   TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 9   TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 10  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 10  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 
    ## $label
    ##  [1] "(Intercept)" "1"           "2"           "3"           "4"          
    ##  [6] "5"           "6"           "7"           "8"           "9"          
    ## [11] "A"           "B"          
    ## 
    ## $size
    ##  [1]  2  2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 11 12
    ## 
    ## $Cp
    ##  [1] 361.37662 369.85075 185.35978 217.86627 115.16570 160.73877  83.84967
    ##  [8]  84.08696  49.18224  63.36721  33.69922  34.29891  19.17558  24.91300
    ## [15]  14.72619  14.79923  13.80739  14.07314  14.43365  14.96836  12.00000

``` r
Cp_b %>% faraway::Cpplot()
```

![](draft_yma_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

size is the number of parameters in the model, Cp is Cp

The plot shows the model that satisfy the criteria, number indicates the
number of parameters it included. in this case, the 1st to the last.

**forward**

``` r
leaps(x = model.matrix(fit_forward)[,-1],
      y = cdi_model$crm_1000,
      nbest = 2,
      method = "Cp") %>% faraway::Cpplot()
```

![](draft_yma_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

So, multiple model satisfies the Cp criteria here.

#### Adj R2

**backward**

``` r
adjr_b = leaps(x = model.matrix(fit_back)[,-1],
      y = cdi_model$crm_1000,
      nbest = 2,
      method = "adjr2") 

adjr_b
```

    ## $which
    ##        1     2     3     4     5     6     7     8     9     A     B
    ## 1  FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
    ## 1  FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE
    ## 2  FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE
    ## 3  FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE
    ## 3  FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE
    ## 4  FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 4  FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE
    ## 5  FALSE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 5  FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE
    ## 6  FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 6   TRUE FALSE FALSE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 7   TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 7   TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 8   TRUE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 8   TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 9   TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 9   TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 10  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 10  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 11  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 
    ## $label
    ##  [1] "(Intercept)" "1"           "2"           "3"           "4"          
    ##  [6] "5"           "6"           "7"           "8"           "9"          
    ## [11] "A"           "B"          
    ## 
    ## $size
    ##  [1]  2  2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 11 12
    ## 
    ## $adjr2
    ##  [1] 0.2290554 0.2208622 0.3998009 0.3683000 0.4685454 0.4242809 0.4997575
    ##  [8] 0.4995265 0.5343835 0.5205424 0.5504069 0.5498204 0.5655639 0.5599397
    ## [15] 0.5708928 0.5708211 0.5727695 0.5725077 0.5731297 0.5726018 0.5765191

``` r
adjr_b%>% 
  faraway::maxadjr(10)
```

    ## 1,2,3,4,5,6,7,8,9,10,11   1,2,3,5,6,7,8,9,10,11     1,2,5,6,7,8,9,10,11 
    ##                   0.577                   0.573                   0.573 
    ##   1,2,4,5,6,7,8,9,10,11     1,2,3,5,7,8,9,10,11       1,2,5,7,8,9,10,11 
    ##                   0.573                   0.573                   0.571 
    ##       1,3,5,7,8,9,10,11         1,5,7,8,9,10,11         1,3,5,8,9,10,11 
    ##                   0.571                   0.566                   0.560 
    ##           5,7,8,9,10,11 
    ##                   0.550

**forward**

``` r
leaps(x = model.matrix(fit_forward)[,-1],
      y = cdi_model$crm_1000,
      nbest = 2,
      method = "adjr2") %>% faraway::maxadjr(10)
```

    ##      1,2,4,5,6,7,8,9,10,11,13,14        1,2,4,5,6,8,9,10,11,13,14 
    ##                            0.577                            0.577 
    ##    1,2,3,4,5,6,7,8,9,10,11,13,14   1,2,4,5,6,7,8,9,10,11,12,13,14 
    ##                            0.576                            0.576 
    ##     1,2,4,5,6,8,9,10,11,12,13,14 1,2,3,4,5,6,7,8,9,10,11,12,13,14 
    ##                            0.576                            0.575 
    ##        1,2,4,6,7,8,9,10,11,13,14          1,2,4,6,7,9,10,11,13,14 
    ##                            0.574                            0.574 
    ##          1,3,4,6,7,9,10,11,13,14            1,2,6,8,9,10,11,13,14 
    ##                            0.573                            0.573
