---
title: "p8130_Final_Project"
output: html_document
---



## Import Date

```r
cdi = read_csv("cdi.csv") %>%
  janitor::clean_names() %>%
  mutate(crm_1000 = 1000*(crimes/pop))
```

```
## Rows: 440 Columns: 17
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): cty, state
## dbl (15): id, area, pop, pop18, pop65, docs, beds, crimes, hsgrad, bagrad, p...
```

```
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

# summary continuous variables

```r
cdi%>%
select(area, pop,pop18,pop65,docs,beds,crimes,hsgrad,bagrad,poverty,unemp,pcincome,totalinc,crm_1000) %>%
summary()
```

```
##       area              pop              pop18           pop65       
##  Min.   :   15.0   Min.   : 100043   Min.   :16.40   Min.   : 3.000  
##  1st Qu.:  451.2   1st Qu.: 139027   1st Qu.:26.20   1st Qu.: 9.875  
##  Median :  656.5   Median : 217280   Median :28.10   Median :11.750  
##  Mean   : 1041.4   Mean   : 393011   Mean   :28.57   Mean   :12.170  
##  3rd Qu.:  946.8   3rd Qu.: 436064   3rd Qu.:30.02   3rd Qu.:13.625  
##  Max.   :20062.0   Max.   :8863164   Max.   :49.70   Max.   :33.800  
##       docs              beds             crimes           hsgrad     
##  Min.   :   39.0   Min.   :   92.0   Min.   :   563   Min.   :46.60  
##  1st Qu.:  182.8   1st Qu.:  390.8   1st Qu.:  6220   1st Qu.:73.88  
##  Median :  401.0   Median :  755.0   Median : 11820   Median :77.70  
##  Mean   :  988.0   Mean   : 1458.6   Mean   : 27112   Mean   :77.56  
##  3rd Qu.: 1036.0   3rd Qu.: 1575.8   3rd Qu.: 26280   3rd Qu.:82.40  
##  Max.   :23677.0   Max.   :27700.0   Max.   :688936   Max.   :92.90  
##      bagrad         poverty           unemp           pcincome    
##  Min.   : 8.10   Min.   : 1.400   Min.   : 2.200   Min.   : 8899  
##  1st Qu.:15.28   1st Qu.: 5.300   1st Qu.: 5.100   1st Qu.:16118  
##  Median :19.70   Median : 7.900   Median : 6.200   Median :17759  
##  Mean   :21.08   Mean   : 8.721   Mean   : 6.597   Mean   :18561  
##  3rd Qu.:25.32   3rd Qu.:10.900   3rd Qu.: 7.500   3rd Qu.:20270  
##  Max.   :52.30   Max.   :36.300   Max.   :21.300   Max.   :37541  
##     totalinc         crm_1000      
##  Min.   :  1141   Min.   :  4.601  
##  1st Qu.:  2311   1st Qu.: 38.102  
##  Median :  3857   Median : 52.429  
##  Mean   :  7869   Mean   : 57.286  
##  3rd Qu.:  8654   3rd Qu.: 72.597  
##  Max.   :184230   Max.   :295.987
```
