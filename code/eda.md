participant
================
Kaki
2018年5月29日

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(dplyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
```

``` r
participant <-read_csv("data/2013-2017 Bike MS Participants.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   `Fiscal Year` = col_integer(),
    ##   `Team ID` = col_integer(),
    ##   `Contact ID` = col_integer(),
    ##   `Participant Accept Email` = col_logical(),
    ##   `Is Team Captain` = col_logical(),
    ##   `Is Secondary Registration` = col_logical(),
    ##   `Emails Sent` = col_integer(),
    ##   `Total of All Confirmed Gifts($)` = col_double(),
    ##   `Total From Participant($)` = col_double(),
    ##   `Total Not From Participant($)` = col_double(),
    ##   `Number From Participant` = col_integer(),
    ##   `Number Not From Participant` = col_integer(),
    ##   `Address -  Participant ZIP/Postal Code` = col_integer(),
    ##   `Event ID` = col_integer(),
    ##   `Participant Goal($)` = col_integer(),
    ##   `Suggested Participant Goal($)` = col_integer()
    ## )

    ## See spec(...) for full column specifications.

    ## Warning in rbind(names(probs), probs_f): number of columns of result is not
    ## a multiple of vector length (arg 1)

    ## Warning: 2839 parsing failures.
    ## row # A tibble: 5 x 5 col     row                                    col   expected  actual expected   <int>                                  <chr>      <chr>   <chr> actual 1  1663 Address -  Participant ZIP/Postal Code an integer Mk168LP file 2  2347 Address -  Participant ZIP/Postal Code an integer Mk168LP row 3  2361 Address -  Participant ZIP/Postal Code an integer  NN15LT col 4  3537 Address -  Participant ZIP/Postal Code an integer  T2T6H2 expected 5  3546 Address -  Participant ZIP/Postal Code an integer  T2S0S7 actual # ... with 1 more variables: file <chr>
    ## ... ................. ... ................................................................. ........ ................................................................. ...... ................................................................. .... ................................................................. ... ................................................................. ... ................................................................. ........ ................................................................. ...... .......................................
    ## See problems(...) for more details.

``` r
event <-read_csv("data/2013-2017 Bike Events.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   `Security Category Name` = col_character(),
    ##   `Public Event Name` = col_character(),
    ##   `Event Created Date` = col_character(),
    ##   `Event Date` = col_character(),
    ##   `Total Fees Paid` = col_double(),
    ##   `Total of All Confirmed Gifts($)` = col_double(),
    ##   `Total Online Gifts($)` = col_double(),
    ##   `Total Offline Confirmed Gifts($)` = col_double(),
    ##   `Total From Participant($)` = col_double(),
    ##   `Total Not From Participant($)` = col_double(),
    ##   `Total Team Gifts($)` = col_double(),
    ##   `Total Event Gifts($)` = col_double(),
    ##   `Total Offline Unconfirmed Gifts($)` = col_double(),
    ##   `Street Address` = col_character(),
    ##   City = col_character(),
    ##   State = col_character(),
    ##   `Internal Event Name` = col_character()
    ## )

    ## See spec(...) for full column specifications.

What occupations were responsible for most of our fundraising?
==============================================================

``` r
participant%>%
  filter(!is.na(`Participant Occupation`))%>%
  group_by(`Fiscal Year`,`Participant Occupation`)%>%
  summarise(ttl_dona = sum(`Total of All Confirmed Gifts($)`))%>%
  filter(rank(desc(ttl_dona))<=10)%>%
  ggplot()+
  geom_col(aes(x = `Fiscal Year`, y= ttl_dona , fill =`Participant Occupation` ), position = "dodge")+
  theme(legend.position="bottom", axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette="Set3")
```

![](eda_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)

What industries have had the strongest involvement in Bike MS in the last five years?
=====================================================================================

record count
------------

``` r
participant%>%
  filter(!is.na(`Participant Occupation`))%>%
  group_by(`Fiscal Year`,`Participant Occupation`)%>%
  summarise(count_team = n())%>%
  filter(rank(desc(count_team))<=10)%>%
  ggplot()+
  geom_col(aes(x = `Fiscal Year`, count_team , fill =`Participant Occupation` ), position = "dodge")+
  theme(legend.position="bottom", axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette="Set3")
```

![](eda_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)

event\_count
------------

``` r
participant%>%
  filter(!is.na(`Participant Occupation`))%>%
  group_by(`Fiscal Year`,`Participant Occupation`)%>%
  summarise(count_event = length(unique(`Event ID`)))%>%
  filter(rank(desc(count_event))<=8)%>%
  ggplot()+
  geom_col(aes(x = `Fiscal Year`, count_event , fill =`Participant Occupation` ), position = "dodge")+
  theme(legend.position="bottom", axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette="Set3")
```

![](eda_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-6-1.png)
