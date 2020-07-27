Serious Incidents in Philadelphia Schools during SY18-19
================
Jin Chen
7/22/2020

I first begin by loading tidyverse and readxl, the two packages we will
be using. I then define the url for the data source as url and the
destination file as destfile. I then load the excel sheet and save it as
the incidents\_df data frame.

``` r
library(tidyverse)
library(readxl)

url <- "https://cdn.philasd.org/offices/performance/Open_Data/School_Performance/Serious_Incidents/School%20Profiles%20Serious%20Incidents%202018-2019.xlsx"
destfile <- "../data/SY18-19.xlsx"
download.file(url, destfile, mode = "wb")

incidents_df <- read_excel(destfile, sheet = 1)
```

I do an initial exploration into the data. Grouping by incident type and
then finding the sum of the incidents allows me to see which incidents
were most prominent during the school year. I see that “Disorderly
Conduct” gets the vast majority of reported incidents followed by
“Assaults” and then “threats”.

``` r
bytype <- incidents_df %>% 
  group_by(INCIDENT_TYPE) %>% 
  summarize( Total = sum(INCIDENT_COUNT)) %>% 
  arrange(desc(Total)) %>% 
  ungroup()
```

This quick analysis can be seen much better in a bar chart.

``` r
ggplot(data = bytype, aes(x = reorder(INCIDENT_TYPE, -Total),y = Total)) +
  geom_bar(stat = "identity", fill = "dodgerblue4") +
  theme_minimal() +
  labs(title = "Serious Incidents in Philly Schools SY18-19",
      x = "Incident Type") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust = 1.0))
```

![](serious_incidents_files/figure-gfm/Graph%20it-1.png)<!-- -->

Thus ends my initial exploration into this data file. The School
District of Philadelphia (SDP) also provides historical data on serious
incidents which I may look into next. I can also explore incidents by
school using the “SCHOOL\_ID” identifier by joining a dataset with that
ID corresponding with the school name.

**Potential future questions**

1.Are there any trends of SDP serious incidents by type? For example,
has “Disorderly Conduct” always been the vast majority of incidents?

2.Has serious incidents been increasing or decreasing in SDP schools?

3.Do some schools have more reported incidents than others?

4.How have some schools performed historically in serious incidents? For
example, have some schools been decreasing and others increasing?

5.Is there any correlation between number of serious incidents and other
factors such as attendance and graduation rates?
