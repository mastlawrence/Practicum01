Tech Review Review
================

1: Business Understanding:

1.1: Business Objectives -Provide summary statistics for tech reviewers.
-Provide leveled list of which tech reviewers reviews the highest
quantity, and how harshly.

1.2: Assess Situation -.csv file with multiple tabs supplied. -No
computational resources provided. -No paid time for project provided.
-Minimal requirements/deliverables provided for the project. -Possibly
results in metric rankings that would bias evaluation of analysts or
tech reviewers. Primary risk.

1.3: Data Mining Goals Success here will look like an even evaluation of
each tech reviewer with appropriate weighting applied to distinguish
method evaluations, validations, and routine testing to properly review
tech reviewer productivity. Weighting will also be applied to the “C1 -
C7” encoded variables which encode for quality errors.

2: Data Understanding:

2.1: Describe Data Dates are imported appropriately, everything else is
a character class. Drop all columns including and after comments. Hans
BY FAR makes up the majority of the data set. More than double. Followed
by Dolly and Vir. Chris’s data is completely missing, and will be
removed from analysis.

Kelly has reviewed an unusually high number of projects given her start
date. Order goes Hans \<- Vir \<- Dolly \<- Dennis \<- Kelly. Differing
number of variables in each sheet, and need to be normalized.

2.2: Explore Data

We really need to find a primary key or make one, or find some way to
join tables. Lets also plan to drop NA values that do not have analyst
name observations attached to.

Hans reviews Lauren’s projects the most. Vir reviewed Katie’s projects
the most. Kim reviews Hillary’s projects the most. Dolly reviews David
M’s projects the most, but very evenly reviews across the entire
department. Clester reviews Katie’s projects the most. Michael reviews
Alexander’s projects the most. Possibly drop Michael from data set as
not applicable to analytical services. Sarah reviews Hillary’s projects
the most. Dennis reviewed Yesenia’s projects the most, and also
Caterine’s. But now Edmond is \#1 of people still at Cambrex. Kelly
reviews Doug’s projects the most of analytical services.

\#tr.chris is completely empty \#tr.jeff having trouble importing

``` r
#Import Data
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.1

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## Warning: package 'tibble' was built under R version 4.2.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(moderndive)
```

    ## Warning: package 'moderndive' was built under R version 4.2.1

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.2.1

``` r
filepath <- "C:/Users/mastl/Desktop/Grad_School/2021_tr.xlsx"

tr.df <- excel_sheets("C:/Users/mastl/Desktop/Grad_School/2021_tr.xlsx")


#tr.jeff <- read_xlsx(filepath, sheet = "Jeff B", skip = 8)
tr.hans <- read_xlsx(filepath, sheet = "Hans H", skip = 8)
tr.vir <- read_xlsx(filepath, sheet = "Vir P", skip = 8)
tr.kim <- read_xlsx(filepath, sheet = "Kim J", skip = 8)
```

    ## New names:
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`

``` r
tr.dolly <- read_xlsx(filepath, sheet = "Dolly W", skip = 8)
```

    ## New names:
    ## • `` -> `...18`
    ## • `` -> `...19`
    ## • `` -> `...20`
    ## • `` -> `...21`

``` r
tr.clester <- read_xlsx(filepath, sheet = "Clester O", skip = 8)
tr.michael <- read_xlsx(filepath, sheet = "Michael K", skip = 8)
tr.chris <- read_xlsx(filepath, sheet = "Chris S", skip = 8)
tr.sarah <- read_xlsx(filepath, sheet = "Sarah C", skip = 8)
tr.dennis <- read_xlsx(filepath, sheet = "Dennis W", skip = 8)
```

    ## New names:
    ## • `` -> `...20`
    ## • `` -> `...21`
    ## • `` -> `...22`
    ## • `` -> `...23`
    ## • `` -> `...24`
    ## • `` -> `...25`
    ## • `` -> `...26`
    ## • `` -> `...27`
    ## • `` -> `...28`
    ## • `` -> `...29`
    ## • `` -> `...30`
    ## • `` -> `...31`
    ## • `` -> `...32`
    ## • `` -> `...33`
    ## • `` -> `...34`
    ## • `` -> `...35`

``` r
tr.kelly <- read_xlsx(filepath, sheet = "Kelly L", skip = 8)

tr.reviewers <- list("hans" = tr.hans, 
                     "vir" =tr.vir, 
                     "kim" = tr.kim, 
                     "dolly" = tr.dolly, 
                     "clester" = tr.clester,
                     "michael" = tr.michael,
                     "sarah" = tr.sarah, 
                     "dennis" = tr.dennis, 
                     "kelly" = tr.kelly)

for(i in 1:9) {
n <-  names(tr.reviewers[i])
s <-  summary(tr.reviewers[[i]])
d <-  dim(tr.reviewers[[i]])
}
```

\#Summarizes how many times a tech reviewer has reviewed each analyst.

``` r
for(i in 1:9) {
  n <- names(tr.reviewers[i])
  a <- table(tr.reviewers[[i]]$Analyst)
  a.df <- as.data.frame(a)
  a.df <- a.df %>% 
    arrange(desc(Freq)) %>%
    mutate(Var1 = tolower(Var1))
  
  colnames(a.df) <- c("Analyst", "Project") 
  
  print(n)
  print(a.df)
  
plot <- ggplot(data = a.df, mapping = aes(reorder(x = Analyst, desc(Project)), y = Project)) +
    geom_col(color = "black", fill = "cadetblue3") + 
    labs(
      title = paste0("Distribution of ", n, "'s reviews"),
      caption = "Source: Tech Review .xlsx file"
    ) +
    xlab("Analyst") +
    ylab("Review Count") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9))

print(plot)
}
```

    ## [1] "hans"
    ##                     Analyst Project
    ## 1            lauren iannaco     160
    ## 2            kaila kleckner     138
    ## 3              kayla thomas     115
    ## 4          stephanie raines     103
    ## 5             matthew laing      69
    ## 6            hillary straub      55
    ## 7    sravankumar assampelli      35
    ## 8             bobby gollmar      31
    ## 9             jonah winkler      26
    ## 10           wesley mickler      22
    ## 11            john goldbaum      21
    ## 12           craig eppolito      20
    ## 13       jose rios-gonzalez      19
    ## 14           preston absher      19
    ## 15     matthew st. lawrence      18
    ## 16             shelly beard      14
    ## 17          vincent thigpen      13
    ## 18         andrew reinhardt       9
    ## 19         catherine murray       9
    ## 20              david milam       9
    ## 21             katie miller       9
    ## 22 kenneth guzman rodriguez       8
    ## 23           caroline ayars       6
    ## 24                chris lee       6
    ## 25        morgan pennington       6
    ## 26               dev sanyal       5
    ## 27              jack tonges       4
    ## 28             torey hunter       4
    ## 29            trevor phares       4
    ## 30            allison lange       3
    ## 31      harris middlesworth       3
    ## 32           robert gollmar       2
    ## 33            cliff sargent       1
    ## 34             felicia hall       1
    ## 35               john bloom       1
    ## 36               kari bautz       1
    ## 37           kenneth guzman       1
    ## 38        kenneth rodriquez       1
    ## 39            rajesh gautam       1
    ## 40        sherona sirkisoon       1
    ## 41             todd sprouse       1
    ## 42              trevor lott       1

![](tr_review_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

    ## [1] "vir"
    ##                                           Analyst Project
    ## 1                                    katie miller      30
    ## 2                                     david milam      16
    ## 3                                carolina merrill      14
    ## 4                                     meg sharkey      12
    ## 5                                       ben pizio       9
    ## 6                                jordan vehlewald       9
    ## 7                                catherine murray       7
    ## 8                                   edmond bailey       7
    ## 9                               morgan pennington       7
    ## 10                                   shelly beard       7
    ## 11                                   tara moening       7
    ## 12                                   felicia hall       6
    ## 13                                 hillary straub       6
    ## 14                                  jonah winkler       5
    ## 15                                 lauren iannaco       5
    ## 16                                    sravankumar       5
    ## 17                                  ashley carver       4
    ## 18                                   ashley garay       4
    ## 19                                 caroline ayars       4
    ## 20                                    hamid lotfi       4
    ## 21                                   kayla thomas       4
    ## 22                                    mike martin       4
    ## 23                                 robert gollmar       4
    ## 24                                 benjamin pizio       3
    ## 25                                 cameron spears       3
    ## 26                                   chris wittum       3
    ## 27                                    jack tonges       3
    ## 28                               michael robinson       3
    ## 29                                 preston absher       3
    ## 30                                yesenia vazquez       3
    ## 31                                  bobby gollmar       2
    ## 32                                 caroline ayars       2
    ## 33                                  douglas moses       2
    ## 34                                jackie oxendine       2
    ## 35                                jessica coleman       2
    ## 36                                     john bloom       2
    ## 37                              kenneth rodriguez       2
    ## 38                                  matthew laing       2
    ## 39                                michael gangwer       2
    ## 40                                    sravankumar       2
    ## 41                                 suraj shrestha       2
    ## 42                                   torey hunter       2
    ## 43                                    trevor lott       2
    ## 44                                  allison lange       1
    ## 45                    ashley carver edmond bailey       1
    ## 46                                      chris lee       1
    ## 47                                     cody perry       1
    ## 48                                  cole mcmullin       1
    ## 49                                 craig eppolito       1
    ## 50                                   david tiberi       1
    ## 51                                    hamid latfi       1
    ## 52                                     jack bloom       1
    ## 53                                  john goldbaum       1
    ## 54 jordan vehlewald katie miller carolina merrill       1
    ## 55                                  justin dancer       1
    ## 56                                           kacy       1
    ## 57                               matthew lawrence       1
    ## 58                                 michael martin       1
    ## 59                                  nicole warren       1
    ## 60                                         shelly       1
    ## 61                                  trevor phares       1

![](tr_review_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

    ## [1] "kim"
    ##             Analyst Project
    ## 1    hillary straub       9
    ## 2  carolina merrill       5
    ## 3  jordan vehlewald       5
    ## 4      tara moening       4
    ## 5       trevor lott       4
    ## 6      ashley garay       3
    ## 7    lauren iannaco       3
    ## 8    preston absher       3
    ## 9    cameron spears       2
    ## 10 catherine murray       2
    ## 11    john goldbaum       2
    ## 12        ben pizio       1
    ## 13   craig eppolito       1
    ## 14              djt       1
    ## 15    edmond bailey       1
    ## 16     felicia hall       1
    ## 17        hillary s       1
    ## 18    matthew laing       1
    ## 19    nicole warren       1
    ## 20   suraj shrestha       1
    ## 21    trevor phares       1
    ## 22  yesenia vazquaz       1

![](tr_review_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

    ## [1] "dolly"
    ##                  Analyst Project
    ## 1                david m      15
    ## 2               hilary s      13
    ## 3                katie m      12
    ## 4             carolina m      10
    ## 5               jordan v      10
    ## 6               lauren i      10
    ## 7            catherine m       9
    ## 8            catherine m       9
    ## 9           katie miller       9
    ## 10            caroline a       8
    ## 11               jonah w       8
    ## 12              lauren i       8
    ## 13           sravankumar       8
    ## 14              ashley g       7
    ## 15            carolina m       7
    ## 16              jordan v       7
    ## 17             yesenia v       7
    ## 18               david t       6
    ## 19             felicia a       6
    ## 20             hillary s       6
    ## 21                john g       6
    ## 22             michael m       6
    ## 23             michael r       6
    ## 24                tara m       6
    ## 25              ashley g       5
    ## 26               david m       5
    ## 27              edmond b       5
    ## 28           jack tonges       5
    ## 29             matthew l       5
    ## 30             matthew l       5
    ## 31                 meg s       5
    ## 32               aaron h       4
    ## 33              ashley c       4
    ## 34                 ben p       4
    ## 35              jackie o       4
    ## 36               kaila k       4
    ## 37               kayla t       4
    ## 38             kenneth g       4
    ## 39              morgan m       4
    ## 40              nicole w       4
    ## 41              preson a       4
    ## 42              sravan k       4
    ## 43             allison l       3
    ## 44               bobby g       3
    ## 45             cameron s       3
    ## 46            caroline a       3
    ## 47                drew d       3
    ## 48               katie m       3
    ## 49               kayla t       3
    ## 50              morgan p       3
    ## 51              morgan p       3
    ## 52               preston       3
    ## 53              shelly b       3
    ## 54                sravan       3
    ## 55               torey h       3
    ## 56              trevor l       3
    ## 57              edmond b       2
    ## 58               jonah w       2
    ## 59               preston       2
    ## 60                  tara       2
    ## 61                tara m       2
    ## 62      aaron h/ashley g       1
    ## 63             allison l       1
    ## 64 austin k(1st analyst)       1
    ## 65                 bobby       1
    ## 66              calvin b       1
    ## 67               cameron       1
    ## 68     cameron s/aaron h       1
    ## 69               chris l       1
    ## 70            danielle m       1
    ## 71                darryl       1
    ## 72              darryl l       1
    ## 73               david m       1
    ## 74  david m(2nd analyst)       1
    ## 75               fangi y       1
    ## 76              fangyi z       1
    ## 77              harris m       1
    ## 78            jack bloom       1
    ## 79                jack t       1
    ## 80                  jaya       1
    ## 81               john g.       1
    ## 82              justin d       1
    ## 83                kacy m       1
    ## 84            kathleen m       1
    ## 85        matthew lawson       1
    ## 86                   meg       1
    ## 87             preston a       1
    ## 88              shelly b       1
    ## 89              wesley m       1

![](tr_review_files/figure-gfm/unnamed-chunk-2-4.png)<!-- -->

    ## [1] "clester"
    ##                            Analyst Project
    ## 1                     katie miller      11
    ## 2                   hillary straub       5
    ## 3                 catherine murray       4
    ## 4                  jackie oxendine       4
    ## 5                 jordan vehlewald       4
    ## 6                     kayla thomas       4
    ## 7                   lauren iannaco       4
    ## 8                     felicia hall       3
    ## 9                    kaila kleeker       3
    ## 10                   matthew laing       3
    ## 11               morgan pennington       3
    ## 12                   aaron heredia       2
    ## 13                    ashley garay       2
    ## 14                  cameron spears       2
    ## 15                      kacy magee       2
    ## 16               kenneth rodriguez       2
    ## 17            matthew st lawerence       2
    ## 18                          sravan       2
    ## 19                    todd sprouse       2
    ## 20                   trevor phares       2
    ## 21                   allison lange       1
    ## 22                     andy slater       1
    ## 23                       ben pizio       1
    ## 24                    calvin brown       1
    ## 25                carolina merrill       1
    ## 26                    david tiberi       1
    ## 27                      fangyi zhu       1
    ## 28                 jessica coleman       1
    ## 29                   john goldbaum       1
    ## 30                  kaila kleckner       1
    ## 31    matthew laing/robert gollmar       1
    ## 32                     mike martin       1
    ## 33                    shelly beard       1
    ## 34                    tara moening       1
    ## 35              tara moening/aaron       1
    ## 36                    torey hunter       1
    ## 37                  trevor  phares       1
    ## 38                  wesley mickler       1
    ## 39                 yesenia vazquez       1
    ## 40 yesenia vazquez/catherine murry       1
    ## 41 yesenia vazquez/jackie oxendine       1
    ## 42                  yesinia vaquez       1

![](tr_review_files/figure-gfm/unnamed-chunk-2-5.png)<!-- -->

    ## [1] "michael"
    ##                              Analyst Project
    ## 1                  alexander bowitch       2
    ## 2                     benjamin pizio       1
    ## 3                      cole mcmullin       1
    ## 4   cole mcmullin /alexander bowitch       1
    ## 5                        hamid lofti       1
    ## 6                        hamid lotfi       1
    ## 7  merrill, carolina/miller, katie p       1
    ## 8                  morgan pennington       1
    ## 9                    shelly r. beard       1
    ## 10 sutton jessica; sherona sirkisoon       1

![](tr_review_files/figure-gfm/unnamed-chunk-2-6.png)<!-- -->

    ## [1] "sarah"
    ##                               Analyst Project
    ## 1                      hillary straub      12
    ## 2                          jack bloom       9
    ## 3                      lauren iannaco       7
    ## 4                    jordan vehlewald       6
    ## 5                        katie miller       6
    ## 6                      preston absher       6
    ## 7                       jonah winkler       5
    ## 8                       trevor phares       5
    ## 9                     yesenia vasquez       5
    ## 10                   carolina merrill       4
    ## 11                       kayla thomas       4
    ## 12                      matthew laing       4
    ## 13                       shelly beard       4
    ## 14                         john bloom       3
    ## 15                        mike martin       3
    ## 16                         doug moses       2
    ## 17                      douglas moses       2
    ## 18 edmond bailey and michael robinson       2
    ## 19                       felicia hall       2
    ## 20                        hamid lotfi       2
    ## 21                    jessica coleman       2
    ## 22                     kaila kleckner       2
    ## 23                      nicole warren       2
    ## 24                       tara moening       2
    ## 25                      allison lange       1
    ## 26                      ashley carver       1
    ## 27                       ashley garay       1
    ## 28                     caroline ayars       1
    ## 29                           douglass       1
    ## 30                      edmond bailey       1
    ## 31                       felicai hall       1
    ## 32                        meg sharkey       1
    ## 33                     michael martin       1
    ## 34                  morgan pennington       1

![](tr_review_files/figure-gfm/unnamed-chunk-2-7.png)<!-- -->

    ## [1] "dennis"
    ##                         Analyst Project
    ## 1               yesenia vazquez      12
    ## 2              catherine murray       9
    ## 3                 edmond bailey       9
    ## 4        sravankumar assampelli       9
    ## 5                 douglas moses       8
    ## 6                  torey hunter       7
    ## 7      kenneth guzman rodriguez       6
    ## 8              michael robinson       6
    ## 9               jackie oxendine       5
    ## 10                nicole warren       5
    ## 11                 kayla thomas       4
    ## 12                  meg sharkey       4
    ## 13                 ashley garay       3
    ## 14                 felicia hall       3
    ## 15        harrison middlesworth       3
    ## 16                matthew laing       3
    ## 17               wesley mickler       3
    ## 18               craig eppolito       2
    ## 19                   dev sanyal       2
    ## 20             jordan vehlewald       2
    ## 21               cameron spears       1
    ## 22             carolina merrill       1
    ## 23               caroline ayars       1
    ## 24                  david milam       1
    ## 25               hillary straub       1
    ## 26              jessica coleman       1
    ## 27                jonah winkler       1
    ## 28                 katie miller       1
    ## 29    katie miller/david tiberi       1
    ## 30               lauren iannaco       1
    ## 31            matt st. lawrence       1
    ## 32 yesenia vazquez/ashley garay       1

![](tr_review_files/figure-gfm/unnamed-chunk-2-8.png)<!-- -->

    ## [1] "kelly"
    ##                                              Analyst Project
    ## 1                                           jake dam       8
    ## 2                                         doug moses       7
    ## 3                                        amber blake       5
    ## 4                                     craig eppolito       5
    ## 5                                     hillary straub       5
    ## 6                                         matt stahl       5
    ## 7                                  sravan assampelli       5
    ## 8                                          chris lee       4
    ## 9                                      nicole warren       4
    ## 10                                        dev sanyal       3
    ## 11                                     jonah winkler       3
    ## 12                                    joslyn sargent       3
    ## 13                                      katie miller       3
    ## 14                                      kayla thomas       3
    ## 15                                   rebekah vincent       3
    ## 16                                            sravan       3
    ## 17                                      torey hunter       3
    ## 18                                     allison lange       2
    ## 19                                     ashley carver       2
    ## 20                                      david tiberi       2
    ## 21                                 matt st. lawrence       2
    ## 22                                     matthew laing       2
    ## 23                                     trevor phares       2
    ## 24                                     aaron heredia       1
    ## 25                                      calvin brown       1
    ## 26                                   cameron / aaron       1
    ## 27                                    cameron spears       1
    ## 28                                  carolina merrill       1
    ## 29                                    diane togbonou       1
    ## 30 doug moses\r\njackie oxendine\r\nmichael robinson       1
    ## 31                                     edmond bailey       1
    ## 32                                      felicia hall       1
    ## 33                                   jessica coleman       1
    ## 34                                  jordan vehlewald       1
    ## 35                                     justin dancer       1
    ## 36                                       kate krontz       1
    ## 37                                   kayla / allison       1
    ## 38                                 kenneth rodriguez       1
    ## 39                                    lauren iannaco       1
    ## 40                                        matt laing       1
    ## 41                              matthew st. lawrence       1
    ## 42                                  michael robinson       1
    ## 43                                  micheal robinson       1
    ## 44                                           sherona       1
    ## 45                                 sherona sirkisoon       1
    ## 46                              wesley m. / jonah w.       1

![](tr_review_files/figure-gfm/unnamed-chunk-2-9.png)<!-- -->

3: Data Preparation

3.1: Select Data

Todd wants us to grab everything, so lets select all of variables except
for comments. Dropping closed date for now due to missing values and
inconsistency. Come back and try to repair it.

Variables for different types of QA findings are decoded below. Other
columns are renamed to be more usable.

``` r
var.decode <- function(tr.dataframe) {
  tr.dataframe <- tr.dataframe %>%
    select(`Start Date`, Project, `Project Description`, Analyst, `Test Type`,
          C1, C2, C3, C4, C5, C6, C7, RFT, `>2X`, `Sample IDs`, Comments) %>%
  
  rename("start_date" = `Start Date`, 
         "project_description" = `Project Description`,
         "test_type" = `Test Type`, 
         "missed_QEM" = C1, 
         "incorrect_result_reported" = C2,
         "reprocessing_Integration" = C3,
         "reprocessing_calculation" = C4,
         "LIMS_incompletation" = C5, 
         "GDP_misses" = C6,
         "missing_client_documentation" = C7,
         "back_and_forth" = `>2X`, 
         "sample_ID" = `Sample IDs`)
  
  return(tr.dataframe)
}

for(i in 1:length(tr.reviewers)){
  print(names(tr.reviewers[i]))
  tr.reviewers[[i]] <- var.decode(tr.reviewers[[i]])
}
```

    ## [1] "hans"
    ## [1] "vir"
    ## [1] "kim"
    ## [1] "dolly"
    ## [1] "clester"
    ## [1] "michael"
    ## [1] "sarah"
    ## [1] "dennis"
    ## [1] "kelly"

``` r
hans <- as.data.frame(tr.reviewers$hans)
vir <- as.data.frame(tr.reviewers$vir)
kim <- as.data.frame(tr.reviewers$kim)
dolly <- as.data.frame(tr.reviewers$dolly)
clester <- as.data.frame(tr.reviewers$clester)
michael <- as.data.frame(tr.reviewers$michael)
sarah <- as.data.frame(tr.reviewers$sarah)
dennis <- as.data.frame(tr.reviewers$dennis)
kelly <- as.data.frame(tr.reviewers$kelly)
```

3.2: Clean Data

Kim and Dolly both have projects where the data entered for the first
test is assumed to apply to the consecutive tests. This is corrected by
applying a fill to the columns where this occurs. The goal of this step
is to associate all testing with the analyst who performed it. Since
QEMs are assigned per test, this will provide more accurate
first-time-correct statistics for each analyst.

Clester has a few rows with an empty analyst field. These observations
will not aid us in analyzing QEMs per analyst, so these rows will be
dropped from analysis.

All tech reviewers were re-listed, and the function “QEM.Encode()” was
written to address N/A values

``` r
kim <- kim %>%
  fill(c(start_date, Project, project_description, Analyst, test_type), .direction = c("down"))

dolly <- dolly %>%
  fill(c(start_date, Project, Analyst, test_type), .direction = c("down")) %>%
    filter(Analyst != is.na(Analyst))

clester <- clester %>%
  filter(Analyst != is.na(Analyst))

michael <- michael %>%
  filter(Analyst != is.na(Analyst))

dennis <- dennis %>%
  filter(Analyst != is.na(Analyst))

kelly <- kelly %>%
  fill(c(start_date, Project, project_description, Analyst, test_type), .direction = c("down")) %>%
  filter(Analyst != is.na(Analyst)) 
```

Re-list all tech reviewers, and encode each correction in order to
produce summary statistics. List is named ‘tr.reviewers’ and the
function used to encode variables is named “QEM.Encode()”

``` r
QEM.Encode <- function(df){
  
df[is.na(df)] = 0
  df[,6] <- ifelse(df[,6] != 0, 1, 0)
  df[,7] <- ifelse(df[,7] != 0, 1, 0)
  df[,8] <- ifelse(df[,8] != 0, 1, 0)
  df[,9] <- ifelse(df[,9] != 0, 1, 0)
  df[,10] <- ifelse(df[,10] != 0, 1, 0)
  df[,11] <- ifelse(df[,11] != 0, 1, 0)
  df[,12] <- ifelse(df[,12] != 0, 1, 0)
  df[,13] <- ifelse(df[,13] != 0, 1, 0)
  df[,14] <- ifelse(df[,14] != 0, 1, 0)
  
  return(df)
}

tr.reviewers <- list("Hans" = hans, 
                     "Vir" =vir, 
                     "Kim" = kim, 
                     "Dolly" = dolly, 
                     "Clester" = clester,
                     "Michael" = michael,
                     "Sarah" = sarah, 
                     "Dennis" = dennis, 
                     "Kelly" = kelly)

for(i in 1:length(tr.reviewers)){
  tr.reviewers[[i]] <- QEM.Encode(tr.reviewers[[i]])
}
```

3.3: Construct Data

As per the business objective provided, new summary statistics will be
added to each dataframe that describe the rate at which each tech
reviewer assigns corrections and Right-First-Time assignments.

``` r
correction.summary <- function(df) {
  
  summary_df <- df %>%
    mutate(missed_QEM_rate = sum(df[,6]) / nrow(df) * 100) %>%
    mutate(incorrect_result_reported_rate = sum(df[,7]) / nrow(df) * 100) %>%
    mutate(reprocessing_integration_rate = sum(df[,8]) / nrow(df) * 100) %>%
    mutate(reprocessing_calculation_rate = sum(df[,9]) / nrow(df) * 100) %>%
    mutate(LIMS_incompletion_rate = sum(df[,10]) / nrow(df) * 100) %>%
    mutate(GDP_misses_rate = sum(df[,11]) / nrow(df) * 100) %>%
    mutate(missing_client_documentation_rate = sum(df[,12]) / nrow(df) * 100) %>%
    mutate(RFT_rate = sum(df[,13]) / nrow(df) * 100)
  
  return(summary_df)
}

for(i in 1:length(tr.reviewers)){
  tr.reviewers[[i]] <- correction.summary(tr.reviewers[[i]])
}
```

3.4: Integrate Data

At this point, our business objective is to analyze the correction rate
of each reviewer. Each reviewer is represented within the list
tr.reviewers, and each tech reviewer has reviewed a different number of
projects. There is no need for integration of data at this point.

3.5: Format Data

Data formatting was performed by summarizing the entire data set by the
constructed data created in step 3.3.

``` r
format.data <- function(df){
  format_data <- df %>%
    
    select(missed_QEM_rate, incorrect_result_reported_rate, reprocessing_integration_rate,
           reprocessing_calculation_rate, LIMS_incompletion_rate, GDP_misses_rate,
           missing_client_documentation_rate, RFT_rate) %>%
    
    summarise(mean(missed_QEM_rate), mean(incorrect_result_reported_rate), mean(reprocessing_integration_rate),
              mean(reprocessing_calculation_rate), mean(LIMS_incompletion_rate), mean(GDP_misses_rate),
              mean(missing_client_documentation_rate), mean(RFT_rate)) %>%
  
    rename(missed_QEM_rate = `mean(missed_QEM_rate)`, incorrect_result_reported_rate = `mean(incorrect_result_reported_rate)`,
           reprocessing_integration_rate = `mean(reprocessing_integration_rate)`, 
           reprocessing_calculation_rate = `mean(reprocessing_calculation_rate)`, 
           LIMS_incompletion_rate = `mean(LIMS_incompletion_rate)`, GDP_misses_rate = `mean(GDP_misses_rate)`,
           missing_client_documentation_rate = `mean(missing_client_documentation_rate)`, RFT_rate = `mean(RFT_rate)`)
  
  return(format_data)
}

for(i in 1:length(tr.reviewers)){
  tr.reviewers[[i]] <- format.data(tr.reviewers[[i]])
}
```

Data Visualization:

``` r
plot.data <- function(df, plot_title){
  df <- df %>%
    select(-RFT_rate) %>%
    pivot_longer(cols = everything()) %>%
    print()
  
  plot_data <- ggplot(data = df, mapping = aes(reorder(x = name, desc(value)), y = value)) +
    geom_col(fill = "aquamarine3", color = "black") +
    labs(
      title = paste(plot_title, "Correction Rates"),
      caption = "source: TR Excel File"
    ) +
    xlab("Correction Type") +
    ylab("Correction Issuance Rate") +
    theme_bw() +
    theme(axis.text.x.bottom = element_text(size = 8)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1.0))
  
  return(plot_data)
}

for(i in 1:length(tr.reviewers)){
  x <- plot.data(tr.reviewers[[i]], names(tr.reviewers[i]))
  print(x)
}
```

    ## # A tibble: 7 × 2
    ##   name                              value
    ##   <chr>                             <dbl>
    ## 1 missed_QEM_rate                   0.718
    ## 2 incorrect_result_reported_rate    1.33 
    ## 3 reprocessing_integration_rate     1.03 
    ## 4 reprocessing_calculation_rate     1.13 
    ## 5 LIMS_incompletion_rate            9.03 
    ## 6 GDP_misses_rate                   6.15 
    ## 7 missing_client_documentation_rate 0.308

![](tr_review_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## # A tibble: 7 × 2
    ##   name                              value
    ##   <chr>                             <dbl>
    ## 1 missed_QEM_rate                   11.4 
    ## 2 incorrect_result_reported_rate     4.49
    ## 3 reprocessing_integration_rate      3.67
    ## 4 reprocessing_calculation_rate      7.76
    ## 5 LIMS_incompletion_rate            11.0 
    ## 6 GDP_misses_rate                   46.1 
    ## 7 missing_client_documentation_rate  2.04

![](tr_review_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

    ## # A tibble: 7 × 2
    ##   name                              value
    ##   <chr>                             <dbl>
    ## 1 missed_QEM_rate                    3.57
    ## 2 incorrect_result_reported_rate    17.9 
    ## 3 reprocessing_integration_rate      3.57
    ## 4 reprocessing_calculation_rate      4.76
    ## 5 LIMS_incompletion_rate             2.38
    ## 6 GDP_misses_rate                   19.0 
    ## 7 missing_client_documentation_rate  1.19

![](tr_review_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

    ## # A tibble: 7 × 2
    ##   name                              value
    ##   <chr>                             <dbl>
    ## 1 missed_QEM_rate                    6.34
    ## 2 incorrect_result_reported_rate    12.1 
    ## 3 reprocessing_integration_rate      4.96
    ## 4 reprocessing_calculation_rate      8.26
    ## 5 LIMS_incompletion_rate            13.5 
    ## 6 GDP_misses_rate                    9.09
    ## 7 missing_client_documentation_rate  1.93

![](tr_review_files/figure-gfm/unnamed-chunk-8-4.png)<!-- -->

    ## # A tibble: 7 × 2
    ##   name                              value
    ##   <chr>                             <dbl>
    ## 1 missed_QEM_rate                    7.95
    ## 2 incorrect_result_reported_rate     6.82
    ## 3 reprocessing_integration_rate     11.4 
    ## 4 reprocessing_calculation_rate      9.09
    ## 5 LIMS_incompletion_rate             2.27
    ## 6 GDP_misses_rate                   42.0 
    ## 7 missing_client_documentation_rate  0

![](tr_review_files/figure-gfm/unnamed-chunk-8-5.png)<!-- -->

    ## # A tibble: 7 × 2
    ##   name                              value
    ##   <chr>                             <dbl>
    ## 1 missed_QEM_rate                     0  
    ## 2 incorrect_result_reported_rate      0  
    ## 3 reprocessing_integration_rate       0  
    ## 4 reprocessing_calculation_rate      27.3
    ## 5 LIMS_incompletion_rate             27.3
    ## 6 GDP_misses_rate                    18.2
    ## 7 missing_client_documentation_rate   0

![](tr_review_files/figure-gfm/unnamed-chunk-8-6.png)<!-- -->

    ## # A tibble: 7 × 2
    ##   name                              value
    ##   <chr>                             <dbl>
    ## 1 missed_QEM_rate                    8.93
    ## 2 incorrect_result_reported_rate    14.3 
    ## 3 reprocessing_integration_rate      2.68
    ## 4 reprocessing_calculation_rate     13.4 
    ## 5 LIMS_incompletion_rate            20.5 
    ## 6 GDP_misses_rate                   40.2 
    ## 7 missing_client_documentation_rate  8.04

![](tr_review_files/figure-gfm/unnamed-chunk-8-7.png)<!-- -->

    ## # A tibble: 7 × 2
    ##   name                              value
    ##   <chr>                             <dbl>
    ## 1 missed_QEM_rate                   1.71 
    ## 2 incorrect_result_reported_rate    0    
    ## 3 reprocessing_integration_rate     4.27 
    ## 4 reprocessing_calculation_rate     1.71 
    ## 5 LIMS_incompletion_rate            0.855
    ## 6 GDP_misses_rate                   5.13 
    ## 7 missing_client_documentation_rate 0

![](tr_review_files/figure-gfm/unnamed-chunk-8-8.png)<!-- -->

    ## # A tibble: 7 × 2
    ##   name                               value
    ##   <chr>                              <dbl>
    ## 1 missed_QEM_rate                    0    
    ## 2 incorrect_result_reported_rate     0.870
    ## 3 reprocessing_integration_rate      2.61 
    ## 4 reprocessing_calculation_rate      1.74 
    ## 5 LIMS_incompletion_rate             0    
    ## 6 GDP_misses_rate                   10.4  
    ## 7 missing_client_documentation_rate  0.870

![](tr_review_files/figure-gfm/unnamed-chunk-8-9.png)<!-- -->

Comparison of RFT rates between tech reviewers

``` r
RFT_Compare <- data.frame(matrix(ncol = 2, nrow = 9))

for(i in 1:length(tr.reviewers)){
  RFT_Compare[i,1] <- names(tr.reviewers[i])
  RFT_Compare[i,2] <- tr.reviewers[[i]]$RFT_rate
}

colnames(RFT_Compare) <- c("Reviewer", "RFT_Rate")

ggplot(data = RFT_Compare, mapping = aes(reorder(x = Reviewer, desc(RFT_Rate)), y = RFT_Rate)) +
  geom_col(fill = "aquamarine3", color = "black")
```

![](tr_review_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
