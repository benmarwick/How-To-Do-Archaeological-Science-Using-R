# Tempo Plots in R for Analysing Radiocarbon Dates

Thomas S. Dye




### Archaeological Motivation

As archaeologists, we are interested in change over time, and as archaeological
scientists we are interested in measuring it.

The tempo plot is one way to measure change over time: it estimates the
cumulative occurrence of archaeological events in a Bayesian calibration.

The tempo plot yields a graphic where the slope of the plot directly reflects the pace of change: a period of rapid change yields a steep slope and a period of slow change yields a gentle slope. When there is no change, the plot is horizontal. When change is instantaneous, the plot is vertical.

The example we'll use in this demo includes radiocarbon dates collected beneath sixteen taro pond fields on three islands in Hawai\`i. The dates provide *termini* *post quos* for the construction of the pond fields.

In my work with the tempo plot so far I've interpreted different plot shapes as distinguishing tradition, innovation, and fashion. The tempo of change for the taro pond fields was interpreted as representing a tradition in old Hawai\`i.

### Obtain MCMC Output from Calibration Software

A tempo plot summarizes the raw MCMC output yielded by a Bayesian calibration software package. A tempo plot can be constructed from the raw MCMC output produced by BCal, OxCal, and Chronomodel. Here, I'll illustrate the use of
OxCal.

OxCal produces raw MCMC output when the `MCMC_sample` function appears near the start of the `.oxcal` file.

OxCal's `MCMC_sample` function takes three parameters: a name for the output
file, the interval for sampling, and the maximum number of samples to write to the file. The example here will yield the file "loi-mcmc.csv" which will contain a maximum of 100,000 lines, using one in twenty-five of the MCMC iterations.

    MCMC_sample("loi-mcmc", 25, 100000)

### Read OxCal MCMC Output

Here, we call the `read.csv` function to read the raw MCMC output file produced by OxCal and assign it to the variable `mcmc`.  This might take a while; the file `loi-mcmc.csv` is 69.4 MB.


```r
# read in the large CSV very quickly
mcmc <- readr::read_csv("loi-mcmc.csv")
# convert colnames to read.csv style
names(mcmc) <- make.names(names(mcmc))
```

This line writes a list of the column names to standard output.  We'll use this to select the columns of interest.

The list written to standard output shows the index of the left-most column
names in square brackets. This is a convention in `R`. In this list, the first entry, `Pass`, refers to the iteration of the MCMC sampler. Subsequent entries 2 &hellip; 89 refer to various components of the Bayesian chronological model, including age determinations and phase boundaries.  Most of this information is not useful for the tempo plot demonstration, so we'll need to separate the wheat from the chaff.


```r
colnames(mcmc)
```

```
##  [1] "Pass"                   "Start.Pre.colonization"
##  [3] "Beta.83313"             "Wk.15982"              
##  [5] "Colonization"           "KOU.CS.5a"             
##  [7] "Beta.20852b"            "Wk.19312"              
##  [9] "Wk.19313"               "Beta.135125"           
## [11] "Beta.42172"             "Beta.310824"           
## [13] "Beta.343212"            "Beta.5613"             
## [15] "NOSAMS.0809.26"         "Beta.339778"           
## [17] "Beta.208143"            "Beta.101871"           
## [19] "Beta.101872"            "Beta.260904"           
## [21] "Beta.138980"            "Beta.150615"           
## [23] "Beta.233042"            "Beta.150620"           
## [25] "Beta.251247"            "Wk.19311"              
## [27] "Wk.19310"               "Beta.135126"           
## [29] "Beta.260905"            "Beta.45363"            
## [31] "Beta.28136"             "Beta.28135"            
## [33] "CAMS.25560"             "CAMS.26396"            
## [35] "CAMS.25561"             "SR.5081"               
## [37] "SR.5085"                "SR.5080"               
## [39] "SR.5082"                "End.Post.colonization" 
## [41] "Anahulu.start"          "X..."                  
## [43] "O.Anahulu"              "Lower.Elialii.start"   
## [45] "Beta.213276"            "M.Lower.Elialii"       
## [47] "Upper.Elialii.start"    "Beta.213274"           
## [49] "M.Upper.Elialii"        "Kuele.central.2.start" 
## [51] "AA.72543"               "M.Kuele.central.2"     
## [53] "Halawa.stage.1.start"   "Beta.263862"           
## [55] "H.Halawa.stage.1"       "Halepoki.makai.start"  
## [57] "AA71542b"               "M.Halepoki.makai"      
## [59] "Kuele.central.1.start"  "AA.71549"              
## [61] "M.Kuele.central.1"      "Halawa.bund.start"     
## [63] "Beta.263861"            "H.Halawa.bund"         
## [65] "Kukuinui.start"         "AA71541"               
## [67] "M.Kukuinui"             "Kuele.west.start"      
## [69] "AA71122"                "M.Kuele.west"          
## [71] "Halepoki.central.start" "AA71550"               
## [73] "M.Halepoki.central"     "Keiu.start"            
## [75] "Beta.193986"            "M.Keiu"                
## [77] "Halepoki.mauka.start"   "AA72162"               
## [79] "M.Halepoki.mauka"       "Pawaa.central.start"   
## [81] "AA71121"                "M.Pawaa.central"       
## [83] "Pawaa.makai.start"      "AA72161"               
## [85] "M.Pawaa.makai"          "Lahokea.start"         
## [87] "Beta.215407"            "M.Lahokea"             
## [89] "X89"
```

### Select and Confirm Columns to Plot

For this example, we are interested in construction date estimates for taro pond fields.  In the Bayesian chronological model these estimates are labeled starting with a capital letter followed by a dot.

Indices of the construction date estimates are assigned to the variable `i`.
From the output of `colnames(mcmc)` above, we can see that column 43 is named "O.Anahulu", which represents the construction date estimate of a taro pond field in the Anahulu Valley of O\`ahu Island, column 46 is named "M.Lower.Elialii", which represents the construction date estimate of a taro pond field in the lower Elialii system in a valley on Moloka\`i Island, etc.


```r
i <- c(43, 46, 49, 52, 55, 58, 61, 64, 67, 70, 73, 76, 79, 82, 85, 88)
```

The interesting portion of `mcmc` is assigned to the variable `mcmc.select`, using the indexing facility of `R`.  The square brackets at the end of this line enclose two values&#x2014;a nil value before the comma, and the vector of indices in our variable `i` after the comma&#x2014;that instruct `R` to select all rows but only the columns indexed in `i`.


```r
mcmc.select <- mcmc[,i]
```

We now have a table with 16 columns, instead of one with 89 columns. Here, we check that the column names of `mcmc.select` are the ones we intended to select. Happily, there are 16 of them and they appear to be correct.  The column named "O.Anahulu" that was the 43rd column of `mcmc` is now the first column of `mcmc.select`.


```r
colnames(mcmc.select)
```

```
##  [1] "O.Anahulu"          "M.Lower.Elialii"    "M.Upper.Elialii"   
##  [4] "M.Kuele.central.2"  "H.Halawa.stage.1"   "M.Halepoki.makai"  
##  [7] "M.Kuele.central.1"  "H.Halawa.bund"      "M.Kukuinui"        
## [10] "M.Kuele.west"       "M.Halepoki.central" "M.Keiu"            
## [13] "M.Halepoki.mauka"   "M.Pawaa.central"    "M.Pawaa.makai"     
## [16] "M.Lahokea"
```

### Load and Run `calc.tempo` Function

This line reads in the tempo plot source code distributed in the file `tempo-plot-demo-saa-2017.r`.  There are two functions in the file, which we'll use to calculate the tempo plot and to print it to a graphics device.


```r
source("tempo-plot-demo-saa-2017.r")
```

Here is the `calc.tempo` function we'll use to calculate the tempo plot.  It is worth looking at in detail because other joint posterior calculations will have similar structure.

The function has three parameters:

-   `mcmc.data` is the selected columns of the raw MCMC output from OxCal
-   `by.int` specifies the interval in years between points for which the statistic will be calculated
-   `out.file` is an optional parameter that will write the tempo calculation to a file.

The first line finds the minimum and maximum values in `mcmc.data` and creates a vector from the minimum to the maximum by the interval `by.int`.  The vector is assigned to the variable `years`; these are the years for which the statistic will be calculated.

The second line declares a matrix `res.mat` large enough to hold the results of the calculation.

Most of the work takes place in the `for` loop, which loops over `years` and determines for each of the 100,000 lines in `mcmc.data` how many taro pond fields were constructed before that year.  This is the part of the function that would change if a different statistic were implemented.

The results of the simulation are summarized in the lines assigning values to the variables `means` and `sds`.

The results are packaged up in a dataframe assigned to the variable `res.df`.

The variable `res.df` is then written to a file if the `out.file` parameter has a value and finally returned.


```r
calc.tempo <- function(mcmc.data, by.int, out.file="") {
  years <- seq(from = floor(min(mcmc.data)), to = ceiling(max(mcmc.data)), by = by.int)
  res.mat <- matrix(ncol = length(years), nrow = dim(mcmc.data)[1])

  for (i in 1:length(years)) {
    gte <- mcmc.data <= years[i]
    res.mat[,i] <- rowSums(gte)
  }
  means <- colMeans(res.mat)
  sds <- apply(res.mat, 2, sd)
  res.df <- data.frame(mean = means, sd = sds, year = years)

  if (!(out.file == "")) {
    write.csv(res.df, out.file)
  }
  return(res.df)
}
```

Here, we call `calc.tempo` with our `mcmc.select` data, ask the function to calculate the statistic by decade, and save the results to a file, `loi-tempo.csv`. In addition, we save the results in a variable, `loi.tempo`, to save ourselves the trouble of reading them back in from the file.

If, at some later date, we desire access to the data, then we can `read.csv("loi-tempo.csv")`.


```r
loi.tempo <- calc.tempo(mcmc.data = mcmc.select, by.in = 10, out.file = "loi-tempo.csv")
```

### Check `calc.tempo` Output

A summary of the `calc.tempo` results suggests all went well.  The mean number of construction events ranges from 0 to just under 16, the number of taro pond fields in our sample.  The calculations were carried out for a period of almost 900 years, from AD 1056 to 1946.


```r
summary(loi.tempo)
```

```
##       mean               sd              year     
##  Min.   : 0.0000   Min.   :0.0000   Min.   :1056  
##  1st Qu.: 0.3658   1st Qu.:0.4707   1st Qu.:1278  
##  Median : 5.7018   Median :1.4603   Median :1501  
##  Mean   : 5.6574   Mean   :1.0657   Mean   :1501  
##  3rd Qu.: 9.0754   3rd Qu.:1.5279   3rd Qu.:1724  
##  Max.   :15.8329   Max.   :1.5576   Max.   :1946
```

### Example Plot with `plot.tempo`

The source file, `tempo-plot-demo-saa-2017.r`, includes a simple function to plot the `calc.tempo` results based on the `ggplot2` library. The function `plot.tempo` accepts data from an `R` variable with the parameter `tempo.data` or from a file created by `calc.tempo` with the `in.file` parameter. If both `tempo.data` and `in.file` are supplied, then the data are taken from `tempo.data` and the `in.file` parameter is ignored.

The `plot.tempo` function displays the graph on-screen. If the parameter
`out.file` is specified with a graphics file extension recognized by `R`, then the graphic will be written to `out.file`. If `out.file` is not specified, then only the on-screen graph is produced.

Some parameters modify the graphic output. The parameters `min.x` and `max.x` can be used to specify the x-axis limits, so you can "zoom" in or out. The parameters `x.label` and `y.label` make it possible to modify the x-axis and y-axis labels. The parameters `plot.ht` and `plot.wi` set the height and width of the plot in inches. Their default values are standard `R` values.


```r
plot.tempo <- function(tempo.data = NULL, in.file = "",  out.file = "", max.x = NA,
                              min.x = NA, y.label = "Cumulative Events",
                              x.label = "Calendar Year", plot.ht = 7,
                              plot.wi = 7){
  library(ggplot2)
  if (is.null(tempo.data)){
    if (in.file == ""){
      stop("No data source")}
    else {
        tempo.data <- read.csv(in.file)}
  }
  h <- ggplot(tempo.data, aes(x = year))
  h <- h + geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd))
  h <- h + geom_line(aes(y = mean))
  h <- h + xlab(x.label) + ylab(y.label)
  if (!(is.na(max.x) | is.na(min.x))){
    h <- h + xlim(min.x, max.x)
  }
  if (!(out.file == "")) {
    ggsave(filename = out.file, plot= h, height = plot.ht, width = plot.wi)
  }
  old.par <- par(no.readonly = T)
  dev.new(width = plot.wi, height = plot.ht)
  print(h)
  par(old.par)
}
```

Here, the `tempo.data` function is called with the variable `loi.tempo` produced by the function `calc.tempo` and the graph is saved in PDF format to the file `loi-tempo.png`. The plot height is set to 3 inches, so both the on-screen graphic and the graphic saved to `loi-tempo.png` will be short and wide.


```r
plot.tempo(tempo.data = loi.tempo, out.file = "loi-tempo.png", plot.ht = 3)
```

![](./loi-tempo.png)

The steady tempo of taro pond field construction events over a period of more than 600 years indicates that the practice was a tradition in old Hawai\`i.

### ArchaeoPhases software

Developers of the Bayesian calibration application `Chronomodel`, Anne Philippe and Marie-Ann Vibet at Jean Leray Mathematics Lab, Université de Nantes, are developing open-source `R` software called [`ArchaeoPhases`](https://cran.r-project.org/package=ArchaeoPhases) to process the raw MCMC output from `Chronomodel` and `OxCal`. Support for the raw MCMC output of `BCal` is planned.

The `ArchaeoPhases` software includes a function, `TempoPlot`, that develops a Bayesian implementation of the tempo plot. Here is an example of graphical
output using code currently under development and expected to be included in the next release of the software.

![](./loi-ap.png)



### Demo Files

The demo comprises these four files:

-   `loi-mcmc.csv`, a large comma-separated value file containing the raw MCMC data from an OxCal calibration;
-   `tempo-plot-demo-saa-2017.r`, an `R` source code file with definitions of the functions `calc.tempo` and `plot.tempo`; and
-   `tempo-plot-demo-saa-2017.Rmd`, the `R Markdown` source for the document you are reading.
-   `loi-ap.png`, a graphics file produced by `ArchaeoPhases` software under development.

These two files are produced during the demo:

-   `loi-tempo.csv`, a comma-separated value file containing the results returned by the `calc.tempo` function; and
-   `loi-tempo.png`, a png file of the graphic produced by the function `plot.tempo`.


```r
sessionInfo()
```

```
## R version 3.3.3 (2017-03-06)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=English_Australia.1252  LC_CTYPE=English_Australia.1252   
## [3] LC_MONETARY=English_Australia.1252 LC_NUMERIC=C                      
## [5] LC_TIME=English_Australia.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  base     
## 
## other attached packages:
## [1] ggplot2_2.2.1
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.10     knitr_1.15.1     magrittr_1.5     hms_0.3         
##  [5] munsell_0.4.3    colorspace_1.3-2 R6_2.2.0         stringr_1.2.0   
##  [9] plyr_1.8.4       tools_3.3.3      grid_3.3.3       gtable_0.2.0    
## [13] htmltools_0.3.5  yaml_2.1.14      lazyeval_0.2.0   rprojroot_1.2   
## [17] digest_0.6.12    assertthat_0.1   tibble_1.2       bookdown_0.3    
## [21] readr_1.1.0      evaluate_0.10    rmarkdown_1.4    labeling_0.3    
## [25] stringi_1.1.3    methods_3.3.3    scales_0.4.1     backports_1.0.5
```
