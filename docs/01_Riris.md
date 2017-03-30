# Basic spatial analysis in R: Point pattern analysis

Phil Riris (Institute of Archaeology, UCL)




### Introduction

This demo is prepared for the SAA 2017 forum at Vancouver "How to do archaeological science using R". This demo has three goals:

1. Demonstrate how to load two types of spatial data (vectors and rasters) and plot them   
2. Demonstrate data coercion into useful formats for spatial analysis    
3. Explore spatial dependency based on a) between-point interaction, b) external covariates   

For more examples of spatial stats applied to archaeological questions, see:

Riris, P. 2017.  "Towards an artefact's-eye view: Non-site analysis of discard patterns and lithic technology in Neotropical settings with a  case from Misiones province, Argentina" Journal of Archaeological Science: Reports 11 DOI: http://dx.doi.org/10.1016/j.jasrep.2017.01.002

#### Loading and plotting spatial data

Suppose we have surveyed a prehistoric settlement landscape in full, and we are reasonably certain all major sites have been located by our awesome field team. We want to understand the empirical pattern in front of us, specifically how smaller sites not only outnumber, but also seem to group around larger and more important sites. How strong is this pattern? Does it hold equally across the whole study area? Can we explain the distribution of a type of site in terms of that of another?

Here are the R packages that we will need to answer these questions:


```r
require(spatstat) # Load packages
require(sp)
require(maptools)
require(rgdal)
require(raster)
```

With time and money limited and excavation difficult, we can't return to the field, so we have to resort to the data we have: in this case a basic distribution map! We will import our field data into R, briefly investigate it to ensure that it looks right:


```r
main.sites <- readShapeSpatial("main.sites.shp") # Load our sites from shapefile

secondary.sites <- readShapeSpatial("secondary.sites.shp")

elev <- raster("elevation.tif") # Load elevation map

str(main.sites) # Investigate and check that the files have loaded correctly.
```

```
## Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots
##   ..@ data       :'data.frame':	21 obs. of  1 variable:
##   .. ..$ marks: Factor w/ 1 level "Primary": 1 1 1 1 1 1 1 1 1 1 ...
##   .. ..- attr(*, "data_types")= chr "C"
##   ..@ coords.nrs : num(0) 
##   ..@ coords     : num [1:21, 1:2] 0.0442 0.4661 0.8143 0.3285 0.7165 ...
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:21] "0" "1" "2" "3" ...
##   .. .. ..$ : chr [1:2] "coords.x1" "coords.x2"
##   ..@ bbox       : num [1:2, 1:2] 0.0314 0.0483 0.9766 0.9296
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:2] "coords.x1" "coords.x2"
##   .. .. ..$ : chr [1:2] "min" "max"
##   ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
##   .. .. ..@ projargs: chr NA
```

```r
str(secondary.sites)
```

```
## Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots
##   ..@ data       :'data.frame':	105 obs. of  1 variable:
##   .. ..$ marks: Factor w/ 1 level "Secondary": 1 1 1 1 1 1 1 1 1 1 ...
##   .. ..- attr(*, "data_types")= chr "C"
##   ..@ coords.nrs : num(0) 
##   ..@ coords     : num [1:105, 1:2] 0.0432 0.1002 0.1007 0.0414 0.4581 ...
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:105] "0" "1" "2" "3" ...
##   .. .. ..$ : chr [1:2] "coords.x1" "coords.x2"
##   ..@ bbox       : num [1:2, 1:2] 0.00799 0.01278 0.9964 0.98552
##   .. ..- attr(*, "dimnames")=List of 2
##   .. .. ..$ : chr [1:2] "coords.x1" "coords.x2"
##   .. .. ..$ : chr [1:2] "min" "max"
##   ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
##   .. .. ..@ projargs: chr NA
```

With the data imported and checked, we can make a simple exploratory plot:


```r
# png(filename = paste0(getwd(), "/", "Sites.png"), 
#     width = 600, 
#     height = 600, 
#     units = "px")

plot(elev, 
     main = "Study Area", 
     axes = FALSE) # Inspect data

plot(main.sites, 
     col = "red", 
     pch = 16, 
     add = TRUE)

plot(secondary.sites, 
     col = "blue", 
     pch = 17, 
     add = TRUE)

legend("bottomleft", 
       title = "Site types", 
              c("Primary", "Secondary"), 
       pch = c(16, 17), 
       col = c("red", "blue"))
```

<img src="01_Riris_files/figure-html/inspect-data-with-plots-1.png" width="672" />

```r
# dev.off()
```
<!-- ![](Sites.png) -->

#### Querying spatial data

For visualisation of raw patterns and classic exploratory tools we must coerce our data into a 'ppp' (planar point pattern) object for spatial statistics using the spatstat library, and then compute the intensity of the data (or 'stuff per unit area'):


```r
main.pp <- as.ppp(main.sites) # Coerce into 'ppp' object for spatial statistics
secondary.pp <- as.ppp(secondary.sites)

sites.pp <- superimpose(main.pp, secondary.pp)

intensity(sites.pp) # Measure intensity of different patterns
```

```
##   Primary Secondary 
##  21.84156 109.20781
```

We can also compute the intesity for all sites together:


```r
allsites <- unmark(sites.pp) # removes "marks" (i.e. descriptors) from point pattern

intensity(allsites) # Intensity (points/unit of area) = 131.0494
```

```
## [1] 131.0494
```

This is not very informative on its own - an abstract number. Let's see if we can get more insights from additional visualisations and stats:


```r
d.sites <- density.ppp(sites.pp) # Overall density of all points in our data

plot(d.sites, main = "Density of all sites") # Plot interpolated values
plot(sites.pp, add = T)
```

<img src="01_Riris_files/figure-html/unnamed-chunk-3-1.png" width="672" />

For numeric marks (e.g. number of structures per settlement) we would use `smooth.ppp()` function. But is this informative on its own? We have two different types of site.


```r
par(mfrow=c(1,2)) # Set number of plotting rows to 1 and number of columns to 2
 
plot(density(main.pp), main = "Density of primary settlements")
plot(density(secondary.pp), main = "Density of satellite settlements")
```

<img src="01_Riris_files/figure-html/unnamed-chunk-4-1.png" width="672" />

Visualisations alone can be misleading. The patterns are clearly related, but how do we formally characterise this? First, it's important to make note of any significant spatial relationships:


```r
clarkevans.test(secondary.pp, correction = c("Donnelly"), nsim = 999)
```

```
## 
## 	Clark-Evans test
## 	Donnelly correction
## 	Monte Carlo test based on 999 simulations of CSR with fixed n
## 
## data:  secondary.pp
## R = 0.65946, p-value = 0.002
## alternative hypothesis: two-sided
```

R < 1 among secondary settlements



```r
clarkevans.test(main.pp, correction = c("Donnelly"), nsim = 999) 
```

```
## 
## 	Clark-Evans test
## 	Donnelly correction
## 	Monte Carlo test based on 999 simulations of CSR with fixed n
## 
## data:  main.pp
## R = 1.053, p-value = 0.662
## alternative hypothesis: two-sided
```

R > 1 among primary sites, but not significant

Tentative conclusion: there are two different processes creating the observed distribution of sites



#### Exploring spatial relationships

More formally, we can state our null hypothesis as: Primary and secondary sites are realisations of two independent point processes (different cultural strategies?)

A point process is simply a process (random or otherwise) that generates an empirical pattern.
 
We know in this case that they are almost certainly related, but it is always worthwhile to go through the motions in case of anything unexpected cropping up.

A good place to start is bivariate spatial statistics such as the bivariate pair-correlation function. It tests for clustering or dispersion between two point patterns, i.e. the influence of one on the other.

The "classic" pair-correlation function and its ancestor Ripley's K are both univariate, but still worth using. However, here, we focus on interaction and dependency between two site-types.


```r
plot(pcfcross(
  sites.pp, 
  divisor = "d", 
  correction = "Ripley"), 
  main = "Bivariate pair-correlation function")
```

<img src="01_Riris_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Empirical pattern obviously differs greatly from function under spatial randomness: `g(pois)`, but how greatly?

We can use the same approach and simulate envelopes of significance to compare agains the empirical pattern.


```r
bivar <- envelope(
  fun = pcfcross, 
  sites.pp, 
  divisor = "d", 
  nsim = 99, 
  correction = c("Ripley")
  ) 
```

```
## Generating 99 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
## 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
## 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98,  99.
## 
## Done.
```

```r
plot(bivar, main = "Pair-correlation function, with simulation envelope")
```

<img src="01_Riris_files/figure-html/unnamed-chunk-9-1.png" width="672" />

This function shows us the following: very strong association at short ranges (up to 10 km), but around 12 km and 16-17 km there are actually fewer secondary sites than expected. A univariate function shows  this dispersion to be present in the whole dataset. In fact, this matches the findings of the Clark-Evans NN test. 

What may have caused secondary sites to be clustered to primary sites, but primary sites to be dispersed?



Now we know: a) the distribution of sites varies in space, b) the presence of one type of site is tied to the presence of another.

We can now take the step from exploratory spatial analysis to confirmatory.

#### Point process modelling

Define a spatstat window object for primary sites, and compute a kernel density estimate of primary settlement sites, to act as covariate, then fit point process model with the covariate:


```r
pwin <- as.owin(c(-0.1, 1.1, -0.1, 1.1)) # 

main.pp$window <- pwin # Assign window

pden <- density(main.pp) # Kernel density estimate of primary settlement sites, to act as covariate

fit.sec <- ppm(unmark(secondary.pp), ~ Cov1, covariates = list(Cov1 = pden)) # Fit point process model with the covariate
```

This appears to be somewhat acceptable model; locations of secondary sites seem to depend on those of primary sites. But how good is this fit?


```r
# png(filename = "density.png", width = 600, height = 600, units = "px")

plot(predict(fit.sec), 
     main="Predicted distribution of secondary settlements", 
     col = rainbow(255))

points(secondary.pp) # Appears to be somewhat acceptable model;
```

<img src="01_Riris_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```r
                  # locations of secondary sites seem to depend
                  # on those of primary sites! But how good is this fit?

#dev.off()
```
<!-- ![](density.png) -->

The Residual K function indicates that the locations of satellite settlements, although spatially autocorrelated with the principal settlements, are poorly explained by using only this covariate. Although a good fit is found between ~ 14 and ~18 km (the purple lines), at scales above and below this narrow band there is both significantly more dispersion and clustering, respectively. What may explain this statistical pattern?


```r
fit.res <- envelope(
  fun = Kres, 
  secondary.pp, 
  model = fit.sec, 
  nsim = 19, 
  correction = c("Ripley")) # Examine 
```

```
## Generating 19 simulations of CSR  ...
## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,  19.
## 
## Done.
```

```r
par(mfrow = c(1,2)) # Plot with two columns

# png(filename = "statplot.png", width = 600, height = 600, units = "px")

plot(fit.res, main="Residual K")

abline(v = 0.14, col = "purple")
abline(v = 0.18, col = "purple")

plot(predict(fit.sec), main = "Predicted distribution", col = rainbow(255))
points(secondary.pp)

# dev.off()

plot(fit.res, main = "Residual K")

abline(v = 0.14, col = "purple")
abline(v = 0.18, col = "purple")

plot(predict(fit.sec), main = "Predicted distribution", col = rainbow(255))
points(secondary.pp)
```

<img src="01_Riris_files/figure-html/unnamed-chunk-13-1.png" width="672" /><img src="01_Riris_files/figure-html/unnamed-chunk-13-2.png" width="672" />


### Summary

This exercise invites us to: 

a) Think critically about the relationships we think we see in our data,      
b) Formalise our hypotheses about the archaeological record and test them,     
c) Pursue further data collection and analysis when we are proven wrong.    







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
## [1] methods   stats     graphics  grDevices utils     datasets  base     
## 
## other attached packages:
## [1] raster_2.5-8    rgdal_1.2-5     maptools_0.9-2  sp_1.2-4       
## [5] spatstat_1.50-0 rpart_4.1-10    nlme_3.1-131   
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.10         knitr_1.15.1         tensor_1.5          
##  [4] magrittr_1.5         spatstat.utils_1.4-1 lattice_0.20-34     
##  [7] stringr_1.2.0        tools_3.3.3          grid_3.3.3          
## [10] mgcv_1.8-17          deldir_0.1-12        htmltools_0.3.5     
## [13] yaml_2.1.14          abind_1.4-5          goftest_1.0-4       
## [16] rprojroot_1.2        digest_0.6.12        bookdown_0.3        
## [19] Matrix_1.2-8         fftwtools_0.9-8      evaluate_0.10       
## [22] rmarkdown_1.4        polyclip_1.6-1       stringi_1.1.3       
## [25] backports_1.0.5      foreign_0.8-67
```
