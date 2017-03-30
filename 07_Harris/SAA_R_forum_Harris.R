## ----functions-----------------------------------------------------------
# predict == predicted value from model raster
# response == site present (1) or site absent (0) at each predicted cell location
balance_threshold <- function(predict, response) {
  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
  auc <- ROCR::performance(ROCR::prediction(predict, response), "auc")
  auc <- round(auc@y.values[[1]],3)
  df <- data.frame(Weight = perf@alpha.values[[1]], 
                   Spec = perf@x.values[[1]], 
                   Sens = perf@y.values[[1]],
                   Back_pcnt = 1 - perf@x.values[[1]],
                   Xover = abs((perf@y.values[[1]] + (1-perf@x.values[[1]]))-1))
  df$kg <- 1-((1-df$Spec)/df$Sens)
  df$reach <- 1-((1-df$Sens)/df$Spec)
  df$reach <- ifelse(df$reach == 1, 0, df$reach) # removing reach == 1
  df <- data.frame(apply(df,2,function(x) round(x,3)))
  sens_spec <- df[which.max(df$Sens + df$Spec), "Weight"]
  xover <- df[which.min(df$Xover), "Weight"]
  kg <- df[which.max(df$kg), "Weight"]
  reach <- df[which.max(df[which(df$reach < 1),"reach"]), "Weight"] # max where it is not == 1
  list(df = df, sens_spec = sens_spec, xover = xover, 
       auc = auc, kg = kg, reach = reach)
}


## ----packages, message=FALSE, warning=FALSE------------------------------
library("raster")       # for raster manipulation
library("rgdal")        # for raster processing
library("dplyr")        # for data processing
library("mapview")      # for interactive map plots
library("ggplot2")      # for plotting results
library("ROCR")         # for model validation functions
library("RColorBrewer") # for raster color scale
library("knitr")        # for printing nicer tables
library("viridis")      # for color scale in ggplot

## ----load_raster_data----------------------------------------------------
data_loc <- "/Users/mattharris/Dropbox/R/SAA_2017_Forum/clip_raster/"
slope <- raster(paste0(data_loc, "slope.tif"))
ed_h2 <- raster(paste0(data_loc, "ed_h2.tif"))
ed_h4 <- raster(paste0(data_loc, "ed_h4.tif"))
sites <- raster(paste0(data_loc, "sites.tif"))
raster_vars <- stack(slope, ed_h2, ed_h4, sites)

## ----plot_rasters, echo=FALSE--------------------------------------------
plot(raster_vars)

## ----construct_weights---------------------------------------------------
### Slope weighting Models###
slp_from <- c(0, 3, 5, 8, 15)
slp_to   <- c(3, 5, 8, 15, 99999)
slp_wght <- c(50, 30, 15, 5, 0)
slp_rcls<- cbind(slp_from, slp_to, slp_wght) 

### Dist to h20 weighting Models###
h20_from <- c(0, 100, 200, 400, 800)
h20_to   <- c(100, 200, 400, 800, 9999)
h20_wght <- c(60, 25, 10, 4, 1)
h20_rcls <- cbind(h20_from, h20_to, h20_wght) 

### Dist to wetland weighting Models###
wtl_from <- c(0, 100, 200, 400, 800)
wtl_to   <- c(100,200, 400, 800, 9999)
wtl_wght <- c(35, 25, 20, 15, 5)
wtl_rcls <- cbind(wtl_from, wtl_to, wtl_wght) 

print(slp_rcls)
print(h20_rcls)
# an example of a more fully formatted table
knitr::kable(wtl_rcls, digits = 0,
             col.names = c("From", "To", "Weight"),
             caption = "Sensitivity Weights for Distance (m) to Wetlands (NWD)")

## ------------------------------------------------------------------------
raster_vars[["slope"]] <- reclassify(raster_vars[["slope"]], slp_rcls)
raster_vars[["ed_h2"]] <- reclassify(raster_vars[["ed_h2"]], h20_rcls)
raster_vars[["ed_h4"]] <- reclassify(raster_vars[["ed_h4"]], wtl_rcls)

## ----plot_reclass, echo=FALSE--------------------------------------------
plot(raster_vars, col = brewer.pal(9 , "Blues"))

## ----sum_rasters---------------------------------------------------------
model_sum <- sum(raster_vars[[1:3]])

## ----plot_sum, echo=FALSE------------------------------------------------
plot(model_sum, col = brewer.pal(11, "PiYG"))

## ------------------------------------------------------------------------
coords <- coordinates(model_sum)
x <- data.frame(x = coords[,"x"], y = coords[,"y"], value = as.vector(model_sum))
ggplot(x, aes(x = x, y = y, fill = value)) +
  geom_raster(hjust = 0, vjust = 0) +
  theme_minimal() +
  viridis::scale_fill_viridis() +
  labs(title = "Summed Sensitivity Weights",
       caption = "Projection: Albers Conic Equal Area")

## ----clip_sites----------------------------------------------------------
sites_sum <- mask(model_sum, raster_vars[["sites"]])

## ----plot_sites, echo=FALSE----------------------------------------------
plot(sites_sum, col = brewer.pal(11, "PiYG"))

## ----mapview_weights-----------------------------------------------------
mapview(model_sum, col.regions = viridisLite::viridis, alpha = 0.75, maxpixels =  897336) +
  mapview(sites_sum, col.regions = viridisLite::magma, maxpixels =  897336)

## ----plot_distribution---------------------------------------------------
# build a data.frame of weights and assign labels for sites and background
sum_dat <- data.frame(wght = c(model_sum@data@values, sites_sum@data@values),
                      model = c(rep("background", length(model_sum)),
                                rep("site", length(sites_sum)))) %>%
  mutate(class = ifelse(model == "site", 1, 0)) %>%
  na.omit()

# plot the wieghts as a density to compare
ggplot(sum_dat, aes(x = wght, group = model, fill = model)) +
  geom_density(alpha = 0.55, adjust = 2) +
  scale_x_continuous(limits = c(0,200), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x="Summed Weight", y="Density",
       title="Density of Summed Weights by Class") +
  theme_bw() 

## ------------------------------------------------------------------------
# plot the wieghts as a boxplots to compare
ggplot(sum_dat, aes(x = model, y = wght, group = model, fill = model)) +
  geom_boxplot() +
  labs(x="Classification", y="Summed Weight",
       title="Summed Weights by Class") +
  theme_bw() 

## ----evaluate_model------------------------------------------------------
# model_pred <- prediction(sum_dat$wght, sum_dat$class) %>%
#   performance(., "sens", "spec")

model_pref <- balance_threshold(sum_dat$wght, sum_dat$class)

## ----plot_crossover------------------------------------------------------
xover_dat <-  tidyr::gather(model_pref$df, metric, value, -Weight, -kg, -Back_pcnt, -Xover, -reach)
threshold_dat <- data.frame(threshold = c("kg", "reach", "X-over", "Sens-Spec"),
                            weight =  c(model_pref$kg, model_pref$reach, model_pref$xover, model_pref$sens_spec))
ggplot() +
  geom_line(data = xover_dat, aes(x = Weight, y = value, group = metric, color = metric), size=1) +
  geom_linerange(data = threshold_dat, aes(x = weight, ymin = 0, ymax = 1, linetype = threshold)) +
  scale_x_continuous(breaks=seq(0,200,5), labels = seq(0,200,5)) +
  scale_y_continuous(breaks=seq(0,1,0.1), labels = seq(0,1,0.1)) +
  labs(title = "Sensitivity and Specificity at Optimized Metrics") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

## ----reclassify_model----------------------------------------------------
class_rcls <- matrix(c(-Inf, model_pref$sens_spec, 0,
                       model_pref$sens_spec, Inf, 1), ncol=3, byrow=TRUE)
model_class <- reclassify(model_sum, class_rcls, right = FALSE)

## ----plot_model, echo=FALSE----------------------------------------------
plot(model_class, col = c("white", "darkgreen"))

## ------------------------------------------------------------------------
model_pref[["df"]] %>%
  dplyr::filter(Weight == model_pref$sens_spec)

## ----mapview_prediction--------------------------------------------------
mapview(model_class, col.regions = viridisLite::viridis, alpha = 0.75, maxpixels =  897336) +
  mapview(sites_sum, col.regions = viridisLite::magma, maxpixels =  897336)

## ------------------------------------------------------------------------
sessionInfo()

