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
