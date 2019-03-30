'%ni%' <- Negate ('%in%')

user <- 'Jacob'
setwd(paste0('/Users/', user, '/Dropbox/Work/Tropical_steepness/'))
GEE <- list.files(path = './GEE_output_data/')
GEE_hist <- GEE[grep('hist.csv', GEE)]
GEE_hist_2000 <- GEE_hist[grep('2000', GEE_hist)]
GEE_hist_GL <- GEE_hist[grep('GL', GEE_hist)]
GEE_hist_2000 <- GEE_hist_2000[order(GEE_hist_2000)]
GEE_hist_GL <- GEE_hist_GL[order(GEE_hist_GL)]

GEE_mean <- GEE[GEE %ni% GEE_hist]
GEE_mean <- GEE_mean[order(GEE_mean)]

for(i in 1:length(GEE_mean)){
  gmeans[[i]] <- read.csv(paste0('./GEE_output_data/', GEE_mean[i]))
}



ghists2000 <- list()
ghistsGL <- list()
for(i in 1:length(GEE_hist_2000)){
  ghists2000[[i]] <- read.csv(paste0('./GEE_output_data/', GEE_hist_2000[i]))
  ghistsGL[[i]] <- read.csv(paste0('./GEE_output_data/', GEE_hist_GL[i]))
}

test <- ghistsGL[[3]]

for(i in 1:length(ghistsGL)){
  ghistsGL[[i]] <- as.numeric(unlist(strsplit(qdapRegex::rm_between(ghistsGL[[i]][2]$groups, 
                                                       left = 'gain=-1, histogram={bucketMin=0.0, bucketWidth=0.5, histogram=[', 
                                                       right = '], bucketMeans=[', extract = T)[[1]], split = ", ")))
}

slopedata <- data.frame(Continent = c('Africa', 'Americas', 'Asia'), 
           mean2000 = c(4.655, 5.503, 12.332),
           meanLoss = c(4.576, 4.271, 7.330))
bsd <- t(as.matrix(slopedata[,2:3]))
pdf('Continents_barplot.pdf', width = 5, height = 5)
barplot(bsd, beside = T, names.arg = slopedata$Continent, ylab = 'Average slope (degrees)', 
        ylim = c(0,13))#, legend.text = c('forest cover year 2000', 'forest loss 2000-2014'))
dev.off()

pdf('Legend.pdf', width = 5, height = 5)
barplot(bsd, beside = T, names.arg = slopedata$Continent, ylab = 'Average slope (degrees)', 
        ylim = c(0,20), legend.text = c('forest cover year 2000', 'forest loss 2000-2014'))
dev.off()


slopedata.country <- data.frame(Country = c('Ghana', 'Congo', 'Uganda', 'Ethiopia'), 
                        mean2000 = c(4.318, 4.063, 11.747, 10.567),
                        meanLoss = c(3.679, 3.403, 10.311, 8.078))
slopedata.country <- slopedata.country[c(2,1,4,3), ]
bsd.country <- t(as.matrix(slopedata.country[,2:3]))

pdf('Africa_countries_barplot.pdf', width = 5, height = 5)
barplot(bsd.country, beside = T, names.arg = slopedata.country$Country,
        ylim = c(0, 12))
dev.off()





for(i in 1:length(ghists2000)){
  ghists2000[[i]] <- as.numeric(unlist(strsplit(qdapRegex::rm_between(ghists2000[[i]][2]$groups, 
                                                                    left = 'extent=1, histogram={bucketMin=0.0, bucketWidth=0.5, histogram=[', 
                                                                    right = '], bucketMeans=[', extract = T)[[1]], split = ", ")))
}

normLoss <- list()
normEx <- list()
normdiff <- list()
for(i in 1:length(ghists2000)){
  normLoss[[i]] <- ghistsGL[[i]]/sum(ghistsGL[[i]]) 
  normEx[[i]] <- ghists2000[[i]]/sum(ghists2000[[i]])
  if(length(normLoss[[i]]) > length(normEx[[i]])){
    normEx[[i]] <- c(normEx[[i]], rep(0, length(normLoss[[i]]) - length(normEx[[i]])))
  }else if(length(normLoss[[i]]) < length(normEx[[i]])){
    normLoss[[i]] <- c(normLoss[[i]], rep(0, length(normEx[[i]]) - length(normLoss[[i]])))
  }
  normdiff[[i]] <- normEx[[i]] - normLoss[[i]]
}

barplot(normdiff[[3]])
barplot(normLoss[[1]])
barplot(normEx[[1]])
