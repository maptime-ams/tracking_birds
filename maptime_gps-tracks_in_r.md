---
title: "Exploring gps-tracks in R"
author: "Emiel van Loon"
date: "June 11, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction


```{r libraries}
library(adehabitatLT)	# to handle and manipulate GPS tracks
library(lubridate)    # to handle time
library(maps)				  # to display maps
library(mapproj)			# to handle projection systems 
library(mapdata)			# more detailed maps
library(circular)			# to handle and plot circular data 
library(ggmap) # to use Google Earth satellite images in R

source('pt2pt_fxns.R')
```

## Load and preprocess the data

Eight Lesser black-backed gulls over the period of one month (June 2010).

```{r load_csv}
gulls <- read.csv('tracking_data.csv')
```

If you are using RStudio, you see in the invironment already: 66146 observations for 10 variables.

```{r explore_data_structure}
str(gulls)
```

```{r explore_data}
summary(gulls)
head(gulls,n=10)
tail(gulls,n=5)
```

```{r convert_datetime}
gulls$date_time <- as.POSIXct(gulls$date_time, format="%Y-%m-%d %H:%M:%S", tz='UTC')
gulls$hr <- hour(gulls$date_time)   # returns hour (0-23)
gulls$day <- day(gulls$date_time)   # returns hour (0-23)

#gulls$day <- as.numeric( format(gulls$date_time, "%d") )
```

```{r ordering}
gulls <- gulls[order(gulls$device_info_serial,gulls$date_time),] 
```

```{r how_many}
( gullun <- unique(gulls$device_info_serial) )
```

```{r mk_subsets}
ss.298 <- subset(gulls, gulls$device_info_serial == "298")
ss.311 <- subset(gulls, gulls$device_info_serial == "311")
ss.317 <- subset(gulls, gulls$device_info_serial == "317")
ss.320 <- subset(gulls, gulls$device_info_serial == "320")
ss.327 <- subset(gulls, gulls$device_info_serial == "327")
ss.329 <- subset(gulls, gulls$device_info_serial == "329")
ss.344 <- subset(gulls, gulls$device_info_serial == "344")
ss.355 <- subset(gulls, gulls$device_info_serial == "355")
```

## Visualise the tracking data

### Basic plotting functions

We use the function 'get_map()' from package ggmap to download a satellite image for Texel, and subsequently plot the gull tracks on top of it, with different colours per bird.

We create the figure using the package 'ggplot2' (http://docs.ggplot2.org/current/) and related package 'ggmap' for manipulating Google Earth imagery in R. 

```{r make_maps}
Texel <- get_map(location=c(4.7,53), zoom = 7, maptype ='satellite')		

p1 <- ggmap(Texel) + xlim(2.8,5.65)+ ylim(52.35,54) +
	geom_path(data=gulls,aes(x=longitude,y=latitude,group=device_info_serial,col=factor(device_info_serial)),size=.6)

p1
```

If you are not familiar with ggplot/gmap, it is nice to explore the possibilities. It has the option to update the existing plot through extra commands. Below are some examples.

Specify the colour for each bird.

```{r}
p2 <- p1 + scale_colour_manual(name="Bird",values=c("298"="darkorange2","311"="cornflowerblue","317"="green","320"="yellow","327"="orange","329"="red","344"="purple","355"="pink"))
p2
```

Change the labels at the axes.

```{r}
p3 <- p2 + xlab("Long[°]") + ylab("Lat[°]")
```

Specify lay-out details, using the `theme()` function

```{r}
p4 <- p3 +  theme(legend.position='bottom',
		legend.direction='horizontal',
		legend.title=element_text(size=10,face='bold'),
		axis.text=element_text(size=8,face='italic'),
		axis.title=element_text(size=10,face='bold'))
p4
```

Separate figures for each bird or time period using `facet_grid()` and `facet_wrap()`.

```{r}
p4 + facet_wrap(~device_info_serial,ncol=4)
```



### Calculating and visualising additional statistics

The gull data does contain a column 'speed_2d'. This is the speed measured by the GPs-tracker over a very short time-interval. While on average it gives a good estimate of the speed, it shows a large variation and does sometimes contain outliers.

Another way to calculate speed is by dividing distance traveled by the time between consecutive GPS-points. We can make this calculation using the functions `pt2pt.distance()` and `pt2pt.duration()`, but have to do this per bird. We call the result 'trajectory speed'. Below this calculation is done for only three birds.

```{r}
ss.298$distance <- pt2pt.distance(ss.298$latitude, ss.298$longitude)
ss.311$distance <- pt2pt.distance(ss.311$latitude, ss.311$longitude)
ss.317$distance <- pt2pt.distance(ss.317$latitude, ss.317$longitude)

ss.298$duration <- pt2pt.duration(ss.298$date_time)
ss.311$duration <- pt2pt.duration(ss.311$date_time)
ss.317$duration <- pt2pt.duration(ss.317$date_time)

ss.298$traj_speed <- pt2pt.speed(ss.298$distance, ss.298$duration)
ss.311$traj_speed <- pt2pt.speed(ss.311$distance, ss.311$duration)
ss.317$traj_speed <- pt2pt.speed(ss.317$distance, ss.317$duration)
```


If you compare the instantaneous speeds with the trajectory speed, you see indeed that there are more extremes in the instantaneous speed (more very low and very high speeds).
```{r}
summary(ss.298$speed_2d)
summary(ss.298$traj_speed)

hist(ss.298$speed_2d)
hist(ss.298$traj_speed)
```

Similar to what we did with speed, we can calculate the direction of each trajectory segment using the function `pt2pt.direction()`

```{r}
ss.298$direction <- pt2pt.direction(ss.298$latitude,ss.298$longitude)
ss.311$direction <- pt2pt.direction(ss.311$latitude,ss.311$longitude)
ss.317$direction <- pt2pt.direction(ss.317$latitude,ss.317$longitude)
```

This stores the direction as a normal number in degrees, with North = 0 but also 360. We do however need an additional step to ensure that this direction is understood properly as a circular variable (it should be clear that directions of 0 and 180 degrees are much further apart than 0 and 270 degrees).

```{r}
ss.298$c_direction <- circular(ss.298$direction, units = 'degrees', template ='geographics')
ss.311$c_direction <- circular(ss.311$direction, units = 'degrees', template ='geographics')
ss.317$c_direction <- circular(ss.317$direction, units = 'degrees', template ='geographics')
```


```{r}
hist(ss.311$direction, col='steelblue1')
```

```{r}
plot.circular(ss.311$c_direction, stack=T, shrink=2, bins=90, cex=.1) ## See ?plot.circular for options
lines(density(ss.311$c_direction, bw=35, na.rm=T), col='red', lwd=3) ## See ?lines.circular for options
axis.circular(at=circular(seq(0, (2*pi)-(pi/2), pi/2)), labels=c("E","N","W","S")) ## See ?axis.circular for options
```


```{r}
windrose(ss.311$c_direction, log(ss.311$traj_speed), increment=.5, fill.col=topo.colors(7))
```

### Challenges 

1. It would be nice to combine maps and statistical graphs to make cartograms, which would for instance visualise the distribution (as in a histogram or windrose over space).

2. There are quite a few nice plotting platforms in R. Some of these contain interactivity like `leaflet`. It would be nice to explore the plotting-options using leaflet.


## Explore time budgets

To learn more about the behaviour and ecology of our birds, it is nice to make time-budgets. A time-budget specifies how much time a bird is spending in different habitats or on different behaviours.

Here we will focus on how much time the gulls spend in different habitats during their breeding period in June. We will use a map with major geographical land-units for North-Holland with relevant units for the lesser black-backed gull. The spatial units are: Wadden Sea, North Sea, Texel, Mainland and Beach. 

```{r}
load('./rGU.rda')   
```

The datafile rGU contains a map (`rGU`). It is stored in a commonly used format for spatial raster data in R (a `RasterLayer`). Let's have a look wich categories this map contains.

```{r}
( lbl_rGU <-levels(rGU) )
```

And also visualise the map.

```{r}
library(rasterVis)
levelplot(rGU, col.regions=c("yellow","green","blue","red","brown"))
```

So how do we collect data from this map? First of all, this map is not in a latitude-longitude coordinate system (the GPS system is called `WGS84`) but the projected Dutch coordinate system (called `RDnew`).
So we have to either transform the GPS-trajectory data to the Map-coordinate system or vice versa. We choose for converting the GPS data to the Map.

```{r}
library(sp)
library(rgdal)
sgull <- SpatialPointsDataFrame( cbind(gulls$longitude,gulls$latitude),
          gulls[,c('device_info_serial','date_time','altitude','speed_2d')] )

# specify that this data is in WSG84 lat-lon  
proj4string(sgull) = CRS("+init=epsg:4326")        
  
# transform the data to the Dutch coordinate system
sgull = spTransform(sgull, CRS("+init=epsg:28992")) 
```

If we check the transformed data in sgull, we see that the coordinates are indeed changed from latitude longitude values into the RD-values (which range from 0 to 300000 in the West-East direction and 300000 to 600000 in the South-North direction).

```{r}
head(sgull)
head(coordinates(sgull))
```

With the data in the same coordinate system, we can record the codes of the geographical units that are visited by the gulls, using the function `extract()`

```{r}
gulls$GU <- extract(rGU,sgull)

# the result in gulls$GU are integers, we turn these into categories 
gulls$GUc <- factor(gulls$GU, levels=1:5, labels=lbl_rGU[[1]]$levels)
```

It is good to note that any points outside the range of the map are coded as NA. 
In this case there are `r sum( is.na(gulls$GU) )` GPS points outside the range.

The resulting data in GU can now be used to quantify habitat use.
For example, if we assume that the duration of the time intervals is comparable between
the birds, we can simply count the number of GPS points in each geographical unit per bird.
This is done below.

```{r}
(bird_GU_time <- table(gulls$device_info_serial,gulls$GUc))
```

This kind of table is hard to read, so let's transform it to a proportional table,
where the fractions sum to 1 per bird (the numbers are also rounded to 2 digits because otherwise they are dificult to read).

```{r}
( bird_GU_time_pr <- round( prop.table(bird_GU_time, margin=1),2) )
```

Follow-up steps in the analysis are often:

- investigating whether the observed difference in habitat use between individuals are in fact large or that they are doing more or less the same;
- investigating whether the selected has been selected is proportional to what is available.

These are statistical analyses. Here we will consider the second analysis.
This analysis requires to first make a subjective choice of what we consider to be 
'available' to these birds. And once that is done, a statistical test can be applied
which compares the selected geographical units by the birds with the available 
area in each of the units (a test for goodness of fit; we use the chi-squared test in this case).

```{r}

# note: the raw values in the rGU RasterLayer are stored in the 'slot' @data@values 
available <- table(rGU@data@values)
selected <- table(gulls$GU)

prop_expected <- available/sum(available)
prop_selected <- selected/sum(selected)

chisq.test(selected,p=expected)
```

The (extremely small) p-value of this test tells is that the birds do not use the landscape proportional to their availability but select for certain uses.


### Challenges 

1. The comparison we made between 'used' and 'available' habitats is dominated by the fact that the birds spend a lot of time in the colony (which is on Texel). It is especially interesting to know where the birds spend their time when not in the colony. So we should first remove the points that fall in this colony and then redo the time-budget analysis.

2. In spite of what we assumed, the time-steps in the data set are not homogeneous over time and also not exactly equal among individuals. We can take this problem into account by calculating the time between different points (see the function `pt2pt.duration()` fro the previous chapter) and subsequently using this information to calculate habitat use more precise.


