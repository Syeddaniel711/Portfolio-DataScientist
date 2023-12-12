###Project
library(imager)
library(magick)
library(EBImage)
library(purrr)
library(igraph)
library(OpenImageR)
library(SuperpixelImageSegmentation)
library(dplyr)
library(plyr)
library(ggplot2)
##load image
Animal1 <- load.image("C:/Users/syedd/OneDrive/Desktop/extdata/animal1.jpg")
Animal2 <- load.image("C:/Users/syedd/OneDrive/Desktop/extdata/animal2.jpg")
Building1 <- load.image("C:/Users/syedd/OneDrive/Desktop/extdata/Building1.jpg")
Building2 <- load.image("C:/Users/syedd/OneDrive/Desktop/extdata/Building2.jpg")
Flower1 <- load.image("C:/Users/syedd/OneDrive/Desktop/extdata/Flower1.jpg")
Flower2 <- load.image("C:/Users/syedd/OneDrive/Desktop/extdata/Flower2.jpg")
Person1 <- load.image("C:/Users/syedd/OneDrive/Desktop/extdata/Person1.jpg")
Person2 <- load.image("C:/Users/syedd/OneDrive/Desktop/extdata/Person2.jpg")
Scenery1 <- load.image("C:/Users/syedd/OneDrive/Desktop/extdata/Scenery1.jpg")
Scenery2 <- load.image("C:/Users/syedd/OneDrive/Desktop/extdata/Scenery2.jpg")
##i) Edge detection
#Animal
Animal1.xedges <- deriche(Animal1,2,order=2,axis="x") #Edge detector along x-axis
Animal1.yedges <- deriche(Animal1,2,order=2,axis="y") #Edge detector along y-axis'

Animal2.xedges <- deriche(Animal2,2,order=2,axis="x") #Edge detector along x-axis
Animal2.yedges <- deriche(Animal2,2,order=2,axis="y") #Edge detector along y-axis'

#Building
Building1.xedges <- deriche(Building1,2,order=2,axis="x") 
Building1.yedges <- deriche(Building1,2,order=2,axis="y")

Building2.xedges <- deriche(Building2,2,order=2,axis="x") 
Building2.yedges <- deriche(Building2,2,order=2,axis="y") 

#Flower
Flower1.xedges <- deriche(Flower1,2,order=2,axis="x") 
Flower1.yedges <- deriche(Flower1,2,order=2,axis="y")

Flower2.xedges <- deriche(Flower2,2,order=2,axis="x") 
Flower2.yedges <- deriche(Flower2,2,order=2,axis="y")

#Person
Person1.xedges <- deriche(Person1,2,order=2,axis="x") 
Person1.yedges <- deriche(Person1,2,order=2,axis="y")

Person2.xedges <- deriche(Person2,2,order=2,axis="x") 
Person2.yedges <- deriche(Person2,2,order=2,axis="y")

#Scenery
Scenery1.xedges <- deriche(Scenery1,2,order=2,axis="x") 
Scenery1.yedges <- deriche(Scenery1,2,order=2,axis="y")

Scenery2.xedges <- deriche(Scenery2,2,order=2,axis="x") 
Scenery2.yedges <- deriche(Scenery2,2,order=2,axis="y")
#---------------------------------------------------------
##ii) Splitting
#Animal
imsplit(Animal1,"c")
imsplit(Animal1,"c") %>% laply(mean)
imsplit(Animal1,"x") %>% laply(mean) %>% head
imsplit(Animal1,"c") %>% purrr::map_dbl(mean)
imsplit(Animal1,"x",4) %>% plot

imsplit(Animal2,"c")
imsplit(Animal2,"c") %>% laply(mean)
imsplit(Animal2,"x") %>% laply(mean) %>% head
imsplit(Animal2,"c") %>% purrr::map_dbl(mean)
imsplit(Animal2,"x",4) %>% plot

#Flower
imsplit(Flower1,"c")
imsplit(Flower1,"c") %>% laply(mean)
imsplit(Flower1,"x") %>% laply(mean) %>% head
imsplit(Flower1,"c") %>% purrr::map_dbl(mean)
imsplit(Flower1,"x",4) %>% plot

imsplit(Flower2,"c")
imsplit(Flower2,"c") %>% laply(mean)
imsplit(Flower2,"x") %>% laply(mean) %>% head
imsplit(Flower2,"c") %>% purrr::map_dbl(mean)
imsplit(Flower2,"x",4) %>% plot

#Scenery
imsplit(Scenery1,"c")
imsplit(Scenery1,"c") %>% laply(mean)
imsplit(Scenery1,"x") %>% laply(mean) %>% head
imsplit(Scenery1,"c") %>% purrr::map_dbl(mean)
imsplit(Scenery1,"x",4) %>% plot

imsplit(Scenery2,"c")
imsplit(Scenery2,"c") %>% laply(mean)
imsplit(Scenery2,"x") %>% laply(mean) %>% head
imsplit(Scenery2,"c") %>% purrr::map_dbl(mean)
imsplit(Scenery2,"x",4) %>% plot

#Person
imsplit(Person1,"c")
imsplit(Person1,"c") %>% laply(mean)
imsplit(Person1,"x") %>% laply(mean) %>% head
imsplit(Person1,"c") %>% purrr::map_dbl(mean)
imsplit(Person1,"x",4) %>% plot

imsplit(Person2,"c")
imsplit(Person2,"c") %>% laply(mean)
imsplit(Person2,"x") %>% laply(mean) %>% head
imsplit(Person2,"c") %>% purrr::map_dbl(mean)
imsplit(Person2,"x",4) %>% plot

#Building
imsplit(Building1,"c")
imsplit(Building1,"c") %>% laply(mean)
imsplit(Building1,"x") %>% laply(mean) %>% head
imsplit(Building1,"c") %>% purrr::map_dbl(mean)
imsplit(Building1,"x",4) %>% plot

imsplit(Building2,"c")
imsplit(Building2,"c") %>% laply(mean)
imsplit(Building2,"x") %>% laply(mean) %>% head
imsplit(Building2,"c") %>% purrr::map_dbl(mean)
imsplit(Building2,"x",4) %>% plot
#----------------------------------------------------------
##iii) Resize, rotate, ext
#Animal
thmb1 <- resize(Animal1,round(width(Animal1)/30),round(height(Animal1)/30))
plot(thmb1,main="Thumbnail") #Pixellated Animal1
thmb1 <- resize(Animal1,-10,-10)
imrotate(Animal1,30) %>% plot(main="Rotating") #Rotate

thmb2 <- resize(Animal2,round(width(Animal2)/50),round(height(Animal2)/50))
plot(thmb2,main="Thumbnail") #Pixellated Animal1
thmb2 <- resize(Animal2,-10,-10)
imrotate(Animal2,30) %>% plot(main="Rotating") #Rotate

#Building
thmb1 <- resize(Building1,round(width(Building1)/30),round(height(Building1)/30))
plot(thmb1,main="Thumbnail") 
thmb1 <- resize(Building1,-10,-10)
imrotate(Building1,30) %>% plot(main="Rotating") 

thmb2 <- resize(Building2,round(width(Building2)/50),round(height(Building2)/50))
plot(thmb2,main="Thumbnail") 
thmb2 <- resize(Building2,-10,-10)
imrotate(Building2,30) %>% plot(main="Rotating")

#Flower
thmb1 <- resize(Flower1,round(width(Flower1)/30),round(height(Flower1)/30))
plot(thmb1,main="Thumbnail") 
thmb1 <- resize(Flower1,-10,-10)
imrotate(Flower1,30) %>% plot(main="Rotating") 

thmb2 <- resize(Flower2,round(width(Flower2)/50),round(height(Flower2)/50))
plot(thmb2,main="Thumbnail") 
thmb2 <- resize(Flower2,-10,-10)
imrotate(Flower2,30) %>% plot(main="Rotating")

#Person
thmb1 <- resize(Person1,round(width(Person1)/30),round(height(Person1)/30))
plot(thmb1,main="Thumbnail") 
thmb1 <- resize(Person1,-10,-10)
imrotate(Person1,30) %>% plot(main="Rotating") 

thmb2 <- resize(Person2,round(width(Person2)/50),round(height(Person2)/50))
plot(thmb2,main="Thumbnail") 
thmb2 <- resize(Person2,-10,-10)
imrotate(Person2,30) %>% plot(main="Rotating")

#Scenery
thmb1 <- resize(Scenery1,round(width(Scenery1)/30),round(height(Scenery1)/30))
plot(thmb1,main="Thumbnail") 
thmb1 <- resize(Scenery1,-10,-10)
imrotate(Scenery1,30) %>% plot(main="Rotating") 

thmb2 <- resize(Scenery2,round(width(Scenery2)/50),round(height(Scenery1)/50))
plot(thmb2,main="Thumbnail") 
thmb2 <- resize(Scenery2,-10,-10)
imrotate(Scenery2,30) %>% plot(main="Rotating")
#------------------------------------------------------------
##iv) Filtering image
#Animal
path1= file.path(getwd(), 'extdata', 'animal1.jpg')#desktop
x1 = readImage(path1)
display(x1, title='Sample')
f <- makeBrush(21, shape='disc', step=FALSE)
display(f, title='Disc filter') #Low-pass disc-shaped filter
f <- f/sum(f)
y <- filter2(x1, f)
display(y, title='Filtered image')

path2 = file.path(getwd(), 'extdata', 'animal2.jpg')
x2 = readImage(path2)
display(x2, title='Sample')
f <- makeBrush(21, shape='disc', step=FALSE)
display(f, title='Disc filter') #Low-pass disc-shaped filter
f <- f/sum(f)
y <- filter2(x2, f)
display(y, title='Filtered image')

#Flower
path1 = file.path(getwd(), 'extdata', 'Flower1.jpg')
x1 = readImage(path1)
display(x1, title='Sample')
f <- makeBrush(21, shape='disc', step=FALSE)
display(f, title='Disc filter') 
f <- f/sum(f)
y <- filter2(x1, f)
display(y, title='Filtered image')

path2 = file.path(getwd(), 'extdata', 'Flower2.jpg')
x2 = readImage(path2)
display(x2, title='Sample')
f <- makeBrush(21, shape='disc', step=FALSE)
display(f, title='Disc filter') 
f <- f/sum(f)
y <- filter2(x2, f)
display(y, title='Filtered image')

#Scenery
path1 = file.path(getwd(), 'extdata', 'Scenery1.jpg')
x1 = readImage(path1)
display(x1, title='Sample')
f <- makeBrush(21, shape='disc', step=FALSE)
display(f, title='Disc filter') 
f <- f/sum(f)
y <- filter2(x1, f)
display(y, title='Filtered image')

path2 = file.path(getwd(), 'extdata', 'Scenery2.jpg')
x2 = readImage(path2)
display(x2, title='Sample')
f <- makeBrush(21, shape='disc', step=FALSE)
display(f, title='Disc filter') 
f <- f/sum(f)
y <- filter2(x2, f)
display(y, title='Filtered image')

#Person
path1 = file.path(getwd(), 'extdata', 'Person1.jpg')
x1 = readImage(path1)
display(x1, title='Sample')
f <- makeBrush(21, shape='disc', step=FALSE)
display(f, title='Disc filter') 
f <- f/sum(f)
y <- filter2(x1, f)
display(y, title='Filtered image')

path2 = file.path(getwd(), 'extdata', 'Person2.jpg')
x2 = readImage(path2)
display(x2, title='Sample')
f <- makeBrush(21, shape='disc', step=FALSE)
display(f, title='Disc filter') 
f <- f/sum(f)
y <- filter2(x2, f)
display(y, title='Filtered image')

#Building
path1 = file.path(getwd(), 'extdata', 'Building1.jpg')
x1 = readImage(path1)
display(x1, title='Sample')
f <- makeBrush(21, shape='disc', step=FALSE)
display(f, title='Disc filter') 
f <- f/sum(f)
y <- filter2(x1, f)
display(y, title='Filtered image')

path2 = file.path(getwd(), 'extdata', 'Building2.jpg')
x2 = readImage(path2)
display(x2, title='Sample')
f <- makeBrush(21, shape='disc', step=FALSE)
display(f, title='Disc filter') 
f <- f/sum(f)
y <- filter2(x2, f)
display(y, title='Filtered image')
----------------------------------------------------------
##V)Rectangular, circular & fuzzy selection.
#Animal
imsub(Animal1,x %inr% c(0,1500),y %inr% c(0,2500)) %>% plot
dot<-(Xc(Animal1) - 200)^2 + (Yc(Animal1) - 350)^2 < 150^2
px.flood(Animal1,100,100,sigma=.1) %>% highlight(col="darkred")
highlight(dot)

imsub(Animal2,x %inr% c(0,3000),y %inr% c(0,2500)) %>% plot
dot<-(Xc(Animal2) - 200)^2 + (Yc(Animal2) - 350)^2 < 150^2
px.flood(Animal2,100,100,sigma=.1) %>% highlight(col="darkred")
highlight(dot)

#Flower
imsub(Flower1,x %inr% c(500,3000),y %inr% c(1000,4000)) %>% plot
dot<-(Xc(Flower1) - 2000)^2 + (Yc(Flower1) - 1050)^2 < 150^2
px.flood(Flower1,2500,1000,sigma=.1) %>% highlight(col="darkred")
highlight(dot)

imsub(Flower2,x %inr% c(500,2500),y %inr% c(1000,4000)) %>% plot
dot<-(Xc(Flower2) - 1500)^2 + (Yc(Flower2) - 1400)^2 < 150^2
px.flood(Flower2,1500,1500,sigma=.1) %>% highlight(col="darkred")
highlight(dot)

#Person
imsub(Person1,x %inr% c(500,1000),y %inr% c(1000,2000)) %>% plot
dot<-(Xc(Person1) - 500)^2 + (Yc(Person1) - 500)^2 < 150^2
px.flood(Person1,100,100,sigma=.1) %>% highlight(col="darkred")
highlight(dot)

imsub(Person2,x %inr% c(500,2500),y %inr% c(1000,4000)) %>% plot
dot<-(Xc(Person2) - 1500)^2 + (Yc(Person2) - 1400)^2 < 150^2
px.flood(Person2,500,1000,sigma=.1) %>% highlight(col="darkred")
highlight(dot)

#Scenery
imsub(Scenery1,x %inr% c(500,3000),y %inr% c(1000,4000)) %>% plot
dot<-(Xc(Scenery1) - 2000)^2 + (Yc(Scenery1) - 1050)^2 < 150^2
px.flood(Scenery1,2500,1000,sigma=.1) %>% highlight(col="darkred")
highlight(dot)

imsub(Scenery2,x %inr% c(500,2500),y %inr% c(1000,4000)) %>% plot
dot<-(Xc(Scenery2) - 1500)^2 + (Yc(Scenery2) - 1400)^2 < 150^2
px.flood(Scenery2,1000,1000,sigma=.1) %>% highlight(col="darkred")
highlight(dot)

#Building
imsub(Building1,x %inr% c(500,3000),y %inr% c(1000,4000)) %>% plot
dot<-(Xc(Building1) - 2000)^2 + (Yc(Scenery1) - 1050)^2 < 150^2
px.flood(Building1,2500,1000,sigma=.1) %>% highlight(col="darkred")
highlight(dot)

imsub(Building2,x %inr% c(500,2500),y %inr% c(1000,4000)) %>% plot
dot<-(Xc(Building2) - 1500)^2 + (Yc(Building2) - 1400)^2 < 150^2
px.flood(Building2,1000,1000,sigma=.1) %>% highlight(col="darkred")
highlight(dot)
#-----------------------------------------------------
##Vi)blurr and shapen
#Animal
Animal1.blurry <- isoblur(Animal1,10)%>% plot #Blurry parrots
Animal1.sharpen<-imsharpen(Animal1,2) %>% plot #sharpen
px <- px.flood(Animal1,200,300,sigma=.58) ##Select image region to blur
plot(Animal1);highlight(px)

Animal2.blurry <- isoblur(Animal2,10)%>% plot #Blurry parrots
Animal2.sharpen<-imsharpen(Animal2,2) %>% plot #sharpen
px <- px.flood(Animal2,200,300,sigma=.58) ##Select image region to blur
plot(Animal2);highlight(px)

#Flower
Flower1.blurry <- isoblur(Flower1,10)%>% plot #Blurry parrots
Flower1.sharpen<-imsharpen(Flower1,2) %>% plot #sharpen
px <- px.flood(Flower1,200,300,sigma=.58) ##Select image region to blur
plot(Flower1);highlight(px)

Flower2.blurry <- isoblur(Flower2,10)%>% plot #Blurry parrots
Flower2.sharpen<-imsharpen(Flower2,2) %>% plot #sharpen
px <- px.flood(Flower2,200,300,sigma=.58) ##Select image region to blur
plot(Flower2);highlight(px)

#Flower
Flower1.blurry <- isoblur(Flower1,10)%>% plot #Blurry parrots
Flower1.sharpen<-imsharpen(Flower1,2) %>% plot #sharpen
px <- px.flood(Flower1,200,300,sigma=.58) ##Select image region to blur
plot(Flower1);highlight(px)

Flower2.blurry <- isoblur(Flower2,10)%>% plot #Blurry parrots
Flower2.sharpen<-imsharpen(Flower2,2) %>% plot #sharpen
px <- px.flood(Flower2,200,300,sigma=.58) ##Select image region to blur
plot(Flower2);highlight(px)

#Scenery
Scenery1.blurry <- isoblur(Scenery1,10)%>% plot #Blurry parrots
Scenery1.sharpen<-imsharpen(Scenery1,2) %>% plot #sharpen
px <- px.flood(Scenery1,200,300,sigma=.58) ##Select image region to blur
plot(Scenery1);highlight(px)

Scenery2.blurry <- isoblur(Scenery2,10)%>% plot #Blurry parrots
Scenery2.sharpen<-imsharpen(Scenery2,2) %>% plot #sharpen
px <- px.flood(Scenery2,200,300,sigma=.58) ##Select image region to blur
plot(Scenery2);highlight(px)

#Person
Person1.blurry <- isoblur(Person1,10)%>% plot #Blurry parrots
Person1.sharpen<-imsharpen(Person1,2) %>% plot #sharpen
px <- px.flood(Person1,200,300,sigma=.58) ##Select image region to blur
plot(Person1);highlight(px)

Person2.blurry <- isoblur(Person2,10)%>% plot #Blurry parrots
Person2.sharpen<-imsharpen(Person2,2) %>% plot #sharpen
px <- px.flood(Person2,200,300,sigma=.58) ##Select image region to blur
plot(Person2);highlight(px)

#Building
Building1.blurry <- isoblur(Building1,10)%>% plot #Blurry parrots
Building1.sharpen<-imsharpen(Building1,2) %>% plot #sharpen
px <- px.flood(Building1,200,300,sigma=.58) ##Select image region to blur
plot(Building1);highlight(px)

Person2.blurry <- isoblur(Building2,10)%>% plot #Blurry parrots
Person2.sharpen<-imsharpen(Building2,2) %>% plot #sharpen
px <- px.flood(Building2,200,300,sigma=.58) ##Select image region to blur
plot(Building2);highlight(px)
#---------------------------------------------------------
#vii)segmentation (getwd()=current location for folder 'extdata,)
##Animal
path1 = file.path(getwd(), 'extdata', 'Animal1.jpg')#desktop
im = OpenImageR::readImage(path1)
init = Image_Segmentation$new()

spx = init$spixel_segmentation(input_image = im, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)
str(spx)
OpenImageR::imageShow(spx$AP_image_data)

path2 = file.path(getwd(), 'extdata', 'animal2.jpg')
x = readImage(path2)
im = OpenImageR::readImage(path2)
init = Image_Segmentation$new()
spx = init$spixel_segmentation(input_image = im, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)
str(spx)
OpenImageR::imageShow(spx$AP_image_data)

##Flower
path1 = file.path(getwd(), 'extdata', 'Flower1.jpg')#desktop
im = OpenImageR::readImage(path1)
init = Image_Segmentation$new()

spx = init$spixel_segmentation(input_image = im, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)
str(spx)
OpenImageR::imageShow(spx$AP_image_data)

path2 = file.path(getwd(), 'extdata', 'Flower2.jpg')
x = readImage(path2)
im = OpenImageR::readImage(path2)
init = Image_Segmentation$new()
spx = init$spixel_segmentation(input_image = im, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)
str(spx)
OpenImageR::imageShow(spx$AP_image_data)

##Scenery
path1 = file.path(getwd(), 'extdata', 'Scenery1.jpg')#desktop
im = OpenImageR::readImage(path1)
init = Image_Segmentation$new()

spx = init$spixel_segmentation(input_image = im, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)
str(spx)
OpenImageR::imageShow(spx$AP_image_data)

path2 = file.path(getwd(), 'extdata', 'Scenery2.jpg')
x = readImage(path2)
im = OpenImageR::readImage(path2)
init = Image_Segmentation$new()
spx = init$spixel_segmentation(input_image = im, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)
str(spx)
OpenImageR::imageShow(spx$AP_image_data)

##Person
path1 = file.path(getwd(), 'extdata', 'Person1.jpg')#desktop
im = OpenImageR::readImage(path1)
init = Image_Segmentation$new()

spx = init$spixel_segmentation(input_image = im, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)
str(spx)
OpenImageR::imageShow(spx$AP_image_data)

path2 = file.path(getwd(), 'extdata', 'Person2.jpg')
x = readImage(path2)
im = OpenImageR::readImage(path2)
init = Image_Segmentation$new()
spx = init$spixel_segmentation(input_image = im, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)
str(spx)
OpenImageR::imageShow(spx$AP_image_data)

##Building
path1 = file.path(getwd(), 'extdata', 'Building1.jpg')#desktop
im = OpenImageR::readImage(path1)
init = Image_Segmentation$new()

spx = init$spixel_segmentation(input_image = im, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)
str(spx)
OpenImageR::imageShow(spx$AP_image_data)

path2 = file.path(getwd(), 'extdata', 'Building2.jpg')
x = readImage(path2)
im = OpenImageR::readImage(path2)
init = Image_Segmentation$new()
spx = init$spixel_segmentation(input_image = im, 
                               superpixel = 600, 
                               AP_data = TRUE,
                               use_median = TRUE, 
                               sim_wL = 3, 
                               sim_wA = 10, 
                               sim_wB = 10,
                               sim_color_radius = 10, 
                               verbose = TRUE)
str(spx)
OpenImageR::imageShow(spx$AP_image_data)
#----------------------------------------------------

##viii)Histogram equalization
#Animal
plot(Animal1)
grayscale(Animal1) %>% hist(main="Luminance values in boats picture")
R(Animal1) %>% hist(main="Red channel values in boats picture")
bdf1<- as.data.frame(Animal1)
head(bdf1,3)
bdf <- mutate(bdf1,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf1,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

plot(Animal2)
grayscale(Animal2) %>% hist(main="Luminance values in boats picture")
R(Animal2) %>% hist(main="Red channel values in boats picture")
bdf2<- as.data.frame(Animal2)
head(bdf2,3)
bdf <- mutate(bdf2,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf2,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

#Flower
plot(Flower1)
grayscale(Flower1) %>% hist(main="Luminance values in boats picture")
R(Flower1) %>% hist(main="Red channel values in boats picture")
bdf1<- as.data.frame(Flower1)
head(bdf1,3)
bdf <- mutate(bdf1,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf1,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

plot(Flower2)
grayscale(Flower2) %>% hist(main="Luminance values in boats picture")
R(Flower2) %>% hist(main="Red channel values in boats picture")
bdf2<- as.data.frame(Flower2)
head(bdf2,3)
bdf <- mutate(bdf2,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf2,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

#Scenery
plot(Scenery1)
grayscale(Scenery1) %>% hist(main="Luminance values in boats picture")
R(Scenery1) %>% hist(main="Red channel values in boats picture")
bdf1<- as.data.frame(Scenery1)
head(bdf1,3)
bdf <- mutate(bdf1,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf1,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

plot(Scenery2)
grayscale(Scenery2) %>% hist(main="Luminance values in boats picture")
R(Scenery2) %>% hist(main="Red channel values in boats picture")
bdf2<- as.data.frame(Scenery2)
head(bdf2,3)
bdf <- mutate(bdf2,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf2,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

#Person
plot(Person1)
grayscale(Person1) %>% hist(main="Luminance values in boats picture")
R(Person1) %>% hist(main="Red channel values in boats picture")
bdf1<- as.data.frame(Person1)
head(bdf1,3)
bdf <- mutate(bdf1,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf1,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

plot(Person2)
grayscale(Person2) %>% hist(main="Luminance values in boats picture")
R(Person2) %>% hist(main="Red channel values in boats picture")
bdf2<- as.data.frame(Person2)
head(bdf2,3)
bdf <- mutate(bdf2,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf2,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

#Building
plot(Building1)
grayscale(Building1) %>% hist(main="Luminance values in boats picture")
R(Building1) %>% hist(main="Red channel values in boats picture")
bdf1<- as.data.frame(Building1)
head(bdf1,3)
bdf <- mutate(bdf1,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf1,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

plot(Building2)
grayscale(Building2) %>% hist(main="Luminance values in boats picture")
R(Building2) %>% hist(main="Red channel values in boats picture")
bdf2<- as.data.frame(Building2)
head(bdf2,3)
bdf <- mutate(bdf2,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf2,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)
#-------------------------------------------------------
library(denoiseR)
library(SpatialPack)
Noise1 <- load.image("C:/Users/syedd/OneDrive/Desktop/Noise Image/Noise1.PNG")
plot(Noise1) #Noise1
Noise2 <- load.image("C:/Users/syedd/OneDrive/Desktop/Noise Image/Noise2.PNG") #Load image
plot(Noise2) #Noise2
Noise3 <- load.image("C:/Users/syedd/OneDrive/Desktop/Noise Image/Noise3.PNG")
plot(Noise3) #Noise3
Noise4 <- load.image("C:/Users/syedd/OneDrive/Desktop/Noise Image/Noise4.PNG") #Load image
plot(Noise4) #Noise4
Noise5 <- load.image("C:/Users/syedd/OneDrive/Desktop/Noise Image/Noise5.PNG")
plot(Noise5) #Noise5
###Part B
#----------------------------------------------------
##i)Denoising
#Noise1
layout(t(1:2))
plot(Noise1,main="Original")
isoblur(Noise1,5) %>% plot(main="Blurred")
plot(Noise1,main="Original")
blur_anisotropic(Noise1,ampl=1e3,sharp=.2) %>% plot(main="Blurred (anisotropic)")
#Noise2
layout(t(1:2))
plot(Noise2,main="Original")
isoblur(Noise2,5) %>% plot(main="Blurred")
plot(Noise2,main="Original")
blur_anisotropic(Noise2,ampl=1e3,sharp=.3) %>% plot(main="Blurred (anisotropic)")
#Noise3
layout(t(1:2))
plot(Noise3,main="Original")
isoblur(Noise3,5) %>% plot(main="Blurred")
plot(Noise3,main="Original")
blur_anisotropic(Noise3,ampl=1e3,sharp=.2) %>% plot(main="Blurred (anisotropic)")
#Noise4
layout(t(1:2))
plot(Noise4,main="Original")
isoblur(Noise4,5) %>% plot(main="Blurred")
plot(Noise4,main="Original")
blur_anisotropic(Noise4,ampl=1e3,sharp=.3) %>% plot(main="Blurred (anisotropic)")
#Noise5
layout(t(1:2))
plot(Noise5,main="Original")
isoblur(Noise5,5) %>% plot(main="Blurred")
plot(Noise5,main="Original")
blur_anisotropic(Noise5,ampl=1e3,sharp=.3) %>% plot(main="Blurred (anisotropic)")
#-------------------------------------------------
##ii) Morphological operations
#Noise1
layout(t(1:3))
threshold(Noise1,"20%") %>% plot
threshold(Noise1,"15%") %>% plot
threshold(Noise1,"10%") %>% plot
#Noise2
plot(Noise2)
layout(t(1:3))
threshold(Noise2,"20%") %>% plot
threshold(Noise2,"15%") %>% plot
threshold(Noise2,"10%") %>% plot
#Noise3
layout(t(1:3))
threshold(Noise3,"20%") %>% plot
threshold(Noise3,"15%") %>% plot
threshold(Noise3,"10%") %>% plot
#Noise4
plot(Noise4)
layout(t(1:3))
threshold(Noise4,"20%") %>% plot
threshold(Noise4,"15%") %>% plot
threshold(Noise4,"10%") %>% plot
#Noise5
plot(Noise5)
layout(t(1:3))
threshold(Noise5,"20%") %>% plot
threshold(Noise5,"15%") %>% plot
threshold(Noise5,"10%") %>% plot
#----------------------------------------------
###Audio 
library(seewave)
library(tuneR)
library(tidyverse)
library(audio)
library(plotly) # for adding some interactivity dust
library(fftw)
library(cowplot)


# loading wav file
one = "C:/Users/syedd/OneDrive/Desktop/THESIS LETSGOOOO!/MOLLY/MOLLY1.wav"
two = "C:/Users/syedd/OneDrive/Desktop/THESIS LETSGOOOO!/SPOOKY/Spooky new.wav"
three = "C:/Users/syedd/OneDrive/Desktop/THESIS LETSGOOOO!/VINCENT/neslo.wav"

##one
# read and normalize wav file using tuneR
data <- tuneR::readWave(one) %>%
  tuneR::normalize(unit = c("1"), center = FALSE, rescale = FALSE)

summary(data)

# extracting sampled data, y
y = data@left

# Discrete Fourier transform of data (DFT)
dft_speech <- fftw::FFT(y)

# amplitude of DFT
dft_amp <- abs(dft_speech) # DFT is a sequence of complex sinusoids 

# number of samples
n <- length(y)

# frequency range
f <- (seq_len(n)-1)*Fs/n

freq_plot <- ggplot(mapping = aes(x = f[1:Fs], y = dft_amp[1:Fs]))+
  geom_line(color = 'blue')+
  labs(x = "Frequency (Hz)", y = "Magnitude",
       title = "Frequency domain for Molly snoring ")+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(freq_plot)

###two

# read and normalize wav file using tuneR
data <- tuneR::readWave(two) %>%
  tuneR::normalize(unit = c("1"), center = FALSE, rescale = FALSE)

summary(data)

# extracting sampled data, y
y = data@left

# Discrete Fourier transform of data (DFT)
dft_speech <- fftw::FFT(y)

# amplitude of DFT
dft_amp <- abs(dft_speech) # DFT is a sequence of complex sinusoids 

# number of samples
n <- length(y)

# frequency range
f <- (seq_len(n)-1)*Fs/n

freq_plot <- ggplot(mapping = aes(x = f[1:Fs], y = dft_amp[1:Fs]))+
  geom_line(color = 'blue')+
  labs(x = "Frequency (Hz)", y = "Magnitude",
       title = "Frequency domain for Spooky snoring")+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(freq_plot)

###three

# read and normalize wav file using tuneR
data <- tuneR::readWave(three) %>%
  tuneR::normalize(unit = c("1"), center = FALSE, rescale = FALSE)

summary(data)

# extracting sampled data, y
y = data@left

# Discrete Fourier transform of data (DFT)
dft_speech <- fftw::FFT(y)

# amplitude of DFT
dft_amp <- abs(dft_speech) # DFT is a sequence of complex sinusoids 

# number of samples
n <- length(y)

# frequency range
f <- (seq_len(n)-1)*Fs/n

freq_plot <- ggplot(mapping = aes(x = f[1:Fs], y = dft_amp[1:Fs]))+
  geom_line(color = 'blue')+
  labs(x = "Frequency (Hz)", y = "Magnitude",
       title = "Frequency domain for Neslo snoring")+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(freq_plot)
