---
title: The Life of Eli
author: |
 | Matthew Malishev^1^*
 | _^1^ Department of Biology, Emory University, 1510 Clifton Road NE, Atlanta, GA, USA, 30322_
#bibliography:/Users/malishev/Documents/Melbourne Uni/Thesis_2016/library.bib
fontsize: 10
geometry: margin=1in
documentclass: article
linkcolor: pink
urlcolor: blue
citecolor: red
output:
  word_document:
    highlight: tango
    keep_md: yes
    pandoc_args: --smart
    #reference: mystyles.docx
    toc: yes
  pdf_document:
    includes:
      in_header: # add .tex file with header content
    highlight: tango
    template: null
    toc: yes
    toc_depth: 4
    number_sections: false
    fig_width: 5
    fig_height: 4
    fig_caption: true
    df_print: tibble 
    citation_package: biblatex # natbib
    latex_engine: xelatex #pdflatex # lualatex
    keep_tex: true # keep .tex file in dir 
  html_document:
    highlight: tango
    code_folding: hide
    toc: yes
    toc_depth: 4
    number_sections: no
    toc_float: yes
inludes:
  before_body: 
subtitle: 
tags:
- nothing
- nothingness
params: 
  dir: "/Users/malishev/Documents/Data/Eli"
  date: !r Sys.Date()
  version: !r getRversion()
  email: "matthew.malishev@gmail.com"
  doi: https://github.com/darwinanddavis/Eli
classoption: portrait
# ^['https://github.com/darwinanddavis/Eli'] # footnote
vignette: >
  %\VignetteIndexEntry{Eli}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::rmarkdown}
---

<script type="text/x-mathjax-config">
  MathJax.Hub.Config({ TeX: { equationNumbers: {autoNumber: "all"} } });
</script>





\  

Date: 2018-08-21  
R version: 3.5.0  
*Corresponding author: matthew.malishev@gmail.com  
This document can be found at https://github.com/darwinanddavis/Eli

\newpage  

## Overview  

Activity data for Eli for his first year, including time spent feeding, sleeping, in leisure and values for growth and other behavoural traits.  

\  

**TO DO**  
* ~~separate activity states~~  
* separate hour and mins, then convert time to hours   



### Install dependencies

```r
packages <- c("stringi","tidyr","sp","RColorBrewer","ggplot2","ggthemes")   
if (require(packages)) {
    install.packages(packages,dependencies = T)
    require(packages)
}
lapply(packages,library,character.only=T)
```
### Set plotting function  

```r
# plotting function (plot for MS or not, set bg color, set color palette from RColorBrewer, set alpha value for transperancy) 
plot_it <- function(manuscript,bg,cp,alpha,family){ 
  graphics.off()
  if(manuscript==0){
    if(bg=="black"){
      colvec<-magma(200,1)
      par(bg = colvec[1],col.axis="white",col.lab="white",col.main="white",
          fg="white",bty="n",las=1,mar=c(5,6,4,2),family=family) #mono
      border=adjustcolor("purple",alpha=0.5)
    }else{
      colvec<-bpy.colors(200)
      par(bg = colvec[1],col.axis="white",col.lab="white",col.main="white",
          fg="white",bty="n",las=1,mar=c(5,6,4,2),family=family) 
      border=adjustcolor("blue",alpha=0.5)
    }
  }else{
#    graphics.off()
    par(bty="n",las=1,family=family) 
  }
  # color palettes
  # ifelse(manuscript==1,colvec<-adjustcolor(brewer.pal(9,cp)[9], alpha = alpha),colvec <- adjustcolor(brewer.pal(9,cp)[5], alpha = alpha)) # fine tune plotting colors for plotting bg
   # colfunc <<- colorRampPalette(brewer.pal(9,cp),alpha=alpha)
  colfunc <<- adjustcolor(brewer.pal(9,cp),alpha=alpha) # USES <<- OPERATOR
}
```


```r
# Setting ggplot theme graphics
plot_it_gg <- function(bg){ # bg = colour to plot bg, family = font family
  if(bg=="white"){
    bg <- "white"
    fg <- "black"
  theme_tufte(base_family = "HersheySans") +
    theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = bg,colour = bg),plot.background = element_rect(fill=bg)) +
    theme(axis.line = element_line(color = fg)) +theme(axis.ticks = element_line(color = fg)) + theme(plot.title = element_text(colour = fg)) +theme(axis.title.x = element_text(colour = fg), axis.title.y = element_text(colour = fg)) + theme(axis.text.x = element_text(color = fg),axis.text.y = element_text(color = fg)) + theme(legend.key = element_rect(fill = bg)) + theme(legend.title = element_text(colour=fg)) + theme(legend.text = element_text(colour=fg))
}
  }# end gg

# define colours  
col1 <- "light blue"
col2 <- "orange"

# Set global plotting parameters
print("1/0, set colour, set colour palette 'display.brewer.all()',set alpha for col,set font")
plot_it(0,"blue","YlOrRd",1,"HersheySans") # set col function params
plot_it_gg("white") # same as above
```

### Load and clean data  

```r
setwd(params$dir) # set wd
list.files()
```

```
 [1] "april.csv"       "babytrackerlogs" "eli_cache"       "eli_files"       "eli_weight.mp4" 
 [6] "eli.html"        "eli.pdf"         "eli.R"           "eli.Rmd"         "Eli.Rproj"      
[11] "eli.tex"         "feb.csv"         "junejulyaug.csv" "march.csv"       "may.csv"        
```

```r
d <- "may" # choose month or total period

data <- read.csv(paste0(d,".csv"),header=T,sep=",", stringsAsFactors=FALSE)
colnames(data) <- c("Activity","Trait","Start","Finish","Value")
data[c("Activity", "Trait")] <- sapply(data[c("Activity", "Trait")],as.character)
head(data) 
```

```
  Activity        Trait                 Start                Finish  Value
1   Growth         Head  16-Feb.-2018 8:06 pm  16-Feb.-2018 8:06 pm   35cm
2   Growth       Height  16-Feb.-2018 8:06 pm  16-Feb.-2018 8:06 pm   53cm
3   Growth       Weight 16-Feb.-2018 11:59 pm 16-Feb.-2018 11:59 pm 3.61kg
4  Feeding Right Breast  18-Feb.-2018 1:25 am  18-Feb.-2018 1:35 am       
5  Feeding  Left Breast  18-Feb.-2018 1:35 am  18-Feb.-2018 1:44 am       
6  Feeding Right Breast  18-Feb.-2018 3:24 am  18-Feb.-2018 3:45 am       
```

```r
# load june july aug + data
# skip first three redundant rows
dd <- "junejulyaug"
data2 <- read.csv(paste0(dd,".csv"),header=T,sep=",", stringsAsFactors=FALSE,skip=3)
colnames(data2) <- c("Activity","Trait","Start","Finish","Value")
data2[c("Activity", "Trait")] <- sapply(data2[c("Activity", "Trait")],as.character)
str(data2) 
```

```
'data.frame':	1330 obs. of  5 variables:
 $ Activity: chr  "Health" "Sleep" "Feeding" "Diapering" ...
 $ Trait   : chr  "Vaccination" "" "Right Breast" "Pee" ...
 $ Start   : chr  "19-Jun.-2018 10:10 am" "27-Jun.-2018 12:05 am" "27-Jun.-2018 9:31 am" "27-Jun.-2018 9:44 am" ...
 $ Finish  : chr  "19-Jun.-2018 10:10 am" "27-Jun.-2018 9:32 am" "27-Jun.-2018 9:40 am" "27-Jun.-2018 9:44 am" ...
 $ Value   : chr  "4mnth immunizations" "" "" "" ...
```

```r
# add june july aug + data to existing data frame
data <- rbind(data,data2)
```

### Subset activities  

```r
unique(data$Activity)
```

```
[1] "Growth"    "Feeding"   "Sleep"     "Diapering" "Health"    "Leisure"   "Pumping"  
```

```r
grow <- subset(data,subset=Activity=="Growth");head(grow)
```

```
    Activity  Trait                 Start                Finish  Value
1     Growth   Head  16-Feb.-2018 8:06 pm  16-Feb.-2018 8:06 pm   35cm
2     Growth Height  16-Feb.-2018 8:06 pm  16-Feb.-2018 8:06 pm   53cm
3     Growth Weight 16-Feb.-2018 11:59 pm 16-Feb.-2018 11:59 pm 3.61kg
292   Growth Weight 27-Feb.-2018 12:00 pm 27-Feb.-2018 12:00 pm 3.67kg
534   Growth Weight  07-Mar.-2018 1:08 pm  07-Mar.-2018 1:08 pm 4.01kg
535   Growth Height  07-Mar.-2018 1:08 pm  07-Mar.-2018 1:08 pm   55cm
```

```r
feed <- subset(data,subset=Activity=="Feeding");head(feed)
```

```
   Activity        Trait                 Start                Finish Value
4   Feeding Right Breast  18-Feb.-2018 1:25 am  18-Feb.-2018 1:35 am      
5   Feeding  Left Breast  18-Feb.-2018 1:35 am  18-Feb.-2018 1:44 am      
6   Feeding Right Breast  18-Feb.-2018 3:24 am  18-Feb.-2018 3:45 am      
7   Feeding  Left Breast  18-Feb.-2018 7:39 am  18-Feb.-2018 8:05 am      
10  Feeding Right Breast 18-Feb.-2018 10:12 am 18-Feb.-2018 10:45 am      
11  Feeding  Left Breast 18-Feb.-2018 10:48 am 18-Feb.-2018 11:35 am      
```

```r
sleep <- subset(data,subset=Activity=="Sleep");head(sleep)
```

```
   Activity Trait                 Start                Finish Value
8     Sleep        18-Feb.-2018 8:53 am  18-Feb.-2018 9:41 am      
12    Sleep       18-Feb.-2018 11:32 am  18-Feb.-2018 3:16 pm      
23    Sleep       18-Feb.-2018 10:24 pm 18-Feb.-2018 10:52 pm      
27    Sleep        19-Feb.-2018 1:40 am  19-Feb.-2018 3:00 am      
29    Sleep        19-Feb.-2018 3:36 am  19-Feb.-2018 3:38 am      
39    Sleep        19-Feb.-2018 5:15 pm  19-Feb.-2018 6:05 pm      
```

```r
diaper <- subset(data,subset=Activity=="Diapering");head(diaper)
```

```
    Activity     Trait                 Start                Finish                           Value
9  Diapering Pee & Poo 18-Feb.-2018 10:01 am 18-Feb.-2018 10:01 am                           olive
18 Diapering       Poo  18-Feb.-2018 6:42 pm  18-Feb.-2018 6:42 pm                 licorice, shiny
22 Diapering       Poo 18-Feb.-2018 10:00 pm 18-Feb.-2018 10:00 pm small like earlier, olive green
47 Diapering       Pee  20-Feb.-2018 2:46 am  20-Feb.-2018 2:46 am                                
48 Diapering       Poo  20-Feb.-2018 2:47 am  20-Feb.-2018 2:47 am              Fresh. Olive/brown
51 Diapering Pee & Poo  20-Feb.-2018 3:54 am  20-Feb.-2018 3:54 am                                
```

```r
leisure <- subset(data,subset=Activity=="Leisure");head(leisure)
```

```
    Activity      Trait                 Start                Finish Value
742  Leisure  Bath time 13-Mar.-2018 10:15 pm 13-Mar.-2018 10:30 pm      
795  Leisure  Bath time  15-Mar.-2018 9:15 pm  15-Mar.-2018 9:30 pm      
855  Leisure Tummy time  17-Mar.-2018 8:00 pm  17-Mar.-2018 8:02 pm      
858  Leisure  Bath time  17-Mar.-2018 9:10 pm  17-Mar.-2018 9:30 pm      
883  Leisure Tummy time  18-Mar.-2018 6:40 pm  18-Mar.-2018 6:45 pm      
919  Leisure Tummy time 20-Mar.-2018 12:09 am 20-Mar.-2018 12:14 am      
```

### Subset traits   

```r
# activity states with traits: grow,feed,diaper,leisure

# grow
grow <- within(grow, rm("Finish")) # only time stamp, so remove Finish time col
head <- subset(grow,subset=Trait=="Head");head
```

```
     Activity Trait                 Start  Value
1      Growth  Head  16-Feb.-2018 8:06 pm   35cm
536    Growth  Head  07-Mar.-2018 1:08 pm 37.5cm
2080   Growth  Head 24-Apr.-2018 10:16 pm   40cm
```

```r
height <- subset(grow,subset=Trait=="Height");height
```

```
     Activity  Trait                 Start  Value
2      Growth Height  16-Feb.-2018 8:06 pm   53cm
535    Growth Height  07-Mar.-2018 1:08 pm   55cm
2079   Growth Height 24-Apr.-2018 10:15 pm 61.5cm
2876   Growth Height        23/05/18 20:20   63cm
```

```r
weight <- subset(grow,subset=Trait=="Weight");weight  
```

```
     Activity  Trait                 Start                Value
3      Growth Weight 16-Feb.-2018 11:59 pm               3.61kg
292    Growth Weight 27-Feb.-2018 12:00 pm               3.67kg
534    Growth Weight  07-Mar.-2018 1:08 pm               4.01kg
965    Growth Weight 21-Mar.-2018 10:45 am              4.695kg
1197   Growth Weight  28-Mar.-2018 6:09 pm                5.1kg
1619   Growth Weight 11-Apr.-2018 11:12 am                5.5kg
1802   Growth Weight  16-Apr.-2018 2:28 pm 5.5kg, @ babybunting
2078   Growth Weight 24-Apr.-2018 10:14 pm               5.73kg
2522   Growth Weight        10/05/18 12:14                  6kg
3190   Growth Weight  02-Jul.-2018 1:10 pm                6.6kg
```

```r
# feed
feed <- within(feed,rm("Value")) # no values, so remove Values col
breast_l <- subset(feed,subset=Trait=="Left Breast");head(breast_l)
```

```
   Activity       Trait                 Start                Finish
5   Feeding Left Breast  18-Feb.-2018 1:35 am  18-Feb.-2018 1:44 am
7   Feeding Left Breast  18-Feb.-2018 7:39 am  18-Feb.-2018 8:05 am
11  Feeding Left Breast 18-Feb.-2018 10:48 am 18-Feb.-2018 11:35 am
14  Feeding Left Breast  18-Feb.-2018 4:17 pm  18-Feb.-2018 4:17 pm
15  Feeding Left Breast  18-Feb.-2018 4:20 pm  18-Feb.-2018 4:20 pm
16  Feeding Left Breast  18-Feb.-2018 5:32 pm  18-Feb.-2018 5:40 pm
```

```r
breast_r <- subset(feed,subset=Trait=="Right Breast");head(breast_r)
```

```
   Activity        Trait                 Start                Finish
4   Feeding Right Breast  18-Feb.-2018 1:25 am  18-Feb.-2018 1:35 am
6   Feeding Right Breast  18-Feb.-2018 3:24 am  18-Feb.-2018 3:45 am
10  Feeding Right Breast 18-Feb.-2018 10:12 am 18-Feb.-2018 10:45 am
13  Feeding Right Breast  18-Feb.-2018 3:23 pm  18-Feb.-2018 3:56 pm
17  Feeding Right Breast  18-Feb.-2018 6:40 pm  18-Feb.-2018 6:52 pm
19  Feeding Right Breast  18-Feb.-2018 7:02 pm  18-Feb.-2018 7:30 pm
```

```r
bottle <- subset(feed,subset=Trait=="Bottle");head(bottle)
```

```
     Activity  Trait                 Start                Finish
696   Feeding Bottle 12-Mar.-2018 11:12 am 12-Mar.-2018 11:22 am
1890  Feeding Bottle  19-Apr.-2018 1:10 pm  19-Apr.-2018 1:12 pm
1906  Feeding Bottle  19-Apr.-2018 9:35 pm  19-Apr.-2018 9:42 pm
2816  Feeding Bottle        21/05/18 11:47        21/05/18 11:55
2926  Feeding Bottle        25/05/18 19:12        25/05/18 19:21
4064  Feeding Bottle 08-Aug.-2018 12:09 pm 08-Aug.-2018 12:09 pm
```

```r
# sleep
# no traits

# diaper
diaper <- within(diaper, rm("Finish")) # only time stamp, so remove Finish time col
pee <- subset(diaper,subset=Trait=="Pee");head(pee)
```

```
    Activity Trait                 Start Value
47 Diapering   Pee  20-Feb.-2018 2:46 am      
59 Diapering   Pee 20-Feb.-2018 11:20 am      
63 Diapering   Pee  20-Feb.-2018 4:29 pm      
67 Diapering   Pee  20-Feb.-2018 7:09 pm      
70 Diapering   Pee  20-Feb.-2018 8:30 pm      
77 Diapering   Pee  21-Feb.-2018 2:29 am      
```

```r
poo <- subset(diaper,subset=Trait=="Poo");head(poo) 
```

```
    Activity Trait                 Start                           Value
18 Diapering   Poo  18-Feb.-2018 6:42 pm                 licorice, shiny
22 Diapering   Poo 18-Feb.-2018 10:00 pm small like earlier, olive green
48 Diapering   Poo  20-Feb.-2018 2:47 am              Fresh. Olive/brown
64 Diapering   Poo  20-Feb.-2018 4:31 pm                                
75 Diapering   Poo 21-Feb.-2018 12:45 am                                
92 Diapering   Poo  21-Feb.-2018 1:51 pm                                
```

```r
both <- subset(diaper,subset=Trait==unique(diaper$Trait)[1]);head(both) 
```

```
     Activity     Trait                 Start Value
9   Diapering Pee & Poo 18-Feb.-2018 10:01 am olive
51  Diapering Pee & Poo  20-Feb.-2018 3:54 am      
72  Diapering Pee & Poo 20-Feb.-2018 11:42 pm      
81  Diapering Pee & Poo  21-Feb.-2018 4:53 am      
137 Diapering Pee & Poo 22-Feb.-2018 10:20 pm      
143 Diapering Pee & Poo  23-Feb.-2018 4:55 am      
```

```r
# leisure  
leisure <- within(leisure, rm("Value")) # no values, so remove Values col
bath <- subset(leisure,subset=Trait=="Bath time");head(bath)
```

```
     Activity     Trait                 Start                Finish
742   Leisure Bath time 13-Mar.-2018 10:15 pm 13-Mar.-2018 10:30 pm
795   Leisure Bath time  15-Mar.-2018 9:15 pm  15-Mar.-2018 9:30 pm
858   Leisure Bath time  17-Mar.-2018 9:10 pm  17-Mar.-2018 9:30 pm
1017  Leisure Bath time 22-Mar.-2018 11:05 pm 22-Mar.-2018 11:29 pm
1082  Leisure Bath time  24-Mar.-2018 9:10 pm  24-Mar.-2018 9:34 pm
1177  Leisure Bath time  27-Mar.-2018 9:10 pm  27-Mar.-2018 9:30 pm
```

```r
tummy <- subset(leisure,subset=Trait=="Tummy time");head(tummy)
```

```
     Activity      Trait                 Start                Finish
855   Leisure Tummy time  17-Mar.-2018 8:00 pm  17-Mar.-2018 8:02 pm
883   Leisure Tummy time  18-Mar.-2018 6:40 pm  18-Mar.-2018 6:45 pm
919   Leisure Tummy time 20-Mar.-2018 12:09 am 20-Mar.-2018 12:14 am
989   Leisure Tummy time 21-Mar.-2018 10:52 pm 21-Mar.-2018 10:54 pm
1083  Leisure Tummy time  24-Mar.-2018 9:37 pm  24-Mar.-2018 9:40 pm
1164  Leisure Tummy time  27-Mar.-2018 1:53 pm  27-Mar.-2018 2:00 pm
```

```r
outdoors <- subset(leisure,subset=Trait=="Outdoors");outdoors
```

```
     Activity    Trait                Start               Finish
2370  Leisure Outdoors        5/05/18 15:37        5/05/18 16:11
2397  Leisure Outdoors        6/05/18 13:46        6/05/18 14:46
2468  Leisure Outdoors        8/05/18 15:10        8/05/18 16:16
2918  Leisure Outdoors       25/05/18 14:03       25/05/18 16:03
3288  Leisure Outdoors 06-Jul.-2018 2:00 pm 06-Jul.-2018 3:45 pm
3451  Leisure Outdoors 13-Jul.-2018 1:45 pm 13-Jul.-2018 3:33 pm
```

```r
play <- subset(leisure,subset=Trait=="Play time");head(play)
```

```
     Activity     Trait          Start         Finish
2423  Leisure Play time  7/05/18 10:35  7/05/18 11:16
2458  Leisure Play time  8/05/18 10:03  8/05/18 10:13
2507  Leisure Play time  9/05/18 22:25  9/05/18 22:35
2533  Leisure Play time 10/05/18 20:57 10/05/18 21:03
2653  Leisure Play time 15/05/18 12:41 15/05/18 13:11
2675  Leisure Play time  16/05/18 6:58  16/05/18 7:10
```
unique(data$Activity)





### Growth  

#### Head  

```r
require("stringi")
require("tidyr")
require("sp")
require("RColorBrewer")

plot_it(0,"blue","Blues",0.5,"HersheySans")
hv <- gsub("[^[:digit:]]", "", head$Value) # get just integers
stri_sub(hv,3,1) <- ".";hv # insert the decimal point in the correct place
```

```
[1] "35."  "37.5" "40." 
```

```r
head$Value <- hv %>% as.numeric() # make numeric

d <- head
par(las=1,bty="n")
ylim <- round(max(d$Value,10))
with(d,plot(Value,
            col=col1,
            type="b",lwd=3,
            pch=20,
            ylim=c(0,ylim),
            ylab="Head circumference (cm)",
            xlab="Time",
            xaxt="n"
))
axis(1,at=1:4,labels=month.abb[2:5])
title("Head circumference (cm)")
```

#### Weight   

```r
require(RColorBrewer)
require(sp)

plot_it(0,"blue","Blues",0.5,"HersheySans")
wv <- gsub("[^[:digit:]]", "", weight$Value) # get just integers
stri_sub(wv,2,1) <- ".";wv # insert the decimal point in the correct place
```

```
 [1] "3.61"  "3.67"  "4.01"  "4.695" "5.1"   "5.5"   "5.5"   "5.73"  "6."    "6.6"  
```

```r
weight$Value <- wv %>% as.numeric() # make numeric

d <- weight
par(las=1,bty="n")
xlim <- length(weight$Value)
ylim <- round(max(weight$Value,10))
with(d,plot(Value,
            col=col1,
            type="b",lwd=3,
            pch=20,
            ylim=c(0,ylim),
            ylab="Weight (kg)",
            xlab="Time",
            xaxt="n"
))
# axis(1,at=1:xlim,labels=rep(month.abb[2:(xlim/2)],each=2))
title("Weight (kg) over time")
weight
```

```
     Activity  Trait                 Start Value
3      Growth Weight 16-Feb.-2018 11:59 pm 3.610
292    Growth Weight 27-Feb.-2018 12:00 pm 3.670
534    Growth Weight  07-Mar.-2018 1:08 pm 4.010
965    Growth Weight 21-Mar.-2018 10:45 am 4.695
1197   Growth Weight  28-Mar.-2018 6:09 pm 5.100
1619   Growth Weight 11-Apr.-2018 11:12 am 5.500
1802   Growth Weight  16-Apr.-2018 2:28 pm 5.500
2078   Growth Weight 24-Apr.-2018 10:14 pm 5.730
2522   Growth Weight        10/05/18 12:14 6.000
3190   Growth Weight  02-Jul.-2018 1:10 pm 6.600
```

#### Height  

```r
require(RColorBrewer)
require(sp)

plot_it(0,"blue","Blues",0.5,"HersheySans")
hhv <- gsub("[^[:digit:]]", "", height$Value) # get just integers
stri_sub(hhv,3,1) <- "." ;hhv # insert the decimal point in the correct place
```

```
[1] "53."  "55."  "61.5" "63." 
```

```r
height$Value <- hhv %>% as.numeric() # make numeric

d <- height
par(las=1,bty="n")
with(d,plot(Value,
            col=col1,
            pch=20,
            type="b",lwd=3,
            ylim=c(0,70),
            ylab="Height (cm)",
            xlab="Time",
            xaxt="n"
))
axis(1,at=1:4,labels=month.abb[2:5])
# axis(1,at=c(0,length(d$Value)),labels=c("",""))# bookending axis tick marks
title("Height (cm) over time")
```

### Feeding  
Only time period, no values    

#### Left breast  
Only time period, no values    

```r
breast_l
```

```
     Activity       Trait                 Start                Finish
5     Feeding Left Breast  18-Feb.-2018 1:35 am  18-Feb.-2018 1:44 am
7     Feeding Left Breast  18-Feb.-2018 7:39 am  18-Feb.-2018 8:05 am
11    Feeding Left Breast 18-Feb.-2018 10:48 am 18-Feb.-2018 11:35 am
14    Feeding Left Breast  18-Feb.-2018 4:17 pm  18-Feb.-2018 4:17 pm
15    Feeding Left Breast  18-Feb.-2018 4:20 pm  18-Feb.-2018 4:20 pm
16    Feeding Left Breast  18-Feb.-2018 5:32 pm  18-Feb.-2018 5:40 pm
20    Feeding Left Breast  18-Feb.-2018 9:04 pm  18-Feb.-2018 9:23 pm
24    Feeding Left Breast 18-Feb.-2018 10:56 pm 18-Feb.-2018 11:23 pm
28    Feeding Left Breast  19-Feb.-2018 3:00 am  19-Feb.-2018 3:31 am
32    Feeding Left Breast  19-Feb.-2018 6:02 am  19-Feb.-2018 6:09 am
34    Feeding Left Breast  19-Feb.-2018 6:50 am  19-Feb.-2018 7:00 am
36    Feeding Left Breast 19-Feb.-2018 10:25 am 19-Feb.-2018 10:27 am
37    Feeding Left Breast 19-Feb.-2018 10:27 am 19-Feb.-2018 10:32 am
40    Feeding Left Breast  19-Feb.-2018 6:08 pm  19-Feb.-2018 6:28 pm
44    Feeding Left Breast 19-Feb.-2018 11:55 pm 20-Feb.-2018 12:01 am
46    Feeding Left Breast  20-Feb.-2018 2:09 am  20-Feb.-2018 2:17 am
55    Feeding Left Breast  20-Feb.-2018 6:18 am  20-Feb.-2018 6:32 am
60    Feeding Left Breast 20-Feb.-2018 12:15 pm 20-Feb.-2018 12:35 pm
66    Feeding Left Breast  20-Feb.-2018 5:57 pm  20-Feb.-2018 6:03 pm
68    Feeding Left Breast  20-Feb.-2018 7:10 pm  20-Feb.-2018 7:20 pm
78    Feeding Left Breast  21-Feb.-2018 2:30 am  21-Feb.-2018 2:37 am
82    Feeding Left Breast  21-Feb.-2018 4:56 am  21-Feb.-2018 5:04 am
84    Feeding Left Breast  21-Feb.-2018 8:52 am  21-Feb.-2018 9:07 am
91    Feeding Left Breast  21-Feb.-2018 1:29 pm  21-Feb.-2018 1:38 pm
93    Feeding Left Breast  21-Feb.-2018 2:13 pm  21-Feb.-2018 2:36 pm
99    Feeding Left Breast  21-Feb.-2018 7:21 pm  21-Feb.-2018 7:31 pm
106   Feeding Left Breast 21-Feb.-2018 10:35 pm 21-Feb.-2018 10:40 pm
107   Feeding Left Breast 21-Feb.-2018 10:42 pm 21-Feb.-2018 10:44 pm
110   Feeding Left Breast  22-Feb.-2018 1:00 am  22-Feb.-2018 1:11 am
113   Feeding Left Breast  22-Feb.-2018 2:06 am  22-Feb.-2018 2:12 am
119   Feeding Left Breast  22-Feb.-2018 6:36 am  22-Feb.-2018 6:52 am
121   Feeding Left Breast  22-Feb.-2018 7:21 am  22-Feb.-2018 7:34 am
128   Feeding Left Breast  22-Feb.-2018 2:33 pm  22-Feb.-2018 2:40 pm
133   Feeding Left Breast  22-Feb.-2018 6:29 pm  22-Feb.-2018 6:37 pm
138   Feeding Left Breast 22-Feb.-2018 10:37 pm 22-Feb.-2018 10:45 pm
140   Feeding Left Breast 23-Feb.-2018 12:03 am 23-Feb.-2018 12:13 am
144   Feeding Left Breast  23-Feb.-2018 5:16 am  23-Feb.-2018 5:34 am
153   Feeding Left Breast 23-Feb.-2018 11:27 am 23-Feb.-2018 11:32 am
155   Feeding Left Breast 23-Feb.-2018 12:12 pm 23-Feb.-2018 12:22 pm
161   Feeding Left Breast  23-Feb.-2018 5:02 pm  23-Feb.-2018 5:16 pm
166   Feeding Left Breast  23-Feb.-2018 7:38 pm  23-Feb.-2018 7:48 pm
171   Feeding Left Breast 23-Feb.-2018 11:43 pm 23-Feb.-2018 11:54 pm
175   Feeding Left Breast  24-Feb.-2018 1:24 am  24-Feb.-2018 1:33 am
179   Feeding Left Breast  24-Feb.-2018 8:47 am  24-Feb.-2018 8:55 am
184   Feeding Left Breast 24-Feb.-2018 11:33 am 24-Feb.-2018 11:51 am
189   Feeding Left Breast  24-Feb.-2018 3:20 pm  24-Feb.-2018 3:32 pm
195   Feeding Left Breast  24-Feb.-2018 9:02 pm  24-Feb.-2018 9:15 pm
197   Feeding Left Breast  24-Feb.-2018 9:47 pm  24-Feb.-2018 9:52 pm
202   Feeding Left Breast 25-Feb.-2018 12:39 am 25-Feb.-2018 12:52 am
204   Feeding Left Breast  25-Feb.-2018 1:40 am  25-Feb.-2018 1:54 am
209   Feeding Left Breast  25-Feb.-2018 4:27 am  25-Feb.-2018 4:37 am
214   Feeding Left Breast  25-Feb.-2018 8:46 am  25-Feb.-2018 8:58 am
218   Feeding Left Breast 25-Feb.-2018 11:40 am 25-Feb.-2018 11:51 am
223   Feeding Left Breast  25-Feb.-2018 3:16 pm  25-Feb.-2018 3:40 pm
228   Feeding Left Breast  25-Feb.-2018 6:06 pm  25-Feb.-2018 6:13 pm
236   Feeding Left Breast  25-Feb.-2018 9:42 pm  25-Feb.-2018 9:53 pm
238   Feeding Left Breast  25-Feb.-2018 9:55 pm 25-Feb.-2018 10:06 pm
241   Feeding Left Breast 26-Feb.-2018 12:27 am 26-Feb.-2018 12:43 am
245   Feeding Left Breast  26-Feb.-2018 3:11 am  26-Feb.-2018 3:25 am
246   Feeding Left Breast  26-Feb.-2018 3:58 am  26-Feb.-2018 4:08 am
257   Feeding Left Breast  26-Feb.-2018 1:35 pm  26-Feb.-2018 1:44 pm
261   Feeding Left Breast  26-Feb.-2018 5:20 pm  26-Feb.-2018 5:30 pm
268   Feeding Left Breast  26-Feb.-2018 9:41 pm  26-Feb.-2018 9:50 pm
272   Feeding Left Breast 26-Feb.-2018 11:35 pm 26-Feb.-2018 11:45 pm
279   Feeding Left Breast  27-Feb.-2018 3:37 am  27-Feb.-2018 3:50 am
285   Feeding Left Breast  27-Feb.-2018 7:55 am  27-Feb.-2018 8:06 am
293   Feeding Left Breast  27-Feb.-2018 3:07 pm  27-Feb.-2018 3:18 pm
297   Feeding Left Breast  27-Feb.-2018 5:49 pm  27-Feb.-2018 6:04 pm
301   Feeding Left Breast 27-Feb.-2018 10:03 pm 27-Feb.-2018 10:10 pm
302   Feeding Left Breast 27-Feb.-2018 11:02 pm 27-Feb.-2018 11:07 pm
308   Feeding Left Breast  28-Feb.-2018 2:46 am  28-Feb.-2018 3:00 am
311   Feeding Left Breast  28-Feb.-2018 5:15 am  28-Feb.-2018 5:19 am
314   Feeding Left Breast  28-Feb.-2018 7:48 am  28-Feb.-2018 8:00 am
319   Feeding Left Breast 28-Feb.-2018 10:02 am 28-Feb.-2018 10:12 am
323   Feeding Left Breast  28-Feb.-2018 2:26 pm  28-Feb.-2018 2:36 pm
328   Feeding Left Breast  28-Feb.-2018 6:37 pm  28-Feb.-2018 6:54 pm
333   Feeding Left Breast 28-Feb.-2018 10:14 pm 28-Feb.-2018 10:22 pm
336   Feeding Left Breast 01-Mar.-2018 12:45 am  01-Mar.-2018 1:05 am
340   Feeding Left Breast  01-Mar.-2018 6:58 am  01-Mar.-2018 7:08 am
346   Feeding Left Breast 01-Mar.-2018 10:34 am 01-Mar.-2018 10:49 am
350   Feeding Left Breast  01-Mar.-2018 1:39 pm  01-Mar.-2018 1:52 pm
354   Feeding Left Breast  01-Mar.-2018 4:30 pm  01-Mar.-2018 4:45 pm
359   Feeding Left Breast  01-Mar.-2018 7:08 pm  01-Mar.-2018 7:18 pm
365   Feeding Left Breast 01-Mar.-2018 11:10 pm 01-Mar.-2018 11:31 pm
370   Feeding Left Breast  02-Mar.-2018 1:42 am  02-Mar.-2018 1:54 am
374   Feeding Left Breast  02-Mar.-2018 7:23 am  02-Mar.-2018 7:35 am
381   Feeding Left Breast 02-Mar.-2018 11:44 am 02-Mar.-2018 12:00 pm
385   Feeding Left Breast  02-Mar.-2018 3:51 pm  02-Mar.-2018 4:07 pm
389   Feeding Left Breast  02-Mar.-2018 6:19 pm  02-Mar.-2018 6:33 pm
394   Feeding Left Breast 02-Mar.-2018 10:21 pm 02-Mar.-2018 10:35 pm
397   Feeding Left Breast 03-Mar.-2018 12:05 am 03-Mar.-2018 12:12 am
399   Feeding Left Breast 03-Mar.-2018 12:58 am  03-Mar.-2018 1:09 am
403   Feeding Left Breast  03-Mar.-2018 2:53 am  03-Mar.-2018 3:03 am
404   Feeding Left Breast  03-Mar.-2018 3:14 am  03-Mar.-2018 3:23 am
407   Feeding Left Breast  03-Mar.-2018 8:24 am  03-Mar.-2018 8:40 am
412   Feeding Left Breast 03-Mar.-2018 11:58 am 03-Mar.-2018 12:12 pm
416   Feeding Left Breast  03-Mar.-2018 2:02 pm  03-Mar.-2018 2:15 pm
421   Feeding Left Breast  03-Mar.-2018 4:35 pm  03-Mar.-2018 4:45 pm
422   Feeding Left Breast  03-Mar.-2018 5:15 pm  03-Mar.-2018 5:28 pm
425   Feeding Left Breast  03-Mar.-2018 7:49 pm  03-Mar.-2018 8:06 pm
429   Feeding Left Breast 03-Mar.-2018 11:16 pm 03-Mar.-2018 11:27 pm
433   Feeding Left Breast  04-Mar.-2018 5:23 am  04-Mar.-2018 5:46 am
437   Feeding Left Breast  04-Mar.-2018 9:15 am  04-Mar.-2018 9:23 am
441   Feeding Left Breast 04-Mar.-2018 12:44 pm 04-Mar.-2018 12:57 pm
444   Feeding Left Breast  04-Mar.-2018 3:19 pm  04-Mar.-2018 3:25 pm
448   Feeding Left Breast  04-Mar.-2018 6:13 pm  04-Mar.-2018 6:29 pm
449   Feeding Left Breast  04-Mar.-2018 7:10 pm  04-Mar.-2018 7:18 pm
454   Feeding Left Breast 04-Mar.-2018 10:35 pm 04-Mar.-2018 10:43 pm
460   Feeding Left Breast  05-Mar.-2018 1:26 am  05-Mar.-2018 1:42 am
463   Feeding Left Breast  05-Mar.-2018 2:47 am  05-Mar.-2018 2:52 am
466   Feeding Left Breast  05-Mar.-2018 6:24 am  05-Mar.-2018 6:37 am
471   Feeding Left Breast 05-Mar.-2018 11:44 am 05-Mar.-2018 11:58 am
477   Feeding Left Breast  05-Mar.-2018 4:02 pm  05-Mar.-2018 4:16 pm
480   Feeding Left Breast  05-Mar.-2018 7:21 pm  05-Mar.-2018 7:36 pm
486   Feeding Left Breast 05-Mar.-2018 10:52 pm 05-Mar.-2018 11:03 pm
488   Feeding Left Breast 05-Mar.-2018 11:24 pm 05-Mar.-2018 11:36 pm
493   Feeding Left Breast  06-Mar.-2018 4:46 am  06-Mar.-2018 4:51 am
494   Feeding Left Breast  06-Mar.-2018 5:02 am  06-Mar.-2018 5:17 am
497   Feeding Left Breast  06-Mar.-2018 6:55 am  06-Mar.-2018 7:04 am
501   Feeding Left Breast 06-Mar.-2018 10:20 am 06-Mar.-2018 10:28 am
506   Feeding Left Breast  06-Mar.-2018 1:59 pm  06-Mar.-2018 2:09 pm
511   Feeding Left Breast  06-Mar.-2018 6:42 pm  06-Mar.-2018 6:52 pm
513   Feeding Left Breast  06-Mar.-2018 7:27 pm  06-Mar.-2018 7:43 pm
518   Feeding Left Breast 06-Mar.-2018 10:55 pm 06-Mar.-2018 11:09 pm
521   Feeding Left Breast 07-Mar.-2018 12:55 am  07-Mar.-2018 1:08 am
527   Feeding Left Breast  07-Mar.-2018 6:41 am  07-Mar.-2018 6:51 am
532   Feeding Left Breast 07-Mar.-2018 12:15 pm 07-Mar.-2018 12:28 pm
541   Feeding Left Breast  07-Mar.-2018 4:55 pm  07-Mar.-2018 5:06 pm
545   Feeding Left Breast  07-Mar.-2018 7:40 pm  07-Mar.-2018 8:01 pm
550   Feeding Left Breast  07-Mar.-2018 9:43 pm 07-Mar.-2018 10:00 pm
556   Feeding Left Breast  08-Mar.-2018 2:27 am  08-Mar.-2018 2:42 am
562   Feeding Left Breast  08-Mar.-2018 7:27 am  08-Mar.-2018 7:38 am
567   Feeding Left Breast 08-Mar.-2018 11:11 am 08-Mar.-2018 11:18 am
569   Feeding Left Breast 08-Mar.-2018 12:45 pm  08-Mar.-2018 1:04 pm
573   Feeding Left Breast  08-Mar.-2018 3:41 pm  08-Mar.-2018 3:51 pm
575   Feeding Left Breast  08-Mar.-2018 3:53 pm  08-Mar.-2018 4:10 pm
579   Feeding Left Breast  08-Mar.-2018 5:38 pm  08-Mar.-2018 5:56 pm
583   Feeding Left Breast  08-Mar.-2018 8:09 pm  08-Mar.-2018 8:20 pm
587   Feeding Left Breast 08-Mar.-2018 11:18 pm 08-Mar.-2018 11:45 pm
591   Feeding Left Breast  09-Mar.-2018 1:17 am  09-Mar.-2018 1:27 am
595   Feeding Left Breast  09-Mar.-2018 5:16 am  09-Mar.-2018 5:22 am
598   Feeding Left Breast  09-Mar.-2018 7:40 am  09-Mar.-2018 7:55 am
605   Feeding Left Breast  09-Mar.-2018 1:19 pm  09-Mar.-2018 1:42 pm
610   Feeding Left Breast  09-Mar.-2018 5:00 pm  09-Mar.-2018 5:16 pm
614   Feeding Left Breast  09-Mar.-2018 7:40 pm  09-Mar.-2018 8:00 pm
618   Feeding Left Breast 09-Mar.-2018 11:40 pm 09-Mar.-2018 11:52 pm
621   Feeding Left Breast  10-Mar.-2018 1:57 am  10-Mar.-2018 2:05 am
627   Feeding Left Breast  10-Mar.-2018 8:25 am  10-Mar.-2018 8:35 am
630   Feeding Left Breast 10-Mar.-2018 10:44 am 10-Mar.-2018 10:59 am
636   Feeding Left Breast  10-Mar.-2018 2:55 pm  10-Mar.-2018 3:20 pm
641   Feeding Left Breast  10-Mar.-2018 6:11 pm  10-Mar.-2018 6:26 pm
645   Feeding Left Breast  10-Mar.-2018 9:12 pm  10-Mar.-2018 9:35 pm
649   Feeding Left Breast 11-Mar.-2018 12:25 am 11-Mar.-2018 12:37 am
653   Feeding Left Breast  11-Mar.-2018 2:12 am  11-Mar.-2018 2:22 am
658   Feeding Left Breast  11-Mar.-2018 6:13 am  11-Mar.-2018 6:23 am
661   Feeding Left Breast  11-Mar.-2018 7:57 am  11-Mar.-2018 8:08 am
667   Feeding Left Breast 11-Mar.-2018 12:16 pm 11-Mar.-2018 12:28 pm
673   Feeding Left Breast  11-Mar.-2018 4:43 pm  11-Mar.-2018 4:50 pm
675   Feeding Left Breast  11-Mar.-2018 6:02 pm  11-Mar.-2018 6:14 pm
680   Feeding Left Breast  11-Mar.-2018 9:24 pm  11-Mar.-2018 9:33 pm
686   Feeding Left Breast 12-Mar.-2018 12:52 am  12-Mar.-2018 1:10 am
691   Feeding Left Breast  12-Mar.-2018 3:42 am  12-Mar.-2018 3:58 am
697   Feeding Left Breast 12-Mar.-2018 11:23 am 12-Mar.-2018 11:28 am
703   Feeding Left Breast  12-Mar.-2018 4:03 pm  12-Mar.-2018 4:19 pm
708   Feeding Left Breast  12-Mar.-2018 7:25 pm  12-Mar.-2018 7:43 pm
714   Feeding Left Breast 12-Mar.-2018 11:45 pm 12-Mar.-2018 11:56 pm
715   Feeding Left Breast 13-Mar.-2018 12:05 am 13-Mar.-2018 12:11 am
719   Feeding Left Breast  13-Mar.-2018 5:31 am  13-Mar.-2018 5:53 am
726   Feeding Left Breast 13-Mar.-2018 11:46 am 13-Mar.-2018 11:55 am
730   Feeding Left Breast  13-Mar.-2018 2:50 pm  13-Mar.-2018 2:55 pm
733   Feeding Left Breast  13-Mar.-2018 4:32 pm  13-Mar.-2018 4:47 pm
740   Feeding Left Breast  13-Mar.-2018 8:32 pm  13-Mar.-2018 8:45 pm
745   Feeding Left Breast 14-Mar.-2018 12:25 am 14-Mar.-2018 12:32 am
750   Feeding Left Breast  14-Mar.-2018 9:02 am  14-Mar.-2018 9:10 am
754   Feeding Left Breast  14-Mar.-2018 9:47 am  14-Mar.-2018 9:57 am
758   Feeding Left Breast  14-Mar.-2018 2:48 pm  14-Mar.-2018 3:04 pm
764   Feeding Left Breast  14-Mar.-2018 6:57 pm  14-Mar.-2018 7:12 pm
769   Feeding Left Breast 14-Mar.-2018 11:55 pm 15-Mar.-2018 12:10 am
773   Feeding Left Breast  15-Mar.-2018 3:39 am  15-Mar.-2018 3:45 am
774   Feeding Left Breast  15-Mar.-2018 5:25 am  15-Mar.-2018 5:32 am
779   Feeding Left Breast 15-Mar.-2018 10:27 am 15-Mar.-2018 10:38 am
783   Feeding Left Breast 15-Mar.-2018 12:55 pm  15-Mar.-2018 1:01 pm
785   Feeding Left Breast  15-Mar.-2018 2:04 pm  15-Mar.-2018 2:16 pm
791   Feeding Left Breast  15-Mar.-2018 6:41 pm  15-Mar.-2018 6:51 pm
794   Feeding Left Breast  15-Mar.-2018 8:58 pm  15-Mar.-2018 9:15 pm
801   Feeding Left Breast  16-Mar.-2018 3:58 am  16-Mar.-2018 4:10 am
803   Feeding Left Breast  16-Mar.-2018 6:50 am  16-Mar.-2018 7:00 am
808   Feeding Left Breast 16-Mar.-2018 10:48 am 16-Mar.-2018 10:53 am
810   Feeding Left Breast  16-Mar.-2018 1:22 pm  16-Mar.-2018 1:35 pm
817   Feeding Left Breast  16-Mar.-2018 5:37 pm  16-Mar.-2018 5:49 pm
824   Feeding Left Breast  16-Mar.-2018 9:30 pm  16-Mar.-2018 9:42 pm
825   Feeding Left Breast 16-Mar.-2018 10:54 pm 16-Mar.-2018 11:06 pm
831   Feeding Left Breast  17-Mar.-2018 1:08 am  17-Mar.-2018 1:30 am
835   Feeding Left Breast  17-Mar.-2018 7:54 am  17-Mar.-2018 8:02 am
837   Feeding Left Breast  17-Mar.-2018 8:23 am  17-Mar.-2018 8:35 am
842   Feeding Left Breast  17-Mar.-2018 1:21 pm  17-Mar.-2018 1:27 pm
845   Feeding Left Breast  17-Mar.-2018 3:01 pm  17-Mar.-2018 3:19 pm
847   Feeding Left Breast  17-Mar.-2018 3:24 pm  17-Mar.-2018 3:26 pm
853   Feeding Left Breast  17-Mar.-2018 6:39 pm  17-Mar.-2018 7:01 pm
860   Feeding Left Breast 17-Mar.-2018 10:09 pm 17-Mar.-2018 10:26 pm
863   Feeding Left Breast 18-Mar.-2018 12:04 am 18-Mar.-2018 12:13 am
867   Feeding Left Breast  18-Mar.-2018 2:43 am  18-Mar.-2018 2:56 am
869   Feeding Left Breast  18-Mar.-2018 7:15 am  18-Mar.-2018 7:37 am
874   Feeding Left Breast 18-Mar.-2018 11:51 am 18-Mar.-2018 12:09 pm
879   Feeding Left Breast  18-Mar.-2018 3:28 pm  18-Mar.-2018 3:40 pm
885   Feeding Left Breast  18-Mar.-2018 7:45 pm  18-Mar.-2018 7:57 pm
890   Feeding Left Breast 19-Mar.-2018 12:15 am 19-Mar.-2018 12:35 am
897   Feeding Left Breast  19-Mar.-2018 8:23 am  19-Mar.-2018 8:32 am
899   Feeding Left Breast  19-Mar.-2018 9:11 am  19-Mar.-2018 9:16 am
905   Feeding Left Breast  19-Mar.-2018 2:33 pm  19-Mar.-2018 2:43 pm
910   Feeding Left Breast  19-Mar.-2018 7:06 pm  19-Mar.-2018 7:22 pm
917   Feeding Left Breast 19-Mar.-2018 11:24 pm 19-Mar.-2018 11:38 pm
921   Feeding Left Breast  20-Mar.-2018 1:05 am  20-Mar.-2018 1:09 am
923   Feeding Left Breast  20-Mar.-2018 1:43 am  20-Mar.-2018 1:53 am
928   Feeding Left Breast  20-Mar.-2018 7:36 am  20-Mar.-2018 7:40 am
931   Feeding Left Breast 20-Mar.-2018 10:52 am 20-Mar.-2018 11:02 am
937   Feeding Left Breast  20-Mar.-2018 2:54 pm  20-Mar.-2018 3:11 pm
942   Feeding Left Breast  20-Mar.-2018 5:10 pm  20-Mar.-2018 5:28 pm
946   Feeding Left Breast  20-Mar.-2018 6:39 pm  20-Mar.-2018 6:45 pm
951   Feeding Left Breast 20-Mar.-2018 11:00 pm 20-Mar.-2018 11:25 pm
957   Feeding Left Breast  21-Mar.-2018 4:06 am  21-Mar.-2018 4:14 am
961   Feeding Left Breast  21-Mar.-2018 6:57 am  21-Mar.-2018 7:09 am
968   Feeding Left Breast 21-Mar.-2018 12:09 pm 21-Mar.-2018 12:20 pm
973   Feeding Left Breast  21-Mar.-2018 3:41 pm  21-Mar.-2018 4:04 pm
978   Feeding Left Breast  21-Mar.-2018 6:12 pm  21-Mar.-2018 6:21 pm
984   Feeding Left Breast  21-Mar.-2018 8:53 pm  21-Mar.-2018 9:15 pm
991   Feeding Left Breast 22-Mar.-2018 12:09 am 22-Mar.-2018 12:27 am
993   Feeding Left Breast  22-Mar.-2018 1:16 am  22-Mar.-2018 1:32 am
997   Feeding Left Breast  22-Mar.-2018 8:19 am  22-Mar.-2018 8:24 am
1000  Feeding Left Breast 22-Mar.-2018 12:17 pm 22-Mar.-2018 12:35 pm
1005  Feeding Left Breast  22-Mar.-2018 3:14 pm  22-Mar.-2018 3:19 pm
1009  Feeding Left Breast  22-Mar.-2018 5:18 pm  22-Mar.-2018 5:33 pm
1015  Feeding Left Breast 22-Mar.-2018 10:25 pm 22-Mar.-2018 10:43 pm
1022  Feeding Left Breast  23-Mar.-2018 2:37 am  23-Mar.-2018 2:45 am
1029  Feeding Left Breast  23-Mar.-2018 9:16 am  23-Mar.-2018 9:27 am
1034  Feeding Left Breast  23-Mar.-2018 1:23 pm  23-Mar.-2018 1:31 pm
1038  Feeding Left Breast  23-Mar.-2018 3:58 pm  23-Mar.-2018 4:10 pm
1046  Feeding Left Breast  23-Mar.-2018 9:00 pm  23-Mar.-2018 9:07 pm
1048  Feeding Left Breast  23-Mar.-2018 9:37 pm  23-Mar.-2018 9:49 pm
1054  Feeding Left Breast 24-Mar.-2018 12:37 am 24-Mar.-2018 12:49 am
1058  Feeding Left Breast  24-Mar.-2018 5:46 am  24-Mar.-2018 5:51 am
1061  Feeding Left Breast  24-Mar.-2018 9:39 am  24-Mar.-2018 9:49 am
1065  Feeding Left Breast 24-Mar.-2018 12:01 pm 24-Mar.-2018 12:13 pm
1072  Feeding Left Breast  24-Mar.-2018 3:28 pm  24-Mar.-2018 3:39 pm
1077  Feeding Left Breast  24-Mar.-2018 6:58 pm  24-Mar.-2018 7:05 pm
1079  Feeding Left Breast  24-Mar.-2018 7:10 pm  24-Mar.-2018 7:17 pm
1084  Feeding Left Breast  24-Mar.-2018 9:53 pm 24-Mar.-2018 10:05 pm
1088  Feeding Left Breast 24-Mar.-2018 11:07 pm 24-Mar.-2018 11:18 pm
1094  Feeding Left Breast  25-Mar.-2018 6:34 am  25-Mar.-2018 6:44 am
1099  Feeding Left Breast 25-Mar.-2018 11:23 am 25-Mar.-2018 11:35 am
1104  Feeding Left Breast  25-Mar.-2018 3:29 pm  25-Mar.-2018 3:42 pm
1109  Feeding Left Breast  25-Mar.-2018 6:16 pm  25-Mar.-2018 6:29 pm
1114  Feeding Left Breast  25-Mar.-2018 9:36 pm  25-Mar.-2018 9:41 pm
1117  Feeding Left Breast 25-Mar.-2018 11:05 pm 25-Mar.-2018 11:18 pm
1122  Feeding Left Breast  26-Mar.-2018 6:02 am  26-Mar.-2018 6:10 am
1129  Feeding Left Breast 26-Mar.-2018 10:19 am 26-Mar.-2018 10:28 am
1135  Feeding Left Breast  26-Mar.-2018 3:35 pm  26-Mar.-2018 3:46 pm
1140  Feeding Left Breast  26-Mar.-2018 6:49 pm  26-Mar.-2018 7:05 pm
1145  Feeding Left Breast 26-Mar.-2018 10:21 pm 26-Mar.-2018 10:38 pm
1146  Feeding Left Breast 26-Mar.-2018 10:51 pm 26-Mar.-2018 10:56 pm
1149  Feeding Left Breast 27-Mar.-2018 12:11 am 27-Mar.-2018 12:20 am
1154  Feeding Left Breast  27-Mar.-2018 6:47 am  27-Mar.-2018 7:00 am
1160  Feeding Left Breast 27-Mar.-2018 11:20 am 27-Mar.-2018 11:34 am
1166  Feeding Left Breast  27-Mar.-2018 2:33 pm  27-Mar.-2018 2:41 pm
1172  Feeding Left Breast  27-Mar.-2018 5:45 pm  27-Mar.-2018 5:56 pm
1176  Feeding Left Breast  27-Mar.-2018 8:11 pm  27-Mar.-2018 8:22 pm
1181  Feeding Left Breast 27-Mar.-2018 10:27 pm 27-Mar.-2018 10:43 pm
1185  Feeding Left Breast  28-Mar.-2018 7:21 am  28-Mar.-2018 7:26 am
1188  Feeding Left Breast 28-Mar.-2018 11:10 am 28-Mar.-2018 11:25 am
1194  Feeding Left Breast  28-Mar.-2018 4:34 pm  28-Mar.-2018 4:43 pm
1195  Feeding Left Breast  28-Mar.-2018 4:47 pm  28-Mar.-2018 4:52 pm
1200  Feeding Left Breast  28-Mar.-2018 8:58 pm  28-Mar.-2018 9:08 pm
1204  Feeding Left Breast  28-Mar.-2018 9:39 pm  28-Mar.-2018 9:53 pm
1210  Feeding Left Breast  29-Mar.-2018 7:13 am  29-Mar.-2018 7:25 am
1217  Feeding Left Breast  29-Mar.-2018 1:39 pm  29-Mar.-2018 1:51 pm
1220  Feeding Left Breast  29-Mar.-2018 2:13 pm  29-Mar.-2018 2:26 pm
1225  Feeding Left Breast  29-Mar.-2018 6:37 pm  29-Mar.-2018 6:50 pm
1230  Feeding Left Breast  29-Mar.-2018 9:42 pm  29-Mar.-2018 9:55 pm
1235  Feeding Left Breast 30-Mar.-2018 12:01 am 30-Mar.-2018 12:13 am
1240  Feeding Left Breast  30-Mar.-2018 7:42 am  30-Mar.-2018 7:45 am
1243  Feeding Left Breast 30-Mar.-2018 11:35 am 30-Mar.-2018 11:52 am
1248  Feeding Left Breast  30-Mar.-2018 3:55 pm  30-Mar.-2018 4:04 pm
1254  Feeding Left Breast  30-Mar.-2018 7:34 pm  30-Mar.-2018 7:50 pm
1260  Feeding Left Breast 30-Mar.-2018 11:27 pm 31-Mar.-2018 12:00 am
1265  Feeding Left Breast  31-Mar.-2018 2:02 am  31-Mar.-2018 2:08 am
1266  Feeding Left Breast  31-Mar.-2018 2:15 am  31-Mar.-2018 2:21 am
1270  Feeding Left Breast  31-Mar.-2018 8:29 am  31-Mar.-2018 8:38 am
1275  Feeding Left Breast 31-Mar.-2018 12:47 pm  31-Mar.-2018 1:03 pm
1281  Feeding Left Breast  31-Mar.-2018 6:01 pm  31-Mar.-2018 6:12 pm
1287  Feeding Left Breast  31-Mar.-2018 9:45 pm 31-Mar.-2018 10:00 pm
1293  Feeding Left Breast  01-Apr.-2018 2:11 am  01-Apr.-2018 2:24 am
1300  Feeding Left Breast  01-Apr.-2018 9:20 am  01-Apr.-2018 9:31 am
1307  Feeding Left Breast 01-Apr.-2018 12:59 pm  01-Apr.-2018 1:10 pm
1316  Feeding Left Breast  01-Apr.-2018 5:27 pm  01-Apr.-2018 5:38 pm
1322  Feeding Left Breast  01-Apr.-2018 9:53 pm 01-Apr.-2018 10:09 pm
1323  Feeding Left Breast 01-Apr.-2018 10:17 pm 01-Apr.-2018 10:22 pm
1327  Feeding Left Breast  02-Apr.-2018 2:43 am  02-Apr.-2018 2:48 am
1328  Feeding Left Breast  02-Apr.-2018 2:53 am  02-Apr.-2018 2:57 am
1335  Feeding Left Breast 02-Apr.-2018 11:09 am 02-Apr.-2018 11:18 am
1343  Feeding Left Breast  02-Apr.-2018 4:28 pm  02-Apr.-2018 4:39 pm
1349  Feeding Left Breast  02-Apr.-2018 8:28 pm  02-Apr.-2018 8:39 pm
1352  Feeding Left Breast 02-Apr.-2018 10:26 pm 02-Apr.-2018 10:41 pm
1355  Feeding Left Breast  03-Apr.-2018 2:38 am  03-Apr.-2018 2:49 am
1362  Feeding Left Breast  03-Apr.-2018 7:56 am  03-Apr.-2018 8:13 am
1369  Feeding Left Breast  03-Apr.-2018 1:20 pm  03-Apr.-2018 1:30 pm
1375  Feeding Left Breast  03-Apr.-2018 5:49 pm  03-Apr.-2018 6:01 pm
1381  Feeding Left Breast 03-Apr.-2018 10:10 pm 03-Apr.-2018 10:26 pm
1387  Feeding Left Breast  04-Apr.-2018 5:48 am  04-Apr.-2018 5:55 am
1388  Feeding Left Breast  04-Apr.-2018 5:59 am  04-Apr.-2018 6:04 am
1389  Feeding Left Breast  04-Apr.-2018 6:22 am  04-Apr.-2018 6:27 am
1397  Feeding Left Breast 04-Apr.-2018 12:27 pm 04-Apr.-2018 12:38 pm
1404  Feeding Left Breast  04-Apr.-2018 3:35 pm  04-Apr.-2018 3:45 pm
1409  Feeding Left Breast  04-Apr.-2018 8:58 pm  04-Apr.-2018 9:07 pm
1414  Feeding Left Breast  05-Apr.-2018 3:18 am  05-Apr.-2018 3:27 am
1420  Feeding Left Breast  05-Apr.-2018 7:42 am  05-Apr.-2018 7:52 am
1427  Feeding Left Breast 05-Apr.-2018 12:57 pm  05-Apr.-2018 1:09 pm
1432  Feeding Left Breast  05-Apr.-2018 5:03 pm  05-Apr.-2018 5:15 pm
1438  Feeding Left Breast  05-Apr.-2018 9:01 pm  05-Apr.-2018 9:08 pm
1442  Feeding Left Breast 05-Apr.-2018 11:47 pm 06-Apr.-2018 12:01 am
1448  Feeding Left Breast  06-Apr.-2018 8:27 am  06-Apr.-2018 8:34 am
1449  Feeding Left Breast  06-Apr.-2018 8:35 am  06-Apr.-2018 8:39 am
1457  Feeding Left Breast  06-Apr.-2018 1:14 pm  06-Apr.-2018 1:27 pm
1462  Feeding Left Breast  06-Apr.-2018 5:00 pm  06-Apr.-2018 5:23 pm
1469  Feeding Left Breast  06-Apr.-2018 8:54 pm  06-Apr.-2018 9:07 pm
1472  Feeding Left Breast 06-Apr.-2018 10:36 pm 06-Apr.-2018 10:51 pm
1478  Feeding Left Breast  07-Apr.-2018 4:33 am  07-Apr.-2018 4:41 am
1488  Feeding Left Breast 07-Apr.-2018 12:05 pm 07-Apr.-2018 12:18 pm
1495  Feeding Left Breast  07-Apr.-2018 2:53 pm  07-Apr.-2018 3:00 pm
1501  Feeding Left Breast  07-Apr.-2018 6:12 pm  07-Apr.-2018 6:21 pm
1508  Feeding Left Breast  07-Apr.-2018 8:53 pm  07-Apr.-2018 9:01 pm
1512  Feeding Left Breast 07-Apr.-2018 11:48 pm 08-Apr.-2018 12:02 am
1519  Feeding Left Breast 08-Apr.-2018 10:22 am 08-Apr.-2018 10:34 am
1526  Feeding Left Breast  08-Apr.-2018 2:14 pm  08-Apr.-2018 2:28 pm
1531  Feeding Left Breast  08-Apr.-2018 5:51 pm  08-Apr.-2018 6:05 pm
1537  Feeding Left Breast  08-Apr.-2018 9:22 pm  08-Apr.-2018 9:40 pm
1542  Feeding Left Breast 09-Apr.-2018 12:00 am 09-Apr.-2018 12:11 am
1546  Feeding Left Breast  09-Apr.-2018 4:53 am  09-Apr.-2018 5:05 am
1547  Feeding Left Breast  09-Apr.-2018 5:09 am  09-Apr.-2018 5:13 am
1554  Feeding Left Breast 09-Apr.-2018 11:31 am 09-Apr.-2018 11:37 am
1558  Feeding Left Breast  09-Apr.-2018 1:37 pm  09-Apr.-2018 1:45 pm
1564  Feeding Left Breast  09-Apr.-2018 5:30 pm  09-Apr.-2018 5:52 pm
1571  Feeding Left Breast  09-Apr.-2018 9:29 pm  09-Apr.-2018 9:50 pm
1576  Feeding Left Breast  10-Apr.-2018 4:13 am  10-Apr.-2018 4:23 am
1578  Feeding Left Breast  10-Apr.-2018 4:39 am  10-Apr.-2018 4:42 am
1589  Feeding Left Breast  10-Apr.-2018 2:07 pm  10-Apr.-2018 2:23 pm
1595  Feeding Left Breast  10-Apr.-2018 6:54 pm  10-Apr.-2018 7:07 pm
1602  Feeding Left Breast  10-Apr.-2018 9:48 pm 10-Apr.-2018 10:04 pm
1606  Feeding Left Breast 11-Apr.-2018 12:37 am 11-Apr.-2018 12:50 am
1610  Feeding Left Breast  11-Apr.-2018 1:52 am  11-Apr.-2018 1:59 am
1616  Feeding Left Breast  11-Apr.-2018 9:31 am  11-Apr.-2018 9:42 am
1625  Feeding Left Breast  11-Apr.-2018 2:25 pm  11-Apr.-2018 2:39 pm
1634  Feeding Left Breast  11-Apr.-2018 6:34 pm  11-Apr.-2018 6:47 pm
1635  Feeding Left Breast  11-Apr.-2018 6:52 pm  11-Apr.-2018 6:59 pm
1638  Feeding Left Breast  11-Apr.-2018 8:52 pm  11-Apr.-2018 9:04 pm
1645  Feeding Left Breast  12-Apr.-2018 4:01 am  12-Apr.-2018 4:14 am
1652  Feeding Left Breast  12-Apr.-2018 9:48 am 12-Apr.-2018 10:06 am
1662  Feeding Left Breast  12-Apr.-2018 3:34 pm  12-Apr.-2018 3:44 pm
1666  Feeding Left Breast  12-Apr.-2018 5:47 pm  12-Apr.-2018 5:57 pm
1671  Feeding Left Breast  12-Apr.-2018 9:43 pm 12-Apr.-2018 10:09 pm
1677  Feeding Left Breast 13-Apr.-2018 12:59 am  13-Apr.-2018 1:08 am
1685  Feeding Left Breast 13-Apr.-2018 10:04 am 13-Apr.-2018 10:15 am
1688  Feeding Left Breast 13-Apr.-2018 12:03 pm 13-Apr.-2018 12:14 pm
1696  Feeding Left Breast  13-Apr.-2018 6:53 pm  13-Apr.-2018 7:07 pm
1700  Feeding Left Breast  13-Apr.-2018 9:31 pm  13-Apr.-2018 9:43 pm
1701  Feeding Left Breast  13-Apr.-2018 9:47 pm  13-Apr.-2018 9:52 pm
1705  Feeding Left Breast 14-Apr.-2018 12:06 am 14-Apr.-2018 12:17 am
1714  Feeding Left Breast  14-Apr.-2018 9:53 am 14-Apr.-2018 10:04 am
1724  Feeding Left Breast  14-Apr.-2018 3:40 pm  14-Apr.-2018 3:50 pm
1727  Feeding Left Breast  14-Apr.-2018 5:28 pm  14-Apr.-2018 5:30 pm
1730  Feeding Left Breast  14-Apr.-2018 7:29 pm  14-Apr.-2018 7:43 pm
1738  Feeding Left Breast 14-Apr.-2018 10:01 pm 14-Apr.-2018 10:15 pm
1739  Feeding Left Breast 14-Apr.-2018 10:17 pm 14-Apr.-2018 10:24 pm
1745  Feeding Left Breast  15-Apr.-2018 1:30 am  15-Apr.-2018 1:48 am
1750  Feeding Left Breast  15-Apr.-2018 5:16 am  15-Apr.-2018 5:27 am
1758  Feeding Left Breast 15-Apr.-2018 10:54 am 15-Apr.-2018 11:02 am
1760  Feeding Left Breast 15-Apr.-2018 11:09 am 15-Apr.-2018 11:17 am
1767  Feeding Left Breast  15-Apr.-2018 4:57 pm  15-Apr.-2018 5:11 pm
1772  Feeding Left Breast  15-Apr.-2018 8:00 pm  15-Apr.-2018 8:09 pm
1778  Feeding Left Breast 15-Apr.-2018 10:34 pm 15-Apr.-2018 10:46 pm
1781  Feeding Left Breast 16-Apr.-2018 12:51 am  16-Apr.-2018 1:06 am
1789  Feeding Left Breast  16-Apr.-2018 8:05 am  16-Apr.-2018 8:17 am
1796  Feeding Left Breast 16-Apr.-2018 12:08 pm 16-Apr.-2018 12:20 pm
1804  Feeding Left Breast  16-Apr.-2018 3:40 pm  16-Apr.-2018 3:49 pm
1809  Feeding Left Breast  16-Apr.-2018 6:47 pm  16-Apr.-2018 6:56 pm
1814  Feeding Left Breast 16-Apr.-2018 10:45 pm 16-Apr.-2018 10:58 pm
1824  Feeding Left Breast  17-Apr.-2018 7:51 am  17-Apr.-2018 8:08 am
1830  Feeding Left Breast 17-Apr.-2018 11:43 am 17-Apr.-2018 11:53 am
1836  Feeding Left Breast  17-Apr.-2018 4:32 pm  17-Apr.-2018 4:40 pm
1842  Feeding Left Breast 17-Apr.-2018 10:03 pm 17-Apr.-2018 10:14 pm
1849  Feeding Left Breast  18-Apr.-2018 5:11 am  18-Apr.-2018 5:26 am
1855  Feeding Left Breast 18-Apr.-2018 11:03 am 18-Apr.-2018 11:16 am
1860  Feeding Left Breast  18-Apr.-2018 2:38 pm  18-Apr.-2018 2:51 pm
1863  Feeding Left Breast  18-Apr.-2018 4:05 pm  18-Apr.-2018 4:12 pm
1869  Feeding Left Breast  18-Apr.-2018 8:53 pm  18-Apr.-2018 9:09 pm
1874  Feeding Left Breast 18-Apr.-2018 11:22 pm 18-Apr.-2018 11:35 pm
1880  Feeding Left Breast  19-Apr.-2018 8:31 am  19-Apr.-2018 8:38 am
1882  Feeding Left Breast  19-Apr.-2018 9:03 am  19-Apr.-2018 9:09 am
1892  Feeding Left Breast  19-Apr.-2018 1:42 pm  19-Apr.-2018 1:50 pm
1897  Feeding Left Breast  19-Apr.-2018 5:22 pm  19-Apr.-2018 5:37 pm
1908  Feeding Left Breast 19-Apr.-2018 10:15 pm 19-Apr.-2018 10:23 pm
1913  Feeding Left Breast  20-Apr.-2018 3:46 am  20-Apr.-2018 3:55 am
1920  Feeding Left Breast 20-Apr.-2018 11:03 am 20-Apr.-2018 11:15 am
1926  Feeding Left Breast  20-Apr.-2018 3:15 pm  20-Apr.-2018 3:22 pm
1934  Feeding Left Breast  20-Apr.-2018 6:54 pm  20-Apr.-2018 7:03 pm
1940  Feeding Left Breast  21-Apr.-2018 1:54 am  21-Apr.-2018 2:01 am
1942  Feeding Left Breast  21-Apr.-2018 2:19 am  21-Apr.-2018 2:22 am
1949  Feeding Left Breast  21-Apr.-2018 8:49 am  21-Apr.-2018 8:57 am
1950  Feeding Left Breast  21-Apr.-2018 8:58 am  21-Apr.-2018 9:05 am
1958  Feeding Left Breast  21-Apr.-2018 1:46 pm  21-Apr.-2018 1:56 pm
1965  Feeding Left Breast  21-Apr.-2018 4:44 pm  21-Apr.-2018 4:50 pm
1968  Feeding Left Breast  21-Apr.-2018 6:42 pm  21-Apr.-2018 6:50 pm
1974  Feeding Left Breast  21-Apr.-2018 9:45 pm 21-Apr.-2018 10:00 pm
1976  Feeding Left Breast 21-Apr.-2018 10:25 pm 21-Apr.-2018 10:26 pm
1977  Feeding Left Breast 21-Apr.-2018 10:28 pm 21-Apr.-2018 10:35 pm
1981  Feeding Left Breast 22-Apr.-2018 12:31 am 22-Apr.-2018 12:36 am
1983  Feeding Left Breast  22-Apr.-2018 1:05 am  22-Apr.-2018 1:13 am
1989  Feeding Left Breast 22-Apr.-2018 11:44 am 22-Apr.-2018 12:04 pm
1998  Feeding Left Breast  22-Apr.-2018 4:33 pm  22-Apr.-2018 4:44 pm
1999  Feeding Left Breast  22-Apr.-2018 4:47 pm  22-Apr.-2018 4:54 pm
2005  Feeding Left Breast  22-Apr.-2018 7:47 pm  22-Apr.-2018 7:59 pm
2012  Feeding Left Breast 22-Apr.-2018 11:32 pm 22-Apr.-2018 11:43 pm
2018  Feeding Left Breast  23-Apr.-2018 8:30 am  23-Apr.-2018 8:41 am
2019  Feeding Left Breast  23-Apr.-2018 8:45 am  23-Apr.-2018 8:50 am
2024  Feeding Left Breast 23-Apr.-2018 11:25 am 23-Apr.-2018 11:33 am
2033  Feeding Left Breast  23-Apr.-2018 4:51 pm  23-Apr.-2018 4:58 pm
2036  Feeding Left Breast  23-Apr.-2018 7:01 pm  23-Apr.-2018 7:10 pm
2040  Feeding Left Breast  23-Apr.-2018 8:39 pm  23-Apr.-2018 8:47 pm
2045  Feeding Left Breast 23-Apr.-2018 10:40 pm 23-Apr.-2018 10:46 pm
2048  Feeding Left Breast  24-Apr.-2018 3:50 am  24-Apr.-2018 3:53 am
2050  Feeding Left Breast  24-Apr.-2018 4:40 am  24-Apr.-2018 4:43 am
2053  Feeding Left Breast  24-Apr.-2018 8:52 am  24-Apr.-2018 9:05 am
2061  Feeding Left Breast  24-Apr.-2018 2:17 pm  24-Apr.-2018 2:25 pm
2065  Feeding Left Breast  24-Apr.-2018 4:50 pm  24-Apr.-2018 4:57 pm
2070  Feeding Left Breast  24-Apr.-2018 7:02 pm  24-Apr.-2018 7:13 pm
2071  Feeding Left Breast  24-Apr.-2018 7:15 pm  24-Apr.-2018 7:18 pm
2077  Feeding Left Breast  24-Apr.-2018 9:42 pm  24-Apr.-2018 9:48 pm
2084  Feeding Left Breast  25-Apr.-2018 5:26 am  25-Apr.-2018 5:38 am
2090  Feeding Left Breast 25-Apr.-2018 10:48 am 25-Apr.-2018 10:53 am
2094  Feeding Left Breast 25-Apr.-2018 12:30 pm 25-Apr.-2018 12:38 pm
2100  Feeding Left Breast  25-Apr.-2018 5:26 pm  25-Apr.-2018 5:37 pm
2106  Feeding Left Breast  25-Apr.-2018 8:53 pm  25-Apr.-2018 9:08 pm
2111  Feeding Left Breast 25-Apr.-2018 11:52 pm 25-Apr.-2018 11:58 pm
2115  Feeding Left Breast  26-Apr.-2018 3:15 am  26-Apr.-2018 3:24 am
2120  Feeding Left Breast  26-Apr.-2018 9:41 am  26-Apr.-2018 9:49 am
2130  Feeding Left Breast  26-Apr.-2018 3:14 pm  26-Apr.-2018 3:22 pm
2138  Feeding Left Breast  26-Apr.-2018 8:03 pm  26-Apr.-2018 8:13 pm
2145  Feeding Left Breast  27-Apr.-2018 3:25 am  27-Apr.-2018 3:33 am
2146  Feeding Left Breast  27-Apr.-2018 4:09 am  27-Apr.-2018 4:14 am
2153  Feeding Left Breast 27-Apr.-2018 10:31 am 27-Apr.-2018 10:40 am
2154  Feeding Left Breast 27-Apr.-2018 11:15 am 27-Apr.-2018 11:18 am
2159  Feeding Left Breast  27-Apr.-2018 3:35 pm  27-Apr.-2018 3:44 pm
2164  Feeding Left Breast  27-Apr.-2018 7:27 pm  27-Apr.-2018 7:38 pm
2169  Feeding Left Breast 27-Apr.-2018 10:00 pm 27-Apr.-2018 10:17 pm
2174  Feeding Left Breast  28-Apr.-2018 4:32 am  28-Apr.-2018 4:39 am
2183  Feeding Left Breast 28-Apr.-2018 12:21 pm 28-Apr.-2018 12:32 pm
2190  Feeding Left Breast  28-Apr.-2018 5:58 pm  28-Apr.-2018 6:07 pm
2200  Feeding Left Breast 28-Apr.-2018 10:28 pm 28-Apr.-2018 10:38 pm
2203  Feeding Left Breast  29-Apr.-2018 8:19 am  29-Apr.-2018 8:23 am
2205  Feeding Left Breast 29-Apr.-2018 10:02 am 29-Apr.-2018 10:10 am
2212  Feeding Left Breast  29-Apr.-2018 4:05 pm  29-Apr.-2018 4:14 pm
2218  Feeding Left Breast  29-Apr.-2018 8:29 pm  29-Apr.-2018 8:47 pm
2224  Feeding Left Breast 29-Apr.-2018 11:34 pm 29-Apr.-2018 11:45 pm
2229  Feeding Left Breast  30-Apr.-2018 9:22 am  30-Apr.-2018 9:29 am
2235  Feeding Left Breast 30-Apr.-2018 12:58 pm  30-Apr.-2018 1:07 pm
2243  Feeding Left Breast  30-Apr.-2018 5:29 pm  30-Apr.-2018 5:40 pm
2248  Feeding Left Breast  30-Apr.-2018 8:32 pm  30-Apr.-2018 8:40 pm
2253  Feeding Left Breast 30-Apr.-2018 10:16 pm 30-Apr.-2018 10:23 pm
2259  Feeding Left Breast         1/05/18 10:43         1/05/18 10:55
2266  Feeding Left Breast         1/05/18 17:01         1/05/18 17:09
2271  Feeding Left Breast         1/05/18 21:06         1/05/18 21:14
2273  Feeding Left Breast         1/05/18 21:26         1/05/18 21:33
2276  Feeding Left Breast         1/05/18 23:18         1/05/18 23:25
2284  Feeding Left Breast         2/05/18 10:53         2/05/18 11:05
2290  Feeding Left Breast         2/05/18 14:13         2/05/18 14:24
2294  Feeding Left Breast         2/05/18 16:57         2/05/18 17:07
2299  Feeding Left Breast         2/05/18 21:55         2/05/18 22:11
2303  Feeding Left Breast         2/05/18 23:50         2/05/18 23:58
2308  Feeding Left Breast         3/05/18 10:57         3/05/18 11:06
2316  Feeding Left Breast         3/05/18 15:41         3/05/18 15:55
2321  Feeding Left Breast         3/05/18 19:04         3/05/18 19:12
2326  Feeding Left Breast         3/05/18 22:30         3/05/18 22:40
2328  Feeding Left Breast         3/05/18 23:45         3/05/18 23:55
2332  Feeding Left Breast          4/05/18 7:23          4/05/18 7:27
2334  Feeding Left Breast         4/05/18 10:18         4/05/18 10:32
2341  Feeding Left Breast         4/05/18 14:55         4/05/18 15:05
2347  Feeding Left Breast         4/05/18 18:49         4/05/18 18:59
2348  Feeding Left Breast         4/05/18 19:00         4/05/18 19:04
2353  Feeding Left Breast         4/05/18 22:27         4/05/18 22:38
2355  Feeding Left Breast         4/05/18 23:40         4/05/18 23:46
2360  Feeding Left Breast          5/05/18 7:18          5/05/18 7:29
2361  Feeding Left Breast          5/05/18 7:35          5/05/18 7:42
2369  Feeding Left Breast         5/05/18 14:56         5/05/18 15:06
2374  Feeding Left Breast         5/05/18 17:30         5/05/18 17:39
2379  Feeding Left Breast         5/05/18 21:25         5/05/18 21:33
2380  Feeding Left Breast         5/05/18 21:38         5/05/18 21:42
2384  Feeding Left Breast          6/05/18 0:09          6/05/18 0:21
2392  Feeding Left Breast         6/05/18 11:13         6/05/18 11:23
2395  Feeding Left Breast         6/05/18 12:44         6/05/18 12:52
2401  Feeding Left Breast         6/05/18 16:38         6/05/18 16:48
2407  Feeding Left Breast         6/05/18 18:47         6/05/18 18:55
2412  Feeding Left Breast         6/05/18 21:50         6/05/18 21:59
2417  Feeding Left Breast          7/05/18 7:45          7/05/18 7:55
2419  Feeding Left Breast          7/05/18 8:04          7/05/18 8:07
2425  Feeding Left Breast         7/05/18 11:17         7/05/18 11:24
2433  Feeding Left Breast         7/05/18 15:36         7/05/18 15:51
2440  Feeding Left Breast         7/05/18 18:52         7/05/18 19:00
2447  Feeding Left Breast         7/05/18 21:48         7/05/18 21:58
2451  Feeding Left Breast         7/05/18 23:28         7/05/18 23:40
2455  Feeding Left Breast          8/05/18 9:05          8/05/18 9:17
2463  Feeding Left Breast         8/05/18 12:40         8/05/18 12:50
2471  Feeding Left Breast         8/05/18 17:18         8/05/18 17:30
2477  Feeding Left Breast         8/05/18 21:09         8/05/18 21:18
2480  Feeding Left Breast         8/05/18 22:17         8/05/18 22:25
2484  Feeding Left Breast          9/05/18 7:11          9/05/18 7:17
2487  Feeding Left Breast          9/05/18 9:14          9/05/18 9:21
2493  Feeding Left Breast         9/05/18 12:26         9/05/18 12:34
2500  Feeding Left Breast         9/05/18 17:21         9/05/18 17:31
2508  Feeding Left Breast         9/05/18 22:36         9/05/18 22:48
2512  Feeding Left Breast         10/05/18 4:57         10/05/18 5:03
2519  Feeding Left Breast        10/05/18 10:24        10/05/18 10:33
2526  Feeding Left Breast        10/05/18 14:27        10/05/18 14:40
2532  Feeding Left Breast        10/05/18 20:04        10/05/18 20:12
2537  Feeding Left Breast        10/05/18 23:11        10/05/18 23:24
2541  Feeding Left Breast         11/05/18 7:03         11/05/18 7:06
2544  Feeding Left Breast        11/05/18 12:02        11/05/18 12:14
2551  Feeding Left Breast        11/05/18 15:52        11/05/18 16:04
2555  Feeding Left Breast        11/05/18 18:32        11/05/18 18:40
2559  Feeding Left Breast        11/05/18 21:45        11/05/18 21:53
2566  Feeding Left Breast         12/05/18 6:19         12/05/18 6:32
2570  Feeding Left Breast        12/05/18 11:36        12/05/18 11:47
2577  Feeding Left Breast        12/05/18 15:59        12/05/18 16:08
2583  Feeding Left Breast        12/05/18 21:48        12/05/18 21:59
2585  Feeding Left Breast        12/05/18 22:55        12/05/18 23:09
2591  Feeding Left Breast         13/05/18 8:57         13/05/18 9:09
2598  Feeding Left Breast        13/05/18 13:59        13/05/18 14:07
2605  Feeding Left Breast        13/05/18 18:02        13/05/18 18:21
2611  Feeding Left Breast        13/05/18 21:35        13/05/18 21:46
2615  Feeding Left Breast         14/05/18 0:13         14/05/18 0:17
2619  Feeding Left Breast         14/05/18 8:49         14/05/18 8:52
2624  Feeding Left Breast        14/05/18 11:42        14/05/18 11:51
2631  Feeding Left Breast        14/05/18 15:57        14/05/18 16:08
2634  Feeding Left Breast        14/05/18 17:37        14/05/18 17:42
2640  Feeding Left Breast        14/05/18 21:28        14/05/18 21:40
2644  Feeding Left Breast        14/05/18 23:26        14/05/18 23:33
2645  Feeding Left Breast        14/05/18 23:37        14/05/18 23:40
2651  Feeding Left Breast        15/05/18 12:21        15/05/18 12:29
2652  Feeding Left Breast        15/05/18 12:31        15/05/18 12:37
2657  Feeding Left Breast        15/05/18 15:25        15/05/18 15:36
2664  Feeding Left Breast        15/05/18 19:10        15/05/18 19:24
2670  Feeding Left Breast        15/05/18 22:10        15/05/18 22:24
2674  Feeding Left Breast         16/05/18 6:37         16/05/18 6:53
2678  Feeding Left Breast        16/05/18 11:15        16/05/18 11:35
2683  Feeding Left Breast        16/05/18 14:44        16/05/18 14:53
2688  Feeding Left Breast        16/05/18 18:22        16/05/18 18:33
2691  Feeding Left Breast        16/05/18 20:20        16/05/18 20:32
2696  Feeding Left Breast         17/05/18 7:13         17/05/18 7:24
2701  Feeding Left Breast        17/05/18 11:23        17/05/18 11:34
2710  Feeding Left Breast        17/05/18 17:43        17/05/18 17:58
2719  Feeding Left Breast        17/05/18 22:10        17/05/18 22:24
2720  Feeding Left Breast        17/05/18 22:33        17/05/18 22:42
2725  Feeding Left Breast         18/05/18 6:56         18/05/18 7:06
2726  Feeding Left Breast         18/05/18 7:10         18/05/18 7:15
2730  Feeding Left Breast        18/05/18 10:34        18/05/18 10:39
2737  Feeding Left Breast        18/05/18 17:08        18/05/18 17:20
2742  Feeding Left Breast        18/05/18 20:32        18/05/18 20:45
2748  Feeding Left Breast        18/05/18 23:02        18/05/18 23:10
2755  Feeding Left Breast        19/05/18 10:24        19/05/18 10:36
2762  Feeding Left Breast        19/05/18 13:43        19/05/18 13:49
2766  Feeding Left Breast        19/05/18 16:31        19/05/18 16:40
2772  Feeding Left Breast        19/05/18 18:59        19/05/18 19:21
2778  Feeding Left Breast        19/05/18 23:08        19/05/18 23:22
2780  Feeding Left Breast        19/05/18 23:49        19/05/18 23:56
2784  Feeding Left Breast        20/05/18 10:49        20/05/18 10:59
2788  Feeding Left Breast        20/05/18 12:16        20/05/18 12:24
2791  Feeding Left Breast        20/05/18 13:36        20/05/18 13:40
2796  Feeding Left Breast        20/05/18 17:25        20/05/18 17:35
2802  Feeding Left Breast        20/05/18 21:41        20/05/18 21:54
2803  Feeding Left Breast        20/05/18 21:58        20/05/18 22:03
2808  Feeding Left Breast         21/05/18 5:56         21/05/18 6:04
2813  Feeding Left Breast        21/05/18 10:18        21/05/18 10:23
2814  Feeding Left Breast        21/05/18 10:24        21/05/18 10:28
2819  Feeding Left Breast        21/05/18 14:18        21/05/18 14:31
2826  Feeding Left Breast        21/05/18 19:36        21/05/18 19:47
2831  Feeding Left Breast        21/05/18 21:39        21/05/18 21:49
2838  Feeding Left Breast         22/05/18 8:56         22/05/18 9:04
2848  Feeding Left Breast        22/05/18 16:20        22/05/18 16:33
2852  Feeding Left Breast        22/05/18 20:48        22/05/18 21:01
2855  Feeding Left Breast        22/05/18 23:25        22/05/18 23:33
2860  Feeding Left Breast         23/05/18 9:34         23/05/18 9:40
2861  Feeding Left Breast        23/05/18 10:28        23/05/18 10:32
2868  Feeding Left Breast        23/05/18 16:14        23/05/18 16:24
2872  Feeding Left Breast        23/05/18 19:32        23/05/18 19:40
2873  Feeding Left Breast        23/05/18 19:42        23/05/18 19:45
2880  Feeding Left Breast        23/05/18 23:03        23/05/18 23:18
2884  Feeding Left Breast         24/05/18 9:17         24/05/18 9:33
2895  Feeding Left Breast        24/05/18 16:34        24/05/18 16:42
2896  Feeding Left Breast        24/05/18 16:57        24/05/18 17:02
2901  Feeding Left Breast        24/05/18 19:12        24/05/18 19:28
2907  Feeding Left Breast        24/05/18 23:10        24/05/18 23:21
2911  Feeding Left Breast         25/05/18 9:46         25/05/18 9:57
2921  Feeding Left Breast        25/05/18 15:38        25/05/18 15:51
2929  Feeding Left Breast        25/05/18 20:50        25/05/18 21:03
2937  Feeding Left Breast         26/05/18 7:40         26/05/18 7:49
2943  Feeding Left Breast        26/05/18 14:00        26/05/18 14:07
2948  Feeding Left Breast        26/05/18 17:09        26/05/18 17:25
2957  Feeding Left Breast        26/05/18 20:47        26/05/18 20:58
2962  Feeding Left Breast        26/05/18 23:45        26/05/18 23:53
2965  Feeding Left Breast        27/05/18 10:33        27/05/18 10:44
2967  Feeding Left Breast        27/05/18 11:51        27/05/18 11:59
2973  Feeding Left Breast        27/05/18 16:12        27/05/18 16:23
2979  Feeding Left Breast        27/05/18 19:43        27/05/18 19:58
2984  Feeding Left Breast        27/05/18 22:08        27/05/18 22:20
2987  Feeding Left Breast        27/05/18 23:41        27/05/18 23:49
2992  Feeding Left Breast         28/05/18 7:44         28/05/18 7:53
3000  Feeding Left Breast        28/05/18 15:01        28/05/18 15:11
3006  Feeding Left Breast        28/05/18 19:06        28/05/18 19:17
3012  Feeding Left Breast        28/05/18 22:39        28/05/18 22:47
3016  Feeding Left Breast         29/05/18 7:15         29/05/18 7:31
3023  Feeding Left Breast        29/05/18 14:00        29/05/18 14:14
3029  Feeding Left Breast        29/05/18 17:54        29/05/18 18:07
3035  Feeding Left Breast        29/05/18 22:25        29/05/18 22:36
3039  Feeding Left Breast         30/05/18 8:47         30/05/18 8:58
3047  Feeding Left Breast        30/05/18 12:45        30/05/18 12:55
3051  Feeding Left Breast        30/05/18 16:54        30/05/18 17:04
3057  Feeding Left Breast        30/05/18 20:20        30/05/18 20:23
3059  Feeding Left Breast        30/05/18 20:55        30/05/18 21:07
3065  Feeding Left Breast         31/05/18 7:30         31/05/18 7:39
3072  Feeding Left Breast        31/05/18 12:34        31/05/18 12:47
3080  Feeding Left Breast 27-Jun.-2018 11:01 am 27-Jun.-2018 11:05 am
3083  Feeding Left Breast  27-Jun.-2018 1:22 pm  27-Jun.-2018 1:30 pm
3089  Feeding Left Breast  27-Jun.-2018 5:33 pm  27-Jun.-2018 5:45 pm
3094  Feeding Left Breast 27-Jun.-2018 10:09 pm 27-Jun.-2018 10:23 pm
3098  Feeding Left Breast  28-Jun.-2018 8:04 am  28-Jun.-2018 8:13 am
3103  Feeding Left Breast 28-Jun.-2018 12:28 pm 28-Jun.-2018 12:37 pm
3108  Feeding Left Breast  28-Jun.-2018 4:33 pm  28-Jun.-2018 4:46 pm
3114  Feeding Left Breast  28-Jun.-2018 8:25 pm  28-Jun.-2018 8:34 pm
3119  Feeding Left Breast  29-Jun.-2018 2:44 am  29-Jun.-2018 2:51 am
3125  Feeding Left Breast 29-Jun.-2018 10:17 am 29-Jun.-2018 10:25 am
3131  Feeding Left Breast  29-Jun.-2018 4:14 pm  29-Jun.-2018 4:27 pm
3138  Feeding Left Breast  29-Jun.-2018 8:38 pm  29-Jun.-2018 8:46 pm
3142  Feeding Left Breast  30-Jun.-2018 8:19 am  30-Jun.-2018 8:28 am
3147  Feeding Left Breast  30-Jun.-2018 1:07 pm  30-Jun.-2018 1:23 pm
3153  Feeding Left Breast  30-Jun.-2018 5:24 pm  30-Jun.-2018 5:41 pm
3159  Feeding Left Breast 30-Jun.-2018 10:08 pm 30-Jun.-2018 10:31 pm
3164  Feeding Left Breast  01-Jul.-2018 9:10 am  01-Jul.-2018 9:16 am
3170  Feeding Left Breast  01-Jul.-2018 1:44 pm  01-Jul.-2018 1:52 pm
3174  Feeding Left Breast  01-Jul.-2018 6:11 pm  01-Jul.-2018 6:24 pm
3180  Feeding Left Breast 01-Jul.-2018 10:27 pm 01-Jul.-2018 10:38 pm
3186  Feeding Left Breast  02-Jul.-2018 9:20 am  02-Jul.-2018 9:25 am
3192  Feeding Left Breast  02-Jul.-2018 2:20 pm  02-Jul.-2018 2:30 pm
3197  Feeding Left Breast  02-Jul.-2018 5:20 pm  02-Jul.-2018 5:26 pm
3203  Feeding Left Breast  02-Jul.-2018 9:56 pm 02-Jul.-2018 10:05 pm
3206  Feeding Left Breast  03-Jul.-2018 7:00 am  03-Jul.-2018 7:15 am
3211  Feeding Left Breast 03-Jul.-2018 11:06 am 03-Jul.-2018 11:21 am
3217  Feeding Left Breast  03-Jul.-2018 2:59 pm  03-Jul.-2018 3:13 pm
3221  Feeding Left Breast  03-Jul.-2018 6:53 pm  03-Jul.-2018 7:10 pm
3225  Feeding Left Breast 03-Jul.-2018 10:14 pm 03-Jul.-2018 10:28 pm
3228  Feeding Left Breast  04-Jul.-2018 2:12 am  04-Jul.-2018 2:14 am
3231  Feeding Left Breast  04-Jul.-2018 3:34 am  04-Jul.-2018 3:47 am
3236  Feeding Left Breast 04-Jul.-2018 12:19 pm 04-Jul.-2018 12:26 pm
3242  Feeding Left Breast  04-Jul.-2018 4:19 pm  04-Jul.-2018 4:29 pm
3251  Feeding Left Breast  04-Jul.-2018 7:46 pm  04-Jul.-2018 7:56 pm
3255  Feeding Left Breast  05-Jul.-2018 9:08 am  05-Jul.-2018 9:16 am
3261  Feeding Left Breast  05-Jul.-2018 1:05 pm  05-Jul.-2018 1:16 pm
3272  Feeding Left Breast  05-Jul.-2018 8:43 pm  05-Jul.-2018 8:51 pm
3277  Feeding Left Breast  06-Jul.-2018 7:51 am  06-Jul.-2018 8:00 am
3287  Feeding Left Breast  06-Jul.-2018 1:38 pm  06-Jul.-2018 1:46 pm
3293  Feeding Left Breast  06-Jul.-2018 5:54 pm  06-Jul.-2018 6:05 pm
3297  Feeding Left Breast  06-Jul.-2018 9:35 pm  06-Jul.-2018 9:54 pm
3302  Feeding Left Breast 07-Jul.-2018 10:14 am 07-Jul.-2018 10:23 am
3307  Feeding Left Breast  07-Jul.-2018 2:43 pm  07-Jul.-2018 3:00 pm
3314  Feeding Left Breast  07-Jul.-2018 6:27 pm  07-Jul.-2018 6:37 pm
3319  Feeding Left Breast 07-Jul.-2018 10:11 pm 07-Jul.-2018 10:33 pm
3323  Feeding Left Breast  08-Jul.-2018 2:50 am  08-Jul.-2018 2:52 am
3329  Feeding Left Breast 08-Jul.-2018 11:19 am 08-Jul.-2018 11:31 am
3337  Feeding Left Breast  08-Jul.-2018 4:40 pm  08-Jul.-2018 4:53 pm
3343  Feeding Left Breast  08-Jul.-2018 9:11 pm  08-Jul.-2018 9:31 pm
3347  Feeding Left Breast 08-Jul.-2018 11:45 pm 08-Jul.-2018 11:55 pm
3351  Feeding Left Breast 09-Jul.-2018 10:43 am 09-Jul.-2018 10:54 am
3360  Feeding Left Breast  09-Jul.-2018 3:46 pm  09-Jul.-2018 3:54 pm
3361  Feeding Left Breast  09-Jul.-2018 3:54 pm  09-Jul.-2018 3:56 pm
3366  Feeding Left Breast  09-Jul.-2018 7:03 pm  09-Jul.-2018 7:19 pm
3373  Feeding Left Breast 09-Jul.-2018 11:28 pm 09-Jul.-2018 11:37 pm
3378  Feeding Left Breast  10-Jul.-2018 9:41 am  10-Jul.-2018 9:51 am
3383  Feeding Left Breast  10-Jul.-2018 2:10 pm  10-Jul.-2018 2:20 pm
3389  Feeding Left Breast  10-Jul.-2018 5:45 pm  10-Jul.-2018 5:56 pm
3394  Feeding Left Breast  10-Jul.-2018 9:43 pm  10-Jul.-2018 9:57 pm
3400  Feeding Left Breast  11-Jul.-2018 7:07 am  11-Jul.-2018 7:07 am
3405  Feeding Left Breast 11-Jul.-2018 12:56 pm  11-Jul.-2018 1:08 pm
3411  Feeding Left Breast  11-Jul.-2018 5:28 pm  11-Jul.-2018 5:47 pm
3417  Feeding Left Breast 11-Jul.-2018 10:44 pm 11-Jul.-2018 10:53 pm
3422  Feeding Left Breast  12-Jul.-2018 8:39 am  12-Jul.-2018 8:51 am
3428  Feeding Left Breast  12-Jul.-2018 2:48 pm  12-Jul.-2018 2:56 pm
3433  Feeding Left Breast  12-Jul.-2018 6:30 pm  12-Jul.-2018 6:40 pm
3438  Feeding Left Breast 12-Jul.-2018 11:02 pm 12-Jul.-2018 11:24 pm
3443  Feeding Left Breast  13-Jul.-2018 8:20 am  13-Jul.-2018 8:41 am
3452  Feeding Left Breast  13-Jul.-2018 2:57 pm  13-Jul.-2018 3:08 pm
3454  Feeding Left Breast  13-Jul.-2018 3:49 pm  13-Jul.-2018 3:52 pm
3455  Feeding Left Breast  13-Jul.-2018 3:53 pm  13-Jul.-2018 3:58 pm
3463  Feeding Left Breast  13-Jul.-2018 7:14 pm  13-Jul.-2018 7:28 pm
3468  Feeding Left Breast 13-Jul.-2018 10:53 pm 13-Jul.-2018 11:14 pm
3472  Feeding Left Breast 14-Jul.-2018 10:36 am 14-Jul.-2018 10:44 am
3478  Feeding Left Breast  14-Jul.-2018 3:25 pm  14-Jul.-2018 3:43 pm
3484  Feeding Left Breast  14-Jul.-2018 7:29 pm  14-Jul.-2018 7:39 pm
3489  Feeding Left Breast 14-Jul.-2018 10:51 pm 14-Jul.-2018 11:02 pm
3493  Feeding Left Breast  15-Jul.-2018 7:55 am  15-Jul.-2018 8:04 am
3501  Feeding Left Breast  15-Jul.-2018 1:39 pm  15-Jul.-2018 1:45 pm
3508  Feeding Left Breast  15-Jul.-2018 5:22 pm  15-Jul.-2018 5:41 pm
3516  Feeding Left Breast 15-Jul.-2018 10:42 pm 15-Jul.-2018 10:53 pm
3521  Feeding Left Breast  16-Jul.-2018 9:09 am  16-Jul.-2018 9:20 am
3529  Feeding Left Breast  16-Jul.-2018 2:34 pm  16-Jul.-2018 2:43 pm
3536  Feeding Left Breast  16-Jul.-2018 8:00 pm  16-Jul.-2018 8:13 pm
3543  Feeding Left Breast 17-Jul.-2018 12:01 am 17-Jul.-2018 12:15 am
3548  Feeding Left Breast 17-Jul.-2018 10:44 am 17-Jul.-2018 10:54 am
3555  Feeding Left Breast  17-Jul.-2018 3:36 pm  17-Jul.-2018 3:50 pm
3559  Feeding Left Breast  17-Jul.-2018 7:24 pm  17-Jul.-2018 7:37 pm
3568  Feeding Left Breast  18-Jul.-2018 1:08 am  18-Jul.-2018 1:17 am
3574  Feeding Left Breast 18-Jul.-2018 10:42 am 18-Jul.-2018 10:54 am
3580  Feeding Left Breast  18-Jul.-2018 2:42 pm  18-Jul.-2018 2:50 pm
3585  Feeding Left Breast  18-Jul.-2018 6:36 pm  18-Jul.-2018 6:46 pm
3591  Feeding Left Breast 18-Jul.-2018 11:14 pm 18-Jul.-2018 11:27 pm
3597  Feeding Left Breast 19-Jul.-2018 12:10 pm 19-Jul.-2018 12:20 pm
3602  Feeding Left Breast  19-Jul.-2018 4:13 pm  19-Jul.-2018 4:22 pm
3611  Feeding Left Breast 19-Jul.-2018 10:21 pm 19-Jul.-2018 10:32 pm
3616  Feeding Left Breast  20-Jul.-2018 6:21 am  20-Jul.-2018 6:30 am
3623  Feeding Left Breast  20-Jul.-2018 3:01 pm  20-Jul.-2018 3:14 pm
3629  Feeding Left Breast  20-Jul.-2018 7:20 pm  20-Jul.-2018 7:42 pm
3634  Feeding Left Breast 20-Jul.-2018 10:57 pm 20-Jul.-2018 11:14 pm
3640  Feeding Left Breast  21-Jul.-2018 8:53 am  21-Jul.-2018 9:11 am
3645  Feeding Left Breast  21-Jul.-2018 1:35 pm  21-Jul.-2018 1:52 pm
3655  Feeding Left Breast  21-Jul.-2018 8:22 pm  21-Jul.-2018 8:32 pm
3660  Feeding Left Breast  22-Jul.-2018 1:22 am  22-Jul.-2018 1:36 am
3665  Feeding Left Breast 22-Jul.-2018 12:13 pm 22-Jul.-2018 12:27 pm
3670  Feeding Left Breast  22-Jul.-2018 3:59 pm  22-Jul.-2018 4:10 pm
3678  Feeding Left Breast  22-Jul.-2018 8:29 pm  22-Jul.-2018 8:40 pm
3683  Feeding Left Breast 22-Jul.-2018 11:35 pm 22-Jul.-2018 11:44 pm
3688  Feeding Left Breast 23-Jul.-2018 10:43 am 23-Jul.-2018 10:53 am
3694  Feeding Left Breast  23-Jul.-2018 2:23 pm  23-Jul.-2018 2:32 pm
3700  Feeding Left Breast  23-Jul.-2018 6:29 pm  23-Jul.-2018 6:43 pm
3706  Feeding Left Breast 23-Jul.-2018 10:00 pm 23-Jul.-2018 10:19 pm
3710  Feeding Left Breast 24-Jul.-2018 10:37 am 24-Jul.-2018 10:51 am
3717  Feeding Left Breast  24-Jul.-2018 2:42 pm  24-Jul.-2018 2:51 pm
3722  Feeding Left Breast  24-Jul.-2018 6:14 pm  24-Jul.-2018 6:28 pm
3729  Feeding Left Breast 24-Jul.-2018 10:05 pm 24-Jul.-2018 10:20 pm
3730  Feeding Left Breast 24-Jul.-2018 10:25 pm 24-Jul.-2018 10:30 pm
3735  Feeding Left Breast 25-Jul.-2018 11:04 am 25-Jul.-2018 11:13 am
3741  Feeding Left Breast  25-Jul.-2018 3:05 pm  25-Jul.-2018 3:22 pm
3748  Feeding Left Breast  25-Jul.-2018 7:41 pm  25-Jul.-2018 7:57 pm
3752  Feeding Left Breast  26-Jul.-2018 7:29 am  26-Jul.-2018 7:43 am
3760  Feeding Left Breast 26-Jul.-2018 12:21 pm 26-Jul.-2018 12:31 pm
3767  Feeding Left Breast  26-Jul.-2018 5:33 pm  26-Jul.-2018 5:46 pm
3773  Feeding Left Breast  26-Jul.-2018 9:35 pm  26-Jul.-2018 9:55 pm
3778  Feeding Left Breast  27-Jul.-2018 4:27 am  27-Jul.-2018 4:36 am
3783  Feeding Left Breast 27-Jul.-2018 10:56 am 27-Jul.-2018 11:06 am
3791  Feeding Left Breast  27-Jul.-2018 4:41 pm  27-Jul.-2018 4:49 pm
3795  Feeding Left Breast  27-Jul.-2018 7:40 pm  27-Jul.-2018 7:48 pm
3800  Feeding Left Breast 27-Jul.-2018 11:30 pm 27-Jul.-2018 11:43 pm
3804  Feeding Left Breast 28-Jul.-2018 10:39 am 28-Jul.-2018 10:52 am
3811  Feeding Left Breast  28-Jul.-2018 2:23 pm  28-Jul.-2018 2:36 pm
3814  Feeding Left Breast  28-Jul.-2018 4:31 pm  28-Jul.-2018 4:42 pm
3820  Feeding Left Breast  28-Jul.-2018 7:34 pm  28-Jul.-2018 7:43 pm
3825  Feeding Left Breast 28-Jul.-2018 11:10 pm 28-Jul.-2018 11:20 pm
3826  Feeding Left Breast 28-Jul.-2018 11:25 pm 28-Jul.-2018 11:27 pm
3830  Feeding Left Breast 29-Jul.-2018 12:10 pm 29-Jul.-2018 12:21 pm
3835  Feeding Left Breast  29-Jul.-2018 5:36 pm  29-Jul.-2018 5:49 pm
3843  Feeding Left Breast  30-Jul.-2018 1:04 am  30-Jul.-2018 1:13 am
3846  Feeding Left Breast  30-Jul.-2018 8:03 am  30-Jul.-2018 8:17 am
3851  Feeding Left Breast 30-Jul.-2018 12:59 pm  30-Jul.-2018 1:12 pm
3855  Feeding Left Breast  30-Jul.-2018 3:43 pm  30-Jul.-2018 3:53 pm
3860  Feeding Left Breast  30-Jul.-2018 7:15 pm  30-Jul.-2018 7:26 pm
3866  Feeding Left Breast 30-Jul.-2018 10:25 pm 30-Jul.-2018 10:34 pm
3871  Feeding Left Breast 31-Jul.-2018 10:14 am 31-Jul.-2018 10:22 am
3878  Feeding Left Breast  31-Jul.-2018 4:35 pm  31-Jul.-2018 4:48 pm
3887  Feeding Left Breast  31-Jul.-2018 9:45 pm  31-Jul.-2018 9:58 pm
3892  Feeding Left Breast  01-Aug.-2018 9:11 am  01-Aug.-2018 9:25 am
3898  Feeding Left Breast  01-Aug.-2018 1:18 pm  01-Aug.-2018 1:24 pm
3899  Feeding Left Breast  01-Aug.-2018 1:24 pm  01-Aug.-2018 1:30 pm
3905  Feeding Left Breast  01-Aug.-2018 5:32 pm  01-Aug.-2018 5:48 pm
3914  Feeding Left Breast 01-Aug.-2018 10:43 pm 01-Aug.-2018 10:56 pm
3919  Feeding Left Breast  02-Aug.-2018 9:34 am  02-Aug.-2018 9:42 am
3927  Feeding Left Breast  02-Aug.-2018 4:09 pm  02-Aug.-2018 4:18 pm
3935  Feeding Left Breast  02-Aug.-2018 9:39 pm  02-Aug.-2018 9:52 pm
3940  Feeding Left Breast  03-Aug.-2018 8:24 am  03-Aug.-2018 8:34 am
3945  Feeding Left Breast 03-Aug.-2018 12:09 pm 03-Aug.-2018 12:29 pm
3951  Feeding Left Breast  03-Aug.-2018 4:39 pm  03-Aug.-2018 4:55 pm
3959  Feeding Left Breast 03-Aug.-2018 11:43 pm 04-Aug.-2018 12:04 am
3964  Feeding Left Breast  04-Aug.-2018 8:59 am  04-Aug.-2018 9:07 am
3971  Feeding Left Breast  04-Aug.-2018 2:19 pm  04-Aug.-2018 2:26 pm
3978  Feeding Left Breast  04-Aug.-2018 7:18 pm  04-Aug.-2018 7:25 pm
3987  Feeding Left Breast 05-Aug.-2018 12:02 am 05-Aug.-2018 12:13 am
3991  Feeding Left Breast  05-Aug.-2018 9:48 am  05-Aug.-2018 9:56 am
3998  Feeding Left Breast  05-Aug.-2018 3:12 pm  05-Aug.-2018 3:20 pm
3999  Feeding Left Breast  05-Aug.-2018 3:22 pm  05-Aug.-2018 3:23 pm
4006  Feeding Left Breast  05-Aug.-2018 7:28 pm  05-Aug.-2018 7:35 pm
4012  Feeding Left Breast 05-Aug.-2018 11:28 pm 05-Aug.-2018 11:42 pm
4017  Feeding Left Breast 06-Aug.-2018 10:37 am 06-Aug.-2018 10:56 am
4024  Feeding Left Breast  06-Aug.-2018 2:40 pm  06-Aug.-2018 2:55 pm
4034  Feeding Left Breast  06-Aug.-2018 8:35 pm  06-Aug.-2018 8:48 pm
4039  Feeding Left Breast  07-Aug.-2018 7:35 am  07-Aug.-2018 7:44 am
4040  Feeding Left Breast  07-Aug.-2018 8:21 am  07-Aug.-2018 8:32 am
4045  Feeding Left Breast 07-Aug.-2018 12:53 pm  07-Aug.-2018 1:09 pm
4051  Feeding Left Breast  07-Aug.-2018 6:10 pm  07-Aug.-2018 6:26 pm
4056  Feeding Left Breast 07-Aug.-2018 10:36 pm 07-Aug.-2018 10:47 pm
4068  Feeding Left Breast 08-Aug.-2018 12:39 pm 08-Aug.-2018 12:39 pm
4076  Feeding Left Breast  08-Aug.-2018 5:26 pm  08-Aug.-2018 5:35 pm
4082  Feeding Left Breast 08-Aug.-2018 10:56 pm 08-Aug.-2018 11:06 pm
4087  Feeding Left Breast  09-Aug.-2018 9:26 am  09-Aug.-2018 9:39 am
4096  Feeding Left Breast  09-Aug.-2018 3:43 pm  09-Aug.-2018 3:59 pm
4102  Feeding Left Breast  09-Aug.-2018 9:36 pm  09-Aug.-2018 9:57 pm
4105  Feeding Left Breast  10-Aug.-2018 4:45 am  10-Aug.-2018 5:00 am
4111  Feeding Left Breast 10-Aug.-2018 12:06 pm 10-Aug.-2018 12:25 pm
4116  Feeding Left Breast  10-Aug.-2018 3:45 pm  10-Aug.-2018 3:50 pm
4118  Feeding Left Breast  10-Aug.-2018 5:09 pm  10-Aug.-2018 5:28 pm
4125  Feeding Left Breast 10-Aug.-2018 11:28 pm 10-Aug.-2018 11:41 pm
4130  Feeding Left Breast  11-Aug.-2018 6:51 am  11-Aug.-2018 6:56 am
4131  Feeding Left Breast  11-Aug.-2018 7:01 am  11-Aug.-2018 7:11 am
4136  Feeding Left Breast 11-Aug.-2018 11:13 am 11-Aug.-2018 11:17 am
4138  Feeding Left Breast 11-Aug.-2018 12:27 pm 11-Aug.-2018 12:40 pm
4149  Feeding Left Breast  11-Aug.-2018 9:54 pm 11-Aug.-2018 10:03 pm
4152  Feeding Left Breast 12-Aug.-2018 12:53 am 12-Aug.-2018 12:57 am
4154  Feeding Left Breast  12-Aug.-2018 8:18 am  12-Aug.-2018 8:31 am
4162  Feeding Left Breast  12-Aug.-2018 2:55 pm  12-Aug.-2018 2:57 pm
4165  Feeding Left Breast  12-Aug.-2018 5:19 pm  12-Aug.-2018 5:32 pm
4173  Feeding Left Breast 12-Aug.-2018 10:16 pm 12-Aug.-2018 10:21 pm
4175  Feeding Left Breast  13-Aug.-2018 9:41 am  13-Aug.-2018 9:48 am
4182  Feeding Left Breast  13-Aug.-2018 1:17 pm  13-Aug.-2018 1:31 pm
4192  Feeding Left Breast  13-Aug.-2018 6:57 pm  13-Aug.-2018 7:05 pm
4198  Feeding Left Breast 14-Aug.-2018 12:18 am 14-Aug.-2018 12:31 am
4203  Feeding Left Breast  14-Aug.-2018 9:55 am 14-Aug.-2018 10:02 am
4211  Feeding Left Breast  14-Aug.-2018 3:24 pm  14-Aug.-2018 3:35 pm
4216  Feeding Left Breast  14-Aug.-2018 7:56 pm  14-Aug.-2018 8:07 pm
4222  Feeding Left Breast 14-Aug.-2018 10:58 pm 14-Aug.-2018 11:05 pm
4227  Feeding Left Breast  15-Aug.-2018 3:35 am  15-Aug.-2018 3:44 am
4232  Feeding Left Breast 15-Aug.-2018 10:27 am 15-Aug.-2018 10:31 am
4238  Feeding Left Breast  15-Aug.-2018 2:31 pm  15-Aug.-2018 2:41 pm
4242  Feeding Left Breast  15-Aug.-2018 6:17 pm  15-Aug.-2018 6:23 pm
4247  Feeding Left Breast 15-Aug.-2018 11:28 pm 15-Aug.-2018 11:39 pm
4251  Feeding Left Breast 16-Aug.-2018 10:31 am 16-Aug.-2018 10:39 am
4259  Feeding Left Breast  16-Aug.-2018 5:18 pm  16-Aug.-2018 5:35 pm
4274  Feeding Left Breast 16-Aug.-2018 11:12 pm 16-Aug.-2018 11:31 pm
4287  Feeding Left Breast 17-Aug.-2018 11:05 am 17-Aug.-2018 11:19 am
4293  Feeding Left Breast  17-Aug.-2018 1:31 pm  17-Aug.-2018 1:38 pm
4302  Feeding Left Breast  17-Aug.-2018 8:02 pm  17-Aug.-2018 8:10 pm
4307  Feeding Left Breast 17-Aug.-2018 10:55 pm 17-Aug.-2018 11:00 pm
4313  Feeding Left Breast  18-Aug.-2018 2:15 am  18-Aug.-2018 2:24 am
4319  Feeding Left Breast  18-Aug.-2018 9:26 am  18-Aug.-2018 9:39 am
4326  Feeding Left Breast  18-Aug.-2018 2:40 pm  18-Aug.-2018 2:56 pm
4332  Feeding Left Breast  18-Aug.-2018 7:41 pm  18-Aug.-2018 7:46 pm
4340  Feeding Left Breast 18-Aug.-2018 11:10 pm 18-Aug.-2018 11:21 pm
4344  Feeding Left Breast  19-Aug.-2018 9:13 am  19-Aug.-2018 9:21 am
4350  Feeding Left Breast  19-Aug.-2018 2:15 pm  19-Aug.-2018 2:21 pm
4355  Feeding Left Breast  19-Aug.-2018 7:05 pm  19-Aug.-2018 7:10 pm
4360  Feeding Left Breast 19-Aug.-2018 10:19 pm 19-Aug.-2018 10:22 pm
4362  Feeding Left Breast 19-Aug.-2018 10:47 pm 19-Aug.-2018 10:49 pm
4366  Feeding Left Breast  20-Aug.-2018 4:46 am  20-Aug.-2018 4:55 am
4374  Feeding Left Breast 20-Aug.-2018 11:06 am 20-Aug.-2018 11:11 am
4381  Feeding Left Breast  20-Aug.-2018 4:50 pm  20-Aug.-2018 4:56 pm
4389  Feeding Left Breast 20-Aug.-2018 10:02 pm 20-Aug.-2018 10:17 pm
4393  Feeding Left Breast  21-Aug.-2018 1:43 am  21-Aug.-2018 1:52 am
4397  Feeding Left Breast  21-Aug.-2018 9:09 am  21-Aug.-2018 9:19 am
4402  Feeding Left Breast  21-Aug.-2018 1:36 pm  21-Aug.-2018 1:43 pm
```

#### Right breast  
Only time period, no values      

```r
breast_r
```

```
     Activity        Trait                 Start                Finish
4     Feeding Right Breast  18-Feb.-2018 1:25 am  18-Feb.-2018 1:35 am
6     Feeding Right Breast  18-Feb.-2018 3:24 am  18-Feb.-2018 3:45 am
10    Feeding Right Breast 18-Feb.-2018 10:12 am 18-Feb.-2018 10:45 am
13    Feeding Right Breast  18-Feb.-2018 3:23 pm  18-Feb.-2018 3:56 pm
17    Feeding Right Breast  18-Feb.-2018 6:40 pm  18-Feb.-2018 6:52 pm
19    Feeding Right Breast  18-Feb.-2018 7:02 pm  18-Feb.-2018 7:30 pm
21    Feeding Right Breast 18-Feb.-2018 10:00 pm 18-Feb.-2018 10:08 pm
25    Feeding Right Breast 18-Feb.-2018 11:37 pm 18-Feb.-2018 11:48 pm
26    Feeding Right Breast 19-Feb.-2018 12:02 am 19-Feb.-2018 12:23 am
30    Feeding Right Breast  19-Feb.-2018 3:54 am  19-Feb.-2018 4:19 am
31    Feeding Right Breast  19-Feb.-2018 4:44 am  19-Feb.-2018 6:00 am
33    Feeding Right Breast  19-Feb.-2018 6:14 am  19-Feb.-2018 6:34 am
35    Feeding Right Breast 19-Feb.-2018 10:22 am 19-Feb.-2018 10:23 am
38    Feeding Right Breast  19-Feb.-2018 5:00 pm  19-Feb.-2018 5:20 pm
41    Feeding Right Breast  19-Feb.-2018 8:30 pm  19-Feb.-2018 8:45 pm
43    Feeding Right Breast 19-Feb.-2018 11:23 pm 19-Feb.-2018 11:36 pm
49    Feeding Right Breast  20-Feb.-2018 2:56 am  20-Feb.-2018 3:02 am
52    Feeding Right Breast  20-Feb.-2018 3:57 am  20-Feb.-2018 4:02 am
54    Feeding Right Breast  20-Feb.-2018 5:44 am  20-Feb.-2018 6:07 am
57    Feeding Right Breast  20-Feb.-2018 8:45 am  20-Feb.-2018 8:51 am
58    Feeding Right Breast  20-Feb.-2018 9:40 am  20-Feb.-2018 9:50 am
62    Feeding Right Breast  20-Feb.-2018 4:22 pm  20-Feb.-2018 4:44 pm
65    Feeding Right Breast  20-Feb.-2018 5:32 pm  20-Feb.-2018 5:36 pm
69    Feeding Right Breast  20-Feb.-2018 7:29 pm  20-Feb.-2018 7:52 pm
73    Feeding Right Breast 20-Feb.-2018 11:42 pm 20-Feb.-2018 11:49 pm
74    Feeding Right Breast 20-Feb.-2018 11:51 pm 21-Feb.-2018 12:14 am
80    Feeding Right Breast  21-Feb.-2018 4:03 am  21-Feb.-2018 4:32 am
88    Feeding Right Breast 21-Feb.-2018 11:12 am 21-Feb.-2018 11:22 am
96    Feeding Right Breast  21-Feb.-2018 5:55 pm  21-Feb.-2018 6:05 pm
98    Feeding Right Breast  21-Feb.-2018 6:30 pm  21-Feb.-2018 6:32 pm
102   Feeding Right Breast  21-Feb.-2018 9:23 pm  21-Feb.-2018 9:32 pm
104   Feeding Right Breast  21-Feb.-2018 9:38 pm  21-Feb.-2018 9:50 pm
114   Feeding Right Breast  22-Feb.-2018 2:47 am  22-Feb.-2018 2:54 am
116   Feeding Right Breast  22-Feb.-2018 4:47 am  22-Feb.-2018 5:03 am
122   Feeding Right Breast  22-Feb.-2018 8:37 am  22-Feb.-2018 8:48 am
125   Feeding Right Breast 22-Feb.-2018 12:30 pm 22-Feb.-2018 12:44 pm
126   Feeding Right Breast  22-Feb.-2018 1:42 pm  22-Feb.-2018 1:53 pm
131   Feeding Right Breast  22-Feb.-2018 3:59 pm  22-Feb.-2018 4:10 pm
135   Feeding Right Breast  22-Feb.-2018 8:59 pm  22-Feb.-2018 9:15 pm
139   Feeding Right Breast 22-Feb.-2018 10:49 pm 22-Feb.-2018 10:59 pm
142   Feeding Right Breast  23-Feb.-2018 4:42 am  23-Feb.-2018 4:50 am
146   Feeding Right Breast  23-Feb.-2018 6:34 am  23-Feb.-2018 6:40 am
150   Feeding Right Breast 23-Feb.-2018 10:28 am 23-Feb.-2018 10:41 am
158   Feeding Right Breast  23-Feb.-2018 2:46 pm  23-Feb.-2018 3:01 pm
164   Feeding Right Breast  23-Feb.-2018 6:24 pm  23-Feb.-2018 6:38 pm
168   Feeding Right Breast 23-Feb.-2018 10:49 pm 23-Feb.-2018 11:01 pm
173   Feeding Right Breast  24-Feb.-2018 1:07 am  24-Feb.-2018 1:12 am
178   Feeding Right Breast  24-Feb.-2018 7:27 am  24-Feb.-2018 7:41 am
187   Feeding Right Breast  24-Feb.-2018 2:34 pm  24-Feb.-2018 2:45 pm
191   Feeding Right Breast  24-Feb.-2018 5:46 pm  24-Feb.-2018 6:01 pm
193   Feeding Right Breast  24-Feb.-2018 6:11 pm  24-Feb.-2018 6:20 pm
200   Feeding Right Breast 24-Feb.-2018 11:55 pm 25-Feb.-2018 12:10 am
206   Feeding Right Breast  25-Feb.-2018 2:27 am  25-Feb.-2018 2:33 am
211   Feeding Right Breast  25-Feb.-2018 4:56 am  25-Feb.-2018 5:08 am
216   Feeding Right Breast 25-Feb.-2018 10:47 am 25-Feb.-2018 11:03 am
221   Feeding Right Breast  25-Feb.-2018 1:47 pm  25-Feb.-2018 2:00 pm
225   Feeding Right Breast  25-Feb.-2018 4:29 pm  25-Feb.-2018 4:37 pm
226   Feeding Right Breast  25-Feb.-2018 4:48 pm  25-Feb.-2018 4:54 pm
232   Feeding Right Breast  25-Feb.-2018 7:43 pm  25-Feb.-2018 7:54 pm
234   Feeding Right Breast  25-Feb.-2018 8:16 pm  25-Feb.-2018 8:27 pm
239   Feeding Right Breast 25-Feb.-2018 10:51 pm 25-Feb.-2018 10:59 pm
244   Feeding Right Breast  26-Feb.-2018 1:42 am  26-Feb.-2018 1:50 am
247   Feeding Right Breast  26-Feb.-2018 4:36 am  26-Feb.-2018 4:51 am
252   Feeding Right Breast 26-Feb.-2018 10:10 am 26-Feb.-2018 10:20 am
254   Feeding Right Breast 26-Feb.-2018 11:00 am 26-Feb.-2018 11:07 am
259   Feeding Right Breast  26-Feb.-2018 4:24 pm  26-Feb.-2018 4:37 pm
263   Feeding Right Breast  26-Feb.-2018 8:35 pm  26-Feb.-2018 8:45 pm
265   Feeding Right Breast  26-Feb.-2018 8:49 pm  26-Feb.-2018 8:53 pm
271   Feeding Right Breast 26-Feb.-2018 11:08 pm 26-Feb.-2018 11:24 pm
276   Feeding Right Breast  27-Feb.-2018 1:01 am  27-Feb.-2018 1:15 am
282   Feeding Right Breast  27-Feb.-2018 6:42 am  27-Feb.-2018 6:58 am
284   Feeding Right Breast  27-Feb.-2018 7:16 am  27-Feb.-2018 7:19 am
289   Feeding Right Breast 27-Feb.-2018 10:04 am 27-Feb.-2018 10:29 am
294   Feeding Right Breast  27-Feb.-2018 4:08 pm  27-Feb.-2018 4:23 pm
298   Feeding Right Breast  27-Feb.-2018 6:58 pm  27-Feb.-2018 7:19 pm
305   Feeding Right Breast 28-Feb.-2018 12:07 am 28-Feb.-2018 12:20 am
309   Feeding Right Breast  28-Feb.-2018 4:10 am  28-Feb.-2018 4:24 am
317   Feeding Right Breast  28-Feb.-2018 8:58 am  28-Feb.-2018 9:18 am
321   Feeding Right Breast  28-Feb.-2018 1:41 pm  28-Feb.-2018 1:47 pm
326   Feeding Right Breast  28-Feb.-2018 5:28 pm  28-Feb.-2018 5:41 pm
330   Feeding Right Breast  28-Feb.-2018 8:19 pm  28-Feb.-2018 8:32 pm
334   Feeding Right Breast 28-Feb.-2018 11:40 pm 28-Feb.-2018 11:51 pm
338   Feeding Right Breast  01-Mar.-2018 6:08 am  01-Mar.-2018 6:19 am
343   Feeding Right Breast  01-Mar.-2018 8:14 am  01-Mar.-2018 8:28 am
347   Feeding Right Breast 01-Mar.-2018 11:03 am 01-Mar.-2018 11:12 am
351   Feeding Right Breast  01-Mar.-2018 2:30 pm  01-Mar.-2018 2:36 pm
357   Feeding Right Breast  01-Mar.-2018 5:55 pm  01-Mar.-2018 6:08 pm
363   Feeding Right Breast  01-Mar.-2018 8:52 pm  01-Mar.-2018 9:10 pm
366   Feeding Right Breast 01-Mar.-2018 11:50 pm 02-Mar.-2018 12:05 am
372   Feeding Right Breast  02-Mar.-2018 5:02 am  02-Mar.-2018 5:13 am
378   Feeding Right Breast  02-Mar.-2018 9:04 am  02-Mar.-2018 9:17 am
383   Feeding Right Breast 02-Mar.-2018 12:46 pm 02-Mar.-2018 12:56 pm
386   Feeding Right Breast  02-Mar.-2018 4:30 pm  02-Mar.-2018 4:42 pm
391   Feeding Right Breast  02-Mar.-2018 8:05 pm  02-Mar.-2018 8:18 pm
396   Feeding Right Breast 02-Mar.-2018 11:17 pm 02-Mar.-2018 11:30 pm
401   Feeding Right Breast  03-Mar.-2018 1:53 am  03-Mar.-2018 2:05 am
406   Feeding Right Breast  03-Mar.-2018 5:54 am  03-Mar.-2018 6:06 am
409   Feeding Right Breast  03-Mar.-2018 9:32 am  03-Mar.-2018 9:42 am
413   Feeding Right Breast 03-Mar.-2018 12:30 pm 03-Mar.-2018 12:51 pm
418   Feeding Right Breast  03-Mar.-2018 3:20 pm  03-Mar.-2018 3:35 pm
423   Feeding Right Breast  03-Mar.-2018 6:08 pm  03-Mar.-2018 6:20 pm
428   Feeding Right Breast 03-Mar.-2018 10:29 pm 03-Mar.-2018 10:41 pm
432   Feeding Right Breast 04-Mar.-2018 12:55 am  04-Mar.-2018 1:10 am
434   Feeding Right Breast  04-Mar.-2018 6:06 am  04-Mar.-2018 6:15 am
440   Feeding Right Breast 04-Mar.-2018 11:42 am 04-Mar.-2018 12:00 pm
443   Feeding Right Breast  04-Mar.-2018 3:08 pm  04-Mar.-2018 3:19 pm
445   Feeding Right Breast  04-Mar.-2018 4:30 pm  04-Mar.-2018 4:41 pm
453   Feeding Right Breast  04-Mar.-2018 9:42 pm  04-Mar.-2018 9:57 pm
457   Feeding Right Breast 04-Mar.-2018 11:55 pm 05-Mar.-2018 12:16 am
458   Feeding Right Breast 05-Mar.-2018 12:23 am 05-Mar.-2018 12:30 am
465   Feeding Right Breast  05-Mar.-2018 4:30 am  05-Mar.-2018 4:48 am
469   Feeding Right Breast  05-Mar.-2018 8:41 am  05-Mar.-2018 9:05 am
474   Feeding Right Breast  05-Mar.-2018 2:39 pm  05-Mar.-2018 2:51 pm
481   Feeding Right Breast  05-Mar.-2018 7:36 pm  05-Mar.-2018 7:43 pm
484   Feeding Right Breast  05-Mar.-2018 8:48 pm  05-Mar.-2018 9:15 pm
489   Feeding Right Breast 06-Mar.-2018 12:31 am 06-Mar.-2018 12:40 am
490   Feeding Right Breast 06-Mar.-2018 12:48 am  06-Mar.-2018 1:00 am
496   Feeding Right Breast  06-Mar.-2018 6:15 am  06-Mar.-2018 6:33 am
499   Feeding Right Breast  06-Mar.-2018 9:35 am  06-Mar.-2018 9:50 am
504   Feeding Right Breast 06-Mar.-2018 12:27 pm 06-Mar.-2018 12:38 pm
508   Feeding Right Breast  06-Mar.-2018 3:37 pm  06-Mar.-2018 3:43 pm
515   Feeding Right Breast  06-Mar.-2018 9:17 pm  06-Mar.-2018 9:29 pm
520   Feeding Right Breast 06-Mar.-2018 11:52 pm 07-Mar.-2018 12:08 am
524   Feeding Right Breast  07-Mar.-2018 3:25 am  07-Mar.-2018 3:30 am
525   Feeding Right Breast  07-Mar.-2018 3:43 am  07-Mar.-2018 3:50 am
526   Feeding Right Breast  07-Mar.-2018 3:58 am  07-Mar.-2018 4:09 am
528   Feeding Right Breast  07-Mar.-2018 9:42 am  07-Mar.-2018 9:52 am
538   Feeding Right Breast  07-Mar.-2018 3:12 pm  07-Mar.-2018 3:25 pm
543   Feeding Right Breast  07-Mar.-2018 6:41 pm  07-Mar.-2018 6:54 pm
548   Feeding Right Breast  07-Mar.-2018 8:54 pm  07-Mar.-2018 9:04 pm
554   Feeding Right Breast 07-Mar.-2018 11:48 pm 08-Mar.-2018 12:03 am
559   Feeding Right Breast  08-Mar.-2018 4:55 am  08-Mar.-2018 5:18 am
565   Feeding Right Breast  08-Mar.-2018 8:57 am  08-Mar.-2018 9:17 am
571   Feeding Right Breast  08-Mar.-2018 1:45 pm  08-Mar.-2018 1:58 pm
577   Feeding Right Breast  08-Mar.-2018 4:52 pm  08-Mar.-2018 5:07 pm
581   Feeding Right Breast  08-Mar.-2018 7:02 pm  08-Mar.-2018 7:13 pm
585   Feeding Right Breast  08-Mar.-2018 9:23 pm  08-Mar.-2018 9:36 pm
590   Feeding Right Breast 09-Mar.-2018 12:59 am  09-Mar.-2018 1:15 am
594   Feeding Right Breast  09-Mar.-2018 5:01 am  09-Mar.-2018 5:11 am
602   Feeding Right Breast 09-Mar.-2018 10:44 am 09-Mar.-2018 11:00 am
607   Feeding Right Breast  09-Mar.-2018 3:01 pm  09-Mar.-2018 3:15 pm
609   Feeding Right Breast  09-Mar.-2018 3:58 pm  09-Mar.-2018 4:23 pm
611   Feeding Right Breast  09-Mar.-2018 6:07 pm  09-Mar.-2018 6:16 pm
617   Feeding Right Breast  09-Mar.-2018 9:01 pm  09-Mar.-2018 9:11 pm
619   Feeding Right Breast 10-Mar.-2018 12:54 am  10-Mar.-2018 1:20 am
623   Feeding Right Breast  10-Mar.-2018 5:09 am  10-Mar.-2018 5:25 am
625   Feeding Right Breast  10-Mar.-2018 5:58 am  10-Mar.-2018 6:07 am
633   Feeding Right Breast 10-Mar.-2018 12:50 pm  10-Mar.-2018 1:10 pm
638   Feeding Right Breast  10-Mar.-2018 4:18 pm  10-Mar.-2018 4:35 pm
643   Feeding Right Breast  10-Mar.-2018 7:30 pm  10-Mar.-2018 7:40 pm
647   Feeding Right Breast 10-Mar.-2018 11:04 pm 10-Mar.-2018 11:30 pm
650   Feeding Right Breast  11-Mar.-2018 1:00 am  11-Mar.-2018 1:20 am
656   Feeding Right Breast  11-Mar.-2018 4:10 am  11-Mar.-2018 4:20 am
663   Feeding Right Breast  11-Mar.-2018 9:15 am  11-Mar.-2018 9:28 am
669   Feeding Right Breast  11-Mar.-2018 2:32 pm  11-Mar.-2018 2:53 pm
671   Feeding Right Breast  11-Mar.-2018 3:24 pm  11-Mar.-2018 3:39 pm
678   Feeding Right Breast  11-Mar.-2018 7:34 pm  11-Mar.-2018 7:51 pm
683   Feeding Right Breast 11-Mar.-2018 11:02 pm 11-Mar.-2018 11:27 pm
688   Feeding Right Breast  12-Mar.-2018 1:30 am  12-Mar.-2018 1:48 am
693   Feeding Right Breast  12-Mar.-2018 6:50 am  12-Mar.-2018 7:06 am
700   Feeding Right Breast  12-Mar.-2018 1:47 pm  12-Mar.-2018 2:07 pm
706   Feeding Right Breast  12-Mar.-2018 5:35 pm  12-Mar.-2018 5:47 pm
710   Feeding Right Breast  12-Mar.-2018 9:47 pm  12-Mar.-2018 9:59 pm
712   Feeding Right Breast 12-Mar.-2018 10:15 pm 12-Mar.-2018 10:27 pm
717   Feeding Right Breast  13-Mar.-2018 1:46 am  13-Mar.-2018 2:07 am
722   Feeding Right Breast  13-Mar.-2018 9:21 am  13-Mar.-2018 9:40 am
729   Feeding Right Breast  13-Mar.-2018 2:19 pm  13-Mar.-2018 2:38 pm
736   Feeding Right Breast  13-Mar.-2018 6:58 pm  13-Mar.-2018 7:19 pm
739   Feeding Right Breast  13-Mar.-2018 7:45 pm  13-Mar.-2018 7:52 pm
743   Feeding Right Breast 13-Mar.-2018 10:47 pm 13-Mar.-2018 11:03 pm
747   Feeding Right Breast  14-Mar.-2018 5:38 am  14-Mar.-2018 6:00 am
756   Feeding Right Breast 14-Mar.-2018 12:29 pm 14-Mar.-2018 12:44 pm
760   Feeding Right Breast  14-Mar.-2018 4:32 pm  14-Mar.-2018 4:50 pm
766   Feeding Right Breast  14-Mar.-2018 8:43 pm  14-Mar.-2018 9:04 pm
771   Feeding Right Breast  15-Mar.-2018 2:55 am  15-Mar.-2018 3:20 am
777   Feeding Right Breast  15-Mar.-2018 9:09 am  15-Mar.-2018 9:16 am
782   Feeding Right Breast 15-Mar.-2018 12:28 pm 15-Mar.-2018 12:44 pm
788   Feeding Right Breast  15-Mar.-2018 4:44 pm  15-Mar.-2018 5:03 pm
793   Feeding Right Breast  15-Mar.-2018 7:46 pm  15-Mar.-2018 8:00 pm
798   Feeding Right Breast  16-Mar.-2018 2:46 am  16-Mar.-2018 3:04 am
800   Feeding Right Breast  16-Mar.-2018 3:18 am  16-Mar.-2018 3:28 am
807   Feeding Right Breast 16-Mar.-2018 10:37 am 16-Mar.-2018 10:47 am
814   Feeding Right Breast  16-Mar.-2018 3:29 pm  16-Mar.-2018 3:54 pm
821   Feeding Right Breast  16-Mar.-2018 7:50 pm  16-Mar.-2018 8:16 pm
826   Feeding Right Breast 16-Mar.-2018 11:07 pm 16-Mar.-2018 11:11 pm
828   Feeding Right Breast 16-Mar.-2018 11:50 pm 17-Mar.-2018 12:06 am
833   Feeding Right Breast  17-Mar.-2018 2:25 am  17-Mar.-2018 2:38 am
840   Feeding Right Breast 17-Mar.-2018 12:13 pm 17-Mar.-2018 12:30 pm
846   Feeding Right Breast  17-Mar.-2018 3:21 pm  17-Mar.-2018 3:24 pm
850   Feeding Right Breast  17-Mar.-2018 4:43 pm  17-Mar.-2018 5:00 pm
856   Feeding Right Breast  17-Mar.-2018 8:45 pm  17-Mar.-2018 9:01 pm
857   Feeding Right Breast  17-Mar.-2018 9:05 pm  17-Mar.-2018 9:10 pm
862   Feeding Right Breast 17-Mar.-2018 11:23 pm 17-Mar.-2018 11:41 pm
864   Feeding Right Breast  18-Mar.-2018 1:07 am  18-Mar.-2018 1:41 am
866   Feeding Right Breast  18-Mar.-2018 1:59 am  18-Mar.-2018 2:09 am
872   Feeding Right Breast 18-Mar.-2018 10:26 am 18-Mar.-2018 10:42 am
876   Feeding Right Breast  18-Mar.-2018 1:05 pm  18-Mar.-2018 1:21 pm
881   Feeding Right Breast  18-Mar.-2018 5:16 pm  18-Mar.-2018 5:34 pm
888   Feeding Right Breast 18-Mar.-2018 10:02 pm 18-Mar.-2018 10:21 pm
893   Feeding Right Breast  19-Mar.-2018 3:57 am  19-Mar.-2018 4:15 am
895   Feeding Right Breast  19-Mar.-2018 4:40 am  19-Mar.-2018 4:53 am
902   Feeding Right Breast 19-Mar.-2018 12:27 pm 19-Mar.-2018 12:42 pm
908   Feeding Right Breast  19-Mar.-2018 5:00 pm  19-Mar.-2018 5:20 pm
913   Feeding Right Breast  19-Mar.-2018 9:44 pm 19-Mar.-2018 10:03 pm
920   Feeding Right Breast 20-Mar.-2018 12:58 am  20-Mar.-2018 1:05 am
922   Feeding Right Breast  20-Mar.-2018 1:11 am  20-Mar.-2018 1:18 am
926   Feeding Right Breast  20-Mar.-2018 7:13 am  20-Mar.-2018 7:31 am
932   Feeding Right Breast 20-Mar.-2018 11:04 am 20-Mar.-2018 11:07 am
934   Feeding Right Breast  20-Mar.-2018 1:18 pm  20-Mar.-2018 1:34 pm
939   Feeding Right Breast  20-Mar.-2018 3:55 pm  20-Mar.-2018 4:06 pm
945   Feeding Right Breast  20-Mar.-2018 6:26 pm  20-Mar.-2018 6:38 pm
948   Feeding Right Breast  20-Mar.-2018 7:52 pm  20-Mar.-2018 7:59 pm
954   Feeding Right Breast  21-Mar.-2018 3:11 am  21-Mar.-2018 3:25 am
956   Feeding Right Breast  21-Mar.-2018 3:26 am  21-Mar.-2018 3:36 am
964   Feeding Right Breast  21-Mar.-2018 9:59 am 21-Mar.-2018 10:07 am
971   Feeding Right Breast  21-Mar.-2018 1:53 pm  21-Mar.-2018 2:09 pm
975   Feeding Right Breast  21-Mar.-2018 4:44 pm  21-Mar.-2018 4:54 pm
981   Feeding Right Breast  21-Mar.-2018 7:32 pm  21-Mar.-2018 7:45 pm
987   Feeding Right Breast 21-Mar.-2018 10:16 pm 21-Mar.-2018 10:28 pm
988   Feeding Right Breast 21-Mar.-2018 10:31 pm 21-Mar.-2018 10:40 pm
992   Feeding Right Breast 22-Mar.-2018 12:53 am  22-Mar.-2018 1:04 am
994   Feeding Right Breast  22-Mar.-2018 1:43 am  22-Mar.-2018 1:53 am
998   Feeding Right Breast  22-Mar.-2018 8:31 am  22-Mar.-2018 8:37 am
1002  Feeding Right Breast  22-Mar.-2018 1:03 pm  22-Mar.-2018 1:08 pm
1006  Feeding Right Breast  22-Mar.-2018 3:19 pm  22-Mar.-2018 3:26 pm
1012  Feeding Right Breast  22-Mar.-2018 8:05 pm  22-Mar.-2018 8:19 pm
1019  Feeding Right Breast 22-Mar.-2018 11:44 pm 22-Mar.-2018 11:50 pm
1026  Feeding Right Breast  23-Mar.-2018 6:12 am  23-Mar.-2018 6:29 am
1031  Feeding Right Breast 23-Mar.-2018 11:23 am 23-Mar.-2018 11:29 am
1036  Feeding Right Breast  23-Mar.-2018 2:40 pm  23-Mar.-2018 3:00 pm
1041  Feeding Right Breast  23-Mar.-2018 7:06 pm  23-Mar.-2018 7:28 pm
1052  Feeding Right Breast 23-Mar.-2018 11:36 pm 23-Mar.-2018 11:57 pm
1057  Feeding Right Breast  24-Mar.-2018 5:28 am  24-Mar.-2018 5:42 am
1064  Feeding Right Breast 24-Mar.-2018 11:32 am 24-Mar.-2018 11:39 am
1069  Feeding Right Breast  24-Mar.-2018 2:02 pm  24-Mar.-2018 2:12 pm
1075  Feeding Right Breast  24-Mar.-2018 5:30 pm  24-Mar.-2018 5:51 pm
1080  Feeding Right Breast  24-Mar.-2018 8:39 pm  24-Mar.-2018 8:51 pm
1087  Feeding Right Breast 24-Mar.-2018 10:53 pm 24-Mar.-2018 11:04 pm
1091  Feeding Right Breast  25-Mar.-2018 3:20 am  25-Mar.-2018 3:39 am
1096  Feeding Right Breast  25-Mar.-2018 7:54 am  25-Mar.-2018 8:03 am
1101  Feeding Right Breast  25-Mar.-2018 1:09 pm  25-Mar.-2018 1:25 pm
1106  Feeding Right Breast  25-Mar.-2018 4:47 pm  25-Mar.-2018 4:54 pm
1110  Feeding Right Breast  25-Mar.-2018 6:32 pm  25-Mar.-2018 6:36 pm
1113  Feeding Right Breast  25-Mar.-2018 9:14 pm  25-Mar.-2018 9:30 pm
1119  Feeding Right Breast 26-Mar.-2018 12:00 am 26-Mar.-2018 12:22 am
1125  Feeding Right Breast  26-Mar.-2018 8:48 am  26-Mar.-2018 8:56 am
1127  Feeding Right Breast  26-Mar.-2018 9:06 am  26-Mar.-2018 9:14 am
1133  Feeding Right Breast  26-Mar.-2018 1:29 pm  26-Mar.-2018 1:59 pm
1137  Feeding Right Breast  26-Mar.-2018 4:49 pm  26-Mar.-2018 4:58 pm
1143  Feeding Right Breast  26-Mar.-2018 8:46 pm  26-Mar.-2018 9:01 pm
1147  Feeding Right Breast 26-Mar.-2018 11:06 pm 26-Mar.-2018 11:22 pm
1152  Feeding Right Breast  27-Mar.-2018 1:38 am  27-Mar.-2018 2:00 am
1157  Feeding Right Breast  27-Mar.-2018 8:17 am  27-Mar.-2018 8:24 am
1162  Feeding Right Breast  27-Mar.-2018 1:06 pm  27-Mar.-2018 1:15 pm
1168  Feeding Right Breast  27-Mar.-2018 3:51 pm  27-Mar.-2018 4:03 pm
1174  Feeding Right Breast  27-Mar.-2018 7:06 pm  27-Mar.-2018 7:15 pm
1179  Feeding Right Breast  27-Mar.-2018 9:39 pm  27-Mar.-2018 9:52 pm
1184  Feeding Right Breast  28-Mar.-2018 5:47 am  28-Mar.-2018 6:06 am
1192  Feeding Right Breast  28-Mar.-2018 1:24 pm  28-Mar.-2018 1:44 pm
1198  Feeding Right Breast  28-Mar.-2018 7:26 pm  28-Mar.-2018 7:42 pm
1206  Feeding Right Breast 28-Mar.-2018 11:36 pm 28-Mar.-2018 11:53 pm
1213  Feeding Right Breast 29-Mar.-2018 11:21 am 29-Mar.-2018 11:37 am
1223  Feeding Right Breast  29-Mar.-2018 4:40 pm  29-Mar.-2018 5:01 pm
1228  Feeding Right Breast  29-Mar.-2018 8:15 pm  29-Mar.-2018 8:33 pm
1234  Feeding Right Breast 29-Mar.-2018 11:16 pm 29-Mar.-2018 11:35 pm
1238  Feeding Right Breast  30-Mar.-2018 6:32 am  30-Mar.-2018 6:55 am
1246  Feeding Right Breast  30-Mar.-2018 2:26 pm  30-Mar.-2018 2:47 pm
1252  Feeding Right Breast  30-Mar.-2018 6:26 pm  30-Mar.-2018 6:47 pm
1259  Feeding Right Breast 30-Mar.-2018 10:24 pm 30-Mar.-2018 10:41 pm
1262  Feeding Right Breast 31-Mar.-2018 12:36 am 31-Mar.-2018 12:47 am
1268  Feeding Right Breast  31-Mar.-2018 7:05 am  31-Mar.-2018 7:22 am
1272  Feeding Right Breast 31-Mar.-2018 11:12 am 31-Mar.-2018 11:25 am
1278  Feeding Right Breast  31-Mar.-2018 4:12 pm  31-Mar.-2018 4:31 pm
1285  Feeding Right Breast  31-Mar.-2018 8:35 pm  31-Mar.-2018 8:43 pm
1290  Feeding Right Breast 31-Mar.-2018 11:19 pm 31-Mar.-2018 11:39 pm
1298  Feeding Right Breast  01-Apr.-2018 6:21 am  01-Apr.-2018 6:32 am
1305  Feeding Right Breast 01-Apr.-2018 11:25 am 01-Apr.-2018 11:35 am
1311  Feeding Right Breast  01-Apr.-2018 3:04 pm  01-Apr.-2018 3:16 pm
1320  Feeding Right Breast  01-Apr.-2018 7:42 pm  01-Apr.-2018 8:06 pm
1325  Feeding Right Breast 01-Apr.-2018 11:31 pm 01-Apr.-2018 11:44 pm
1332  Feeding Right Breast  02-Apr.-2018 7:46 am  02-Apr.-2018 7:59 am
1339  Feeding Right Breast  02-Apr.-2018 2:14 pm  02-Apr.-2018 2:29 pm
1346  Feeding Right Breast  02-Apr.-2018 5:59 pm  02-Apr.-2018 6:14 pm
1350  Feeding Right Breast  02-Apr.-2018 9:35 pm  02-Apr.-2018 9:43 pm
1353  Feeding Right Breast 02-Apr.-2018 11:30 pm 02-Apr.-2018 11:38 pm
1359  Feeding Right Breast  03-Apr.-2018 4:10 am  03-Apr.-2018 4:21 am
1365  Feeding Right Breast 03-Apr.-2018 11:02 am 03-Apr.-2018 11:17 am
1372  Feeding Right Breast  03-Apr.-2018 3:34 pm  03-Apr.-2018 3:50 pm
1379  Feeding Right Breast  03-Apr.-2018 8:18 pm  03-Apr.-2018 8:33 pm
1385  Feeding Right Breast 04-Apr.-2018 12:19 am 04-Apr.-2018 12:27 am
1393  Feeding Right Breast 04-Apr.-2018 10:25 am 04-Apr.-2018 10:45 am
1401  Feeding Right Breast  04-Apr.-2018 2:10 pm  04-Apr.-2018 2:26 pm
1402  Feeding Right Breast  04-Apr.-2018 2:47 pm  04-Apr.-2018 2:54 pm
1407  Feeding Right Breast  04-Apr.-2018 7:38 pm  04-Apr.-2018 7:52 pm
1411  Feeding Right Breast 04-Apr.-2018 10:09 pm 04-Apr.-2018 10:17 pm
1412  Feeding Right Breast 04-Apr.-2018 10:22 pm 04-Apr.-2018 10:36 pm
1417  Feeding Right Breast  05-Apr.-2018 6:50 am  05-Apr.-2018 6:59 am
1423  Feeding Right Breast 05-Apr.-2018 10:09 am 05-Apr.-2018 10:30 am
1429  Feeding Right Breast  05-Apr.-2018 2:33 pm  05-Apr.-2018 3:00 pm
1435  Feeding Right Breast  05-Apr.-2018 6:44 pm  05-Apr.-2018 7:01 pm
1440  Feeding Right Breast 05-Apr.-2018 10:17 pm 05-Apr.-2018 10:40 pm
1444  Feeding Right Breast  06-Apr.-2018 4:58 am  06-Apr.-2018 5:07 am
1454  Feeding Right Breast 06-Apr.-2018 10:51 am 06-Apr.-2018 11:03 am
1460  Feeding Right Breast  06-Apr.-2018 3:00 pm  06-Apr.-2018 3:12 pm
1466  Feeding Right Breast  06-Apr.-2018 7:12 pm  06-Apr.-2018 7:21 pm
1470  Feeding Right Breast  06-Apr.-2018 9:10 pm  06-Apr.-2018 9:16 pm
1474  Feeding Right Breast 06-Apr.-2018 11:57 pm 07-Apr.-2018 12:19 am
1483  Feeding Right Breast  07-Apr.-2018 8:43 am  07-Apr.-2018 8:57 am
1491  Feeding Right Breast  07-Apr.-2018 1:24 pm  07-Apr.-2018 1:35 pm
1497  Feeding Right Breast  07-Apr.-2018 5:01 pm  07-Apr.-2018 5:12 pm
1505  Feeding Right Breast  07-Apr.-2018 7:20 pm  07-Apr.-2018 7:30 pm
1511  Feeding Right Breast 07-Apr.-2018 10:14 pm 07-Apr.-2018 10:23 pm
1515  Feeding Right Breast  08-Apr.-2018 7:31 am  08-Apr.-2018 7:40 am
1521  Feeding Right Breast 08-Apr.-2018 11:45 am 08-Apr.-2018 11:56 am
1529  Feeding Right Breast  08-Apr.-2018 4:12 pm  08-Apr.-2018 4:25 pm
1535  Feeding Right Breast  08-Apr.-2018 7:30 pm  08-Apr.-2018 7:52 pm
1541  Feeding Right Breast 08-Apr.-2018 10:55 pm 08-Apr.-2018 11:05 pm
1543  Feeding Right Breast 09-Apr.-2018 12:13 am 09-Apr.-2018 12:19 am
1550  Feeding Right Breast  09-Apr.-2018 9:12 am  09-Apr.-2018 9:25 am
1557  Feeding Right Breast  09-Apr.-2018 1:13 pm  09-Apr.-2018 1:27 pm
1561  Feeding Right Breast  09-Apr.-2018 3:58 pm  09-Apr.-2018 4:10 pm
1566  Feeding Right Breast  09-Apr.-2018 6:49 pm  09-Apr.-2018 6:54 pm
1569  Feeding Right Breast  09-Apr.-2018 8:18 pm  09-Apr.-2018 8:32 pm
1572  Feeding Right Breast 09-Apr.-2018 10:04 pm 09-Apr.-2018 10:17 pm
1581  Feeding Right Breast  10-Apr.-2018 7:10 am  10-Apr.-2018 7:21 am
1584  Feeding Right Breast 10-Apr.-2018 10:35 am 10-Apr.-2018 10:50 am
1592  Feeding Right Breast  10-Apr.-2018 4:32 pm  10-Apr.-2018 4:50 pm
1599  Feeding Right Breast  10-Apr.-2018 8:25 pm  10-Apr.-2018 8:39 pm
1600  Feeding Right Breast  10-Apr.-2018 8:48 pm  10-Apr.-2018 8:57 pm
1605  Feeding Right Breast 10-Apr.-2018 11:27 pm 10-Apr.-2018 11:47 pm
1612  Feeding Right Breast  11-Apr.-2018 6:47 am  11-Apr.-2018 6:56 am
1622  Feeding Right Breast 11-Apr.-2018 12:19 pm 11-Apr.-2018 12:31 pm
1629  Feeding Right Breast  11-Apr.-2018 4:33 pm  11-Apr.-2018 4:46 pm
1631  Feeding Right Breast  11-Apr.-2018 4:52 pm  11-Apr.-2018 4:57 pm
1637  Feeding Right Breast  11-Apr.-2018 8:27 pm  11-Apr.-2018 8:38 pm
1641  Feeding Right Breast 11-Apr.-2018 10:06 pm 11-Apr.-2018 10:21 pm
1642  Feeding Right Breast 11-Apr.-2018 10:27 pm 11-Apr.-2018 10:38 pm
1648  Feeding Right Breast  12-Apr.-2018 7:11 am  12-Apr.-2018 7:15 am
1650  Feeding Right Breast  12-Apr.-2018 7:18 am  12-Apr.-2018 7:25 am
1656  Feeding Right Breast  12-Apr.-2018 1:03 pm  12-Apr.-2018 1:15 pm
1657  Feeding Right Breast  12-Apr.-2018 1:17 pm  12-Apr.-2018 1:20 pm
1664  Feeding Right Breast  12-Apr.-2018 4:23 pm  12-Apr.-2018 4:35 pm
1669  Feeding Right Breast  12-Apr.-2018 7:23 pm  12-Apr.-2018 7:42 pm
1673  Feeding Right Breast 12-Apr.-2018 11:38 pm 12-Apr.-2018 11:52 pm
1674  Feeding Right Breast 12-Apr.-2018 11:54 pm 12-Apr.-2018 11:59 pm
1681  Feeding Right Breast  13-Apr.-2018 7:47 am  13-Apr.-2018 8:00 am
1682  Feeding Right Breast  13-Apr.-2018 8:16 am  13-Apr.-2018 8:21 am
1692  Feeding Right Breast  13-Apr.-2018 2:58 pm  13-Apr.-2018 3:14 pm
1699  Feeding Right Breast  13-Apr.-2018 8:14 pm  13-Apr.-2018 8:39 pm
1703  Feeding Right Breast 13-Apr.-2018 11:05 pm 13-Apr.-2018 11:27 pm
1708  Feeding Right Breast  14-Apr.-2018 6:05 am  14-Apr.-2018 6:14 am
1710  Feeding Right Breast  14-Apr.-2018 6:36 am  14-Apr.-2018 6:42 am
1712  Feeding Right Breast  14-Apr.-2018 7:02 am  14-Apr.-2018 7:12 am
1718  Feeding Right Breast 14-Apr.-2018 12:36 pm 14-Apr.-2018 12:48 pm
1725  Feeding Right Breast  14-Apr.-2018 3:50 pm  14-Apr.-2018 3:57 pm
1734  Feeding Right Breast  14-Apr.-2018 9:01 pm  14-Apr.-2018 9:14 pm
1741  Feeding Right Breast 14-Apr.-2018 11:20 pm 14-Apr.-2018 11:30 pm
1747  Feeding Right Breast  15-Apr.-2018 2:54 am  15-Apr.-2018 3:02 am
1753  Feeding Right Breast  15-Apr.-2018 8:22 am  15-Apr.-2018 8:30 am
1763  Feeding Right Breast  15-Apr.-2018 1:21 pm  15-Apr.-2018 1:34 pm
1764  Feeding Right Breast  15-Apr.-2018 1:37 pm  15-Apr.-2018 1:42 pm
1770  Feeding Right Breast  15-Apr.-2018 6:17 pm  15-Apr.-2018 6:25 pm
1774  Feeding Right Breast  15-Apr.-2018 9:15 pm  15-Apr.-2018 9:30 pm
1779  Feeding Right Breast 15-Apr.-2018 11:30 pm 15-Apr.-2018 11:36 pm
1784  Feeding Right Breast  16-Apr.-2018 5:15 am  16-Apr.-2018 5:28 am
1785  Feeding Right Breast  16-Apr.-2018 5:31 am  16-Apr.-2018 5:34 am
1793  Feeding Right Breast 16-Apr.-2018 10:33 am 16-Apr.-2018 10:44 am
1801  Feeding Right Breast  16-Apr.-2018 2:08 pm  16-Apr.-2018 2:21 pm
1807  Feeding Right Breast  16-Apr.-2018 5:27 pm  16-Apr.-2018 5:37 pm
1812  Feeding Right Breast  16-Apr.-2018 9:38 pm  16-Apr.-2018 9:57 pm
1815  Feeding Right Breast 16-Apr.-2018 11:00 pm 16-Apr.-2018 11:03 pm
1819  Feeding Right Breast  17-Apr.-2018 4:00 am  17-Apr.-2018 4:12 am
1820  Feeding Right Breast  17-Apr.-2018 4:45 am  17-Apr.-2018 4:52 am
1827  Feeding Right Breast  17-Apr.-2018 9:54 am 17-Apr.-2018 10:13 am
1834  Feeding Right Breast  17-Apr.-2018 2:57 pm  17-Apr.-2018 3:21 pm
1840  Feeding Right Breast  17-Apr.-2018 8:14 pm  17-Apr.-2018 8:29 pm
1845  Feeding Right Breast 17-Apr.-2018 11:35 pm 17-Apr.-2018 11:50 pm
1852  Feeding Right Breast  18-Apr.-2018 8:30 am  18-Apr.-2018 8:42 am
1858  Feeding Right Breast  18-Apr.-2018 1:01 pm  18-Apr.-2018 1:12 pm
1865  Feeding Right Breast  18-Apr.-2018 5:49 pm  18-Apr.-2018 6:03 pm
1872  Feeding Right Breast 18-Apr.-2018 10:02 pm 18-Apr.-2018 10:19 pm
1876  Feeding Right Breast  19-Apr.-2018 4:24 am  19-Apr.-2018 4:37 am
1885  Feeding Right Breast 19-Apr.-2018 11:24 am 19-Apr.-2018 11:38 am
1894  Feeding Right Breast  19-Apr.-2018 2:17 pm  19-Apr.-2018 2:24 pm
1901  Feeding Right Breast  19-Apr.-2018 7:36 pm  19-Apr.-2018 7:43 pm
1902  Feeding Right Breast  19-Apr.-2018 7:50 pm  19-Apr.-2018 7:55 pm
1910  Feeding Right Breast 19-Apr.-2018 11:18 pm 19-Apr.-2018 11:28 pm
1916  Feeding Right Breast  20-Apr.-2018 8:01 am  20-Apr.-2018 8:15 am
1917  Feeding Right Breast  20-Apr.-2018 8:21 am  20-Apr.-2018 8:25 am
1924  Feeding Right Breast  20-Apr.-2018 1:39 pm  20-Apr.-2018 1:49 pm
1930  Feeding Right Breast  20-Apr.-2018 5:02 pm  20-Apr.-2018 5:13 pm
1932  Feeding Right Breast  20-Apr.-2018 5:40 pm  20-Apr.-2018 5:48 pm
1937  Feeding Right Breast  20-Apr.-2018 8:36 pm  20-Apr.-2018 8:52 pm
1944  Feeding Right Breast  21-Apr.-2018 5:54 am  21-Apr.-2018 6:01 am
1945  Feeding Right Breast  21-Apr.-2018 6:02 am  21-Apr.-2018 6:13 am
1954  Feeding Right Breast 21-Apr.-2018 12:06 pm 21-Apr.-2018 12:15 pm
1961  Feeding Right Breast  21-Apr.-2018 2:37 pm  21-Apr.-2018 2:43 pm
1964  Feeding Right Breast  21-Apr.-2018 4:27 pm  21-Apr.-2018 4:41 pm
1971  Feeding Right Breast  21-Apr.-2018 8:18 pm  21-Apr.-2018 8:30 pm
1975  Feeding Right Breast 21-Apr.-2018 10:08 pm 21-Apr.-2018 10:19 pm
1979  Feeding Right Breast 22-Apr.-2018 12:19 am 22-Apr.-2018 12:27 am
1980  Feeding Right Breast 22-Apr.-2018 12:28 am 22-Apr.-2018 12:31 am
1985  Feeding Right Breast  22-Apr.-2018 8:07 am  22-Apr.-2018 8:14 am
1987  Feeding Right Breast  22-Apr.-2018 8:18 am  22-Apr.-2018 8:20 am
1993  Feeding Right Breast  22-Apr.-2018 2:06 pm  22-Apr.-2018 2:19 pm
2003  Feeding Right Breast  22-Apr.-2018 6:06 pm  22-Apr.-2018 6:11 pm
2009  Feeding Right Breast  22-Apr.-2018 9:36 pm  22-Apr.-2018 9:54 pm
2014  Feeding Right Breast  23-Apr.-2018 5:01 am  23-Apr.-2018 5:11 am
2022  Feeding Right Breast 23-Apr.-2018 10:31 am 23-Apr.-2018 10:38 am
2028  Feeding Right Breast  23-Apr.-2018 2:58 pm  23-Apr.-2018 3:05 pm
2030  Feeding Right Breast  23-Apr.-2018 3:25 pm  23-Apr.-2018 3:32 pm
2037  Feeding Right Breast  23-Apr.-2018 7:12 pm  23-Apr.-2018 7:24 pm
2042  Feeding Right Breast  23-Apr.-2018 9:41 pm  23-Apr.-2018 9:56 pm
2047  Feeding Right Breast  24-Apr.-2018 3:37 am  24-Apr.-2018 3:49 am
2057  Feeding Right Breast 24-Apr.-2018 12:48 pm 24-Apr.-2018 12:59 pm
2063  Feeding Right Breast  24-Apr.-2018 3:57 pm  24-Apr.-2018 4:07 pm
2076  Feeding Right Breast  24-Apr.-2018 9:07 pm  24-Apr.-2018 9:22 pm
2081  Feeding Right Breast 24-Apr.-2018 10:25 pm 24-Apr.-2018 10:30 pm
2087  Feeding Right Breast  25-Apr.-2018 9:12 am  25-Apr.-2018 9:24 am
2097  Feeding Right Breast  25-Apr.-2018 2:41 pm  25-Apr.-2018 2:50 pm
2102  Feeding Right Breast  25-Apr.-2018 7:15 pm  25-Apr.-2018 7:29 pm
2109  Feeding Right Breast 25-Apr.-2018 11:08 pm 25-Apr.-2018 11:24 pm
2114  Feeding Right Breast  26-Apr.-2018 3:12 am  26-Apr.-2018 3:14 am
2118  Feeding Right Breast  26-Apr.-2018 7:00 am  26-Apr.-2018 7:11 am
2125  Feeding Right Breast 26-Apr.-2018 12:12 pm 26-Apr.-2018 12:19 pm
2135  Feeding Right Breast  26-Apr.-2018 5:48 pm  26-Apr.-2018 5:57 pm
2142  Feeding Right Breast  26-Apr.-2018 9:52 pm 26-Apr.-2018 10:02 pm
2149  Feeding Right Breast  27-Apr.-2018 7:38 am  27-Apr.-2018 7:48 am
2157  Feeding Right Breast 27-Apr.-2018 12:37 pm 27-Apr.-2018 12:46 pm
2161  Feeding Right Breast  27-Apr.-2018 5:30 pm  27-Apr.-2018 5:38 pm
2166  Feeding Right Breast  27-Apr.-2018 8:28 pm  27-Apr.-2018 8:34 pm
2167  Feeding Right Breast  27-Apr.-2018 8:35 pm  27-Apr.-2018 8:46 pm
2172  Feeding Right Breast 28-Apr.-2018 12:11 am 28-Apr.-2018 12:19 am
2178  Feeding Right Breast  28-Apr.-2018 8:51 am  28-Apr.-2018 8:59 am
2180  Feeding Right Breast  28-Apr.-2018 9:03 am  28-Apr.-2018 9:05 am
2188  Feeding Right Breast  28-Apr.-2018 3:49 pm  28-Apr.-2018 4:01 pm
2194  Feeding Right Breast  28-Apr.-2018 8:09 pm  28-Apr.-2018 8:22 pm
2198  Feeding Right Breast  28-Apr.-2018 9:15 pm  28-Apr.-2018 9:20 pm
2202  Feeding Right Breast  29-Apr.-2018 8:08 am  29-Apr.-2018 8:19 am
2208  Feeding Right Breast 29-Apr.-2018 11:53 am 29-Apr.-2018 12:08 pm
2214  Feeding Right Breast  29-Apr.-2018 6:06 pm  29-Apr.-2018 6:14 pm
2220  Feeding Right Breast  29-Apr.-2018 9:27 pm  29-Apr.-2018 9:36 pm
2226  Feeding Right Breast  30-Apr.-2018 5:47 am  30-Apr.-2018 5:59 am
2232  Feeding Right Breast 30-Apr.-2018 10:34 am 30-Apr.-2018 10:43 am
2240  Feeding Right Breast  30-Apr.-2018 3:10 pm  30-Apr.-2018 3:20 pm
2247  Feeding Right Breast  30-Apr.-2018 8:00 pm  30-Apr.-2018 8:16 pm
2250  Feeding Right Breast  30-Apr.-2018 9:33 pm  30-Apr.-2018 9:44 pm
2256  Feeding Right Breast          1/05/18 6:19          1/05/18 6:32
2262  Feeding Right Breast         1/05/18 14:01         1/05/18 14:10
2269  Feeding Right Breast         1/05/18 18:56         1/05/18 19:09
2275  Feeding Right Breast         1/05/18 22:25         1/05/18 22:41
2280  Feeding Right Breast          2/05/18 6:12          2/05/18 6:23
2281  Feeding Right Breast          2/05/18 6:28          2/05/18 6:31
2287  Feeding Right Breast         2/05/18 12:42         2/05/18 12:49
2292  Feeding Right Breast         2/05/18 15:45         2/05/18 15:51
2297  Feeding Right Breast         2/05/18 20:03         2/05/18 20:16
2302  Feeding Right Breast         2/05/18 23:30         2/05/18 23:43
2306  Feeding Right Breast          3/05/18 7:44          3/05/18 7:56
2311  Feeding Right Breast         3/05/18 13:07         3/05/18 13:18
2313  Feeding Right Breast         3/05/18 13:25         3/05/18 13:33
2320  Feeding Right Breast         3/05/18 18:38         3/05/18 18:54
2322  Feeding Right Breast         3/05/18 20:23         3/05/18 20:29
2327  Feeding Right Breast         3/05/18 23:30         3/05/18 23:41
2330  Feeding Right Breast          4/05/18 6:57          4/05/18 7:09
2339  Feeding Right Breast         4/05/18 12:58         4/05/18 13:06
2344  Feeding Right Breast         4/05/18 17:19         4/05/18 17:34
2351  Feeding Right Breast         4/05/18 21:29         4/05/18 21:36
2352  Feeding Right Breast         4/05/18 21:46         4/05/18 21:58
2354  Feeding Right Breast         4/05/18 23:25         4/05/18 23:37
2356  Feeding Right Breast         4/05/18 23:50         4/05/18 23:53
2364  Feeding Right Breast         5/05/18 11:43         5/05/18 11:55
2371  Feeding Right Breast         5/05/18 16:27         5/05/18 16:38
2377  Feeding Right Breast         5/05/18 20:53         5/05/18 21:05
2378  Feeding Right Breast         5/05/18 21:13         5/05/18 21:17
2382  Feeding Right Breast         5/05/18 22:44         5/05/18 22:58
2385  Feeding Right Breast          6/05/18 0:29          6/05/18 0:33
2388  Feeding Right Breast          6/05/18 6:52          6/05/18 7:06
2389  Feeding Right Breast          6/05/18 7:14          6/05/18 7:20
2399  Feeding Right Breast         6/05/18 15:14         6/05/18 15:30
2406  Feeding Right Breast         6/05/18 18:33         6/05/18 18:44
2411  Feeding Right Breast         6/05/18 21:29         6/05/18 21:48
2415  Feeding Right Breast         6/05/18 23:37         6/05/18 23:46
2422  Feeding Right Breast          7/05/18 9:50         7/05/18 10:01
2428  Feeding Right Breast         7/05/18 13:02         7/05/18 13:17
2435  Feeding Right Breast         7/05/18 17:06         7/05/18 17:16
2436  Feeding Right Breast         7/05/18 17:17         7/05/18 17:23
2444  Feeding Right Breast         7/05/18 20:19         7/05/18 20:31
2445  Feeding Right Breast         7/05/18 20:37         7/05/18 20:42
2448  Feeding Right Breast         7/05/18 21:59         7/05/18 22:06
2453  Feeding Right Breast          8/05/18 4:38          8/05/18 4:48
2460  Feeding Right Breast         8/05/18 10:51         8/05/18 10:59
2467  Feeding Right Breast         8/05/18 14:45         8/05/18 14:57
2474  Feeding Right Breast         8/05/18 19:24         8/05/18 19:36
2479  Feeding Right Breast         8/05/18 22:04         8/05/18 22:16
2483  Feeding Right Breast          9/05/18 6:07          9/05/18 6:16
2490  Feeding Right Breast         9/05/18 10:30         9/05/18 10:43
2497  Feeding Right Breast         9/05/18 15:55         9/05/18 16:08
2504  Feeding Right Breast         9/05/18 21:28         9/05/18 21:44
2510  Feeding Right Breast         10/05/18 4:20         10/05/18 4:32
2515  Feeding Right Breast         10/05/18 8:58         10/05/18 9:08
2524  Feeding Right Breast        10/05/18 13:03        10/05/18 13:14
2529  Feeding Right Breast        10/05/18 17:56        10/05/18 18:11
2536  Feeding Right Breast        10/05/18 21:44        10/05/18 21:56
2540  Feeding Right Breast         11/05/18 6:41         11/05/18 6:52
2548  Feeding Right Breast        11/05/18 13:51        11/05/18 13:59
2552  Feeding Right Breast        11/05/18 16:05        11/05/18 16:12
2557  Feeding Right Breast        11/05/18 19:50        11/05/18 20:04
2562  Feeding Right Breast        11/05/18 22:58        11/05/18 23:09
2563  Feeding Right Breast        11/05/18 23:14        11/05/18 23:17
2567  Feeding Right Breast         12/05/18 7:03         12/05/18 7:10
2574  Feeding Right Breast        12/05/18 14:22        12/05/18 14:31
2580  Feeding Right Breast        12/05/18 19:12        12/05/18 19:24
2584  Feeding Right Breast        12/05/18 22:28        12/05/18 22:46
2588  Feeding Right Breast         13/05/18 4:38         13/05/18 4:51
2589  Feeding Right Breast         13/05/18 4:54         13/05/18 5:03
2595  Feeding Right Breast        13/05/18 11:20        13/05/18 11:33
2601  Feeding Right Breast        13/05/18 15:55        13/05/18 16:14
2609  Feeding Right Breast        13/05/18 21:08        13/05/18 21:16
2610  Feeding Right Breast        13/05/18 21:18        13/05/18 21:27
2614  Feeding Right Breast        13/05/18 23:55         14/05/18 0:09
2618  Feeding Right Breast         14/05/18 7:35         14/05/18 7:43
2627  Feeding Right Breast        14/05/18 13:35        14/05/18 13:50
2632  Feeding Right Breast        14/05/18 16:57        14/05/18 17:09
2637  Feeding Right Breast        14/05/18 19:02        14/05/18 19:11
2641  Feeding Right Breast        14/05/18 21:41        14/05/18 21:55
2642  Feeding Right Breast        14/05/18 22:16        14/05/18 22:24
2647  Feeding Right Breast         15/05/18 8:50         15/05/18 9:04
2655  Feeding Right Breast        15/05/18 13:16        15/05/18 13:24
2661  Feeding Right Breast        15/05/18 17:20        15/05/18 17:29
2667  Feeding Right Breast        15/05/18 20:24        15/05/18 20:34
2671  Feeding Right Breast        15/05/18 23:03        15/05/18 23:12
2676  Feeding Right Breast         16/05/18 7:35         16/05/18 7:45
2680  Feeding Right Breast        16/05/18 12:45        16/05/18 12:56
2685  Feeding Right Breast        16/05/18 15:59        16/05/18 16:10
2690  Feeding Right Breast        16/05/18 19:35        16/05/18 19:53
2694  Feeding Right Breast        16/05/18 22:35        16/05/18 22:49
2698  Feeding Right Breast         17/05/18 8:01         17/05/18 8:11
2702  Feeding Right Breast        17/05/18 11:34        17/05/18 11:38
2707  Feeding Right Breast        17/05/18 15:05        17/05/18 15:22
2711  Feeding Right Breast        17/05/18 18:19        17/05/18 18:26
2715  Feeding Right Breast        17/05/18 20:46        17/05/18 20:52
2716  Feeding Right Breast        17/05/18 20:56        17/05/18 21:01
2722  Feeding Right Breast        17/05/18 23:40        17/05/18 23:48
2727  Feeding Right Breast         18/05/18 7:17         18/05/18 7:20
2729  Feeding Right Breast        18/05/18 10:11        18/05/18 10:23
2734  Feeding Right Breast        18/05/18 13:43        18/05/18 13:55
2738  Feeding Right Breast        18/05/18 17:23        18/05/18 17:31
2746  Feeding Right Breast        18/05/18 21:58        18/05/18 22:11
2747  Feeding Right Breast        18/05/18 22:23        18/05/18 22:35
2751  Feeding Right Breast         19/05/18 7:14         19/05/18 7:24
2752  Feeding Right Breast         19/05/18 7:27         19/05/18 7:32
2757  Feeding Right Breast        19/05/18 12:25        19/05/18 12:34
2761  Feeding Right Breast        19/05/18 13:40        19/05/18 13:43
2768  Feeding Right Breast        19/05/18 17:17        19/05/18 17:23
2776  Feeding Right Breast        19/05/18 20:44        19/05/18 20:53
2781  Feeding Right Breast         20/05/18 0:17         20/05/18 0:32
2786  Feeding Right Breast        20/05/18 11:09        20/05/18 11:13
2792  Feeding Right Breast        20/05/18 13:40        20/05/18 13:56
2800  Feeding Right Breast        20/05/18 19:51        20/05/18 20:06
2807  Feeding Right Breast         21/05/18 4:51         21/05/18 5:07
2811  Feeding Right Breast        21/05/18 10:01        21/05/18 10:09
2812  Feeding Right Breast        21/05/18 10:12        21/05/18 10:14
2824  Feeding Right Breast        21/05/18 17:29        21/05/18 17:39
2827  Feeding Right Breast        21/05/18 19:50        21/05/18 19:57
2834  Feeding Right Breast        21/05/18 23:32        21/05/18 23:47
2839  Feeding Right Breast         22/05/18 9:10         22/05/18 9:13
2843  Feeding Right Breast        22/05/18 12:27        22/05/18 12:37
2850  Feeding Right Breast        22/05/18 18:27        22/05/18 18:36
2854  Feeding Right Breast        22/05/18 22:46        22/05/18 22:55
2857  Feeding Right Breast         23/05/18 7:06         23/05/18 7:15
2865  Feeding Right Breast        23/05/18 13:28        23/05/18 13:41
2871  Feeding Right Breast        23/05/18 18:27        23/05/18 18:38
2874  Feeding Right Breast        23/05/18 19:46        23/05/18 19:53
2881  Feeding Right Breast        23/05/18 23:55         24/05/18 0:10
2889  Feeding Right Breast        24/05/18 13:48        24/05/18 14:03
2897  Feeding Right Breast        24/05/18 17:03        24/05/18 17:09
2905  Feeding Right Breast        24/05/18 21:14        24/05/18 21:22
2908  Feeding Right Breast        24/05/18 23:48         25/05/18 0:01
2916  Feeding Right Breast        25/05/18 12:43        25/05/18 12:56
2923  Feeding Right Breast        25/05/18 17:13        25/05/18 17:24
2933  Feeding Right Breast        25/05/18 22:25        25/05/18 22:35
2934  Feeding Right Breast        25/05/18 22:37        25/05/18 22:44
2940  Feeding Right Breast        26/05/18 11:44        26/05/18 11:57
2947  Feeding Right Breast        26/05/18 16:28        26/05/18 16:36
2955  Feeding Right Breast        26/05/18 20:14        26/05/18 20:20
2956  Feeding Right Breast        26/05/18 20:21        26/05/18 20:23
2959  Feeding Right Breast        26/05/18 22:12        26/05/18 22:24
2960  Feeding Right Breast        26/05/18 22:50        26/05/18 22:55
2970  Feeding Right Breast        27/05/18 14:07        27/05/18 14:19
2977  Feeding Right Breast        27/05/18 18:36        27/05/18 18:47
2982  Feeding Right Breast        27/05/18 21:39        27/05/18 21:45
2983  Feeding Right Breast        27/05/18 21:52        27/05/18 22:00
2986  Feeding Right Breast        27/05/18 23:16        27/05/18 23:26
2989  Feeding Right Breast         28/05/18 0:17         28/05/18 0:27
2995  Feeding Right Breast        28/05/18 11:21        28/05/18 11:34
3003  Feeding Right Breast        28/05/18 16:15        28/05/18 16:23
3009  Feeding Right Breast        28/05/18 20:17        28/05/18 20:33
3013  Feeding Right Breast        28/05/18 23:25        28/05/18 23:35
3019  Feeding Right Breast        29/05/18 11:13        29/05/18 11:27
3026  Feeding Right Breast        29/05/18 15:45        29/05/18 15:54
3032  Feeding Right Breast        29/05/18 20:42        29/05/18 20:58
3037  Feeding Right Breast        29/05/18 23:49         30/05/18 0:03
3043  Feeding Right Breast        30/05/18 10:30        30/05/18 10:44
3049  Feeding Right Breast        30/05/18 14:27        30/05/18 14:34
3055  Feeding Right Breast        30/05/18 18:37        30/05/18 18:47
3062  Feeding Right Breast        30/05/18 23:26        30/05/18 23:39
3067  Feeding Right Breast        31/05/18 10:31        31/05/18 10:45
3075  Feeding Right Breast        31/05/18 14:35        31/05/18 14:47
3078  Feeding Right Breast  27-Jun.-2018 9:31 am  27-Jun.-2018 9:40 am
3086  Feeding Right Breast  27-Jun.-2018 3:24 pm  27-Jun.-2018 3:33 pm
3090  Feeding Right Breast  27-Jun.-2018 7:28 pm  27-Jun.-2018 7:38 pm
3092  Feeding Right Breast  27-Jun.-2018 8:09 pm  27-Jun.-2018 8:14 pm
3096  Feeding Right Breast 27-Jun.-2018 11:47 pm 28-Jun.-2018 12:01 am
3101  Feeding Right Breast 28-Jun.-2018 10:27 am 28-Jun.-2018 10:40 am
3106  Feeding Right Breast  28-Jun.-2018 3:06 pm  28-Jun.-2018 3:14 pm
3112  Feeding Right Breast  28-Jun.-2018 6:25 pm  28-Jun.-2018 6:33 pm
3117  Feeding Right Breast 28-Jun.-2018 10:27 pm 28-Jun.-2018 10:40 pm
3122  Feeding Right Breast  29-Jun.-2018 8:39 am  29-Jun.-2018 8:53 am
3127  Feeding Right Breast  29-Jun.-2018 1:06 pm  29-Jun.-2018 1:19 pm
3134  Feeding Right Breast  29-Jun.-2018 6:37 pm  29-Jun.-2018 6:46 pm
3140  Feeding Right Breast 29-Jun.-2018 10:15 pm 29-Jun.-2018 10:31 pm
3145  Feeding Right Breast 30-Jun.-2018 11:34 am 30-Jun.-2018 11:43 am
3150  Feeding Right Breast  30-Jun.-2018 3:33 pm  30-Jun.-2018 3:39 pm
3156  Feeding Right Breast  30-Jun.-2018 8:00 pm  30-Jun.-2018 8:07 pm
3162  Feeding Right Breast  01-Jul.-2018 7:44 am  01-Jul.-2018 7:53 am
3166  Feeding Right Breast 01-Jul.-2018 11:40 am 01-Jul.-2018 11:45 am
3172  Feeding Right Breast  01-Jul.-2018 3:52 pm  01-Jul.-2018 4:13 pm
3177  Feeding Right Breast  01-Jul.-2018 8:21 pm  01-Jul.-2018 8:40 pm
3183  Feeding Right Breast  02-Jul.-2018 6:33 am  02-Jul.-2018 6:41 am
3189  Feeding Right Breast 02-Jul.-2018 11:55 am 02-Jul.-2018 12:04 pm
3194  Feeding Right Breast  02-Jul.-2018 2:47 pm  02-Jul.-2018 2:57 pm
3200  Feeding Right Breast  02-Jul.-2018 7:55 pm  02-Jul.-2018 8:03 pm
3204  Feeding Right Breast 02-Jul.-2018 11:04 pm 02-Jul.-2018 11:19 pm
3208  Feeding Right Breast  03-Jul.-2018 9:00 am  03-Jul.-2018 9:13 am
3214  Feeding Right Breast  03-Jul.-2018 1:38 pm  03-Jul.-2018 1:46 pm
3219  Feeding Right Breast  03-Jul.-2018 4:38 pm  03-Jul.-2018 4:43 pm
3222  Feeding Right Breast  03-Jul.-2018 8:11 pm  03-Jul.-2018 8:24 pm
3227  Feeding Right Breast  04-Jul.-2018 1:50 am  04-Jul.-2018 2:03 am
3234  Feeding Right Breast  04-Jul.-2018 9:57 am 04-Jul.-2018 10:07 am
3240  Feeding Right Breast  04-Jul.-2018 2:22 pm  04-Jul.-2018 2:34 pm
3247  Feeding Right Breast  04-Jul.-2018 6:06 pm  04-Jul.-2018 6:16 pm
3253  Feeding Right Breast 04-Jul.-2018 10:23 pm 04-Jul.-2018 10:38 pm
3257  Feeding Right Breast 05-Jul.-2018 10:00 am 05-Jul.-2018 10:04 am
3259  Feeding Right Breast 05-Jul.-2018 11:48 am 05-Jul.-2018 11:56 am
3264  Feeding Right Breast  05-Jul.-2018 2:20 pm  05-Jul.-2018 2:25 pm
3268  Feeding Right Breast  05-Jul.-2018 5:33 pm  05-Jul.-2018 5:40 pm
3275  Feeding Right Breast 05-Jul.-2018 10:51 pm 05-Jul.-2018 11:05 pm
3282  Feeding Right Breast 06-Jul.-2018 10:40 am 06-Jul.-2018 10:46 am
3283  Feeding Right Breast 06-Jul.-2018 11:07 am 06-Jul.-2018 11:15 am
3290  Feeding Right Breast  06-Jul.-2018 3:57 pm  06-Jul.-2018 4:06 pm
3296  Feeding Right Breast  06-Jul.-2018 8:14 pm  06-Jul.-2018 8:26 pm
3300  Feeding Right Breast  07-Jul.-2018 8:40 am  07-Jul.-2018 8:48 am
3305  Feeding Right Breast  07-Jul.-2018 1:13 pm  07-Jul.-2018 1:21 pm
3309  Feeding Right Breast  07-Jul.-2018 4:25 pm  07-Jul.-2018 4:31 pm
3311  Feeding Right Breast  07-Jul.-2018 4:53 pm  07-Jul.-2018 4:57 pm
3317  Feeding Right Breast  07-Jul.-2018 8:06 pm  07-Jul.-2018 8:13 pm
3322  Feeding Right Breast  08-Jul.-2018 2:14 am  08-Jul.-2018 2:21 am
3326  Feeding Right Breast 08-Jul.-2018 10:03 am 08-Jul.-2018 10:12 am
3333  Feeding Right Breast  08-Jul.-2018 2:16 pm  08-Jul.-2018 2:28 pm
3341  Feeding Right Breast  08-Jul.-2018 6:30 pm  08-Jul.-2018 6:44 pm
3345  Feeding Right Breast 08-Jul.-2018 10:42 pm 08-Jul.-2018 10:58 pm
3349  Feeding Right Breast  09-Jul.-2018 9:00 am  09-Jul.-2018 9:10 am
3354  Feeding Right Breast 09-Jul.-2018 12:29 pm 09-Jul.-2018 12:37 pm
3356  Feeding Right Breast  09-Jul.-2018 1:25 pm  09-Jul.-2018 1:28 pm
3364  Feeding Right Breast  09-Jul.-2018 5:53 pm  09-Jul.-2018 6:01 pm
3369  Feeding Right Breast  09-Jul.-2018 8:56 pm  09-Jul.-2018 9:02 pm
3375  Feeding Right Breast  10-Jul.-2018 3:41 am  10-Jul.-2018 3:52 am
3380  Feeding Right Breast 10-Jul.-2018 12:08 pm 10-Jul.-2018 12:15 pm
3386  Feeding Right Breast  10-Jul.-2018 4:24 pm  10-Jul.-2018 4:32 pm
3392  Feeding Right Breast  10-Jul.-2018 7:37 pm  10-Jul.-2018 7:46 pm
3396  Feeding Right Breast 10-Jul.-2018 11:10 pm 10-Jul.-2018 11:17 pm
3397  Feeding Right Breast 10-Jul.-2018 11:25 pm 10-Jul.-2018 11:28 pm
3401  Feeding Right Breast  11-Jul.-2018 8:13 am  11-Jul.-2018 8:23 am
3408  Feeding Right Breast  11-Jul.-2018 2:36 pm  11-Jul.-2018 2:50 pm
3415  Feeding Right Breast  11-Jul.-2018 8:52 pm  11-Jul.-2018 9:05 pm
3419  Feeding Right Breast  12-Jul.-2018 3:08 am  12-Jul.-2018 3:21 am
3425  Feeding Right Breast 12-Jul.-2018 11:36 am 12-Jul.-2018 11:45 am
3430  Feeding Right Breast  12-Jul.-2018 4:56 pm  12-Jul.-2018 5:10 pm
3436  Feeding Right Breast  12-Jul.-2018 9:21 pm  12-Jul.-2018 9:31 pm
3440  Feeding Right Breast  13-Jul.-2018 4:56 am  13-Jul.-2018 5:06 am
3446  Feeding Right Breast 13-Jul.-2018 11:24 am 13-Jul.-2018 11:33 am
3448  Feeding Right Breast 13-Jul.-2018 12:22 pm 13-Jul.-2018 12:35 pm
3458  Feeding Right Breast  13-Jul.-2018 5:10 pm  13-Jul.-2018 5:19 pm
3466  Feeding Right Breast  13-Jul.-2018 9:18 pm  13-Jul.-2018 9:26 pm
3470  Feeding Right Breast  14-Jul.-2018 9:06 am  14-Jul.-2018 9:18 am
3475  Feeding Right Breast 14-Jul.-2018 12:54 pm  14-Jul.-2018 1:03 pm
3481  Feeding Right Breast  14-Jul.-2018 5:19 pm  14-Jul.-2018 5:28 pm
3487  Feeding Right Breast  14-Jul.-2018 8:59 pm  14-Jul.-2018 9:16 pm
3490  Feeding Right Breast 14-Jul.-2018 11:05 pm 14-Jul.-2018 11:14 pm
3495  Feeding Right Breast  15-Jul.-2018 9:35 am  15-Jul.-2018 9:39 am
3498  Feeding Right Breast 15-Jul.-2018 11:50 am 15-Jul.-2018 12:08 pm
3504  Feeding Right Breast  15-Jul.-2018 3:16 pm  15-Jul.-2018 3:21 pm
3511  Feeding Right Breast  15-Jul.-2018 7:28 pm  15-Jul.-2018 7:36 pm
3519  Feeding Right Breast  16-Jul.-2018 6:53 am  16-Jul.-2018 6:53 am
3522  Feeding Right Breast 16-Jul.-2018 11:05 am 16-Jul.-2018 11:10 am
3525  Feeding Right Breast 16-Jul.-2018 12:21 pm 16-Jul.-2018 12:30 pm
3533  Feeding Right Breast  16-Jul.-2018 5:22 pm  16-Jul.-2018 5:36 pm
3540  Feeding Right Breast 16-Jul.-2018 10:15 pm 16-Jul.-2018 10:25 pm
3541  Feeding Right Breast 16-Jul.-2018 10:27 pm 16-Jul.-2018 10:29 pm
3546  Feeding Right Breast  17-Jul.-2018 9:14 am  17-Jul.-2018 9:22 am
3552  Feeding Right Breast  17-Jul.-2018 1:29 pm  17-Jul.-2018 1:43 pm
3557  Feeding Right Breast  17-Jul.-2018 5:54 pm  17-Jul.-2018 6:06 pm
3565  Feeding Right Breast 17-Jul.-2018 10:48 pm 17-Jul.-2018 11:02 pm
3571  Feeding Right Breast  18-Jul.-2018 3:54 am  18-Jul.-2018 4:03 am
3577  Feeding Right Breast 18-Jul.-2018 12:39 pm 18-Jul.-2018 12:58 pm
3582  Feeding Right Breast  18-Jul.-2018 4:23 pm  18-Jul.-2018 4:42 pm
3589  Feeding Right Breast  18-Jul.-2018 9:10 pm  18-Jul.-2018 9:22 pm
3595  Feeding Right Breast 19-Jul.-2018 10:44 am 19-Jul.-2018 10:57 am
3599  Feeding Right Breast  19-Jul.-2018 2:05 pm  19-Jul.-2018 2:12 pm
3608  Feeding Right Breast  19-Jul.-2018 8:19 pm  19-Jul.-2018 8:31 pm
3613  Feeding Right Breast 19-Jul.-2018 11:59 pm 20-Jul.-2018 12:09 am
3620  Feeding Right Breast 20-Jul.-2018 12:11 pm 20-Jul.-2018 12:22 pm
3626  Feeding Right Breast  20-Jul.-2018 5:13 pm  20-Jul.-2018 5:33 pm
3632  Feeding Right Breast  20-Jul.-2018 9:50 pm 20-Jul.-2018 10:03 pm
3637  Feeding Right Breast  21-Jul.-2018 1:10 am  21-Jul.-2018 1:20 am
3643  Feeding Right Breast 21-Jul.-2018 11:40 am 21-Jul.-2018 11:47 am
3649  Feeding Right Breast  21-Jul.-2018 5:01 pm  21-Jul.-2018 5:12 pm
3658  Feeding Right Breast 21-Jul.-2018 11:19 pm 21-Jul.-2018 11:43 pm
3663  Feeding Right Breast 22-Jul.-2018 10:25 am 22-Jul.-2018 10:38 am
3668  Feeding Right Breast  22-Jul.-2018 3:01 pm  22-Jul.-2018 3:09 pm
3673  Feeding Right Breast  22-Jul.-2018 5:53 pm  22-Jul.-2018 6:04 pm
3681  Feeding Right Breast 22-Jul.-2018 10:23 pm 22-Jul.-2018 10:34 pm
3686  Feeding Right Breast  23-Jul.-2018 7:22 am  23-Jul.-2018 7:31 am
3690  Feeding Right Breast 23-Jul.-2018 12:31 pm 23-Jul.-2018 12:45 pm
3696  Feeding Right Breast  23-Jul.-2018 4:17 pm  23-Jul.-2018 4:35 pm
3703  Feeding Right Breast  23-Jul.-2018 8:05 pm  23-Jul.-2018 8:18 pm
3708  Feeding Right Breast  24-Jul.-2018 8:53 am  24-Jul.-2018 9:06 am
3713  Feeding Right Breast  24-Jul.-2018 1:40 pm  24-Jul.-2018 1:47 pm
3714  Feeding Right Breast  24-Jul.-2018 2:00 pm  24-Jul.-2018 2:05 pm
3720  Feeding Right Breast  24-Jul.-2018 4:42 pm  24-Jul.-2018 4:51 pm
3727  Feeding Right Breast  24-Jul.-2018 8:38 pm  24-Jul.-2018 8:48 pm
3732  Feeding Right Breast  25-Jul.-2018 9:19 am  25-Jul.-2018 9:32 am
3738  Feeding Right Breast  25-Jul.-2018 1:35 pm  25-Jul.-2018 1:43 pm
3744  Feeding Right Breast  25-Jul.-2018 5:37 pm  25-Jul.-2018 5:46 pm
3750  Feeding Right Breast 25-Jul.-2018 10:35 pm 25-Jul.-2018 10:51 pm
3756  Feeding Right Breast 26-Jul.-2018 10:01 am 26-Jul.-2018 10:12 am
3764  Feeding Right Breast  26-Jul.-2018 3:46 pm  26-Jul.-2018 3:55 pm
3765  Feeding Right Breast  26-Jul.-2018 3:55 pm  26-Jul.-2018 3:58 pm
3769  Feeding Right Breast  26-Jul.-2018 7:36 pm  26-Jul.-2018 7:44 pm
3775  Feeding Right Breast 26-Jul.-2018 11:29 pm 26-Jul.-2018 11:41 pm
3780  Feeding Right Breast  27-Jul.-2018 9:17 am  27-Jul.-2018 9:27 am
3786  Feeding Right Breast 27-Jul.-2018 12:41 pm 27-Jul.-2018 12:56 pm
3793  Feeding Right Breast  27-Jul.-2018 5:57 pm  27-Jul.-2018 6:04 pm
3798  Feeding Right Breast 27-Jul.-2018 10:03 pm 27-Jul.-2018 10:23 pm
3802  Feeding Right Breast  28-Jul.-2018 8:38 am  28-Jul.-2018 8:49 am
3808  Feeding Right Breast 28-Jul.-2018 12:31 pm 28-Jul.-2018 12:37 pm
3813  Feeding Right Breast  28-Jul.-2018 4:06 pm  28-Jul.-2018 4:17 pm
3818  Feeding Right Breast  28-Jul.-2018 6:35 pm  28-Jul.-2018 6:44 pm
3823  Feeding Right Breast  28-Jul.-2018 9:45 pm 28-Jul.-2018 10:02 pm
3828  Feeding Right Breast 29-Jul.-2018 10:14 am 29-Jul.-2018 10:26 am
3833  Feeding Right Breast  29-Jul.-2018 3:32 pm  29-Jul.-2018 3:41 pm
3840  Feeding Right Breast  29-Jul.-2018 9:20 pm  29-Jul.-2018 9:28 pm
3844  Feeding Right Breast  30-Jul.-2018 2:41 am  30-Jul.-2018 2:56 am
3849  Feeding Right Breast 30-Jul.-2018 10:50 am 30-Jul.-2018 10:58 am
3853  Feeding Right Breast  30-Jul.-2018 2:11 pm  30-Jul.-2018 2:15 pm
3858  Feeding Right Breast  30-Jul.-2018 5:24 pm  30-Jul.-2018 5:29 pm
3864  Feeding Right Breast  30-Jul.-2018 9:37 pm  30-Jul.-2018 9:51 pm
3868  Feeding Right Breast  31-Jul.-2018 8:05 am  31-Jul.-2018 8:16 am
3874  Feeding Right Breast 31-Jul.-2018 12:21 pm 31-Jul.-2018 12:28 pm
3881  Feeding Right Breast  31-Jul.-2018 6:47 pm  31-Jul.-2018 6:58 pm
3890  Feeding Right Breast  01-Aug.-2018 7:43 am  01-Aug.-2018 7:54 am
3895  Feeding Right Breast 01-Aug.-2018 10:58 am 01-Aug.-2018 11:15 am
3902  Feeding Right Breast  01-Aug.-2018 3:28 pm  01-Aug.-2018 3:34 pm
3909  Feeding Right Breast  01-Aug.-2018 8:05 pm  01-Aug.-2018 8:15 pm
3916  Feeding Right Breast  02-Aug.-2018 6:13 am  02-Aug.-2018 6:31 am
3922  Feeding Right Breast 02-Aug.-2018 12:21 pm 02-Aug.-2018 12:31 pm
3930  Feeding Right Breast  02-Aug.-2018 7:10 pm  02-Aug.-2018 7:19 pm
3937  Feeding Right Breast  03-Aug.-2018 5:33 am  03-Aug.-2018 5:43 am
3942  Feeding Right Breast  03-Aug.-2018 9:53 am 03-Aug.-2018 10:03 am
3948  Feeding Right Breast  03-Aug.-2018 2:10 pm  03-Aug.-2018 2:25 pm
3955  Feeding Right Breast  03-Aug.-2018 8:48 pm  03-Aug.-2018 8:55 pm
3963  Feeding Right Breast  04-Aug.-2018 7:53 am  04-Aug.-2018 8:01 am
3967  Feeding Right Breast 04-Aug.-2018 11:05 am 04-Aug.-2018 11:14 am
3969  Feeding Right Breast 04-Aug.-2018 12:04 pm 04-Aug.-2018 12:10 pm
3975  Feeding Right Breast  04-Aug.-2018 5:02 pm  04-Aug.-2018 5:10 pm
3985  Feeding Right Breast 04-Aug.-2018 10:33 pm 04-Aug.-2018 10:43 pm
3989  Feeding Right Breast  05-Aug.-2018 7:56 am  05-Aug.-2018 8:06 am
3994  Feeding Right Breast 05-Aug.-2018 12:17 pm 05-Aug.-2018 12:25 pm
4002  Feeding Right Breast  05-Aug.-2018 5:03 pm  05-Aug.-2018 5:15 pm
4009  Feeding Right Breast  05-Aug.-2018 9:31 pm  05-Aug.-2018 9:40 pm
4014  Feeding Right Breast  06-Aug.-2018 8:25 am  06-Aug.-2018 8:34 am
4020  Feeding Right Breast 06-Aug.-2018 12:42 pm 06-Aug.-2018 12:54 pm
4027  Feeding Right Breast  06-Aug.-2018 5:19 pm  06-Aug.-2018 5:23 pm
4028  Feeding Right Breast  06-Aug.-2018 5:24 pm  06-Aug.-2018 5:26 pm
4029  Feeding Right Breast  06-Aug.-2018 5:29 pm  06-Aug.-2018 5:30 pm
4030  Feeding Right Breast  06-Aug.-2018 6:06 pm  06-Aug.-2018 6:18 pm
4036  Feeding Right Breast 07-Aug.-2018 12:01 am 07-Aug.-2018 12:14 am
4042  Feeding Right Breast 07-Aug.-2018 10:34 am 07-Aug.-2018 10:41 am
4048  Feeding Right Breast  07-Aug.-2018 3:14 pm  07-Aug.-2018 3:21 pm
4054  Feeding Right Breast  07-Aug.-2018 8:26 pm  07-Aug.-2018 8:38 pm
4060  Feeding Right Breast  08-Aug.-2018 7:02 am  08-Aug.-2018 7:15 am
4073  Feeding Right Breast  08-Aug.-2018 3:09 pm  08-Aug.-2018 3:25 pm
4078  Feeding Right Breast  08-Aug.-2018 7:30 pm  08-Aug.-2018 7:45 pm
4085  Feeding Right Breast  09-Aug.-2018 6:36 am  09-Aug.-2018 6:49 am
4092  Feeding Right Breast  09-Aug.-2018 2:14 pm  09-Aug.-2018 2:22 pm
4099  Feeding Right Breast  09-Aug.-2018 6:44 pm  09-Aug.-2018 6:50 pm
4103  Feeding Right Breast 09-Aug.-2018 11:34 pm 09-Aug.-2018 11:43 pm
4107  Feeding Right Breast  10-Aug.-2018 9:30 am  10-Aug.-2018 9:36 am
4113  Feeding Right Breast  10-Aug.-2018 1:28 pm  10-Aug.-2018 1:38 pm
4123  Feeding Right Breast  10-Aug.-2018 9:44 pm  10-Aug.-2018 9:57 pm
4127  Feeding Right Breast  11-Aug.-2018 1:46 am  11-Aug.-2018 1:55 am
4133  Feeding Right Breast  11-Aug.-2018 9:15 am  11-Aug.-2018 9:26 am
4141  Feeding Right Breast  11-Aug.-2018 2:38 pm  11-Aug.-2018 2:41 pm
4143  Feeding Right Breast  11-Aug.-2018 4:52 pm  11-Aug.-2018 5:05 pm
4150  Feeding Right Breast 11-Aug.-2018 11:39 pm 11-Aug.-2018 11:49 pm
4159  Feeding Right Breast 12-Aug.-2018 12:10 pm 12-Aug.-2018 12:27 pm
4172  Feeding Right Breast 12-Aug.-2018 10:04 pm 12-Aug.-2018 10:14 pm
4179  Feeding Right Breast 13-Aug.-2018 11:22 am 13-Aug.-2018 11:36 am
4186  Feeding Right Breast  13-Aug.-2018 2:39 pm  13-Aug.-2018 2:43 pm
4189  Feeding Right Breast  13-Aug.-2018 4:09 pm  13-Aug.-2018 4:18 pm
4195  Feeding Right Breast  13-Aug.-2018 9:32 pm  13-Aug.-2018 9:44 pm
4201  Feeding Right Breast  14-Aug.-2018 7:49 am  14-Aug.-2018 8:03 am
4207  Feeding Right Breast 14-Aug.-2018 12:23 pm 14-Aug.-2018 12:36 pm
4214  Feeding Right Breast  14-Aug.-2018 5:55 pm  14-Aug.-2018 6:05 pm
4221  Feeding Right Breast 14-Aug.-2018 10:40 pm 14-Aug.-2018 10:58 pm
4224  Feeding Right Breast  15-Aug.-2018 1:41 am  15-Aug.-2018 1:55 am
4229  Feeding Right Breast  15-Aug.-2018 8:14 am  15-Aug.-2018 8:25 am
4235  Feeding Right Breast 15-Aug.-2018 12:13 pm 15-Aug.-2018 12:23 pm
4241  Feeding Right Breast  15-Aug.-2018 5:38 pm  15-Aug.-2018 5:45 pm
4245  Feeding Right Breast  15-Aug.-2018 8:56 pm  15-Aug.-2018 9:04 pm
4248  Feeding Right Breast 15-Aug.-2018 11:49 pm 15-Aug.-2018 11:57 pm
4255  Feeding Right Breast  16-Aug.-2018 1:10 pm  16-Aug.-2018 1:16 pm
4256  Feeding Right Breast  16-Aug.-2018 2:17 pm  16-Aug.-2018 2:28 pm
4266  Feeding Right Breast  16-Aug.-2018 8:45 pm  16-Aug.-2018 8:56 pm
4276  Feeding Right Breast  17-Aug.-2018 8:12 am  17-Aug.-2018 8:24 am
4292  Feeding Right Breast  17-Aug.-2018 1:00 pm  17-Aug.-2018 1:10 pm
4297  Feeding Right Breast  17-Aug.-2018 5:29 pm  17-Aug.-2018 5:37 pm
4303  Feeding Right Breast  17-Aug.-2018 9:08 pm  17-Aug.-2018 9:14 pm
4312  Feeding Right Breast  18-Aug.-2018 1:07 am  18-Aug.-2018 1:21 am
4315  Feeding Right Breast  18-Aug.-2018 3:57 am  18-Aug.-2018 4:04 am
4321  Feeding Right Breast 18-Aug.-2018 12:47 pm 18-Aug.-2018 12:57 pm
4329  Feeding Right Breast  18-Aug.-2018 4:31 pm  18-Aug.-2018 4:41 pm
4336  Feeding Right Breast 18-Aug.-2018 10:01 pm 18-Aug.-2018 10:11 pm
4341  Feeding Right Breast 18-Aug.-2018 11:45 pm 18-Aug.-2018 11:51 pm
4347  Feeding Right Breast 19-Aug.-2018 12:04 pm 19-Aug.-2018 12:09 pm
4348  Feeding Right Breast 19-Aug.-2018 12:12 pm 19-Aug.-2018 12:14 pm
4352  Feeding Right Breast  19-Aug.-2018 3:59 pm  19-Aug.-2018 4:11 pm
4356  Feeding Right Breast  19-Aug.-2018 8:10 pm  19-Aug.-2018 8:21 pm
4363  Feeding Right Breast 19-Aug.-2018 11:50 pm 19-Aug.-2018 11:55 pm
4368  Feeding Right Breast  20-Aug.-2018 8:27 am  20-Aug.-2018 8:37 am
4379  Feeding Right Breast  20-Aug.-2018 3:11 pm  20-Aug.-2018 3:16 pm
4383  Feeding Right Breast  20-Aug.-2018 7:36 pm  20-Aug.-2018 7:43 pm
4384  Feeding Right Breast  20-Aug.-2018 7:45 pm  20-Aug.-2018 7:47 pm
4390  Feeding Right Breast 20-Aug.-2018 10:52 pm 20-Aug.-2018 11:01 pm
4396  Feeding Right Breast  21-Aug.-2018 6:01 am  21-Aug.-2018 6:13 am
4399  Feeding Right Breast 21-Aug.-2018 11:05 am 21-Aug.-2018 11:13 am
4405  Feeding Right Breast  21-Aug.-2018 4:10 pm  21-Aug.-2018 4:18 pm
```


### Diaper  
Only time stamp (count)  

```r
unique(diaper$Trait)
```

```
[1] "Pee & Poo" "Poo"       "Pee"      
```

#### Pee  
Only time stamp (count)  

```r
which(pee$Start!=pee$Finish) # are there any time periods > 1?
```

```
integer(0)
```

```r
nrow(pee)
```

```
[1] 1085
```

#### Poo  
Only time stamp (count)  

```r
which(poo$Start!=poo$Finish) # are there any time periods > 1?
```

```
integer(0)
```

```r
nrow(poo)
```

```
[1] 139
```

#### Both    
Only time stamp (count)  

```r
which(both$Start!=both$Finish) # are there any time periods > 1?
```

```
integer(0)
```

```r
nrow(both)
```

```
[1] 279
```

### Leisure  
Only time period, no values  


#### Bath
Only time period, no values  


#### Tummy  
Only time period, no values  


#### Outdoors
Only time period, no values  


#### Play  
Only time period, no values  


### Lab  




