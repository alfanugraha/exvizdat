library(foreign)
library(skimr)
library(dplyr)

## baca data podes
podes2018_1 <- read.dbf("podes2018_desa_part1.dbf")
podes2018_2 <- read.dbf("podes2018_desa_part2.dbf")
podes2018_3 <- read.dbf("podes2018_desa_part3.dbf")
podes2018_4 <- read.dbf("podes2018_desa_part4.dbf")

banjir_bandang <- podes2018_2 %>%
  select(Prov = R101N, 
         AdaBanjirBandang = R601CK2, 
         JmlKejadian15 = R601CK3, 
         JmlKorban15 = R601CK4, 
         JmlKejadian16 = R601CK5, 
         JmlKorban16 = R601CK6, 
         JmlKejadian17 = R601CK7, 
         JmlKorban17 = R601CK8) %>%
  filter(AdaBanjirBandang == 1) 
banjir_bandang
View(banjir_bandang)

banjir_bandang1 <- podes2018_2 %>%
  select(Prov = R101N, 
         AdaBanjirBandang = R601CK2, 
         JmlKejadian15 = R601CK3, 
         JmlKorban15 = R601CK4, 
         JmlKejadian16 = R601CK5, 
         JmlKorban16 = R601CK6, 
         JmlKejadian17 = R601CK7, 
         JmlKorban17 = R601CK8) 
View(banjir_bandang1)
# total kejadian
tk <- banjir_bandang %>% 
  group_by(Prov) %>%
  summarise(total = n())

# all
al <- banjir_bandang %>% 
  group_by(Prov) %>%
  summarise(across(everything(), list(sum)))

# using skim
sk <- banjir_bandang %>% 
  group_by(Prov) %>%
  skim()
View(sk)


#eksplorasi
hist(tk$total,breaks = ,5)
indeks <- (banjir_bandang$Prov==c("BANTEN","JAWA BARAT","JAWA TENGAH","JAWA TIMUR","DI YOGYAKARTA"))
jawa <- banjir_bandang[(banjir_bandang$Prov==c("BANTEN","JAWA BARAT","JAWA TENGAH","JAWA TIMUR","DI YOGYAKARTA")),]
View(jawa)
jawa <- banjir_bandang[1:602,]
View(jawa)
jabar <- banjir_bandang[1:220,]
View(jabar)
jawa1 <- banjir_bandang[1:25269,]
jawa2 <- read.csv("jawa1.csv")
write.csv(jawa,file = "jawa.csv")

jawa$total_kejadian = jawa$JmlKejadian15+jawa$JmlKejadian16+jawa$JmlKejadian17
jawa$total_korban = jawa$JmlKorban15+jawa$JmlKorban16+jawa$JmlKorban17
hist(jawa$total_kejadian)
hist(jawa$total_korban)
boxplot(jawa$total_kejadian)
boxplot(jawa$total_korban)

#pertahun
hist(jawa$JmlKorban15)
hist(jawa$JmlKorban16)
hist(jawa$JmlKorban17)

jabar$total_kejadian = jabar$JmlKejadian15+jabar$JmlKejadian16+jabar$JmlKejadian17
jabar$total_korban = jabar$JmlKorban15+jabar$JmlKorban16+jabar$JmlKorban17
hist(jabar$total_kejadian)
hist(jabar$total_korban)
boxplot(jabar$total_kejadian)
boxplot(jabar$total_korban)

library(ggplot2)
qplot(jawa$total_kejadian)
View(jawa)

abb <- table(jawa1$AdaBanjirBandang)
persentase <- prop.table(abb)*100
pie(abb,text(persentase))
abb

library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
jawa2$Tahun <- as.factor(jawa2$Tahun)
ggplot(jawa2, aes( x = JmlKorban, y = Tahun, fill = ..x..))+
  geom_density_ridges_gradient(scale=1, rel_min_height=0.01)+
  scale_fill_viridis(name = "Banyaknya Korban Banjir Bandang", option = "C") +
  labs(title = "") +
  theme_ipsum() +
  theme(legend.position = "none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size=8))

boxplot(jawa2$JmlKorban ~ jawa2$Tahun,
        #col=c("coral","green","skyblue"),
        ylab="Banyaknya Korban Banjir Bandang",
        xlab = "Tahun")
max(jawa$total_korban)



library(RJSONIO)
nrow <- nrow(banjir_bandang)
counter <- 1
banjir_bandang$lon[counter] <- 0
banjir_bandang$lat[counter] <- 0
while (counter <= nrow){
  CityName <- gsub(' ','%20',banjir_bandang$DesaKel[counter]) #remove space for URLs
  # CountryCode <- banjir_bandang$Country[counter]
  url <- paste(
    "http://nominatim.openstreetmap.org/search?city="
    , CityName
    , "&limit=9&format=json"
    , sep="")
  x <- fromJSON(url)
  if(is.vector(x)){
    banjir_bandang$lon[counter] <- x[[1]]$lon
    banjir_bandang$lat[counter] <- x[[1]]$lat    
  }
  counter <- counter + 1
}