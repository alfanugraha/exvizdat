# https://www.beritasatu.com/news/971021/bencana-alam-akan-picu-kerugian-ekonomi-global-rp-116712-t

library(foreign)
library(skimr)
library(dplyr)

## baca data podes
podes2018_1 <- read.dbf("podes2018_desa_part1.dbf")
podes2018_2 <- read.dbf("podes2018_desa_part2.dbf")
podes2018_3 <- read.dbf("podes2018_desa_part3.dbf")
podes2018_4 <- read.dbf("podes2018_desa_part4.dbf")

View(podes2018_2)
skim(podes2018_2)

banjir_bandang <- podes2018_2 %>%
  select(Prov = R101N, 
         AdaBanjirBandang = R601CK2, 
         JmlKejadian15 = R601CK3, 
         JmlKorban15 = R601CK4, 
         JmlKejadian16 = R601CK5, 
         JmlKorban16 = R601CK6, 
         JmlKejadian17 = R601CK7, 
         JmlKorban17 = R601CK8,
         PeringatanDini = R602A,
         Perlengkapan = R602C,
         RambuEvakuasi = R602D,
         PerawatanPengairan = R602E) %>%
  filter(AdaBanjirBandang == 1) 
banjir_bandang
View(banjir_bandang)

#ganti mitigasi jadi dummy (0,1)
banjir_bandang$PeringatanDini <- ifelse(banjir_bandang$PeringatanDini==1,1,0)
banjir_bandang$Perlengkapan <- ifelse(banjir_bandang$Perlengkapan==5,1,0)
banjir_bandang$RambuEvakuasi <- ifelse(banjir_bandang$RambuEvakuasi==7,1,0)
banjir_bandang$PerawatanPengairan <- ifelse(banjir_bandang$PerawatanPengairan==1,1,0)
banjir_bandang$Mitigasi <- banjir_bandang$PeringatanDini + banjir_bandang$Perlengkapan +
                               banjir_bandang$RambuEvakuasi + banjir_bandang$PerawatanPengairan

# ubah mitigasi ke faktor
banjir_bandang$PeringatanDini <- as.factor(banjir_bandang$PeringatanDini)
banjir_bandang$Perlengkapan <- as.factor(banjir_bandang$Perlengkapan)
banjir_bandang$RambuEvakuasi <- as.factor(banjir_bandang$RambuEvakuasi)
banjir_bandang$PerawatanPengairan <- as.factor(banjir_bandang$PerawatanPengairan)
banjir_bandang$Mitigasi <- as.factor(banjir_bandang$Mitigasi)

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

#eksplorasi univariate
hist(tk$total,breaks = ,5)
indeks <- (banjir_bandang$Prov==c("BANTEN","JAWA BARAT","JAWA TENGAH","JAWA TIMUR","DI YOGYAKARTA"))
jawa <- banjir_bandang[(banjir_bandang$Prov==c("BANTEN","JAWA BARAT","JAWA TENGAH","JAWA TIMUR","DI YOGYAKARTA")),]
View(jawa)

jabar <- banjir_bandang[1:220,]
View(jabar)
jawa1 <- banjir_bandang1[1:25269,]
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

#eksplorasi bivariate
jawa <- banjir_bandang[1:602,]
View(jawa)
jawa$TotalKejadian <- jawa$JmlKejadian15 + jawa$JmlKejadian16 + jawa$JmlKejadian17
jawa$TotalKorban <- jawa$JmlKorban15 + jawa$JmlKorban16 + jawa$JmlKorban17


# alfa here
bb_jawa <- banjir_bandang_j
bb_jawa$TotalKejadian <- bb_jawa$JmlKejadian15 + bb_jawa$JmlKejadian16 + bb_jawa$JmlKejadian17
bb_jawa$TotalKorban <- bb_jawa$JmlKorban15 + bb_jawa$JmlKorban16 + bb_jawa$JmlKorban17

#ganti mitigasi jadi dummy (0,1)
bb_jawa$PeringatanDini <- ifelse(bb_jawa$SisBencanaAlam==1,1,0)
bb_jawa$Perlengkapan <- ifelse(bb_jawa$Safety==5,1,0)
bb_jawa$RambuEvakuasi <- ifelse(bb_jawa$Evakuasi==7,1,0)
bb_jawa$PerawatanPengairan <- ifelse(bb_jawa$MaintainDAS==1,1,0)
bb_jawa$Mitigasi <- bb_jawa$PeringatanDini + bb_jawa$Perlengkapan + bb_jawa$RambuEvakuasi + bb_jawa$PerawatanPengairan

# ubah mitigasi ke faktor
bb_jawa$PeringatanDini <- as.factor(bb_jawa$PeringatanDini)
bb_jawa$Perlengkapan <- as.factor(bb_jawa$Perlengkapan)
bb_jawa$RambuEvakuasi <- as.factor(bb_jawa$RambuEvakuasi)
bb_jawa$PerawatanPengairan <- as.factor(bb_jawa$PerawatanPengairan)
bb_jawa$Mitigasi <- as.factor(bb_jawa$Mitigasi)


boxplot(bb_jawa$TotalKejadian ~ bb_jawa$Mitigasi,
        #col=c("coral","green","skyblue"),
        ylab="Total Kejadian Banjir Bandang",
        xlab = "Banyaknya Mitigasi yang dilakukan")

boxplot(bb_jawa$JmlKorban17 ~ bb_jawa$Mitigasi,
        #col=c("coral","green","skyblue"),
        ylab="Banyaknya Korban Banjir Bandang 2017",
        xlab = "Banyaknya Mitigasi yang dilakukan")

library(ggridges)
library(viridis)
library(hrbrthemes)
ggplot(bb_jawa, aes( x = TotalKorban, y = Mitigasi, fill = ..x..))+
  geom_density_ridges_gradient(scale=1, rel_min_height=0.01)+
  scale_fill_viridis(name = "Banyaknya Korban Banjir Bandang", option = "C") +
  labs(title = "") +
  theme_ipsum() +
  theme(legend.position = "none",
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size=8))

boxplot(bb_jawa$JmlKorban17 ~ bb_jawa$PeringatanDini,
        #col=c("coral","green","skyblue"),
        ylab="Total Kejadian Banjir Bandang",
        xlab = "Perawatan Pengairan")

boxplot(bb_jawa$TotalKorban ~ bb_jawa$RambuEvakuasi,
        #col=c("coral","green","skyblue"),
        ylab="Total Korban Banjir Bandang",
        xlab = "Rambu-rambu dan Jalur Evakuasi")

#ada mitigasi atau tidak
bb_jawa$AdaMitigasi <- ifelse(bb_jawa$Mitigasi==0,0,1)
bb_jawa$AdaMitigasi <- as.factor(bb_jawa$AdaMitigasi)

plot(bb_jawa$TotalKejadian, bb_jawa$TotalKorban, ylab="Total Korban Banjir Bandang",
     xlab = "Banyaknya Kejadian Banjir Bandang", 
     pch = 19, cex = 1.5)
     #col=ifelse(jawa$AdaMitigasi == 0, "coral", "navyblue"))
