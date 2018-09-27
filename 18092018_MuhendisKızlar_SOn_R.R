#2017

#Ilk olarak kitle belirleniyor 
require(readr)
require(dplyr)
hhia2017<-read.csv(file.choose(),sep=";",stringsAsFactors = TRUE)
hhia.2017<-hhia2017%>%subset(YAS>22&YAS<35&OKUL_BITEN_K%in%c("4","5")&ISCEDF13_K%in%c("13","9","10","11","12","14","15")&CINSIYET==2)

nrow(hhia.2017)
# 13 Muhendislik 
# 9 Bioloji ve Life Sciences 
# 10 Fizik Bilimleri 
# 11 Matematik ve Istatistik 
# 12 Bilisim ve Iletisim 
# 13 Mühendislik isleri 
# 14 imalat ve isleme 
# 15 Mimarlik ve Insaat 
nrow(hhia.2017)

#Simdide flagler yaratiliyor 
#Bilim ve mühedislik ile ilgili profesyonel meslek gruplarinda istihdam edilenler ISCO08 = 21
hhia.2017$ISTIHDAM_STEM_GENC_KADIN_F<-ifelse(hhia.2017$REFERANS_YIL==2017 & hhia.2017$YAS>22 & hhia.2017$YAS<35 & hhia.2017$OKUL_BITEN_K%in%c("4","5")&hhia.2017$DURUM%in%c(1)&hhia.2017$CINSIYET==2&hhia.2017$ISCO08_ESAS_K=="21",1,0)
table(hhia.2017$ISTIHDAM_STEM_GENC_KADIN_F)#Dagilim durumu 
#ID kodu üretiyorum 
hhia.2017$ID<-paste(hhia.2017$REFERANS_YIL,hhia.2017$BIRIMNO,hhia.2017$FERTNO,sep="")

#********************************************************************************************************
#*  Simdi Bagimsiz degiskenler üretiliyor.                                                             *
#*********************************************************************************************************
        
        # Hane Halki Büyüklügü 
        #  1   2   3   4   5   6   7   8   9  11 
        # 67 326 644 460 168  80  22  10   3   1
        hhia.2017$HH_SAYI<-hhia.2017$HH_BUYUKLUK

#Üç Büyük SEhir Ikamet 
hhia.2017$BUYUK_SEHIR_TOP3_F<-if_else(hhia.2017$IBBS_2%in%c("TR10","TR31","TR51"),1,0)

#Az Gelismis BÖlge ikamet 
hhia.2017$AZ_GE_TOP_F<-ifelse(hhia.2017$IBBS_2%in%c("TRB1","TRB2","TRA1","TRA2","TRC1","TRC2"),1,0)

#Yas 
hhia.2017$Age<-hhia.2017$YAS_K

#Dogmus oldugun il de mi yasiyorsunuz?

hhia.2017$NO_EVER_GOC_F<-if_else(hhia.2017$BUIL_YASAMA==1,1,0)

#Kaç senedir ikamet ettiginiz ilde yasiyorsunuz 
hhia.2017$BUIL_SURE<-(hhia.2017$REFERANS_YIL-hhia.2017$BUIL_YIL)

#Hiç 12 ay dan daha uzun süre yurtdisinda yasadiniz mi?
hhia.2017$EVER_ABOARD_F<-if_else(hhia.2017$YURTDISI_DURUM==1,1,0)

#Hanehalki Reisi misiniz?
hhia.2017$HH_REIS_F<-if_else(hhia.2017$YAKINLIK==1,1,0)

#Hanehalki Reisinin evladi misiniz? 
hhia.2017$HH_COCUK_F<-if_else(hhia.2017$YAKINLIK==3,1,0)

#YüsekLIsans MEzunu musun? 
hhia.2017$Yks_Ls_F<-ifelse(hhia.2017$OKUL_BITEN_K==5,1,0)

#Yüksek Okul ve Fakulte mi mezunusun?
hhia.2017$Fakul_F<-ifelse(hhia.2017$OKUL_BITEN_K==4,1,0)

#Mühendislik diplomaniz var mi? 
hhia.2017$Muhendis_F<-if_else(hhia.2017$ISCEDF13_K==13,1,0)

#Mimarlik diplomaniz var mi?
hhia.2017$Mimar_F<-if_else(hhia.2017$ISCEDF13_K==15,1,0)

#BIlisim diplomanız var mı?  
hhia.2017$Bilisim<-if_else(hhia.2017$ISCEDF13_K==12,1,0)

#Örgün egitime devam ediyor musunuz?
hhia.2017$EGT_DEVAM_F<-if_else(hhia.2017$EGITIM_DEVAM_K==1,1,0)

#Örgün egitim disinda bir kursa gidiyor musunuz? 
hhia.2017$KURS_F<-ifelse(hhia.2017$KURS==1,1,0)

#MEdeni durum ile ilgili degiskenler 
hhia.2017$EVLI_F<-ifelse(hhia.2017$MEDENI_DURUM==2,1,0)
hhia.2017$HIC_EVLNMDI_F<-ifelse(hhia.2017$MEDENI_DURUM==1,1,0)

#Hayatiniz boyunca bir iste çalistiniz mi? 
hhia.2017$Ever_Worked<- case_when(hhia.2017$DURUM%in%c(2,3) & hhia.2017$GECMIS_IS==1 ~ 1,
                                  hhia.2017$DURUM==1  ~ 1,
                                  TRUE ~ 0)

#Gelir
hhia.2017$GELIR<-hhia.2017$GELIR_GECENAY_K
hhia.2017$GELIR[is.na(hhia.2017$GELIR)]<-0

#Son iş değiştirme tarihinden bugüne kadar geçen süre
hhia.2017$SON_DEG_TAR<-(hhia.2017$REFERANS_YIL-hhia.2017$IS_AYRIL_YIL)
sum(!is.na(hhia.2017$SON_DEG_TAR))

#Geçen sene öğrenci olan flag
hhia.2017$prev_ogr<-if_else(hhia.2017$FAAL_DURUM_GECENYIL==3,1,0)

#Data Kontrol 
hhia.2017<-hhia.2017[c(103:125)]
glimpse(hhia.2017)
Stat_2017<-skim(hhia.2017)
table(hhia.2017$ISTIHDAM_STEM_GENC_KADIN_F)

require(skimr)
skim(hhia.2017)

#2016
#Ilk olarak kitle belirleniyor 

require(readr)
require(dplyr)
#hhia2016<-read.csv(file.choose(),sep=";",stringsAsFactors = TRUE)
hhia.2016<-hhia2016%>%subset(YAS>22&YAS<35&OKUL_BITEN_K%in%c("4","5")&FOET99_K%in%c("8","9","10","11","12","13","14")&CINSIYET==2)
nrow(hhia.2016)
# 13 Muhendislik 
# 9 Bioloji ve Life Sciences 
# 10 Fizik Bilimleri 
# 11 Matematik ve Istatistik 
# 12 Bilisim ve Iletisim 
# 13 Mühendislik isleri 
# 14 imalat ve isleme 
# 15 Mimarlik ve Insaat 
nrow(hhia.2016)

#Simdide flagler yaratiliyor 
#Bilim ve mühedislik ile ilgili profesyonel meslek gruplarinda istihdam edilenler ISCO08 = 21
hhia.2016$ISTIHDAM_STEM_GENC_KADIN_F<-ifelse(hhia.2016$REFERANS_YIL==2016 & hhia.2016$YAS>22 & hhia.2016$YAS<35 & hhia.2016$OKUL_BITEN_K%in%c("4","5")&hhia.2016$DURUM%in%c(1)&hhia.2016$CINSIYET==2&hhia.2016$ISCO08_ESAS_K=="21",1,0)
table(hhia.2016$ISTIHDAM_STEM_GENC_KADIN_F)#Dagilim durumu 
#ID kodu üretiyorum 
hhia.2016$ID<-paste(hhia.2016$REFERANS_YIL,hhia.2016$BIRIMNO,hhia.2016$FERTNO,sep="")

#********************************************************************************************************
#*  Simdi Bagimsiz degiskenler üretiliyor.                                                             *
#*********************************************************************************************************

# Hane Halki Büyüklügü 
#  1   2   3   4   5   6   7   8   9  11 
# 67 326 644 460 168  80  22  10   3   1
hhia.2016$HH_SAYI<-hhia.2016$HH_BUYUKLUK

#Üç Büyük SEhir Ikamet 
hhia.2016$BUYUK_SEHIR_TOP3_F<-if_else(hhia.2016$IBBS_2%in%c("TR10","TR31","TR51"),1,0)

#Az Gelismis BÖlge ikamet 
hhia.2016$AZ_GE_TOP_F<-ifelse(hhia.2016$IBBS_2%in%c("TRB1","TRB2","TRA1","TRA2","TRC1","TRC2"),1,0)

#Yas 
hhia.2016$Age<-hhia.2016$YAS_K

#Dogmus oldugun il de mi yasiyorsunuz?

hhia.2016$NO_EVER_GOC_F<-if_else(hhia.2016$BUIL_YASAMA==1,1,0)

#Kaç senedir ikamet ettiginiz ilde yasiyorsunuz 
hhia.2016$BUIL_SURE<-(hhia.2016$REFERANS_YIL-hhia.2016$BUIL_YIL)

#Hiç 12 ay dan daha uzun süre yurtdisinda yasadiniz mi?
hhia.2016$EVER_ABOARD_F<-if_else(hhia.2016$YURTDISI_DURUM==1,1,0)

#Hanehalki Reisi misiniz?
hhia.2016$HH_REIS_F<-if_else(hhia.2016$YAKINLIK==1,1,0)

#Hanehalki Reisinin evladi misiniz? 
hhia.2016$HH_COCUK_F<-if_else(hhia.2016$YAKINLIK==3,1,0)

#YüsekLIsans MEzunu musun? 
hhia.2016$Yks_Ls_F<-ifelse(hhia.2016$OKUL_BITEN_K==5,1,0)

#Yüksek Okul ve Fakulte mi mezunusun?
hhia.2016$Fakul_F<-ifelse(hhia.2016$OKUL_BITEN_K==4,1,0)

#Mühendislik diplomaniz var mi? 
hhia.2016$Muhendis_F<-if_else(hhia.2016$FOET99_K==12,1,0)

#Mimarlik diplomaniz var mi?
hhia.2016$Mimar_F<-if_else(hhia.2016$FOET99_K==14,1,0)

#BIlisim diplomanız var mı?  
hhia.2016$Bilisim<-if_else(hhia.2016$FOET99_K==11,1,0)

#Örgün egitime devam ediyor musunuz?
hhia.2016$EGT_DEVAM_F<-if_else(hhia.2016$EGITIM_DEVAM_K==1,1,0)

#Örgün egitim disinda bir kursa gidiyor musunuz? 
hhia.2016$KURS_F<-ifelse(hhia.2016$KURS==1,1,0)

#MEdeni durum ile ilgili degiskenler 
hhia.2016$EVLI_F<-ifelse(hhia.2016$MEDENI_DURUM==2,1,0)
hhia.2016$HIC_EVLNMDI_F<-ifelse(hhia.2016$MEDENI_DURUM==1,1,0)

#Hayatiniz boyunca bir iste çalistiniz mi? 
hhia.2016$Ever_Worked<- case_when(hhia.2016$DURUM%in%c(2,3) & hhia.2016$GECMIS_IS==1 ~ 1,
                                  hhia.2016$DURUM==1  ~ 1,
                                  TRUE ~ 0)

#Gelir
hhia.2016$GELIR<-hhia.2016$GELIR_GECENAY_K
hhia.2016$GELIR[is.na(hhia.2016$GELIR)]<-0

#Son iş değiştirme tarihinden bugüne kadar geçen süre
hhia.2016$SON_DEG_TAR<-(hhia.2016$REFERANS_YIL-hhia.2016$IS_AYRIL_YIL)
sum(!is.na(hhia.2016$SON_DEG_TAR))

#Geçen sene öğrenci olan flag
hhia.2016$prev_ogr<-if_else(hhia.2016$FAAL_DURUM_GECENYIL==3,1,0)

#Data Kontrol 
names(hhia.2016)
hhia.2016<-hhia.2016[c(103:125)]
glimpse(hhia.2016)
Stat_2016<-skim(hhia.2016)


#2015
#Ilk olarak kitle belirleniyor 

require(readr)
require(dplyr)
#hhia2015<-read.csv(file.choose(),sep=";",stringsAsFactors = TRUE)
hhia.2015<-hhia2015%>%subset(YAS>22&YAS<35&OKUL_BITEN_K%in%c("4","5")&FOET99_K%in%c("8","9","10","11","12","13","14")&CINSIYET==2)
nrow(hhia.2015)
# 13 Muhendislik 
# 9 Bioloji ve Life Sciences 
# 10 Fizik Bilimleri 
# 11 Matematik ve Istatistik 
# 12 Bilisim ve Iletisim 
# 13 Mühendislik isleri 
# 14 imalat ve isleme 
# 15 Mimarlik ve Insaat 
nrow(hhia.2015)

#Simdide flagler yaratiliyor 
#Bilim ve mühedislik ile ilgili profesyonel meslek gruplarinda istihdam edilenler ISCO08 = 21
hhia.2015$ISTIHDAM_STEM_GENC_KADIN_F<-ifelse(hhia.2015$REFERANS_YIL==2015 & hhia.2015$YAS>22 & hhia.2015$YAS<35 & hhia.2015$OKUL_BITEN_K%in%c("4","5")&hhia.2015$DURUM%in%c(1)&hhia.2015$CINSIYET==2&hhia.2015$ISCO08_ESAS_K=="21",1,0)
table(hhia.2015$ISTIHDAM_STEM_GENC_KADIN_F)#Dagilim durumu 
#ID kodu üretiyorum 
hhia.2015$ID<-paste(hhia.2015$REFERANS_YIL,hhia.2015$BIRIMNO,hhia.2015$FERTNO,sep="")

#********************************************************************************************************
#*  Simdi Bagimsiz degiskenler üretiliyor.                                                             *
#*********************************************************************************************************

# Hane Halki Büyüklügü 
#  1   2   3   4   5   6   7   8   9  11 
# 67 326 644 460 168  80  22  10   3   1
hhia.2015$HH_SAYI<-hhia.2015$HH_BUYUKLUK

#Üç Büyük SEhir Ikamet 
hhia.2015$BUYUK_SEHIR_TOP3_F<-if_else(hhia.2015$IBBS_2%in%c("TR10","TR31","TR51"),1,0)

#Az Gelismis BÖlge ikamet 
hhia.2015$AZ_GE_TOP_F<-ifelse(hhia.2015$IBBS_2%in%c("TRB1","TRB2","TRA1","TRA2","TRC1","TRC2"),1,0)

#Yas 
hhia.2015$Age<-hhia.2015$YAS_K

#Dogmus oldugun il de mi yasiyorsunuz?

hhia.2015$NO_EVER_GOC_F<-if_else(hhia.2015$BUIL_YASAMA==1,1,0)

#Kaç senedir ikamet ettiginiz ilde yasiyorsunuz 
hhia.2015$BUIL_SURE<-(hhia.2015$REFERANS_YIL-hhia.2015$BUIL_YIL)

#Hiç 12 ay dan daha uzun süre yurtdisinda yasadiniz mi?
hhia.2015$EVER_ABOARD_F<-if_else(hhia.2015$YURTDISI_DURUM==1,1,0)

#Hanehalki Reisi misiniz?
hhia.2015$HH_REIS_F<-if_else(hhia.2015$YAKINLIK==1,1,0)

#Hanehalki Reisinin evladi misiniz? 
hhia.2015$HH_COCUK_F<-if_else(hhia.2015$YAKINLIK==3,1,0)

#YüsekLIsans MEzunu musun? 
hhia.2015$Yks_Ls_F<-ifelse(hhia.2015$OKUL_BITEN_K==5,1,0)

#Yüksek Okul ve Fakulte mi mezunusun?
hhia.2015$Fakul_F<-ifelse(hhia.2015$OKUL_BITEN_K==4,1,0)

#Mühendislik diplomaniz var mi? 
hhia.2015$Muhendis_F<-if_else(hhia.2015$FOET99_K==12,1,0)

#Mimarlik diplomaniz var mi?
hhia.2015$Mimar_F<-if_else(hhia.2015$FOET99_K==14,1,0)

#BIlisim diplomanız var mı?  
hhia.2015$Bilisim<-if_else(hhia.2015$FOET99_K==11,1,0)

#Örgün egitime devam ediyor musunuz?
hhia.2015$EGT_DEVAM_F<-if_else(hhia.2015$EGITIM_DEVAM_K==1,1,0)

#Örgün egitim disinda bir kursa gidiyor musunuz? 
hhia.2015$KURS_F<-ifelse(hhia.2015$KURS==1,1,0)

#MEdeni durum ile ilgili degiskenler 
hhia.2015$EVLI_F<-ifelse(hhia.2015$MEDENI_DURUM==2,1,0)
hhia.2015$HIC_EVLNMDI_F<-ifelse(hhia.2015$MEDENI_DURUM==1,1,0)

#Hayatiniz boyunca bir iste çalistiniz mi? 
hhia.2015$Ever_Worked<- case_when(hhia.2015$DURUM%in%c(2,3) & hhia.2015$GECMIS_IS==1 ~ 1,
                                  hhia.2015$DURUM==1  ~ 1,
                                  TRUE ~ 0)

#Gelir
hhia.2015$GELIR<-hhia.2015$GELIR_GECENAY_K
hhia.2015$GELIR[is.na(hhia.2015$GELIR)]<-0

#Son iş değiştirme tarihinden bugüne kadar geçen süre
hhia.2015$SON_DEG_TAR<-(hhia.2015$REFERANS_YIL-hhia.2015$IS_AYRIL_YIL)
sum(!is.na(hhia.2015$SON_DEG_TAR))

#Geçen sene öğrenci olan flag
hhia.2015$prev_ogr<-if_else(hhia.2015$FAAL_DURUM_GECENYIL==3,1,0)

#Data Kontrol 
names(hhia.2015)
hhia.2015<-hhia.2015[c(103:125)]
glimpse(hhia.2015)
Stat_2015<-skim(hhia.2015)

#2014

#2014
#Ilk olarak kitle belirleniyor 

require(readr)
require(dplyr)
#hhia2014<-read.csv(file.choose(),sep=";",stringsAsFactors = TRUE)
hhia.2014<-hhia2014%>%subset(YAS>22&YAS<35&OKUL_BITEN_K%in%c("4","5")&FOET99_K%in%c("8","9","10","11","12","13","14")&CINSIYET==2)
nrow(hhia.2014)
# 13 Muhendislik 
# 9 Bioloji ve Life Sciences 
# 10 Fizik Bilimleri 
# 11 Matematik ve Istatistik 
# 12 Bilisim ve Iletisim 
# 13 Mühendislik isleri 
# 14 imalat ve isleme 
# 15 Mimarlik ve Insaat 
nrow(hhia.2014)

#Simdide flagler yaratiliyor 
#Bilim ve mühedislik ile ilgili profesyonel meslek gruplarinda istihdam edilenler ISCO08 = 21
hhia.2014$ISTIHDAM_STEM_GENC_KADIN_F<-ifelse(hhia.2014$REFERANS_YIL==2014 & hhia.2014$YAS>22 & hhia.2014$YAS<35 & hhia.2014$OKUL_BITEN_K%in%c("4","5")&hhia.2014$DURUM%in%c(1)&hhia.2014$CINSIYET==2&hhia.2014$ISCO08_ESAS_K=="21",1,0)
table(hhia.2014$ISTIHDAM_STEM_GENC_KADIN_F)#Dagilim durumu 
#ID kodu üretiyorum 
hhia.2014$ID<-paste(hhia.2014$REFERANS_YIL,hhia.2014$BIRIMNO,hhia.2014$FERTNO,sep="")

#********************************************************************************************************
#*  Simdi Bagimsiz degiskenler üretiliyor.                                                             *
#*********************************************************************************************************

# Hane Halki Büyüklügü 
#  1   2   3   4   5   6   7   8   9  11 
# 67 326 644 460 168  80  22  10   3   1
hhia.2014$HH_SAYI<-hhia.2014$HH_BUYUKLUK

#Üç Büyük SEhir Ikamet 
hhia.2014$BUYUK_SEHIR_TOP3_F<-if_else(hhia.2014$IBBS_2%in%c("TR10","TR31","TR51"),1,0)

#Az Gelismis BÖlge ikamet 
hhia.2014$AZ_GE_TOP_F<-ifelse(hhia.2014$IBBS_2%in%c("TRB1","TRB2","TRA1","TRA2","TRC1","TRC2"),1,0)

#Yas 
hhia.2014$Age<-hhia.2014$YAS_K

#Dogmus oldugun il de mi yasiyorsunuz?

hhia.2014$NO_EVER_GOC_F<-if_else(hhia.2014$BUIL_YASAMA==1,1,0)

#Kaç senedir ikamet ettiginiz ilde yasiyorsunuz 
hhia.2014$BUIL_SURE<-(hhia.2014$REFERANS_YIL-hhia.2014$BUIL_YIL)

#Hiç 12 ay dan daha uzun süre yurtdisinda yasadiniz mi?
hhia.2014$EVER_ABOARD_F<-if_else(hhia.2014$YURTDISI_DURUM==1,1,0)

#Hanehalki Reisi misiniz?
hhia.2014$HH_REIS_F<-if_else(hhia.2014$YAKINLIK==1,1,0)

#Hanehalki Reisinin evladi misiniz? 
hhia.2014$HH_COCUK_F<-if_else(hhia.2014$YAKINLIK==3,1,0)

#YüsekLIsans MEzunu musun? 
hhia.2014$Yks_Ls_F<-ifelse(hhia.2014$OKUL_BITEN_K==5,1,0)

#Yüksek Okul ve Fakulte mi mezunusun?
hhia.2014$Fakul_F<-ifelse(hhia.2014$OKUL_BITEN_K==4,1,0)

#Mühendislik diplomaniz var mi? 
hhia.2014$Muhendis_F<-if_else(hhia.2014$FOET99_K==12,1,0)

#Mimarlik diplomaniz var mi?
hhia.2014$Mimar_F<-if_else(hhia.2014$FOET99_K==14,1,0)

#BIlisim diplomanız var mı?  
hhia.2014$Bilisim<-if_else(hhia.2014$FOET99_K==11,1,0)

#Örgün egitime devam ediyor musunuz?
hhia.2014$EGT_DEVAM_F<-if_else(hhia.2014$EGITIM_DEVAM_K==1,1,0)

#Örgün egitim disinda bir kursa gidiyor musunuz? 
hhia.2014$KURS_F<-ifelse(hhia.2014$KURS==1,1,0)

#MEdeni durum ile ilgili degiskenler 
hhia.2014$EVLI_F<-ifelse(hhia.2014$MEDENI_DURUM==2,1,0)
hhia.2014$HIC_EVLNMDI_F<-ifelse(hhia.2014$MEDENI_DURUM==1,1,0)

#Hayatiniz boyunca bir iste çalistiniz mi? 
hhia.2014$Ever_Worked<- case_when(hhia.2014$DURUM%in%c(2,3) & hhia.2014$GECMIS_IS==1 ~ 1,
                                  hhia.2014$DURUM==1  ~ 1,
                                  TRUE ~ 0)

#Gelir
hhia.2014$GELIR<-hhia.2014$GELIR_GECENAY_K
hhia.2014$GELIR[is.na(hhia.2014$GELIR)]<-0

#Son iş değiştirme tarihinden bugüne kadar geçen süre
hhia.2014$SON_DEG_TAR<-(hhia.2014$REFERANS_YIL-hhia.2014$IS_AYRIL_YIL)
sum(!is.na(hhia.2014$SON_DEG_TAR))

#Geçen sene öğrenci olan flag
hhia.2014$prev_ogr<-if_else(hhia.2014$FAAL_DURUM_GECENYIL==3,1,0)

#Data Kontrol 
names(hhia.2014)
hhia.2014<-hhia.2014[c(103:125)]
glimpse(hhia.2014)
Stat_2014<-skim(hhia.2014)

###########################################################
###Univariate Kontrol

a1=hhia.2017

cnt=0;r=2
x_d1<-NULL;x_d<-NULL
for(r in 2:ncol(a1)){
        tryCatch({
                
                if(class(a1[,r])=="factor" |class(a1[,r])=="character" ){
                        
                        Data_Type<-class(a1[,r])
                        num_obs<-length(a1[,r])
                        miss<-sum(is.na(a1[,r])) 
                        miss_pc<-100*miss/nrow(a1)
                        mean<-NA
                        median<-NA 
                        sd<-NA
                        qtl<-NA
                        num_unique_obs<-length(unique(na.omit(a1[,r])))
                        cnt2=0
                        for(i in 1:num_unique_obs){
                                if(cnt2==0){
                                        res<-data.frame(table(a1[,r]))[,2][i]
                                        nam<-data.frame(table(a1[,r]))[,1][i]
                                        r_f<-paste(nam,res,sep = ":-")
                                        cnt2=cnt2+1
                                }else{
                                        res<-data.frame(table(a1[,r]))[,2][i]
                                        nam<-data.frame(table(a1[,r]))[,1][i]
                                        r_f_1<-paste(nam,res,sep = ":-")
                                        r_f<-rbind(r_f,r_f_1)            
                                }
                        }
                        
                        factor_unique_level_obs<-paste(unlist(r_f),collapse=";  ")
                        
                }else{
                        
                        Data_Type<-class(a1[,r])
                        num_obs<-length(a1[,r])
                        miss<-sum(is.na(a1[,r])) 
                        miss_pc<-100*miss/nrow(a1)
                        mean<-mean(a1[,r],na.rm = T)
                        median<-median(a1[,r],na.rm = T)
                        sd<-sd(a1[,r],na.rm = T)
                        qtl<-as.numeric(quantile(a1[,r],prob=seq(0,1,0.05),na.rm = T))
                        num_unique_obs<-NA
                        factor_unique_level_obs<-NA
                } #If loop factor
                
                x_d<-cbind.data.frame(Variable_Names=names(a1[r]),Data_Type,num_obs,miss,miss_pc,mean,median,sd,
                                      min=qtl[1],qtl_5th=qtl[2],qtl_10th=qtl[3],qtl_15th=qtl[4],
                                      qtl_20th=qtl[5],qtl_25th=qtl[6],qtl_30th=qtl[7],qtl_35th=qtl[8],
                                      qtl_40th=qtl[9],qtl_45th=qtl[10],qtl_50th=qtl[11],
                                      qtl_55th=qtl[12],qtl_60th=qtl[13],qtl_65th=qtl[14],qtl_70th=qtl[15],
                                      qtl_75th=qtl[16],qtl_80th=qtl[17],qtl_85th=qtl[18],
                                      qtl_90th=qtl[19],qtl_95th=qtl[20],max=qtl[21],
                                      num_unique_obs,factor_unique_level_obs)
                
                #rownames(x_d)<-names(a1[r])
                
                if(cnt==0){
                        x_d1<-x_d
                        cnt=cnt+1
                }else{
                        x_d1<-rbind.data.frame(x_d1,x_d)
                }},error=function(e){cat("Error:","Occured in Column-",r,"-in your Data","\n")})
}

View(x_d1)


a1
