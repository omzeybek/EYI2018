#Ilk olarak kitle belirleniyor 
hhia2017<-read.csv(file.choose(),sep=";",stringsAsFactors = TRUE)
hhia.2017<-hhia2017%>%filter(YAS>22&YAS<35&OKUL_BITEN_K%in%c("4","5")&ISCEDF13_K%in%c("13","9","10","11","12","14","15")&CINSIYET==2)
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
hhia.2017$ISTIHDAM_STEM_GENC_KADIN_F<-ifelse(hhia.2017$REFERANS_YIL==2017 & hhia.2017$YAS>22 & hhia.2017$YAS<35 & hhia.2017$OKUL_BITEN_K%in%c("4","5")&hhia.2017$ISCEDF13_K%in%c("13","9","10","11","12")==1&hhia.2017$DURUM%in%c(1)&hhia.2017$CINSIYET==2&hhia.2017$ISCO08_ESAS_K=="21",1,0)
table(hhia.2017$ISTIHDAM_STEM_GENC_KADIN_F)#Dagilim durumu 
#ID kodu üretiyorum 
hhia.2017$ID<-paste(hhia.2017$REFERANS_YIL,hhia.2017$BIRIMNO,hhia.2017$FERTNO,sep="")

#********************************************************************************************************
*  #Simdi Bagimsiz degiskenler üretiliyor. 
*
*********************************************************************************************************

# Hane Halki Büyüklügü 
#  1   2   3   4   5   6   7   8   9  11 
# 67 326 644 460 168  80  22  10   3   1
  hhia.2017$HH_SAYI<-hhia.2017$HH_BUYUKLUK

#Üç Büyük SEhir Ikamet 
hhia.2017$BUYUK_SEHIR_TOP3_F<-if_else(hhia.2017$IBBS_2%in%c("10","31","51"),1,0)

#Az Gelismis BÖlge ikamet 
hhia.2017$AZ_GE_TOP_F<-ifelse(hhia.2017$IBBS_2%in%c("TRB","TRA","TRC"),1,0)

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
hhia.2017$Muhendis_F<-if_else(hhia.2017$ISCEDF13_K==15,1,0)

#BIlisim sektöründe mi çalisiyorsunuz? 
hhia.2017$Muhendis_F<-if_else(hhia.2017$ISCEDF13_K==12,1,0)

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





