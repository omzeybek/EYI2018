#save.image("Muhendis_kizlar.Rdata")
hhia.2017<-read.csv(file.choose(),sep=";",stringsAsFactors = TRUE)
library(dplyr)
library(plyr)
library(ggplot2)
library(sqldf)
library(mosaic)
library(scales)
library(tidyverse)
library(crayon)
names(hhia.2017)
class(hhia.2017$HG110)

MK_hhia<-hhia.2017[,c(1:2,4:10,24,25:26,28:29,33,22,101)]
names(MK_hhia)
class(hhia.2017$EGITIM_DEVAM_K)
####################################################
#Model 1 ve Model 2 için Targetları oluşturuyorum. #
###################################################

MK_hhia$ID<-paste(MK_hhia$REFERANS_YIL,MK_hhia$BIRIMNO,MK_hhia$FERTNO,sep="")


MK_hhia$ISTIHDAM_STEM_GENC_F<-ifelse(hhia.2017$REFERANS_YIL==2017 & hhia.2017$YAS>22 & hhia.2017$YAS<35 & hhia.2017$OKUL_BITEN_K%in%c("4","5")&hhia.2017$ISCEDF13_K%in%c("13","9","10","11","12")==1 &hhia.2017$DURUM==1,1,0)
#22 - 35 yaş arası Yüksek Okul, Fakulte, Yüksek Lisans mezunu, STEM'e giren alanlardan mezun kişiler = 1, all other= 0

MK_hhia$ISTIHDAM_STEM_GENC_KADIN_F<-ifelse(hhia.2017$REFERANS_YIL==2017 & hhia.2017$YAS>22 & hhia.2017$YAS<35 & hhia.2017$OKUL_BITEN_K%in%c("4","5")&hhia.2017$ISCEDF13_K%in%c("13","9","10","11","12")==1 &hhia.2017$DURUM==1&hhia.2017$CINSIYET==2,1,0)
#22 -35 yaş arası Yüksek Okul, Fakulte, Yüksek Lisans mezunu, STEM'e giren alanlardan mezun çalışan insanlar içersinden kadınlar =1 , all other= 0


#############################################################
#Model 1 ve 2 için bağımsız değişkenler buradan üretiliyor. #
############################################################

#Yüksek Lisans Flag - Yüksek Lisans MEzunları =1 
MK_hhia$Yks_Ls_F<-ifelse(hhia.2017$OKUL_BITEN_K==5,1,0)

#Örgün Eğitime Devam mı? Flagi yaratılıyor 
MK_hhia$EGITIM_DEVAM_K_F<-ifelse(MK_hhia$EGITIM_DEVAM_K==1,1,0)

#Özel ders veya kursa gidiyor musunuz? flagi yazılıyor 
MK_hhia$KURS_F<-ifelse(MK_hhia$KURS==1,1,0)

#Medeni durum flagi yaratılıyor (TUIK flagleri 1/2 şekilinde yarattığından, ikili cevap
#olsa dahi her flagi yeniden yaratmalıyız.)
MK_hhia$EVLI_F<-ifelse(MK_hhia$MEDENI_DURUM==2,1,0)
MK_hhia$HIC_EVLNMDI_F<-ifelse(MK_hhia$MEDENI_DURUM==1,1,0)

#Bu işinizden dolayı Sosyal Güvenlik Kurumu'na (SGK) kayıtlı mısınız? Flagi yapılıyor 

MK_hhia$KAYITLILIK<-if_else(hhia.2017$KAYITLILIK==1,1,0)

#Mevcut işte çalışma süresi 

MK_hhia$Calisma_sure<-hhia.2017$REFERANS_YIL-hhia.2017$IS_BASLAMA_YIL

#Hayatınız boyunca bir işte çalıştınız mı flag'i 

MK_hhia$GECMIS_IS_F<-if_else(hhia.2017$GECMIS_IS==1,1,0)

#Bu işinizin süreklilik durumu nedir? Flag

MK_hhia$IS_SUREKLILIK<-if_else(hhia.2017$IS_SUREKLILIK==1,1,0)
MK_hhia$GECICI_MEVSIMLIK_ISCI_F<-if_else(hhia.2017$IS_SUREKLILIK==2,1,0)

#İşyerinizdeki çalışma şekliniz Tam vs. Yarı Zamanlı 

MK_hhia$YARI_ZAMANLI_F<-if_else(hhia.2017$CALISMA_SEKLI==2,1,0)
MK_hhia$TAM_ZAMANLI_F<-if_else(hhia.2017$CALISMA_SEKLI==1,1,0)

#IBBS Bölge 
MK_hhia$IBBS_2<-hhia.2017$IBBS_2

#Doğduğunuzdan  beri bu ilde mi yaşıyorsunuz? 
MK_hhia$BUIL_YASAMA<-hhia.2017$BUIL_YASAMA

#Yuksek Lisans'a Devam 
MK_hhia$YUKSEK_LISANS_D<-if_else(hhia.2017$OKUL_DEVAM_K==5,1,0)

#Lisans'a devam 
MK_hhia$Lisans_D<-if_else(hhia.2017$OKUL_DEVAM_K==4,1,0)

#Gelir
MK_hhia$GELIR<-hhia.2017$GELIR_GECENAY_K

#Şimdi ikiden fazla farklı değer alan nominal değişkenlerimi var/yok şeklinde dummy variable'a değiştireceğim. 
#Nominal değişkenler arasında büyüklük küçüklük ilişkisi yoktur. Bu nedenle kodlandıkları 1,2,3,4 gibi şekillerle modele yerleştirilemezler 
#Bunları modelimizde temsil edebilmek için, 
# 1. Ücretli, maaşlı veya yevmiyeli 
# 2. İşveren
# 3. Kendi hesabına
# 4. Ücretsiz aile işçisi
# şeklinde kırılımı olan bir yapıyı 4 ayrı değişken ile takip edeceğim. 

#Bu işi plyr'nin map values fonksiyonu ile yapacağız 
library(plyr)
names(MK_hhia)
#**************************************************************************************
#*Nominal değişkenlerden 0/1 dummylerine geçebilmek için önce kodlar ile labelları de-*
#ğiştiriyorum.                                                                        * 
#**************************************************************************************

#DEĞİŞKEN : Referans haftası içinde bu işinizde/işyerinizde niçin çalışmadınız?
MK_hhia_T1<-MK_hhia[,c(18,2,3,4)]
names(MK_hhia_T1)
#Bütün boş alanları 0 ile dolduruyorum. 

hhia.2017$CALISMAMA_NEDEN_REF[is.na(hhia.2017$CALISMAMA_NEDEN_REF)]<-0
hhia.2017[is.na(hhia.2017)]<-0


MK_hhia_T1$CALISMAMA_NEDEN_REF<-mapvalues(as.factor(hhia.2017$CALISMAMA_NEDEN_REF),
                                         from= c(0,1,2,3,4,5,6,7,8,98),
                                         to= c("Boş","Kendisinin hastalanması, yaralanması veya geçici rahatsızlanması",
                                               "Doğum izni",
                                               "Tatil veya izin",
                                               "Kötü hava koşulları",
                                               "İşin gereği",
                                               "Eğitim,öğretim",
                                               "Teknik veya ekonomik nedenlerle iş yavaşlatılması veya durdurulması",
                                               "İş olmadığından",
                                               "Diğer"))

#DEĞİŞKEN : Bu yer, kuruluş veya işyerinde işteki durumunuz nedir?

MK_hhia_T1$ISTEKI_DURUM_K<-mapvalues(as.factor(hhia.2017$ISTEKI_DURUM_K),
                                         from= c(0,1,2,3,4),
                                         to= c("Boş","Ücretli, maaşlı veya yevmiyeli",
                                               "İşveren",
                                               "Kendi hesabına",
                                               "Ücretsiz aile işçisi"))
                      table(MK_hhia_T1$ISTEKI_DURUM_K)
#DEĞİŞKEN : Çalıştığınız bu işyerinin statüsünü belirtiniz.

                      
MK_hhia_T1$OZEL_KAMU<-mapvalues(as.factor(hhia.2017$OZEL_KAMU),
                               from=c(0,1,2,98),
                               to=c("Boş","Özel",
                                    "Kamu",
                                    "Diğer"))

#DEĞİŞKEN: Çalıştığınız bu işyerinin durumu?

MK_hhia_T1$ISYERI_DURUM<-mapvalues(hhia.2017$ISYERI_DURUM,
                                from=c(0,1,2,3,4,5),
                                to=c("Boş","Tarla,bahçe",
                                     "Düzenli işyeri (Fabrika, büro, mağaza, vb.)",
                                     "Pazar yeri",
                                     "Seyyar veya sabit olmayan işyeri",
                                     "Evde (Kendi veya başkasının evinde)"))

#DEĞİŞKEN: Bu yer, kuruluş veya işyerinde çalışan sayısını belirtiniz.

MK_hhia_T1$CALISAN_SAYI_HH<-mapvalues(hhia.2017$CALISAN_SAYI_HH,
                                      from=c(0,1,2,3,4,5),
                                      to=c("Boş","10 ve daha az kişi",
                                           "11-19 Kişi",
                                           "20-49 Kişi",
                                           "50 veya daha fazla kişi",
                                           "Bilmiyor, fakat 10'dan fazla kişi"))

#DEĞİŞKEN: Kişinin yaptığı işe uygun meslek kodu ISCO08 

MK_hhia_T1$ISCO08<-mapvalues(hhia.2017$ISCO08_ESAS_K,
                             from = c(0,11,	12,	13,	14,	21,	22,	23,	24,	25,	26,	31,	32,	33,	34,	35,	41,	42,	43,	44,	51,	52,	53,	54,	61,	62,	63,	71,	72,	73,	74,	75,	81,	82,	83,	91,	92,	93,	94,	95,	96),
                             to=c(
                                   "Boş","Başkanlar, üst düzey yöneticiler ve kanun yapıcılar",
"Ticari ve idari müdürler",
"Üretim ve uzmanlaşmış hizmet müdürleri",
"Ağırlama, perakende ve diğer hizmet müdürleri",
"Bilim ve mühendislik alanlarındaki profesyonel meslek mensupları",
"Sağlık profesyonelleri",
"Eğitim ile ilgili profesyonel meslek mensupları",
"İş ve yönetim ile ilgili profesyonel meslek mensupları",
"Bilgi ve iletişim teknolojisi ile ilgili profesyonel meslek mensupları",
"Hukuk, sosyal ve kültür ile ilgili profesyonel meslek mensupları",
"Bilim ve mühendislik ile ilgili yardımcı profesyonel meslek mensupları",
"Yardımcı sağlık profesyonelleri",
"İş ve idare ile ilgili yardımcı profesyonel meslek mensupları",
"Hukuk, sosyal, kültür ve benzeri alanlar ile ilgili yardımcı profesyonel meslek mensupları",
"Bilgi ve iletişim teknisyenleri",
"Genel büro elemanları ile klavye kullanan büro elemanları",
"Müşteri hizmetlerinde çalışan elemanlar",
"Sayısal işlemler yapan ve malzeme kayıtları tutan büro elemanları",
"Diğer büro hizmetlerinde çalışan elemanlar",
"Kişisel hizmetler veren elemanlar",
"Satış hizmetleri veren elemanlar",
"Kişisel bakım hizmetleri veren elemanlar",
"Koruma hizmetleri veren elemanlar",
"Pazara yönelik nitelikli tarım çalışanları",
"Pazara yönelik nitelikli ormancılık, su ürünleri ve avcılık çalışanları",
"Kendi geçimine yönelik çiftçiler, balıkçılar, avcılar ve  toplayıcılar",
"İnşaat ve ilgili işlerde çalışan sanatkarlar (elektrikçiler hariç)",
"Metal işleme, makine ve ilgili işlerde çalışan sanatkarlar",
"El sanatları ve basım ile ilgili işlerde çalışanlar",
"Elektrik ve elektronik işlerde çalışan sanatkarlar",
"Gıda işleme, ağaç işleri, giyim eşyası ve diğer sanatkarlar ve ilgili işlerde çalışanlar",
"Sabit tesis ve makine operatörleri",
"Montajcılar",
"Sürücüler ve hareketli makine ve teçhizat operatörleri",
"Temizlikçiler ve yardımcılar",
"Tarım, ormancılık ve balıkçılık  sektörlerinde nitelik gerektirmeyen işlerde çalışanlar",
"Madencilik, inşaat, imalat ve ulaştırma sektörlerinde nitelik gerektirmeyen işlerde çalışanlar",
"Yiyecek hazırlama yardımcıları",
"Cadde ve sokaklarda satış ve hizmet işlerinde çalışanlar",
"Çöpçüler, atık toplayıcılar ve diğer nitelik gerektirmeyen işlerde çalışanlar"
))

#DEĞİŞKEN : En son çalıştığınız bu işinizden ayrılmanızdaki esas neden neydi?

MK_hhia_T1$IS_AYRIL_NEDEN<-mapvalues(as.factor(hhia.2017$IS_AYRIL_NEDEN),
                                     from=c(0,1,2,3,4,5,6,7,8,9,10,98),
                                     to=c("Boş","Geçici bir işti bitti",
                                          "Mevsimlik çalışıyordu",
                                          "İşten çıkartıldı/işyeri kapandı/iflas etti",
                                          "İşinden memnun değildi",
                                          "Kendisinin hastalanması veya sakatlanması",
                                          "Ailedeki çocuklara veya bakıma muhtaç yetişkinlere baktığı için",
                                          "Eşinin isteği üzerine/evlilik nedeniyle",
                                          "Eğitim / öğretim",
                                          "Emeklilik (Erken emeklilik dahil",
                                          "Askere gitti",
                                          "Diğer"))

#DEĞİŞKEN: Referans haftası içinde, esas işinizde neden genellikle çalıştığınız süreden daha fazla çalıştınız?

MK_hhia_T1$FAZLACAL_NEDEN<-mapvalues(hhia.2017$FAZLACAL_NEDEN,
                                     from=c(0,1,2,98),
                                     to=c("Boş","Esnek_Çalışma",
                                          "Fazla Mesai",
                                          "Diğer"))
table(MK_hhia_T1$FAZLACAL_NEDEN)
#DEĞİŞKEN: İşiniz tamamını veya belli bir bölümünü evinizde gerçekleştiriyor musunuz? 

MK_hhia_T1$EVDE_CAL_SIKLIK<-mapvalues(hhia.2017$EVDE_CAL_SIKLIK,
                                      from=c(0,1,2,3),
                                      to=c("Boş","Genellikle Evde Çalışıyorum",
                                           "Bazen Evde Çalışıyorum",
                                           "Hiç Evde Çalışmıyorum"))

#DEĞİŞKEN: Neden yarı zamanlı çalışıyorsunuz?"

MK_hhia_T1$YARIZAMAN_NEDEN<-mapvalues(hhia.2017$YARIZAMAN_NEDEN,
                                     c(0,11,12,13,2,3,4,5,6,98),
                                     c("Boş","Ailedeki çocuklara baktığı için",
                                       "Ailedeki bakıma muhtaç yetişkinlere baktığı için",
                                       "Hem ailedeki çocuklara hem de bakıma muhtaç yetişkinlere baktığı için",
                                       "Eğitimine devam ettiği için",
                                       "Kendi hastalığı ya da engellilik hali nedeniyle",
                                       "Diğer ailevi ve kişisel nedenlerden dolayı",
                                       "Tam zamanlı bir iş bulamadığı için",
                                       "İşin niteliği gereği",
                                       "Diğer"))

#DEĞİŞKEN: Bu işinizi nasıl buldunuz?

MK_hhia_T1$ISBUL_YONTEM<-mapvalues(hhia.2017$ISBUL_YONTEM,
                                   from=c(0,1,2,3,4,98),
                                   to=c("Boş","Kendi imkanlarımla",
                                        "Türkiye İş Kurumu kanalıyla",
                                        "Özel istihdam ofisleri kanalıyla",
                                        "Akraba, eş ve dost aracılığıyla",
                                        "Diğer"))

#DEĞİŞKEN : Eski Yerleşim Bölgesi Tür 

MK_hhia_T1$YERLESIM_TUR<-mapvalues(hhia.2017$YERLESIM_TUR,
                                  from=c(0,1,2,3),
                                  to=c("Boş","İl merkezi",
                                       "İlçe merkezi",
                                       "Bucak veya köy"))

#DEĞİŞKEN: En son bitirilen bölüm kodu
hhia.2017$ISCEDF13_K[is.na(hhia.2017$ISCEDF13_K)]<-0
table(hhia.2017$ISCEDF13_K)
MK_hhia_T1$MEZUN_BOLUM<-mapvalues(hhia.2017$ISCEDF13_K,from=c(0:22),
                                  to=c("Boş","Eğitim",
                                    "Sanat",
                                    "Beşeri bilimler",
                                    "Diller",
                                    "Sosyal bilimler ve davranış bilimleri",
                                    "Gazetecilik ve enformasyon",
                                    "İş ve yönetim",
                                    "Hukuk",
                                    "Biyoloji, çevre ve ilgili birimler",
                                    "Fiziki bilimler",
                                    "Matematik ve istatistik",
                                    "Bilişim ve iletişim teknolojileri",
                                    "Mühendislik ve mühendislik işleri",
                                    "İmalat ve işleme",
                                    "Mimarlık ve inşaat",
                                    "Tarım, ormancılık ve balıkçılık ",
                                    "Veterinerlik",
                                    "Sağlık",
                                    "Refah (Sosyal hizmetler)",
                                    "Kişisel hizmetler",
                                    "İş sağlığı ve ulaştırma hizmetleri",
                                    "Güvenlik hizmetleri"))
install.packages("stats")
library(stats)



Categoricaltobinary<-data.frame(model.matrix(~MEZUN_BOLUM+YERLESIM_TUR+ISBUL_YONTEM+YARIZAMAN_NEDEN+EVDE_CAL_SIKLIK+ISCO08+CALISAN_SAYI_HH+ISYERI_DURUM+ISCO08+CALISAN_SAYI_HH+OZEL_KAMU+ISTEKI_DURUM_K,MK_hhia_T1))
MK_hhia_t1[]
Categoricaltobinary_2<-merge(data.frame(MK_hhia_T1),data.frame(Categoricaltobinary))


