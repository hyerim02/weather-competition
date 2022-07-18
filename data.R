library(tidyverse)
getwd()
wind <- read.csv("C:/Users/wls40/OneDrive/바탕 화면/DB_SFC_WIND_DD_2012_2016.csv")

ta <- read.csv("C:/Users/wls40/OneDrive/바탕 화면/DB_SFC_TA_DD_2012_2016.csv")

icsr <- read.csv("C:/Users/wls40/OneDrive/바탕 화면/DB_SFC_ICSR_SS_DD_2012_2016.csv")

prsr <- read.csv("C:/Users/wls40/OneDrive/바탕 화면/DB_SFC_PRSR_DD_2012_2016.csv")

rn <- read.csv("C:/Users/wls40/OneDrive/바탕 화면/DB_SFC_RN_DD_2012_2016.csv")

pm10 <- read.csv("C:/Users/wls40/OneDrive/바탕 화면/pm10_final.csv")

txt <- read.csv("C:/Users/wls40/OneDrive/바탕 화면/station_name.csv")


train <- read.csv("C:/Users/wls40/OneDrive/바탕 화면/train.csv",fileEncoding ="UTF-8")
read.csv( )
############

len <- txt%>%group_by(name)%>%tally()%>%data.frame()
data <- txt[order(txt$name),]

ud <- unique(data$name)

for(i in 1:17){
    wind$stn_id[which(wind$stn_id%in%data[data$name==ud[i],1])]<- ud[i]
}

for(i in 1:17){
    ta$stn_id[which(ta$stn_id%in%data[data$name==ud[i],1])]<- ud[i]
}

for(i in 1:17){
    icsr$stn_id[which(icsr$stn_id%in%data[data$name==ud[i],1])]<- ud[i]
}

for(i in 1:17){
    prsr$stn_id[which(prsr$stn_id%in%data[data$name==ud[i],1])]<- ud[i]
}

for(i in 1:17){
    rn$stn_id[which(rn$stn_id%in%data[data$name==ud[i],1])]<- ud[i]
}

for(i in 1:17){
    pm10$stn_id[which(pm10$stn_id%in%data[data$name==ud[i],1])]<- ud[i]
}


wind2 <- wind %>% 
    group_by(tma, stn_id) %>% 
    summarise(avg_ws = mean(avg_ws, na.rm=T), max_ws = mean(max_ws, na.rm=T)) 

ta2 <- ta %>% 
    group_by(tma, stn_id) %>% 
    summarise(avg_ta = mean(avg_ta, na.rm=T), max_ta = mean(max_ta, na.rm=T), min_ta = mean(min_ta, na.rm=T))


icsr2 <- icsr %>% 
    group_by(tma, stn_id) %>% 
    summarise(ssrate = mean(ssrate, na.rm=T), sum_gsr = mean(sum_gsr, na.rm=T))

prsr2 <- prsr %>% 
    group_by(tma, stn_id) %>% 
    summarise(avg_pa = mean(avg_pa, na.rm=T), max_pa = mean(max_pa, na.rm=T), min_pa = mean(min_pa, na.rm=T))

rn2 <- rn %>% 
    group_by(tma, stn_id) %>% 
    summarise(sum_rn = mean(sum_rn, na.rm=T), sum_rn_dur = mean(sum_rn_dur, na.rm=T))
names(pm10)
pm10_2 <- pm10 %>% 
    group_by(date, stn_id) %>% 
    summarise(pm10 = mean(pm10, na.rm=T))


##울산은 지역 전체가 NA라서 0으로 대체
icsr2 <- icsr2 %>% 
    mutate(sum_gsr = ifelse(stn_id == "울산", 0, sum_gsr)) 

is.na(wind2$max_ws) %>% 
    sum()
wind2[is.na(wind2$avg_ws),]

wind2 <- wind2 %>% 
        mutate(avg_ws = ifelse(is.na(avg_ws),2.063487, avg_ws))

wind2 <- wind2 %>% 
    mutate(max_ws = ifelse(is.na(max_ws),4.555805, max_ws))

wind2 %>% 
    group_by(stn_id) %>% 
    filter(stn_id == "대구") %>% 
     summarise(max_ws = mean(max_ws, na.rm = T)) %>% 
                                    pull()


#########

is.na(ta2$avg_ta) %>% 
    sum()
ta2[is.na(ta2$avg_ta),]
is.na(ta2$max_ta) %>% 
    sum()
ta2[is.na(ta2$max_ta),]
is.na(ta2$min_ta) %>% 
    sum()

ta2 %>% 
    filter(avg_ta == 0)

ta2 <- ta2 %>% 
    mutate(avg_ta = ifelse(is.na(avg_ta),20.3, avg_ta))

ta2 <- ta2 %>% 
    mutate(max_ta = ifelse(is.na(max_ta),22.3, max_ta))


####

icsr2
is.na(icsr2$ssrate) %>% 
    sum()
icsr2[is.na(icsr2$ssrate),] %>% 
    filter(stn_id != "864")

is.na(icsr2$sum_gsr) %>% 
    sum()
icsr2[is.na(icsr2$sum_gsr),]

icsr2 <- icsr2 %>% 
    mutate(ssrate = ifelse(is.na(ssrate),13.3, ssrate))

icsr2 <- icsr2 %>% 
    mutate(sum_gsr = ifelse(is.na(sum_gsr),mean(sum_gsr, na.rm=T), sum_gsr))



###
prsr2
is.na(prsr2$avg_pa) %>% 
    sum() #3
prsr2[is.na(prsr2$avg_pa),]

is.na(prsr2$max_pa) %>% 
    sum()
prsr2[is.na(prsr2$max_pa),] %>% 
    filter(stn_id != "864")  ##176빼고 11개

unique(prsr2[is.na(prsr2$max_pa),]$stn_id)

is.na(prsr2$min_pa) %>% 
    sum()
prsr2[is.na(prsr2$min_pa),]%>% 
    filter(stn_id != "864" & stn_id != "176")
unique(prsr2[is.na(prsr2$min_pa),]$stn_id) #13

prsr2 <- prsr2 %>% 
    mutate(avg_pa = ifelse(is.na(avg_pa),mean(avg_pa, na.rm=T), avg_pa))

prsr2 <- prsr2 %>% 
    mutate(max_pa = ifelse(is.na(max_pa),mean(max_pa, na.rm=T), max_pa))

prsr2 <- prsr2 %>% 
    mutate(min_pa = ifelse(is.na(min_pa),mean(min_pa, na.rm=T), min_pa))

######## 
rn2
is.na(rn2$sum_rn) %>% 
    sum() #na 다 0으로 하기 
rn2[is.na(rn2$sum_rn),]
rn2 <- rn2 %>% 
    select(-"sum_rn_dur")

rn2 <- rn2 %>% 
    mutate(sum_rn = ifelse(is.na(sum_rn),0, sum_rn))

#####
pm10_2 #얘는 처리 안하기 
is.na(pm10_2$pm10) %>% 
    sum()  
pm10_2[is.na(pm10_2$pm10),]%>% 
    filter(stn_id != "399" & stn_id != "116"& stn_id != "229")

#########

icsr3 <- icsr2

icsr3 <- icsr3 %>% 
    inner_join(prsr2, by = c("tma", "stn_id"))



icsr3 <- icsr3 %>% 
    inner_join(rn2, by = c("tma", "stn_id"))

icsr3 <- icsr3 %>% 
    inner_join(ta2, by = c("tma", "stn_id"))

icsr3 <- icsr3 %>% 
    inner_join(wind2, by = c("tma", "stn_id"))

ts <- gsub("00:00:00.0$","",icsr3$tma)
ts %>% 
    head()
ts2 <- gsub(" ","",ts)
ts2 %>% 
    head()

icsr3$tma <- ts2
pm10_2 %>% 
    head()
pm10_2<-pm10_2 %>% 
    rename(tma = date)

icsr3 <- icsr3 %>% 
    left_join(pm10_2, by = c("tma", "stn_id"))

unique(icsr3$stn_id)


final <- icsr3 %>% 
    filter(stn_id != "141" & stn_id != "176"& stn_id != "864")
unique(final$stn_id)
summary(final)


######

ts3 <- gsub("-","",ts2)
ts3 %>% 
    head()
final$tma <- ts3

49674/2 - 31059

final %>% 
    group_by(tma)

unique(final$tma)
names(train)
train %>% 
    group_by(back_hospital.yyyymmdd)

train %>% 
    tail()

train_final <- final[1:24837,]
test_final <- final[24838:nrow(final),]

train_final <- train_final %>% 
    ungroup()


txt2 <- txt

rep(txt2, 2)

t<- rbind(txt2,txt2)
t<-t%>%
    mutate(g = c(rep(1,nrow(txt2)),rep(2,nrow(txt2))
                 ))

txt2 %>% 
    left_join(t, by = c("X", "name"))

train_final2<- rbind(train_final,train_final)

train_final2 <- train_final2%>%
    mutate(sex = c(rep(1,nrow(train_final)),rep(2,nrow(train_final))
    )) 


ts3 <- gsub("-","",train_final2$tma)
ts3 %>% 
    head()
train_final2$tma <- ts3

colnames(train) <- c("tma","stn_id", "sex", "frequency")

train_final2 %>% 
    str()
train$tma <- as.character(train$tma)
train %>% 
    str()
train_final2 %>% 
    str()
train_final2$sex <- as.integer(train_final2$sex)

train_final3 <- train_final2 %>% 
    left_join(train, by = c("tma", "stn_id"))

train_final3<-train_final3[1:49674,] %>% 
    select(-"sex.x") %>% 
    rename(sex = sex.y) 

write.csv(train_final3, file = "train_final.csv")

###############

train_0702 <- read.csv("C:/Users/wls40/OneDrive/바탕 화면/train_final_0702.csv")
train_0702 %>% 
    head()

pop <- read.csv("C:/Users/wls40/OneDrive/바탕 화면/population2012_2016.csv")
pop %>% 
    head()
pop2 <- pop %>% 
    pivot_longer(cols = -`행정구역별.1.`) %>% 
    filter(`행정구역별.1.` != "행정구역별(1)", `행정구역별.1.` != "전국")

pop2$name <- gsub("X", "", pop2$name)
pop2 %>% 
    head()
pop2$name <-gsub("\\.","",pop2$name, perl=TRUE)
  

str_replace(pop2$name, ".","") %>% 
    head()


pop3 <- pop2 %>% 
    mutate(tma = str_sub(name, 1,6),
           value = as.numeric(as.character(value))) %>% 
    rename(stn_id = `행정구역별.1.`)
pop3 %>% 
    head()
str_length(pop3$name)
pop3$name <- str_replace(pop3$name, str_sub(pop3$name, 1,6),"a")
pop4 <- pop3 %>% 
    mutate(name = ifelse(name == "a", "elder_rate", 
                         ifelse(name == "a1", "elder", "total_pop")))

pop4 <- pop4 %>% 
    pivot_wider(values_from = value) 
switch(pop4$stn_id,
       "서울특별시"="서울",
       "부산광역시"="부산",
       "대구광역시"="대구",
       "인천광역시"="인천",
       "광주광역시"="광주",
       "대전광역시"="대전")

pop4$stn_id<-gsub("특별시","", pop4$stn_id)
pop4$stn_id<-gsub("광역시","", pop4$stn_id)
pop4$stn_id<-gsub("특별자치시","", pop4$stn_id)
pop4$stn_id<-gsub("특별자치도","", pop4$stn_id)
pop4$stn_id<-gsub("청","", pop4$stn_id)

identical(sort(unique(pop4$stn_id)), unique(train_final3$stn_id))
pop4 <- pop4 %>% 
    rename(month = tma)


train_0702 %>% 
    str()
train_0702_2 <- train_0702 %>% 
    mutate(month = str_sub(tma,1,6))

train_0702_2<-train_0702_2 %>% 
    left_join(pop4, by = c("month","stn_id"))
train_0702_2 %>% 
    filter(stn_id=="강원") %>% 
    select(tma, elder_rate, elder) %>% 
    head()

write.csv(train_0702_2, file = "train_final_20220702.csv")
