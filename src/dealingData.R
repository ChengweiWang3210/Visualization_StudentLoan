library(data.table)
library(ggplot2)
setwd("/Users/greatyifan/Desktop/@Columbia/2020spring/2_DataViz/course_materials/Exercises/03_student_loans")
dt <- fread('data/survey_SCF.txt', header = T)
dim(dt)

colnames(dt) <- tolower(colnames(dt))



hist(dt$edn_inst, breaks = 100, xlim = c(1,20000))

dt_year <- dt[, mean(edn_inst), by = year]
ggplot(dt_year, aes(x = year, y = V1)) +
  geom_line()

ggplot(dt[,mean(hdebt), by=year], aes(x = year, y = V1))+
  geom_line() +
  ylim(0.5,0.8)

ggplot(dt[hdebt == 1,mean(edn_inst/debt), by=year], aes(x = year, y = V1))+
  geom_line()
#' the debt hold ratio doesnt change
#' how about the edu debt hold ratio?

# ggplot(dt[hdebt == 1,mean((nh_mort+othloc+ccbal+install+veh_inst) / edn_inst, na.rm = T), by=year], aes(x = year, y = V1))+
#   geom_line()
ggplot()+
  # geom_line(data = dt[, mean(levratio, na.rm = T), by=year], aes(x = year, y = V1)) + 
  geom_line(data = dt[hdebt == 1,mean(edn_inst/debt), by=year], aes(x = year, y = V1)) +
  geom_line(data = dt[,mean(hdebt), by=year], aes(x = year, y = V1))
#' 
#' not use "dt[, mean(debt2inc, na.rm = T), by=year]", debt2inc,  there is
#' a outlier in 1992. According to the economy records in usa,  early 1990s 
#' was being through a great recession
#' 
#' 


mean(dt$hhsex)
dt[,edn_inst, by = .(hhsex, year)] # 2 -- female

ggplot(data = dt[,mean(edn_inst), by = .(hhsex, year)], 
       aes(x = year, y = V1, color = as.factor(hhsex))) +
  geom_line()

dt[, haveKids := ifelse(kids == 0, 0, 1)]

dt[year == 2016 & married == 2, mean(edn_inst), by = .(year, hhsex, haveKids)]

ggplot(dt[hhsex == 2, mean(edn_inst), by = .(year, hhsex, haveKids)], 
       aes(x = year, y = V1, color = as.factor(haveKids))) +
  geom_line()


# 3 
#' BNKRUPLAST5
#' FORECLLAST5	
mean(dt$bnkruplast5)

dt[year == 2016 & (bnkruplast5 == 1 | forecllast5 == 1), mean(edn_inst)]

dt[year == 2016, mean(bnkruplast5), by = nwcat]

dt[year == 2016, mean(bnkruplast5), by = inccat]

dt[, thrifty := foodhome/(foodhome + fooddelv + foodaway)]
?cut()

dt[, isThrifty := cut(dt$thrifty, 5)]

ggplot(dt[year == 2016, mean(hdebt), by = isThrifty], aes(x = isThrifty, y = V1))+
  geom_bar(stat = 'identity')











