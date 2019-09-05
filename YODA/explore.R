library(dplyr)
library(tidyr)
library(ggplot2)

dat <- read.delim("ICPI_MER_Structured_Dataset_Site_IM_Kenya_20180323_v2_1.txt", encoding="UTF-8", stringsAsFactors=FALSE)


dat1 <- dat %>% 
  select(-ends_with("_TARGETS"), -ends_with("APR")) %>%
  gather("year","value", starts_with("FY")) %>%
  filter(!is.na(value))
dat1$year <- recode(dat1$year,
                    `FY2015Q1` = 2015, `FY2015Q2` = 2015.25, `FY2015Q3` = 2015.5, `FY2015Q4` = 2015.75,
                    `FY2016Q1` = 2016, `FY2016Q2` = 2016.25, `FY2016Q3` = 2016.5, `FY2016Q4` = 2016.75,
                    `FY2017Q1` = 2017, `FY2017Q2` = 2017.25, `FY2017Q3` = 2017.5, `FY2017Q4` = 2017.75,
                    `FY2018Q1` = 2018, `FY2018Q2` = 2018.25, `FY2018Q3` = 2018.5, `FY2018Q4` = 2018.75)
dat1$tmpKey <- do.call(paste, dat1[-c(28,44)])
dat1 <- dat1 %>% spread(numeratorDenom, value)

qplot(value,fill=numeratorDenom,data=dat1[dat1$value != 0,]) + facet_wrap(~indicator, scales="free") + theme(legend.position = "bottom")
qplot(value,fill=numeratorDenom,data=dat1[dat1$value != 0 & dat1$indicator == "TX_VIRAL",]) 