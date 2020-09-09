#-------------Implement VAR with RR Shock---------------------------------------
# Author: Pritha Chaudhuri
# Date: 1/16/2020

library(tidyverse)
library(vars)
select <- dplyr::select

#--------------------------------------------------------------------------------
## Obtain Aggregate data, RR monetary policy shock calculated in Matlab inlcuded
aggdata <- read.csv("data/var_data.csv", header = TRUE, stringsAsFactors = F) %>% 
  mutate(date = as.Date.character(Date, format = "%m/%d/%Y"),
         year = lubridate::year(date),
         month = lubridate::month(date),
         rrshock = cumsum(mpshock))

#--------------------------------------------------------------------------------
## Run create_cps_data.R to get unemployment data for different groups.

#-------------------------------------------------------------------------------
## Function to plot impulse response
plot.irf2 <- function(model1,model2,shock1,shock2){
  irfresult1 <- irf(model1,
                    impulse = shock1, n.ahead = 48, ortho = T, cumulative = F, boot = F)
  impulse1 <- as.data.frame(100*irfresult1$irf[[shock1]]) %>% 
    select("logip", "lopcpi", starts_with("unemp_")) %>% 
    mutate(model = "RR shock",
           tt = row_number())
  
  irfresult2 <- irf(model2,
                    impulse = shock2, n.ahead = 48, ortho = T, cumulative = F, boot = F)
  impulse2 <- as.data.frame(100*irfresult2$irf[[shock2]]) %>% 
    select("logip", "lopcpi", starts_with("unemp_")) %>% 
    mutate(model = "FFR shock",
           tt = row_number())
  
  impulse <- bind_rows(impulse1,impulse2)
  
  return(impulse)
  
}



#-------------------------------------------------------------------------------
# High-skill vs low-skill only
# 2 categories: HS and LS
# Fig 2 in paper
#-------------------------------------------------------------------------------

## Data
vardata_skill$date <- lubridate::ymd(paste0(sprintf("%d-%02d",
                                                    vardata_skill$year,
                                                    vardata_skill$intmonth),"-01"))
# vardata_skill created in separate file, create_cps_data.R

vardata <- select(aggdata,date,year,month,logip,lopcpi,logwcp,rrshock,ff)%>%
  left_join(select(vardata_skill,date,unemp_HS,unemp_LS),by="date")%>%
  filter(year %in% c(1979:2007))%>%
  arrange(year,month)

## Run VAR with RR shock
cols <- c("logip","lopcpi","logwcp","unemp_HS","unemp_LS","rrshock")
vardata.rr <- VAR(ts(vardata[,cols],frequency = 12,start=1979),
                  p = 12, type = "const")

## Run VAR with Federal Funds Rate
cols <- c("logip","lopcpi","logwcp","unemp_HS","unemp_LS","ff")
vardata.ff <- VAR(ts(vardata[,cols],frequency = 12,start=1979),
                  p = 12, type = "const")

## Create impulse response functions
hsls.plot <- plot.irf2(vardata.rr, vardata.ff, "rrshock", "ff")

## Plot impulse response functions, save each plot separately in folder named "figures"
skill <- c("logip", "lopcpi", "unemp_HS", "unemp_LS")
# yup <- c(0.2, 0.05, 15, 15)
# ydown <- c(-0.4, -0.25, -5, -5)
for (i in skill){
  select(hsls.plot,tt,model,i) %>%
    ggplot(aes_string("tt", i, group="model")) +
    geom_line(aes(linetype = model, color = model), size = 1, show.legend = F) +
    geom_hline(yintercept=0, size = 0.25)+
    theme_minimal() +
    scale_linetype_manual(values=c("solid", "solid")) +
    scale_color_manual(values=c("blue", "red")) +
    scale_y_continuous("LS unemployment", limits = c(-5, 15)) +
    # scale_y_continuous("Industrial production") +
    # scale_x_continuous() +
    theme(text = element_text(size = 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  ggsave(paste0("figures/",i,".pdf"))
}


#-------------------------------------------------------------------------------
# High-skill low-skill and Male-Female
# 4 categories: HS Male, HS Female, LS Male, LS Female
# Fig 3 in paper
#-------------------------------------------------------------------------------

## Data
vardata_skill_gender$date <- lubridate::ymd(paste0(sprintf("%d-%02d",
                                                           vardata_skill_gender$year,
                                                           vardata_skill_gender$intmonth),"-01"))

vardata <- select(aggdata,date,year,month,logip,lopcpi,logwcp,rrshock,ff)%>%
  left_join(select(vardata_skill_gender,date,unemp_HSM,unemp_HSF,unemp_LSM,unemp_LSF),by="date")%>%
  filter(year %in% c(1979:2007))%>%
  arrange(year,month)

## Run VAR with RR shock
cols <- c("logip","lopcpi","logwcp","unemp_HSM", "unemp_HSF","unemp_LSM","unemp_LSF","rrshock")
vardata.rr.g <- VAR(ts(vardata[,cols],frequency = 12,start=1979),
                    p = 12, type = "const")

## Run VAR with Federal Funds Rate shock
cols <- c("logip","lopcpi","logwcp","unemp_HSM", "unemp_HSF","unemp_LSM","unemp_LSF","ff")
vardata.ff.g <- VAR(ts(vardata[,cols],frequency = 12,start=1979),
                    p = 12, type = "const")

## Create impulse response functions
hsls.mf.plot <- plot.irf2(vardata.rr.g, vardata.ff.g, "rrshock", "ff")

## Plot impulse response functions, save each plot separately in folder named "figures"
gender <- c("unemp_HSM", "unemp_HSF", "unemp_LSM", "unemp_LSF")
for (i in gender){
  select(hsls.mf.plot,tt,model,i) %>%
    ggplot(aes_string("tt", i, group="model")) +
    geom_line(aes(linetype = model, color = model), size = 1, show.legend = F) +
    geom_hline(yintercept=0, size = 0.25)+
    theme_minimal() +
    scale_linetype_manual(values=c("solid", "solid")) +
    scale_color_manual(values=c("blue", "red")) +
    scale_y_continuous("Low-skill female", limits = c(-5, 20)) +
    # scale_x_continuous("Months") +
    theme(text = element_text(size = 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  ggsave(paste0("figures/",i,"_gender.pdf"))
}


#-------------------------------------------------------------------------------
# High-skill low-skill and White-Black-Other
# 6 categories: HS White, HS Black, HS Other, LS White, LS Black, LS Other
# Fig 4 in paper
#-------------------------------------------------------------------------------

## Data
vardata_skill_race$date <- lubridate::ymd(paste0(sprintf("%d-%02d",
                                                         vardata_skill_race$year,
                                                         vardata_skill_race$intmonth),"-01"))

vardata <- select(aggdata,date,year,month,logip,lopcpi,logwcp,rrshock,ff)%>%
  left_join(select(vardata_skill_race,date,unemp_HSB,unemp_HSW,unemp_HSO,unemp_LSB,unemp_LSW,unemp_LSO),by="date")%>%
  filter(year %in% c(1979:2007))%>%
  arrange(year,month)

## Run VAR with RR shock
cols <- c("logip","lopcpi","logwcp","unemp_HSB", "unemp_HSW","unemp_HSO","unemp_LSB","unemp_LSW","unemp_LSO","rrshock")
vardata.rr.r <- VAR(ts(vardata[,cols],frequency = 12,start=1979),
                    p = 12, type = "const")

## Run VAR with Federal Funds Rate shock
cols <- c("logip","lopcpi","logwcp","unemp_HSB", "unemp_HSW","unemp_HSO","unemp_LSB","unemp_LSW","unemp_LSO","ff")
vardata.ff.r <- VAR(ts(vardata[,cols],frequency = 12,start=1979),
                    p = 12, type = "const")

## Create impulse response functions
hsls.race.plot <- plot.irf2(vardata.rr.r, vardata.ff.r, "rrshock", "ff")

## Plot impulse response functions, save each plot separately in folder named "figures"
race <- c("unemp_HSB", "unemp_HSW", "unemp_HSO", "unemp_LSB", "unemp_LSW", "unemp_LSO")
for (i in race){
  select(hsls.race.plot,tt,model,i) %>%
    ggplot(aes_string("tt", i, group="model")) +
    geom_line(aes(linetype = model, color = model), size = 1, show.legend = F) +
    geom_hline(yintercept=0, size = 0.25)+
    theme_minimal() +
    scale_linetype_manual(values=c("solid", "solid")) +
    scale_color_manual(values=c("blue", "red")) +
    scale_y_continuous("Low-skill Others", limits = c(-30, 30)) +
    # scale_x_continuous("Months") +
    theme(text = element_text(size = 20),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  ggsave(paste0("figures/",i,"_race.pdf"))
}


#-------------------------------------------------------------------------------
# Fig 1 in paper: dFFR and RR shock series
#-------------------------------------------------------------------------------

aggdata$dff <- aggdata$ff-lag(aggdata$ff)
aggdata$dff[1] <- 0

aggdata %>% 
  filter(year <= 2007) %>%
  select(meeting=date, dff, mpshock) %>% 
  gather(var, value, -meeting) %>% 
  ggplot(aes(meeting, value, group = var)) +
  geom_line(aes(linetype = var, color = var), size = 1, show.legend = T) +
  geom_hline(yintercept=0, size = 0.25)+
  theme_minimal() +
  scale_linetype_manual(values=c("solid", "solid")) +
  scale_color_manual(values=c("blue", "red")) +
  # scale_y_continuous("% points") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme(text = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_blank())
ggsave(paste0("figures/shockseries.pdf"))
