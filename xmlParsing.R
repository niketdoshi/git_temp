library(XML)
library(plyr)
library(tidyr)
library(dplyr)


nullfn <- function(x,...) NULL


xt <- xmlTreeParse("20160701000503_sma.xml",
                   handlers = list(Reference = nullfn,Time = nullfn),asTree = TRUE)
xtList <- xmlToList(xt)
df <- ldply(xtList,data.frame)

df <- tbl_df(df)
df1 <- select(df,Name, Values.Mmt,Values.V) %>% 
  filter(!is.na(Values.Mmt)) %>% 
  spread(Values.Mmt,Values.V)
  
df2 <- select(df,Name, Values.Mmt.1,Values.V.1) %>% 
  filter(!is.na(Values.Mmt.1)) %>% 
  spread(Values.Mmt.1,Values.V.1)

cleanedXMLdata <- select(df1,-Insolation) %>% 
  filter(!is.na(Temperature)) %>% 
  rbind(select(df2,-Insolation) %>% filter(!is.na(Temperature))) %>% 
  full_join(select(df1,-Temperature) %>% 
              filter(!is.na(Insolation)) %>% 
              rbind(select(df2,-Temperature) %>% filter(!is.na(Insolation))),by = "Name")