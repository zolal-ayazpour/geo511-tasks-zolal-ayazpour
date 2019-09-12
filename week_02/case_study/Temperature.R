library(tidyverse)

# define the link to the data - you can try this in your browser too
dataurl="https://raw.githubusercontent.com/AdamWilsonLab/SpatialDataScience/master/docs/02_assets/buffaloweather.csv"

temp=read_csv(dataurl,
              skip=1, #skip the first line which has column names
              na="999.90", # tell R that 999.90 means missing in this dataset
              col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                            "APR","MAY","JUN","JUL",  
                            "AUG","SEP","OCT","NOV",  
                            "DEC","DJF","MAM","JJA",  
                            "SON","metANN"))
# renaming is necessary becuase they used dashes ("-")
# in the column names and R doesn't like that.

title = paste0("Mean Summer Temperature in Buffalo, NY (1880-2018)")
subtitle= paste0("Summer includes Jun, July, and August","\n",
                 "Data from the Global Historical Climate Network", "\n",
                  "Red line is a LOESS smooth")

ggplot(temp,aes(x=YEAR,y=JJA))+
  geom_line()+
  geom_smooth(col="red")+
  xlab("Year")+
  ylab("Mean Summer Temperature(C)")+
  ggtitle(title, subtitle)
ggsave("Graph.png")

#png(filename = "Graph1.png")
#plot ...
#dev.off()


