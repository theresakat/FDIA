
# EDA of responses to understand the Maturity Index results
distinctElems<- longVals %>% group_by(ID.x, Theme) %>% select(DataElem) %>% distinct()
write.csv(distinctElems, "c:\\Temp\\FDIA\\CSV\\distincElems.csv")

dataElemsCount <- distinctElems %>% group_by(Theme) %>% 
  summarize(n =n())

# These 3 themes had only 1 or 2 elements each
distinctElems[distinctElems$Theme == "Cadastral",]
distinctElems[distinctElems$Theme == "Transportation",]
distinctElems[distinctElems$Theme == "Utilities",]
distinctElems[distinctElems$Theme == "Hydrography",]

# Which data elements should be omitted?
write.csv(mydata, "c:\\Temp\\FDIA\\CSV\\mydata.csv")



# Some gauge graphs
# Life Cycle Stage Maturity
grid.arrange(arrangeGrob(gg.gauge(43," "),
                         gg.gauge(45," "),
                         gg.gauge(43," "), ncol=3))
grid.arrange(arrangeGrob(
                         gg.gauge(56," "),
                         gg.gauge(33," "),
                         gg.gauge(32," "),
                         gg.gauge(32," "),
                         ncol=4))

# Gauge graphs for themes

distinctElems[distinctElems$Theme == "Climate",]
matThmStage %>% filter(Theme == "Climate" )

grid.arrange(arrangeGrob(gg.gauge(62," "),
                         gg.gauge(42," "),
                         gg.gauge(37," "),ncol=3))
grid.arrange(arrangeGrob(gg.gauge(64," "),
                         gg.gauge(35," "),
                         gg.gauge(29," "),
                         gg.gauge(30," "),
                         ncol=4))


matThmStage %>% filter(Theme == "Hydrography" )

grid.arrange(arrangeGrob(gg.gauge(53," "),
                         gg.gauge(49," "),
                         gg.gauge(60," "),
                         gg.gauge(60," "),
                         gg.gauge(38),
                         gg.gauge(29," "),
                         gg.gauge(10," "),
                         ncol=3))


