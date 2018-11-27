# Plots
# 

source("1_load_FDIA.R") 

### Cleveland Dot Chart (aka, flipped lollipop dot plot; 
### https://www.r-graph-gallery.com/301-custom-lollipop-chart/)
### 
### Note the use of the pipe "%>" to create temporary products from maturity data 
### contained in matSort. Also note that the ggplot call omits the data set, as the data
### is piped to it using %>%.
matSort %>%
  arrange(matSum) %>%
  mutate(x=factor(V10,V10)) %>%
  ggplot( aes(x=x, y=matSum)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=matSum), color = "skyblue") +
  geom_point( color="blue", size=3, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size=7,lineheight = 1.2)
  ) + 
  ggtitle("Maturity of Framework Data Elements") +
  xlab("") +
  ylab("Basic maturity index")