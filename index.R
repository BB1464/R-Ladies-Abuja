library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics, CRAN v3.3.5

head(iris,2)

# Aesthetic

ggplot(data = iris)

ggplot(data = iris,
mapping = aes(
x = Sepal.Length,
y =  Sepal.Width,
colour=Species,shape=Species),
alpha=0.5
) 


# Geometry

apropos('^geom_')


ggplot(data = iris,
mapping = aes(
x = Sepal.Length,
y =  Sepal.Width,
colour=Species,shape=Species),
alpha=0.5
)+
geom_point()



ggplot(data = iris,
mapping = aes(
x = Sepal.Length,
y =  Sepal.Width,shape=Species))+
geom_point(colour='blue')




ggplot(data=faithful) + 
geom_point(aes(x = eruptions, y = waiting, colour = eruptions < 3))





ggplot(data = iris)+
geom_bar(mapping = aes(x = Species,fill=Species),data = NULL,width = 0.7)


ggplot(data = iris,
mapping = aes(
x = Species,
y= Sepal.Length,
fill=Species)
)+
geom_bar(width = 0.9)




ggplot(data = diamonds,mapping = aes(x = color,y=price,fill=cut))+
  geom_col(position = 'fill')




ggplot(data = diamonds,mapping = aes(x = reorder(color,price,FUN=mean),y=price,fill=cut))+
  geom_bar(stat='summary',fun=mean)+xlab('Diamond quality')


ggplot(data = iris,
mapping = aes(
x = Species,
y =  Sepal.Width,
fill=Species))+
geom_boxplot()+geom_jitter()+
stat_summary(fun = mean,geom =             
'point',shape=23,fill='white',size=2.5)


ggplot(data = iris)+
geom_histogram(
mapping = aes(x = Sepal.Length,fill=Species)
)





ggplot(iris, aes(x = Sepal.Length, y = after_stat(count),fill=Species)) + geom_histogram()



ggplot(data = economics,mapping = aes(x = date,y = uempmed))+
  stat_summary(fun = mean,geom = 'line')





ggplot(diamonds, aes(depth, fill = cut, colour = cut)) +
  geom_density(alpha = 0.2, na.rm = TRUE)





ggplot(data = iris,mapping = aes(x = Sepal.Length,y = Petal.Length,fill=Species))+geom_violin()



# Stat

apropos('^stat_')


ggplot(data = iris,
mapping = aes(
x = reorder(Species,Sepal.Width,FUN=median),
y =  Sepal.Width,
fill=Species))+
stat_boxplot(geom = 'errorbar')+
geom_boxplot(notch = TRUE)+geom_jitter()+
stat_summary(fun = mean,geom =             
'point',shape=23,fill='white',size=2.5)+xlab('Species')




ggplot(data=mtcars)+stat_identity(mapping = aes(x = wt,y = mpg))



# After_sat function
ggplot(mpg) + 
  geom_bar(aes(x = class, y = after_stat(100 * count / sum(count))))




ggplot(data = mpg,mapping = aes(x=class,y=stat(100*..count../sum(..count..))))+geom_bar()



ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(aes(fill = stage(class, after_scale = alpha(fill, 0.4))))


ggplot(data=diamonds,aes(x = cut, y = price)) +
stat_summary(
geom = "bar",
fun = "mean",
alpha = .6
) +
stat_summary(
geom = "errorbar",
fun.data = "mean_se",
width = .2
) +
stat_summary(
geom = "point",
fun = "mean"
)




ggplot(mpg)+
  geom_jitter(mapping = aes(x = class,y = hwy),width = 0.2)+
  stat_summary(mapping = aes(x = class,y=hwy),fun = mean,geom='point',colour='red')



ggplot(mpg, aes(x = cty, y = displ)) +
    geom_point(alpha = 0.4, size = 2) +
    scale_x_log10() + geom_smooth(method = "lm")




iris |>
  ggplot(aes(x = Species,
             y = Sepal.Length))+
  stat_summary(geom = 'pointrange',fun.max = max,fun.min = min,fun = mean)



# Scale function

apropos('^scale_')|> head(n=50)


ggplot(mpg, aes(displ, hwy)) + 
geom_point(aes(colour = class))



ggplot(mpg, aes(displ, hwy)) + 
geom_point(aes(colour = class)) +
scale_x_continuous() + 
scale_y_continuous() + 
scale_colour_discrete()



ggplot(mpg, aes(displ, hwy,colour=class)) + 
geom_point() + 
scale_x_log10()



ggplot(data = mpg,mapping = aes(x = displ,y = hwy,colour=class))+
geom_point()+
scale_color_brewer(name='Class',palette = 'Set1')




ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy, colour = class)) + 
  scale_colour_brewer(type = 'qual')



ggplot(data = iris, mapping = aes(x = Species,fill=Species)) +
geom_bar()+
scale_fill_brewer(name='Species',palette = 'Dark2')




ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  scale_x_continuous(breaks = c(3, 5, 6)) + 
  scale_y_continuous(trans = 'log10')


ggplot(data = mpg,mapping = aes(x = displ,y = hwy,colour=class))+
geom_point()+
scale_colour_manual(name='Class',values =                      
c('red','blue','green','cyan',
'gray50','pink','black'))




ggplot(data = economics,mapping = aes(x = date,y = unemploy))+
  geom_line()+
  scale_x_date(date_breaks = '10 years')



ggplot(data = iris, mapping = aes(x =Species,fill=Species))+
geom_bar()+
scale_fill_brewer(name='Species',palette='Dark2')+
scale_x_discrete(name='Species',
labels=c('setosa\n(N=50)','versicolor\n(N=50)','virginica\n(N=50)'))




# Coordinate

apropos('^coord_')



df <- data.frame(Group=c('Tall','Short','Average'),Height=c(14,30,45))

df |>
  dplyr::mutate(Percentage=round(prop.table(Height)*100,1)) |>
  ggplot(aes(x = '',y = Percentage,fill=Group))+
  geom_bar(stat = 'identity',width = 1)+
  coord_polar(theta = 'y',direction = 1)+
  geom_text(aes(label=paste0(Percentage,'%')),position = position_stack(vjust = 0.5))+
  theme_void()+
  scale_fill_brewer(palette = 'Set1')




ggplot(mpg) + 
  geom_bar(aes(x = class)) + 
  coord_cartesian(ylim = c(0, 40))


ggplot(mpg) + 
  geom_bar(aes(x = class)) + 
  coord_cartesian(ylim = c(0, 40),expand=expansion(mult = 0,add = 0))



# Facet

apropos('^facet_')


ggplot(data = mpg)+ 
geom_point(mapping = aes(x = displ, y = hwy)) + 
facet_wrap(~ class, nrow = 2)



ggplot(data = mpg) + 
geom_point(mapping = aes(x = displ, y = hwy)) + 
facet_grid(drv ~ cyl)


# Theme

apropos('^*theme_()')


length(theme_get())

theme_get() |> head(1)

theme_get()$plot.title


ggplot(mpg) + 
  geom_bar(aes(y = class)) + 
  facet_wrap(~year) + 
  theme_minimal()



ggplot(mpg)+geom_bar(aes(y = class))+facet_wrap( ~ year)+
  labs(
    title = "Number of car models per class",
    caption = "source:http://fueleconomy.gov",
    x = NULL, y = NULL
  )+ scale_x_continuous(expand = c(0, NA))+theme_minimal()+
  theme(
    text = element_text('Avenir Next Condensed'),
    strip.text = element_text(face = 'bold', hjust = 0),
    plot.caption = element_text(face = 'italic'),
    panel.grid.major= element_line('white', size = 0.5),
    panel.grid.minor=element_blank(),
    panel.grid.major.y = element_blank(),
    panel.ontop=TRUE
  )



# Saving the Plot

ggsave('plot.name.png',plot = last_plot,width = 9,height = 6,units = 'cm',dpi = 450)

