# load data
source('loadPackages.R')

subjects <- dir('data','*.dat') # raw files

# import text function
readRaw <- function(filename){
  fullName = file.path('.','data',filename)
  raw = fread(fullName)
  raw$sub = substr(filename,1,2)
  raw$session = substr(filename,3,3)
  raw
}
# import all data
raw_data <- do.call(rbind,lapply(subjects,readRaw))

# change variable names
names(raw_data) <- c('trialno','display','configration','tar_side','tar_position',
                     'dpos1','dpos2','dpos3',
                     'tar_color','tar_orientation','dori1','dori2','dori3',
                     'position_priming','color_priming','orientation_priming',
                     'rt','error','sub','session')
# make meaningful factors
raw_data$display <- factor(raw_data$display, labels = c('square','diamond'))
raw_data$tar_side <- factor(raw_data$tar_side,labels = c('vertical','left','right'))
raw_data$tar_color <- factor(raw_data$tar_color, labels = c('red','green'))
raw_data$tar_orientation <- factor(raw_data$tar_orientation,labels = c('top','bottom'))
raw_data$position_priming <- factor(raw_data$position_priming, labels = c('NA','TT','TD','TN'))
raw_data$color_priming <- factor(raw_data$color_priming, labels = c('same','different'))
raw_data$orientation_priming <- factor(raw_data$orientation_priming, labels = c('same','different'))

# make a mean RT plot
raw_data %>% filter(error == 0, position_priming != c('NA')) %>% # remove outliers, errors
  group_by(sub, position_priming) %>% summarise(rt = mean(rt)) %>% # average for individual subject, position priming cond
  group_by(position_priming) %>% summarise(mrt = mean(rt),sert = sd(rt)/sqrt(19)) %>% # collapse subjects
  ggplot(aes(x=position_priming, y= mrt, ymin = mrt-sert, ymax = mrt+sert, group=1)) + #plot error bars
    geom_point() + geom_errorbar(width =0.2) + geom_line() + 
    xlab('Position Priming') + ylab('Mean RTs (ms)')

