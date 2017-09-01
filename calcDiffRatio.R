require(gridExtra)
require(ggplot2)
require(data.table)
require(xtable)

####
## Processes data from differentiation experiments
## E.g. NS1 from Yannick
## Execute in cp.outXX folder


####
## Main parameters

# names of two files with paramaters for analysis and experiment
# these files are typically located one folder up of cp.outXX from which the script is executed
s.par.plot = '../plotFormat.xlsx'

# name for a column with merged condition names
s.par.treatconc.col = 'Metadata_TreatConc'

####
# Read params
l.par = myParRead(s.par.plot)

# Read plate map
s.par.exp = l.par$files.platemap
dt.exp = myExpRead(s.par.exp)

####
## Read data
# Load single-cell data
dt.nuc = fread(paste(l.par$dir.out, l.par$files.nuc, sep = '/'))

# load Image data
dt.im = fread(paste(l.par$dir.out, l.par$files.img, sep = '/'))

# assign column with image number (used for mergind image file with single-cell data)
# Sometimes, the column name might be different in Image and Nuclei file (important for merging)
s.par.im.imno = names(dt.im)[names(dt.im) %like% 'ImageNumber']
s.par.nuc.imno = names(dt.nuc)[names(dt.nuc) %like% 'ImageNumber']


# merge image data with plate map by metadata_well
dt.im = merge(dt.im, dt.exp, by = l.par$metadata.well)

# merge single-cell data with image data
dt.nuc = merge(dt.nuc, dt.im, by.x = s.par.nuc.imno, by.y = s.par.im.imno)


#####
## Process data

# assign column name with total neurite outgrowth per cell
s.par.meas.totlen = names(dt.nuc)[names(dt.nuc) %like% 'TotalNeuriteLength']

# assign column name with number of neurite outgrowths per cell
s.par.meas.nout = names(dt.nuc)[names(dt.nuc) %like% 'NumberTrunks']



# assign a (0,1) column, if total neurite outgrowth per cell is greater than 10
dt.nuc[, diff.01 := ifelse(get(s.par.meas.totlen) > l.par$n.diff.thresh, 1, 0)]

# calculate stats of total neurite outgrowth per condition
dt.nuc.aggr = dt.nuc[, .(l.mn  = mean(get(s.par.meas.totlen)),
                         l.md  = median(get(s.par.meas.totlen)),
                         l.min = min(get(s.par.meas.totlen)),
                         l.max = max(get(s.par.meas.totlen)),
                         n.mn  = mean(get(s.par.meas.nout)),
                         n.min = min(get(s.par.meas.nout)),
                         n.max = max(get(s.par.meas.nout)),
                         l.frac = sum(diff.01) / .N), by = .(Metadata_TreatAux, Metadata_Treat, Metadata_Conc)]



# create a column with merged condition columns
dt.nuc[, xxx := paste(Metadata_Treat, Metadata_Conc, sep = '_')]
setnames(dt.nuc, 'xxx', s.par.treatconc.col)

dt.nuc.aggr[, xxx := paste(Metadata_Treat, Metadata_Conc, sep = '_')]
setnames(dt.nuc.aggr, 'xxx', s.par.treatconc.col)


## Save processed single-cell data with experimental info
write.csv(dt.nuc, 'cellsProcessed.csv', row.names = FALSE)


## Plots
l.plots = list()

l.plots$fracDiffCells_perCond_barplot = ggplot(dt.nuc.aggr, aes_string(x = s.par.treatconc.col, y = 'l.frac')) +
  geom_col(aes(fill = Metadata_Treat)) +
  xlab('') +
  ylab(paste0('fraction of differentiated cells\n(total neurite outgrowth greater than ', l.par$n.diff.thresh, 'px)\n')) +
  facet_grid(~ Metadata_TreatAux) +
  scale_fill_discrete(name = '') +
  myGGtheme +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  guides(fill=guide_legend(
    keywidth=.4,
    keyheight=0.2,
    default.unit="inch")
  )


l.plots$totalNeuriteOutgrowth_perWell_boxplot = ggplot(dt.nuc, 
                                                       aes_string(x = l.par$metadata.well,
                                                                  y = s.par.meas.totlen)) +
  geom_boxplot(outlier.colour = NA) +
  coord_cartesian(ylim = c(0, 100)) +
  xlab('') +
  ylab('total neurite outgrowth [px]') +
  myGGtheme +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))


l.plots$totalNeuriteOutgrowth_perCond_boxplot = ggplot(dt.nuc, 
                                                       aes_string(x = s.par.treatconc.col,
                                                                  y = s.par.meas.totlen)) +
  geom_boxplot(aes(fill = Metadata_Treat), outlier.colour = NA) +
  coord_cartesian(ylim = c(0, 100)) +
  xlab('') +
  ylab('total neurite outgrowth [px]\n') +
  facet_grid(~ Metadata_TreatAux) +
  scale_fill_discrete(name = '') +
  myGGtheme +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1))





#####
## Save files
if (l.par$plot.save) {
  
  # Create directory for plots in the currenty working directory
  ifelse(!dir.exists(file.path(".", l.par$dir.plot)), dir.create(file.path(".", l.par$dir.plot)), FALSE)
  
  # all plots
  lapply(names(l.plots),
         function(x)
           ggsave(
             filename = paste0(l.par$dir.plot, '/', x, ".pdf"),
             plot = l.plots[[x]],
             width = l.par$plot.width, 
             height = l.par$plot.height
           ))
  
  # tables
  pdf(paste0(l.par$dir.plot, '/', "tab_neuriteStats_perCond.pdf"),
      width = 15,
      height = 20)

  grid.table(dt.nuc.aggr, 
             theme = ttheme_default(base_size = 12))
  dev.off()
}

