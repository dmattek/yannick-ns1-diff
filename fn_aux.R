require(data.table)
require(ggplot2)
require(xlsx)
require(grid)

# definition of custom colour palette
rhg_cols <- c(
  "#771C19",
  "#AA3929",
  "#E25033",
  "#F27314",
  "#F8A31B",
  "#E2C59F",
  "#B6C5CC",
  "#8E9CA3",
  "#556670",
  "#000000"
)

md_cols <- c(
  "#FFFFFF",
  "#F8A31B",
  "#F27314",
  "#E25033",
  "#AA3929",
  "#FFFFCC",
  "#C2E699",
  "#78C679",
  "#238443"
)

myCheckDigits <- function(x) {
  grepl('^[-]?[0-9]+[.]?[0-9]*$' , x)
}

myCheckLogical <- function(x) {
  grepl('^TRUE$|^FALSE$' , x)
}

myConvertStringListToTypes <- function(in.l) {
  # convert strings with digits to numeric
  # uses logical indexing: http://stackoverflow.com/questions/42207235/replace-list-elements-by-name-with-another-list
  loc.l = myCheckDigits(in.l)
  in.l[loc.l] = lapply(in.l[loc.l], as.numeric)
  
  # convert strings with TRUE/FALSE to logical
  loc.l = myCheckLogical(in.l)
  in.l[loc.l] = lapply(in.l[loc.l], as.logical)
  
  return(in.l)
}

# f-n to read experimental description
# returns data table
myExpRead = function(inFname, 
                     inCleanRowCol = TRUE, 
                     inCleanMissing = TRUE, 
                     inStartRow = 1, 
                     inSheetName = 1, 
                     inRowIndex = NULL) {
  # read the file
  loc.dt.exp = as.data.table(read.xlsx(
    file = inFname,
    sheetName = inSheetName,
    rowIndex = inRowIndex,
    startRow = inStartRow
  ))
  
  if(inCleanRowCol) {
    # sometimes an NA column appears at the end; remove
    loc.dt.exp = loc.dt.exp[, names(loc.dt.exp)[!(names(loc.dt.exp) %like% 'NA')], with = FALSE]
    
    # sometimes an NA row appears at the end; remove
    loc.dt.exp = loc.dt.exp[loc.dt.exp[,!Reduce(`&`, lapply(.SD, is.na))]]
  }
  
  if(inCleanMissing) {
    # replace missing values with ''
    for (i in seq_along(loc.dt.exp))
      set(loc.dt.exp,
          i = which(is.na(loc.dt.exp[[i]])),
          j = i,
          value = '')
  }
  
  return(loc.dt.exp)
}


####
## Read parameter file, e.g. plotFormat.xlsx
## The file contains 2 columns in the 1st sheet
## 1st column contains parameter names
## 2nd columns contains parameter values
## Returns list with parameters

myParRead = function(in.fname, in.cols = 1:2, in.sheet.idx = 1) {

  df.par = read.xlsx(
    in.fname,
    sheetIndex = in.sheet.idx,
    header = FALSE,
    as.data.frame = TRUE,
    colIndex = in.cols,
    colClasses = rep("character", 2),
    stringsAsFactors = FALSE
  )
  
  # convert data frame with parameters to a list 
  l.par = split(df.par[, 2], df.par[, 1])
  
  # convert strings with digits to numeric and strings with TRUE/FALSE to logical
  l.par = myConvertStringListToTypes(l.par)
  
  return(l.par)
}

myGGtheme =     theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.25),
    axis.line.y = element_line(color = "black", size = 0.25),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    strip.text.x = element_text(size = 14, face = "bold"),
    strip.text.y = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    legend.key = element_blank(),
    legend.key.height = unit(1, "lines"),
    legend.key.width = unit(2, "lines"),
    legend.position = "right"
  )
