# combines outputs from multiple output folders from batch processing of CellProfiler pipeline
# two files should be present per sub-folder:
# image data, e.g Images.csv
# single-cell data, e.g Nuclei.csv (this should include merged output from different images; results in two lines of the header)

require(data.table)

# reads single-cell data with a single-line header
myFreadNuc = function(fileIn,   
                      s.cols.rm = c(
                        'Metadata_C',
                        'Metadata_ChannelName',
                        'Metadata_ColorFormat',
                        'Metadata_Frame',
                        'Metadata_Plate',
                        'Metadata_SizeC',
                        'Metadata_SizeT',
                        'Metadata_SizeX',
                        'Metadata_SizeY',
                        'Metadata_SizeZ',
                        'Metadata_Z'
                      )# list of typically unnecesary columns
                      ) {
  
  # read the output
  dt.loc = fread(fileIn)
  
  s.head = names(dt.loc)
  # remove duplicated columns
  dt.loc = dt.loc[, s.head[!duplicated(s.head)], with = FALSE]
  
  loc.s.cols = intersect(s.cols.rm, s.head)

  if (length(loc.s.cols) > 0)
    dt.loc[, (loc.s.cols) := NULL]
  
  return(dt.loc)
}

# reads single-cell data with a two-line header
myFreadNuc2LineHeader = function(fileIn,   
                      s.cols.rm = c(
                        'Metadata_C',
                        'Metadata_ChannelName',
                        'Metadata_ColorFormat',
                        'Metadata_Frame',
                        'Metadata_Plate',
                        'Metadata_SizeC',
                        'Metadata_SizeT',
                        'Metadata_SizeX',
                        'Metadata_SizeY',
                        'Metadata_SizeZ',
                        'Metadata_Z'
                      )# list of typically unnecesary columns
) {
  
  # Read the first two rows
  dt.head = fread(fileIn, nrows = 2, header = FALSE)
  
  # make a joint single-row header from two rows
  s.head = paste(dt.head[1], dt.head[2], sep = '_')
  
  # read the rest of the output (except first two rows)
  dt.loc = fread(fileIn, skip = 2)
  
  # set column names
  setnames(dt.loc, s.head)
  
  # remove duplicated columns
  dt.loc = dt.loc[, s.head[!duplicated(s.head)], with = FALSE]
  
  loc.s.cols = intersect(s.cols.rm, s.head)
  
  if (length(loc.s.cols) > 0)
    dt.loc[, (loc.s.cols) := NULL]
  
  return(dt.loc)
}

myFreadImg = function(fileIn,
                      s.cols.rem = c('Group_Index', 
                                     'Group_Number')) {
  
  dt.loc = fread(fileIn)
  
  # remove unnecessary columns
  loc.s.cols = intersect(s.cols.rem, names(dt.loc))
  
  if (length(loc.s.cols) > 0)
    dt.loc[, (loc.s.cols) := NULL]
  
  return(dt.loc)
}


# write files with combine output
myCombineOutput = function(s.files.nuc.core = 'Nuclei.csv', # define filenames with single-cell output
                           s.files.img.core = 'Image.csv', # define filenames with image data
                           s.dir.out = "output", # define directory with output
                           s.dir.out.mer = "output.mer", # define directory with merged output
                           header.2lines = FALSE # set TRUE if single-cell data has two header lines (happens when outputs from different images are in the single-cell data)
                           ) {
  # search subdirectories for csv files with Nuclei data
  s.files.nuc = list.files(
    path = paste0(s.dir.out, '/.'),
    pattern = s.files.nuc.core,
    recursive = TRUE,
    full.names = TRUE
  )
  
  s.files.img = list.files(
    path = paste0(s.dir.out, '/.'),
    pattern = s.files.img.core,
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Load single-cell files using custom file reading function
  # Deals with single-line and double-line headers
  if (header.2lines)
    dt.nuc = do.call(rbind, lapply(s.files.nuc, myFreadNuc2LineHeader))
  else
    dt.nuc = do.call(rbind, lapply(s.files.nuc, myFreadNuc))
  
  # Load image files using custom file reading function
  dt.img = do.call(rbind, lapply(s.files.img, myFreadImg))
  
  
  # Create directory for merged output in the currenty working directory
  ifelse(!dir.exists(file.path(".", s.dir.out.mer)), dir.create(file.path(".", s.dir.out.mer)), FALSE)
  
  write.csv(file = paste(s.dir.out.mer, s.files.nuc.core, sep = '/'), x = dt.nuc, row.names = FALSE)
  write.csv(file = paste(s.dir.out.mer, s.files.img.core, sep = '/'), x = dt.img, row.names = FALSE)
  
  return(NULL)
}


myCombineOutput(s.files.nuc.core = 'objCells.csv',
                s.files.img.core = 'Image.csv',
                header.2lines = TRUE)

