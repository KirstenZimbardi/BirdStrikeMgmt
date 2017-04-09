
# divide by 0
div <- function(x,y) ifelse(y==0,0,base:::"/"(x,y))

# remove leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


# Sentence Case
SentenceCase <- function(x) {
  s <- strsplit(x, " ")[[1]]
  sc = paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
  return(sc)
}




# remove all columns and rows that are completely NA
all.na = function(df) {
  ind = which(isTRUE(apply(df, 1, function(x) all(is.na(x)))))
  ind2 = which(is.na(apply(df, 1, function(x) all(x ==""))))
  ind.all = sort(c(ind, ind2))
  if(length(ind.all > 0)) {
    df = df[-ind.all,]
  }
  
  col = which(colSums(is.na(df)) == nrow(df))
  if(length(col > 0)) {
    df = df[,-col]
  }
  return(df)
} 

#remove duplicates
de.dup = function(df) {
  require(dplyr)
  dup = which(duplicated(df))
  if(length(dup > 0)) {
    df = df[-dup,]
  }
  return(df)
}

# fill down when values are missing but repeated - not working
fill.down = function(df, col) {
  m1 = which(df[,col] == "")
  m2 = which(df[,col] == " ")
  m3 = which(is.na(df[,col]))
  missing = c(m1, m2, m3)
  if(length(missing) > 0) {
    missing = sort(missing)
    missing.b4 = missing-1
    for(i in 1:length(missing)) {
      df[missing[i],col] = df[missing.b4[i],col]
  }
    }
  return(df[,col])
}



# rename columns
rename = function(df, cols, new.names) {
  n = names(df)
  n[cols] = new.names
  names(df) = n
  return(df)
}

# percentage
as.percent = function(df, col) {
  df[,col] = as.character(df[,col])
  df[,col] = paste0(df[,col],"%")
  return(df)
}

#DT customised

kz.dt = function(t, title = NULL, col1 = NULL, col2 = NULL, coln = NULL) {
  if(!is.null(title)) { h = paste0("<h2>", title, "</h2") }
  if(is.null(title)) { h = "<br/>" }
  if(is.null(col2)) { col1 = 0 }
  if(is.null(col2)) { col2 = ncol(t)-1 }
  return(datatable(t, caption = HTML(h), 
                   rownames = F, colnames = coln,
                   width = "90%", class = "cell-border stripe hover compact", 
                   options = list(
                     columnDefs = list(list(className = 'dt-center', targets = c(col1:col2))),
                     pageLength = 200 )
                   ) ) 
}

kz.dt.small = function(t, title = NULL, col1 = NULL, col2 = NULL, coln = NULL) {
  if(!is.null(title)) { h = paste0("<h2>", title, "</h2") }
  if(is.null(title)) { h = "<br/>" }
  if(is.null(col2)) { col1 = 0 }
  if(is.null(col2)) { col2 = ncol(t)-1 }
  return(datatable(t, caption = HTML(h), 
                   rownames = F, colnames = coln,
                   width = "70%", class = "cell-border stripe hover compact", 
                   options = list(
                     dom = 'ft',
                     columnDefs = list(list(className = 'dt-center', targets = c(col1:col2))),
                     pageLength = 50
                   ) ) )
}
