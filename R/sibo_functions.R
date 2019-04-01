### --------- Function List ------------#######


Notebook_new_sibo <- function() {
	   file.copy('C:\\Program Files\\R\\library\\aaasibo\\Sibo_file\\Notebook_sibo.Rmd',
				   getwd(),
				   overwrite = TRUE)
   shell.exec('Notebook_sibo.Rmd')
}


Rprofile_sibo  <- function()  {
            file.copy('C:\\Program Files\\R\\library\\aaasibo\\Sibo_file\\.Rprofile',getwd() )
}



wd_open_file <- function() {
  shell.exec( dirname(rstudioapi::getActiveDocumentContext()$path))
}

wd_set_file <- function() {
 setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}


write_csv_open <- function(dfg,csvfile)  {  write_csv(dfg,csvfile) ;    shell.exec(csvfile)     }


##------------------------ Sibo Theme --------------------------------##
## make new theme


tb2clip  <- function(tb)  {
  write.table(tb,"clipboard",sep="\t",row.names=FALSE)
  }


## Rstuido - 1. first build source package 2. build binary package 3. close the project


##------------- write csv with comments on the first row ---------------##
write_csv_comm <- function(dataset,filename,com='No Comments')  {
  head_com <- paste('#',comments)      # add '#' for NONMEM to ignore
  write(head_com ,filename)                   # add comments first
  write(colnames(dataset),filename,ncolumns=ncol(dataset),append = TRUE,sep=',' )
  write_csv(dataset,filename,append = TRUE, na='.')
}


read_csv_in_NM <- function(mod_path)  {
    if (!file.exists(mod_path) )   {winDialog( "ok", "Mod File Dose Not Exist") }
  ##------------------- find csv path inside mod --------------------##
  csv_name <- read_file(mod_path) %>% str_extract(., "FILE=\\S+[Cc][Ss][Vv]") %>% str_sub(., 6)
  csv_dir   <-  dirname(mod_path)
  csv_path <- paste0(csv_dir ,'/',  csv_name ) %>% print
   if (!file.exists(csv_path) )   {winDialog( "ok", "Csv File Dose Not Exist") }

  ##----------------- back CSV file ---------------------##
  file.copy(csv_path,paste0(getwd(),'/',csv_name ,'-backup', '.csv'))


  ##--------------- read csv sim/fit file from MOD ----------------------##
  return ( na.omit(as.data.frame(sapply(read_csv(csv_path,skip=1), as.numeric))))

}


outersect  <- function(x, y,side='default') {
  if (side=='left')     { cat ('only left:') ; print (setdiff(x, y))}
  if (side=='right')    { cat ('only right:') ; print (setdiff(y, x))}
  if (side=='Combined') { cat ('Combined:')  ; print(sort(c(setdiff(x, y), setdiff(y, x))))  }
  if (side=='default') {
    cat ('\n','only left:')  ; print (setdiff(x, y))
    cat ('\n','only right:') ; print (setdiff(y, x))
      }
}


para2vector <- function(para)   {
 		## remove space and line return
		para  <- para %>%  gsub(" ", "",., fixed = TRUE) %>%   gsub("\n", "",., fixed = TRUE)
		## convert to list
		para_rawlist <-   as.list(strsplit(para, ",")[[1]])
		## blank list variable to add thing on
		paralist <- list()
		## evaluate every element in the list
		for (i in 1:length(para_rawlist) )   {
		  newlist <- eval(parse(text=paste('list(', para_rawlist[[i]], ')')))  # great new list other than last
		  assign (  names( newlist)[1],newlist[[1]] )  # evaluate before
		  paralist <- append(paralist,eval(parse(text=paste('list(', para_rawlist[[i]], ')')))  ) 	}
		  new <-  unlist(paralist)
	return(new)
  }



para2list <- function(para)   {
  	  ## remove space and line return
	  para  <- para %>%  gsub(" ", "",., fixed = TRUE) %>%   gsub("\n", "",., fixed = TRUE)
	  # convert to list
	  para_rawlist <-   as.list(strsplit(para, ",")[[1]])
	  ## blank list variable to add thing on
	  paralist <- list()
	  ## evaluate every element in the list
	  for (i in 1:length(para_rawlist) )   {
		newlist <- eval(parse(text=paste('list(', para_rawlist[[i]], ')')))  # great new list other than last
		assign (  names( newlist)[1],newlist[[1]] )  # evaluate before
		paralist <- append(paralist,eval(parse(text=paste('list(', para_rawlist[[i]], ')')))  )    }
	return( paralist)
}


ifrm <- function(arg) {
  if (exists(as.character(substitute(arg)))) {
    rm(list=as.character(substitute(arg)), envir=sys.frame())
  }
}

  
exists_ask = function(x) {
  if(!exists(x)){
    print('Input Parameters Missing') ; stop()
  } 
}
  
