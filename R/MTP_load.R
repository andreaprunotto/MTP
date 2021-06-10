#' Setup the Remote Driver (via docker)
#'@return The active remote driver
remDr_setup=function(){
	options(browser="firefox")
	# check windows/linux based on java.exe
	java_check=""
	tryCatch({java_check=suppressWarnings(system("java.exe -version", ignore.stdout = TRUE, ignore.stderr = TRUE))},error=function(e)e)

	if(java_check==127) # Linux
	{
		sele=nchar(system("sele=`docker ps --filter name= |grep selenium | awk '{print $1}'`;echo $sele",intern=TRUE))
		if(sele==0)
		{
			message("Starting the remote driver.")
			system("docker run -d -p 4444:4444 -v /dev/shm:/dev/shm selenium/standalone-firefox:3.141.59-20200409")
		}
		fprof=RSelenium::makeFirefoxProfile(list(
			"browser.cache.disk.enable"=FALSE,
			"browser.cache.memory.enable"=FALSE,
			"browser.cache.offline.enable"=FALSE,
			"moz:firefoxOptions"=list(args=list('--headless')),
			"network.http.use-cache"=FALSE))
		remDr=RSelenium::remoteDriver(remoteServerAddr="localhost",port=4444, browserName="firefox",version="87.0",extraCapabilities=fprof);
	}else
	{
		tryCatch(system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE),error=function(e) e)
		#Sys.setenv(JAVA_HOME='C:\\Users\\prunotto\\Downloads\\CommonFiles\\OpenJDKJRE64')
		java_home=system("where java",intern=TRUE)
		java_home=strsplit(java_home,split="bin")[[1]][1]
		Sys.setenv(JAVA_HOME=java_home)
		rD=RSelenium::rsDriver(port=4567L,verbose=FALSE,browser="firefox",extraCapabilities=list("moz:firefoxOptions"=list(args=list('--headless'))))
		remDr=rD$client
	}

	return(remDr)
}
	

#' Open the remote driver
#'@param remDr The active remote driver to be opened 
openDr=function(remDr){
	
	message("Opening the remote driver...")
	status="down"
    while(!is.null(status))
    {
    	#message("Opening the remote driver...")
    	tryCatch(remDr$quit(),error=function(e)e)
		tryCatch(remDr$open(silent=TRUE),error=function(e)e)
		status=tryCatch(remDr$navigate("https://www.google.com"),error=function(e)e)
	}
}

#' Close the remote driver
#'@param remDr The active remote driver to be closed 
closeDr=function(remDr){
	
	message("Closing the remote driver...")
	status=c("open")
    while(length(status)>0)
    {
    	status=tryCatch(remDr$quit(),error=function(e)e)
    }
}


#' Equivalent to RSelenium "findElements", but with tryCatch/timeout
#'@param remDr The active remote driver (string)
#'@param using A string, eg. "xpath", "css selector", see remDr$findElements
#'@param tag The webElem to identify (string)
#'@return result, the seeked webElemt (list), empty if not found/timeout
getElem=function(remDr,using,tag){
	elem=list()
	l=0
	td=0
	timeout=25
	t0=Sys.time()
	while(l==0&td<timeout)
	{
		tryCatch({
			td=round(as.numeric(difftime(Sys.time(),t0,units="sec")),0);
			elem=remDr$findElements(using=using,tag);
			},error=function(e)e)
		l=length(elem)
	}
	if(l>0)
	{
		result=elem[[1]]
	}else
	{
		result=list()
	}
	return(result)
}

#' Decode special characters
#'@param block Translated description block (list of strings)
#'@return block, with special characters
decode_special_chars = function(block){
	
	block=gsub("_00_","%",  block)
	block=gsub("_01_","&",  block)
	block=gsub("_02_","+",  block)
	block=gsub("_03_","/",  block)
	block=gsub("_04_",">",  block)
	block=gsub("_05_","<",  block)
	block=gsub("_06_",";",  block)
	block=gsub("_07_","#",  block)
	block=gsub("_08_","\\.",block)
	return(block)
}

#' Encode special characters
#'@param block Translated description block (list of strings)
#'@return block, without special characters
encode_special_chars = function(block){
	
	block=gsub("%",  "_00_",block)
	block=gsub("&",  "_01_",block)
	block=gsub("\\+","_02_",block)
	block=gsub("/",  "_03_",block)
	block=gsub(">",  "_04_",block)
	block=gsub("<",  "_05_",block)
	block=gsub(";",  "_06_",block)
	block=gsub("#",  "_07_",block)
	block=gsub("\\.","_08_",block)
	return(block)
}

#' First letter up
#'@param str_list A string
#'@return str_list, same string, with capitalized first letter
firstup=function(str_list){
  substr(str_list,1,1)=toupper(substr(str_list,1,1))
  str_list
}

#' Remove last string in between parentheses 
#'@param str_list A string to trim
#'@return str_list, same string, without the last sentence in between parenthesis (cut SNOMED CT hierarchical tag)
rmlastp=function(str_list){
	str_list=stringi::stri_replace_last(str=str_list,regex=' \\((.*?)\\)',replacement="")
	str_list
}

#' Remove consequent spaces and hyphens
#'@param str_list List of strings 
#'@return str_list without double spaces or hyphens 
rmsphyp=function(str_list){
	str_list=gsub("-"," ",str_list)
	str_list=gsub("^ *|(?<= ) | *$", "", str_list, perl = TRUE)
	str_list
}



#' Subdivide blocks to be submitted to MT
#'@param engine The engine to which submit the descriptions (string)
#'@param description A list of strings to be chunked in blocks
#'@return A data frame  in which the descriptions are chunked into blocks
get_blocks=function(engine,description){

	iden=c(description[,1])
	orig=c(description[,2])
	desc=stringr::str_conv(orig,"Latin1")

	keylength=0
	sep_list=list()
	sep_list[["deepl"]]="%0A"
	sep_list[["google"]]="\n"
	sep_list[["alternative_google"]]="\n"
	sep_list[["systran"]]="%0A"	
	sep_list[["translatecom"]]="\n"
	
	charlimit_list=list()
	charlimit_list[["deepl"]]=5000
	charlimit_list[["google"]]=5000
	charlimit_list[["alternative_google"]]=5000
	charlimit_list[["systran"]]=1000
	charlimit_list[["translatecom"]]=1000

	charlimit=charlimit_list[[engine]]
	seplength=nchar(encode_special_chars(stringr::str_conv(sep_list[[engine]],"Latin1")))
	maxchar=charlimit-keylength
	ncharperline=nchar(encode_special_chars(desc))+seplength

	block_beg=list()
	block_end=list()

	sum=0
	block_beg[[1]]=1
	nob=1
	for(l in 1:length(ncharperline))
	{
		sum=sum+ncharperline[l]
		if(sum<maxchar)
		{
			block_end[[nob]]=l
		}
		else
		{
			nob=nob+1
			block_beg[[nob]]=l
			sum=ncharperline[l]
		}
	}
	block_end[[nob]]=length(ncharperline)

	block_begs=do.call(rbind,block_beg)
	block_ends=do.call(rbind,block_end)
	block_lims=cbind.data.frame(begin=block_begs,end=block_ends)


	blocks=list()
	idents=list()
	for(b in 1:nob)
	{
		blocks[[b]]=cbind.data.frame(
			block_id=b,
			block_length=block_lims[b,]$end-block_lims[b,]$begin+1,
			description_id=iden[block_lims$begin[b]:block_lims$end[b]],
			description=orig[block_lims$begin[b]:block_lims$end[b]]
			)
	}

	blocks_info=do.call(rbind,blocks)
	return(blocks_info)
}



#' Sample plan list to minimize consecutive MT engines queries
#'@param plan A translation plan (data frame) 
#'@param engine_field A string, either "support_target_engine" or "source_support_engine"
#'@return Same plan, but with shuffled rownames to reduce consecutive MT engines queries
sample_engines=function(plan,engine_field){

	if(nrow(plan)>0)
	{
		wait=FALSE
		max_wait=5
		max_cons=50
		index=rownames(plan)

		skip=FALSE
		test_engines=unique(plan[,colnames(plan)==engine_field])
		if(length(test_engines)==1)
		{
			if(test_engines=="google"|test_engines=="alternative_google")
			{
				skip=TRUE
			}
		}

		if(skip==FALSE)
		{
			min_cons=1000

			while(wait==FALSE)
			{
				#print(max_cons)
				consec_blocks=100
				t0=Sys.time()
				td=0
				while(consec_blocks>max_cons & td<max_wait)
				{
					td=round(as.numeric(difftime(Sys.time(),t0,units="sec")),0)
					#print(td)
					index=sample(rownames(plan))
					plan=plan[index,]
					c0=rle(plan[,colnames(plan)==engine_field])
					c1=cbind.data.frame(engine=c0$values,consec_blocks=c0$lengths)
					#consec_blocks=max(c1$consec_blocks)
					consec_blocks=max(c1[c1$engine%in%c("deepl","systran","translatecom"),]$consec_blocks)
					if(consec_blocks<min_cons)
					{
						min_cons=consec_blocks
						min_index=index
					}
				}
				max_cons=max_cons-1
				if(td>=max_wait){wait=TRUE}
			}
		}else
		{
			min_index=sample(index)
		}
	}else
	{
		min_index=NULL
	}
	return(min_index)
}

#' Define the TPS as a function of the selected languages and engines
#'@param mtp_directory Working directory (string)
#'@param project_name Translation project name (string)
#'@param input_file_name File name, with path, containing the original descriptions (string)
#'@param text_identifier_field_name Name of the field containing the description ids (string)
#'@param source_languages A list of string with language ids, e.g. c("en","es","sv")
#'@param source_support_engines A list of string with MT tools names, e.g. c("google","deepl")
#'@param support_languages A list of string with language ids, e.g. c("en","es","sv")
#'@param support_target_engines A list of string with MT tools names, e.g. c("google","deepl")
#'@param target_language Target languange (string)
#'@return A data frame containing all the possible TPS
generate_tps=function(
	mtp_directory,
	project_name,
	input_file_name,
	text_identifier_field_name,
	source_languages,
	source_support_engines,
	support_languages,
	support_target_engines,
	target_language){

	options(stringsAsFactors=FALSE, showWarnings=FALSE)

	########################################################
	# Create essential directories for the current project #
	########################################################

	project_directory=paste(mtp_directory,"/translation_projects/",project_name,sep="")
	dir.create(paste(project_directory,"/direct_translations",sep=""),recursive=TRUE,showWarnings=FALSE)
	dir.create(paste(project_directory,"/support_translations",sep=""),recursive=TRUE,showWarnings=FALSE)
	dir.create(paste(project_directory,"/final_translations",sep=""),recursive=TRUE,showWarnings=FALSE)

	########################################################
	# Generate the file containing the translation paths  ##
	########################################################
	
	comb_tps=as.data.frame(tidyr::crossing(source_languages,source_support_engines,support_languages,support_target_engines,target_language))
	# remove source==supl
	comb_tps=comb_tps[comb_tps$source_languages!=comb_tps$support_languages,]
	rownames(comb_tps)=c(1:nrow(comb_tps))
	comb_tps$doable=TRUE

	# Remove deepl unavailable tps

 	deepl_languages=c(
 		"bg","zh","cs","da","nl","en","et","fi",
 		"fr","de","el","hu","it","ja","lv","lt",
 		"pl","pt","ro","ru","sk","sl","es","sv")

	deepl_index=(comb_tps$source_support_engines=="deepl"|comb_tps$support_target_engines=="deepl") & 
	(!comb_tps$source_languages%in%deepl_languages|!comb_tps$support_languages%in%deepl_languages)

	comb_tps$doable[deepl_index]=FALSE

	# Systran available tps

	systran_sl=c('sq','sq','sq','ar','ar','ar','ar','bn','bg','bg','bg','my','ca','ca','ca','zh','zh','zh','zh','zh','hr','hr','hr','cs','cs','cs','cs','da','da','da','da','nl','nl','nl','nl','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','en','et','et','et','et','fi','fi','fi','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','fr','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','de','el','el','el','el','el','he','he','he','hi','hi','hi','hi','hu','hu','hu','hu','id','id','id','it','it','it','it','it','ja','ja','ja','ja','ja','ja','ko','ko','ko','ko','ko','lv','lv','lv','lt','lt','lt','ms','ms','ms','no','no','no','no','fa','fa','fa','pl','pl','pl','pt','pt','pt','ro','ro','ro','ru','ru','ru','ru','ru','sr','sr','sk','sk','sk','sk','sl','sl','sl','sl','so','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','es','sv','sv','sv','sv','tl','ta','th','th','tr','tr','tr','uk','uk','uk','uk','uk','ur','ur','ur','vi','vi','vi')
	systran_tl=c('en','es','fr','fr','en','de','es','en','fr','en','es','en','es','fr','en','en','de','es','ja','fr','en','es','fr','de','fr','en','es','de','en','es','fr','de','fr','es','en','sq','nl','it','pt','ar','zh','lv','ro','bn','bg','th','et','vi','my','ca','fi','sr','hi','de','ur','sk','el','lt','ko','sl','he','no','so','hr','uk','fa','ta','cs','hu','pl','ms','da','id','fr','sv','tr','tl','ja','es','ru','ps','de','fr','en','es','en','fr','es','nl','en','hi','th','zh','et','uk','sq','ca','sr','bg','hr','no','lv','ar','lt','ur','de','sl','he','ko','id','fi','pt','fa','sv','ro','sk','pl','hu','tr','da','it','ms','el','es','ja','cs','ru','vi','zh','ar','lt','en','fr','he','no','bg','fi','hi','fa','ko','sv','pt','pl','hr','th','ca','uk','ur','ms','tr','nl','vi','hu','ro','lv','sq','ja','it','es','ru','da','el','sk','cs','sl','et','es','en','de','it','fr','en','fr','es','de','es','en','fr','es','de','en','fr','fr','en','es','es','fr','el','de','en','zh','ko','de','es','en','fr','fr','ja','de','es','en','en','fr','es','fr','es','en','en','de','fr','en','de','es','fr','fr','en','es','es','fr','en','en','es','fr','en','es','fr','fr','de','es','uk','en','fr','en','en','es','fr','de','de','es','fr','en','en','ar','el','ca','bg','uk','fa','lv','he','ro','hi','ur','no','pt','sk','vi','hu','lt','fi','en','it','cs','hr','sv','th','nl','et','ru','sq','sl','de','ko','ja','tr','da','fr','ms','pl','fr','es','de','en','en','en','en','fr','en','fr','es','en','es','de','fr','ru','en','es','fr','en','fr','es')
	systran_tps=cbind.data.frame(input_lang=systran_sl,engine="systran",output_lang=systran_tl)

	for(l in 1:nrow(comb_tps))
	{
		f=TRUE
		f1=TRUE 
		f2=TRUE
		temp=comb_tps[l,]
		if(temp$source_support_engines=="systran")
		{
			comb1=paste(temp$source_languages,temp$support_languages,sep="-")
			syst=paste(systran_tps[,1],systran_tps[,3],sep="-")
			f1=comb1%in%syst
		}
		if(temp$support_target_engines=="systran")
		{
			if(temp$support_languages!=temp$target_language)
			{
				comb2=paste(temp$support_languages,temp$target_language,sep="-")
				syst=paste(systran_tps[,1],systran_tps[,3],sep="-")
				f2=comb2%in%syst
			}
		}
		f=f1&f2
		comb_tps[l,]$doable=comb_tps[l,]$doable&f
	}

	comb_tps=comb_tps[comb_tps$doable,c(1:5)]
	tps=cbind.data.frame(tps=do.call(paste, c(comb_tps, sep="-")))

	###############################
	# Save tps for inspection     #
	###############################


	tps_file_name=paste(project_directory,"/",project_name,"_tps.tab",sep="")
	write.table(tps,tps_file_name,sep="\t",row.names=FALSE,col.names=TRUE,quote=FALSE)

	return(tps)

}

#' Google translate function
#'@param sl Source language (string)
#'@param tl Target language (string)
#'@param send List of string to be translated
#'@param remDr The active remote driver
#'@return A data frame containing the translations of send, plus a checktag "OK" or "KO" related to a successful translation
google_translate=function(sl,tl,send,remDr){

	url="https://consent.google.com/m?continue=https://translate.google.com/&gl=DE&m=0&pc=t&hl=en-US&src=1"
	#agree=list()
	#while(length(agree)==0)
	#{
		remDr$navigate(url)
		#agree=getElem(remDr,"xpath","/html/body/div/c-wiz/div/div/div/div[2]/div[1]/div[4]/form/div[1]/div/button")
		agree=getElem(remDr,"css selector","VfPpkd-vQzf8d")
		Sys.sleep(3)
		agree=getElem(remDr,"xpath","/html/body/c-wiz/div/div/div/div[2]/div[1]/div[4]/form/div[1]/div/button")

	#}
	tryCatch(agree$clickElement(),error=function(e)e)
	Sys.sleep(2)
	
	url=URLencode(paste("https://translate.google.com/?sl=",sl,"&tl=",tl,"&op=translate&text=",paste(send,collapse="\n"),sep=""))
	remDr$navigate(url)
	output_area=getElem(remDr,"css selector",".J0lOec")
		
	if(length(output_area)>0)
	{
		translation=strsplit(output_area$getElementText()[[1]],split="\n")[[1]]
		translation=translation[-length(translation)]
		result=cbind.data.frame(translation=translation,status="OK")

		# Try this if the separator didn't work  
		if(nrow(result)!=length(send))
		{
			temp0=getElem(remDr,"css selector",".J0lOec")
			temp1=temp0$getPageSource()[[1]]
			temp2=strsplit(temp1,split='text=')[[1]]
			temp3=strsplit(temp2[8],split="%0A")[[1]]
			temp4=URLdecode(paste(temp3,collapse="\n"))
			temp5=strsplit(temp4,split='\"><div')[[1]][1]
			temp6=strsplit(temp5,split="\n")[[1]]
			result=cbind.data.frame(translation=temp6,status="OK")
		}

	}else
	{
		result=cbind.data.frame(translation=send,status="KO")
	}

	return(result)
}





#' Systran translate function
#'@param sl Source language (string)
#'@param tl Target language (string)
#'@param send List of string to be translated
#'@param remDr The actuve remote driver
#'@return A data frame containing the translations of send, plus a checktag "OK" or "KO" related to a successful translation
systran_translate=function(sl,tl,send,remDr){

	url=URLencode(paste("https://translate.systran.net/translationTools/text?source=",sl,"&target=",tl,sep=""))
	remDr$navigate(url)

	input_area=getElem(remDr,"css selector","textarea.form-control")
	if(length(input_area)>0)
	{
		input_area$sendKeysToElement(list(paste(send,collapse="\n"),key="enter"))
		# get Elem, with reduced timeout
		elem=list()
		l=0
		td=0
		timeout=3
		t0=Sys.time()
		while(l==0&td<timeout)
		{
			tryCatch({
				td=round(as.numeric(difftime(Sys.time(),t0,units="sec")),0);
				elem=remDr$findElements(using="css selector",".uiMessage > span:nth-child(3)");
				},error=function(e)e)
			l=length(elem)
		}
		if(l>0)
		{
			error_lang=elem[[1]]
		}else
		{
			error_lang=list()
		}

		if(length(error_lang)==0)
		{
			output_area=getElem(remDr,"css selector","#translateContent")
			if(length(output_area)>0)
			{
				translation=strsplit(output_area$getElementText()[[1]],split="\n")[[1]]
				if(length(translation)==length(send))
				{
					result=cbind.data.frame(translation=translation,status="OK")
				}else
				{
					result=cbind.data.frame(translation=send,status="KO")
				}
			}else
			{
				result=cbind.data.frame(translation=send,status="KO")
			}
		}else
		{
			result=cbind.data.frame(translation=send,status="KO")
		}
	}else
	{
		result=cbind.data.frame(translation=send,status="KO")
	}
	return(result)
}


#' Deepl translate function
#'@param sl Source language (string)
#'@param tl Target language (string)
#'@param send List of string to be translated
#'@param remDr The actuve remote driver
#'@return A data frame containing the translations of send, plus a checktag "OK" or "KO" related to a successful translation
deepl_translate=function(sl,tl,send,remDr){

	url=URLencode(paste("https://www.deepl.com/translator#",sl,"/",tl,"/",paste(send,collapse="%0A"),sep=""))
	remDr$navigate(url)
	timeout=20
	t0=Sys.time()
	wait=0
	l=0
	output_area=list()
	while(l==0&wait<timeout)
	{
		wait=round(as.numeric(difftime(Sys.time(),t0,units="sec")),0)
		tryCatch({
			output_area=remDr$findElements(using="css selector",".lmt__target_textarea");
			translation=strsplit(output_area[[1]]$getElementAttribute("value")[[1]],split="\n")[[1]];
				},error=function(e) e)
		l=length(translation)==length(send)
	}
	if(l>0)
	{
		result=cbind.data.frame(translation=translation,status="OK")
	}else
	{
		result=cbind.data.frame(translation=send,status="KO")
	}
	return(result)
}


#' Translate function
#'@param engine The MT engine (string)
#'@param sl Source language (string)
#'@param tl Target language (string)
#'@param send List of string to be translated
#'@param remDr The actuve remote driver
#'@return A data frame containing the translations of send, plus a checktag "OK" or "KO" related to a successful translation, and restored special characters
translate=function(engine,sl,tl,send,remDr){

	temp=encode_special_chars(send)
	if(engine=="google")
	{
		#google_agree()
		result=google_translate(sl,tl,temp,remDr)
	}
	if(engine=="deepl")
	{
		result=deepl_translate(sl,tl,temp,remDr)
	}
	if(engine=="systran")
	{
		result=systran_translate(sl,tl,temp,remDr)
	}
	result$translation=decode_special_chars(result$translation)
	return(result)
}



#' Subdivide source-engine tps in blocks 
#'@param mtp_directory Working directory (string)
#'@param project_name Translation project name (string)
#'@param input_file_name File name, with path, containing the original descriptions (string)
#'@param text_identifier_field_name Field of the input file containing the decription ids (string)
#'@param tps Data frame of available translation paths (data frame)
#'@return blocks_file_name, which contains the blocks to be translated (source-support) and the translation plan (plan_file)
generate_source_support_blocks=function(mtp_directory,project_name,input_file_name,text_identifier_field_name,tps){
	
	project_directory=paste(mtp_directory,"/translation_projects/",project_name,sep="")
	        blocks_file_name=paste(project_directory,"/direct_translations/",project_name,"_direct_blocks.tab",sep="")
	               plan_file=paste(project_directory,"/direct_translations/",project_name,"_direct_translations_plan.tab",sep="")
	direct_translations_file=paste(project_directory,"/direct_translations/",project_name,"_direct_translations.tab",sep="")

	if(!file.exists(blocks_file_name)|!file.exists(plan_file))
	{
		source_tps=as.data.frame(unique(do.call(rbind,strsplit(tps$tps,split="-"))[,1:2]))
		colnames(source_tps)=c("source_languages","source_support_engines")
		source_languages=unique(source_tps$source_languages)

		support_tps=as.data.frame(unique(do.call(rbind,strsplit(tps$tps,split="-"))[,3:4]))
		colnames(support_tps)=c("support_languages","support_target_engines")


		inpu=read.table(input_file_name,sep="\t",header=TRUE,comment.char="",quote="",colClasses=c("character"))
		id_col=which(colnames(inpu)==text_identifier_field_name)
		language_cols=which(colnames(inpu)%in%source_languages)

		index=colnames(inpu)=="en"
		inpu[,index]=trimws(firstup(rmlastp(inpu[,index])))
		index=colnames(inpu)=="es"
		inpu[,index]=trimws(firstup(rmlastp(inpu[,index])))
		index=colnames(inpu)=="sv"
		inpu[,index]=trimws(firstup(        inpu[,index] ))

		#inpu=head(inpu,10)

		# get descriptions for all available sources
		s=1
		source_blocks_list=list()
		message("Generating direct translation blocks... (may take up to 3 minutes for 350,000 descriptions)")
		for(source in unique(source_tps$source_languages))
		{
			desc=inpu[,colnames(inpu)==text_identifier_field_name|colnames(inpu)==source]
			for(engine in unique(source_tps$source_support_engines))
			{
				source_blocks_list[[s]]=cbind.data.frame(
					source_language=source,
					source_support_engine=engine,
					get_blocks(engine,desc))
				s=s+1
			}
		}
		source_blocks=do.call(rbind,source_blocks_list)

		
		write.table(source_blocks,blocks_file_name,sep="\t",row.names=FALSE,col.names=TRUE,quote=FALSE)

		source_blocks_plan=unique(source_blocks[,c(1:3)])
		

		# each available support (from tps) will have the same structure
		comp=unique(as.data.frame(do.call(rbind,strsplit(tps$tps,split="-"))[,c(1:3)]))
		colnames(comp)=c("source_language","source_support_engine","support_language")
		temp=merge(comp,source_blocks_plan,by=c("source_language","source_support_engine"),all.x=TRUE)
		source_support_blocks_plan=temp

		message("Generating first direct translation plan... (may take up to 5 minutes for 350,000 descriptions)")

		min_index=sample_engines(source_support_blocks_plan,"source_support_engine")
		if(!is.null(min_index))
		{
			source_support_blocks_plan=source_support_blocks_plan[min_index,]
			source_support_blocks_plan$block_id_unique=c(1:nrow(source_support_blocks_plan))
			write.table(source_support_blocks_plan,plan_file,sep="\t",row.names=FALSE,col.names=TRUE,quote=FALSE)
		}else
		{
			message("Translations completed!")
		}
	}else
	{
		message("A source-support block/plan file for this project already exists. Run translate_source_support_blocks.")
	}
}


#' Subdivide support-engine tps in blocks 
#'@param mtp_directory Working directory (string)
#'@param project_name Translation project name (string)
#'@param tps Data frame of available translation paths (data frame)
#'@param target_language Target languange (string)
#'@return blocks_file_name, which contains the blocks to be translated (support-target) and the translation plan (plan_file)
generate_support_target_blocks=function(mtp_directory,project_name,tps,target_language){

	message("Reading direct translations...")
	project_directory=paste(mtp_directory,"/translation_projects/",project_name,sep="")
	direct_translations_file=paste(project_directory,"/direct_translations/", project_name,"_direct_translations.tab",sep="")
	        blocks_file_name=paste(project_directory,"/support_translations/",project_name,"_support_blocks.tab",sep="")
   support_translations_file=paste(project_directory,"/support_translations/",project_name,"_support_translations.tab",sep="")
	               plan_file=paste(project_directory,"/support_translations/",project_name,"_support_translations_plan.tab",sep="")

	if(!file.exists(blocks_file_name)&!file.exists(plan_file))
	{

		support_tps=as.data.frame(unique(do.call(rbind,strsplit(tps$tps,split="-"))[,3:4]))
		colnames(support_tps)=c("support_languages","support_target_engines")


		f_supp=function(x){paste(x,collapse="\t")}
		g_supp=function(x)(as.data.frame(table(trimws(firstup(rmsphyp(x))))))
		#g=function(x)(as.data.frame(table(x)))

		temp=read.table(direct_translations_file,sep="\t",header=TRUE,comment.char="",quote="",colClasses=c("character"))

		aggr=aggregate(support_description~description_id+support_language,data=temp,f_supp)

		#message("Evaluating common descriptions... (may take up to 3 minutes for 350,000 descriptions)")
		dire_freq=list()
		dire_resu=list()
		for(support in unique(aggr$support_language))
		{
			writeLines(paste("Evaluating common descriptions for support language ",support," ...",sep=""))
			ids=unique(aggr[aggr$support_language==support,1])
			l=strsplit(aggr[aggr$support_language==support,3],split="\t")
			m=lapply(l,g_supp)
			names(m)=ids
			df=cbind.data.frame(do.call(rbind,m))
			df$description_id=do.call(rbind,strsplit(rownames(df),split="\\."))[,1]
			df$support_language=support
			dire_resu[[support]]=df
		}
		message("Integrating common descriptions...")
		tosupp0=do.call(rbind,dire_resu)
		tosupp0$support_description_id=rownames(tosupp0)
		# get only tps combinations of support language and engines
		tosupp=merge(tosupp0,support_tps,by.x="support_language",by.y="support_languages")


		dire=tosupp
	
		
		message("Generating support translation blocks... (may take up to 3 minutes for 350,000 descriptions)")

		s=1
		support_target_block_list=list()
		for(support in unique(tosupp$support_language))
		{
			for(engine in unique(tosupp$support_target_engines))
			{
				index=(tosupp$support_language==support) & (tosupp$support_target_engines==engine)
				if(nrow(tosupp[index,])>0) # i.e. if the combination support lang - engine is available
				{
					desc=tosupp[index,c(5,2)]
					desc[,2]=as.character(desc[,2])
					freq=tosupp[index,]$Freq
					orig=tosupp[index,]$description_id

					support_target_block_list[[s]]=cbind.data.frame(
						dire_freq=freq,
						original_description_id=orig,
						support_language=support,
						support_target_engine=engine,
						get_blocks(engine,desc)
						)
				s=s+1
				}
			}
		}
		support_target_blocks=do.call(rbind,support_target_block_list)

		write.table(support_target_blocks,blocks_file_name,sep="\t",row.names=FALSE,col.names=TRUE,quote=FALSE)

		support_blocks_plan=unique(support_target_blocks[,c(3:5)])
		support_target_blocks_plan=cbind.data.frame(support_blocks_plan,target_language=target_language)
		
		message("Generating first support translation plan... (may take up to 5 minutes for 350,000 descriptions)")

		min_index=sample_engines(support_target_blocks_plan,"support_target_engine")
		if(!is.null(min_index))
		{
			support_target_blocks_plan=support_target_blocks_plan[min_index,]
			support_target_blocks_plan$block_id_unique=c(1:nrow(support_target_blocks_plan))
			write.table(support_target_blocks_plan,plan_file,sep="\t",row.names=FALSE,col.names=TRUE,quote=FALSE)
		}else
		{
			message("Translations completed!")
		}
	}else
	{
		message("A support-target block/plan file for this project already exists. Run translate_support_target_blocks.")
	}
}



#' Translate source-engine tps in blocks 
#'@param mtp_directory Working directory (string)
#'@param project_name Translation project name (string)
#'@param reftime Refreshing time, numeric. Default 60 (seconds)
#'@param refsleep Sleeping time, numeric. Default 60 (seconds)
#'@param timeout_translations Default 20 seconds
#'@param remDr The active remote driver
#'@return A data frame with appending direct translations
translate_source_support_blocks=function(mtp_directory,project_name,reftime,refsleep,timeout_translations,remDr){
	message("Reading files... ")
	project_directory=paste(mtp_directory,"/translation_projects/",project_name,sep="")
	        blocks_file_name=paste(project_directory,"/direct_translations/",project_name,"_direct_blocks.tab",sep="")
	direct_translations_file=paste(project_directory,"/direct_translations/",project_name,"_direct_translations.tab",sep="")
	          plan_file_name=paste(project_directory,"/direct_translations/",project_name,"_direct_translations_plan.tab",sep="")
	blocks=read.table(blocks_file_name,sep="\t",header=TRUE,quote="",colClasses="character")

	message("Evaluating actual translation plan... ")
	original_plan=read.table(plan_file_name,sep="\t",header=TRUE,quote="",colClasses="character")


	min_index=rownames(original_plan)

	while(!is.null(min_index))
	{
		if(file.exists(direct_translations_file))
		{
			rest=read.table(direct_translations_file,sep="\t",header=TRUE,quote="",colClasses="character")
			nupl=rest[!duplicated(rest),]
			if(!identical(nupl,rest))
			{
				tag=gsub(":| ","-",Sys.time())
				write.table(rest,file=paste(direct_translations_file,".",tag,".back",sep=""),col.names=TRUE,row.names=FALSE,quote=FALSE,sep="\t")
				write.table(nupl,file=direct_translations_file,col.names=TRUE,row.names=FALSE,quote=FALSE,sep="\t")
				rest=nupl
			}
			actual_plan=original_plan[!(original_plan$block_id_unique%in%rest$block_id_unique),]
		}else
		{
			actual_plan=original_plan
		}

		# ignore impossible paths
		original_plan=original_plan[!(original_plan$source_language=="pt" & original_plan$source_support_engine=="systran" & original_plan$support_language=="de"),]
		original_plan=original_plan[!(original_plan$source_language=="pl" & original_plan$source_support_engine=="systran" & original_plan$support_language=="de"),]
		
		actual_plan=actual_plan[!(actual_plan$source_language=="pt" & actual_plan$source_support_engine=="systran" & actual_plan$support_language=="de"),]
		actual_plan=actual_plan[!(actual_plan$source_language=="pl" & actual_plan$source_support_engine=="systran" & actual_plan$support_language=="de"),]

		nblocks_original=nrow(original_plan)
		nblocks_actual=nrow(actual_plan)


		openDr(remDr)

		t0=Sys.time()
		min_index=sample_engines(actual_plan,"source_support_engine")
		if(!is.null(min_index))
		{
		bcounter=1
		for(block in actual_plan[min_index,]$block_id_uniq)
		{
			plan_supp=actual_plan[actual_plan$block_id_unique==block,]
			td=round(as.numeric(difftime(Sys.time(),t0,units="sec")),0)

			if(nrow(plan_supp)>0)
			{
				td=round(as.numeric(difftime(Sys.time(),t0,units="sec")),0)
				if(td>reftime)
				{
					writeLines("rD refresh...")
					closeDr(remDr)
					Sys.sleep(refsleep)
					gc()
					openDr(remDr)
					t0=Sys.time()
					td=0
				}

				source=plan_supp$source_language
				engine=plan_supp$source_support_engine
				support=plan_supp$support_language
				block_id=plan_supp$block_id

				send=blocks[blocks$source_language==source & blocks$source_support_engine==engine & blocks$block_id==block_id,]$description
				sids=blocks[blocks$source_language==source & blocks$source_support_engine==engine & blocks$block_id==block_id,]$description_id

				ctp=paste(source,engine,support,sep="-")
				#writeLines(paste("translating ",ctp, " ...",sep=""))
				#writeLines(paste(Sys.time()," translating block ",bcounter,":  ",nrow(original_plan)-nrow(actual_plan)-bcounter+1,"of ", nrow(actual_plan)," [",source,"-",engine,"-",support,"] (",length(send)," terms)",sep=""))
				mess1=paste(Sys.time()," translating: ",length(send)," terms",sep="")
				mess2=paste("tp: [",source,"-",engine,"-",support,"]",sep="")
				mess3=paste("block: ",bcounter," [",nrow(original_plan)-nrow(actual_plan)+bcounter," of ", nrow(original_plan),"]",sep="")
				writeLines(paste(mess1,mess2,mess3,sep="\t"))
				tran=list()
				
				tryCatch({tran=R.utils::withTimeout(translate(engine,source,support,send,remDr),timeout=timeout_translations)},error=function(e) e)
				
				if(length(tran)>0)
				{
					if(!any(tran$status=="KO"))
					{
						if(length(tran$translation)==length(send))
						{
							temp=cbind.data.frame(
								description_id=sids,
								block_id_unique=block,
								source_description=send,
								source_language=source,
								source_support_engine=engine,
								support_language=support,
								support_description=tran$translation
								)
							if(file.exists(direct_translations_file))
							{
								write.table(temp,file=direct_translations_file,col.names=FALSE,row.names=FALSE,quote=FALSE,sep="\t",append=TRUE)
							}else
							{
								write.table(temp,file=direct_translations_file,col.names=TRUE,row.names=FALSE,quote=FALSE,sep="\t")
							}
							bcounter=bcounter+1
						}else
						{
							message("failed (translation errors)")
						} # tranlsation failed
					}else
					{
						message("failed (translation engine blocked/timed out)")
					}
				}else
				{
					message("failed (did not get the translation)")
				} # tranlsation failed	
			} # supp lang is completed	
		} # supp lang cycle 
		}else
		{
			message("Translations completed!")
		}
	}
	#message("Translations completed!")
	closeDr(remDr)
}


#' Translate support engines tps in blocks 
#'@param mtp_directory Working directory (string)
#'@param project_name Translation project name (string)
#'@param reftime Refreshing time, numeric. Default 60 (seconds)
#'@param refsleep Sleeping time, numeric. Default 60 (seconds)
#'@param timeout_translations Default 20 seconds
#'@param remDr The active remote driver
#'@return support_translations_file tcs A data frame with the TCs and the related score
#'@return A data frame with appending support translations
translate_support_target_blocks=function(mtp_directory,project_name,reftime,refsleep,timeout_translations,remDr){

	project_directory=paste(mtp_directory,"/translation_projects/",project_name,sep="")
	         blocks_file_name=paste(project_directory,"/support_translations/",project_name,"_support_blocks.tab",sep="")
	support_translations_file=paste(project_directory,"/support_translations/",project_name,"_support_translations.tab",sep="")
	           plan_file_name=paste(project_directory,"/support_translations/",project_name,"_support_translations_plan.tab",sep="")
	blocks=read.table(blocks_file_name,sep="\t",header=TRUE,quote="",colClasses="character")
	original_plan=read.table(plan_file_name,sep="\t",header=TRUE,quote="",colClasses="character")

	min_index=rownames(original_plan)
	message("Evaluating actual translation plan... ")
	while(!is.null(min_index))
	{
		if(file.exists(support_translations_file))
		{
			rest=read.table(support_translations_file,sep="\t",header=TRUE,quote="",colClasses="character")
			nupl=rest[!duplicated(rest),]
			if(!identical(nupl,rest))
			{
				tag=gsub(":| ","-",Sys.time())
				write.table(rest,file=paste(support_translations_file,".",tag,".back",sep=""),col.names=TRUE,row.names=FALSE,quote=FALSE,sep="\t")
				write.table(nupl,file=support_translations_file,col.names=TRUE,row.names=FALSE,quote=FALSE,sep="\t")
				rest=nupl
			}
			actual_plan=original_plan[!(original_plan$block_id_unique%in%rest$block_id_unique),]
		}else
		{
			actual_plan=original_plan
		}

		# ignore impossible paths
		original_plan=original_plan[!(original_plan$support_language=="pt" & original_plan$support_target_engine=="systran" & original_plan$target_language=="de"),]
		original_plan=original_plan[!(original_plan$support_language=="pl" & original_plan$support_target_engine=="systran" & original_plan$target_language=="de"),]
		
		actual_plan=actual_plan[!(actual_plan$support_language=="pt" & actual_plan$support_target_engine=="systran" & actual_plan$target_language=="de"),]
		actual_plan=actual_plan[!(actual_plan$support_language=="pl" & actual_plan$support_target_engine=="systran" & actual_plan$target_language=="de"),]
		
		nblocks_original=nrow(original_plan)
		nblocks_actual=nrow(actual_plan)


		openDr(remDr)

		t0=Sys.time()
		min_index=sample_engines(actual_plan,"support_target_engine")
		if(!is.null(min_index))
		{
		bcounter=1
		#blocked_warn=list()
		#for()
		for(block in actual_plan[min_index,]$block_id_uniq)
		{
			plan_supp=actual_plan[actual_plan$block_id_unique==block,]
			td=round(as.numeric(difftime(Sys.time(),t0,units="sec")),0)

			if(nrow(plan_supp)>0)
			{
				td=round(as.numeric(difftime(Sys.time(),t0,units="sec")),0)
				if(td>reftime)
				{
					writeLines("rD refresh...")
					closeDr(remDr)
					Sys.sleep(refsleep)
					gc()
					openDr(remDr)
						t0=Sys.time()
					td=0
				}

				support=plan_supp$support_language
				engine=plan_supp$support_target_engine
				target=plan_supp$target_language
				block_id=plan_supp$block_id
				#tp=plan_supp$tp
				#block_tp=paste(strsplit(tp,split="-")[[1]][1:3],collapse="-")

				send=blocks[blocks$support_language==support & blocks$support_target_engine==engine & blocks$block_id==block_id,]$description
				sids=blocks[blocks$support_language==support & blocks$support_target_engine==engine & blocks$block_id==block_id,]$description_id
				orig=blocks[blocks$support_language==support & blocks$support_target_engine==engine & blocks$block_id==block_id,]$original_description_id
				freq=blocks[blocks$support_language==support & blocks$support_target_engine==engine & blocks$block_id==block_id,]$dire_freq
				#writeLines(paste(Sys.time()," block ", block, " of ",length(original_plan$block_id_uniq)," [",support,"-",engine,"-",target,"] (",length(send)," terms)",sep=""))
				#writeLines(paste(Sys.time()," ",bcounter," of ",nrow(original_plan)-nrow(actual_plan)-bcounter+1," [",support,"-",engine,"-",target,"] (",length(send)," terms)",sep=""))
				mess1=paste(Sys.time()," translating: ",length(send)," terms",sep="")
				mess2=paste("tp: [",support,"-",engine,"-",target,"]",sep="")
				mess3=paste("block: ",bcounter," [",nrow(original_plan)-nrow(actual_plan)+bcounter-1," of ", nrow(original_plan),"]",sep="")
				writeLines(paste(mess1,mess2,mess3,sep="\t"))
				writeble=FALSE
				tran=list()
				if(support==target)
				{
					temp_tran=send
					writeble=TRUE
				}else
				{
					tryCatch({tran=R.utils::withTimeout(translate(engine,support,target,send,remDr),timeout=timeout_translations)},error=function(e) e)
					if(length(tran)>0)
					{
						if(!any(tran$status=="KO"))
						{
							if(length(tran$translation)==length(send))
							{
								temp_tran=tran$translation
								writeble=TRUE
							}else
							{
								message("failed (translation errors)")
							}
						}else
						{
							message("failed (translation engine blocked/timed out)")
						}
					}
				}

				if(writeble==TRUE)
				{
					temp=cbind.data.frame(
						support_description_id=sids,
						description_id=orig,
						block_id_unique=block,
						dire_freq=freq,
						support_description=send,
						support_language=support,
						support_target_engine=engine,
						target_language=target,
						target_description=temp_tran
						)
					if(file.exists(support_translations_file))
					{
						write.table(temp,file=support_translations_file,col.names=FALSE,row.names=FALSE,quote=FALSE,sep="\t",append=TRUE)
					}else
					{
						write.table(temp,file=support_translations_file,col.names=TRUE,row.names=FALSE,quote=FALSE,sep="\t")
					}
					bcounter=bcounter+1
				}#else
				#{
				#	message("failed (translation errors)")
				#} # tranlsation failed
			} # supp lang is completed	
		} # supp lang cycle 
		}else
		{
			message("Translations completed!")
		}
	}
	closeDr(remDr)
}


#' Integrate all the translations for each description, generating the respective translation candidates
#'@param mtp_directory Working directory (string)
#'@param project_name Translation project name (string)
#'@return tcs A data frame with the TCs and the related score
integrate_translations=function(mtp_directory,project_name){


	project_directory=paste(mtp_directory,"/translation_projects/",project_name,sep="")
	support_translations_file=paste(project_directory,"/support_translations/",project_name,"_support_translations.tab",sep="")
	candidates_file=paste(project_directory,"/",project_name,"_translation_candidates.tab",sep="")
	supp=read.table(support_translations_file,sep="\t",header=TRUE,quote="",colClasses="character")
	supp$dire_freq=as.numeric(supp$dire_freq)

	ids=unique(supp$description_id)
	
	tc=list()
	c=1
	for(id in ids)
	{
		temp_supp=supp[supp$description_id==id,]
		cand=aggregate(dire_freq~rmsphyp(firstup(target_description)),data=temp_supp,sum)
		cand=cand[order(cand$dire_freq,decreasing=TRUE),]
		s=sum(cand$dire_freq)
		cand$perc_score=cand$dire_freq/s#/nrow(cand)
		#cand$corr_score=pnorm(scale(cand$perc_score))#p.adjust(cand$perc_score)
		colnames(cand)[1]="tc"
		tc[[c]]=cbind.data.frame(description_id=id,cand,rank=c(1:nrow(cand)))
		
		c=c+1
		#writeLines(paste(id,s,sep=" "))
	}


	tcs=do.call(rbind,tc)
	write.table(tcs,file=candidates_file,col.names=TRUE,row.names=FALSE,quote=FALSE,sep="\t")
	return(tcs)
}



# #' Compare the translation candidates with a benchmark
# #'@param mtp_directory Working directory (string)
# #'@param project_name Translation project name (string)
# #'@param benchmark_file_name Translation project name (string)
# #'@param target_language The target language (e.g. "fr")
# #'@param benchmark_file_text_identifier_field_name Text identifier in the benchmark file #"Snomed_conceptId"
# #'@param benchmark_file_text_to_compare_field_name Name of the field containing the text to compare with MTP 
# #'@return A data frame containing the exact match with the benchmark (DT vs MTP)
# compare_candidates_with_benchmark=function(
# 	mtp_directory,
# 	project_name,
# 	benchmark_file_name,
# 	benchmark_file_text_identifier_field_name,
# 	benchmark_file_text_to_compare_field_name){

# 	project_directory=paste(mtp_directory,"/translation_projects/",project_name,sep="")

# 	direct_translations_file=paste(project_directory,"/direct_translations/",project_name,"_direct_translations.tab",sep="")	
# 	support_translations_file=paste(project_directory,"/support_translations/",project_name,"_support_translations.tab",sep="")

# 	candidates_file=paste(project_directory,"/",project_name,"_translation_candidates.tab",sep="")
# 	benchmark_file=paste(project_directory,"/",project_name,"/",benchmark_file_name,sep="")

# 	direct_translations=read.table(direct_translations_file,sep="\t",header=TRUE,quote="",colClasses="character")

# 	support_translations=read.table(support_translations_file,sep="\t",header=TRUE,quote="",colClasses="character")
# 	support_translations$dire_freq=as.numeric(support_translations$dire_freq)

# 	candidates=read.table(candidates_file,sep="\t",header=TRUE,quote="",colClasses="character")
# 	candidates$dire_freq=as.numeric(candidates$dire_freq)
# 	candidates$perc_score=as.numeric(candidates$perc_score)
# 	candidates$rank=as.numeric(candidates$rank)
	
# 	benchmark=read.table(benchmark_file_name,sep="\t",header=TRUE,quote="",colClasses="character")
# 	human=cbind.data.frame(
# 		description_id=benchmark[,which(colnames(benchmark)==benchmark_file_text_identifier_field_name)],
# 		human=benchmark[,which(colnames(benchmark)==benchmark_file_text_to_compare_field_name)]
# 		)

# 	ids=intersect(support_translations$description_id,human$description_id)
	


# 	huma_dire=list()
# 	huma_cand=list()
# 	c=1
# 	for(id in ids)
# 	{
# 		print(c)
# 		huma=cbind.data.frame(id=id,type="human",tp="human",term=human[human$description_id==id,]$human)
		
# 		dire=direct_translations[direct_translations$description_id==id & direct_translations$support_language==target_language,]
# 		dire$tp=paste(dire$source_language,dire$source_support_engine,dire$support_language,sep="-")
# 		dire=cbind.data.frame(id=id,type="direct translation",tp=dire$tp,term=dire$support_description)

# 		cand=candidates[candidates$description_id==id,]
# 		cand=cbind.data.frame(id=id,type="MTP",tp=cand$rank,term=cand$tc)
# 		cand=cand[c(1:nrow(dire)),]

# 		huma$term=firstup(rmsphyp(huma$term))
# 		dire$term=firstup(rmsphyp(dire$term))
# 		cand$term=firstup(rmsphyp(cand$term))

# 		index_dire=dire$term%in%huma$term
# 		if(nrow(dire[index_dire,])>0)
# 		{
# 			huma_dire[[id]]=dire[index_dire,]
# 		}

# 		index_cand=cand$term%in%huma$term
# 		if(nrow(cand[index_cand,])>0)
# 		{
# 			huma_cand[[id]]=cand[index_cand,]
# 		}
# 		c=c+1
# 	}
# 	dire_match=do.call(rbind,huma_dire)
# 	cand_match=do.call(rbind,huma_cand)

# 	cand_exact_match=aggregate(id~type+tp,cand_match,length)
# 	cand_exact_match$cumulative=round(cumsum(cand_exact_match$id)/length(ids)*100,2)

# 	dire_exact_match=aggregate(id~type+tp,dire_match,length)
# 	dire_exact_match=dire_exact_match[order(dire_exact_match$id,decreasing=TRUE),]
# 	dire_exact_match$cumulative=round(dire_exact_match$id/length(ids)*100,2)

# 	topl=rbind.data.frame(dire_exact_match,cand_exact_match)
# 	#library(ggpubr)
# 	pdf("comparison.pdf")
# 	ggpubr::ggbarplot(topl,x="tp",y="cumulative",fill="type")+
# 	scale_fill_manual(values=c("white","grey"))+
# 	geom_text(aes(x=factor(tp),y=2,label=paste(cumulative,"%"),group=type),topl,position = position_dodge(.8),angle=90)+
# 	rotate_x_text(45)+
# 	xlab("Translation Paths\ndirect translations vs. MTP at various ranks")+
# 	ylab("Percent match with benchmark (cumulative for MTP)")+
# 	labs(fill="translation method")
# 	dev.off()
# }

