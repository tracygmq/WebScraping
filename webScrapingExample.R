library(RCurl)

### 1) First task is to get all of the web links we will need ##
base_url<-"http://gtrnadb.ucsc.edu/"
base_html<-getURLContent(base_url)[[1]]#get all links from the base url
links<-strsplit(base_html,"a href=")[[1]]

get_data_url<-function(s) {
  u_split1<-strsplit(s,"/")[[1]][1]
  u_split2<-strsplit(u_split1,'\\"')[[1]][2]
  ifelse(grep("[[:upper:]]",u_split2)==1 & length(strsplit(u_split2,"#")[[1]])<2,return(u_split2),return(NA))
}

# Extract only those element that are relevant
genomes<-unlist(lapply(links,get_data_url))
genomes<-genomes[which(is.na(genomes)==FALSE)]

parse_genomes<-function(g) {
  g_split1<-strsplit(g,"\n")[[1]]
  g_split1<-g_split1[2:5]
  # Pull all of the data and stick it in a list
  g_split2<-strsplit(g_split1[1],"\t")[[1]]
  ID<-g_split2[1]                             # Sequence ID
  LEN<-strsplit(g_split2[2],": ")[[1]][2]     # Length
  g_split3<-strsplit(g_split1[2],"\t")[[1]]
  TYPE<-strsplit(g_split3[1],": ")[[1]][2]    # Type
  AC<-strsplit(g_split3[2],": ")[[1]][2]      # Anticodon
  SEQ<-strsplit(g_split1[3],": ")[[1]][2]     # ID
  STR<-strsplit(g_split1[4],": ")[[1]][2]     # String
  return(c(ID,LEN,TYPE,AC,SEQ,STR))
}

get_structs<-function(u) {
  struct_url<-paste(base_url,u,"/",u,"-structs.html",sep="")
  raw_data<-getURLContent(struct_url)
  s_split1<-strsplit(raw_data,"<PRE>")[[1]]
  all_data<-s_split1[seq(3,length(s_split1))]
  data_list<-lapply(all_data,parse_genomes)
  for (d in 1:length(data_list)) {data_list[[d]]<-append(data_list[[d]],u)}
  return(data_list)
}

# Collect data, manipulate, and create data frame (with slight cleaning)
genomes_list<-lapply(genomes[1:2],get_structs) # Limit to the first two genomes (Bdist & Spurp), a full scrape will take a LONG time
genomes_rows<-unlist(genomes_list,recursive=FALSE) # The recursive=FALSE saves a lot of work, now we can just do a straigh forward manipulation
genome_data<-t(sapply(genomes_rows,rbind))
colnames(genome_data)<-c("ID","LEN","TYPE","AC","SEQ","STR","NAME")
genome_data<-as.data.frame(genome_data)
genome_data<-subset(genome_data,ID!="</PRE>")   # Some malformed web pages produce bad rows, but we can remove them


head(genome_data)
