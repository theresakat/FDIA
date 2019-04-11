# Dummy Cleaning Code

######################################################

FixText<-function(curtext,checkvals){
  newvals<-curtext[!curtext %in% checkvals] ## figure out if any thing in the text string you're 
  # checking already has a corresponding entry in 'checkvals'.  This is just a placeholder... 
  # you'll want some other type of check for your ID values, I presume.
  
  if(length(newvals)>0){
    for(i in newvals){
      ## for each unmatched value, either correct it to an existing code, or add code to checkvals.
      checking<-T
      while(checking){
        cat("Current value:",i,"\n")
        cat("see view for possible values in list.")
        View(checkvals)
        fixval<-readline("enter new value:")
        if(!readline(paste("Is '",fixval,"'#######' OK?('n' if no)", sep = "")) %in% "n")checking<-F## use readline to pause the entry process, check your work, and allow you to change your mind.
      }
      if(fixval %in% checkvals)curtext[curtext == i]<-fixval ## either fix the 'curtext' vector with new values (currently done for all instances)
      if(!fixval %in% checkvals)checkvals<-c(checkvals,fixval) ## or, add the the value to the checking vector.
    }
  }else{
    cat("text matches established values")
  }
  return(list(curtext,checkvals))
}
ctext<-c("A","B","C","D","E","F","G")
cvals<-c("A","B","C","E","F","G")
out<-FixText(ctext,cvals)
