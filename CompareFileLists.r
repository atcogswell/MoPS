
# ------------------------------- Compare directory listings and send e-mail with differences ---------------------------#

library(RDCOMClient) #needed for sending e-msil

setwd("C:/Temp")
#getwd()

# I mapped "\\dcnsbiona01b\EDC_V1_SHR2\Shared\DATA_WCTS_ARP_SABS" to Z:\
# R can't seem to have a network drive as working directory unless
# it's got a drive letter
#setwd("Z:")

# Get username and create an e-mail address
user <- Sys.getenv("USERNAME")
email <- paste(user,"@dfo-mpo.gc.ca", sep = "")
#print(email)

fileold <- "dirold.txt"
filenew <- "dirnew.txt"

#If the original directory listing doesn't exist, create it
if (!file.exists(fileold)) {
  system("cmd.exe /c dir /b/s > dirold.txt")
}

#If the new directory listing doesn't exist, create it
if (!file.exists(filenew)) {
  system("cmd.exe /c dir /b/s > dirnew.txt")
}

# If both directory listings exist, compare them and 
# any differences are stored as 'diff'.  If diff has
# values in it, write them to a new file and send 
# file via e-mail.
if (file.exists(fileold) & file.exists(filenew)) {
  dirlistnew <- scan(filenew, what="", sep="\n", quote = "\"")
  dirlistold <- scan(fileold, what="", sep="\n", quote = "\"")
  diff <- setdiff(dirlistnew, dirlistold)
  if (length(diff)>0) {
    # Why does this command write an 'x' to the first line of the file???
    write.csv(diff, "NewFiles.csv", row.names=FALSE, col.names = FALSE)
    #--------Start original ---------------------------------#
    OutApp <- COMCreate("Outlook.Application")
    ## create an email 
    outMail = OutApp$CreateItem(0)
    ## configure  email parameter 
    outMail[["To"]] = email
    outMail[["subject"]] = "New files in drive"
    #outMail[["body"]] = ""
    outMail[["Attachments"]]$Add("C:/Temp/NewFiles.csv")
    ## send it                     
    outMail$Send()
    #--------End original ---------------------------------#
    # Append all new files to dirold.txt
    write.table(diff, fileold, append = TRUE, row.names=FALSE, col.names = FALSE)
    
  }

}






# Create current directory listing
system("cmd.exe dir /b \"//dcnsbiona01b/EDC_V1_SHR2/Shared/DATA_WCTS_ARP_SABS\" > DirTest.txt")


dirlistnew <- scan(filenew, what="", sep="\n", quote = "\"")
dirlistold <- scan("dirold.txt", what="", sep="\n", quote = "\"")
textold2 <- scan("dir.txt", what="", sep="\n", quote = "\"")
textnew2 <- read.csv("dir2.txt", header = FALSE)
textold3 <- read.csv("dir.txt", header = FALSE)

class(textnew2)
class(difftest)
class(textnew)

diff <- setdiff(textnew, textold)
class(diff)
if(length(diff) = 0){
  print("don't send e-mail")
  quit(save = "no")
} else 

ifelse (length(diff) == 0,print("don't send e-mail"),print("send e-mail")) 


  
# Why does this command write an 'x' to the first line of the file???
write.csv(diff, "NewFiles.csv", row.names=FALSE, col.names = FALSE)


#--------Start original ---------------------------------#
OutApp <- COMCreate("Outlook.Application")
## create an email 
outMail = OutApp$CreateItem(0)
## configure  email parameter 
outMail[["To"]] = email
outMail[["subject"]] = "New files in drive"
#outMail[["body"]] = ""
outMail[["Attachments"]]$Add("C:/Temp/NewFiles.csv")
## send it                     
outMail$Send()
#--------End original ---------------------------------#

email_fn <- function() {
  OutApp <- COMCreate("Outlook.Application")
  ## create an email 
  outMail = OutApp$CreateItem(0)
  ## configure  email parameter 
  outMail[["To"]] = email
  outMail[["subject"]] = "New files in drive"
  #outMail[["body"]] = ""
  outMail[["Attachments"]]$Add("C:/Temp/NewFiles.csv")
  ## send it                     
  outMail$Send()
}

email_fn

#Hmmm, this didn't work as a function

# this noquote() didn't work to remove the quotatation marks
write.table(diff, "dir.txt", append = TRUE, row.names=FALSE, col.names = FALSE)




str(textnew)
str(diff)
class(text1)

text1
text2
getwd()

rm(list = ls())
diff

textold
textold2
