
# ------------------------------- Compare directory listings and send e-mail with differences ---------------------------#

# get the path for the R executable
# file.path(R.home(), "bin", "R")
# in my case it was "C:/PROGRA~1/R/R-31~1.0/bin/R"

# function to check if a package is installed
is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 

# The "RDCOMClient" is needed for sending e-mail
# Check to see that it's installed and if not, install it

if(!is_installed("RDCOMClient"))  
{  
  install.packages("RDCOMClient")  
}  
library("RDCOMClient",character.only=TRUE,quietly=TRUE,verbose=FALSE)  

# Set the working directory
# I mapped "\\dcnsbiona01b\EDC_V1_SHR2\Shared\DATA_WCTS_ARP_SABS" to Z:\
# R can't seem to have a network drive as working directory unless
# it's got a drive letter

#setwd("Z:")

#setwd("C:/Temp") # Change as necessary

#setwd("Z:")
setwd( "R:\\Shared\\Cogswell\\_BIOWeb\\BBMP") # Change as necessary
#getwd()

# Get username and create an e-mail address

email <- "Andrew.Cogswell@dfo-mpo.gc.ca"

# -------- function to send e-mail --------#
email_fn <- function(email, subject, attachment) {
  OutApp <- COMCreate("Outlook.Application")
  ## create an email 
  outMail = OutApp$CreateItem(0)
  ## configure  email parameter 
  outMail[["To"]] = email
  outMail[["subject"]] = subject
  #outMail[["body"]] = ""
  outMail[["Attachments"]]$Add(attachment)
  ## send it                     
  outMail$Send()
}
# -------- end function --------#


fileold <- "dirold.txt"
filenew <- "dirnew.txt"
fileall <- "dirfull.txt"



#If the original directory listing doesn't exist, create it
if (!file.exists(fileold)) {
  system("cmd.exe /c dir /b/s > dirold.txt")
}

if (!file.exists(fileall)) {
  system("cmd.exe /c copy dirold.txt dirfull.txt")
}

# Create a new file listing (bare format, check all sub-directories)
system("cmd.exe /c dir /b/s > dirnew.txt")

# start Outlook if it isn't running
#system("cmd.exe /c tasklist /FI \"IMAGENAME eq outlook.exe\" | find /I /N \"outlook.exe\" || \"C:/Program Files (x86)/Microsoft Office/Office14/OUTLOOK.EXE\"")
#email_fn(email, "More new files", "C:/Temp/NewFiles.csv")
#system("cmd.exe /c taskkill /IM outlook.exe")

#system("C:/CheckFileScript/Look-for-new-files-code/CheckOutlook.bat")

#TRY 
#shell.exec("\\\\network\\path\\file.bat")

# Compare both directory listings and store
# any differences are stored as 'diff'.  If diff has
# values in it, write them to a new file and send 
# file via e-mail.
# This doesn't error check the e-mail address
dirlistnew <- scan(filenew, what="", sep="\n", quote = "\"")
dirlistold <- scan(fileold, what="", sep="\n", quote = "\"")
dirlistfull <- scan(fileall, what="", sep="\n", quote = "\"")
filesadded <- setdiff(dirlistnew, dirlistold)
filesremoved <- setdiff(dirlistfull, dirlistnew)
if (length(filesadded)>0) {
  # Why does this command write an 'x' to the first line of the file???
  write.csv(filesadded, "R:/Shared/Cogswell/_BIOWeb/BBMP/NewFiles.csv", row.names=FALSE, col.names = FALSE)
  # Send e-mail with attachment
  email_fn(email, "More new files","R:/Shared/Cogswell/_BIOWeb/BBMP/NewFiles.csv")
  write.table(filesadded, fileall, append = TRUE, row.names=FALSE, col.names = FALSE)
}

if (length(filesremoved)!=0) {
  # Why does this command write an 'x' to the first line of the file???
  write.csv(filesremoved, "RemovedFiles.csv", row.names=FALSE, col.names = FALSE)
  # Send e-mail with attachment
  email_fn(email, "Some files were removed", "R:/Shared/Cogswell/_BIOWeb/BBMP/RemovedFiles.csv")
}
# delete the old directory listing and copy new to old
system("cmd.exe /c del dirold.txt")
system("cmd.exe /c rename dirnew.txt dirold.txt")


# ---------Clean up---------------#
rm(list = ls())
