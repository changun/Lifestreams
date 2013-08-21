# helper function to talk to Ohmage api
library(Ohmage)
library(plyr)
library(httr)
library(ggplot2)
library(doMC)
registerDoMC(cores=16)
source("classes.R")
# fix parse prompt function for remote activity prompt
for(src in dir("utils/", full.names=T))
  source(src)
for(src in dir("vizs/", full.names=T))
  source(src)
# TODO: get campaign info from url
#session$clientData$url_search

mobilizeInfo = list(user=PrivateConfigurations.MOBILIZE_USERNAME, 
                    password=PrivateConfigurations.MOBILIZE_PASSWORD, 
                    serverURL="https://lausd.mobilizingcs.org/app",
                    projectName="mobilize2013")

familyInfo = list(user=PrivateConfigurations.FAMILYWELLNESS_USERNAME, 
                  password=PrivateConfigurations.FAMILYWELLNESS_PASSWORD, 
                  serverURL="https://internal.ohmage.org/app",
                  projectName="family2013")

#campaignInfo = familyInfo

campaignInfo =mobilizeInfo

# logout first in case there is an existing session.
oh.logout()
# login to the specified server
oh.login(campaignInfo$user , campaignInfo$password, campaignInfo$serverURL)

# load project specific code
source(paste("projects/",campaignInfo$projectName,"/createProject.R", sep=""))

# create project
#project = createProject()
