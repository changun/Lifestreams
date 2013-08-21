# helper function to talk to Ohmage api
library(Ohmage)
library(plyr)
# fix parse prompt function for remote activity prompt
source("utils/parsePrompt.R")

mobilize2012Info = list(user="mobilize.dev", 
                          password="Hedgehog!567", 
                          serverURL="https://lausd.mobilizingcs.org/app",
                          projectName="mobilize")

familyInfo = list(user="as_admin", 
                        password="Password@admin", 
                        serverURL="https://internal.ohmage.org/app",
                        projectName="family2013")

campaignInfo = familyInfo


# login
oh.login(campaignInfo$user , campaignInfo$password, campaignInfo$serverURL)

# load project specific code
source(paste("projects/",campaignInfo$projectName,"/createProject.R", sep=""))

# create project
project = createProject()
