# NY Load Forecasting

This repository contains code in PowerShell and R to collect data from NY weather stations and then perform linear fitting to project load values for given weather conditions.

In 2016 I enrolled in the Foundations of Data Science program offered by [springboard.com](https://www.springboard.com/). As part of the curriculum I needed to upload my project to GitHub in order for it to be reviewed by my mentor in the program. This repository is a reupload with a cleaned up layout and removal of API keys. In the intervening years I have learned a few things about what not to do...

## How to Run the Model

1. Run each powershell file to collect data. The order of the files does not matter, but the outputs will need to be stored in a `Data` folder.
2. Run file `Capstone_Project.R`, it will execute the linear regression model.

**NOTE:** All files are configured to process data for 2016. This can be updated in the files.

## MesoWest API Access

This repository uses data from [MesoWest](https://mesowest.utah.edu/) and will require an account in order to programatically access the data. An API Access Key can be requested [here](https://developers.synopticdata.com/mesonet/). The API Access Key and generated token will need to be stored in the file `MESOWEST_API.json` which must be located in the working directory.

This file must be set up as follows:

```json
{
    "Key": "arst",
    "Token": "1234"
}
```
