# shiny-weather-app

This Shiny app was created as part of a university project, outlined in `Project_brief_2022.pdf`

We were provided with files containing weather data for 20 weather stations, and asked to build a shiny app to visualise and summarise the data, including calculating the Hutton Criteria.

### 1. View on shinyapps.io

The app can be viewed in action on shinyapps.io:

https://charlywhitlow.shinyapps.io/WeatherApp/

### 2. Run locally

To run locally you'll need to create an account with [Stadia Maps](https://docs.stadiamaps.com/tutorials/getting-started-in-r-with-ggmap/), to generate an API key

Create a `config.yml` file in the main project directory, with the following format, and replace with your API key:

```
default :
STADIA_MAPS_API_KEY : "YOUR-API-KEY-HERE"
```