# The Graph Network Shiny App

This application provides an interactive analytical COVID-19 dashboard for the African continent.

## Deploying the App
The application can be run locally or from Docker containers.
before Building the container make sure an `.env_db` file exists with the proper variables declared.

to start the containers, you need to have docker and docker-compose installed and run the following commands:

```bash
docker-compose build
```  

after the containers are built they can be started with

```bash
docker-compose -p shinyapp -f docker-compose.yml --env-file .env_db up --build -d
```
One the containers are up the database will be accessible from on the host on por 5432 and can beaccessed with any postgresql client.
To stop the containers you can

```bash
docker-compose down
```
Once the 

## Running shinyapp with Rstudio

### Step 0
If you're working on the code and need to get the app up and down easily for feedback you can still use Rstudio to launch it locally.
If you have only worked with Renku in our project you will neet to download R & Rstudio first avilable at :

https://cran.r-project.org/mirrors.html

https://rstudio.com/products/rstudio/

### Step 2
Once Rstudio is setup you can select the cloned repo GRAPHnetEpiApp and launch the project.

Open in the editor either ui.R / server.R or global.R and in the editor's toolbar you will find the "Run App" button, click it and _voilà_

In your console, you will have a line notifying you on which adress you can open the app in your navigator. Copy the link in your adress bar and _voilà_




## To Know more
### Working with containers
* [Getting started with Docker](https://docs.docker.com/get-started/overview/)
* [Overview of Docker Compose](https://docs.docker.com/compose/)
* [Docker desktop ( for Mac and Windows users)](https://docs.docker.com/desktop/)
* [Using Docker as Shiny server](https://github.com/rocker-org/shiny)
