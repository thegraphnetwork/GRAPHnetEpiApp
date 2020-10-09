# The Graph Network Shiny App

This application provides an interactive analytical COVID-19 dashboard for the African continent.

## Deploying the App
The application can be run locally or from Docker containers.

to start the containers, you need to have docker and docker-compose installed and run the following commands:

```bash
docker-compose build
```  

after the containers are built they can be started with

```bash
docker-compose up -d
```

To stop the containers you can

```bash
docker-compose down
```

## To Know more
### Working with containers
* [Getting started with Docker](https://docs.docker.com/get-started/overview/)
* [Overview of Docker Compose](https://docs.docker.com/compose/)
* [Docker desktop ( for Mac and Windows users)](https://docs.docker.com/desktop/)
* [Using Docker as Shiny server](https://github.com/rocker-org/shiny)