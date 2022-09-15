# Running the Word Vector Interface with Docker

The Word Vector Interface can be run in a Docker image, using the files contained in the `docker/` directory. This method is useful for reproducing the app in a working environment, without a lot of fiddly installation work.

It is not a fast option, however: models take *much* longer to load when the Shiny app is run this way. You may have to wait up to an hour for the application to finish initializing.

You will need:

* some familiarity with the command line;
* [Docker](https://docs.docker.com/get-docker/); and
* [a copy of the Word Vector Interface repository](https://github.com/NEU-DSG/word-vector-interface/releases).


## Build the Docker image

First, you’ll need to [build a Docker image](https://docs.docker.com/engine/reference/builder/). Using the command line, navigate to the main directory which contains the Shiny app code, for example:

```
cd /Users/aclark/Documents/word-vector-interface
```

Then, instruct Docker to use the `Dockerfile` in this folder to construct an image with the name "wvi":

```
docker build --file docker/Dockerfile --tag wvi .
```

As part of this process, Docker follows the instructions given in [the WVI Dockerfile](./Dockerfile). We start with a pre-built [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/) Docker container from [The Rocker Project](https://rocker-project.org/). From there, Docker:

* installs software and R libraries;
* copies the code into the right place;
* and sets up a [custom Shiny Server configuration file](./shiny-server.conf).

Building the image will take some time, but once you have the image, you probably won't need to rebuild it very often. If you make changes to the R code, Dockerfile, or models, just rerun the steps above.


## Start the application

Once the build process is complete, you can start up a Docker container by running this command:

```
docker run -p 3838:3838 wvi
```

When you see a line that reads “[INFO] shiny-server - Starting listener on http://[::]:3838”, you can visit <localhost:3838> to start the process of initializing the Shiny app.

As mentioned earlier, the process of loading all the models takes a very long time, much longer than the app takes when run through RStudio. Unfortunately, there’s no easy way to tell how far along the process is. You can check for progress by refreshing the page, or [use Docker to enter the container's command line interface](https://docs.docker.com/desktop/use-desktop/container/#integrated-terminal) and view the logs.


### Inside the Docker container

Here are the main points of interest within the Docker container environment:

* Shiny Server config: `/etc/shiny-server/shiny-server.conf`
* Log files: `/var/log/shiny-server/`
* Apps directory: `/srv/shiny-server/`
  * Word Vector Interface: `/srv/shiny-server/wvi/`
