# Running the Word Vector Interface with Docker

The Word Vector Interface can be run in a Docker image, using the files contained in the `docker/` directory. This method is useful for reproducing the app in a working environment, without a lot of fiddly installation work.

It is not a fast option, however: models take *much* longer to load when the Shiny app is run this way. You may have to wait up to an hour for the application to finish initializing.

You will need:

* some familiarity with the command line;
* [Docker](https://docs.docker.com/get-docker/); and
* [a copy of the Word Vector Interface repository](https://github.com/NEU-DSG/word-vector-interface/releases).


## Build the Docker image

First, you’ll need to [build a Docker image](https://docs.docker.com/engine/reference/builder/). Using the command line, navigate to the main directory which contains the Shiny app code, for example:

```shell
cd /Users/aclark/Documents/word-vector-interface
```

Then, instruct Docker to use the `Dockerfile` in this folder to construct an image with the name "wvi":

```shell
docker build --file docker/Dockerfile --tag wvi .
```

As part of this process, Docker follows the instructions given in [the WVI Dockerfile](./Dockerfile). We start with a pre-built [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/) Docker container from [The Rocker Project](https://rocker-project.org/). From there, Docker:

* installs software and R libraries;
* copies the code into the right place;
* and sets up a [custom Shiny Server configuration file](./shiny-server.conf).

Building the image will take some time, but once you have the image, you probably won't need to rebuild it very often. If you make changes to the R code, Dockerfile, or models, see the section on [updating the Docker image](#updating-the-docker-image).


## Start the application

Once the build process is complete, you can start up a Docker container by running this command:

```shell
docker run -p 3838:3838 --name=shiny-app wvi
```

This command tells Docker to use the "wvi" image to generate and run a container named "shiny-app". Docker will map the container's port 3838 to your computer's port 3838, letting you visit the application in the browser.

When you see a line that looks like this: 

```
[INFO] shiny-server - Starting listener on http://[::]:3838
```

...you can visit [localhost:3838](http://localhost:3838). The page will fail to load, but you’ll have told the Shiny Server to start loading the Word Vector Interface and the word2vec models.

As mentioned earlier, the process of loading all the models takes a very long time, much longer than the app takes when run through RStudio. Unfortunately, there’s no easy way to tell how far along the process is. You can check for progress by refreshing the page, or [use Docker to enter the container's command line interface](https://docs.docker.com/desktop/use-desktop/container/#integrated-terminal) and view the logs.

To stop the application, hit the <kbd>Control</kbd> and <kbd>c</kbd> keys while inside the Terminal window where the Docker container is running.

To restart the "shiny-app" Docker container, you can run this command:

```shell
docker restart shiny-app
```


### Inside the Docker container

Here's how to enter the Docker container command line interface through the Terminal:

```shell
docker exec -it shiny-app /bin/sh
```

The main points of interest within the Docker container environment are:

* Shiny Server config: `/etc/shiny-server/shiny-server.conf`
* Log files: `/var/log/shiny-server/`
* Apps directory: `/srv/shiny-server/`
  * Word Vector Interface code: `/srv/shiny-server/wvi/`

To read or edit files inside the Docker container, use the `nano` editor.


## Updating the Docker image

Once you’ve made changes to the R Shiny code, catalog file, or models, you’ll need to rebuild the "wvi" Docker image so that your changes are reflected. As before, navigate to the `word-vector-interface` folder and run this command:

```shell
docker build --file docker/Dockerfile --tag wvi .
```

To run the Shiny app Docker container, you’ll first have to delete the old one, then tell Docker to start up a new container with the same name:

```shell
docker rm shiny-app
docker run -p 3838:3838 --name=shiny-app wvi
```


## Troubleshooting

### Memory problems

If you see this line soon after starting the Docker container:

```
[INFO] shiny-server - Error getting worker: Error: The application exited during initialization.
```

...the Shiny app may have run out of memory as it tried to load all the "public" word vector models. If it's not possible to switch to a computer with more RAM available, you can find out how much memory is available by running this command:

```shell
docker info
```

You can probably still run the application by loading a smaller number word vector models. To do so, you can [edit the catalog](../components.md#word-embedding-models) so that fewer models need to be loaded.

You can also edit [app.R](../app.R) to use the [“mini” catalog](../data/catalog_mini.json), which specifies only two models. Find the line that looks like this:

```R
catalog_filename <- "data/catalog.json"
```

Then change the filename so the line looks like this:

```R
catalog_filename <- "data/catalog_mini.json"
```

