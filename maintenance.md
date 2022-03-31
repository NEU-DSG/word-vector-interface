# Maintaining the Word Vector Interface

## Where to work

As with other git repositories, in general work should be done in the `develop` branch, the changes tested, then merged into the `main` branch.

However, since the WWP uses this app for pedagogical purposes, we've set up other git branches. Here's a rundown:

* `main`: The publication branch. The code from this branch runs at <lab.wwp.northeastern.edu>.
* `develop`: The branch used for development and testing. Changes to the Shiny app as a whole should be made here, and merged into the other branches when ready.
* `experimentals-only`: This branch contains a subdirectory in the `data` folder, containing experimental models. The models are also listed in the catalog. This branch serves as the base for the event-specific branches.
* Event-specific branches contain additional models which have been published in the WVI sandbox for pedagogical reasons. These branches are:
  * `institute-2019-07`
  * `institute-2021-05`
  * `institute-2021-07`


### Determining which models would be shown in the app

Open the [model catalog](data/catalog.json) and search for the string `"public" : "true"`. (Regular expression: `"public"\s*:\s*"true"`.) The objects containing that key-value pair represent models that will be loaded when the Shiny app starts up.


### Updating for an Institute or workshop

First, make sure that the `experimentals-only` branch is up-to-date with the `main` branch. Determine if any changes need to be made to the experimental models, or if the catalog needs to be updated to show or hide a publication or experimental model. Push any updates to GitHub.

Once the `experimentals-only` branch is up to date, it's time to create a new branch for participant/additional models. Make sure you're in the `experimentals-only` branch, then create a new one using the [command line](https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging) or [GitHub Desktop](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/making-changes-in-a-branch/managing-branches). Name the branch for the event.

Check out your new branch and begin adding word vector models for the event. In past Institutes, we've created a `participants` folder inside `data`, and placed the new models there. For each new model you want shown, [create a catalog entry for it](./components.md#word-embedding-models).

Once you've created the new catalog entries, **be sure to test that the Shiny app runs.** You can do this by pressing the “Run app” button in RStudio. While the process of loading the models takes a long time, this is the easiest way to be sure that the paths to the models are correct, that there are no missing or stray commas, and that the models themselves are showing up as expected.
