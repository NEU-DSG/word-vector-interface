# Maintaining the Word Vector Interface

## Where to work

As with other git repositories, in general work should be done in the `develop` branch, the changes tested, then merged into the `main` branch.

However, since the WWP uses this app for pedagogical purposes, we've set up other git branches. Here's a rundown:

* `main`: The publication branch. The code from this branch runs at <http://lab.wwp.northeastern.edu>.
* `develop`: The branch used for development and testing. Changes to the Shiny app as a whole should be made here, and merged into the other branches when ready.
* `experimentals-only`: This branch contains a subdirectory in the `data` folder, containing experimental models. The models are also listed in the catalog. This branch serves as the base for the event-specific branches.
* Event-specific branches contain additional models which have been published in the WVI sandbox for pedagogical reasons. These branches are:
  * `institute-2019-07`
  * `institute-2021-05`
  * `institute-2021-07`


### Determining which models would be shown in the app

Open the [model catalog](data/catalog.json) and search for the string `"public" : "true"`. (Regular expression: `"public"\s*:\s*"true"`.) The objects containing that key-value pair represent models that will be loaded when the Shiny app starts up.


### Creating a release

1. Check that the `data/catalog_mini.json` file is up-to-date with metadata from the standard `catalog.json`.
    1. If not, copy the relevant entries from the standard file to the mini one.
    2. Save and commit your changes to `develop`.
    3. Either: open a PR to merge the changes into `main`, or use git commands to make the merge.
2. Make sure you're on the `main` branch and you have all commits from GitHub.
    1. `git checkout main`
    2. `git pull`
2. Create an [annotated git tag](https://git-scm.com/book/en/v2/Git-Basics-Tagging#_creating_tags) for the last commit in this branch.
    1. The tag name should be a [semantic version](https://en.wikipedia.org/wiki/Software_versioning), such as `v3.2`. Increment the minor version number unless the WVI app has seen significant changes.
    2. For example: `git tag -a -m "Updated models and added testing script" v3.1`
3. Push the tag to the GitHub repository.
    1. `git push --tags origin`
    2. Your new tag should now be listed on the repository's [Tags page](https://github.com/NEU-DSG/word-vector-interface/tags).
5. Create a ZIP of the codebase for customization, using an Apache Ant build.
    1. `ant`
    2. You will be prompted for a versioning string. Use your new tag, e.g. `v3.2`.
    3. (Optional.) Make sure you can run the customizable app.
        1. Unzip the release.
        2. Open the RStudio project file.
        3. Run `app.R` as a Shiny app.
        4. Make sure there are no errors in the startup, and the app runs as expected.
5. [Draft a release in GitHub.](https://github.com/NEU-DSG/word-vector-interface/releases/new)
    1. Select your new tag from the "Tag" dropdown.
    2. For the release title, use "Word Vector Interface, YYYY-MM-DD" (with the current date).
    3. Write a short description of significant updates. (A sample appears below.)
    4. Click the button that says "Attach binaries".
        1. Navigate to your ZIP file and select it.
    6. Make sure "Set as the latest release" is checked.
    7. Click the "Publish release" button.
6. Trumpet the achievement!

#### Sample release description

Title: Word Vector Interface, 2026-01-20

> This release includes a new "testing" directory and `Model-Testing-Template.Rmd`, a script for testing new models against several thousand pairs of words which we expect to have high similarity.
> This release also provides updates to the models included in the customizable Word Vector Interface.


### Updating for an Institute or workshop

First, make sure that the `experimentals-only` branch is up-to-date with the `main` branch. Determine if any changes need to be made to the experimental models, or if the catalog needs to be updated to show or hide a publication or experimental model. Push any updates to GitHub.

Once the `experimentals-only` branch is up to date, it's time to create a new branch for participant/additional models. Make sure you're in the `experimentals-only` branch, then create a new one using the [command line](https://git-scm.com/book/en/v2/Git-Branching-Basic-Branching-and-Merging) or [GitHub Desktop](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/making-changes-in-a-branch/managing-branches). Name the branch for the event.

Check out your new branch and begin adding word vector models for the event. In past Institutes, we've created a `participants` folder inside `data`, and placed the new models there. For each new model you want shown, [create a catalog entry for it](./components.md#word-embedding-models).

Once you've created the new catalog entries, **be sure to test that the Shiny app runs.** You can do this by pressing the “Run app” button in RStudio. While the process of loading the models takes a long time, this is the easiest way to be sure that the paths to the models are correct, that there are no missing or stray commas, and that the models themselves are showing up as expected.
