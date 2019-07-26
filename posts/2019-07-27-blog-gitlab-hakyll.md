---
title: Making your own blog with GitLab Pages and Hakyll
author: DavSanchez
---

So, I decided to give blogging a try and made a GitLab Pages site with the help of [Hakyll](https://jaspervdj.be/hakyll/), a static site generator written in Haskell. This post details how I configured it.

To follow the instructions you need [a working `stack` installation](https://www.haskellstack.org/) (and a [GitLab](https://gitlab.com/) account, of course!)

At the time of writing this post, the blog's aesthetics are not so customized. I'm mostly using a fine-looking template I found. So, what did I do to make it work in GitLab Pages?

# First steps

First of all, I got [that template I was talking about](https://github.com/stackbuilders/dr-hakyll), which includes Bootstrap and support for RSS/Atom:

```bash
git clone https://github.com/stackbuilders/dr-hakyll
cd dr-hakyll
```

# Modifying the project configuration

Then, what I did was modifying a little bit the HTML partials in the `templates` directory and customizing the `.cabal` file, which I also renamed to `blog.cabal`. The name used in the `.cabal` file's `name` field would be the name of the executable program via `stack`. This is the content:

```yaml
name:               blog
version:            0.1.0
build-type:         Simple
cabal-version:      >= 1.10

executable blog
  hs-source-dirs:      app
  main-is:             Main.hs
  ghc-options:         -threaded
  build-depends:       base >= 4.7 && < 5
                     , hakyll >= 4.6 && < 5
  default-language:    Haskell2010
```
## The `stack.yaml` file

I also deleted the `stack.yaml` file to generate another one from scratch, as I had some problems in GitLab CI with the original file. To generate a new `stack.yaml` file, run the following command inside the project's root directory:

```bash
stack init
```

`stack` will then check the `.cabal` file present in the directory and will generate a new `stack.yaml` file with the best snapshot available. The snapshot selected is indicated in the `resolver` field, as follows:

```yaml
resolver: lts-12.26
```

Take into account that the `resolver` indicates a specific version of the Haskell compiler, GHC. In my case, `lts-12.26` corresponds to GHC 8.4.4, which will be used later on for setting up GitLab's CI service.

## Preparing the GitLab repo

While you are modifying all these files, you can also prepare your git repository for these contents, like setting the GitLab remote to upload the working directory, renaming the project directory to suit your needs, or adding a [specific `.gitignore` file for Haskell code](https://github.com/github/gitignore/blob/master/Haskell.gitignore).

If you want the blog to be available from your "root URL" you should create a repository called `<username>.gitlab.io`. If you don't do this, the blog URL will be `<username>.gitlab.io/<repo-name>`.

# Modifying app defaults

Since GitLab Pages' static content has to be served in the `public` directory, I added the following code at the end of `app/Main.hs` to modify the field `destinationDirectory`:

```haskell
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "public"
  }
```

At the beginning of the same file, after `main = `, replace `hakyll $ do` with `hakyllWith config $ do`. This references the `config` function defined above.

# Instructing GitLab CI to build the site

To deploy statically generated sites, GitLab uses its CI service. This service can be configured creating a `.gitlab-ci.yml` file located in the project's root directory.

```yaml
image: haskell:8.4.4

before_script:
  - stack --system-ghc build

pages:
  stage: deploy
  script:
  - stack --system-ghc exec blog build
  artifacts:
    paths:
    - public
  only:
  - master
```

  - The `image` field specifies the Haskell image used to execute the Haskell program which will generate the static files
  - The `before_script` field executes `stack build` with the image's GHC (same version as the `stack.yaml`'s `resolver` field)
  - The `pages` field specifies the `stack` execution of the Haskell program (in the `script` field) for a specific stage (`stage` field), with the `artifacts` field being the publicly available directory of our project.
  - The `only` field specifies the branch on which the CI job will act. In my case, I put the `master` branch. This is useful if for example you test changes in a `develop` branch and don't want the CI to try to build the page on every commit to that branch, only building when you merge changes into `master`.

# Deploying the site

If you commit all these changes and push to your GitLab remote, the CI will launch a job with the configuration. If it succeeds, you will be able to access your new blog!

The process of building the static site with Haskell is a little slow, though. I recommend increasing the job's running time to 2 hours (in your GitLab project, __Settings > CI / CD > General pipelines > Timeout: `2h`__), just in case.

Remember, the actual URL will depend on your GitLab remote's name. If you want the blog to be available on `<username>.gitlab.io` you should create a repository with that name. Otherwise, the blog URL will be `<username>.gitlab.io/<repo-name>`.

## Testing locally

If you want to set up a local server to check the changes before pushing into `master`, you can do it with the following commands from the project's root directory, assuming you wrote `executable blog` in your `.cabal` file (check above):

```bash
stack setup
stack build
stack exec blog build
stack exec blog watch
```

This will set up a simple `warp` web server, serving the generated files (present on the `public` directory) in your `localhost:8000` (the terminal will indicate the port used). You can interrupt the server with `Ctrl+C`.

Additionally, you can remove the generated static files and rebuild the project with

```bash
stack exec blog clean
stack exec blog build
```
or with

```bash
stack exec blog rebuild
```

If you modify `Main.hs`, you'll need to do `stack build` again and then run `stack exec blog rebuild` to regenerate the static files.

# Adding syntax highlighting

To add syntax highlighting to your code blocks, you just need to add specific CSS files. [This repo](https://github.com/tejasbubane/hakyll-css) has some CSS files with different themes, so you can use whichever you like, drop it in your `css` directory, and reference it in the `default.html` template file to get that nice colors in your code!