---
title: Making your own blog with GitLab Pages and Hakyll
author: DavSanchez
---

So, I decided to give blogging a try and made a GitLab Pages site with the help of [Hakyll](https://jaspervdj.be/hakyll/), a static site generator written in Haskell. This post details how I configured it.

To follow the instructions you need a working [`stack`](https://www.haskellstack.org/) installation (and a [GitLab](https://gitlab.com/) account, of course!)

At the time of writing this post, the blog's aesthetics are not so customized. I'm mostly using a fine-looking template I found. So, what did I do to make it work in GitLab Pages?

# First steps

First of all, I got [that template I was talking about](https://github.com/stackbuilders/dr-hakyll), which includes Bootstrap and support for RSS/Atom:

```bash
git clone https://github.com/stackbuilders/dr-hakyll && cd dr-hakyll
```

Then, what I did was modifying a little bit the HTML partials in the `templates` directory and customizing the `.cabal` file, which I renamed to `blog.cabal`. This is the content:

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

I also deleted the `stack.yaml` file to generate another one from scratch, as I had some problems in GitLab CI with the original file. To generate a new `stack.yaml` file, run the following command inside the project's root directory:

```bash
stack init
```

`stack` will then check the `.cabal` file present in the directory and will generate a new `stack.yaml` file with the best snapshot available.

While you are at it, you can also prepare your git repository for these contents, like setting the GitLab remote to upload the working directory, renaming the project directory to suit your needs, or adding a [specific `.gitignore` file for Haskell code](https://github.com/github/gitignore/blob/master/Haskell.gitignore).

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

To deploy statically generated sites, GitLab uses its CI service. This service can be configured with a `.gitlab-ci.yml` file located in the project's root directory.

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



