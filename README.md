# [flatmap.io](http://www.flatmap.io)

The right place to find right IT company.

[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![CircleCI](https://circleci.com/gh/vadimbakaev/flatmap.io/tree/master.svg?style=svg&circle-token=bbb1dac62e12142d3ce4d5d3bec7f3f3ac192768)](https://circleci.com/gh/vadimbakaev/flatmap.io/tree/master)
[![StackShare](http://img.shields.io/badge/tech-stack-0690fa.svg?style=flat)](https://stackshare.io/vadimbakaev/flatmap-io)
[![FOSSA Status](https://app.fossa.com/api/projects/custom%2B17867%2Fgit%40github.com%3Avadimbakaev%2Fflatmap.io.git.svg?type=shield)](https://app.fossa.com/projects/custom%2B17867%2Fgit%40github.com%3Avadimbakaev%2Fflatmap.io.git?ref=badge_shield)


### Prerequisites

You need to install the following software:
- [Stack](https://www.haskell.org/downloads/#stack) for Haskell
  - [yesod](https://www.yesodweb.com/) web framework
  - [hlint](https://github.com/ndmitchell/hlint) to improve your Haskell
  - [hindent](https://github.com/chrisdone/hindent) to make your Haskell pretty
- [Docker](https://www.docker.com/get-started) to run mongo
  - [MongoDB](https://www.mongodb.com/) to store data

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Hlint
```
stack install hlint
```

## Hindent
```
stack install hindent
```

## How to run

Expose MongoDB on localhost with default port 27017

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to http://localhost:3000.

## Tests

```
stack test --flag h-map:library-only --flag h-map:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.

