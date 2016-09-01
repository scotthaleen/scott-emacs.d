###My emacs thingy


checkout yasnippets [snippets](https://github.com/AndreaCrotti/yasnippet-snippets/tree/master) 
to `~/.emacs.d/yasnippet-snippets`


Started from Alan's [emacs.d](https://github.com/alandipert/alan-emacs.d)



### Setup

Install emacs through [Homebrew](http://brew.sh)

```sh
$ brew install emacs --with-cocoa --with-imagemagick
```

Make sure homebrew is setup correctly with `/usr/local/bin` first in your [search path](http://stackoverflow.com/questions/10343834/homebrew-wants-me-to-amend-my-path-no-clue-how).


To make sure that the correct version is being launched run 

```sh
$ emacs --version
GNU Emacs 24.5.1
Copyright (C) 2015 Free Software Foundation, Inc.
GNU Emacs comes with ABSOLUTELY NO WARRANTY.
You may redistribute copies of Emacs
under the terms of the GNU General Public License.
For more information about these matters, see the file named COPYING.
```

### Install

Clone this repo. If you already have a `~/.emacs.d/` directory (and do not need anything in it) delete it.  Then move/rename the clone repo to `~/.emacs.d/` then just launch `emacs` and wait for it to install the packages.

For example:

```sh
$ rm -rf ~/.emacs.d/

$ git clone https://github.com/scotthaleen/scott-emacs.d ~/.emacs.d 

$ emacs 
```


[A Quick Emacs key Reference](EMACS-QUICK-REFERENCE.md)


