# project-rootfile

An Emacs [project](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html) backend that uses a root file (e.g. Gemfile) for detection.

It is useful when editing files outside of VCS. And it may also be useful if you are using monorepo.

## Usage

If you prefer VCS root over root file for project detection, add the following to your init file:

```lisp
(add-to-list 'project-find-functions #'project-rootfile-try-detect t)
```

Otherwise, if you prefer a root file, add the following:

```lisp
(add-to-list 'project-find-functions #'project-rootfile-try-detect)
```


## License

GPLv3
