# project-rootfile

An Emacs [project](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html) backend that uses a root file (e.g. Gemfile) for detection.


This package is working in progress.

## Usage

If you prefer VCS root over root file for project detection, add the following to your init file:

```lisp
(add-to-list 'project-find-functions #'project-rootfile-try t)
```

Otherwise, if you prefer a root file, add the following:

```lisp
(add-to-list 'project-find-functions #'project-rootfile-try)
```


## License

GPLv3
