# project-rootfile

Extension of `project.el` to detect project root with root file (e.g. `Gemfile`).

It is useful when editing files outside of VCS.  And it may also be useful if you are using monorepo.
It is very smiller to [Projectile](https://github.com/bbatsov/projectile)'s `projectile-root-top-down`, but it relies on the Emacs standard `project.el`.

If you want to learn more about `project.el`, see info node [(emacs) Projects](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html).


## Usage

As is the default for Projectile, you prefer a VCS root over a root file for project detection, add the following to your init file:

```lisp
(add-to-list 'project-find-functions #'project-rootfile-try-detect t)
```

Otherwise, if you prefer a root file, add the following:

```lisp
(add-to-list 'project-find-functions #'project-rootfile-try-detect)
```

You can customize `project-rootfile-list` to add your own root file.

## Credit

`project-rootfile-list` is heavily taken from Projectile's `projectile-project-root-files`. Thanks!

## License

GPLv3
