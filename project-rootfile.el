;;; project-rootfile.el --- Project backend by root file  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>
;; URL: https://github.com/buzztaiki/project-rootfile.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A project backend that uses a root file (e.g. Gemfile) for detection.

;; Usage:
;; If you prefer VCS root over root file for project detection, add the following to your init file:
;;
;;      (add-to-list 'project-find-functions #'project-rootfile-try t)
;;
;; Otherwise, if you prefer a root file, add the following:
;;
;;      (add-to-list 'project-find-functions #'project-rootfile-try)

;; TODO:
;; - Support Emacs version < 28.1?
;;   `project-root' is introduced from this version
;; - Add more files to `project-rootfile-list'.

;;; Code:

(require 'project)
(require 'cl-lib)

(defgroup project-rootfile nil
  "Project backend by root file."
  :group 'project)

(defcustom project-rootfile-list
  '("Makefile" "CMakeLists.txt" "go.mod" "Gemfile" "package.json" "pom.xml" "build.gradle" "Cargo.toml")
  "List of files from which a project root."
  :group 'project-rootfile
  :type '(repeat :type string))

(cl-defstruct project-rootfile
  "Project backend by root file."
  root base)


;;;###autoload
;; TODO: naming
(defun project-rootfile-try (dir)
  "Return an instance of `project-rootfile' if determine DIR is it's target."
  (let ((roots (delq nil (mapcar (lambda (x) (locate-dominating-file dir x)) project-rootfile-list))))
    (and roots
         (make-project-rootfile :root (car (sort roots (lambda (a b) (> (length a) (length b)))))
                                :base (project-try-vc dir)))))

               
(cl-defmethod project-root ((project project-rootfile))
  "Return root directory of the current PROJECT."
  (project-rootfile-root project))

(cl-defmethod project-external-roots ((_project project-rootfile))
  "Return the list of external roots for PROJECT."
  (cl-call-next-method))

(cl-defmethod project-ignores ((project project-rootfile) dir)
  "Return the list of glob patterns to ignore inside DIR for PROJECT."
  (let ((base (project-rootfile-base project)))
    (if (null base)
        (cl-call-next-method)
      (project-ignores base dir))))

(cl-defmethod project-files ((project project-rootfile) &optional dirs)
  "Return a list of files in directories DIRS in PROJECT."
  (let ((base (project-rootfile-base project))
        (dirs (or dirs (list (project-root project)))))
    (if (null base)
        (cl-call-next-method)
      (project-files base (or dirs (list (project-root project)))))))

(provide 'project-rootfile)
;;; project-rootfile.el ends here
