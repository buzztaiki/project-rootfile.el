;;; project-rootfile-tests.el ---                    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Taiki Sugawara

;; Author: Taiki Sugawara <buzz.taiki@gmail.com>

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

;;

;;; Code:

(require 'ert)
(require 'vc)                           ; to register vc callbacks
(require 'project-rootfile)


(cl-defmacro project-rootfile-tests-with-setup ((dirvar &key (inside-git nil) (touch nil)) &rest body)
  "Setup test and eval BODY with temporary directory DIRVAR."
  (declare (indent 1) (debug ((symbolp) body)))
  (unless (listp touch)
    (setq touch (list touch)))
  `(let ((,dirvar (expand-file-name (make-temp-name "project-rootfile-") temporary-file-directory))
         (project-rootfile-list '("Makefile" "debian/control")))
     (unwind-protect
         (progn
           (make-directory ,dirvar)
           ,(when inside-git
              `(let ((default-directory ,dirvar))
                 (call-process-shell-command "git init")))
           ,@(mapcar (lambda (x) `(make-empty-file (expand-file-name ,x ,dirvar) t)) touch)
           ,@body)
       (delete-directory ,dirvar t))))

(defun project-rootfile-tests-root-equal (project dir)
  "Return non-nil if root of PROJECT is DIR."
  (file-equal-p (project-rootfile--project-root project) (file-name-as-directory dir)))

(ert-deftest test-project-rootfile-try-detect ()
  (project-rootfile-tests-with-setup (dir)
    (should (null (project-rootfile-try-detect dir)))

    (make-empty-file (expand-file-name "Makefile" dir))
    (let ((project (project-rootfile-try-detect dir)))
      (should (cl-typep project 'project-rootfile-plain))
      (should (project-rootfile-tests-root-equal project dir))
      (should (project-rootfile-tests-root-equal project dir)))))

(ert-deftest test-project-rootfile-try-detect/inside-child-dir ()
  (project-rootfile-tests-with-setup (dir :touch "Makefile")
    (make-directory (expand-file-name "child" dir))
    (let ((project (project-rootfile-try-detect (expand-file-name "child" dir))))
      (should (project-rootfile-tests-root-equal project dir)))))

(ert-deftest test-project-rootfile-try-detect/nested-root-file ()
  (project-rootfile-tests-with-setup (dir :touch "debian/control")
    (let ((project (project-rootfile-try-detect dir)))
      (should (project-rootfile-tests-root-equal project dir)))))

(ert-deftest test-project-rootfile-try-detect/inside-git ()
  (project-rootfile-tests-with-setup (dir :inside-git t :touch "Makefile")
    (let ((project (project-rootfile-try-detect dir)))
      (should (eq (car project) 'vc))
      (should (project-rootfile-tests-root-equal project dir)))))

(ert-deftest test-project-rootfile-try-detect/git-monorepo ()
  (project-rootfile-tests-with-setup (dir :inside-git t :touch ("sub1/Makefile" "sub2/Makefile"))
    (dolist (sub '("sub1" "sub2"))
      (let ((project (project-rootfile-try-detect (expand-file-name sub dir))))
        (should (eq 'vc (car project)))
        (should (project-rootfile-tests-root-equal project (expand-file-name sub dir)))))))

(ert-deftest test-project-rootfile-try-detect/stop-detection-at-vcs-directory ()
  (project-rootfile-tests-with-setup (dir :touch "Makefile")
    (make-directory (expand-file-name "vcs/child" dir) t)
    (let ((default-directory (expand-file-name "vcs" dir)))
      (call-process-shell-command "git init"))
    (should (project-rootfile-try-detect dir))
    (should-not (project-rootfile-try-detect (expand-file-name "vcs" dir)))
    (should-not (project-rootfile-try-detect (expand-file-name "vcs/child" dir)))))

(ert-deftest test-project-rootfile/project-current ()
  (project-rootfile-tests-with-setup (dir :touch "Makefile")
    (let ((project-find-functions '(project-rootfile-try-detect)))
      (should (project-current nil dir)))))

(ert-deftest test-project-rootfile/project-ignores/plain ()
  (project-rootfile-tests-with-setup (dir :touch "Makefile")
    (let ((project (project-rootfile-try-detect dir))
          (project-rootfile-plain-ignores '("ignore")))
      (should (member "*~" (project-ignores project dir)))
      (should (member "ignore" (project-ignores project dir))))))

(ert-deftest test-project-rootfile/project-ignores/inside-git ()
  ;; skip when emacs29. see https://github.com/emacs-mirror/emacs/commit/c640e978874385f9774c2903b97677406bee97a2
  (skip-unless (not (emacs-major-version emacs-major-version 29)))

  (project-rootfile-tests-with-setup (dir :inside-git t :touch "Makefile")
    (let ((project (project-rootfile-try-detect dir)))
      (should (not (member "ignore" (project-ignores project dir))))
      (with-temp-file (expand-file-name ".gitignore" dir)
        (insert "ignore"))
      (should (member "ignore" (project-ignores project dir))))))

(ert-deftest test-project-rootfile/project-files ()
  (project-rootfile-tests-with-setup (dir :touch "Makefile")
    (let ((project (project-rootfile-try-detect dir)))
      (should (equal (project-files project) (list (expand-file-name "Makefile" dir))))
      (should (equal (project-files project (list dir)) (list (expand-file-name "Makefile" dir)))))))

(ert-deftest test-project-rootfile/project-files/inside-git ()
  (project-rootfile-tests-with-setup (dir :inside-git t :touch "Makefile")
    (let ((project (project-rootfile-try-detect dir)))
      (should (equal (project-files project) (list (expand-file-name "Makefile" dir))))
      (should (equal (project-files project (list dir)) (list (expand-file-name "Makefile" dir)))))))

(ert-deftest test-project-rootfile/project-files/git-monorepo ()
  (project-rootfile-tests-with-setup (dir :inside-git t :touch ("README.md" "sub/Makefile" "sub/ignore"))
    (with-temp-file (expand-file-name "sub/.gitignore" dir)
      (insert "ignore"))
    (let ((project (project-rootfile-try-detect (expand-file-name "sub" dir))))
      (should (seq-set-equal-p (project-files project) (list (expand-file-name "sub/Makefile" dir)
                                                             (expand-file-name "sub/.gitignore" dir)))))))

(provide 'project-rootfile-tests)
;;; project-rootfile-tests.el ends here
