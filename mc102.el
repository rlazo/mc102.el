;;; mc102.el ---

;; Copyright (C) 2012  Rodrigo Lazo

;; Author: Rodrigo Lazo <rlazo.paz@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Mode for simplified SuSy (UNICAMP) based courses grading.

;;; Code:

(defgroup rl/mc102 nil
  "Grading tool for Susy, specially for course mc102."
  :prefix "rl/mc102-"
  :group 'tools)

(defcustom rl/mc102-working-dir
  "/home/rodrigo/mestrado/teaching/1s2012/mc102/subs/"
  "Directory where to unzip, or find, the susy generated
  submission directory hierarchy."
  :group 'rl/mc102
  :type 'directory)

(defcustom rl/mc102-website-dir
  "/home/rodrigo/mestrado/teaching/1s2012/website/"
  "Website dir."
  :group 'rl/mc102
  :type 'directory)

(defcustom rl/mc102-website-index
  "/home/rodrigo/mestrado/teaching/1s2012/website/notas.html"
  "Website index template."
  :group 'rl/mc102
  :type 'file)

(defcustom rl/mc102-compilation-tmp-dir
  "/var/tmp/susy_runner"
  "Temporal dir for compilation pourposes."
  :group 'rl/mc102
  :type 'directory)

(defcustom rl/mc102-homework-zip-dir
  "/home/rodrigo/mestrado/teaching/1s2012/mc102/tarefas/"
  "Homework dir"
  :group 'rl/mc102
  :type 'directory)

(defcustom rl/mc102-width 25
  "Window width"
  :group 'rl/mc102
  :type 'integer)

(defcustom rl/mc102-buffer-name "*mc102*"
  "Buffer name"
  :group 'rl/mc102
  :type 'string)

(defcustom rl/mc102-compiler-cmd "clang"
  "Compiler executable"
  :group 'rl/mc102
  :type 'string)

(defvar rl/mc102--current-turma nil)
(defvar rl/mc102--current-homework nil)
(defvar rl/mc102--solution-binary nil)
(defvar rl/mc102--student-solution-binary nil)

(defvar rl/mc102--grades (make-hash-table :test 'equal))
; (setq rl/mc102--grades (make-hash-table :test 'equal))

(defvar rl/mc102--grade-comments '())
; (setq rl/mc102--grade-comments '())

(defun rl/mc102--make-mode-map ()
  "Creates and returns a mode map with nav's key bindings."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'rl/mc102--quit)
    (define-key keymap "n" 'rl/mc102--next)
    (define-key keymap "p" 'rl/mc102--prev)
    (define-key keymap "c" 'rl/mc102--compile-file-at-point)
    (define-key keymap "w" 'rl/mc102--view-susy-result)
    (define-key keymap "e" 'rl/mc102--edit-grade)
    (define-key keymap "+" 'rl/mc102--add-grade-comment)
    (define-key keymap "v" 'rl/mc102--view-grade-file-at-point)
    (define-key keymap "=" 'rl/mc102--grade-file-at-point)
    (define-key keymap (kbd "RET") 'rl/mc102--open-at-point)
    (define-key keymap (kbd "SPC") 'scroll-other-window)
    (define-key keymap (kbd "DEL") 'scroll-other-window-down)
    keymap))

;; (setq rl/mc102-mode-map (rl/mc102--make-mode-map))
(defvar rl/mc102-mode-map
  (rl/mc102--make-mode-map)
  "Mode map for mc102 mode")

;; Common functions
;;-----------------
(defun rl/--join-path (path &rest args)
  (dolist (elt args path)
    (setq path (concat (file-name-as-directory path) elt))))

(defun rl/--join-directory-path (path &rest args)
  (file-name-as-directory (apply 'rl/--join-path path args)))

(defun rl/--unzip-file (archive target)
  (call-process "unzip" nil nil nil (expand-file-name archive)
                "-d" (expand-file-name target)))

(defun rl/mc102--find-code-fullpath (student)
  (car (directory-files
        (rl/--join-directory-path rl/mc102-working-dir
                                  rl/mc102--current-homework
                                  "subs" rl/mc102--current-turma
                                  student)
        t "[.]c$")))

;; Grading mode
;; ------------
(defun rl/mc102--create-grade-dir (dir)
  (make-directory dir t)
  (copy-file rl/mc102-website-index
             (rl/--join-path dir "index.html")))

(defun rl/mc102--grade-file-at-point ()
  (interactive)
  (with-output-to-temp-buffer "*rl/mc102-comments*"
    (princ "Registered comments\n----------------\n")
    (dotimes (counter (length rl/mc102--grade-comments))
      (princ (concat (number-to-string counter) ") "
                     (car (nth counter rl/mc102--grade-comments)) " ("
                     (number-to-string
                      (cdr (nth counter rl/mc102--grade-comments)))
                     ")\n"))))
  (let* ((student (substring (thing-at-point 'word) 2))
         (grade-cons (gethash student rl/mc102--grades (cons ""  0)))
         (index (string-to-number (read-string "Comment index: ")))
         (comment-cons (nth index rl/mc102--grade-comments))
         (comment (car comment-cons))
         (grade (cdr comment-cons)))
    (puthash student (cons (if (equal (car grade-cons) "")
                               (capitalize comment)
                             (concat (car grade-cons) ", " (downcase comment)))
                           (+ (cdr grade-cons) grade))
             rl/mc102--grades))
  (kill-buffer "*rl/mc102-comments*"))

(defun rl/mc102--view-grade-file-at-point ()
  (interactive)
  (let* ((student (substring (thing-at-point 'word) 2))
         (grade-cons (gethash student rl/mc102--grades (cons ""  0))))
    (message "%s: %s (%.02f pt)." student (car grade-cons) (cdr grade-cons))))

(defun rl/mc102--add-grade-comment (comment points)
  (interactive "sComment: \nnPoints: ")
  (setq rl/mc102--grade-comments (cons (cons comment points)
                                       rl/mc102--grade-comments)))

(defun rl/mc102--edit-grade ()
  (interactive)
  (let* ((student (substring (thing-at-point 'word) 2))
         (grade-cons (gethash student rl/mc102--grades (cons ""  0)))
         (comment (read-string "Comment: " (car grade-cons)))
         (grade (read-number "Grade:" (cdr grade-cons))))
    (puthash student (cons comment grade)
             rl/mc102--grades)))


(defun rl/mc102-dump-grades-at-point ()
  (interactive)
  (maphash (lambda (key value)
             (pp (cons key (cdr value)) 'insert))
           rl/mc102--grades))

(defun rl/mc102-dump-comments-at-point ()
  (interactive)
  (maphash (lambda (key value)
             (pp (cons key (car value)) 'insert))
           rl/mc102--grades))

(defun rl/mc102-generate-web-report()
  (interactive)
  (let* ((website-dir (rl/--join-path
                       rl/mc102-website-dir
                       (concat "lab" rl/mc102--current-homework)))
         (grade-file (rl/--join-path website-dir "notas.json")))
    (unless (file-exists-p website-dir)
      (rl/mc102--create-grade-dir website-dir))
    (with-current-buffer (find-file-noselect grade-file)
      (delete-region (point-min) (point-max))
      (set-buffer-file-coding-system 'iso-latin-1)
      (goto-char (point-min))
      (insert "{\n")
      (maphash (lambda (key value)
                 (insert "\"" key "\": \"" (car value) "\",\n"))
               rl/mc102--grades)
      (insert "}\n")
      (save-buffer)
      (kill-buffer))))


;; Compilation
;;--------------
(defun rl/mc102--compile-mkdir-tmp-dir ()
  (when (file-exists-p rl/mc102-compilation-tmp-dir)
    (delete-directory rl/mc102-compilation-tmp-dir t))
  (make-directory rl/mc102-compilation-tmp-dir))

(defun rl/mc102--compile-solution-filename ()
  (let ((solution-dir (concat rl/mc102-compilation-tmp-dir "/"
                              rl/mc102--current-homework "/sols/")))
    (car (directory-files solution-dir t "[.]c$"))))

(defun rl/mc102--compile-data-dir ()
  (concat rl/mc102-compilation-tmp-dir "/"
                              rl/mc102--current-homework "/dados/"))

(defun rl/mc102--compile-extract-homework-zip ()
  (let ((archive (concat rl/mc102-homework-zip-dir "/"
                         rl/mc102--current-homework ".zip")))
    (if (file-exists-p archive)
        (rl/--unzip-file archive rl/mc102-compilation-tmp-dir)
      (error "Zip file does not exist."))))

(defun rl/mc102--compile-file (filename destination)
  (call-process rl/mc102-compiler-cmd nil nil nil  filename
                "-o" destination))

(defun rl/mc102--compile-file-at-point ()
  (interactive)
  (message "Extracting data...")
  (rl/mc102--compile-mkdir-tmp-dir)
  (rl/mc102--compile-extract-homework-zip)
  (let* ((student (thing-at-point 'word))
         (code-file (rl/mc102--find-code-fullpath student))
         (solution-file (rl/mc102--compile-solution-filename))
         (data-dir (rl/mc102--compile-data-dir))
         (solution-bin (concat rl/mc102-compilation-tmp-dir "/sol-bin"))
         (solution-output (concat rl/mc102-compilation-tmp-dir "/output"))
         (student-solution-bin (concat rl/mc102-compilation-tmp-dir "/student-bin"))
         (student-solution-output (concat rl/mc102-compilation-tmp-dir "/student-output")))
    (message "Compiling..")
    (rl/mc102--compile-file solution-file solution-bin)
    (setq rl/mc102--solution-binary solution-bin)
    (rl/mc102--compile-file code-file student-solution-bin)
    (setq rl/mc102--student-solution-binary student-solution-bin)
    (make-directory solution-output)
    (make-directory student-solution-output)
    (message "Testing")
    (dolist (input-path (directory-files data-dir t "[.]in$"))
      (let ((filename (substring input-path
                                 (string-match  "arq[0-9]+\\.in" input-path) -3)))
        (call-process solution-bin
                      input-path `(:file ,(concat solution-output "/" filename ".out"))
                      nil)
        (call-process student-solution-bin
                      input-path `(:file ,(concat student-solution-output "/" filename ".out"))
                      nil)))
    (let ((output-buffer (get-buffer-create "*mc102-results*")))
      (with-current-buffer output-buffer
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))))
      (call-process "diff" nil output-buffer t "-s" "-r"
                    solution-output student-solution-output))
    (save-selected-window
      (view-buffer-other-window "*mc102-results*"))))

;; View mode
;; -----------
(defun rl/mc102--set-window-width (n)
  (let ((n (max n window-min-width)))
    (if (> (window-width) n)
        (rl/mc102--shrink-window-horizontally (- (window-width) n)))
    (if (< (window-width) n)
        (rl/mc102--enlarge-window-horizontally (- n (window-width))))))

(defun rl/mc102--shrink-window-horizontally (delta)
  "First, compute a new value for the delta to make sure we don't
make the window too small, according to the following equation:

window-width - delta' = max(window-min-width, window-width - delta)
"
  (let ((delta (- (window-width)
                  (max window-min-width
                       (- (window-width) delta)))))
    (shrink-window-horizontally delta)))

(defun rl/mc102--enlarge-window-horizontally (delta)
  (enlarge-window-horizontally delta))

(defun rl/mc102--prev()
  (interactive)
  (forward-line -1)
  (rl/mc102--open-at-point))

(defun rl/mc102--next()
  (interactive)
  (forward-line)
  (rl/mc102--open-at-point))

(defun rl/mc102--quit()
  (interactive)
  (kill-buffer)
  (delete-window))

(defun rl/mc102--open-at-point()
  (interactive)
  (let* ((student (thing-at-point 'word))
         (code-file (rl/mc102--find-code-fullpath student)))
    (save-selected-window
      (view-file-other-window code-file))))

(defun rl/mc102--view-susy-result ()
  (interactive)
  (let* ((student (thing-at-point 'word))
         (working-dir (concat  rl/mc102-working-dir "/" rl/mc102--current-homework
                               "/subs/" rl/mc102--current-turma "/"
                               student))
         (result-file (car (directory-files working-dir t "[.]html$"))))
    (if (not (file-exists-p result-file))
        (message "No html file found.")
      (save-selected-window
        (other-window 1)
        (w3m-find-file result-file)))))

(defun rl/mc102--decompress-archive (archive)
  (interactive "fZip file: ")
  (let* ((target-dir (concat rl/mc102-working-dir "/" rl/mc102--current-homework)))
    (make-directory target-dir t)
    (rl/--unzip-file archive target-dir)))

(defun rl/mc102--update-vars (turma homework)
  (interactive
   (list (read-string "Turma: " rl/mc102--current-turma)
         (read-string "Tarefa: " rl/mc102--current-homework)))
  (setq rl/mc102--current-turma turma
        rl/mc102--current-homework homework))

(defun rl/mc102--list-students()
  (let ((working-dir (concat  rl/mc102-working-dir "/" rl/mc102--current-homework
                              "/subs/" rl/mc102--current-turma)))
    (directory-files working-dir nil "^ra[0-9]+")))

(defun rl/mc102--refresh()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (student (rl/mc102--list-students))
      (insert (concat student "\n")))
    (goto-char (point-min))))


(define-derived-mode rl/mc102-mode fundamental-mode
  "calificar"
  (use-local-map rl/mc102-mode-map)
  (hl-line-mode 1)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;; (setq font-lock-defaults '(rl/mc102-font-lock-keywords))
  (rl/mc102--refresh))

(defun rl/mc102()
  (interactive)
  (call-interactively 'rl/mc102--update-vars)
  (when (yes-or-no-p "Decompress archive?")
    (call-interactively 'rl/mc102--decompress-archive))
  (split-window-horizontally)
  (switch-to-buffer (generate-new-buffer-name rl/mc102-buffer-name))
  (rl/mc102-mode)
  (rl/mc102--refresh)
  (rl/mc102--set-window-width rl/mc102-width))

(provide 'rl/mc102)
;;; mc102.el ends here
