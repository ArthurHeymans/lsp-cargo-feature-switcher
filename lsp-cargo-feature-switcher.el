;;; cargo-features.el --- Toggle Cargo features via LSP configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Arthur Heymans <arthur@aheymans.xyz>

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; URL: https://github.com/ArthurHeymans/lsp-cargo-feature-switcher
;; Keywords: rust, cargo, lsp
;; Package: lsp-cargo-feature-switcher
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (tomlparse "1.0.0"))

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package provides an interactive interface for toggling Cargo features
;; in Rust projects by updating LSP configuration in .dir-locals.el files.
;;
;; Usage:
;;   M-x cargo-features-toggle-menu
;;
;;; Code:

(require 'tomlparse)
(require 'files-x)

(defcustom cargo-features-lsp-mode 'eglot
  "LSP client mode to use for Cargo feature configuration.
Can be either 'lsp-mode or 'eglot."
  :type '(choice (const :tag "lsp-mode" lsp-mode)
                 (const :tag "eglot" eglot))
  :group 'cargo-features)

(defun cargo-features-find-closest-toml (&optional dir)
  "Find the closest Cargo.toml file starting from DIR or current directory."
  (let ((start-dir (or dir default-directory)))
    (locate-dominating-file start-dir "Cargo.toml")))

(defun cargo-features-parse-available (cargo-toml-path)
  "Parse available features from Cargo.toml at CARGO-TOML-PATH.
Returns a list of (feature-name . (same-crate-features dep-features))."
  (when (and cargo-toml-path (file-exists-p cargo-toml-path))
    (let* ((parsed-toml (tomlparse-file cargo-toml-path :object-type 'alist))
           (features-section (assoc 'features parsed-toml))
           (all-features '()))
      
      (when features-section
        (dolist (feature (cdr features-section))
          (when (consp feature)
            (let* ((feature-name (symbol-name (car feature)))
                   (feature-def (cdr feature))
                   (same-crate-features '())
                   (dep-features '())
                   (all-deps '()))
              (setq all-deps
                    (cond
                     ((null feature-def) nil)
                     ((vectorp feature-def) (append feature-def nil))
                     ((listp feature-def) feature-def)
                     ((stringp feature-def) (list feature-def))
                     (t nil)))
              
              (dolist (dep all-deps)
                (when (stringp dep)
                  (if (string-match-p "/" dep)
                      (push dep dep-features)
                    (push dep same-crate-features))))
              
              (push (cons feature-name (list (nreverse same-crate-features) (nreverse dep-features))) all-features)))))
      
      (nreverse all-features))))

(defun cargo-features-compute-transitive (selected-features no-default-features available-features)
  "Compute all transitively enabled features.
SELECTED-FEATURES is a list of explicitly selected feature names.
NO-DEFAULT-FEATURES is a boolean.
AVAILABLE-FEATURES is the parsed feature list from cargo-features-parse-available.
Returns an alist mapping feature-name to either nil (directly selected) or the name of the feature that enabled it."
  (let ((enabled-by (make-hash-table :test 'equal))
        (to-process '()))
    
    (let ((initial-features (if no-default-features
                                selected-features
                              (delete-dups (cons "default" selected-features)))))
      (dolist (feat initial-features)
        (puthash feat nil enabled-by)
        (push feat to-process)))
    
    (while to-process
      (let* ((current (pop to-process))
             (feature-entry (assoc current available-features)))
        (when feature-entry
          (let* ((feature-data (cdr feature-entry))
                 (same-crate-features (car feature-data)))
            (dolist (dep same-crate-features)
              (unless (gethash dep enabled-by)
                (puthash dep current enabled-by)
                (push dep to-process)))))))
    
    (let ((result '()))
      (maphash (lambda (feat enabled-by-feat)
                 (push (cons feat enabled-by-feat) result))
               enabled-by)
      result)))

(defun cargo-features-find-project-root (&optional dir)
  "Find project root directory starting from DIR or current directory.
For eglot mode, looks for .git directory first, falls back to Cargo.toml directory.
For lsp-mode, uses Cargo.toml directory."
  (let ((start-dir (or dir default-directory)))
    (if (eq cargo-features-lsp-mode 'eglot)
        (or (locate-dominating-file start-dir ".git")
            (cargo-features-find-closest-toml start-dir))
      (cargo-features-find-closest-toml start-dir))))

(defun cargo-features-find-dir-locals (&optional dir)
  "Find .dir-locals.el file in DIR or closest project root."
  (when-let ((project-root (cargo-features-find-project-root dir)))
    (expand-file-name ".dir-locals.el" project-root)))

(defun cargo-features--revert-dir-locals-buffer (dir-locals-file)
  "Revert the buffer visiting DIR-LOCALS-FILE if it exists."
  (when-let ((buf (find-buffer-visiting dir-locals-file)))
    (with-current-buffer buf
      (revert-buffer t t t))))

(defun cargo-features--refresh-rustic-buffers (project-root local-vars restart-fn)
  "Refresh rustic-mode buffers in PROJECT-ROOT.
LOCAL-VARS is a list of variables to kill, RESTART-FN is the LSP restart function."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'rustic-mode)
                 (string-prefix-p project-root default-directory))
        (dolist (var local-vars)
          (kill-local-variable var))
        (hack-local-variables)
        (when restart-fn
          (funcall restart-fn))))))

(defun cargo-features-get-current-from-file (dir-locals-file)
  "Get current LSP/eglot rust features and no-default-features setting from DIR-LOCALS-FILE.
Returns (features . no-default-features)."
  (when (and dir-locals-file (file-exists-p dir-locals-file))
    (condition-case nil
        (let* ((dir (file-name-directory dir-locals-file))
               (class (dir-locals-read-from-dir dir))
               (variables (when class (dir-locals-get-class-variables class)))
               (rustic-config (assoc 'rustic-mode variables))
               (lsp-features-config (when rustic-config 
                                      (assoc 'lsp-rust-features (cdr rustic-config))))
               (lsp-no-default-config (when rustic-config
                                        (assoc 'lsp-rust-no-default-features (cdr rustic-config))))
               (eglot-config (when rustic-config
                               (assoc 'eglot-workspace-configuration (cdr rustic-config))))
               (eglot-data (when eglot-config
                             (let* ((config-plist (cdr eglot-config))
                                    (rust-analyzer (plist-get config-plist :rust-analyzer))
                                    (cargo (plist-get rust-analyzer :cargo)))
                               (cons (plist-get cargo :features)
                                     (plist-get cargo :noDefaultFeatures)))))
                (features (cond
                           (lsp-features-config (cdr lsp-features-config))
                           (eglot-data 
                            (let ((f (car eglot-data)))
                              (and (vectorp f) (append f nil))))))
                (no-default (or (and lsp-no-default-config (cdr lsp-no-default-config))
                                (and eglot-data (cdr eglot-data)))))
           (cons (and features (if (vectorp features) (append features nil) features))
                 no-default))
      (error nil))))

(defun cargo-features-get-current (&optional dir)
  "Get current LSP rust features and no-default-features from .dir-locals.el in DIR or current directory.
Returns (features . no-default-features)."
  (let ((dir-locals-file (cargo-features-find-dir-locals dir)))
    (if dir-locals-file
        (cargo-features-get-current-from-file dir-locals-file)
      (cons nil nil))))

(defun cargo-features-update-dir-locals-lsp-mode (features no-default-features project-root)
  "Update .dir-locals.el with lsp-mode configuration.
FEATURES is the list of features, NO-DEFAULT-FEATURES is boolean, PROJECT-ROOT is the directory."
  (let* ((dir-locals-file (expand-file-name ".dir-locals.el" project-root))
         (default-directory project-root))
    (save-window-excursion
      (modify-dir-local-variable 'rustic-mode 'lsp-rust-features (vconcat features) 'add-or-replace dir-locals-file)
      (modify-dir-local-variable 'rustic-mode 'lsp-rust-no-default-features no-default-features 'add-or-replace dir-locals-file)
      (save-buffer))
    (cargo-features--revert-dir-locals-buffer dir-locals-file)
    (cargo-features--refresh-rustic-buffers 
     project-root
     '(lsp-rust-features lsp-rust-no-default-features)
     (when (fboundp 'lsp-workspace-restart)
       (lambda () (call-interactively 'lsp-workspace-restart))))))

(defun cargo-features-update-dir-locals-eglot (features no-default-features project-root)
  "Update .dir-locals.el with eglot configuration.
FEATURES is the list of features, NO-DEFAULT-FEATURES is boolean, PROJECT-ROOT is the directory."
  (let* ((dir-locals-file (expand-file-name ".dir-locals.el" project-root))
         (default-directory project-root)
         (cargo-config (list :features (vconcat features)))
         (cargo-config (if no-default-features
                          (plist-put cargo-config :noDefaultFeatures t)
                        cargo-config))
         (eglot-config (list :rust-analyzer
                            (list :cargo cargo-config))))
    (save-window-excursion
      (modify-dir-local-variable 'rustic-mode 'eglot-workspace-configuration eglot-config 'add-or-replace dir-locals-file)
      (save-buffer))
    (cargo-features--revert-dir-locals-buffer dir-locals-file)
    (cargo-features--refresh-rustic-buffers
     project-root
     '(eglot-workspace-configuration)
     (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
       (lambda () (call-interactively 'eglot-reconnect))))))

(defun cargo-features-update-dir-locals (features no-default-features &optional dir)
  "Update .dir-locals.el with FEATURES list and NO-DEFAULT-FEATURES in DIR or current directory.
Uses either lsp-mode or eglot configuration based on `cargo-features-lsp-mode'."
  (let ((project-root (or dir (cargo-features-find-project-root))))
    (if (eq cargo-features-lsp-mode 'eglot)
        (cargo-features-update-dir-locals-eglot features no-default-features project-root)
      (cargo-features-update-dir-locals-lsp-mode features no-default-features project-root))))

;;;###autoload
(defun cargo-features-toggle-menu ()
  "Interactive menu to toggle Cargo features."
  (interactive)
  (if-let* ((cargo-dir (cargo-features-find-closest-toml))
            (cargo-toml (expand-file-name "Cargo.toml" cargo-dir)))
      (progn
        (unless (file-exists-p cargo-toml)
          (user-error "Cargo.toml file does not exist: %s" cargo-toml))
        (let* ((available-features (cargo-features-parse-available cargo-toml))
           (current-state (cargo-features-get-current))
           (current-features (or (car current-state) '()))
           (no-default-features (cdr current-state))
           (transitive-map (cargo-features-compute-transitive current-features no-default-features available-features))
           (prompt "Toggle Cargo features:")
           (candidates (mapcar (lambda (feature)
                                 (let* ((name (car feature))
                                        (feature-data (cdr feature))
                                        (same-crate-features (car feature-data))
                                        (dep-features (cadr feature-data))
                                        (transitive-entry (assoc name transitive-map))
                                        (enabled-by (when transitive-entry (cdr transitive-entry)))
                                        (is-default (string= name "default"))
                                        (enabled-str (if transitive-entry
                                                         (if enabled-by
                                                             (format "[ENABLED BY: %s]" enabled-by)
                                                           (if is-default "[DEFAULT]" "[SELECTED]"))
                                                       ""))
                                        (all-deps (append same-crate-features dep-features))
                                        (dep-str (if all-deps
                                                     (format "dep: [ %s ]" (string-join all-deps " "))
                                                   "")))
                                   (format "%-30s %-25s %s" name enabled-str dep-str)))
                               available-features))
           (selection (completing-read prompt candidates nil t)))
      
      (when selection
        (let* ((selected-name (car (split-string selection)))
               (selected-feature (assoc selected-name available-features))
               (is-default (string= selected-name "default")))
          (when selected-feature
            (if is-default
                (let ((new-no-default (not no-default-features)))
                  (cargo-features-update-dir-locals current-features new-no-default)
                  (message "Updated LSP rust no-default-features: %s" new-no-default))
              (let ((new-features 
                     (if (member selected-name current-features)
                         (remove selected-name current-features)
                       (sort (append current-features (list selected-name)) 'string<))))
                (cargo-features-update-dir-locals new-features no-default-features)
                (message "Updated LSP rust features: %s" 
                         (string-join new-features " ")))))))))
    (user-error "No Cargo.toml found in current or parent directories")))

(provide 'lsp-cargo-feature-switcher)
;;; cargo-features.el ends here
