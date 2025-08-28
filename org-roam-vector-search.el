;;; org-roam-vector-search.el --- Vector embeddings and AI assistance for org-roam -*- lexical-binding: t; -*-
;;; Version: 1.0.0
;;;
;;; Commentary:
;; This package adds vector embedding support and direct AI integration to org-roam.
;; It stores embeddings as org properties and provides semantic similarity search.
;; Requires Ollama with nomic-embed-text model for embeddings.

;;; Code:

(require 'org-roam)
(require 'json)
(require 'url)
(require 'org)
(require 'ox-md)

;;; Version

(defconst org-roam-semantic-version "1.0.0"
  "Version of the org-roam-semantic package suite.")

(defun org-roam-semantic-version ()
  "Display the version of org-roam-semantic."
  (interactive)
  (message "org-roam-semantic version %s" org-roam-semantic-version))

;;; Configuration

(defgroup org-roam-vector-search nil
  "Vector embeddings and semantic search for org-roam."
  :group 'org-roam
  :prefix "my/")

(defcustom my/ollama-base-url "http://localhost:11434"
  "Base URL for Ollama API server.
This should point to your Ollama installation. Common values:
- http://localhost:11434 (local installation)
- http://your-server:11434 (remote server)"
  :type 'string
  :group 'org-roam-vector-search)

(defcustom my/embedding-model "nomic-embed-text"
  "Model to use for generating embeddings.
Must be available in your Ollama installation.
Recommended models:
- nomic-embed-text (default, good balance)
- all-minilm (smaller, faster)
- mxbai-embed-large (larger, more accurate)"
  :type 'string
  :group 'org-roam-vector-search)

(defcustom my/generation-model "llama3.1:8b"
  "Model to use for text generation and AI assistance.
Must be available in your Ollama installation.
Popular models:
- llama3.1:8b (recommended default)
- qwen2.5:7b (good alternative)
- gemma2:9b (Google's model)"
  :type 'string
  :group 'org-roam-vector-search)

(defcustom my/embedding-dimensions 768
  "Number of dimensions in the embedding vectors.
This should match your embedding model:
- nomic-embed-text: 768
- all-minilm: 384
- mxbai-embed-large: 1024
Change this only if you switch embedding models."
  :type 'integer
  :group 'org-roam-vector-search)

;;; Utility Functions

(defun my/get-similar-notes-data (query-text &optional limit)
  "Get similarity data programmatically (returns list of (file similarity) pairs).
This is a non-interactive version of my/find-similar-notes for use by other functions."
  (let ((limit (or limit 10))
        (similarities '()))
    ;; Generate embedding for query
    (let ((query-embedding (my/call-ollama-embeddings-sync query-text)))
      (if query-embedding
          (progn
            ;; Compare with all notes that have embeddings
            (dolist (file (org-roam-list-files))
              (let ((note-embedding (my/get-embedding-from-note file)))
                (when note-embedding
                  (let ((similarity (my/cosine-similarity query-embedding note-embedding)))
                    (when similarity
                      (push (list file similarity) similarities))))))
            ;; Sort by similarity and take top results
            (setq similarities (sort similarities (lambda (a b) (> (cadr a) (cadr b)))))
            (if (> (length similarities) limit)
                (butlast similarities (- (length similarities) limit))
              similarities))
        (message "Failed to generate embedding for query")
        nil))))

(defun my/normalize-text (text)
  "Normalize text for embedding by removing extra whitespace and formatting."
  (when text
    (let ((normalized (replace-regexp-in-string "[ \t\n\r]+" " " text)))
      (string-trim normalized))))

(defun my/get-note-content (file)
  "Extract the main content of an org-roam note, excluding properties and metadata."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    ;; Skip past the properties drawer and title
    (when (re-search-forward "^:END:" nil t)
      (forward-line 1))
    ;; Get content from here to end of buffer
    (let ((content (buffer-substring (point) (point-max))))
      (my/normalize-text content))))

(defun my/vector-magnitude (vector)
  "Calculate the magnitude of a vector."
  (sqrt (apply '+ (mapcar (lambda (x) (* x x)) vector))))

(defun my/cosine-similarity (vec1 vec2)
  "Calculate cosine similarity between two vectors."
  (condition-case err
      (when (and vec1 vec2
                 (listp vec1) (listp vec2)
                 (= (length vec1) (length vec2))
                 (> (length vec1) 0))
        (let* ((dot-product 0.0)
               (mag1-sq 0.0)
               (mag2-sq 0.0)
               (valid-count 0))
          ;; Calculate dot product and magnitudes in one pass
          (dotimes (i (length vec1))
            (let ((v1 (nth i vec1))
                  (v2 (nth i vec2)))
              (when (and (numberp v1) (numberp v2))
                (setq dot-product (+ dot-product (* v1 v2)))
                (setq mag1-sq (+ mag1-sq (* v1 v1)))
                (setq mag2-sq (+ mag2-sq (* v2 v2)))
                (setq valid-count (1+ valid-count)))))
          ;; Only calculate if we have valid numbers
          (when (> valid-count 0)
            (let ((mag1 (sqrt mag1-sq))
                  (mag2 (sqrt mag2-sq)))
              (if (or (zerop mag1) (zerop mag2))
                  0.0
                (/ dot-product (* mag1 mag2)))))))
    (error
     (message "Error in cosine similarity calculation: %s" err)
     nil)))

;;; Ollama API Functions

(defun my/call-ollama-embeddings-sync (text)
  "Call Ollama embeddings API synchronously using built-in URL functions."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string
                           (json-encode `((model . ,my/embedding-model)
                                        (prompt . ,text)))
                           'utf-8))
        (url (concat my/ollama-base-url "/api/embeddings")))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (let* ((json-response (decode-coding-string
                                (buffer-substring (point) (point-max)) 'utf-8))
                 (data (json-read-from-string json-response))
                 (embedding (cdr (assoc 'embedding data))))
            (kill-buffer (current-buffer))
            ;; Convert vector to list if necessary
            (if (vectorp embedding)
                (append embedding nil)
              embedding)))
      (error
       (message "Error calling Ollama embeddings: %s" err)
       nil))))

(defun my/call-ollama-generate-sync (prompt &optional system-prompt)
  "Call Ollama generate API synchronously using built-in URL functions."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string
                           (json-encode `((model . ,my/generation-model)
                                        (prompt . ,prompt)
                                        (system . ,(or system-prompt ""))
                                        (stream . nil)))
                           'utf-8))
        (url (concat my/ollama-base-url "/api/generate")))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (let* ((json-response (decode-coding-string
                                (buffer-substring (point) (point-max)) 'utf-8))
                 (data (json-read-from-string json-response))
                 (response (cdr (assoc 'response data))))
            (kill-buffer (current-buffer))
            response))
      (error
       (message "Error calling Ollama generate: %s" err)
       nil))))

;;; Embedding Storage and Retrieval

(defun my/store-embedding-in-note (file embedding)
  "Store embedding vector in the note's properties."
  (when embedding
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        ;; Find or create properties drawer
        (unless (re-search-forward "^[ \t]*:PROPERTIES:" nil t)
          ;; If no properties drawer exists, create one after the title
          (goto-char (point-min))
          (when (re-search-forward "^#\\+title:" nil t)
            (end-of-line)
            (insert "\n:PROPERTIES:\n:END:")))
        ;; Find the properties drawer
        (goto-char (point-min))
        (when (re-search-forward "^[ \t]*:PROPERTIES:" nil t)
          (let ((props-start (point))
                (props-end (when (re-search-forward "^[ \t]*:END:" nil t)
                            (match-beginning 0))))
            (when props-end
              ;; Remove existing embedding property if it exists
              (goto-char props-start)
              (when (re-search-forward "^[ \t]*:EMBEDDING:.*$" props-end t)
                (delete-region (match-beginning 0) (1+ (match-end 0))))
              ;; Add new embedding property
              (goto-char props-end)
              (insert (format ":EMBEDDING: %s\n"
                            (mapconcat 'number-to-string embedding " "))))))
        (save-buffer)))))

(defun my/get-embedding-from-note (file)
  "Retrieve embedding vector from note's properties."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*:EMBEDDING:[ \t]*\\(.*\\)$" nil t)
      (let ((embedding-str (match-string 1)))
        (when (and embedding-str (not (string-empty-p embedding-str)))
          (condition-case err
              (mapcar 'string-to-number (split-string embedding-str))
            (error
             (message "Error parsing embedding in %s: %s" (file-name-nondirectory file) err)
             nil)))))))

(defun my/note-has-embedding-p (file)
  "Check if note already has an embedding."
  (not (null (my/get-embedding-from-note file))))

;;; Main Embedding Functions

(defun my/generate-embedding (text)
  "Generate embedding for text synchronously."
  (my/call-ollama-embeddings-sync text))

(defun my/generate-embedding-for-note (file)
  "Generate and store embedding for a single note."
  (interactive (list (buffer-file-name)))
  (unless file
    (error "No file associated with current buffer"))
  (let ((content (my/get-note-content file)))
    (if content
        (progn
          (message "Generating embedding for %s..." (file-name-nondirectory file))
          (let ((embedding (my/call-ollama-embeddings-sync content)))
            (if embedding
                (progn
                  (my/store-embedding-in-note file embedding)
                  (message "Embedding generated and stored for %s"
                         (file-name-nondirectory file)))
              (message "Failed to generate embedding for %s"
                     (file-name-nondirectory file)))))
      (message "No content found in %s" (file-name-nondirectory file)))))

(defun my/generate-embeddings-for-all-notes ()
  "Generate embeddings for all org-roam notes that don't have them."
  (interactive)
  (let* ((files (org-roam-list-files))
         (total (length files))
         (processed 0)
         (skipped 0))
    (message "Starting embedding generation for %d notes..." total)
    (dolist (file files)
      (if (my/note-has-embedding-p file)
          (progn
            (cl-incf skipped)
            (message "Skipping %s (already has embedding) [%d/%d]"
                   (file-name-nondirectory file) (+ processed skipped) total))
        (let ((content (my/get-note-content file)))
          (if content
              (progn
                (cl-incf processed)
                (message "Processing %s [%d/%d]..."
                       (file-name-nondirectory file) (+ processed skipped) total)
                (let ((embedding (my/call-ollama-embeddings-sync content)))
                  (if embedding
                      (my/store-embedding-in-note file embedding)
                    (message "Failed to generate embedding for %s"
                           (file-name-nondirectory file)))))
            (cl-incf skipped)
            (message "Skipping %s (no content) [%d/%d]"
                   (file-name-nondirectory file) (+ processed skipped) total)))))
    (message "Embedding generation complete: %d processed, %d skipped"
           processed skipped)))

;;; Vector Search Functions

(defun my/get-note-title-from-file (file)
  "Extract the title from an org-roam note file."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (if (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
            (string-trim (match-string 1))
          (file-name-sans-extension (file-name-nondirectory file))))
    (error (file-name-sans-extension (file-name-nondirectory file)))))

(defun my/get-node-id-from-file (file)
  "Extract the node ID from an org-roam note file."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward "^:ID:\\s-*\\(.+\\)$" nil t)
          (string-trim (match-string 1))))
    (error nil)))

(defun my/find-similar-notes (query-text &optional limit)
  "Find notes similar to the query text and display in a results buffer with clickable links."
  (interactive "sSearch for concept: ")
  (let ((similarities (my/get-similar-notes-data query-text (or limit 10))))
    (if similarities
        (with-current-buffer (get-buffer-create "*Similar Notes*")
          (erase-buffer)
          (org-mode) ; Enable org-mode for clickable links
          (insert (format "* Similar notes for: %s\n\n" query-text))
          (insert "Click links to open notes, or copy the org-roam links below:\n\n")

          (dolist (result similarities)
            (let* ((file (car result))
                   (similarity (cadr result))
                   (title (my/get-note-title-from-file file))
                   (node-id (my/get-node-id-from-file file)))
              ;; Insert clickable file link and copyable org-roam link
              (insert (format "** %.3f - [[file:%s][%s]]\n" similarity file title))
              (when node-id
                (insert (format "   Org-roam link: =[[id:%s][%s]]=\n" node-id title)))
              (insert "\n")))

          (insert "\n** Usage:\n")
          (insert "- Click file links to open notes\n")
          (insert "- Copy org-roam links (the =[[id:...]]= parts) to insert elsewhere\n")
          (insert "- Use C-c C-c on org-roam links to follow them\n")

          (goto-char (point-min))
          (display-buffer (current-buffer))
          (message "Found %d similar notes - click links to open" (length similarities)))
      (message "No similar notes found"))))

(defun my/find-similar-notes-and-insert (query-text &optional limit)
  "Find similar notes and insert org-roam links into the current buffer."
  (interactive "sSearch for concept: ")
  (if (not (derived-mode-p 'org-mode))
      (message "This function only works in org-mode buffers")
    (let ((similarities (my/get-similar-notes-data query-text (or limit 5))))
      (if similarities
          (progn
            (insert (format "\n** Related Notes - %s\n" query-text))
            (dolist (result similarities)
              (let* ((file (car result))
                     (similarity (cadr result))
                     (title (my/get-note-title-from-file file))
                     (node-id (my/get-node-id-from-file file)))
                (if node-id
                    (insert (format "- [[id:%s][%s]] (%.3f)\n" node-id title similarity))
                  (insert (format "- [[file:%s][%s]] (%.3f)\n" file title similarity)))))
            (insert "\n")
            (message "Inserted %d similar note links" (length similarities)))
        (message "No similar notes found")))))

(defun my/search-notes-by-concept (concept)
  "Interactive search for notes by concept - displays results buffer."
  (interactive "sConcept to search for: ")
  (my/find-similar-notes concept))

(defun my/insert-related-notes (concept)
  "Search for related notes and insert links at point."
  (interactive "sConcept to find related notes for: ")
  (my/find-similar-notes-and-insert concept))

(defun my/insert-similar-notes (&optional limit)
  "Find notes similar to current note and insert org-roam links at point."
  (interactive "P")
  (if (not (and (derived-mode-p 'org-mode) (org-roam-file-p)))
      (message "This function only works in org-roam files")
    (let* ((current-file (buffer-file-name))
           (title (or (org-roam-get-keyword "TITLE")
                     (file-name-sans-extension (file-name-nondirectory current-file))))
           (content (my/get-note-content current-file))
           (query-text (or content title))
           (limit (or limit 5))
           (similarities (my/get-similar-notes-data query-text (1+ limit)))) ; Get one extra to exclude current note

      ;; Filter out the current note from results
      (setq similarities (seq-remove (lambda (result)
                                      (string= (car result) current-file))
                                    similarities))
      ;; Take the requested number after filtering
      (setq similarities (seq-take similarities limit))

      (if similarities
          (progn
            (insert (format "\n** Related Notes\n"))
            (dolist (result similarities)
              (let* ((file (car result))
                     (similarity (cadr result))
                     (title (my/get-note-title-from-file file))
                     (node-id (my/get-node-id-from-file file)))
                (if node-id
                    (insert (format "- [[id:%s][%s]] (%.3f)\n" node-id title similarity))
                  (insert (format "- [[file:%s][%s]] (%.3f)\n" file title similarity)))))
            (insert "\n")
            (message "Inserted %d similar note links" (length similarities)))
        (message "No similar notes found")))))

;;; Status and Maintenance Functions

(defun my/debug-embedding (file)
  "Debug embedding for a specific file."
  (interactive (list (read-file-name "Check embedding for file: "
                                     org-roam-directory nil t)))
  (let ((embedding (my/get-embedding-from-note file)))
    (if embedding
        (message "File: %s\nEmbedding: %d dimensions\nFirst few values: %s"
                 (file-name-nondirectory file)
                 (length embedding)
                 (mapconcat 'number-to-string
                           (list (nth 0 embedding) (nth 1 embedding) (nth 2 embedding)
                                 (nth 3 embedding) (nth 4 embedding)) ", "))
      (message "File: %s has no embedding" (file-name-nondirectory file)))))

(defun my/vector-search-status ()
  "Show status of vector embeddings in the knowledge base."
  (interactive)
  (let* ((all-files (org-roam-list-files))
         (total-notes (length all-files))
         (notes-with-embeddings 0)
         (notes-without-embeddings '())
         (embedding-sizes '()))
    (dolist (file all-files)
      (let ((embedding (my/get-embedding-from-note file)))
        (if embedding
            (progn
              (cl-incf notes-with-embeddings)
              (push (length embedding) embedding-sizes))
          (push (file-name-nondirectory file) notes-without-embeddings))))
    (let ((coverage (if (> total-notes 0)
                       (/ (* 100.0 notes-with-embeddings) total-notes)
                     0))
          (unique-sizes (seq-uniq embedding-sizes)))
      (message "Vector Search Status: %d/%d notes have embeddings (%.1f%% coverage). Embedding sizes: %s"
               notes-with-embeddings total-notes coverage unique-sizes)
      (when notes-without-embeddings
        (with-current-buffer (get-buffer-create "*Embedding Status*")
          (erase-buffer)
          (insert (format "Embedding Coverage: %d/%d notes (%.1f%%)\n"
                         notes-with-embeddings total-notes coverage))
          (insert (format "Embedding dimensions found: %s\n\n" unique-sizes))
          (when notes-without-embeddings
            (insert "Notes without embeddings:\n")
            (dolist (file notes-without-embeddings)
              (insert (format "- %s\n" file))))
          (display-buffer (current-buffer)))))))

;;;;;; Minimal org → Markdown for n8n (ox-md), safe for files with leading drawers

(require 'org)
(require 'ox-md)

(defun my/org--md-wrap-if-needed ()
  "If buffer has no headlines or starts with a :PROPERTIES: drawer,
wrap contents under a synthetic top-level heading using #+title or filename."
  (save-excursion
    (goto-char (point-min))
    (let* ((has-heading (save-excursion (re-search-forward org-heading-regexp nil t)))
           (starts-with-drawer (looking-at-p "\\`\\s-*:PROPERTIES:\\s-*\n")))
      (when (or (not has-heading) starts-with-drawer)
        (let* ((title (or (cadr (assoc "TITLE" (org-collect-keywords '("title"))))
                          (file-name-base (or buffer-file-name "note"))))
               (content (buffer-string)))
          (erase-buffer)
          (insert "* " title "\n\n" content))))))

(defun my/org--strip-property-drawers ()
  "Remove all :PROPERTIES:…:END: drawers anywhere in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((rx (rx line-start (* blank) ":PROPERTIES:" (* blank) "\n"
                  (*? anything)
                  line-start (* blank) ":END:" (* blank) "\n")))
      (while (re-search-forward rx nil t)
        (replace-match "" t t)))))

(defun my/org--with-org-file (org-file thunk)
  "Open ORG-FILE into a temp org buffer and run THUNK there."
  (unless (and org-file (file-readable-p org-file))
    (error "File not found or unreadable: %s" org-file))
  (with-temp-buffer
    (insert-file-contents org-file)
    (let ((default-directory (file-name-directory org-file)))
      (delay-mode-hooks (org-mode)))
    (my/org--md-wrap-if-needed)
    (my/org--strip-property-drawers)
    (let ((org-export-use-babel nil)
          (org-confirm-babel-evaluate nil)
          (org-export-with-broken-links 'mark)
          (org-export-with-toc nil)
          (org-export-with-section-numbers nil)
          (org-export-with-author nil)
          (org-export-with-creator nil)
          (org-export-with-email nil)
          (org-export-with-date nil)
          ;; also tell Org not to export any drawers (LOGBOOK, etc.)
          (org-export-with-drawers nil))
      (funcall thunk))))

(defun my/org-export-md-string (org-file)
  "Export ORG-FILE to Markdown and return it as a Lisp string."
  (my/org--with-org-file
   org-file
   (lambda ()
     ;; (org-export-as BACKEND SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)
     (org-export-as 'md nil nil t '(:explicit-links t)))))

(defun my/org--temp-md-path (org-file)
  "Deterministic-but-unique temp filename for ORG-FILE."
  (let* ((abs (expand-file-name org-file))
         (mtime (or (nth 5 (file-attributes abs)) (current-time)))
         (sig  (secure-hash 'sha1 (format "%s::%s" abs mtime))))
    (expand-file-name (format "orgmd-%s.md" sig) temporary-file-directory)))

(defun my/org-export-md-to-tempfile (org-file)
  "Export ORG-FILE to a deterministic temp file and return the path."
  (let* ((out (my/org--temp-md-path org-file))
         (md  (my/org-export-md-string org-file)))
    (with-temp-file out (insert md))
    out))

(defun my/org-export-md-to-temp-read-delete (org-file)
  "Export ORG-FILE, read result, delete temp, return Markdown as Lisp string."
  (let* ((path (my/org-export-md-to-tempfile org-file))
         (contents (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
    (ignore-errors (delete-file path))
    contents))

;;; Auto-embedding hook
(after! org-roam-vector-search
  (defun my/maybe-update-embedding-on-save ()
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name)
             (org-roam-file-p)
             (not (string-match-p "/daily/" (buffer-file-name))))
    (my/generate-embedding-for-note (buffer-file-name)))))

;; Add the hook
(add-hook 'after-save-hook 'my/maybe-update-embedding)

;;; Key Bindings for Vector Search

(global-set-key (kbd "C-c v s") 'my/search-notes-by-concept)
(global-set-key (kbd "C-c v i") 'my/insert-similar-notes)
(global-set-key (kbd "C-c v r") 'my/insert-related-notes) ; Keep the manual search version too

(provide 'org-roam-vector-search)

;;; org-roam-vector-search.el ends here
