;;; gpt-n-mode.el --- GPT-N Interaction Mode -*- lexical-binding: t -*-

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (request "0.3.3"))
;; Keywords: tools, languages, extensions
;; URL: https://github.com/yourusername/gpt-n-mode

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

;; This package provides GPT-N Interaction mode, which allows users
;; to communicate with the OpenAI GPT-N (e.g., GPT-4) API.
;;
;; To create a new GPT-N buffer, call the `gpt-n-create-buffer` function

;;; Code:

(require 'request)

(defgroup gpt-n nil
  "GPT-N Interaction mode"
  :prefix "gpt-n-"
  :group 'external)

(defun gpt-n-read-api-key-from-config ()
  "Read the GPT-N API key from the config file in the user's home directory."
  (let* ((config-file (expand-file-name "~/.openai-api-key"))
         (api-key (when (file-exists-p config-file)
                    (with-temp-buffer
                      (insert-file-contents config-file)
                      (string-trim (buffer-string))))))
    (or api-key "your-api-key")))

(defcustom gpt-n-api-key (gpt-n-read-api-key-from-config)
  "API key for OpenAI GPT-N.
If the API key is not found in the config file (~/.openai-api-key),
set it to \"your-api-key\"."
  :type 'string
  :group 'gpt-n)

(defcustom gpt-n-model "gpt-3.5-turbo"
  "OpenAI GPT-N Model."
  :type 'string
  :group 'gpt-n)

(defcustom gpt-n-api-endpoint "https://api.openai.com/v1/chat/completions"
  "Endpoint for the OpenAI GPT-N API."
  :type 'string
  :group 'gpt-n)

(defface gpt-n-user-face
  '((t :inherit default))
  "Face for the user's messages in the GPT-N chat buffer.")

(defface gpt-n-assistant-face
  '((t :inherit font-lock-function-name-face))
  "Face for the assistant's messages in the GPT-N chat buffer.")

(defface gpt-n-loading-face
  '((t :inherit font-lock-comment-face))
  "Face for the loading message in the GPT-N chat buffer.")

(defun gpt-n-yank-handler (face beg end)
  "Apply the 'gpt-n-user-face' face to the yanked text."
  (let ((inhibit-read-only t))
    (message "Got face %s, overwriting" face)
    (put-text-property beg end 'face 'gpt-n-user-face)
    (put-text-property end (point) 'face 'gpt-n-user-face)))

(define-derived-mode gpt-n-mode fundamental-mode "GPT-N Interaction"
  "Major mode for interacting with GPT-N."
  (setq yank-handled-properties '((face . gpt-n-yank-handler)))
  (define-key gpt-n-mode-map (kbd "C-c C-c") 'gpt-n-cancel-request))

(defvar gpt-n--request-in-progress nil
  "Variable to track if a request is in progress.")

(defun gpt-n--set-read-only (read-only)
  "Set the read-only state of the current buffer to READ-ONLY."
  (setq buffer-read-only read-only))

(defun gpt-n-cancel-request ()
  "Cancel the current request if it's in progress."
  (interactive)
  (when gpt-n--request-in-progress
    (cancel-function-timers #'gpt-n--set-read-only)
    (gpt-n--set-read-only nil)
    (setq gpt-n--request-in-progress nil)
    (gpt-n-remove-loading-message)
    (insert (propertize "\n\n> " 'face 'gpt-n-user-face))
    (message "GPT-N request canceled.")))

;;;###autoload
(defun gpt-n-create-buffer ()
  "Create a new GPT-N buffer and switch to it."
  (interactive)
  (let ((buffer (get-buffer-create "*GPT-N Interaction*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'gpt-n-mode)
        (gpt-n-mode))
      ;; Ensure the user face is applied to the first message
      (when (= (point-min) (point-max))
        (insert (propertize "> " 'face 'gpt-n-user-face))))
    (pop-to-buffer buffer)))

(defun gpt-n-insert-loading-message ()
  "Insert the loading message at the end of the chat buffer."
  (with-current-buffer "*GPT-N Interaction*"
    (goto-char (point-max))
    (insert (propertize "\n\n[Loading...]" 'face 'gpt-n-loading-face))))

(defun gpt-n-remove-loading-message ()
  "Remove the loading message from the chat buffer."
  (with-current-buffer "*GPT-N Interaction*"
    (save-excursion
      (goto-char (point-max))
      (when (search-backward-regexp (rx bol "[Loading...]" eol) nil t)
        (delete-region (match-beginning 0) (match-end 0))
        ;; Delete the newline before the "[Loading...]" line
        (when (search-backward-regexp (rx bol eol) nil t)
          (delete-region (match-beginning 0) (match-end 0))))
      (goto-char (point-max))
      (when (search-backward-regexp (rx bol "> " eol) nil t)
        (delete-region (match-beginning 0) (match-end 0))))))

(defun gpt-n-insert-content (content role)
  "Insert the content string at the end of the chat buffer and apply the appropriate face based on the role."
  (with-current-buffer "*GPT-N Interaction*"
    (goto-char (point-max))
    (let ((face (if (string= role "user") 'gpt-n-user-face 'gpt-n-assistant-face)))
      (insert (propertize (format "%s" content) 'face face)))
    (when (string= role "assistant")
      (insert (propertize "\n\n> " 'face 'gpt-n-user-face)))))

(defun gpt-n-parse-buffer-messages ()
  "Parse the chat buffer into messages using face properties to determine the role of each message."
  (let ((messages '())
        (current-message '())
        (current-role "")
        (buffer "*GPT-N Interaction*"))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((face (get-text-property (point) 'face))
                 (role (cond ((eq face 'gpt-n-user-face) "user")
                             ((eq face 'gpt-n-assistant-face) "assistant")))
                 (text (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
            (message "Text: %s Role: %s" text role)
            (if role
                (progn
                  (when (not (string= current-role role))
                    (when current-message
                      (push `(("role" . ,current-role)
                               ("content" . ,(apply #'concat (nreverse current-message))))
                            messages))
                    (setq current-message '())
                    (setq current-role role))
                  (push (concat text "\n") current-message))
              (when current-message
                (push `(("role" . ,current-role)
                         ("content" . ,(apply #'concat (nreverse current-message))))
                      messages))
                (setq current-message '())
                (setq current-role "")))
          (forward-line 1))))
    (when current-message
      (push `(("role" . ,current-role)
               ("content" . ,(apply #'concat (nreverse current-message))))
            messages))
    (nreverse (last messages 20))))


(defun gpt-n-send-buffer ()
  "Send the chat messages from the buffer to the GPT-N API and insert the response at the end of the chat buffer."
  (interactive)
  (unless gpt-n--request-in-progress
    (setq gpt-n--request-in-progress t)
    (gpt-n-insert-loading-message)
    (gpt-n--set-read-only t)
    (let ((messages (append '((("role" . "system")
                               ("content" . "You are a helpful assistant.")))
                            (gpt-n-parse-buffer-messages)))
          (response-buffer (generate-new-buffer "*gpt-n-response*"))
          (error-buffer (get-buffer-create "*gpt-n-error*")))
      (request
        gpt-n-api-endpoint
        :type "POST"
        :headers `(("Content-Type" . "application/json")
                   ("Authorization" . ,(concat "Bearer " gpt-n-api-key)))
        :data (json-encode `(("model" . ,gpt-n-model)
                             ("messages" . ,(apply #'vector messages))))
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (when data
                      (let ((response (cdr (assoc 'choices data))))
                        (when response
                          (let* ((choice (aref response 0))
                                 (message (cdr (assoc 'message choice)))
                                 (content (cdr (assoc 'content message))))
                            ;; Insert the assistant's response with the 'assistant' role
                            (gpt-n--set-read-only nil)
                            (gpt-n-remove-loading-message)
                            (gpt-n-insert-content content "assistant")
                            (setq gpt-n--request-in-progress nil)))))))
        :error (cl-function
                (lambda (&rest args &key error-thrown response &allow-other-keys)
                  (with-current-buffer error-buffer
                    (insert (format "Error interacting with GPT-N API: %S\n" error-thrown))
                    (insert (format "Error response body: %s" (request-response-data response)))
                    (display-buffer error-buffer)
                    (gpt-n-remove-loading-message)
                    (gpt-n--set-read-only nil)
                    (setq gpt-n--request-in-progress nil))))
        :complete (lambda (&rest _)
                    (kill-buffer response-buffer))))))

(defun gpt-n-cancel-request ()
  "Cancel the current GPT-N API request."
  (interactive)
  (when gpt-n--request-in-progress
    (setq gpt-n--request-in-progress nil)
    (gpt-n--set-read-only nil)
    (gpt-n-remove-loading-message)
    (message "GPT-N API request cancelled.")))

(define-key gpt-n-mode-map (kbd "C-c C-s") 'gpt-n-send-buffer)
(define-key gpt-n-mode-map (kbd "C-c C-x") 'gpt-n-cancel-request)

(provide 'gpt-n-mode)

;; (require 'request)

;; (defgroup gpt-n nil
;;   "GPT-N Interaction mode"
;;   :prefix "gpt-n-"
;;   :group 'external)


;; (defun gpt-n-read-api-key-from-config ()
;;   "Read the GPT-N API key from the config file in the user's home directory."
;;   (let* ((config-file (expand-file-name "~/.openai-api-key"))
;;          (api-key (when (file-exists-p config-file)
;;                     (with-temp-buffer
;;                       (insert-file-contents config-file)
;;                       (string-trim (buffer-string))))))
;;     (or api-key "your-api-key")))

;; (defcustom gpt-n-api-key (gpt-n-read-api-key-from-config)
;;   "API key for OpenAI GPT-N.
;; If the API key is not found in the config file (~/.openai-api-key),
;; set it to \"your-api-key\"."
;;   :type 'string
;;   :group 'gpt-n)

;; (defcustom gpt-n-model "gpt-3.5-turbo"
;;   "OpenAI GPT-N Model."
;;   :type 'string
;;   :group 'gpt-n)

;; (defcustom gpt-n-api-endpoint "https://api.openai.com/v1/chat/completions"
;;   "Endpoint for the OpenAI GPT-N API."
;;   :type 'string
;;   :group 'gpt-n)

;; (defface gpt-n-user-face
;;   '((t :inherit font-lock-variable-name-face))
;;   "Face for the user's messages in the GPT-N chat buffer.")

;; (defface gpt-n-assistant-face
;;   '((t :inherit font-lock-function-name-face))
;;   "Face for the assistant's messages in the GPT-N chat buffer.")

;; (define-derived-mode gpt-n-mode fundamental-mode "GPT-N Interaction"
;;   "Major mode for interacting with GPT-N.")

;; ;;;###autoload
;; (defun gpt-n-create-buffer ()
;;   "Create a new GPT-N buffer and switch to it."
;;   (interactive)
;;   (let ((buffer (get-buffer-create "*GPT-N Interaction*")))
;;     (with-current-buffer buffer
;;       (unless (eq major-mode 'gpt-n-mode)
;;         (gpt-n-mode))
;;       ;; Ensure the user face is applied to the first message
;;       (when (= (point-min) (point-max))
;;         (insert (propertize "> " 'face 'gpt-n-user-face))))
;;     (pop-to-buffer buffer)))

;; (defun gpt-n-insert-content (content role)
;;   "Insert the content string at the end of the chat buffer and apply the appropriate face based on the role."
;;   (with-current-buffer (current-buffer)
;;     (goto-char (point-max))
;;     (let ((face (if (string= role "user") 'gpt-n-user-face 'gpt-n-assistant-face)))
;;       (insert (propertize (format "\n\n%s" content) 'face face)))
;;     (when (string= role "assistant")
;;       (insert (propertize "\n\n> " 'face 'gpt-n-user-face)))))

;; (defun gpt-n-parse-buffer-messages ()
;;   "Parse the chat buffer into messages using face properties to determine the role of each message."
;;   (let ((messages '())
;;         (current-message '())
;;         (current-role "")
;;         (buffer (current-buffer)))
;;     (with-current-buffer buffer
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (not (eobp))
;;           (let* ((face (get-text-property (point) 'face))
;;                  (role (cond ((eq face 'gpt-n-user-face) "user")
;;                              ((eq face 'gpt-n-assistant-face) "assistant")))
;;                  (text (buffer-substring-no-properties
;;                         (line-beginning-position) (line-end-position))))
;;             (message "Text: %s Role: %s" text role)
;;             (if role
;;                 (progn
;;                   (when (not (string= current-role role))
;;                     (when current-message
;;                       (push `(("role" . ,current-role)
;;                                ("content" . ,(apply #'concat (nreverse current-message))))
;;                             messages))
;;                     (setq current-message '())
;;                     (setq current-role role))
;;                   (push (concat text "\n") current-message))
;;               (when current-message
;;                 (push `(("role" . ,current-role)
;;                          ("content" . ,(apply #'concat (nreverse current-message))))
;;                       messages))
;;                 (setq current-message '())
;;                 (setq current-role "")))
;;           (forward-line 1))))
;;     (when current-message
;;       (push `(("role" . ,current-role)
;;                ("content" . ,(apply #'concat (nreverse current-message))))
;;             messages))
;;     (let ((parsed-messages (nreverse (last messages 20))))
;;       (with-current-buffer (get-buffer-create "*gpt-n-debug*")
;;         (erase-buffer)
;;         (insert (format "Parsed messages:\n%S" parsed-messages))
;;         (display-buffer (current-buffer)))
;;       parsed-messages)))

;; (defun gpt-n-send-buffer ()
;;   "Send the chat messages from the buffer to the GPT-N API and insert the response at the end of the chat buffer."
;;   (interactive)
;;   (let ((messages (append '((("role" . "system")
;;                             ("content" . "You are a helpful assistant.")))
;;                           (gpt-n-parse-buffer-messages)))
;;         (response-buffer (generate-new-buffer " *gpt-n-response*"))
;;         (error-buffer (generate-new-buffer "*gpt-n-error*")))
;;     (request
;;       gpt-n-api-endpoint
;;       :type "POST"
;;       :headers `(("Content-Type" . "application/json")
;;                  ("Authorization" . ,(concat "Bearer " gpt-n-api-key)))
;;       :data (json-encode `(("model" . ,gpt-n-model)
;;                            ("messages" . ,(apply #'vector messages))))
;;       :parser 'json-read
;;       :success (cl-function
;;                 (lambda (&key data &allow-other-keys)
;;                   (when data
;;                     (let ((response (cdr (assoc 'choices data))))
;;                       (when response
;;                         (let* ((choice (aref response 0))
;;                                (message (cdr (assoc 'message choice)))
;;                                (content (cdr (assoc 'content message))))
;;                           ;; Insert the assistant's response with the 'assistant' role
;;                           (gpt-n-insert-content content "assistant")))))))
;;       :error (cl-function
;;               (lambda (&rest args &key error-thrown response &allow-other-keys)
;;                 (with-current-buffer error-buffer
;;                   (insert (format "Error interacting with GPT-N API: %S\n" error-thrown))
;;                   (insert (format "Error response body: %s" (request-response-data response)))
;;                   (display-buffer error-buffer))))
;;       :complete (lambda (&rest _)
;;                   (kill-buffer response-buffer)))))

;; (provide 'gpt-n-mode)
