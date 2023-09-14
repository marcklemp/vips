;;; vips.el --- An Emacs front end for OpenAI's GPT API and DeepL's translation API  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Marc Klemp
;; Keywords: GPT, DeepL

;; POST-related code adapted and extended from `aide.el` by Junji Zhi.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

(require 'request)

;; General settings

(defgroup vips nil
  "vips.el custom settings"
  :group 'external
  :prefix "vips-")

(defconst vips-openai-api-url "https://api.openai.com/v1/chat/completions")
(defconst vips-deepl-api-url "https://api.deepl.com/v2/translate")
(defconst vips-recenter-position -2
  "Position to recenter the view after inserting the result.")

;; OpenAI model settings

(defcustom vips-system-message "You are a helpful assistant"
  "System message sent to OpenAI API."
  :type 'string
  :group 'vips)

(defcustom vips-max-tokens 4000
  "The max-tokens paramater that vips.el sends to OpenAI API."
  :type 'integer
  :group 'vips)

(defcustom vips-temperature 0.7
  "The temperature paramater that vips.el sends to OpenAI API."
  :type 'float
  :group 'vips)

(defcustom vips-top-p 0.1
  "The top-p paramater that vips.el sends to OpenAI API."
  :type 'float
  :group 'vips)

(defcustom vips-frequency-penalty 0.1
  "The frequency_penalty paramater that vips.el sends to OpenAI API."
  :type 'float
  :group 'vips)

(defcustom vips-presence-penalty 0.1
  "The presence_penalty paramater that vips.el sends to OpenAI API."
  :type 'float
  :group 'vips)

;; DeepL-related settings
(defcustom vips-languages '("DA" "EN")
  "List of languages available for selection."
  :type '(repeat string)
  :group 'vips)

(defvar-local vips-selected-language "DA"
  "Selected target language for translation.")

(defun vips-deepl-translate (api-key prompt)
  "Return translated text from Deepl API. \n\n API-KEY is Deepl API key. \n\n PROMPT is text string sent to the API."
  (vips-select-language)
  (let ((result nil)
        (auth-value (format "DeepL-Auth-Key %s" api-key)))
    (request
      vips-deepl-api-url
      :type "POST"
      :data `(("text" . ,prompt) ("target_lang" . ,vips-selected-language))
      :parser 'json-read
      :headers `(("Authorization" . ,auth-value))
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result (concat "\n" (alist-get 'text (elt (alist-get 'translations data) 0)))))))
   result))

(defun vips-select-language ()
  "Prompt user to select a language and set the global variable 'vips-selected-language' to the selected language code."
  (interactive)
  (setq vips-selected-language (completing-read "Select target language: " vips-languages)))

(defun vips--deepl-translate-string (string)
  (vips-deepl-translate deepl-api-key string))

(defun vips-translate (start end)
  "Send region or paragraph to Deepl and insert result to end of region. \n\n START and END are selected region boundaries."
  (interactive "r")
  (vips-process-region start end 'vips--deepl-translate-string))

(defun vips--openai-api-request (api-key model prompt)
  "Return prompt answer from OpenAI API. \n\n API-KEY is OpenAI API key. \n\n MODEL is the model string, e.g., \"gpt-4\". \n\n PROMPT is prompt string sent to the API."
  (let* ((auth-value (format "Bearer %s" api-key))
         (insertion-marker (make-marker)) ;; create a new marker
         (prompt-end (+ (region-beginning) (length prompt))) ;; calculate the end of the prompt
         (current-buffer (current-buffer))) ;; capture the current buffer
    (set-marker insertion-marker prompt-end current-buffer) ;; set the marker at the end of the prompt in the current buffer
    (message "Request sent, waiting for response.") ;; display message after request is sent
    (request
      vips-openai-api-url
      :type "POST"
      :data (json-encode `(("model" . ,model)
                           ("messages" . ((("role" . "system") ("content" . ,vips-system-message))
                                         (("role" . "user") ("content" . ,prompt))))
                           ("max_tokens" . ,vips-max-tokens)
                           ("temperature" . ,vips-temperature)
                           ("frequency_penalty" . ,vips-frequency-penalty)
                           ("presence_penalty" . ,vips-presence-penalty)
                           ("top_p" . ,vips-top-p)))
      :headers `(("Authorization" . ,auth-value) ("Content-Type" . "application/json"))
      :sync nil ;; make the request asynchronous
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (with-current-buffer current-buffer ;; switch to the captured buffer
                    (goto-char insertion-marker) ;; go to the position of the marker
                    (insert (concat "\n" (alist-get 'content (alist-get 'message (elt (alist-get 'choices data) 0))))))
                  (message "Response inserted."))) ;; display message after response is inserted
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error: %S" error-thrown))))
  nil)) ;; return nil explicitly

(defun vips--openai-chat (model prompt)
  (vips--openai-api-request openai-api-key model prompt))

(defun vips-get-region-or-paragraph ()
  "Return the start and end of the selected region or the current paragraph."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (save-excursion
            (backward-paragraph)
            (point))
          (save-excursion
            (forward-paragraph)
            (point)))))

(defun vips-chat-region (start end model)
  "Send region or paragraph to OpenAI and insert result to end of region. \n\n START and END are selected region boundaries."
  (interactive "r")
  (vips-process-region start end (lambda (region) (vips--openai-chat model region))))

(defun vips-chat-region-gpt-4 ()
  "Call `vips-chat-region' with the gpt-4 model argument passed in."
  (interactive)
  (let* ((region (vips-get-region-or-paragraph))
         (start (car region))
         (end (cadr region)))
    (vips-chat-region start end "gpt-4")))

(defun vips-chat-region-gpt-3.5-turbo ()
  "Call `vips-chat-region' with the gpt-3.5-turbo model argument passed in."
  (interactive)
  (let* ((region (vips-get-region-or-paragraph))
         (start (car region))
         (end (cadr region)))
    (vips-chat-region start end "gpt-3.5-turbo")))

(defun vips-chat-region-select-model ()
  "Call `vips-chat-region' with a user-selected model."
  (interactive)
  (let* ((region (vips-get-region-or-paragraph))
         (start (car region))
         (end (cadr region))
         (models '("gpt-4" "gpt-3.5-turbo" "gpt-3.5-turbo-16k"))
         (selected-model (completing-read "Select a model: " models nil t)))
    (vips-chat-region start end selected-model)))

(defun vips-process-region (start end process-fn)
  "Process the region or paragraph using PROCESS-FN."
  (let* ((region (buffer-substring-no-properties start end))
         (result (funcall process-fn region)))
    (if result
        (progn
          (goto-char (max end (point))) ;; go to the end of the processed region
          (insert "" result "\n")
          (push-mark start 'no-message)
          (setq deactivate-mark nil)
          (unless (pos-visible-in-window-p (point))
            (recenter vips-recenter-position)))
      (message "Waiting for response..."))))

(defun mark-and-run-vips-chat-region-gpt-4 ()
  "Mark the entire buffer and run 'vips-chat-region-gpt-4'."
  (interactive)
  (let* ((start (point-min))
         (end (point-max)))
    (vips-process-region start end (lambda (region) (vips--openai-chat "gpt-4" region)))))

(defun mark-and-run-vips-chat-region-gpt-4-to-current ()
  "Mark the text from the start of the buffer to the current position and run 'vips-chat-region-gpt-4'."
  (interactive)
  (let* ((start (point-min))
         (end (if (region-active-p)
                  (region-end)
                (point))))
    (push-mark start)
    (goto-char end)
    (activate-mark)
    (vips-process-region start end (lambda (region) (vips--openai-chat "gpt-4" region)))))

(define-minor-mode vips-mode
  "Minor mode to use vips functions."
  :lighter " Vips"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c <left>") 'vips-chat-region-gpt-4)
            (define-key map (kbd "C-c <right>") 'vips-chat-region-gpt-3.5-turbo)
            (define-key map (kbd "C-c <down>") 'vips-chat-region-select-model)
            (define-key map (kbd "C-c C-a C-c") 'mark-and-run-vips-chat-region-gpt-4-to-current)
            (define-key map (kbd "C-c C-a C-v") 'mark-and-run-vips-chat-region-gpt-4)
            (define-key map (kbd "C-c SPC") 'vips-translate)
            map))

(provide 'vips)
