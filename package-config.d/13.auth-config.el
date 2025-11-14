;; from <https://magnus.therning.org/2024-09-01-improving-how-i-handle-secrets-in-my-work-notes.html>
(require 'dash)
(defun kc/auth-get-pwd (host)
  "Get the password for a host (authinfo.gpg)"
  (-> (auth-source-search :host host)
      car
      (plist-get :secret)
      funcall))

(defun kc/auth-get-key (host key)
  "Get a key's value for a host (authinfo.gpg)

Not usable for getting the password (:secret), use 'kc/auth-get-pwd'
for that."
  (-> (auth-source-search :host host)
      car
      (plist-get key)))
