;; -*- mode: eask; lexical-binding: t -*-

(package "qq"
         "2.0.0"
         "Native QQ chat client")

(website-url "https://github.com/0WD0/emacs-qq")

(keywords "comm")

(package-file "qq.el")

(files "*.el"
       "qq-face-names.json")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "29.1")
(depends-on "websocket" "1.16")
(depends-on "transient" "0.7")
(depends-on "appkit" :file "../appkit.el")
(depends-on "browser-session" :file "../browser-session")

(development
 (depends-on "evil"))

(script "refresh-appkit" "eask -f install-file ../appkit.el")
(script "test" "eask test ert --quiet test/*.el")
(script "test-concise" "./scripts/test-concise.sh")
(script "test-local"
        "eask -f install-file ../appkit.el && eask recompile && eask test ert --quiet test/*.el")
