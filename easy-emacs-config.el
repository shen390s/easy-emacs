;; -*- lexical-binding: t -*-
;; Configuration for easy-emacs
;; this file will be loaded

(easy! :vars (a . 10) (b . 20)
       :modes plantuml mermaid
       :ui my-theme
       :completion ivy -helm -autoc-complete
       :app emacs-server)
