;;; Compiled snippets and support files for `clojure-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'clojure-mode
                     '(("when" "(when $1\n      $2)$>\n$0$>" "when" nil nil nil "/Users/marlon/.emacs.d/snippets/clojure-mode/when" nil nil)
                       ("reduce" "(reduce ${1:(fn [p n] $0)} $2)" "reduce" nil nil nil "/Users/marlon/.emacs.d/snippets/clojure-mode/reduce" nil nil)
                       ("let" "(let [$1 $2]$>\n  $3)$>\n$0" "let" nil nil nil "/Users/marlon/.emacs.d/snippets/clojure-mode/let" nil nil)
                       ("if" "(if $1\n  $2$>\n  $3)$>\n$0" "if" nil nil nil "/Users/marlon/.emacs.d/snippets/clojure-mode/if" nil nil)
                       ("for" "(for [$1 $2]\n  $3)$>" "for" nil nil nil "/Users/marlon/.emacs.d/snippets/clojure-mode/for" nil nil)
                       ("fn" "(fn [$1]\n  $0)$>" "fn" nil nil nil "/Users/marlon/.emacs.d/snippets/clojure-mode/fn" nil nil)
                       ("defn" "(defn $1\n  \"$2\"$>\n  [$3]$>\n  $0)$>" "defn" nil nil nil "/Users/marlon/.emacs.d/snippets/clojure-mode/defn" nil nil)
                       ("def" "(def $0)" "def" nil nil nil "/Users/marlon/.emacs.d/snippets/clojure-mode/def" nil nil)))


;;; Do not edit! File generated at Fri Jun 30 21:38:35 2023
