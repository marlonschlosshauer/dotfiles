;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("try" "try {\n  $1\n} catch (err) {\n  $2\n}${3: finally {\n  $4\n}}" "try...catch statement" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/try-catch" nil nil)
                       ("t" "this." "t" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/t" nil nil)
                       ("sw" "switch (${1:condition}) {\n  case ${2:expression}:\n    $0\n    break;\n  default:\n}" "switch" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/switch" nil nil)
                       ("r" "return" "r" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/r" nil nil)
                       ("let" "let ${1:name} = ${2:initial};" "let declaration" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/let" nil nil)
                       ("if" "if (${1:condition}) {\n  $0\n}" "if" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/if" nil nil)
                       ("f" "function ${1:name}(${2:arg}) {\n         $0\n}\n" "function" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/function" nil nil)
                       ("for" "for (var ${1:i} = ${2:0}; $1 < ${3:collection}.length; $1++) {\n  $0\n}" "for" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/for" nil nil)
                       ("fc" "import React from \"react\";\nimport styled from \"styled-components\";\n\nconst $1Wrapper = styled.div\\`\\`;\n\nexport const $1 = ({}) => <$1Wrapper>$2</$1Wrapper>;\n" "fc" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/fc" nil nil)
                       ("doc" "/**\n * $0\n */\n" "doc" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/doc" nil nil)
                       ("const" "const ${1:name} = ${2:initial};" "const declaration" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/const" nil nil)
                       ("cl" "console.log($0)" "cl" nil nil nil "/Users/marlon/.emacs.d/snippets/js-mode/cl" nil nil)))


;;; Do not edit! File generated at Fri Jun 30 21:38:35 2023
