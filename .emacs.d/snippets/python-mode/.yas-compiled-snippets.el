;;; Compiled snippets and support files for `python-mode'
;;; contents of the .yas-setup.el support file:
;;;
(require 'yasnippet)
(defvar yas-text)

(defun python-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun python-args-to-docstring ()
  "return docstring format for the python arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                (lambda (x)
                   (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                           (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
                args
                indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Keyword Arguments:" formatted-args) indent))))

(defun python-args-to-docstring-numpy ()
  "return docstring format for the python arguments in yas-text"
  (let* ((args (python-split-args yas-text))
         (format-arg (lambda(arg)
                       (concat (nth 0 arg) " : " (if (nth 1 arg) ", optional") "\n")))
         (formatted-params (mapconcat format-arg args "\n"))
         (formatted-ret (mapconcat format-arg (list (list "out")) "\n")))
    (unless (string= formatted-params "")
      (mapconcat 'identity
                 (list "\nParameters\n----------" formatted-params
                       "\nReturns\n-------" formatted-ret)
                 "\n"))))


(add-hook 'python-mode-hook #'yasnippet-snippets--fixed-indent)
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("with" "with ${1:expr}${2: as ${3:alias}}:\n    $0" "with" nil
                        ("control structure")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/with" nil nil)
                       ("wh" "while ${1:True}:\n    $0" "while" nil
                        ("control structure")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/while" nil nil)
                       ("try" "try:\n    $0\nexcept $1:\n    $2\nelse:\n    $3" "tryelse" nil nil nil "/Users/marlon/.emacs.d/snippets/python-mode/tryelse" nil nil)
                       ("try" "try:\n    $0\nexcept ${1:Exception}:\n    $2" "try" nil nil nil "/Users/marlon/.emacs.d/snippets/python-mode/try" nil nil)
                       ("setup" "from setuptools import setup\n\npackage = '${1:name}'\nversion = '${2:0.1}'\n\nsetup(name=package,\n      version=version,\n      description=\"${3:description}\",\n      url='${4:url}'$0)\n" "setup" nil
                        ("distribute")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/setup" nil nil)
                       ("script" "#!/usr/bin/env python\n\ndef main():\n    pass\n\nif __name__ == '__main__':\n    main()\n" "script" nil nil nil "/Users/marlon/.emacs.d/snippets/python-mode/script" nil nil)
                       ("r" "return $0" "return" nil nil nil "/Users/marlon/.emacs.d/snippets/python-mode/return" nil nil)
                       ("p" "print($0)" "print" nil nil nil "/Users/marlon/.emacs.d/snippets/python-mode/print" nil nil)
                       ("pars" "parser = argparse.ArgumentParser(description='$1')\n$0" "parser" nil
                        ("argparser")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/parser" nil nil)
                       ("m" "def ${1:method}(self${2:, $3}):\n    $0" "method" nil
                        ("object oriented")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/method" nil nil)
                       ("li" "[${1:el} for $1 in ${2:list}]\n$0" "list" nil
                        ("definitions")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/list" nil nil)
                       ("lam" "lambda ${1:x}: $0" "lambda" nil nil nil "/Users/marlon/.emacs.d/snippets/python-mode/lambda" nil nil)
                       ("init" "def __init__(self${1:, args}):\n    ${2:\"${3:docstring}\"\n    }$0" "init" nil
                        ("definitions")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/init" nil nil)
                       ("imp" "import ${1:lib}${2: as ${3:alias}}\n$0" "import" nil
                        ("general")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/import" nil nil)
                       ("ifm" "if __name__ == '__main__':\n    ${1:main()}" "ifmain" nil nil nil "/Users/marlon/.emacs.d/snippets/python-mode/ifmain" nil nil)
                       ("ife" "if $1:\n    $2\nelse:\n    $0\n" "ife" nil
                        ("control structure")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/ife" nil nil)
                       ("if" "if ${1:cond}:\n    $0\n" "if" nil
                        ("control structure")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/if" nil nil)
                       ("doxy_func" "\"\"\"\n@brief      ${1:function description}\n\n@details    ${2:detailed description}\n\n@param      ${3:param}\n\n@return     ${4:return type}\n\"\"\"" "Function Doxygen Doc" nil
                        ("doxygen")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/function_doxygen_doc" nil nil)
                       ("fd" "def ${1:name}($2):\n \\\"\\\"\\\"$3\n ${2:$(python-args-to-docstring)}\n \\\"\\\"\\\"\n $0" "function_docstring" nil
                        ("definitions")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/function_docstring" nil nil)
                       ("f" "def ${1:fun}(${2:args}):\n    $0\n" "function" nil
                        ("definitions")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/function" nil nil)
                       ("from" "from ${1:lib} import ${2:funs}" "from" nil
                        ("general")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/from" nil nil)
                       ("for" "for ${var} in ${collection}:\n    $0" "for ... in ... : ..." nil
                        ("control structure")
                        nil "/Users/marlon/.emacs.d/snippets/python-mode/for" nil nil)))


;;; Do not edit! File generated at Fri Jun 30 21:38:35 2023
