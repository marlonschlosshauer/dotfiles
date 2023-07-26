;;; Compiled snippets and support files for `csharp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'csharp-mode
                     '(("cwl" "Console.WriteLine(${1:Write});" "Console.WriteLine(...);" nil nil nil "/Users/marlon/.emacs.d/snippets/csharp-mode/writeline" nil nil)
                       ("cw" "Console.Write(${1:Write});" "Console.Write(...);" nil nil nil "/Users/marlon/.emacs.d/snippets/csharp-mode/write" nil nil)
                       ("tc" "try\n{\n	$0\n}\ncatch (${1:Exception} ex)\n{\n	${2:Console.WriteLine(ex.ToString());}\n}" "try{...} catch (ex) {...}" nil nil nil "/Users/marlon/.emacs.d/snippets/csharp-mode/trycatch" nil nil)
                       ("region" "#region $1\n$0\n#endregion" "#region ... #endregion" nil nil nil "/Users/marlon/.emacs.d/snippets/csharp-mode/region" nil nil)
                       ("crl" "Console.ReadLine(${1:Read});\n" "Console.ReadLine(...);" nil nil nil "/Users/marlon/.emacs.d/snippets/csharp-mode/readline" nil nil)
                       ("namespace" "namespace $1\n{\n$0\n}" "namespace .. { ... }" nil nil nil "/Users/marlon/.emacs.d/snippets/csharp-mode/namespace" nil nil)
                       ("method" "/// <summary>\n/// ${5:Description}\n/// </summary>${2:$(if (string= (upcase yas-text) \"VOID\") \"\" (format \"%s%s%s\" \"\\n/// <returns><c>\" yas-text \"</c></returns>\"))}\n${1:public} ${2:void} ${3:MethodName}($4)\n{\n$0\n}" "public void Method { ... }" nil nil nil "/Users/marlon/.emacs.d/snippets/csharp-mode/method" nil nil)
                       ("main" "static void Main(string[] args)\n{\n    $0\n}\n" "main" nil nil nil "/Users/marlon/.emacs.d/snippets/csharp-mode/main" nil nil)
                       ("fore" "foreach (${1:var} ${2:item} in ${3:list})\n{\n    $0\n}" "foreach { ... }" nil nil nil "/Users/marlon/.emacs.d/snippets/csharp-mode/fore" nil nil)
                       ("class" "${5:public} class ${1:Name}\n{\n    #region Ctor & Destructor\n    /// <summary>\n    /// ${3:Standard Constructor}\n    /// </summary>\n    public $1($2)\n    {\n    }\n\n    /// <summary>\n    /// ${4:Default Destructor}\n    /// </summary>\n    public ~$1()\n    {\n    }\n    #endregion\n}" "class ... { ... }" nil nil nil "/Users/marlon/.emacs.d/snippets/csharp-mode/class" nil nil)))


;;; Do not edit! File generated at Fri Jun 30 21:38:35 2023
