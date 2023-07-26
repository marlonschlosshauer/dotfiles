;;; Compiled snippets and support files for `ts-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ts-mode
                     '(("story" "import { StoryObj } from \"@storybook/react\";\nimport { $1 } from \"./$1\";\n\nconst meta = {\n  title: \"Shared/$1\",\n  component: $1,\n};\n\nexport default meta;\ntype Story = StoryObj<typeof meta>;\n\nexport const Base: Story = {\n  args: {},\n};\n" "storybook template" nil nil nil "/Users/marlon/.emacs.d/snippets/ts-mode/story" nil nil)
                       ("nc" "import React from \"react\";\nimport styled from \"styled-components/native\";\n\nconst $1Wrapper = styled.View\\`\\`;\n\nexport interface $1Props {}\n\nexport const $1: React.FC<$1Props> = ({}) => <$1Wrapper>$2</$1Wrapper>;\n" "nc" nil nil nil "/Users/marlon/.emacs.d/snippets/ts-mode/nc" nil nil)
                       ("fcc" "import React, { PropsWithChildren } from \"react\";\nimport styled from \"styled-components\";\n\nconst $1Wrapper = styled.div\\`\\`;\n\nexport interface $1Props {}\n\nexport const $1: React.FC<PropsWithChildren<$1Props>> = ({}) => (\n    <$1Wrapper></$1Wrapper>\n);\n" "react component definition" nil nil nil "/Users/marlon/.emacs.d/snippets/ts-mode/fcc" nil nil)
                       ("fc" "import React from \"react\";\nimport styled from \"styled-components\";\n\nconst $1Wrapper = styled.div\\`\\`;\n\nexport interface $1Props {}\n\nexport const $1: React.FC<$1Props> = ({}) => <$1Wrapper>$2</$1Wrapper>;\n" "fc" nil nil nil "/Users/marlon/.emacs.d/snippets/ts-mode/fc" nil nil)))


;;; Do not edit! File generated at Fri Jun 30 21:38:35 2023
