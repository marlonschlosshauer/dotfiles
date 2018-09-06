set number
set display+=lastline
set encoding=utf-8
set linebreak
set ignorecase
set hlsearch
set laststatus=2

syntax enable
syntax on
colorscheme citylights
set background=dark

call plug#begin('~/.vim/plugins')
Plug 'scrooloose/nerdtree'
Plug 'mattn/emmet-vim'
Plug 'pangloss/vim-javascript'
Plug 'elzr/vim-json'
Plug 'othree/html5.vim'
Plug 'powerline/powerline'
call plug#end()
