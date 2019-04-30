set number
set display+=lastline
set encoding=utf-8
set linebreak
set ignorecase
set hlsearch
set laststatus=2

syntax enable
syntax on
set background=dark

inoremap jj <Esc>

call plug#begin('~/.vim/plugins')
Plug 'mattn/emmet-vim'
Plug 'pangloss/vim-javascript'
Plug 'elzr/vim-json'
Plug 'othree/html5.vim'
Plug 'powerline/powerline'
Plug 'SirVer/ultisnips'
Plug 'Valloric/YouCompleteMe'
Plug 'tpope/vim-surround'
call plug#end()
