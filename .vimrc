set tabstop=4
set smartindent
set exrc
set guicursor=
set relativenumber
set nu
set hidden
set noswapfile
set nobackup
set incsearch
set scrolloff=8
set colorcolumn=80
set signcolumn=yes
" set clipboard+=unnamedplus

set nocompatible
filetype plugin on
syntax on

let mapleader=" "

call plug#begin('~/.vim/plugged')

" syntax highlighting
Plug 'sheerun/vim-polyglot'

" auto pairing of (, [ and {
Plug 'jiangmiao/auto-pairs'

" note taking
Plug 'vimwiki/vimwiki'

" smooth scrolling for C-d and C-u
Plug 'psliwka/vim-smoothie'

" fuzzy finder config
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'

" status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" editorconfig plugin
Plug 'editorconfig/editorconfig-vim'

" neovim only plugins
if has('nvim')
	" LSP support
	Plug 'neovim/nvim-lspconfig'
endif

call plug#end()

augroup RUN_AT_START
    autocmd!

	" Trims white spaces from the end of lines
    autocmd BufWritePre * %s/\s\+$//e
augroup END
