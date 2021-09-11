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

if has('nvim')
	set nocompatible
endif

call plug#begin('~/.vim/plugged')

Plug 'sheerun/vim-polyglot'

if has('nvim')
	Plug 'neovim/nvim-lspconfig'
endif

call plug#end()
