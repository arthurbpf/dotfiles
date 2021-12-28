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

set nocompatible
filetype plugin on
syntax on

call plug#begin('~/.vim/plugged')

Plug 'sheerun/vim-polyglot'
Plug 'vimwiki/vimwiki'
Plug 'psliwka/vim-smoothie'

if has('nvim')
	Plug 'neovim/nvim-lspconfig'
	Plug 'nvim-lua/plenary.nvim'
	Plug 'nvim-telescope/telescope.nvim'
endif

call plug#end()

if has('nvim')
	lua require'lspconfig'.tsserver.setup{}
endif

let mapleader=" "
if has('nvim')
	nnoremap <leader>ps :lua require('telescope.builtin').grep_string({ search = vim.fn.input("Grep For > ")})<CR>
endif

augroup RUN_AT_START
    autocmd!

	" Trims white spaces from the end of lines
    autocmd BufWritePre * %s/\s\+$//e
augroup END
