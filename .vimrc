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
set nowrap

set nocompatible
filetype plugin on
syntax on

let mapleader=" "
let maplocalleader="\\"

call plug#begin('~/.vim/plugged')

" syntax highlighting
Plug 'sheerun/vim-polyglot'

" auto pairing of (, [ and {
Plug 'jiangmiao/auto-pairs'

" change surrounding tags or quotes
Plug 'tpope/vim-surround'

" note taking
Plug 'vimwiki/vimwiki'

" smooth scrolling for C-d and C-u
Plug 'psliwka/vim-smoothie'

" fuzzy finder config
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'airblade/vim-rooter'

" editorconfig plugin
Plug 'editorconfig/editorconfig-vim'

" auto close tags
Plug 'alvan/vim-closetag'

" code formatting
Plug 'sbdchd/neoformat'

" LaTeX features
Plug 'lervag/vimtex'
let g:vimtex_view_method = 'zathura'

" Neovim only plugins
if has('nvim')
	" LSP support
	Plug 'neovim/nvim-lspconfig'

	Plug 'hrsh7th/nvim-cmp'
	Plug 'hrsh7th/cmp-nvim-lsp'
	Plug 'hrsh7th/cmp-buffer'
	Plug 'hrsh7th/cmp-path'
	Plug 'hrsh7th/cmp-cmdline'

	" support for vscode style snippets
	Plug 'hrsh7th/cmp-vsnip'
	Plug 'hrsh7th/vim-vsnip'

	" collection of snippets
	Plug 'rafamadriz/friendly-snippets'
endif

call plug#end()

augroup RUN_AT_START
    autocmd!
	" trims white spaces from the end of lines
    autocmd BufWritePre * %s/\s\+$//e
augroup END

augroup FILETYPE_CONFIGS
	autocmd!
	autocmd FileType tex
		\ set wrap |
		\ set colorcolumn=0 |
augroup END
