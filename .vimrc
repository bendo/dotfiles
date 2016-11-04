call plug#begin()
" Editing
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
" UI and apps
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'kien/ctrlp.vim'
Plug 'NLKNguyen/papercolor-theme'
Plug 'altercation/vim-colors-solarized'
Plug 'bling/vim-airline'
" File types
Plug 'tpope/vim-markdown'
"Plug 'LaTeX-Box-Team/LaTeX-Box'
Plug 'lervag/vimtex'
Plug 'ap/vim-css-color'
Plug 'groenewege/vim-less'
Plug 'vitalk/vim-lesscss'  "Autocompiling LESS files
"Plug 'tclem/vim-arduino'
call plug#end()

" Tabs & indentation =========================================================
set expandtab
set shiftwidth=4
set textwidth=79
set tabstop=8
set softtabstop=4
set autoindent

set shortmess=I
set visualbell
set hidden

" Lines ======================================================================
set linebreak
set breakindent
set relativenumber

" Search =====================================================================
set smartcase
set gdefault

" Wild =======================================================================
set wildignore+=*.pyc,*.pyo,*.hi,*.o "Programming
set wildignore+=*.aux,*.dvi,*.pdf,*.log,*.out,*.bbl,*.blg,*.fdb_latexmk,*.fls,*.synctex.gz "LaTeX

" Filetypes ==================================================================
autocmd FileType html,xhtml,xml,xsl,htmldjango setlocal shiftwidth=2
autocmd FileType make setlocal noexpandtab nosmarttab
let python_highlight_all = 1
au FileType tex setlocal shiftwidth=2 spell
au BufRead,BufNewFile *.pde set filetype=arduino
au BufRead,BufNewFile *.ino set filetype=arduino
let g:tex_flavor = 'latex'
let g:vimtex_quickfix_open_on_warning=0

let g:cpp_class_scope_highlight = 1

" GUI ========================================================================
set background=dark
colorscheme solarized

let g:airline_powerline_fonts = 1

nnoremap <buffer> <F9> :exec '!python' shellescape(@%, 1)<cr>

syntax on
