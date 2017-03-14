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
Plug 'junegunn/vim-easy-align'
" Git
Plug 'airblade/vim-gitgutter'
Plug 'kshenoy/vim-signature'
" JS
Plug 'pangloss/vim-javascript'
" File types
Plug 'tpope/vim-markdown'
"Plug 'LaTeX-Box-Team/LaTeX-Box'
Plug 'lervag/vimtex'
Plug 'ap/vim-css-color'
Plug 'groenewege/vim-less'
Plug 'vitalk/vim-lesscss'  "Autocompiling LESS files
call plug#end()

" Tabs & indentation =========================================================
filetype plugin indent on
set expandtab
set shiftwidth=4
set textwidth=79
set tabstop=8
set softtabstop=4

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
" Binary
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.o,*.obj,*.exe,*.dll,*.jar,*.pyc,*.pyo,*.rbc,*.class,*.hi
set wildignore+=*.ai,*.bmp,*.gif,*.ico,*.jpg,*.jpeg,*.png,*.psd,*.webp
set wildignore+=*.avi,*.m4a,*.mp3,*.oga,*.ogg,*.wav,*.webm
set wildignore+=*.eot,*.otf,*.ttf,*.woff
set wildignore+=*.doc,*.pdf
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz
" Cache
set wildignore+=.sass-cache
set wildignore+=*/vendor/cache/*,*/.bundle/*
" Temp/System
set wildignore+=*.*~,*~
set wildignore+=*.swp,.lock,.DS_Store,._*,tags.lock
" LaTeX
set wildignore+=*.dvi,*.log,*.out,*.bbl,*.blg,*.fdb_latexmk,*.fls,*.synctex.gz

let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|dist'

" Press F4 to toggle highlighting on/off, and show current value.
:noremap <F4> :set hlsearch! hlsearch?<CR>

nnoremap <F2> :<C-U>setlocal lcs=tab:➜.,trail:-,eol:↵ list! list? <CR>

" Filetypes ==================================================================
autocmd FileType c,ino,arduino setlocal noexpandtab shiftwidth=8 tabstop=8
autocmd FileType html,xhtml,xml,xsl,htmldjango setlocal shiftwidth=2
autocmd FileType make setlocal noexpandtab nosmarttab
let python_highlight_all = 1
au FileType tex setlocal shiftwidth=2 spell
au BufRead,BufNewFile *.ino,*.pde set filetype=c
let g:tex_flavor = 'latex'
let g:vimtex_quickfix_open_on_warning=0

let g:cpp_class_scope_highlight = 1

" Git ========================================================================
set updatetime=250
let g:gitgutter_max_signs = 500

" GUI ========================================================================
set background=dark
colorscheme solarized

let g:airline_powerline_fonts = 1

nmap ga <Plug>(EasyAlign)

nnoremap <c-\> :CtrlP<CR>
nnoremap <buffer> <F9> :exec '!python' shellescape(@%, 1)<cr>

syntax on
