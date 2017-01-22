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
Plug 'Wolfy87/vim-syntax-expand' "???
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

" JS =========================================================================
" Map the conceal characters to their expanded forms.
inoremap <silent> @ <C-r>=syntax_expand#expand("@", "this")<CR>
inoremap <silent> # <C-r>=syntax_expand#expand("#", "prototype")<CR>
inoremap <silent> < <C-r>=syntax_expand#expand_head("<", "return")<CR>

" even when your cursor is on top of them.
set conceallevel=1
set concealcursor=nvic

" vim-javascript conceal settings.
let g:javascript_conceal_function = "λ"
let g:javascript_conceal_this = "@"
let g:javascript_conceal_return = "<"
let g:javascript_conceal_prototype = "#"

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
