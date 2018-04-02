call plug#begin()
" Editing
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
" UI and apps
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'altercation/vim-colors-solarized'
Plug 'bling/vim-airline'
" Git
Plug 'airblade/vim-gitgutter' " shows changes on left side
Plug 'kshenoy/vim-signature'  " shows registers on left side
" File types
Plug 'tpope/vim-markdown'
Plug 'lervag/vimtex'
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
" Elm
Plug 'elmcast/elm-vim'
" Presentation
Plug 'sotte/presenting.vim'
Plug 'junegunn/goyo.vim'
call plug#end()

execute pathogen#infect()

" Tabs & indentation =========================================================
syntax on
filetype plugin indent on

set expandtab
set shiftwidth=4
set textwidth=79
set tabstop=8
set softtabstop=4

set shortmess=I
set visualbell
set hidden

set nowrap
set showmode
set mouse=a 

" Lines ======================================================================
set linebreak
set breakindent
set relativenumber

" Search =====================================================================
set smartcase
set gdefault
set path+=**

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
" Wild =======================================================================

let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git\|dist'

"Elm format on save
let g:elm_format_autosave = 1

" Press F4 to toggle highlighting on/off, and show current value.
nnoremap <F4> :set hlsearch! hlsearch?<CR>

nnoremap <F2> :<C-U>setlocal lcs=tab:➜.,trail:-,eol:↵ list! list?<CR>

" count occurence of world under the cursor ,*
map ,* *<C-O>:%s///gn<CR>

" Filetypes ==================================================================
autocmd FileType c,ino,arduino setlocal noexpandtab shiftwidth=8 tabstop=8
autocmd FileType html,xhtml,xml,xsl,htmldjango setlocal shiftwidth=2
autocmd FileType make setlocal noexpandtab nosmarttab
au FileType tex setlocal shiftwidth=2 spell
au BufRead,BufNewFile *.ino,*.pde set filetype=c

au FileType haskell nnoremap <buffer> <silent> <F5> :HdevtoolsInfo<CR>

let python_highlight_all = 1
let g:tex_flavor = 'latex'
let g:vimtex_quickfix_open_on_warning=0
let g:cpp_class_scope_highlight = 1

" Syntastic ==================================================================
map <Leader>s :SyntasticToggleMode<CR>
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 1

" ghc-mod ====================================================================
"map <silent> tw :GhcModTypeInsert<CR>
"map <silent> ts :GhcModSplitFunCase<CR>
"map <silent> tq :GhcModType<CR>
"map <silent> te :GhcModTypeClear<CR>

" tabularize =================================================================
let g:haskell_tabular = 1

vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>

" Git ========================================================================
set updatetime=250
let g:gitgutter_max_signs = 500

" GUI ========================================================================
set background=dark
colorscheme solarized

let g:airline_powerline_fonts = 1

nnoremap <c-\> :CtrlP<CR>
nnoremap <buffer> <F9> :exec '!python' shellescape(@%, 1)<cr>

" Use left / right arrows to switch buffers
nnoremap <silent> <right> :bnext<cr>
nnoremap <silent> <left> :bprev<cr>

if &term =~ "xterm\\|rxvt"
  " use an green cursor in insert mode
  let &t_SI = "\<Esc>]12;green\x7"
  " use a gray cursor otherwise
  let &t_EI = "\<Esc>]12;gray\x7"
  silent !echo -ne "\033]12;gray\007"
  " reset cursor when vim exits
  autocmd VimLeave * silent !echo -ne "\033]112\007"
  " use \003]12;gray\007 for gnome-terminal and rxvt up to version 9.21
endif
