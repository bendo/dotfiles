call plug#begin()
Plug 'ctrlpvim/ctrlp.vim'
"Nerdtree
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'ryanoasis/vim-devicons'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
" Editing
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-commentary'
" UI and apps
Plug 'arcticicestudio/nord-vim'
Plug 'altercation/vim-colors-solarized'
"Plug 'morhetz/gruvbox'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline'
" Git
Plug 'airblade/vim-gitgutter' " shows changes on left side
Plug 'kshenoy/vim-signature'  " shows registers on left side
" File types
Plug 'tpope/vim-markdown'
Plug 'lervag/vimtex'
Plug 'Shougo/vimproc.vim', {'do' : 'make'}
" Presentation
Plug 'sotte/presenting.vim'
Plug 'junegunn/goyo.vim'
" Rescript
Plug 'rescript-lang/vim-rescript'
" CSS
Plug 'genoma/vim-less'
Plug 'wavded/vim-stylus'
" Haskell
Plug 'edkolev/curry.vim'
"Plug 'godlygeek/tabular'
Plug 'alx741/vim-stylishask'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neovimhaskell/haskell-vim'
call plug#end()

set encoding=UTF-8
set guifont=DroidSansMono\ Nerd\ Font\ 12
" Tabs & indentation =========================================================
syntax on
filetype plugin indent on

set expandtab
set shiftwidth=4
" set textwidth=79
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
set number
set relativenumber

" Search =====================================================================
set smartcase
set path+=**

" Swap files =================================================================
set directory=$HOME/.vim/swp//
set undofile
set undodir=~/.vim/undodir

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

" Press F4 to toggle highlighting on/off, and show current value.
nnoremap <F4> :set hlsearch! hlsearch?<CR>
nnoremap <F3> :let &cc = &cc == '' ? '80' : ''<CR>
nnoremap <F2> :<C-U>setlocal lcs=tab:➜.,trail:-,eol:↵ list! list?<CR>

" count occurence of world under the cursor ,*
map ,* *<C-O>:%s///gn<CR>

" Filetypes ==================================================================
autocmd FileType c,ino,arduino setlocal noexpandtab shiftwidth=8 tabstop=8
" autocmd FileType html,xhtml,xml,xsl,htmldjango setlocal shiftwidth=2
autocmd FileType make setlocal noexpandtab nosmarttab
au FileType tex setlocal shiftwidth=2 spell
au BufRead,BufNewFile *.ino,*.pde set filetype=c

" Haskell ====================================================================
let g:airline#extensions#tabline#enabled = 1
"autocmd FileType haskell autocmd BufWritePre <buffer> call CocAction('format')
let g:haskell_indent_disable=1
set formatprg=stylish-haskell

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
"nmap <silent> gi <Plug>(coc-implementation) -- not supported byt hls
nmap <silent> gr <Plug>(coc-references)

inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

nmap <leader>ac  <Plug>(coc-codeaction)
" END Haskell ================================================================

" Nerdtree ===================================================================
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

let g:NERDTreeIgnore = ['^node_modules']

" au VimEnter *  NERDTree " will open NERDTree automaticaly

function! IsNERDTreeOpen()        
  return exists("t:NERDTreeBufName") && (bufwinnr(t:NERDTreeBufName) != -1)
endfunction

" Call NERDTreeFind iff NERDTree is active, current window contains a modifiable
" file, and we're not in vimdiff
function! SyncTree()
  if &modifiable && IsNERDTreeOpen()
    NERDTreeFind
    wincmd p
  endif
endfunction

" Highlight currently open buffer in NERDTree
autocmd BufEnter * call SyncTree()

let g:WebDevIconsUnicodeDecorateFileNodesDefaultSymbol = ''

let g:WebDevIconsUnicodeDecorateFileNodesExactSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesExactSymbols['Dockerfile'] = ''

let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesPatternSymbols['Docker.*'] = ''

let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols = {} " needed
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['hs'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['js'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['res'] = 'ﰄ'
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['yaml'] = ''
let g:WebDevIconsUnicodeDecorateFileNodesExtensionSymbols['conf'] = '難'

let s:brown = "905532"
let s:aqua =  "3AFFDB"
let s:blue = "689FB6"
let s:darkBlue = "44788E"
let s:purple = "834F79"
let s:lightPurple = "834F79"
let s:red = "AE403F"
let s:orange = "D4843E"
let s:darkOrange = "F16529"
let s:pink = "CB6F6F"
let s:salmon = "EE6E73"
let s:green = "8FAA54"
let s:lightGreen = "31B53E"
let s:white = "FFFFFF"
let s:rspec_red = 'FE405F'
let s:git_orange = 'F54D27'

let g:NERDTreeExtensionHighlightColor = {} " this line is needed to avoid error
let g:NERDTreeExtensionHighlightColor['res'] = s:git_orange " sets the color of css files to blue
let g:NERDTreeExtensionHighlightColor['conf'] = s:purple " sets the color of css files to blue
" END Nerdtree ===============================================================

let python_highlight_all = 1
let g:tex_flavor = 'latex'
let g:vimtex_quickfix_open_on_warning=0
let g:cpp_class_scope_highlight = 1

" tabularize =================================================================
"let g:haskell_tabular = 1

"vmap a= :Tabularize /=<CR>
"vmap a; :Tabularize /::<CR>
"vmap a- :Tabularize /-><CR>

" Git ========================================================================
set updatetime=250
let g:gitgutter_max_signs = 500

" GUI ========================================================================
set bg=dark
colorscheme solarized
"colorscheme gruvbox

" Airline ====================================================================
let g:airline_powerline_fonts = 1
let g:airline#extensions#ale#enabled = 1
"let g:airline#extensions#tabline#buffer_idx_mode = 1

" Config =====================================================================
set autochdir   " switch to current file's parent directory

nnoremap n nzz
nnoremap N Nzz

" Use left / right arrows to switch buffers
nnoremap <silent> <right> :bnext<cr>
nnoremap <silent> <left> :bprev<cr>

" Cursor =====================================================================
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

" Whitespace deletion ========================================================
au BufWritePre *.hs :%s/\s\+$//e
au BufWritePre *.js :%s/\s\+$//e
au BufWritePre *.py :%s/\s\+$//e
au BufWritePre *.jsx :%s/\s\+$//e
au BufWritePre *.css :%s/\s\+$//e
au BufWritePre *.less :%s/\s\+$//e
au BufWritePre *.java :%s/\s\+$//e
