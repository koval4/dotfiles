"**************************************************
"NeoBundle Scripts-----------------------------
"**************************************************
if has('vim_starting')
  if &compatible
    set nocompatible               " Be iMproved
  endif

  " Required:
  set runtimepath+=/home/koval4/.nvim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('/home/koval4/.nvim/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

"********************************************************************
"" Plugins
"********************************************************************

" Add or remove your Bundles here:
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'scrooloose/nerdcommenter'
NeoBundle 'jistr/vim-nerdtree-tabs'
NeoBundle 'gcmt/taboo.vim'
NeoBundle 'Valloric/YouCompleteMe'
NeoBundle 'Townk/vim-autoclose'
"NeoBundle 'vimgdb'
NeoBundle 'Conque-GDB'
NeoBundle 'bling/vim-airline'
NeoBundle 'octol/vim-cpp-enhanced-highlight'
NeoBundle 'rdnetto/YCM-Generator'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'eagletmt/ghcmod-vim'
NeoBundle 'ervandew/supertab'
NeoBundle 'eagletmt/neco-ghc'

NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }

" You can specify revision/branch/tag.
NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------


"*****************************************************************************
"" Basic Setup
"*****************************************************************************"
"" Encoding
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8

" Sets how many lines of history VIM has to remember
set history=500

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread

"" Tabs. May be overriten by autocmd rules
set tabstop=4
set softtabstop=0
set shiftwidth=4
set expandtab

"" Searching
set hlsearch
set incsearch
set ignorecase
set smartcase

"" Directories for swp files
set nobackup
set noswapfile

"*****************************************************************************
"" Visual Settings
"*****************************************************************************

syntax on
set ruler
set number

set title

set colorcolumn=100

" Colorscheme
set background=dark
colorscheme delek
highlight Pmenu ctermfg=1 ctermbg=10 guifg=#ffffff guibg=#0000ff
highlight Comment cterm=italic ctermfg=3
let g:airline_theme='term'

"********************************************************************
" Key Mapping
" *******************************************************************

let mapleader=","
map <F4> :NERDTreeTabsToggle<CR>
map <M-Tab> :tabnext<CR>

"*************************************************************
" Plugins Configuration
"*************************************************************

let g:ycm_global_ycm_extra_conf = '/home/koval4/.nvim/'

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>

au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <silent> <F3> :HdevtoolsInfo<CR>

let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

if has("gui_running")
  imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
  if has("unix")
    inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
  endif
endif

let g:haskellmode_completion_ghc = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
autocmd BufEnter *.hs set formatprg=pointfree
