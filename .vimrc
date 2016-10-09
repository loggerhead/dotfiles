" vim:se fdm=marker:
" Vundle {{{
set nocompatible
filetype off
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin('~/.vim/bundle')

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-commentary'
Plugin 'airblade/vim-gitgutter'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'dyng/ctrlsf.vim'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-fugitive'
Plugin 'nginx.vim'
Plugin '907th/vim-auto-save'
Plugin 'ntpeters/vim-better-whitespace'

Plugin 'ervandew/supertab'
Plugin 'Shougo/neocomplete.vim'
Plugin 'scrooloose/syntastic'

Plugin 'wakatime/vim-wakatime'

" Plugin 'Valloric/YouCompleteMe'
" Plugin 'rust-lang/rust.vim'
" Plugin 'racer-rust/vim-racer'
" Plugin 'CodeFalling/fcitx-vim-osx'

call vundle#end()

filetype plugin indent on
" }}}

" 全局初始化 {{{
let mapleader = " "
" 打开语法高亮
syntax on
set bs=2
set autoread
set ignorecase

" enable mouse support
" set mouse=a
" map <LeftMouse> <nop>
" map <2-LeftMouse> <LeftMouse>
" 代码折叠
set foldmethod=syntax
set foldlevelstart=99
" 自动换行
set wrap
set encoding=utf-8
" 文件保存编码
set fileencoding=utf-8
" 载入文件时检测的编码
set fileencodings=utf-8,gd2312,gbk,gd18030,cp936
" copy 到系统剪切板
" set clipboard=unnamed
" 高亮匹配的括号
set showmatch
" 实时匹配搜索结果
set incsearch
" 高亮搜索结果
set hlsearch
" 编辑时显示光标状态
set ruler
" swp 文件位置
set backupdir=~/.vim/backup//
set directory=~/.vim/swap//
set undodir=~/.vim/undo//
" Tab = 4*space
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
" 缩进
set autoindent
set smartindent
set cindent
set pastetoggle=<C-C>
" 显示文本处理模式
" set showmode
set showcmd
set cursorline
set number
set fenc=gbk
" check file change every 4 seconds ('CursorHold') and reload the buffer upon detecting change
set autoread
au CursorHold * checktime
" Use 256 colours
set t_Co=256
" 隐藏GUI界面工具条
set guioptions-=T
" 字体
set guifont=Monaco:h14
" theme
set background=dark
let g:solarized_visibility = "high"
let g:solarized_contrast = "high"
colorscheme solarized
" }}}

" syntastic {{{
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_python_checkers=['flake8']
let g:syntastic_python_flake8_args='--ignore=E501,E225,E203,E221,E302'

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
" }}}

" vim-better-whitespace {{{
autocmd VimEnter * DisableWhitespace
autocmd VimEnter * EnableWhitespace
" }}}

" YouCompleteMe {{{
let g:ycm_path_to_python_interpreter = '/usr/bin/python'
" }}}
"
" vim-gitgutter {{{
let g:gitgutter_realtime = 0
let g:gitgutter_eager = 0
" }}}

" ctrlp {{{
let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_map = '<c-o>'
set wildignore+=*/tmp/*,*/node_modules/*,*.so,*.swp,*.zip,*.pyc,*.exe,*.dylib,*.png,*.gif,*.jpg,*.jpeg,*.xlsx,*.bmp
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
" }}}

" nginx.vim {{{
au BufRead,BufNewFile /etc/nginx/*,/usr/local/nginx/conf/*,/usr/local/etc/nginx* if &ft == '' | setfiletype nginx | endif
" }}}

" supertab {{{
" let g:SuperTabDefaultCompletionType = "<c-p>"
let g:SuperTabDefaultCompletionType = "<c-x><c-u>"
" }}}

" neocomplete {{{
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
" Close popup by <Space>.
"inoremap <expr><Space> pumvisible() ? "\<C-y>" : "\<Space>"

" AutoComplPop like behavior.
"let g:neocomplete#enable_auto_select = 1

" Shell like behavior(not recommended).
"set completeopt+=longest
"let g:neocomplete#enable_auto_select = 1
"let g:neocomplete#disable_auto_complete = 1
"inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

" Enable omni completion.
autocmd Filetype c          setlocal omnifunc=ccomplete#Complete
autocmd FileType python     setlocal omnifunc=python3complete#Complete
autocmd Filetype tex        setlocal omnifunc=syntaxcomplete#Complete
autocmd FileType css        setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType xml        setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType html       setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType markdown   setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
"let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
"let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
"let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
" }}}

" ctrlsf {{{
let g:ctrlsf_default_root = 'cwd'
" }}}

" racer {{{
set hidden
let g:racer_cmd = "~/.cargo/bin/racer"
" }}}

" NERDTree {{{
" open a NERDTree automatically when vim starts up if no files were specified
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" close vim if the only window left open is a NERDTree
autocmd WinEnter * call s:CloseIfOnlyNerdTreeLeft()
function! s:CloseIfOnlyNerdTreeLeft()
  if exists("t:NERDTreeBufName")
    if bufwinnr(t:NERDTreeBufName) != -1
      if winnr("$") == 1
        q
      endif
    endif
  endif
endfunction
"toggle tree view
nnoremap <silent> <C-k><C-b> :NERDTreeToggle<CR>
" }}}

" airline {{{
set noshowmode
set timeoutlen=300
set laststatus=2
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_powerline_fonts = 0
let g:airline_theme = "wombat"
let g:airline_extensions = [ 'ctrlp', 'hunks' ]
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#tabline#enabled = 0

let g:airline_section_a = airline#section#create(['mode'])
let g:airline_section_b = airline#section#create(['%{fugitive#head()}'])
let g:airline_section_c = airline#section#create(['%f'])
let g:airline_section_x = airline#section#create_left(['hunks'])
let g:airline_section_y = airline#section#create(['%{strlen(&fenc)?&fenc:none}'])
let g:airline_section_z = airline#section#create_right(['%l line'])
" }}}

" gitgutter {{{
let g:gitgutter_realtime = 1
" }}}

" Shortcut {{{
nnoremap zz za
nnoremap zC zM
nnoremap zO zR
inoremap <C-D> <Delete>
inoremap <C-B> <Left>
inoremap <C-F> <Right>
inoremap <C-P> <Up>
inoremap <C-N> <Down>

" `Ctrl+/` to comment
nmap <C-_> gcc
imap <C-_> <Esc>gcci
vmap <C-_> gc

nmap <C-L>            <Plug>(easymotion-s)
imap <C-L>       <Esc><Plug>(easymotion-s)
nmap <leader><leader> <Plug>(easymotion-s)
nnoremap <leader>r :source $MYVIMRC<CR>

nnoremap <C-W><bar> :vsplit<CR><C-W>l
nnoremap <C-W>-     :split<CR><C-W>j
nnoremap <C-W>x     :q!<CR>

noremap  <C-S> :update<CR>
inoremap <C-S> <Esc>:update<CR>

nnoremap <C-C> :set invpaste paste?<CR>
inoremap <C-C> <Esc>:set invpaste paste?<CR>i

nmap     <C-F>f <Plug>CtrlSFPrompt
nnoremap <C-F>t :CtrlSFToggle<CR>
inoremap <C-F>t <Esc>:CtrlSFToggle<CR>
" }}}
