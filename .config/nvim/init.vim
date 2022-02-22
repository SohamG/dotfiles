" Beginners .vimrc
" v0.1 2012-10-22 Philip Thrasher
"
" Important things for beginners:
" * Start out small... Don't jam your vimrc full of things you're not ready to
"   immediately use.
" * Read other people's vimrc's.
" * Use a plugin manager for christ's sake! (I highly recommend vundle)
" * Spend time configuring your editor... It's important. Its the tool you
"   spend 8 hours a day crafting your reputation.
" * remap stupid things to new keys that make you more efficient.
" * Don't listen to the haters that complain about using non-default
"   key-bindings. Their argument is weak. I spend most of my time in the editor
"   on my computer, not others, so I don't care if customizing vim means I'll
"   have a harder time using remote vim.
"
" Below I've left some suggestions of good default settings to have in a bare
" minimal vimrc. You only what you want to use, and nothing more. I've heavily
" commented each, and these are what I consider bare necessities, my workflow
" absolutely depends on these things.
"
" If you have any questions, email me at pthrash@me.com

set nocompatible " Fuck VI... That's for grandpas.
filetype off
set number relativenumber

set rtp+=~/.vim/bundle/vundle/
call plug#begin('~/.config/nvim/plugged')
    " Just a shitload of color schemes.
    " https://github.com/flazz/vim-colorschemes#current-colorschemes
    Plug 'flazz/vim-colorschemes'
    " Fuzzy finder -- absolutely must have.
    Plug 'kien/ctrlp.vim'
    " Support for easily toggling comments.
    Plug 'tpope/vim-commentary'
    " Proper JSON filetype detection, and support.
    "Plug 'leshill/vim-json'
    " vim already has syntax support for javascript, but the indent support is
    " horrid. This fixes that.
    "Plug 'pangloss/vim-javascript'
    " vim indents HTML very poorly on it's own. This fixes a lot of that.
    "Plug 'indenthtml.vim'
    " I write markdown a lot. This is a good syntax.
    Plug 'tpope/vim-markdown'
    " LessCSS -- I use this every day.
    "Plug 'groenewege/vim-less'
    " Coffee-script syntax.
    "Plug 'kchmck/vim-coffee-script'
    Plug 'vimwiki/vimwiki'
    Plug 'junegunn/goyo.vim'
    Plug 'dracula/vim', { 'as': 'dracula' }
    Plug 'liuchengxu/vim-which-key' 
    Plug 'preservim/nerdtree'
    Plug 'ryanoasis/vim-devicons'
    Plug 'jlanzarotta/bufexplorer'

    Plug 'ObserverOfTime/coloresque.vim'
call plug#end()

" We have to turn this stuff back on if we want all of our features.
filetype plugin indent on " Filetype auto-detection
syntax on " Syntax highlighting

set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab " use spaces instead of tabs.
set smarttab " let's tab key insert 'tab stops', and bksp deletes tabs.
set shiftround " tab / shifting moves to closest tabstop.
set autoindent " Match indents on new lines.
set smartindent " Intellegently dedent / indent new lines based on rules.

" We have VCS -- we don't need this stuff.
set nobackup " We have vcs, we don't need backups.
set nowritebackup " We have vcs, we don't need backups.
set noswapfile " They're just annoying. Who likes them?

" don't nag me when hiding buffers
set hidden " allow me to have buffers with unsaved changes.
set autoread " when a file has changed on disk, just load it. Don't ask.

" Make search more sane
set ignorecase " case insensitive search
set smartcase " If there are uppercase letters, become case-sensitive.
set incsearch " live incremental searching
set showmatch " live match highlighting
set hlsearch " highlight matches
set gdefault " use the `g` flag by default.

" allow the cursor to go anywhere in visual block mode.
set virtualedit+=block

" leader is a key that allows you to have your own "namespace" of keybindings.
" You'll see it a lot below as <leader>
let mapleader = ","

" So we don't have to press shift when we want to get into command mode.
nnoremap ; :
vnoremap ; :

" So we don't have to reach for escape to leave insert mode.
inoremap jf <esc>

" create new vsplit, and switch to it.
noremap <leader>v <C-w>v

" bindings for easy split nav
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Use sane regex's when searching
nnoremap / /\v
vnoremap / /\v

" Clear match highlighting
noremap <leader><space> :noh<cr>:call clearmatches()<cr>

nnoremap <leader><leader> :CtrlPBuffer<CR>


" Visual line nav, not real line nav
" If you wrap lines, vim by default won't let you move down one line to the
" wrapped portion. This fixes that.
"noremap j gj
"noremap k gk

" Plugin settings:
" Below are some 'sane' (IMHO) defaults for a couple of the above plugins I
" referenced.

" Map the key for toggling comments with vim-commentary
nnoremap <leader>c :Commentary<CR>

" Remap ctrlp to ctrl-t -- map it however you like, or stick with the
" defaults. Additionally, in my OS, I remap caps lock to control. I never use
" caps lock. This is highly recommended.
let g:ctrlp_map = '<c-t>'

" Let ctrlp have up to 30 results.
let g:ctrlp_max_height = 30


" Finally the color scheme. Choose whichever you want from the list in the
" link above (back up where we included the bundle of a ton of themes.)
colorscheme dracula

"""""""""""""""""""""""""""""""""""""""""
"Custom stuff
"""""""""""""""""""""""""""""""""""""""""

"Keybinds
nnoremap <leader>t :split<CR>
nnoremap <leader>nn :e ~/.config/nvim/init.vim<CR>
"nnoremap <leader><tab> <Esc>/<++><Enter>"_c4l
vnoremap <leader><tab> <Esc>/<backspace><backspace><++><Enter>"_c4l
map <leader><tab> <Esc>/<backspace><backspace><++><Enter>"_c4l
nnoremap <leader>g :Goyo<CR>

"Variables
let g:vimwiki_list = [{'path': '~/Sync/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]

let g:vim_markdown_auto_extension_ext = 'wiki'

"Autocmds
"autocmd BufNewFile,BufRead *.wiki setlocal filetype=markdown
autocmd BufWritePost ~/.config/nvim/init.vim source %
autocmd BufWritePost patches.h,config.h,config.def.h !sudo make clean install
" autocmd BufWritePost ~/.local/bin/dwm-status !refbar
autocmd BufWritePost ~/zshconf/zshrc !source %

set wildmenu
set wildmode=longest,list,full

" inoremap <C-'> <ESC>:Commentary<CR>
nnoremap <leader>b :NERDTreeToggle<CR>
nnoremap <leader>f :NERDTreeCWD<CR>
nnoremap <leader>ntf :NERDTreeFind<CR>

set encoding=UTF-8
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

set splitbelow splitright
set clipboard=unnamedplus
let g:coloresque_extra_filetypes = ['.Xresources']
