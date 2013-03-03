set nocompatible 
syntax on
set number
set cursorline
set ruler
set shiftwidth=4
set softtabstop=4
set tabstop=8
set nobackup
set autochdir
filetype plugin indent on
set backupcopy=yes
set ignorecase smartcase
set nowrapscan
set incsearch
set hlsearch
set noerrorbells
set novisualbell
set showmatch
set matchtime=2
set magic
set hidden
set smartindent
set foldenable
set foldmethod=syntax
set foldcolumn=0
setlocal foldlevel=1
nnoremap <space> @=((foldclosed(line('.')) < 0) ? 'zc':'zo')<CR>
