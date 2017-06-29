" Styling section
:colorscheme koehler
:set laststatus=2
:set cursorline
:set number
:set numberwidth=4



" Editing section
:set incsearch
:set hlsearch



" Custom mappings
" find visually selected text
vnoremap // y/<C-R>"<CR>
let mapleader= " "
map <leader>h :wincmd h<CR>
map <leader>j :wincmd j<CR>
map <leader>k :wincmd k<CR>
map <leader>l :wincmd l<CR>
nmap <silent> <Leader>f :NERDTreeToggle<CR>



" Vundle section
" Vundle settings
set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
" Run :VundleInstall to install plugins inside vim

" Vundle Plugins
Plugin 'gmarik/vundle' " Vundle itself
Plugin 'scrooloose/nerdtree.git' " File browsing
Plugin 'Xuyuanp/nerdtree-git-plugin' " NERDTree browsing git integration
Plugin 'airblade/vim-gitgutter' " Git diff show
Plugin 'tpope/vim-fugitive' " Git integration

" Theme settings
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
let g:airline_theme='powerlineish'
set t_Co=256



" Settings executed after Vundle is set up
filetype plugin indent on
syntax on " Syntax highlighting
