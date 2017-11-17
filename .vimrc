" Styling section
colorscheme koehler
set laststatus=2
set cursorline
set number
set numberwidth=4



" Editing section
set incsearch
set hlsearch



" Custom mappings
" find visually selected text
vnoremap // y/<C-R>"<CR>
let mapleader= " "
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
Plugin 'tpope/vim-surround' " Easy surround
Plugin 'sheerun/vim-polyglot' " Language support
Plugin 'slashmili/alchemist.vim' " Elixir support
Plugin 'elixir-editors/vim-elixir' " Elixir syntax support
Plugin 'christoomey/vim-tmux-navigator' " Navigate tmux and vim splits
Plugin 'tpope/vim-unimpaired.git' " Navigation mapping
let g:tmux_navigator_no_mappings = 0 " Use tmux-navigator mappings
let g:tmux_navigator_disable_when_zoomed = 1 " Disable tmux navigation when vim is zoomed

" Theme settings
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
set encoding=utf-8
let g:airline_theme='powerlineish'
set t_Co=256



" Settings executed after Vundle is set up
filetype plugin indent on
syntax on " Syntax highlighting
set showcmd
