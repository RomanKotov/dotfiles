"=== Styling section
:colorscheme koehler
:set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v]\ [%p%%]\ [LEN=%L]
:set laststatus=2
:set cursorline
:set number
:set numberwidth=4



"=== Editing section
:set incsearch
:set hlsearch



"=== Custom mappings
" find visually selected text
vnoremap // y/<C-R>"<CR>
