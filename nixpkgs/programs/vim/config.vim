" Vim settings
set encoding=utf-8
scriptencoding utf-8
colorscheme solarized
set softtabstop=4
set backspace=2
set ai
set nocompatible
set nobackup
set confirm
set ruler
set syntax=on
set incsearch "jump to search results
set hlsearch
set showmatch
set showmode
set vb
set foldmethod=indent
set foldnestmax=2
set foldlevel=0
" we're using a terminal which supports 256 colors
set t_Co=256
"set encoding=UTF-8
set wildmode=longest,list,full
set wildmenu
" jump 5 lines when we hit bottom
set scrolljump=5
" we don't need to hit the exact bottom to trigger the scrolljump
set scrolloff=3
"see :help wrap
set sidescroll=5
set listchars=tab:_¸,trail:·
set list
set tw=72
set spelllang=en
set spell
set tags=tags;/
" colour the textwidth
set statusline=%{fugitive#statusline()}<%f%=\ [%1*%M%*%n%R%H]\ %-19(%3l,%02c%03V%)'%02b'%{&enc}%h>
" Always show the status line
set laststatus=2
" I like a fancy cursorline
set cursorline

" enable filetype plugins such those for editing XML files
filetype plugin on
filetype indent on

" for handling mouse events in xterm
colo default
syntax on

" pep8 plugin: ignore a few errors which are currently not *work*
" compatible: multiple spaces after ',' and line too long. 
let g:pep8_ignore="E501,E241"

"" Map Python trickery to function keys and commands
map <F8> :Explore<CR>
nnoremap <cr> :noh<cr><cr>
inoremap ,a ä
inoremap ,o ö
inoremap ,u ü
inoremap ,A Ä
inoremap ,O Ö
inoremap ,U Ü
inoremap ,s ß

augroup filetypedetect
    au! BufNewFile,BufRead *.pt         set ft=xml shiftwidth=2 softtabstop=2
    au! BufNewFile,BufRead *.zcml       set ft=xml shiftwidth=2 softtabstop=2
    au! BufNewFile,BufRead *.zpt        set ft=xml shiftwidth=2 softtabstop=2
    au! BufNewFile,BufRead *.xml        set ft=xml shiftwidth=2 softtabstop=2
    au! BufNewFile,BufRead *.css.dtml   set syntax=css
    au! BufNewFile,BufRead ChangeLog    set shiftwidth=8 tabstop=8 noexpandtab
    au! BufNewFile,BufReadPost *.md     set ft=markdown
    autocmd BufRead *.vala set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
    autocmd BufRead *.vapi set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
    au! BufRead,BufNewFile *.vala       setfiletype vala
    au! BufRead,BufNewFile *.vapi       setfiletype vala
    au  BufRead,BufNewFile *.rb         set ts=2 sts=2 sw=2 expandtab
    au! BufRead,BufNewFile /home/rjoost/Maildir/Drafts/new/* set ft=mail
augroup END

autocmd FileType html map <F2> <Esc>:1,$!tidy -q -i --show-errors 0<CR>
autocmd FileType python map <F9> :ToggleSliceBuffer<CR>
autocmd FileType python vmap g/ :call SortMultipleLines()<CR>
autocmd FileType python vmap m/ :call DeMartinify()<CR>
autocmd FileType python iab pdb import pdb; pdb.set_trace()
autocmd FileType ruby iab pdb require 'ruby-debug'; debugger

" delete with Strg+D all to signature
autocmd FileType mail map <C-d> :.;/^-- $/d<CR>O-- <UP><End><CR>
" Automatically delete trailing whitespace in python files
command! -nargs=0 TidyWhiteSpace :call s:tidy_whitespace()

function s:tidy_whitespace()
    exe ':%s/\s\+$//e'
endfunction

" abbreviations
ab impytest import pytest; pytest.set_trace()
ab cbred border: 1px solid red

" disable F1 for immediately open the help -- make a choice
inoremap <F1> <Esc>
noremap <F1> :call MapF1()<cr>

function! MapF1()
  if &buftype == "help"
    exec 'quit'
  else
    exec 'help'
  endif
endfunction

" Use a differen color for pyflakes
highlight SpellBad cterm=bold ctermbg=none ctermfg=brown

" Sort multiline imports
function! SortMultipleLines() range
    let save_cursor = getpos(".")
    execute a:firstline . "," . a:lastline . 's/\\\@<=\n/|'
    execute a:firstline . "," . a:lastline . 'sort'
    execute a:firstline . "," . a:lastline . 's/\\\@<=|/\r/g'
    call setpos('.', save_cursor)
endfunction

" https://gist.github.com/287147
" http://vimcasts.org/episodes/aligning-text-with-tabular-vim/
inoremap <silent> <Bar>   <Bar><Esc>:call <SID>align()<CR>a

function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':Tabularize') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let column = strlen(substitute(getline('.')[0:col('.')],'[^|]','','g'))
    let position = strlen(matchstr(getline('.')[0:col('.')],'.*|\s*\zs.*'))
    Tabularize/|/l1
    normal! 0
    call search(repeat('[^|]*|',column).'\s\{-\}'.repeat('.',position),'ce',line('.'))
  endif
endfunction

" vim-airline
let g:airline_powerline_fonts=1

" ctrlp
let g:ctrlp_custom_ignore = {
    \ 'dir': '\v(eggs|coverage|dist/build|dist-*sandbox*)',
    \ }