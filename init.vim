set nocompatible

syntax on

" set 'selection', 'selectmode', 'mousemodel' and 'keymodel' for MS-Windows
behave mswin

" backspace in Visual mode deletes selection
vnoremap <BS> d

" clipboard with win32yank.exe
" in ~/.local/bin/win32yank.exe
let g:clipboard = {
                \   'name': 'win32yank-wsl',
                \   'copy': {
                \      '+': 'win32yank.exe -i --crlf',
                \      '*': 'win32yank.exe -i --crlf',
                \    },
                \   'paste': {
                \      '+': 'win32yank.exe -o --lf',
                \      '*': 'win32yank.exe -o --lf',
                \   },
                \   'cache_enabled': 1,
                \ }

if has("clipboard")
  " CTRL-X and SHIFT-Del are Cut
  vnoremap <C-X> "+x
  vnoremap <S-Del> "+x

  " CTRL-C and CTRL-Insert are Copy
  vnoremap <C-C> "+y
  vnoremap <C-Insert> "+y

  " CTRL-V and SHIFT-Insert are Paste
  map <C-V> "+gP
  map <S-Insert> "+gP

  cmap <C-V> <C-R>+
  cmap <S-Insert> <C-R>+
endif

" Pasting blockwise and linewise selections is not possible in Insert and
" Visual mode without the +virtualedit feature.  They are pasted as if they
" were characterwise instead.
" Uses the paste.vim autoload script.
" Use CTRL-G u to have CTRL-Z only undo the paste.

if 1
  exe 'inoremap <script> <C-V> <C-G>u' . paste#paste_cmd['i']
  exe 'vnoremap <script> <C-V> ' . paste#paste_cmd['v']
endif

imap <S-Insert> <C-V>
vmap <S-Insert> <C-V>

" Use CTRL-Q to do what CTRL-V used to do
noremap <C-Q> <C-V>

" Use CTRL-S for saving, also in Insert mode (<C-O> doesn't wor k well when
" using completions).
noremap <C-S> :update<CR>
vnoremap <C-S> <C-C>:update<CR>
inoremap <C-S> <Esc>:update<CR>gi

" For CTRL-V to work autoselect must be off.
" On Unix we have two selections, autoselect can be used.
if !has("unix")
  set guioptions-=a
endif

" CTRL-Z is Undo; not in cmdline though
noremap <C-Z> u
inoremap <C-Z> <C-O>u

" CTRL-Y is Redo (although not repeat); not in cmdline though
noremap <C-Y> <C-R>
inoremap <C-Y> <C-O><C-R>

" CTRL-A is Select all
noremap <C-A> gggH<C-O>G
inoremap <C-A> <C-O>gg<C-O>gH<C-O>G
cnoremap <C-A> <C-C>gggH<C-O>G
onoremap <C-A> <C-C>gggH<C-O>G
snoremap <C-A> <C-C>gggH<C-O>G
xnoremap <C-A> <C-C>ggVG

set statusline=%r%h%w%=\ [%4l,%3v]%3p%%\ [%L]
set tabstop=4 softtabstop=4 shiftwidth=4 expandtab

let g:rainbow_active = 1

" when there are unwanted white spaces and tabs, highlight them
set listchars=tab:>~,nbsp:_,trail:.
exec "set listchars=tab:\uBB\uBB,trail:\uB7,nbsp:~"
set list

" Various customizations for Haskell
let g:ormolu_command="/nix/store/mv4869fvrzwfjskpr1h9w1hg1bzaksph-fourmolu-0.10.1.0/bin/fourmolu"
let g:ormolu_options=["--no-cabal"]
let g:ormolu_suppress_stderr=1

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function',''')}

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1,1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0,1) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1,1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0,1)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1,1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0,1) : "\<C-b>"
endif

set termguicolors
set background=dark
colorscheme gruvbox

set number
set colorcolumn=80
let g:airline_section_z = '%#__accent_bold#%v%#__restore__#:%#__accent_bold#%l%#__restore__#/%L☰%#__accent_bold#%p%%%#__restore__#'
" highlight CocErrorHighlight ctermfg=223 ctermbg=124
" highlight CocUnusedHighlight ctermbg=172
" highlight CocFloating ctermbg=234

set incsearch hlsearch

