function! custom#before() abort
endfunction

function! custom#after() abort
  set clipboard=unnamedplus

  "" License
  let g:license_author = 'Álan Crístoffer'
  let g:license_email = 'acristoffers@gmail.com'

  "" COC Exensions
  let g:coc_global_extensions = [
            \ 'coc-marketplace', 'coc-tsserver', 'coc-json', 'coc-html',
            \ 'coc-css', 'coc-python', 'coc-angular', 'coc-clangd',
            \ 'coc-elixir', 'coc-erlang_ls', 'coc-eslint', 'coc-flutter',
            \ 'coc-fsharp', 'coc-git', 'coc-go', 'coc-highlight',
            \ 'coc-lsp-wl', 'coc-markdownlint', 'coc-omnisharp', 'coc-java',
            \ 'coc-powershell', 'coc-rls', 'coc-snippets', 'coc-solargraph',
            \ 'coc-sourcekit', 'coc-actions', 'coc-spell-checker',
            \ 'coc-texlab', 'coc-xml', 'coc-yaml', 'coc-yank', 'coc-vetur',
            \ 'coc-spell-checker', 'coc-cmake', 'coc-svg'
            \ ]
  "" Ale
  let g:ale_set_quickfix=1
  let g:ale_fix_on_save=1
  let g:ale_fixers = {
              \ 'javascript': ['eslint'],
              \ 'typescript': ['eslint'],
              \ 'r': 'styler',
              \ 'html': 'prettier'
              \ }

  "" LaTeX Suite
  let g:tex_flavor="latex"

  "" Autoformat
  let g:formatdef_custom_c='"uncrustify -q -l C -c /Users/Alan/bin/cstyle.cfg"'
  let g:formatdef_custom_cpp='"uncrustify -q -l CPP -c /Users/Alan/bin/cstyle.cfg"'
  let g:formatdef_custom_js='"js-beautify"'
  let g:formatdef_custom_ts='"eslint --fix "'
  let g:formatters_c=['custom_c']
  let g:formatters_cpp=['custom_cpp']
  let g:formatters_javascript=['custom_js']
  let g:formatters_typescript=['custom_ts']

  autocmd FileType fish compiler fish
  autocmd FileType fish setlocal textwidth=80
  autocmd FileType fish set foldmethod=expr
  autocmd FileType python set foldmethod=indent
  autocmd FileType vue set tabstop=2 softtabstop=2 shiftwidth=2
  autocmd FileType typescript set tabstop=2 softtabstop=2 shiftwidth=2
  autocmd FileType tex imap ( (<++>)<++><c-j>
  autocmd FileType tex imap [ [<++>]<++><c-j>
  autocmd FileType tex imap { {<++>}<++><c-j>
  autocmd FileType tex inoremap $$ \(<++>\)<++>
  autocmd FileType tex nmap § <F5>
  autocmd FileType tex setlocal makeprg=latexmk
  autocmd FileType tex setlocal nofoldenable
  autocmd FileType tex vmap ,eq :call VEnclose('\enquote{', '}', '\enquote{', '}')<CR>

  if exists('+colorcolumn')
      autocmd BufEnter,FocusGained,VimEnter,WinEnter * let &l:colorcolumn='+' . join(range(1, 255), ',+')
      autocmd FocusLost,WinLeave * let &l:colorcolumn=join(range(1, 255), ',')
  endif
endfunction

