" Vim color file
" Create by Andy
" QQ24375048

set background=dark
if version > 580
    hi clear
    if exists("syntax_on")
	syntax reset
    endif
endif
let g:colors_name="test"

"hi Normal	guifg=#DFD6C1 guibg=gray10 gui=none
highlight Normal guifg=#969696 guibg=gray10 gui=NONE ctermfg=246 ctermbg=234 cterm=NONE

" highlight groups
highlight Cursor guifg=black guibg=yellow gui=NONE ctermfg=16 ctermbg=226 cterm=NONE
highlight ErrorMsg guifg=white guibg=red gui=NONE ctermfg=231 ctermbg=196 cterm=NONE
highlight VertSplit guifg=gray40 guibg=gray40 gui=NONE ctermfg=241 ctermbg=241 cterm=NONE
highlight Folded guifg=darkslategray3 guibg=grey30 gui=NONE ctermfg=116 ctermbg=239 cterm=NONE
highlight FoldColumn guifg=tan guibg=grey30 gui=NONE ctermfg=180 ctermbg=239 cterm=NONE
highlight IncSearch guifg=#b0ffff guibg=#2050d0 ctermfg=159 ctermbg=26
highlight LineNr term=underline guifg=#509050 guibg=grey10 gui=NONE ctermfg=65 ctermbg=234 cterm=NONE
highlight ModeMsg guifg=skyblue gui=NONE ctermfg=117 cterm=NONE
highlight MoreMsg guifg=seagreen gui=NONE ctermfg=29 cterm=NONE
highlight NonText guifg=cyan gui=NONE ctermfg=51 cterm=NONE
highlight Question guifg=springgreen gui=NONE ctermfg=48 cterm=NONE
highlight Search guifg=gray80 guibg=#445599 gui=NONE ctermfg=252 ctermbg=61 cterm=NONE
highlight SpecialKey guifg=cyan gui=NONE ctermfg=51 cterm=NONE
highlight StatusLine guifg=black guibg=pink gui=bold ctermfg=16 ctermbg=217 cterm=bold
highlight StatusLineNC guifg=grey guibg=gray40 gui=NONE ctermfg=250 ctermbg=241 cterm=NONE
highlight Title guifg=#ff4400 gui=bold ctermfg=202 cterm=bold
highlight Visual guifg=gray17 guibg=tan1 gui=NONE ctermfg=235 ctermbg=215 cterm=NONE
highlight WarningMsg guifg=salmon gui=NONE ctermfg=210 cterm=NONE
highlight Pmenu guifg=white guibg=#445599 gui=NONE ctermfg=231 ctermbg=61 cterm=NONE
highlight PmenuSel guifg=#445599 guibg=gray ctermfg=61 ctermbg=250
highlight WildMenu guifg=gray guibg=gray17 gui=NONE ctermfg=250 ctermbg=235 cterm=NONE
highlight MatchParen guifg=cyan guibg=#6c6c6c gui=bold ctermfg=51 ctermbg=242 cterm=bold
highlight DiffAdd guifg=black guibg=wheat1 ctermfg=16 ctermbg=223
highlight DiffChange guifg=black guibg=skyblue1 ctermfg=16 ctermbg=117
highlight DiffText guifg=black guibg=hotpink1 gui=NONE ctermfg=16 ctermbg=205 cterm=NONE
highlight DiffDelete guifg=black guibg=gray45 gui=NONE ctermfg=16 ctermbg=243 cterm=NONE

" syntax highlighting groups
highlight Comment term=underline guifg=#509050 guibg=grey10 gui=italic ctermfg=65 ctermbg=234
highlight Constant guifg=#ff77ff gui=NONE ctermfg=213 cterm=NONE
highlight Identifier guifg=#6fdef8 gui=NONE ctermfg=81 cterm=NONE
highlight Function guifg=#82ef2a gui=NONE ctermfg=118 cterm=NONE
highlight Statement guifg=#006666 gui=NONE ctermfg=23 cterm=NONE
highlight PreProc guifg=#ff6600 gui=NONE ctermfg=202 cterm=NONE
highlight Type guifg=#33aff3 gui=NONE ctermfg=39 cterm=NONE
highlight Special guifg=orange gui=NONE ctermfg=214 cterm=NONE
highlight Ignore guifg=red gui=NONE ctermfg=196 cterm=NONE
highlight Todo guifg=red guibg=yellow2 gui=NONE ctermfg=196 ctermbg=226 cterm=NONE

