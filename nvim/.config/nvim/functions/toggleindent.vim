" Función para cambiar la forma en que se representan las indentaciones, ya
" sea con espacios o tabs

command! ToogleIndent call TabToggle()

function TabToggle()
	if &expandtab
		set tabstop=4
		set shiftwidth=4
		set noexpandtab
	else
		set tabstop=2
		set shiftwidth=2
		set expandtab
		set softtabstop=0
		set smarttab
	endif
endfunction
