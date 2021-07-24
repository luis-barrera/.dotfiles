" Función para cambiar el tamaño de los tabuladores
command! -nargs=* Stab call stab#stab(<f-args>)

function! stab#stab(...)
	if a:0 > 0
		let &l:tabstop = a:1
		let &l:shiftwidth = a:1
	else
		let &l:tabstop = 4
		let &l:shiftwidth = 4
	endif

	setlocal noexpandtab
	setlocal autoindent
endfunction
