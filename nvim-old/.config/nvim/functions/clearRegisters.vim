" Función para limpiar todos los registros, no afecta a los registros
" especiales * + %

command! ClearRegisters call ClearRegisters()

function ClearRegisters()
	let regs=split('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/-"', '\zs')

	for r in regs
		call setreg(r, [])
	endfor
endfunction
