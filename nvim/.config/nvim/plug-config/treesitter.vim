" Tree-sitter es un parser que soporta varios lenguajes, tener un parser nos
" permite mejorar varios aspectos del editor como mejores esquemas de color,
" mejor indentado, etc. (etc. porque si hay más cosas que se pueden hacer con
" el plugin, pero no me interesan por el momento)

lua <<EOF
require'nvim-treesitter.configs'.setup {
	-- Los lenguajes a instalar, maintained significa lenguajes que tienen
	-- soporte extendido
	ensure_installed = "maintained",

	-- Aquí ponemos lenguajes que no queremos que se instalen
	ignore_install = { "supercollider", "erlang", "cuda", "ocaml",
		"ocaml_interface", "tsx", "c_sharp", "zig", "hcl", "verilog", "kotlin" },

	-- Extensión que permite mejor detección de los elementos del lenguaje
	highlight = {
		enable = true,
		-- disable = { "c", "rust" }, -- list of language that will be disabled

		-- Setting this to true will run `:h syntax` and tree-sitter at the same time.
		-- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
		-- Using this option may slow down your editor, and you may see some duplicate highlights.
		-- Instead of true it can also be a list of languages
		additional_vim_regex_highlighting = false,
	},

	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = "gnn",
			node_incremental = "grn",
			scope_incremental = "grc",
			node_decremental = "grm",
		},
	},

	indent = {
		enable = true
	},
}
EOF
