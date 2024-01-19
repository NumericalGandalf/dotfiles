local M = {}

local function set_configs()
	vim.g.mapleader = " "
	vim.opt.mouse = ""

	vim.opt.termguicolors = true

	vim.opt.number = true
	vim.opt.relativenumber = true
	vim.opt.scrolloff = 15

	vim.opt.autoindent = true
	vim.opt.smartindent = true
	vim.opt.wrap = false

	vim.opt.expandtab = true
	vim.opt.tabstop = 4
	vim.opt.softtabstop = 4
	vim.opt.shiftwidth = 4

	vim.opt.swapfile = false
	vim.opt.backup = false

	vim.opt.incsearch = true
	vim.opt.hlsearch = false

	vim.opt.laststatus = 3

	vim.g.netrw_banner = 0

	vim.fn.sign_define("DiagnosticSignError", {
		text = "E",
		texthl = "DiagnosticSignError",
	})
	vim.fn.sign_define("DiagnosticSignWarn", {
		text = "W",
		texthl = "DiagnosticSignWarn",
	})
	vim.fn.sign_define("DiagnosticSignInfo", {
		text = "I",
		texthl = "DiagnosticSignInfo",
	})
	vim.fn.sign_define("DiagnosticSignHint", {
		text = "H",
		texthl = "DiagnosticSignHint",
	})
end

local function set_keymaps()
	vim.keymap.set("v", "<C-j>", ":m '>+1<CR>gv=gv")
	vim.keymap.set("v", "<C-k>", ":m '<-2<CR>gv=gv")
end

function M.setup()
	M.gandalfs = vim.api.nvim_create_augroup("Gandalfs", {
		clear = true,
	})

	set_configs()
	set_keymaps()

	vim.cmd(":colorscheme default")
end

return M
