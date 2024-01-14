local M = {}

local function settings()
	vim.g.mapleader = " "

	vim.opt.termguicolors = true
	M.colorscheme = "default"

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

	vim.keymap.set("v", "<C-j>", ":m '>+1<CR>gv=gv")
	vim.keymap.set("v", "<C-k>", ":m '<-2<CR>gv=gv")

	vim.g.netrw_banner = 0
	vim.g.netrw_winsize = 20

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

function M.setup()
	M.gandalf_augroup = vim.api.nvim_create_augroup("gandalf_augroup", {
		clear = true,
	})

	settings()

	vim.api.nvim_create_user_command("ColDefault", function()
		vim.cmd(":colorscheme default")
		vim.cmd(":highlight Normal guibg=None")
	end, { nargs = 0 })
end

return M
