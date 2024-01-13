local M = {}

M.enablings = {
	autopairs = false,
	autosave = false,
	fidget = false,
	neotree = false,
	lspsaga = false,
	dapui = false,
	whichkey = false,
}

M.col_enablings = {
	tokyonight = true,
	gruvbox = false,
}

local function set_prefs()
	vim.g.mapleader = " "

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

	vim.keymap.set("v", "<C-j>", ":m '>+1<CR>gv=gv")
	vim.keymap.set("v", "<C-k>", ":m '<-2<CR>gv=gv")

	if M.enablings.neotree then
		vim.g.loaded_netrw = 1
		vim.g.loaded_netrwPlugin = 1
	else
		vim.g.netrw_banner = 0
		vim.g.netrw_winsize = 20
		vim.keymap.set("n", "<leader>ft", function()
			vim.cmd(":Lexplore")
		end)
	end

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
	set_prefs()
	M.initial_col = "default"
end

return M
