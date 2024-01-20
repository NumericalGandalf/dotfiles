local M = { "neovim/nvim-lspconfig" }

M.dependencies = {
	"folke/neoconf.nvim",
	"folke/neodev.nvim",
	"williamboman/mason-lspconfig.nvim",
	"hrsh7th/cmp-nvim-lsp",
	"stevearc/conform.nvim",
	"mfussenegger/nvim-lint",
}

M.event = "BufEnter"

local lsp_actions = {}

local function set_lsp_actions_nosaga()
	lsp_actions.go_def = vim.lsp.buf.definition
	lsp_actions.go_typedef = vim.lsp.buf.type_definition

	lsp_actions.hover = vim.lsp.buf.hover
	lsp_actions.ca = vim.lsp.buf.code_action
	lsp_actions.rename = vim.lsp.buf.rename

	lsp_actions.diag_p = vim.diagnostic.goto_prev
	lsp_actions.diag_n = vim.diagnostic.goto_next
end

local function set_lsp_actions_wsaga()
	lsp_actions.go_def = function()
		vim.cmd(":Lspsaga goto_definition")
	end
	lsp_actions.go_typedef = function()
		vim.cmd(":Lspsaga goto_type_definition")
	end

	lsp_actions.hover = function()
		vim.cmd(":Lspsaga hover_doc")
	end
	lsp_actions.rename = function()
		vim.cmd(":Lspsaga rename")
	end
	lsp_actions.ca = function()
		vim.cmd(":Lspsaga code_action")
	end

	lsp_actions.diag_p = function()
		vim.cmd(":Lspsaga diagnostic_jump_prev")
	end
	lsp_actions.diag_n = function()
		vim.cmd(":Lspsaga diagnostic_jump_next")
	end
end

local function attach_saga_extras(opts)
	vim.keymap.set("n", "gpd", function()
		vim.cmd(":Lspsaga peek_definition")
	end, opts)
	vim.keymap.set("n", "gpr", function()
		vim.cmd(":Lspsaga finder def+ref+imp")
	end, opts)
	vim.keymap.set("n", "gpt", function()
		vim.cmd(":Lspsaga peek_type_definition")
	end, opts)
end

local function on_lsp_attach(ev)
	local opts = { buffer = ev.buf }
	vim.bo[opts.buffer].omnifunc = "v:lua.vim.lsp.omnifunc"
	local telescope = require("telescope.builtin")

	vim.keymap.set("n", "gd", lsp_actions.go_def)
	vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
	vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
	vim.keymap.set("n", "gt", lsp_actions.go_typedef)
	vim.keymap.set("n", "gr", telescope.lsp_references, opts)

	vim.keymap.set("n", "K", lsp_actions.hover)
	vim.keymap.set("n", "ca", lsp_actions.ca)
	vim.keymap.set("n", "<leader>rn", lsp_actions.rename)

	vim.keymap.set("n", "[d", lsp_actions.diag_p, opts)
	vim.keymap.set("n", "]d", lsp_actions.diag_n, opts)
	vim.keymap.set("n", "<leader>vd", vim.diagnostic.setqflist, opts)
	vim.keymap.set("n", "<leader>vD", vim.diagnostic.setloclist, opts)

	vim.keymap.set("n", "gS", telescope.lsp_document_symbols, opts)
	vim.keymap.set("n", "gs", telescope.lsp_workspace_symbols, opts)

	if require("gandalf.plugins.lspsaga").cond then
		attach_saga_extras(opts)
	end

	vim.keymap.set("n", "gh", function()
		vim.cmd(":ClangdSwitchSourceHeader")
	end, opts)
end

function M.config()
	if require("gandalf.plugins.lspsaga").cond then
		require("lspsaga")
		set_lsp_actions_wsaga()
	else
		set_lsp_actions_nosaga()
	end

	vim.api.nvim_create_autocmd("LspAttach", {
		group = require("gandalf.settings").gandalfs,
		callback = on_lsp_attach,
	})

	local lspconfig = require("lspconfig")
	local capabilities = require("cmp_nvim_lsp").default_capabilities()

	lspconfig.lua_ls.setup({
		capabilities = capabilities,
	})

	lspconfig.pyright.setup({
		capabilities = capabilities,
	})

	lspconfig.bashls.setup({
		filetypes = { "sh", "zsh" },
		capabilities = capabilities,
	})

	lspconfig.clangd.setup({
		capabilities = capabilities,
	})

	lspconfig.jsonls.setup({
		capabilities = capabilities,
	})
end

return M
