return {
	"neovim/nvim-lspconfig",
	dependencies = {
		"williamboman/mason-lspconfig.nvim",
		"nvimdev/lspsaga.nvim",
		"hrsh7th/cmp-nvim-lsp",
	},
	event = "BufEnter",
	config = function()
		vim.api.nvim_create_autocmd("LspAttach", {
			group = vim.api.nvim_create_augroup("UserLspConfig", {}),
			callback = function(ev)
				vim.bo[ev.buf].omnifunc = "v:lua.vim.lsp.omnifunc"
				local opts = { buffer = ev.buf }
				local telescope = require("telescope.builtin")
				vim.keymap.set("n", "<leader>vd", vim.diagnostic.open_float, opts)
				vim.keymap.set("n", "[d", function()
					vim.cmd(":Lspsaga diagnostic_jump_prev")
				end, opts)
				vim.keymap.set("n", "]d", function()
					vim.cmd(":Lspsaga diagnostic_jump_next")
				end, opts)
				vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
				vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
				vim.keymap.set("n", "gpd", function()
					vim.cmd(":Lspsaga peek_definition")
				end, opts)
				vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
				vim.keymap.set("n", "gr", telescope.lsp_references, opts)
				vim.keymap.set("n", "gpr", function()
					vim.cmd(":Lspsaga finder def+ref+imp")
				end, opts)
				vim.keymap.set("n", "gt", vim.lsp.buf.type_definition, opts)
				vim.keymap.set("n", "gpt", function()
					vim.cmd(":Lspsaga peek_type_definition")
				end, opts)
				vim.keymap.set("n", "gh", function()
					vim.cmd(":ClangdSwitchSourceHeader")
				end, opts)
				-- vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
				vim.keymap.set("n", "K", function()
					vim.cmd(":Lspsaga hover_doc")
				end, opts)
				vim.keymap.set("n", "Ks", vim.lsp.buf.signature_help, opts)
				vim.keymap.set({ "n", "v" }, "<leader>ca", function()
					vim.cmd(":Lspsaga code_action")
				end, opts)
				vim.keymap.set("n", "<leader>rn", function()
					vim.cmd(":Lspsaga rename")
				end, opts)
				vim.keymap.set("n", "gS", telescope.lsp_document_symbols, opts)
				vim.keymap.set("n", "gs", telescope.lsp_workspace_symbols, opts)
			end,
		})
		require("mason-lspconfig").setup()
		require("lspsaga").setup({
			lightbulb = {
				virtual_text = false,
			},
		})
		local lspconfig = require("lspconfig")
		local capabilities = require("cmp_nvim_lsp").default_capabilities()
		lspconfig.lua_ls.setup({
			capabilities = capabilities,
			settings = {
				Lua = {
					runtime = {
						version = "LuaJIT",
					},
					diagnostics = {
						globals = { "vim" },
					},
					completion = {
						callSnippet = "Replace",
					},
					format = {
						enable = false,
					},
				},
			},
		})
		lspconfig.pyright.setup({
			capabilities = capabilities,
		})
		lspconfig.bashls.setup({
			capabilities = capabilities,
			filetypes = { "sh", "zsh" },
			settings = {
				bashIde = {
					globPattern = "*@(.sh|.inc|.bash|.command)",
				},
			},
		})
		lspconfig.clangd.setup({
			capabilities = capabilities,
		})
	end,
}
