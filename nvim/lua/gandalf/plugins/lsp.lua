return {
	{
		"williamboman/mason.nvim",
		build = ":MasonUpdate",
		event = "VimEnter",
		config = function()
			require("mason").setup()
		end,
	},
	{
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
	},
	{
		"stevearc/conform.nvim",
		event = "BufEnter",
		config = function()
			require("conform").setup({
				formatters_by_ft = {
					lua = { "stylua" },
					python = { "isort", "black" },
					sh = { "shfmt" },
					c = { "clang_format" },
					cpp = { "clang_format" },
				},
				format_on_save = {
					lsp_fallback = false,
					async = false,
					timeout_ms = 2000,
				},
			})
		end,
	},
	{
		"mfussenegger/nvim-lint",
		event = "BufEnter",
		config = function()
			local lint = require("lint")
			lint.linters_by_ft = {
				python = { "flake8", "mypy" },
			}
			vim.api.nvim_create_autocmd("BufWritePost", {
				callback = function()
					lint.try_lint()
				end,
			})
		end,
	},
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-buffer",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-cmdline",
			"L3MON4D3/LuaSnip",
			"saadparwaiz1/cmp_luasnip",
		},
		event = { "CmdlineEnter", "InsertEnter" },
		config = function()
			local cmp = require("cmp")
			cmp.setup({
				snippet = {
					expand = function(args)
						require("luasnip").lsp_expand(args.body)
					end,
				},
				window = {
					completion = cmp.config.window.bordered(),
					documentation = cmp.config.window.bordered(),
				},
				mapping = cmp.mapping.preset.insert({
					["<C-b>"] = cmp.mapping.scroll_docs(-4),
					["<C-f>"] = cmp.mapping.scroll_docs(4),
					-- ['<Tab>'] = cmp.mapping.complete(),
					["<C-e>"] = cmp.mapping.abort(),
					["<C-o>"] = cmp.mapping.confirm({ select = true }),
				}),
				sources = cmp.config.sources({
					{ name = "nvim_lsp" },
					{ name = "luasnip" },
				}, {
					{ name = "buffer" },
				}),
			})
			cmp.setup.cmdline({ "/", "?" }, {
				mapping = cmp.mapping.preset.cmdline(),
				sources = {
					{ name = "buffer" },
				},
			})
			cmp.setup.cmdline(":", {
				mapping = cmp.mapping.preset.cmdline(),
				sources = cmp.config.sources({
					{ name = "path" },
				}, {
					{ name = "cmdline" },
				}),
			})
		end,
	},
}
