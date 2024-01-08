return {
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
}
