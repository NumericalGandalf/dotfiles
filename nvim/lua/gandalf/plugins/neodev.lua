local M = { "folke/neodev.nvim" }

M.ft = "lua"

function M.config()
	require("neodev").setup({
		library = {
			enabled = true,
			runtime = true,
			types = true,
			plugins = true,
		},
		setup_jsonls = true,
		lspconfig = true,
		pathStrict = true,
	})
end

return M
