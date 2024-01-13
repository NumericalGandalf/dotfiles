local M = { "stevearc/conform.nvim" }

M.event = "BufEnter"

M.by_autosave = false
M.timeout = 2500

function M.invoke()
	require("conform").format({
		lsp_fallback = false,
		async = false,
		timeout_ms = M.timeout,
	})
end

function M.config()
	require("conform").setup({
		formatters_by_ft = {
			lua = { "stylua" },
			python = { "isort", "black" },
			sh = { "shfmt" },
			c = { "clang_format" },
			cpp = { "clang_format" },
		},
	})

	vim.api.nvim_create_autocmd("BufWritePre", {
		group = require("gandalf.prefs").gandalf_augroup,
		callback = function()
			if not M.by_autosave then
				M.invoke()
			end
		end,
	})
end

return M
