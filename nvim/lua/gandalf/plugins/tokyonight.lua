local M = { "folke/tokyonight.nvim" }

M.cond = false

M.lazy = false

M.priority = 61

function M.config()
	require("tokyonight").setup({
		style = "night",
		transparent = true,
		terminal_colors = true,
		styles = {
			comments = { italic = true },
			keywords = {},
			functions = {},
			variables = {},
			sidebars = "transparent",
			floats = "transparent",
		},
		hide_inactive_statusline = true,
		dim_inactive = false,
		lualine_bold = false,
	})
end

return M
