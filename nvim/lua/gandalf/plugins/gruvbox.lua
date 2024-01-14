local M = { "ellisonleao/gruvbox.nvim" }

M.cond = false

M.lazy = false

M.priority = 61

function M.config()
	require("gruvbox").setup({
		terminal_colors = true,
		undercurl = true,
		underline = true,
		bold = false,
		italic = {
			strings = false,
			emphasis = false,
			comments = true,
			operators = false,
			folds = false,
		},
		strikethrough = true,
		invert_selection = false,
		invert_signs = false,
		invert_tabline = false,
		invert_intend_guides = false,
		inverse = true,
		contrast = "",
		palette_overrides = {},
		overrides = {},
		dim_inactive = false,
		transparent_mode = true,
	})
end

return M
