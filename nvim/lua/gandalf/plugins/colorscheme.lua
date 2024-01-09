return {
	{
		"ellisonleao/gruvbox.nvim",
		lazy = false,
		priority = 99,
		enabled = false,
		config = function()
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
			vim.cmd(":colorscheme gruvbox")
		end,
	},
	{
		"folke/tokyonight.nvim",
		lazy = false,
		priority = 99,
		enabled = true,
		config = function()
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
			vim.cmd(":colorscheme tokyonight")
		end,
	},
}
