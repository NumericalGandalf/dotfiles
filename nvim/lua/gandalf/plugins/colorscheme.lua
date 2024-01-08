return {
	{
		"ellisonleao/gruvbox.nvim",
		lazy = false,
		priority = 99,
		enabled = true,
		config = function()
			require("gruvbox").setup({
				terminal_colors = true,
				undercurl = true,
				underline = true,
				bold = true,
				italic = {
					strings = true,
					emphasis = true,
					comments = true,
					operators = false,
					folds = true,
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
				transparent_mode = false,
			})
			vim.opt.fillchars = "eob: "
			vim.cmd(":colorscheme gruvbox")
		end,
	},
	{
		"folke/tokyonight.nvim",
		lazy = false,
		priority = 99,
		enabled = false,
		config = function()
			require("tokyonight").setup({
				style = "moon",
				transparent = true,
				terminal_colors = true,
				styles = {
					comments = { italic = true },
					keywords = { italic = false },
					functions = { bold = true },
					variables = {},
					sidebars = "transparent",
					floats = "transparent",
				},
				hide_inactive_statusline = true,
				dim_inactive = false,
				lualine_bold = true,
			})
			vim.cmd(":colorscheme tokyonight")
		end,
	},
}
