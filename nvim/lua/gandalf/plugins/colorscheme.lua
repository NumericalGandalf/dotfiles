local A = { "ellisonleao/gruvbox.nvim" }
local B = { "folke/tokyonight.nvim" }

local initial = require("gandalf.prefs").initial_col

local schemes = {
	default = "default",
	gruvbox = "gruvbox",
	tokyonight = "tokyonight",
}

local function is_enabled(col)
	if col == initial then
		return true
	elseif require("gandalf.prefs").col_enablings[col] then
		return true
	else
		return false
	end
end

local function set_col(col, from_api)
	if not from_api and not (col == initial) then
		return
	end

	if col == schemes.default then
		vim.cmd(":highlight Normal guibg=none")
	end

	vim.cmd(":colorscheme " .. col)
end

vim.api.nvim_create_user_command("Col", function(opts)
	if not opts.args then
		return
	end
	set_col(opts.args, true)
end, {
	nargs = 1,
	complete = function()
		local cols = {}
		local utils = require("gandalf.utils")

		for _, v in pairs(schemes) do
			table.insert(cols, v)
		end
		for _, v in ipairs(vim.fn.getcompletion("", "color")) do
			if not utils.arr_contains(cols, v) then
				table.insert(cols, v)
			end
		end

		return cols
	end,
})

A.lazy = false

A.priority = 99

A.enabled = is_enabled(schemes.gruvbox)

function A.config()
	require(schemes.gruvbox).setup({
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

	set_col(schemes.gruvbox)
end

B.lazy = A.lazy

B.priority = A.priority

B.enabled = is_enabled(schemes.tokyonight)

function B.config()
	require(schemes.tokyonight).setup({
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

	set_col(schemes.tokyonight)
end

return { A, B }
