vim.api.nvim_create_user_command('SetColorscheme',
    function(opts)
        local colorscheme = opts.fargs[1]
        if colorscheme == "tokyonight" then
            ;
        elseif colorscheme == "gruvbox" then
            ;
        else
            error(string.format("Invalid colorscheme '%s'", colorscheme))
            return
        end
        vim.api.nvim_command(string.format("colorscheme %s", colorscheme))
    end,
    { nargs = 1, complete = function() return { "tokyonight", "gruvbox" } end }
)

return {
    {
        'folke/tokyonight.nvim',
        lazy = false,
        enabled = false,
        config = function()
            require("tokyonight").setup({
                style = "night",
                transparent = true,
                terminal_colors = true,
                styles = {
                    comments = { italic = true },
                    keywords = { italic = false },
                    functions = {},
                    variables = {},
                    sidebars = "transparent",
                    floats = "transparent",
                },
                hide_inactive_statusline = true,
                dim_inactive = false,
                lualine_bold = false,
                on_colors = function(colors) end,
                on_highlights = function(highlights, colors) end,
            })
            vim.cmd.SetColorscheme("tokyonight")
        end
    },
    {
        'ellisonleao/gruvbox.nvim',
        lazy = false,
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
                    operators = true,
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
            vim.cmd.SetColorscheme("gruvbox")
        end
    }
}
