vim.api.nvim_create_user_command('SetColorscheme',
    function(opts)
        local colorscheme = opts.fargs[1]
        if colorscheme == "tokyonight" then
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
        else
            error(string.format("Invalid colorscheme '%s'", colorscheme))
            return
        end

        vim.api.nvim_command(string.format("colorscheme %s", colorscheme))
    end,
    { nargs = 1, complete = function() return { "tokyonight" } end }
)

vim.cmd.SetColorscheme("tokyonight")
