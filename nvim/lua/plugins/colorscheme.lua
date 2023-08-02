require("tokyonight").setup({
    style = "night",
    transparent = true,
    styles = {
        comments = { italic = true },
        keywords = { italic = true },
        sidebars = "transparent",
        floats = "transparent",
    }
})

vim.cmd[[colorscheme tokyonight]]
