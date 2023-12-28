return {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    event = 'BufEnter',
    config = function()
        require('nvim-treesitter.configs').setup({
            ensure_installed = {},
            sync_install = false,
            auto_install = true,
            ignore_install = {},
            highlight = {
                enable = true,
                disable = {},
                additional_vim_regex_highlighting = false
            },
            indent = {
                enable = true
            }
        })
    end
}
