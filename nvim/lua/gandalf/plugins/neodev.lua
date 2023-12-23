return {
    {
        'folke/which-key.nvim',
        event = 'VimEnter',
        enabled = false,
    },
    {
        'folke/neodev.nvim',
        ft = 'lua',
        config = function()
            require("neodev").setup({
                {
                    library = {
                        enabled = true,
                        runtime = true,
                        types = true,
                        plugins = true,
                    },
                    setup_jsonls = true,
                    override = function(root_dir, options) ; end,
                    lspconfig = true,
                    pathStrict = true,
                }
            })
        end
    },
    {
        'dstein64/vim-startuptime',
        cmd = "StartupTime"
    }
}
