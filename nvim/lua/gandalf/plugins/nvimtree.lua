return {
    'nvim-tree/nvim-tree.lua',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    lazy = false,
    config = function()
        require("nvim-tree").setup({
            view = {
                width = "20%"
            }
        })
        local api = require("nvim-tree.api")
        vim.keymap.set('n', '<leader>ft', api.tree.toggle)
    end
}
