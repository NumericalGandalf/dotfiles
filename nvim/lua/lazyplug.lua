local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local plugins = {
    'folke/which-key.nvim',
    'folke/neodev.nvim',
    'folke/tokyonight.nvim',
    'ellisonleao/gruvbox.nvim',
    {
        'nvim-telescope/telescope.nvim',
        dependencies = { 'nvim-lua/plenary.nvim' },
    },
    {
        'nvim-treesitter/nvim-treesitter',
        build = ':TSUpdate'
    },
    {
        'nvim-lualine/lualine.nvim',
        dependencies = { 'nvim-tree/nvim-web-devicons' }
    },
    {
        'williamboman/mason.nvim',
        build = ':MasonUpdate'
    },
    "williamboman/mason-lspconfig.nvim",
    "neovim/nvim-lspconfig",
    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-buffer",
            'hrsh7th/cmp-path', 'hrsh7th/cmp-cmdline'
        }
    },
    'L3MON4D3/LuaSnip',
    'saadparwaiz1/cmp_luasnip',
    'pocco81/auto-save.nvim',
    'mfussenegger/nvim-dap',
    {
        "rcarriga/nvim-dap-ui",
        dependencies = { "mfussenegger/nvim-dap" }
    },
}

local opts = {
    checker = {
        enabled = true
    }
}

require("lazy").setup(plugins, opts)
