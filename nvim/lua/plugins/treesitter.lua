require'nvim-treesitter.configs'.setup {
    ensure_installed = { "bash", "c", "cpp", "python", "lua" },
    sync_install = false,
    auto_install = true,
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false
    }
}
